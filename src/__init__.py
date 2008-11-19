from subprocess import Popen, PIPE
import ansicolor as clr
import cmd_util as cu
import os
import re
import readline
import select
import sys
import tempfile
import time

comm_poll_timeout = 0.01

def help_message():
   return """\
-- Help --
Type php commands and they will be evaluted each time you hit enter. Ex:
php> $msg = "hello world"

Put = at the beginning of a line as syntactic sugar for return. Ex:
php> = 2 + 2
4

phpsh will print any returned value (in yellow) and also assign the last
returned value to the variable $_.  Anything printed to stdout shows up blue,
and anything sent to stderr shows up red.

You can enter multiline input, such as a multiline if statement.  phpsh will
accept further lines until you complete a full statement, or it will error if
your partial statement has no syntactic completion.  You may also use ^C to
cancel a partial statement.

You can use tab to autocomplete function names, global variable names,
constants, classes, and interfaces.  If you are using ctags, then you can hit
tab again after you've entered the name of a function, and it will show you
the signature for that function.  phpsh also supports all the normal
readline features, like ctrl-e, ctrl-a, and history (up, down arrows).

Note that stdout and stderr from the underlying php process are line-buffered;
so  php> for ($i = 0; $i < 3; $i++) {echo "."; sleep(1);}
will print the three dots all at once after three seconds.
(echo ".\n" would print one a second.)

See phpsh -h for invocation options.

-- phpsh quick command list --
    h     Display this help text.
    r     Reload (e.g. after a code change).  args to r append to add
            includes, like: php> r ../lib/username.php
            (use absolute paths or relative paths from where you start phpsh)
    R     Like 'r', but change includes instead of appending.
    d     Get documentation for a function or other identifier.
             ex: php> d my_function
    D     Like 'd', but gives more extensive documentation for builtins.
    v     Open vim read-only where a function or other identifer is defined.
             ex: php> v some_function
    V     Open vim (not read-only) and reload (r) upon return to phpsh.
    e     Open emacs where a function or other identifer is defined.
             ex: php> e some_function
    c     Append new includes without restarting; display includes.
    C     Change includes without restarting; display includes.
    !     Execute a shell command.
             ex: php> ! pwd
    q     Quit (ctrl-D also quits)
"""

def do_sugar(line):
    line = line.lstrip()
    if line.startswith("="):
        line = "return " + line[1:]
    if line:
        line += ";"
    return line

def line_encode(line):
    return cu.multi_sub({'\n': '\\n', '\\': '\\\\'}, line) + "\n"

def inc_args(s):
    """process a string of includes to a set of them"""
    return set([inc.strip() for inc in s.split(" ") if inc.strip()])

class PhpMultiliner:
    """This encapsulates the process and state of intaking multiple input lines
    until a complete php expression is formed, or (hopefully eventually..)
    detecting a syntax error.

    Note: this is not perfectly encapsulated while the parser has global state
    """

    complete = "complete"
    incomplete = "incomplete"
    syntax_error = "syntax_error"

    def __init__(self):
        self.partial = ""

    def check_syntax(self, line):
        p = Popen(["phpsh_check_syntax", line], stderr=PIPE)
        p.wait()
        l = p.stderr.readline()
        if l.find('syntax error') != -1:
            if l.find('unexpected $end') != -1:
                return (self.incomplete, l)
            return (self.syntax_error, l)
        return (self.complete, l)

    def input_line(self, line):
        if self.partial:
            self.partial += "\n"
        self.partial += line
        partial_mod = do_sugar(self.partial)
        if not partial_mod:
            return (self.complete, "")

        # There is a terrible bug in php/eval where and unclosed ' only creates
        # a warning!  (" and ` correctly give syntax errors.)
        # So we have to explicitly and hackily check for this error..
        #
        # If this does _not_ error, then you have an unclosed '
        may_be_right = True
        (syntax_info, result_str) = self.check_syntax(partial_mod + ";())';")
        if syntax_info == self.complete:
            may_be_right = False

        if may_be_right:
            (syntax_info, result_str) = self.check_syntax(partial_mod)
            if syntax_info == self.complete:
                # multiline inputs are encoded to one line
                partial_mod = line_encode(partial_mod)
                self.clear()
                return (syntax_info, partial_mod)
        # need to pull off syntactic sugar ; to see if line failed the syntax
        # check because of syntax_error, or because of incomplete
        return self.check_syntax(partial_mod[:-1])

    def clear(self):
        self.partial = ""

class ProblemStartingPhp(Exception):
    def __init__(self, file_name = None, line_num = None):
        self.file_name = file_name
        self.line_num = line_num

def until_paren_close_balanced(s):
    lparens = 1
    for i in range(len(s)):
        if s[i] == '(':
            lparens += 1
        elif s[i] == ')':
            lparens -= 1
        if lparens == 0:
            return s[:i]
    return s

class PhpshState:
    """This doesn't perfectly encapsulate state (e.g. the readline module has
    global state), but it is a step in the
    right direction and it already fulfills its primary objective of
    simplifying the notion of throwing a line of input (possibly only part of a
    full php line) at phpsh.
    """

    phpsh_root = os.path.dirname(os.path.realpath(__file__))

    php_prompt = "php> "
    php_more_prompt = " ... "

    no_command = "no_command"
    yes_command = "yes_command"
    quit_command = "quit_command"

    def __init__(self, cmd_incs, do_color, do_echo, codebase_mode,
            do_autocomplete, do_ctags, interactive):
        """start phpsh.php and do other preparations (colors, ctags)
        """

        self.do_echo = do_echo
        self.temp_file_name = tempfile.mktemp()
        self.comm_base = "php " + self.phpsh_root + "/phpsh.php " + \
            self.temp_file_name + " " + cu.arg_esc(codebase_mode)
        if not do_color:
            self.comm_base += " -c"
        if not do_autocomplete:
            self.comm_base += " -A"
        self.cmd_incs = cmd_incs

        # so many colors, so much awesome
        if not do_color:
            self.clr_cmd = ""
            self.clr_err = ""
            self.clr_help = ""
            self.clr_announce = ""
            self.clr_default = ""
        else:
            self.clr_cmd = clr.Green
            self.clr_err = clr.Red
            self.clr_help = clr.Green
            self.clr_announce = clr.Magenta
            self.clr_default = clr.Default

        # ctags integration
        self.ctags = None
        if do_ctags and os.path.isfile("tags"):
            print self.clr_cmd + "Loading ctags" + self.clr_default
            try:
                import ctags
                self.ctags = ctags.Ctags()
                try:
                    self.function_signatures = \
                        ctags.CtagsFunctionSignatures().function_signatures
                except Exception, e:
                    self.function_signatures = {}
                    print self.clr_err + \
                        "Problem loading function signatures" + \
                        self.clr_default
            except Exception, e:
                print self.clr_err + "Problem loading ctags" + self.clr_default

        import rlcompleter
        input_rc_file = os.path.join(os.environ["HOME"], ".inputrc")
        if os.path.isfile(input_rc_file):
            readline.read_init_file(input_rc_file)
        readline.parse_and_bind("tab: complete")

        # persistent readline history
        # we set the history length to be something reasonable
        # so that we don't write a ridiculously huge file every time
        # someone executes a command
        home_phpsh_dir = os.path.join(os.environ["HOME"], ".phpsh")
        if not os.path.exists(home_phpsh_dir):
            os.mkdir(home_phpsh_dir)
        self.history_file = os.path.join(home_phpsh_dir, "history")
        readline.set_history_length(100)

        try:
            readline.read_history_file(self.history_file)
        except IOError:
            # couldn't read history (probably one hasn't been created yet)
            pass

        self.autocomplete_identifiers = []
        self.autocomplete_cache = None
        self.autocomplete_match = None
        self.autocomplete_signature = None

        self.show_incs(start=True)
        self.php_open_and_check()

        def tab_complete(text, state):
            """The completer function is called as function(text, state),
            for state in 0, 1, 2, ..., until it returns a non-string value."""

            if not text:
                # currently there is a segfault in readline when you complete
                # on nothing.  so just don't allow completing on that for now.
                # in the long term, we may use ipython's prompt code instead
                # of readline
                return None
            if state == 0:
                self.autocomplete_cache = []
                for identifier in self.autocomplete_identifiers:
                    if identifier.startswith(text):
                        self.autocomplete_cache.append(identifier)

                if self.function_signatures.has_key(text):
                    for sig in self.function_signatures[text]:
                        func_str = sig[1]
                        if func_str[-1] == ',':
                            file_contents = ''.join(
                                l[:-1] for l in file(sig[0]).readlines())
                            # this is not perfect but it should be good enough
                            look_for = 'function ' + text + '('
                            i = file_contents.find(look_for)
                            if i != -1:
                                i_paren = func_str.find('(')
                                if i_paren != -1:
                                    func_str = func_str[:i_paren + 1]
                                i_end = i + len(look_for)
                                s = until_paren_close_balanced(
                                    file_contents[i_end:])
                                s = re.sub(', +', ', ', s, 1000)
                                func_str += s + ')'
                        self.autocomplete_cache.append(func_str)
            try:
                return self.autocomplete_cache[state]
            except IndexError:
                return None

        readline.set_completer(tab_complete)

        # print welcome message
        if interactive:
            print self.clr_help + \
                "type 'h' or 'help' to see instructions & features" + \
                self.clr_default

    def do_expr(self, expr):
        self.p.stdin.write(expr)
        self.wait_for_comm_finish()
        return self.result

    def wait_on_ready(self):
        while True:
            a = self.comm_file.readline()
            if a:
                break
            time.sleep(comm_poll_timeout)

    def php_open_and_check(self):
        self.p = None
        while not self.p:
            try:
                self.php_open()
            except ProblemStartingPhp, e:
                print self.clr_cmd + """phpsh failed to initialize PHP.
Fix the problem and hit enter to reload or ctrl-C to quit."""
                if e.line_num:
                    print "Type V to vim to %s: %s" % (e.file_name, e.line_num)
                    print self.clr_default
                    if raw_input() == "V":
                        Popen("vim +" + str(e.line_num) + " " + e.file_name,
                            shell=True).wait()
                else:
                    print self.clr_default
                    raw_input()
        # this file is how phpsh.php tells us it is done with a command
        self.comm_file = open(self.temp_file_name)
        self.wait_on_ready()
        self.wait_for_comm_finish()

    def php_restart(self):
        self.initialized_successfully = False
        try:
            self.p.stdout.close()
            self.p.stderr.close()
            self.p.stdin.close()
            self.p.wait()
        except IOError:
            pass

        return self.php_open_and_check()

    def php_open(self):
        self.autocomplete_identifiers = []
        cmd = " ".join([self.comm_base] + list(self.cmd_incs))
        self.p = Popen(cmd, shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE,
            preexec_fn=os.setsid)
        p_line = self.p.stdout.readline().rstrip()
        if p_line != "#start_autocomplete_identifiers":
            err_lines = self.p.stderr.readlines();
            if len(err_lines) >= 1:
                err_str = err_lines[-1].rstrip()
            else:
                err_str = "UNKNOWN ERROR (maybe php build does not support signals/tokenizer?)"
            print self.clr_err + err_str + self.clr_default
            m = re.match("PHP Parse error: .* in (.*) on line ([0-9]*)", err_str)
            if m:
                file_name, line_num = m.groups()
                raise ProblemStartingPhp(file_name, line_num)
            else:
                raise ProblemStartingPhp()
        while True:
            p_line = self.p.stdout.readline().rstrip()
            if p_line == "#end_autocomplete_identifiers":
                break
            self.autocomplete_identifiers.append(p_line)

    def wait_for_comm_finish(self):
        try:
            # wait for signal that php command is done
            # keep checking for death
            out_buff = ["", ""]
            buffer_size = 4096
            self.result = ""
            died = False

            debug = False
            #debug = True

            while True:
                if debug:
                    print 'polling'
                ret_code = self.p.poll()
                if debug:
                    print 'ret_code: ' + str(ret_code)
                if ret_code != None:
                    if debug:
                        print 'NOOOOO'
                    died = True
                    break
                while not died:
                    # line-buffer stdout and stderr
                    if debug:
                        print 'start loop'
                    s = select.select([self.p.stdout, self.p.stderr], [], [],
                        comm_poll_timeout)
                    if s == ([], [], []):
                        if debug:
                            print 'empty'
                        break
                    if debug:
                        print s[0]
                    for r in s[0]:
                        if r is self.p.stdout:
                            out_buff_i = 0
                        else:
                            out_buff_i = 1
                        buff = os.read(r.fileno(), buffer_size)
                        if not buff:
                            # process has died
                            died = True
                            break
                        out_buff[out_buff_i] += buff
                        last_nl_pos = out_buff[out_buff_i].rfind('\n')
                        if last_nl_pos != -1:
                            l = out_buff[out_buff_i][:last_nl_pos + 1]
                            self.result += l
                            if self.do_echo:
                                if r is self.p.stdout:
                                    sys.stdout.write(l)
                                else:
                                    l = self.clr_err + l + self.clr_default
                                    sys.stderr.write(l)
                            out_buff[out_buff_i] = out_buff[out_buff_i][last_nl_pos + 1:]
                # don't sleep if the command is already done
                # (even tho sleep period is small; maximize responsiveness)
                if self.comm_file.readline():
                    break
                time.sleep(comm_poll_timeout)

            if died:
                self.show_incs("PHP died. ")
                self.php_open_and_check()

        except KeyboardInterrupt:
            self.show_incs("Interrupt! ")
            self.php_restart()

    def show_incs(self, pre_str="", restart=True, start=False):
        s = self.clr_cmd + pre_str
        inc_str = str(list(self.cmd_incs))
        if start or restart:
            if start:
                start_word = "Starting"
            else:
                start_word = "Restarting"
            if self.cmd_incs:
                s += start_word + " php with extra includes: " + inc_str
            else:
                s += start_word + " php"
        else:
            s += "Extra includes are: " + inc_str
        print s + self.clr_default

    def try_command(self, line):
        if line == "r" or line.startswith("r "):
            # add args to phpsh.php (includes), reload
            self.cmd_incs = self.cmd_incs.union(inc_args(line[2:]))
            self.show_incs()
            self.php_restart()
        elif line == "R" or line.startswith("R "):
            # change args to phpsh.php (includes), reload
            self.cmd_incs = inc_args(line[2:])
            self.show_incs()
            self.php_restart()
        elif line == "c" or line.startswith("c "):
            # add args to phpsh.php (includes)
            self.cmd_incs = self.cmd_incs.union(inc_args(line[2:]))
            self.show_incs(restart=False)
            self.p.stdin.write("\n")
        elif line == "C" or line.startswith("C "):
            # change args to phpsh.php (includes)
            self.cmd_incs = inc_args(line[2:])
            self.show_incs(restart=False)
            self.p.stdin.write("\n")
        elif line.startswith("d ") or line.startswith("D "):
            identifier = line[2:]
            if identifier.startswith("$"):
                identifier = identifier[1:]

            print self.clr_help

            lookup_tag = False
            ctags_error = "ctags not enabled"
            try:
                if self.ctags:
                    tags = self.ctags.py_tags[identifier]
                    ctags_error = None
                    lookup_tag = True
            except KeyError:
                ctags_error = "no ctag info found for '" + identifier + "'"
            if lookup_tag:
                print repr(tags)
                for t in tags:
                    try:
                        file = self.ctags.tags_root + os.path.sep + t["file"]
                        doc = ""
                        append = False
                        line_num = 0
                        for line in open(file):
                            line_num += 1
                            if not append:
                                if line.find("/*") != -1:
                                    append = True
                                    doc_start_line = line_num
                            if append:
                                if line.find(t["context"]) != -1:
                                    print "%s, lines %d-%d:" % (file, doc_start_line, line_num)
                                    print doc
                                    break
                                if line.find("*") == -1:
                                    append = False
                                    doc = ""
                                else:
                                    doc += line
                    except:
                        pass
            import manual
            manual_ret = manual.get_documentation_for_identifier(identifier,
                short=line.startswith("d "))
            if manual_ret:
                print manual_ret
            if not manual_ret and ctags_error:
                print "could not find in php manual and " + ctags_error
            print self.clr_default
        elif line.startswith("v "):
            self.editor_tag(line[2:], "vim", read_only=True)
        elif line.startswith("V "):
            self.editor_tag(line[2:], "vim")
        elif line.startswith("e "):
            self.editor_tag(line[2:], "emacs")
        elif line.startswith("!"):
            # shell command
            Popen(line[1:], shell=True).wait()
        elif line == "h" or line == "help":
            print self.clr_help + help_message() + self.clr_default
        elif line == "q" or line == "exit" or line == "exit;":
            return self.quit_command
        else:
            return self.no_command
        return self.yes_command

    def editor_tag(self, tag, editor, read_only=False):
        if tag.startswith("$"):
            tag = tag[1:]

        def not_found():
            print self.clr_cmd + "no tag '" + tag + "' found" + self.clr_default
            self.p.stdin.write("\n")

        if not self.ctags.py_tags.has_key(tag):
            not_found()
            return

        if editor == "emacs":
            t = self.ctags.py_tags[tag][0]
            # get line number (or is there a way to start emacs at a
            # particular tag location?)
            try:
                file = self.ctags.tags_root + os.path.sep + t["file"]
                doc = ""
                append = False
                line_num = 1
                found_tag = False
                for line in open(file):
                    line_num += 1
                    if line.find(t["context"]) != -1:
                        emacs_line = line_num
                        found_tag = True
                        break
            except:
                pass
            if found_tag:
                # -nw opens it in the terminal instead of using X
                cmd = "emacs -nw +%d %s" % (emacs_line, file)
                p_emacs = Popen(cmd, shell=True)
                p_emacs.wait()
                self.p.stdin.write("\n")
            else:
                not_found()
                return
        else:
            if read_only:
                vim = "vim -R"
            else:
                vim = "vim"
            vim += ' -c "set tags=' + self.ctags.tags_file + '" -t '
            p_vim = Popen(vim + tag, shell=True)
            p_vim.wait()
            self.p.stdin.write("\n")
            if not read_only:
                self.show_incs()
                self.php_open_and_check()

    def write(self):
        try:
            readline.write_history_file(self.history_file)
        except IOError:
            print >> sys.stderr, \
                "Could not write history file.  No write permissions?"

    def close(self):
        self.write()
        print self.clr_default
        os.remove(self.temp_file_name)
