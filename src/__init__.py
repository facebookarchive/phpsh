__version__ = "1.3"
__author__ = "phpsh@googlegroups.com"
__date__ = "Nov 20, 2008"

from subprocess import Popen, PIPE, call
from threading import Thread
from bisect import bisect
import ansicolor as clr
import cmd_util as cu
import ctags
import ConfigParser
import os
import re
import readline
import select
import signal
import sys
import tempfile
import time

comm_poll_timeout = 0.01

PHP_RESERVED_WORDS = [
    # Include reserved words from
    # http://us3.php.net/manual/en/reserved.keywords.php
    "abstract",
    "and",
    "array",
    "as",
    "break",
    "case",
    "catch",
    "cfunction",
    "class",
    "clone",
    "const",
    "else",
    "continue",
    "declare",
    "default",
    "do",
    "else",
    "elseif",
    "enddeclare",
    "endfor",
    "endforeach",
    "endif",
    "endswitch",
    "endwhile",
    "extends",
    "final",
    "for",
    "foreach",
    "function",
    "global",
    "goto", # (!)
    "if",
    "implements",
    "interface",
    "instanceof",
    "namespace",
    "new",
    "old_function",
    "or",
    "private",
    "protected",
    "public",
    "static",
    "switch",
    "throw",
    "try",
    "use",
    "var",
    "while",
    "xor",
    "__CLASS__",
    "__DIR__",
    "__FILE__",
    "__FUNCTION__",
    "__METHOD__",
    "__NAMESPACE__",
    "die",
    "echo",
    "empty",
    "exit",
    "eval",
    "include",
    "include_once",
    "isset",
    "list",
    "require",
    "require_once",
    "return",
    "print",
    "unset",
    ]


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
    x [=]function([args]) Execute function() with args under debugger
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
    return cu.multi_sub({"\n": "\\n", "\\": "\\\\"}, line) + "\n"

def inc_args(s):
    """process a string of includes to a set of them"""
    return set([inc.strip() for inc in s.split(" ") if inc.strip()])

def xdebug_loaded():
    """checks if Xdebug is already loaded"""
    try:
        retcode = call("php -m | grep Xdebug &> /dev/null", shell=True)
        return retcode == 0
    except OSError:
        return False

def get_php_ext_path():
   extension_dir = Popen("php-config | grep extension-dir",
                         shell=True, stdout=PIPE, stderr=PIPE).communicate()[0]
   if extension_dir:
      lbr = extension_dir.find("[")
      rbr = extension_dir.find("]")
      if 0 < lbr < rbr:
         return extension_dir[lbr+1:rbr]

def sigalrm_handler(sig, frame):
   raise OSError, "Alarm"


class PhpMultiliner:
    """This encapsulates the process and state of intaking multiple input lines
    until a complete php expression is formed, or detecting a syntax error.

    Note: this is not perfectly encapsulated while the parser has global state
    """

    complete = "complete"
    incomplete = "incomplete"
    syntax_error = "syntax_error"

    def __init__(self):
        self.partial = ""

    def check_syntax(self, line):
        p = Popen(["php", "-r", "return;" + line], stdout=PIPE, stderr=PIPE)
        p.wait()
        # "php -r" lint errors seem to only use stdout, but it might (idk)
        # depend on configuration or change later, so just grab everything.
        ls = p.stdout.readlines() + p.stderr.readlines()
        # hack so that missing extensions etc don't halt all phpsh use.
        # these php startup errors will still show at phpsh start up.
        ls = [l for l in ls if l.find("PHP Startup:") == -1]
        l = "".join(ls)
        if l:
            if l.find("unexpected $end") != -1 or l.find("unexpected end") != -1:
                return (self.incomplete, "")
            return (self.syntax_error, l)
        return (self.complete, "")

    def input_line(self, line):
        if self.partial:
            self.partial += "\n"
        self.partial += line
        partial_mod = do_sugar(self.partial)
        if not partial_mod:
            return (self.complete, "")

        (syntax_info, _) = self.check_syntax(partial_mod)
        if syntax_info == self.complete:
            # Multiline inputs are encoded to one line.
            partial_mod = line_encode(partial_mod)
            self.clear()
            return (syntax_info, partial_mod)

        # We need to pull off the syntactic sugar ; to see if the line failed
        # the syntax check because of syntax_error, or because of incomplete.
        return self.check_syntax(partial_mod[:-1])

    def clear(self):
        self.partial = ""

class ProblemStartingPhp(Exception):
    def __init__(self,
                 file_name=None,
                 line_num=None,
                 stdout_lines=None,
                 stderr_lines=None):
        self.file_name = file_name
        self.line_num = line_num
        self.stdout_lines = stdout_lines
        self.stderr_lines = stderr_lines

class PhpshConfig:
    def __init__(self):
        self.config = ConfigParser.RawConfigParser({
            "UndefinedFunctionCheck": "yes",
            "Xdebug"          : None,
            "DebugClient"     : "emacs",
            "ClientTimeout"   : 60,
            "ClientHost"      : "localhost",
            "ClientPort"      : None,
            "ProxyPort"       : None,
            "Help"            : "no",
            "LogDBGp"         : "no",
            "ForegroundColor" : "black",
            "BackgroundColor" : "white",
            "InactiveColor"   : "grey75",
            "InactiveMinimize": "yes",
            "FontFamily"      : None,
            "FontSize"        : None,
            "XdebugClientPath": "debugclient",
            "X11"             : "yes"})
        self.config.add_section("General")
        self.config.add_section("Debugging")
        self.config.add_section("Emacs")

    def read(self):
        config_files = ["/etc/phpsh/config"]
        home = os.getenv("HOME")
        if home:
            homestr = home.strip()
            if homestr:
                config_files.append(os.path.join(homestr, ".phpsh/config"))
        self.config.read(config_files)
        return self.config

    def get_option(self, s, o):
        if self.config.has_option(s, o):
            return self.config.get(s, o)
        else:
            return None

def until_paren_close_balanced(s):
    lparens = 1
    for i in range(len(s)):
        if s[i] == "(":
            lparens += 1
        elif s[i] == ")":
            lparens -= 1
        if lparens == 0:
            return s[:i]
    return s

class LoadCtags(Thread):
    def __init__(self, phpsh_state):
        Thread.__init__(self)
        self.phpsh_state = phpsh_state
        self.phpsh_state.function_signatures = {}
    def run(self):
        try:
           tags_file_path = None
           try:
              tags_file_path = ctags.find_tags_file()
           except ctags.CantFindTagsFile, e:
              return

           print self.phpsh_state.clr_cmd + \
               "Loading ctags (in background)" + \
               self.phpsh_state.clr_default
           self.phpsh_state.ctags = ctags.Ctags(tags_file_path)
           try:
              self.phpsh_state.function_signatures = \
                  ctags.CtagsFunctionSignatures().function_signatures
           except Exception, e:
              print self.phpsh_state.clr_err + \
                  "Problem loading function signatures" + \
                  self.phpsh_state.clr_default
        except Exception, e:
           if tags_file_path:
              path = tags_file_path
           else:
              path = ""
           print self.phpsh_state.clr_err + \
               "Problem loading ctags %(path)s\n(%(e)s)\n" % locals() + \
               self.phpsh_state.clr_default

class PhpshState:
    """This doesn't perfectly encapsulate state (e.g. the readline module has
    global state), but it is a step in the
    right direction and it already fulfills its primary objective of
    simplifying the notion of throwing a line of input (possibly only part of a
    full php line) at phpsh.
    """

    php_prompt = "php> "
    php_more_prompt = " ... "

    no_command = "no_command"
    yes_command = "yes_command"
    quit_command = "quit_command"
    debug_command = "x "

    def __init__(self, cmd_incs, do_color, do_echo, codebase_mode,
            do_autocomplete, do_ctags, interactive, with_xdebug, verbose):
        """start phpsh.php and do other preparations (colors, ctags)
        """

        self.phpsh_root = os.path.dirname(os.path.realpath(__file__))

        self.do_echo = do_echo
        self.p_dbgp = None; # debugging proxy
        self.dbgp_port = 9000; # default port on which dbgp proxy listens
        self.temp_file_name = tempfile.mkstemp()[1]
        self.output_tempfile = None # tempfile to buffer php output
        self.with_xdebug = with_xdebug;
        self.verbose = verbose
        self.xdebug_path = None # path to xdebug.so read from config file
        self.xdebug_disabled_reason = None # why debugging was disabled
        self.to_dbgp = None   # fds of pipe endpoints for writing commands
        self.from_dbgp = None # to dbgp proxy and reading replies

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

        self.config = PhpshConfig()
        try:
           self.config.read()
        except Exception, msg:
           self.print_error("Failed to load config file, using default "\
                            "settings: " + str(msg))
        if self.with_xdebug:
            xdebug = self.config.get_option("Debugging", "Xdebug")
            if xdebug and xdebug != "yes":
                if xdebug == "no":
                    self.with_xdebug = False
                    self.xdebug_disabled_reason = \
                        "Xdebug is set to 'no' in config file"
                else:
                    self.xdebug_path = xdebug

        self.comm_base = ["php"]

        if self.with_xdebug:
            xdebug_comm_base = self.comm_base[:]
            if not xdebug_loaded():
                php_ext_dir = get_php_ext_path()
                if php_ext_dir:
                    if not self.xdebug_path:
                        self.xdebug_path = php_ext_dir + "/xdebug.so"
                    try:
                        os.stat(self.xdebug_path)
                        xdebug_comm_base += ["-d"]
                        extension = "zend_extension"
                        if php_ext_dir.find("php/extensions/debug") >= 0:
                            extension += "_debug"
                        extension += "=\"" + self.xdebug_path + "\""
                        xdebug_comm_base += [extension]
                        # The following is a workaround if role.ini is overly
                        # restrictive.  role.ini currently sets max nesting
                        # level to 50 at facebook.
                        xdebug_comm_base += ["-d",
                                             "xdebug.max_nesting_level=500"]
                        try:
                            xdebug_version = self.get_xdebug_version(
                                xdebug_comm_base)
                            if xdebug_version < [2, 0, 3]:
                                self.xdebug_disabled_reason = "\
Xdebug version %s is too low.  xdebug-2.0.3 or above required." % \
xdebug_version
                                self.with_xdebug = False
                        except Exception, msg:
                            self.xdebug_disabled_reason = self.xdebug_path + \
                                " is incompatible with your php build"
                            self.with_xdebug = False
                    except OSError:
                        self.xdebug_disabled_reason = \
                            "xdebug.so not found, tried " + self.xdebug_path
                        self.with_xdebug = False
                        self.xdebug_path = None
                else:
                    self.xdebug_disabled_reason = """\
Could not identify PHP extensions directory.
Make sure php-config is in your PATH."""
                    self.with_xdebug = False
                    self.xdebug_path = None
                if self.verbose and not self.with_xdebug and \
                        self.xdebug_disabled_reason:
                    self.print_warning("PHP debugging will be disabled:\n" +
                        self.xdebug_disabled_reason)
        if self.with_xdebug:
            self.comm_base = xdebug_comm_base
            self.start_xdebug_proxy()

        self.comm_base += [self.phpsh_root + "/phpsh.php",
                           self.temp_file_name,
                           codebase_mode]
        if not do_color:
            self.comm_base += ["-c"]
        if not do_autocomplete:
            self.comm_base += ["-A"]
        if self.config.get_option("General", "UndefinedFunctionCheck") == "no":
            self.comm_base += ["-u"]
        if not self.with_xdebug:
            self.comm_base += ["-f"]
        self.cmd_incs = cmd_incs

        # ctags integration
        self.ctags = None
        if do_ctags:
           LoadCtags(self).start()
        else:
            self.function_signatures = {}

        import rlcompleter
        input_rc_file = os.path.join(os.environ["HOME"], ".inputrc")
        if os.path.isfile(input_rc_file):
            readline.parse_and_bind(open(input_rc_file).read())
        readline.parse_and_bind("tab: complete")

        # persistent readline history
        # we set the history length to be something reasonable
        # so that we don't write a ridiculously huge file every time
        # someone executes a command
        home_phpsh_dir = os.path.join(os.environ["HOME"], ".phpsh")
        if not os.path.exists(home_phpsh_dir):
            os.mkdir(home_phpsh_dir)
        self.history_file = os.path.join(home_phpsh_dir, "history")
        readline.set_history_length(1000)

        try:
            readline.read_history_file(self.history_file)
        except IOError:
            # couldn't read history (probably one hasn't been created yet)
            pass

        self.autocomplete_identifiers = sorted(PHP_RESERVED_WORDS)

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
                pos = bisect(self.autocomplete_identifiers, text)

                while self.autocomplete_identifiers[pos].startswith(text):
                    identifier = self.autocomplete_identifiers[pos]
                    self.autocomplete_cache.append(identifier)
                    pos = pos + 1

                if self.function_signatures.has_key(text):
                    for sig in self.function_signatures[text]:
                        func_str = sig[1]
                        if func_str[-1] == ",":
                            file_contents = "".join(
                                l[:-1] for l in file(sig[0]).readlines())
                            # this is not perfect but it should be good enough
                            look_for = "function " + text + "("
                            i = file_contents.find(look_for)
                            if i != -1:
                                i_paren = func_str.find("(")
                                if i_paren != -1:
                                    func_str = func_str[:i_paren + 1]
                                i_end = i + len(look_for)
                                s = until_paren_close_balanced(
                                    file_contents[i_end:])
                                s = re.sub(", +", ", ", s, 1000)
                                func_str += s + ")"
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

    def get_xdebug_version(self, comm_base):
        p = Popen(comm_base + ["-r", "phpinfo();"], stdout=PIPE, stderr=PIPE)
        out, err = p.communicate()
        if p.returncode is not 0:
            raise Exception, "Failed to load Xdebug\n" + err
        m = re.compile(" *with Xdebug v([0-9.]+)").search(out)
        if not m:
            raise Exception, \
                "Could not find xdebug version number in phpinfo() output"
        try:
            return [int(s) for s in m.group(1).strip(".").split(".")]
        except ValueError:
            raise ValueError, "invalid Xdebug version format: " + m.group(1)

    def start_xdebug_proxy(self):
        try:
            to_r, to_w = os.pipe()
            from_r, from_w = os.pipe()
            dbgp_py = ["dbgp-phpsh.py",
                str(to_r), str(to_w), str(from_r), str(from_w)]
            self.p_dbgp = Popen(dbgp_py)
            os.close(to_r)
            os.close(from_w)
            self.to_dbgp = os.fdopen(to_w, "w", 0)
            self.from_dbgp = os.fdopen(from_r, "r", 0)
            try:
                dbgp_status = self.from_dbgp.readline()
                if dbgp_status.startswith("initialized"):
                    r = re.compile(".*port=([0-9]+).*")
                    m = r.match(dbgp_status)
                    if m:
                        self.dbgp_port = m.group(1)
                else:
                    self.to_dbgp.close()
                    self.from_dbgp.close()
                    self.p_dbgp = None
                    self.with_xdebug = False
                    self.xdebug_disabled_reason = "xdebug proxy " + dbgp_status
            except Exception, msg:
                self.print_error("\
Could not obtain initialization status from xdebug proxy: %s" % msg)
                self.to_dbgp.close()
                self.from_dbgp.close()
                self.p_dbgp = None
                self.with_xdebug = False
        except Exception, msg:
            self.print_error("Failed to start xdebug proxy: " + str(msg))
            self.with_xdebug = False
        if self.verbose and not self.with_xdebug and \
                self.xdebug_disabled_reason:
            self.print_warning("PHP debugging will be disabled:\n" +
                self.xdebug_disabled_reason)

    def print_error(self, msg):
        print self.clr_err + msg + self.clr_default

    def print_warning(self, msg):
        print self.clr_announce + msg + self.clr_default

    # pass expr to php for evaluation, wait for completion
    # if debug_funcall is True, the expression is being run under
    # debugger.
    def do_expr(self, expr, debug_funcall=False):
        defer_output = debug_funcall and (not os.getenv("DISPLAY") or \
            self.config.get_option("Debugging", "X11") == "no")
        # if we are executing a function call under debugger and debug
        # client is running in the same terminal as phpsh, do not print
        # the output we get from php in terminal until evaluation is done
        # and debug client exits, so that we do not mess up the debug
        # client UI.
        self.p.stdin.write(expr)
        self.wait_for_comm_finish(defer_output)
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
                self.end_process()

                print(self.clr_cmd + """\
phpsh failed to initialize PHP.
Fix the problem and hit enter to reload or ctrl-C to quit.""")

                print self.clr_err
                print "".join(e.stdout_lines + e.stderr_lines)

                if e.line_num:
                    print("\
Type 'e' to open emacs or 'V' to open vim to %s: %s" %
(e.file_name, e.line_num))
                    print self.clr_default

                    response = raw_input()
                    if response == "V":
                        editor = "vim"
                    elif response == "e":
                        editor = "emacs -nw"
                    else:
                        editor = ""

                    if editor != "":
                        Popen(editor + " +" + str(e.line_num) + " " +
                              e.file_name, shell=True).wait()

                else:
                    print self.clr_default
                    raw_input()

        # this file is how phpsh.php tells us it is done with a command
        self.comm_file = open(self.temp_file_name)
        self.wait_on_ready()
        self.wait_for_comm_finish()

    def php_restart(self):
        if self.with_xdebug and self.p_dbgp:
            self.to_dbgp.write("run php\n")

        self.initialized_successfully = False
        self.end_process()

        return self.php_open_and_check()

    def php_open(self):
        self.autocomplete_identifiers = sorted(PHP_RESERVED_WORDS)
        cmd = self.comm_base + list(self.cmd_incs)
        env = os.environ.copy()

        if self.with_xdebug:
           env["XDEBUG_CONFIG"] = \
               "remote_port=%(port)s remote_enable=1" % {"port": self.dbgp_port}

        self.p = Popen(cmd, env=env, stdin=PIPE, stdout=PIPE, stderr=PIPE,
                       preexec_fn=os.setsid)

        if self.with_xdebug:
            # disable remote debugging for other instances of php started by
            # this script, such as the multiline syntax verifyer
            os.putenv("XDEBUG_CONFIG", "remote_enable=0");

        p_line = self.p.stdout.readline().rstrip()

        if p_line != "#start_autocomplete_identifiers":
            err_lines = self.p.stderr.readlines()
            out_lines = self.p.stdout.readlines()

            parse_error_re = re.compile(
                "PHP Parse error: .* in (.*) on line ([0-9]*)")
            m = None
            for line in reversed(err_lines):
                m = parse_error_re.match(line)
                if m:
                    file_name, line_num = m.groups()
                    raise ProblemStartingPhp(file_name,
                                             line_num,
                                             stdout_lines=out_lines,
                                             stderr_lines=err_lines)

            raise ProblemStartingPhp(stdout_lines=out_lines,
                                    stderr_lines=err_lines)

        while True:
            p_line = self.p.stdout.readline().rstrip()
            if p_line == "#end_autocomplete_identifiers":
                break
            self.autocomplete_identifiers.append(p_line)
        self.autocomplete_identifiers.sort()

    def wait_for_comm_finish(self, defer_output=False):
        try:
            if defer_output:
                if self.output_tempfile:
                    self.output_tempfile.truncate(0)
                else:
                    self.output_tempfile = tempfile.TemporaryFile()
                out = self.output_tempfile
                err = self.output_tempfile
            else:
                out = sys.stdout
                err = sys.stderr
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
                    print "polling"
                ret_code = self.p.poll()
                if debug:
                    print "ret_code: " + str(ret_code)
                if ret_code != None:
                    if debug:
                        print "NOOOOO"
                    print "subprocess died with return code: " + repr(ret_code)
                    died = True
                    break
                while not died:
                    # line-buffer stdout and stderr
                    if debug:
                        print "start loop"
                    s = select.select([self.p.stdout, self.p.stderr], [], [],
                        comm_poll_timeout)
                    if s == ([], [], []):
                        if debug:
                            print "empty"
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
                            died = True
                            break
                        out_buff[out_buff_i] += buff
                        last_nl_pos = out_buff[out_buff_i].rfind("\n")
                        if last_nl_pos != -1:
                            l = out_buff[out_buff_i][:last_nl_pos + 1]
                            self.result += l
                            if self.do_echo:
                                if r is self.p.stdout:
                                    out.write(l)
                                else:
                                    l = self.clr_err + l + self.clr_default
                                    err.write(l)
                            out_buff[out_buff_i] = \
                                out_buff[out_buff_i][last_nl_pos + 1:]
                # at this point either:
                #  the php instance died
                #  select timed out

                # read till the end of the file
                l = self.comm_file.readline()
                lastline = l
                while l.strip() != "":
                    l = self.comm_file.readline()
                    if l.strip() != "":
                        lastline = l
                l = lastline

                if l.startswith("child"):
                    ret_code = self.p.poll()
                    os.kill(self.p.pid, signal.SIGHUP)
                    self.p.pid = int(l.split()[1])
                elif l.startswith("ready"):
                    break
                time.sleep(comm_poll_timeout)

            if defer_output and self.output_tempfile.tell() > 0:
                self.output_tempfile.seek(0, os.SEEK_SET)
                for line in self.output_tempfile:
                    print line

            if died:
                self.show_incs("PHP died. ")
                self.php_open_and_check()

        except KeyboardInterrupt:
            self.show_incs("Interrupt! ")
            if defer_output and self.output_tempfile.tell() > 0:
               self.output_tempfile.seek(0, os.SEEK_SET)
               for line in self.output_tempfile: print line
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
                                    print ("%s, lines %d-%d:" %
                                        (file, doc_start_line, line_num))
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
        elif line.startswith("x "):
            if self.with_xdebug and self.p_dbgp:
                return PhpshState.debug_command
            else:
                self.print_warning("PHP debugging is disabled")
                if self.xdebug_disabled_reason:
                    self.print_warning(self.xdebug_disabled_reason)
                return PhpshState.yes_command
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


    # check if line is of the form "=?<function-name>(<args>?)"
    # if it is, send it to the DBGp proxy and if the proxy reports
    # that it is ready to start debugging, return True. Otherwise
    # return False.
    def setup_debug_client(self, funcall):
        # extract function name and optional leading "=" from line
        if funcall.startswith("return "):
            funcall = funcall[6:].lstrip()
        m = re.compile(" *([A-Za-z_][A-Za-z0-9_]*) *[(]").match(funcall)
        if not m:
            self.print_error("Invalid function call syntax")
            return False
        dbgp_cmd = "x " + m.group(1)
        try:
            self.to_dbgp.write(dbgp_cmd + "\n")
            # TODO: put a timeout on this:
            dbgp_reply = self.from_dbgp.readline()
            if dbgp_reply != "ready\n":
                self.print_error("xdebug proxy error: " + dbgp_reply)
                return False
        except Exception, msg:
            self.print_error("Failed to communicate with xdebug proxy, "\
                "disabling PHP debugging: " + str(msg))
            self.to_dbgp.close()
            self.from_dbgp.close()
            self.p_dbgp = None
            self.with_xdebug = False
            return False
        # return PHP code to pass to PHP for eval
        return True


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
        except IOError, e:
            print >> sys.stderr, \
                "Could not write history file %s: %s" % \
                (self.history_file, e)

    def close(self):
        self.write()
        print self.clr_default
        os.remove(self.temp_file_name)
        self.end_process(True)

    def end_process(self, alarm=False):
        # shutdown php, if it doesn't exit in 5s, kill -9
        if alarm:
            signal.signal(signal.SIGALRM, sigalrm_handler)
        # if we have fatal-restart prevention, the child proess can't be waited
        #  on since it's no longer a child of this process
        try:
            self.p.stdout.close()
            self.p.stderr.close()
            self.p.stdin.close()
            if alarm:
               signal.alarm(5)
            os.waitpid(self.p.pid, 0)
        except (IOError, OSError, KeyboardInterrupt):
            os.kill(self.p.pid, signal.SIGKILL)
            # collect the zombie
            try:
              os.waitpid(self.p.pid, 0)
            except (OSError):
              pass
        self.p = None

