# utility functions for running shell commands

import re
import subprocess as spc
import threading as thg
import sys
import time

PIPE = spc.PIPE

def multi_sub(reps, s):
    """multi-substitute: do replacements (dictionary of strings to strings)
       simultaneously
    """
    i = 0
    ret = ""
    for m in re.finditer("(" + "|".join(map(re.escape, reps.keys())) + ")", s):
        ret += s[i:m.start()] + reps[m.group()]
        i = m.end()
    ret += s[i:]
    return ret

# the simpler way:
#   s.replace("\\", "\\\\").replace("'", "'\\''")
# does _not_ work when there are backslashes on the scene:
# you need simultaneous substitution bc \ inside ' is weird in shell-scripting
# (this really does come up, such double escaping for dsh e.g.)
arg_esc_subs = {
    "'": "'\\''",
    "\\": "'\\\\'"
}

def arg_esc(s):
    """escape an argument for a shell command"""
    return "'" + multi_sub(arg_esc_subs, s) + "'"

def args_esc(ss):
    """escape arguments for a shell command"""
    return tuple([arg_esc(s) for s in ss])

def error_out(s):
    raise Exception(s + " ..aborting!")

def cmd_run(cmd, shell=True, stdout=None, stdin=None, stderr=None):
    """
    run a command, applying escaping properly on array.
    basically this does what Popen should do already
    (Popen is wontfix busted on unix for passing multiple args as array,
    since it just does bash -c "$@" which silently kills args.  pretty lame)
    """
    if type(cmd) == type([]):
        cmd = " ".join([arg_esc(a) for a in cmd])
    return spc.Popen(cmd, shell=shell, stdout=stdout, stdin=stdin,
        stderr=stderr)

def cmd_wait(cmd, shell=True, stdout=None, stderr=None, can_fail=False):
    """wait for a command to finish and return the subprocess object"""
    p = cmd_run(cmd, shell=shell, stdout=stdout, stderr=stderr)
    ret = p.wait()
    if not can_fail and ret != 0:
        error_out("got ret " + str(ret) + " from command:\n" + str(cmd))
    return p

def cmd_output(cmd, can_fail=False):
    """wait for a command to finish and return the output"""
    ls = cmd_wait(cmd, stdout=spc.PIPE, can_fail=can_fail).stdout.readlines()
    return [l[:-1] for l in ls]

def are_you_sure(timeout=5, msg=""):
    print "WARNING: YOU ARE PERFORMING A POTENTIALLY DANGEROUS ACTION"
    if msg:
        print
        print msg
        print
    print "waiting " + str(timeout) + " seconds before continuing."
    print "^C TO CANCEL"
    timeout_list = range(1, timeout + 1)
    timeout_list.reverse()
    for i in timeout_list:
        print i
        time.sleep(1)

def try_rep(n, cmd):
    """try a command repeatedly until it works,
       erroring after some number of failures
    """
    for i in xrange(n):
        if i > 0:
            print >> sys.stderr, "retrying " + cmd
        ret = spc.Popen(cmd, shell=True).wait()
        if ret == 0:
            return ret
    print >> sys.stderr,  "***** FAILED ***** (with ret %d): %s" % (ret, cmd)
    return ret

default_fanout = 5
# note this to retry the ssh connection/execution
#   not to retry your command, e.g.: dsh(hosts, 'false') will only be tried
#   once on each host, even though false "fails" every time it is run
default_retry_num = 10

def dsh(hosts, cmd, fanout=default_fanout, retry_num=default_retry_num):
    """runs a command on several boxes via ssh"""
    def my_cmd_fcn(x):
        return "ssh " + x + " " + cu.arg_esc(cmd)
    dsh_fcn(hosts, my_cmd_fcn, fanout, retry_num)

def dsh_fcn(hosts, cmd_fcn, fanout=default_fanout,
        retry_num=default_retry_num):
    """very generic form of dsh function"""
    print "Runinng " + cu.arg_esc(cmd_fcn("<x>")) + ":" + \
        "\n- with fanout " + str(fanout) + " and retry " + str(retry_num) + \
        "\n- on <x> in " + str(hosts)

    fanout_sema = thg.BoundedSemaphore(value=fanout)

    class ShellCommThread(thg.Thread):
        def __init__(self, cmd, retry_num):
            thg.Thread.__init__(self)
            self.cmd = cmd
            self.retry_num = retry_num
        def run(self):
            fanout_sema.acquire()
            try:
                spc.Popen(self.cmd, shell=True).wait()
            finally:
                fanout_sema.release()

    threads = []
    for host in hosts:
        t = ShellCommThread(cmd_fcn(host), retry_num)
        threads.append(t)
        t.start()
    for t in threads:
        t.join()

def sql_esc(s):
    return '"' + s.replace('"', '\"').replace('\\', '\\\\') + '"'

def sql_like_esc(s):
    return s.replace('%', '\\%').replace('_', '\\_')
