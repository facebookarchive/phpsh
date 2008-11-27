#!/usr/bin/env python

from subprocess import Popen, PIPE
from select import poll, POLLIN, POLLHUP
from signal import SIGKILL
from phpsh import PhpshConfig
import xml.dom.minidom
import socket
import shlex
import time
import sys
import re
import os

"""This is a DBGp xdebug protocol proxy started by phpsh. It accepts a
connection from xdebug, connects to an IDE debug client and
communicates with its parent phpsh over stdin/out."""

__version__ = "1.0"
__author__ = "march@facebook.com"
__date__ = "Nov 05, 2008"

usage = "dbgp.py"

client_init_error_msg = """
Timed out while waiting for debug client for %ds. Make sure the client is
configured for PHP debugging and expects xdebug connections on port
%d. Client command was: %s"""


logfile = None
def debug_log(s):
    global tracing_enabled
    global logfile
    if not tracing_enabled:
        return
    if not logfile:
        logfile = open("dbgp.log", "a", 1)
        logfile.write('\n>>>>>>>>>>>>>>>>>>>>>>>\n\n')
    logfile.write(s+'\n\n')
    logfile.flush()


def stdin_closed():
    """Return True if stdin has HUP condition"""
    evset = poll()
    evset.register(0, POLLIN|POLLHUP)
    events = evset.poll(0)
    if events:
        fd, e = events[0]
        if e & POLLHUP:
            debug_log("stdin_closed(): detected HUP")
            return True
    return False


def flush_stdout(str):
    print str
    sys.stdout.flush()


def dbgp_get_filename(dbgp_response):
    """If dbgp_response is a dbgp <response> message with status='break' and
    'filename' attribute set, return the value of filename. Otherwise
    return None"""
    doc = xml.dom.minidom.parseString(dbgp_response)
    res = doc.getElementsByTagName("response")
    if res and res[0].getAttribute('status') == "break":
        msg = doc.getElementsByTagName("xdebug:message")
        if msg and msg[0].hasAttribute('filename'):
            return msg[0].getAttribute('filename')


def dbgp_get_txid(dbgp_response):
    doc = xml.dom.minidom.parseString(dbgp_response)
    res = doc.getElementsByTagName("response")
    if res:
        return res[0].getAttribute('transaction_id')


def dbgp_get_bpid(dbgp_response):
    """If dbgp_response is a response to 'breakpoint_set' with
    transaction_id=txid, return the value of id attribute as a string.
    Otherwise return None"""
    doc = xml.dom.minidom.parseString(dbgp_response)
    res = doc.getElementsByTagName("response")
    if res and res[0].getAttribute('command') == 'breakpoint_set':
      return res[0].getAttribute('id')


def xdebug_is_stopping(dbgp_response):
    doc = xml.dom.minidom.parseString(dbgp_response)
    res = doc.getElementsByTagName("response")
    return res and res[0].getAttribute("status") == "stopping"


def parse_port(portstr):
    if not portstr:
        return None
    try:
        port = int(portstr)
        if port < 0:
            raise ValueError, "Invalid port: " + portstr
        elif port == 0:
            port = None
    except ValueError:
        raise ValueError, "Invalid port: " + portstr
    return port


def parse_timeout(timeoutstr):
    if not timeoutstr:
        return None
    try:
        timeout = int(timeoutstr)
        if timeout <= 0:
            return None
    except ValueError:
        raise ValueError, "Invalid timeout: " + timeoutstr
    return timeout


def get_emacs_version():
    vline = Popen("emacs --version | head -n 1", shell=True,
                  stdout=PIPE, stderr=PIPE).communicate()[0]
    if not vline:
        raise OSError, "emacs not found. Make sure it's in your PATH."
    m = re.compile("GNU Emacs ([0-9.]+)").match(vline)
    if not m:
        raise ValueError, "could not parse emacs version: " + vline +\
                          "\nexpected GNU Emacs [0-9.]+"
    try:
        return [int(s) for s in m.group(1).strip('.').split('.')]
    except ValueError:
        raise ValueError, "invalid Emacs version format: " + m.group(1)


def get_debugclient_version(debugclient_path):
    vline = Popen(debugclient_path + " -v | head -n 1", shell=True,
                  stdout=PIPE, stderr=PIPE).communicate()[0]
    if not vline:
        raise OSError, "debugclient not found\nThis is a simple xdebug "\
              "protocol client distributed with xdebug\n"\
              "Make sure it's in your PATH."
    m = re.compile("Xdebug Simple DBGp client \(([0-9.]+)\)").match(vline)
    if not m:
        raise ValueError, "could not parse debugclient version: " + vline +\
              "\nexpected Xdebug Simple DBGp client ([0-9.]+)"
    try:
        return [int(s) for s in m.group(1).strip('.').split('.')]
    except ValueError:
        raise ValueError, "invalid debugclient version format: " + m.group(1)


class DebugClient:
    """Objects of this class represent instances of debug IDE clients"""

    def __init__(self, host, port, cmd, timeout):
        self.p_client = None # Popen to client
        self.conn = None     # DBGpConn to client
        self.lasttxid = None     # last txid seen from this client
        self.lastdbgpcmd = None  # name of last command read from client
        self.stopped = True  # never sent anything to this client, or
                             # last message was "stopped"
        self.host = host
        self.port = port
        self.cmd = cmd
        self.timeout = timeout
        self.connect()

    def connect(self):
        """Try to connect to self.host:self.port (if host is an empty string,
        connect to localhost). If can't connect and host is localhost,
        execute cmd and try to connect again until timeout. Raises
        socket.timeout if client is not up until timeout, OSError if
        client could not be started"""
        if self.conn and self.conn.isconnected():
            return # already connected
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            sock.connect((self.host, self.port))
        except socket.error, msg:
            if self.host != '' and self.host != 'localhost' \
                   and self.host != '127.0.0.1' or not self.cmd:
                # could not connect and client is not local or no command
                # to start a client. Propagate exception.
                raise socket.error, msg
            if not os.getenv('DISPLAY'):
                raise Exception, "X11 is required and DISPLAY is not set"
            # client is local, X display is set, try to start it
            self.p_client = Popen(self.cmd, close_fds=True)
            sock.settimeout(self.timeout)
            tstart = time.time()
            while True:
                try:
                    sock.connect(('', self.port))
                    sock.settimeout(None)
                    break
                except socket.error:
                    # failed to connect, likely client is not up yet
                    # keep trying
                    if self.timeout and time.time() - tstart > self.timeout:
                        debug_log(client_init_error_msg % (self.timeout,
                                                           self.port,
                                                           self.cmd))
                        self.close()
                        raise socket.timeout, \
                              client_init_error_msg % (self.timeout, self.cmd)
                    time.sleep(1)
        self.conn = DBGpConn(sock)

    def isconnected(self):
        return self.conn and self.conn.isconnected()

    def send_msg(self, msg):
        self.conn.send_msg(msg)
        self.stopped = False

    def recv_cmd(self):
        cmd = self.conn.recv_cmd()
        try:
            lcmd = cmd.split()
            i = lcmd.index("-i")
            self.lasttxid = lcmd[i+1]
            self.lastdbgpcmd = lcmd[0]
        except Exception, msg:
            debug_log("ERROR: did not find txid in command read from client: "\
                      + cmd)
        return cmd

    def get_sockfd(self):
        return self.conn.get_sockfd()

    def stop(self):
        if self.stopped or not self.lasttxid or not self.lastdbgpcmd or \
           not self.conn.isconnected():
            return
        stopping = '<?xml version="1.0" encoding="iso-8859-1"?>\n'\
                   '<response xmlns="urn:debugger_protocol_v1" '\
                   'xmlns:xdebug="http://xdebug.org/dbgp/xdebug" '\
                   'command="'+self.lastdbgpcmd+'" transaction_id="'\
                   +self.lasttxid+'" status="stopped" reason="ok"></response>'
        self.send_msg(stopping)
        self.stopped = True

    def close(self):
        """Close connection to debug client and kill it if we started it"""
        if self.conn and self.conn.isconnected():
            self.stop()
            self.conn.close()
        if self.p_client:
            try:
                os.kill(self.p_client.pid, SIGKILL)
            except OSError:
                pass
            self.p_client = None


class XDebug:
    """Encapsulates XDebug connection and communication"""

    def __init__(self, s_accept):
        sock, addr = s_accept.accept()
        self.conn = DBGpConn(sock) # xdebug DBGp connection
        self.dbgp_init = self.conn.recv_msg()
        self.txid = 1000000 # use high txids to avoid clashes with clients
        self.run()

    def isconnected(self):
        return self.conn and self.conn.isconnected()

    def get_dbgp_init(self):
        return self.dbgp_init

    def set_breakpoint(self, function):
        """Attempt to set a breakpoint at the beginning of _function_.
        Return breakpoint id on success, None if breakpoint could not be set"""
        self.txid+=1
        cmd = "breakpoint_set -i " + str(self.txid) + " -t call -m " + function
        self.send_cmd(cmd)
        reply = self.recv_reply()
        bpid = dbgp_get_bpid(reply)
        return bpid

    def remove_breakpoint(self, bpid):
        self.txid+=1
        cmd = "breakpoint_remove -i " + str(self.txid) + " -d " + bpid
        self.send_cmd(cmd)
        self.recv_reply() # discard reply

    def remove_all_breakpoints(self):
        self.txid+=1
        cmd = "breakpoint_list -i " + str(self.txid)
        self.send_cmd(cmd)
        response = self.recv_reply()
        doc = xml.dom.minidom.parseString(response)
        bps = doc.getElementsByTagName("breakpoint")
        for bp in bps:
            if bp.hasAttribute("id"):
                self.remove_breakpoint(bp.getAttribute("id"))

    def run(self):
        self.txid+=1
        cmd = "run -i " + str(self.txid)
        self.send_cmd(cmd)

    def stop(self):
        self.txid+=1
        cmd = "stop -i " + str(self.txid)
        self.send_cmd(cmd)

    def recv_msg(self):
        msg = self.conn.recv_msg()
        if xdebug_is_stopping(msg):
            self.stop()
            self.disconnect()
            raise EOFError, "xdebug is stopping"
        else:
            return msg

    def recv_reply(self, txid=None):
        if not txid:
            txid = self.txid
        while True:
            msg = self.recv_msg()
            rtxid = dbgp_get_txid(msg)
            if rtxid and str(txid) == str(rtxid):
                return msg

    def send_cmd(self, cmd):
        return self.conn.send_cmd(cmd)

    def get_sockfd(self):
        return self.conn.get_sockfd()

    def disconnect(self):
        self.conn.close()


class DebugSession:
    """This class encapsulates the process of debugging a single
    function"""

    def __init__(self, function, client, xdebug):
        self.client = client
        self.clienthost = client.host
        self.clientport = client.port
        self.clientcmd = client.cmd
        self.xdebug = xdebug
        self.function = function
        self.funbp = None # breakpoint id for .function
        self.donebp = None # breakpoint id for PHPShell__eval_completed()

    def run(self):
        # xdebug must be in 'break' state here
        # set a breakpoint on the function being debugged and on
        # PHPShell__eval_completed(), which phpsh.php will execute
        # right after the eval(), continue PHP execution
        self.funbp = self.xdebug.set_breakpoint(self.function)
        self.donebp = self.xdebug.set_breakpoint('PHPShell__eval_completed')
        self.xdebug.run()

        filename = None
        while not filename:
            reply = self.xdebug.recv_msg()
            filename = dbgp_get_filename(reply)

        if not filename.startswith("file://") or \
           filename.endswith("/phpsh.php"):
            # Execution stopped at PHPShell__eval_completed() or in the eval().
            # Abort the session.
            self.client = None
            return

        # at this point reply and filename are initialized. Delete
        # breakpoint at self.function, then send <init> to client with
        # fileuri attr set to filename (followed by the break message
        # itself?)
        self.xdebug.remove_breakpoint(self.funbp)
        self.funbp = None
        r = re.compile(' fileuri="([^"]*)"', re.M)
        client_init = r.sub(' fileuri="' + filename + '"',
                            self.xdebug.get_dbgp_init())
        self.client.send_msg(client_init)
        #self.client.send_msg(reply) --this breaks geben, so disabling for now
        # but vim seems to need id

        # forward messages between client and xdebug until xdebug
        # stops in phpsh.php, client closes connection or parent closes stdin
        session_set = poll()
        session_set.register(self.client.get_sockfd(), POLLIN)
        session_set.register(self.xdebug.get_sockfd(), POLLIN)
        session_set.register(0, POLLIN|POLLHUP)

        while True:
            events = session_set.poll()
            for fd, e in events:
                if fd == 0:
                    phpsh_cmd = raw_input()
                    if phpsh_cmd == 'run php':
                        # phpsh wants to restart php
                        # if php is blocked in xdebug, send a run command
                        raise EOFError # TODO: handle this gracefully
                    else:
                        raise EOFError
                elif fd == self.client.get_sockfd():
                    dbgp_cmd = self.client.recv_cmd()
                    self.xdebug.send_cmd(dbgp_cmd)
                elif fd == self.xdebug.get_sockfd():
                    reply = self.xdebug.recv_msg()
                    filename = dbgp_get_filename(reply)
                    if filename and filename.endswith("/phpsh.php"):
                        return
                    self.client.send_msg(reply)

    def stop(self):
        """Do our best to clean up if we got to the end of a session
        or if client or xdebug connection had an exception. This
        function does not throw any IO exceptions."""
        if self.xdebug and self.xdebug.isconnected():
            try:
                # remove all bps we set in xdebug so that php will not
                # get stuck on them when phpsh issues non-debug evals
                self.xdebug.remove_all_breakpoints()
                self.xdebug.run()
            except (socket.error, EOFError):
                pass
        self.funbp = self.donebp = None
        # do not close the client at the end of session just send it a
        # stop message
        if self.client:
            try:
                self.client.stop()
            except (socket.error, EOFError):
                pass


class PhpshDebugProxy:
    """This is a DBGp filtering proxy for phpsh. It sits between xdebug.so
    extension in PHP interpreter and a GUI debug client such as
    Geben/Emacs and alters the conversation in a way that makes the
    debugger and client begin debugging on a function specified through
    commands that the proxy reads from stdin"""

    # if doing dynamic port assignment, pick ports from this range:
    minport = 9002
    maxport = 9998

    def __init__(self, config):
        self.config = config # RawConfigParser
        self.cmd = None      # Popen command list to start client if local
        self.client = None   # DebugClient
        self.xdebug = None   # XDebug
        self.session = None  # DebugSession
        self.s_accept = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        # host on which client runs:
        self.clienthost = config.get_option("Debugging", "ClientHost")
        # client listens on this port
        self.clientport = parse_port(config.get_option("Debugging",
                                                       "ClientPort"))
        # seconds to wait for client to init
        self.client_timeout = parse_timeout(config.get_option("Debugging",
                                                              "ClientTimeout"))
        listenport = parse_port(config.get_option("Debugging", "ProxyPort"))
        if listenport:
            self.s_accept.bind(('', listenport))
        else:
            listenport = self.bind_to_port()

        self.listenport = listenport # port on which proxy listens
        if not self.clientport:
            self.clientport = listenport+1

        cmd = config.get_option("Debugging", "DebugClient")
        if cmd.startswith("emacs"):
            emacs_version = get_emacs_version()
            if emacs_version < [22, 1]:
                raise Exception, "emacs version " + str(emacs_version) +\
                                 " is too low, 22.1 or above required"
            debugclient_path = config.get_option("Emacs", "XdebugClientPath")
            debugclient_version = get_debugclient_version(debugclient_path)
            if debugclient_version < [0, 10, 0]:
                raise Exception, "debugclient (xdebug client) version " +\
                      str(debugclient_version) + " is too low. 0.10.0 or "\
                      "above required"
            self.cmd = self.emacs_command()
        else:
            self.cmd = shlex.split(cmd)

        self.s_accept.listen(1)
        self.s_accept.settimeout(1)

        # tell parent we have initialized
        debug_log("initialized, bound to port " + str(listenport))
        flush_stdout('initialized port='+ str(listenport))


    def emacs_command(self):
        """Returns a list containing a shell command to start and
        configure emacs according to the settings in phpsh config file"""
        phpsh_root = os.path.dirname(os.path.realpath(__file__))
        elisp_root = os.path.join(phpsh_root, "xdebug-clients/geben")
        geben_elc = os.path.join(elisp_root, "geben.elc")
        phpsh_el = os.path.join(phpsh_root, "phpsh.el")
        help = os.path.join(elisp_root, "help")
        fg = self.config.get_option("Emacs", "ForegroundColor")
        bg = self.config.get_option("Emacs", "BackgroundColor")
        ina = self.config.get_option("Emacs", "InactiveColor")
        family = self.config.get_option("Emacs", "FontFamily")
        size = self.config.get_option("Emacs", "FontSize")
        debugclient_path = config.get_option("Emacs", "XdebugClientPath")

        elisp = "(progn (set-face-foreground 'default \""+fg+"\") "+\
                "(setq active-bg \""+bg+"\") "+\
                "(setq inactive-bg \""+ina+"\") "\
                "(setq geben-dbgp-command-line \""+debugclient_path+\
                            " -p "+str(self.clientport)+"\") "
        if family or size:
            elisp += "(set-face-attribute 'default nil"
            if family:
                elisp += " :family \""+family+"\""
            if size:
                elisp += " :height "+size
            elisp += ") "
        if self.config.get_option("Emacs", "InactiveMinimize") == "yes":
            elisp +="(add-hook 'geben-session-finished-hook "\
                     "'iconify-frame) "\
                     "(add-hook 'geben-session-starting-hook "\
                     "'make-all-frames-visible) "
        if self.config.get_option("Debugging", "Help") == "yes":
            elisp += "(split-window) "\
                     "(find-file-read-only \""+help+"\") "\
                     "(other-window 1) "
        else:
            elisp += "(find-file-read-only \""+help+"\") "\
                     "(switch-to-buffer \"*scratch*\") "
        elisp += ")"

        return ["emacs", "--name", "phpsh-emacs", "-Q", "-l", geben_elc,
                "-l", phpsh_el, "--eval", elisp, "-f", "geben"]

    def bind_to_port(self):
        """Find an unused pair of adjacent ports (n,n+1) where n is
        between PhpshDebugProxy.minport and maxport. Bind .s_accept to
        n and return n. Throw socket.exception if suitable pair was
        available."""

        for n in xrange(PhpshDebugProxy.minport, PhpshDebugProxy.maxport+2, 2):
            try:
                self.s_accept.bind(('', n))
            except socket.error:
                continue
            # check if client port is also available
            trysock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            try:
                trysock.bind(('', n+1))
                trysock.close()
                return n
            except socket.error:
                trysock.close()
                self.s_accept.close()
                self.s_accept = socket.socket(socket.AF_INET,
                                              socket.SOCK_STREAM)
        raise socket.error, "No ports available"


    def setup_client(self):
        """If we don't yet have a client, start and connect, return
        False. If we had a client and it disconnected, clean up,
        restart, and reconnect, return True. If the client exists and
        is ok, do nothing, just return False."""

        # check if the client is still connected by reading everything that
        # the client sent us since the end of last session (if any) and
        # checking for HUP
        if self.client:
            if self.client.isconnected():
                client_set = poll()
                client_set.register(self.client.get_sockfd(), POLLIN)
                events = client_set.poll(0)
                try:
                    while events:
                        fd, e = events[0]
                        if e&POLLHUP:
                            self.client.close()
                            self.client = None
                            raise EOFError
                        else:
                            self.client.recv_cmd()
                            events = client_set.poll(0)
                except (socket.error, EOFError):
                    pass

        if not self.client or not self.client.isconnected():
            self.client = DebugClient(self.clienthost, self.clientport,
                                      self.cmd, self.client_timeout)

    def run(self):
        """This is the proxy's main loop"""
        while True:
            if self.xdebug == None or not self.xdebug.isconnected():
                # if we do not have an xdebug connection, accept one
                # keep an eye on phpsh, exit if it closes our stdin
                self.xdebug = None
                while not self.xdebug:
                    try:
                        self.xdebug = XDebug(self.s_accept)
                    except (socket.error, socket.timeout, EOFError):
                        if stdin_closed():
                            debug_log("parent HUP'd our stdin")
                            return
                        time.sleep(1)
            # at this point self.xdebug is initialized
            # block waiting for a command from phpsh, if xdebug disconnects
            # because PHP was restarted, start over
            try:
                cmd_pollset = poll()
                cmd_pollset.register(0, POLLIN|POLLHUP)
                cmd_pollset.register(self.xdebug.get_sockfd(), POLLIN|POLLHUP)
                phpsh_cmd = None
                while not phpsh_cmd:
                    # wait for a command from phpsh
                    # keep an eye on PHP, handle restarts
                    events = cmd_pollset.poll()
                    for fd, e in events:
                        if fd == 0:
                            phpsh_cmd = raw_input()
                            if phpsh_cmd == "run php":
                                # phpsh sends this when it needs to restart PHP
                                # Since PHP is not blocked in debugger there
                                # is nothing to do. Ignore.
                                debug_log("got 'run php' from phpsh while "\
                                          "waiting for x command")
                                phpsh_cmd = None
                            break
                        elif fd == self.xdebug.get_sockfd():
                            # xdebug disconnected or sent us something
                            # after <init> or after previous client
                            # session ended
                            res = self.xdebug.recv_msg()
                            filename = dbgp_get_filename(res)
                            if filename and filename.endswith("/phpsh.php"):
                                # there is a bug in xdebug where it breaks
                                # on PHPShell__eval_completed() after the
                                # breakpoint on that function was removed. This
                                # happens only if we reach the function via
                                # a "step out" command. Compensate by sending
                                # a run command.
                                debug_log("ERROR: xdebug stopped in phpsh.php "\
                                          "after breakpoint was removed.")
                                self.xdebug.run()
            except (socket.error, EOFError), msg:
                if stdin_closed():
                    return # phpsh went away
                if not self.xdebug.isconnected():
                    continue # xdebug disconnected
                else:
                    debug_log("Error: unexpected exception " + str(msg))

            # at this point phpsh_cmd has a new command from phpsh
            if phpsh_cmd.startswith("x "):
                function = phpsh_cmd[2:].strip()
                session = None
                try:
                    # verify that client is running, start and (re)connect
                    # if necessary
                    self.setup_client()
                    session = DebugSession(function, self.client, self.xdebug)
                    flush_stdout("ready")
                except socket.timeout:
                    flush_stdout("timed out while waiting for "\
                                 "client to start")
                except Exception, msg:
                    mstr = str(msg)
                    flush_stdout("failed to start client: "+mstr[0:40])
                if not session:
                    continue
                try:
                    # wait for an initial break message issued by
                    # xdebug_break() that phpsh is going to execute
                    # right before the target function
                    self.xdebug.recv_msg()
                    session.run()
                except (socket.error, EOFError):
                    # client, PHP, or phpsh exited, session.stop() will
                    # clean up
                    pass
                session.stop()
                if stdin_closed():
                    return
            else:
                flush_stdout("ERROR: invalid command: " + phpsh_cmd)


# Based on debugger.py for Vim. Closes underlying socket on all exceptions.
#
# Authors:
#    Seung Woo Shin <segv <at> sayclub.com>
#    Sam Ghods <sam <at> box.net>
class DBGpConn:
  """ DBGp Connection class """
  def __init__(self, sock):
    self.sock     = sock

  def isconnected(self):
    return self.sock != None

  def close(self):
    if self.sock != None:
      self.sock.close()
      self.sock = None

  def _recv_length(self):
    length = ''
    while 1:
      c = self.sock.recv(1)
      if c == '':
        self.close()
        raise EOFError, 'Socket Closed'
      if c == '\0':
        return int(length)
      if c.isdigit():
        length = length + c

  def _recv_null(self):
    while 1:
      c = self.sock.recv(1)
      if c == '':
        self.close()
        raise EOFError, 'Socket Closed'
      if c == '\0':
        return

  def _recv_body(self, to_recv):
    body = ''
    while to_recv > 0:
      buf = self.sock.recv(to_recv)
      if buf == '':
        self.close()
        raise EOFError, 'Socket Closed'
      to_recv -= len(buf)
      body = body + buf
    return body

  def recv_msg(self):
      try:
          length = self._recv_length()
          body   = self._recv_body(length)
          self._recv_null()
          debug_log("received from " + str(self.sock.fileno()) + ": " + body)
          return body
      except:
          self.close()
          raise

  def recv_cmd(self):
      try:
          cmd = ''
          while True:
              c = self.sock.recv(1)
              if c == '':
                  self.close()
                  raise EOFError, 'Socket Closed'
              elif c == '\0':
                  debug_log("received from " + str(self.sock.fileno()) + ": " +
                            cmd)
                  return cmd
              else:
                  cmd += c
      except:
          self.close()
          raise

  def send_cmd(self, cmd):
      try:
          self.sock.send(cmd + '\0')
          debug_log("sent to " + str(self.sock.fileno()) + ": " + cmd)
      except:
          self.close()
          raise

  def send_msg(self, msg):
      try:
          self.sock.send(str(len(msg))+'\0'+msg+'\0')
          debug_log("sent to " + str(self.sock.fileno()) + ": " + msg)
      except:
          self.close()
          raise

  def get_sockfd(self):
      if self.sock:
          return self.sock.fileno()


config = PhpshConfig()
try:
    config.read()
except Exception, msg:
    pass

tracing_enabled = (config.get_option("Debugging", "LogDBGp") == "yes")

# remove this process from the fg process group so that Ctrl-C of phpsh
# doesn't send us a SIGINT
os.setpgrp()

try:
    proxy = PhpshDebugProxy(config)
except Exception, msg:
    print "failed to initialize: " + str(msg)
    sys.exit(1)

proxy.run()

if proxy.client:
    debug_log("Closing client")
    proxy.client.close()
else:
    debug_log("No client to close")
