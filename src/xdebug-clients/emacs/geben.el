;;; geben.el --- DBGp protocol frontend, a script debugger
;; $Id: geben.el 49 2008-11-06 04:44:14Z fujinaka.tohru $
;; 
;; Filename: geben.el
;; Author: reedom <fujinaka.tohru@gmail.com>
;; Maintainer: reedom <fujinaka.tohru@gmail.com>
;; Version: 0.19
;; URL: http://code.google.com/p/geben-on-emacs/
;; Keywords: DBGp, debugger, PHP, Xdebug, Perl, Python, Ruby, Tcl, Komodo
;; Compatibility: Emacs 22.1
;;
;; This file is not part of GNU Emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;; GEBEN is a software package that interfaces Emacs to DBGp protocol
;; with which you can debug running scripts interactive. At this present
;; DBGp protocol are supported in several script languages with help of
;; custom extensions.
;;
;;; Usage
;;
;; 1. Insert autoload hooks into your .Emacs file.
;;    -> (autoload 'geben "geben" "PHP Debugger on Emacs" t)
;; 2. Start GEBEN. By default, M-x geben will start it.
;;    GEBEN starts to listening to DBGp protocol session connection.
;; 3. Run debuggee script.
;;    When the connection is established, GEBEN loads the entry script
;;    file in geben-mode.
;; 4. Start debugging. To see geben-mode key bindings, type ?.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Requirements:
;;
;; [Server side]
;; - PHP with Xdebug 2.0.3
;;    http://xdebug.org/
;; - Perl, Python, Ruby, Tcl with Komodo Debugger Extension
;;    http://aspn.activestate.com/ASPN/Downloads/Komodo/RemoteDebugging
;;
;; [Client side]
;; - Emacs 22.1 and later
;; - DBGp client(Debug client)
;;    http://xdebug.org/
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(eval-when-compile
  (when (or (not (boundp 'emacs-version))
	    (string< emacs-version "22.1"))
    (error (concat "geben.el: This package requires Emacs 22.1 or later."))))

(eval-and-compile
  (require 'cl)
  (require 'gud)
  (require 'xml)
  (require 'tree-widget))

;;--------------------------------------------------------------
;; constants
;;--------------------------------------------------------------

(defconst geben-process-buffer-name "*GEBEN process*"
  "Name for DBGp client process console buffer.")
(defconst geben-redirect-combine-buffer-name "*GEBEN output*"
  "Name for the debuggee script's STDOUT and STDERR redirection buffer.")
(defconst geben-redirect-stdout-buffer-name "*GEBEN stdout*"
  "Name for the debuggee script's STDOUT redirection buffer.")
(defconst geben-redirect-stderr-buffer-name "*GEBEN stderr*"
  "Name for the debuggee script's STDERR redirection buffer.")
(defconst geben-backtrace-buffer-name "*GEBEN backtrace*"
  "Name for backtrace buffer.")
(defconst geben-breakpoint-list-buffer-name "*GEBEN breakpoint list*"
  "Name for breakpoint list buffer.")
(defconst geben-context-buffer-name "*GEBEN context*"
  "Name for context buffer.")

;;--------------------------------------------------------------
;; customization
;;--------------------------------------------------------------

;; For compatibility between versions of custom
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable)
	   ;; Some XEmacsen w/ custom don't have :set keyword.
	   ;; This protects them against custom.
	   (fboundp 'custom-initialize-set))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (if (boundp 'defgroup)
	nil
      (defmacro defgroup (&rest args)
	nil))
    (if (boundp 'defcustom)
	nil
      (defmacro defcustom (var value doc &rest args)
	`(defvar (,var) (,value) (,doc))))))

;; customize group

(defgroup geben nil
  "A PHP Debugging environment."
  :group 'debug)

(defgroup geben-highlighting-faces nil
  "Faces for GEBEN."
  :group 'geben
  :group 'font-lock-highlighting-faces)

;; geben session start/finish hooks

(defcustom geben-session-starting-hook nil
  "*Hook running at when the geben debugging session is starting."
  :group 'geben
  :type 'hook)

(defcustom geben-session-finished-hook nil
  "*Hook running at when the geben debugging session is finished."
  :group 'geben
  :type 'hook)

;; file hooks

(defcustom geben-after-visit-hook 'geben-enter-geben-mode
  "*Hook running at when GEBEN visits a debuggee script file.
Each functions is invoked with an argument BUFFER."
  :group 'geben
  :type 'hook)

;; display window behavior

(defcustom geben-display-window-function 'pop-to-buffer
  "*Function to display a debuggee script's content.
Typically `pop-to-buffer' or `switch-to-buffer'."
  :group 'geben
  :type 'function)

(defun geben-dbgp-display-window (buf)
  "Display a buffer anywhere in a window, depends on the circumstance."
  (cond
   ((get-buffer-window buf)
    (select-window (get-buffer-window buf))
    (switch-to-buffer buf))
   ((or (eq 1 (count-windows))
	(not (geben-dbgp-dynamic-property-buffer-visiblep)))
    (funcall geben-display-window-function buf))
   (t
    (let (target-window)
      (condition-case nil
	  (walk-windows (lambda (window)
			  (when (and (not (geben-dbgp-dynamic-property-bufferp
					   (window-buffer window)))
				     (eq (selected-window) window))
			    (setq target-window window)
			    (error nil))))
	(error nil))
      (when target-window
	(select-window target-window))
      (switch-to-buffer buf))))
  buf)

;; temporary file (files fetched from remote are stored temporary)

(defcustom geben-temporary-file-directory temporary-file-directory
  "*Base directory path where GEBEN creates a temporary directory."
  :group 'geben
  :type 'directory)

(defcustom geben-close-mirror-file-after-finish t
  "*Specify whether GEBEN should close fetched files from remote site after debugging.
Since the remote files is stored temporary that you can confuse
they were editable if they were left after a debugging session.
If the value is non-nil, GEBEN closes temporary files when
debugging is finished.
If the value is nil, the files left in buffers."
  :group 'geben
  :type 'boolean)

(defcustom geben-always-use-mirror-file-p nil
  "*If nil, GEBEN uses local source file directly in debugging
session if possible.
If non-nil, GEBEN never uses local source file but uses mirror
copied source file."
  :group 'geben
  :type 'boolean)

;;--------------------------------------------------------------
;;  cross emacs overlay definitions
;;--------------------------------------------------------------

(eval-and-compile
  (if (featurep 'xemacs)
      (progn
	(defalias 'geben-overlay-livep 'extent-live-p)
	(defalias 'geben-overlay-make
	  (lambda (beg end &optional buffer front-advance rear-advance)
	    (let ((e (make-extent beg end buffer)))
	      (and front-advance
		   (set-extent-property e 'start-open t))
	      (and rear-advance e 'end-open t)
	      e)))
	(defalias 'geben-overlay-move 'set-extent-endpoints)
	(defalias 'geben-overlay-put 'set-extent-property)
	(defalias 'geben-overlay-get 'extent-property)
	(defalias 'geben-overlay-delete 'delete-extent)
	(defalias 'geben-overlays-at
	  (lambda (pos) (extent-list nil pos pos)))
	(defalias 'geben-overlays-in 
	  (lambda (beg end) (extent-list nil beg end)))
	(defalias 'geben-overlay-buffer 'extent-buffer)
	(defalias 'geben-overlay-start 'extent-start-position)
	(defalias 'geben-overlay-end 'extent-end-position)
	(defalias 'geben-overlay-next-change 'next-extent-change)
	(defalias 'geben-overlay-previous-change 'previous-extent-change)
	(defalias 'geben-overlay-lists
	  (lambda () (list (extent-list))))
	(defalias 'geben-overlayp 'extentp)
	)
    (defalias 'geben-overlay-livep 'overlay-buffer)
    (defalias 'geben-overlay-make 'make-overlay)
    (defalias 'geben-overlay-move 'move-overlay)
    (defalias 'geben-overlay-put 'overlay-put)
    (defalias 'geben-overlay-get 'overlay-get)
    (defalias 'geben-overlay-delete 'delete-overlay)
    (defalias 'geben-overlays-at 'overlays-at)
    (defalias 'geben-overlays-in 'overlays-in)
    (defalias 'geben-overlay-buffer 'overlay-buffer)
    (defalias 'geben-overlay-start 'overlay-start)
    (defalias 'geben-overlay-end 'overlay-end)
    (defalias 'geben-overlay-next-change 'next-overlay-change)
    (defalias 'geben-overlay-previous-change 'previous-overlay-change)
    (defalias 'geben-overlay-lists 'overlay-lists)
    (defalias 'geben-overlayp 'overlayp)
    ))

(defun geben-overlay-make-line (lineno &optional buf)
  "Create a whole line overlay."
  (with-current-buffer (or buf (current-buffer))
    (save-excursion
      (widen)
      (goto-line lineno)
      (beginning-of-line)
      (geben-overlay-make (point)
			  (save-excursion
			    (forward-line) (point))
			  nil t nil))))

;;-------------------------------------------------------------
;;  macros
;;-------------------------------------------------------------

(defun geben-dbgp-decode-string (string data-encoding coding-system)
  "Decode encoded STRING."
  (when string
    (let ((s string))
      (when (consp s)
	(setq s (car s)))
      (when (stringp s)
	(setq s (cond
		 ((equal "base64" data-encoding)
		  (base64-decode-string s))
		 (t s)))
	(if coding-system
	    (decode-coding-string s coding-system)
	  s)))))

;;==============================================================
;; DBGp handlers
;;==============================================================

;;--------------------------------------------------------------
;; cmd hash
;;--------------------------------------------------------------

(defvar geben-dbgp-cmd-hash (make-hash-table :test #'equal)
  "Hash table of transaction commands.
Key is transaction id used in a dbgp command.
Value is a cmd object.")

(defmacro geben-dbgp-cmd-param-for (key)
  `(plist-get '(:depth "-d"
		:context-id "-c"
		:max-data-size "-m"
		:type "-t"
		:page "-p"
		:key "k"
		:address "-a"
		:name "-n"
		:fileuri "-f"
		:lineno "-n"
		:class "-a"
		:function "-m"
		:state "-s"
		:exception "-x"
		:hit-value "-h"
		:hit-condition "-o"
		:run-once "-r"
		:expression "--")
	      ,key))

(defmacro geben-dbgp-cmd-sequence (send-command &rest callback)
  "Invoke expression sequentially.
CALLBACK is invoked after the response message for SEND-COMMAND
has been received, with three argument. The first one is
SEND-COMMAND. The second is a response message. The third is
decoded error message or nil."
  `(let (tid cmd)
     (when (and (setq tid ,send-command)
		(setq cmd (gethash tid geben-dbgp-cmd-hash)))
       (geben-dbgp-cmd-add-callback cmd ,@callback))))

(defmacro geben-dbgp-cmd-store (tid cmd)
  "Store a CMD to the command transaction list.
TID is transaction id used in a dbgp command.
CMD is a list of command and parameters.
The stored CMD will be pulled later when GEBEN receives a response
message for the CMD."
  `(puthash ,tid ,cmd geben-dbgp-cmd-hash))

(defmacro geben-dbgp-cmd-get (tid)
  "Get a command object from the command hash table specified by TID."
  `(gethash ,tid geben-dbgp-cmd-hash))

(defun geben-dbgp-cmd-remove (tid msg err)
  "Remove command from the command hash table."
  (let ((cmd (geben-dbgp-cmd-get tid)))
    (remhash tid geben-dbgp-cmd-hash)
    (mapc (lambda (callback)
	    (funcall callback cmd msg err))
	  (plist-get cmd :callback))
    cmd))

(defmacro geben-dbgp-cmd-make (operand params &rest callback)
  "Create a new command object.
A command object forms a property list with three properties
:operand, :params and :callback."
  `(list :operand ,operand :param ,params :callback ,callback))

(defmacro geben-dbgp-cmd-param-arg (cmd flag)
  "Get an argument of FLAG from CMD.
For a DBGp command \`stack_get -i 1 -d 2\',
`(geben-dbgp-cmd-param-arg cmd \"-d\")\' gets \"2\"."
  `(cdr-safe (assoc ,flag (plist-get ,cmd :param))))

(defun geben-dbgp-cmd-expand (tid cmd)
  "Build a send command string for DBGp protocol."
  (mapconcat #'(lambda (x)
		 (cond ((stringp x) x)
		       ((integerp x) (int-to-string x))
		       ((atom (format "%S" x)))
		       ((null x) "")
		       (t x)))
	     (geben-flatten (list (plist-get cmd :operand)
				  "-i"
				  tid
				  (plist-get cmd :param)))
	     " "))
  
(defmacro geben-dbgp-cmd-add-callback (cmd &rest callback)
  "Add CALLBACK(s) to CMD.
Command callbacks is invoked at when command is finished."
  `(dolist (cb (list ,@callback))
     (plist-put ,cmd :callback (cons cb (plist-get cmd :callback)))))

;;--------------------------------------------------------------
;; features
;;--------------------------------------------------------------

(defcustom geben-dbgp-feature-list
  '((:set max_data 32768)
    (:set max_depth 1)
    (:set max_children 32)
    (:get breakpoint_types geben-dbgp-store-breakpoint-types))
  "*Specifies set of feature variables for each new debugging session.
Each entry forms a list (METHOD FEATURE_NAME VALUE_OR_CALLBACK).
METHOD is either `:get' or `:set'.
FEATURE_NAME is a feature name described in DBGp specification.
VALUE_OR_CALLBACK is, when the METHOD is `:get' then it should
be symbol of a callback function will be invoked 3 arguments
\(CMD MSG ERR), which are results of feature_get DBGp command.
If the method is `:set' VALUE_OR_CALLBACK can be either a value
or a symbol of a function. In the latter case the result value
of the function is passed to feature_set DBGp command."
  :group 'geben
  :type '(repeat (list (radio (const :get)
			      (const :set))
		       (radio (const :help-echo ":get" :tag "language_supports_threads (:get)" language_supports_threads)
			      (const :tag "language_name (:get)" language_name)
			      (const :tag "encoding (:get)" encoding)
			      (const :tag "protocol_version (:get)" protocol_version)
			      (const :tag "supports_async (:get)" supports_async)
			      (const :tag "data_encoding (:get)" data_encoding)
			      (const :tag "breakpoint_languages (:get)" breakpoint_languages)
			      (const :tag "breakpoint_types (:get)" breakpoint_types)
			      (const :tag "multiple_sessions (:get :set)" multiple_sessions)
			      (const :tag "encoding (:get :set)" encoding)
			      (const :tag "max_children (:get :set)" max_children)
			      (const :tag "max_data (:get :set)" max_data)
			      (const :tag "max_depth (:get :set)" max_depth)
			      (const :tag "supports_postmortem (:get)" supports_postmortem)
			      (const :tag "show_hidden (:get :set)" show_hidden)
			      (const :tag "notify_ok (:get :set)" notify_ok))
		       sexp)))

(defun geben-dbgp-init-features ()
  "Configure debugger engine with value of `geben-dbgp-feature-list'."
  (dolist (entry geben-dbgp-feature-list)
    (let ((method (car entry))
	  (name (symbol-name (nth 1 entry)))
	  (param (nth 2 entry)))
      (case method
	(:set 
	 (let ((value (cond
		       ((null param) nil)
		       ((symbolp param)
			(if (fboundp param)
			    (funcall param)
			  (if (boundp param)
			      (symbol-value param)
			    (symbol-name param))))
		       (t param))))
	   (geben-dbgp-command-feature-set name value)))
	(:get
	 (if (and (symbolp param)
		  (fboundp param))
	     (geben-dbgp-cmd-sequence
	      (geben-dbgp-command-feature-get name)
	      param)
	   (error "`geben-dbgp-feature-alist' has invalid entry: %S" entry)))))))

;;--------------------------------------------------------------
;; tid
;;--------------------------------------------------------------

(defvar geben-dbgp-tid 30000
  "Transaction ID.")

(defmacro geben-dbgp-next-tid ()
  "Make a new transaction id."
  `(incf geben-dbgp-tid))

(defmacro geben-dbgp-tid-of (msg)
  "Get a transaction id of MSG."
  `(let ((tid (cdr (assoc 'transaction_id (cadr ,msg)))))
     (if tid (string-to-number tid))))
  
;;--------------------------------------------------------------
;; session
;;--------------------------------------------------------------

(defvar geben-dbgp-init-info nil
  "DBGp initial message.")

(defvar geben-dbgp-xdebug-p nil
  "Non-nil means the debugger engine is Xdebug.")

(defvar geben-dbgp-target-language nil
  "The current debugging target language.
This will be set at run-time.
Possible values: :php :ruby :python etc.")

(defvar geben-dbgp-current-status nil
  "Store the current session status")

(defmacro geben-dbgp-in-session ()
  `(not (null geben-dbgp-init-info)))
  
(defun geben-dbgp-update-session-status (msg)
  "Remain current status of the current session."
  (case (xml-node-name msg)
    ('connect
     (setq geben-dbgp-current-status 'connect))
    ('init
     (setq geben-dbgp-current-status 'init))
    ('response
     (let ((status (xml-get-attribute msg 'status)))
       (when (string< "" status)
	 (setq geben-dbgp-current-status (intern status)))))))

;;--------------------------------------------------------------
;; stack
;;--------------------------------------------------------------

(defvar geben-dbgp-current-stack nil
  "Current stack list of the debuggee script.")

;; backtrace

(defface geben-backtrace-fileuri
  '((((class color))
     (:foreground "green" :weight bold))
    (t (:weight bold)))
  "Face used to highlight fileuri in backtrace buffer."
  :group 'geben-highlighting-faces)

(defface geben-backtrace-lineno
  '((t :inherit font-lock-variable-name-face))
  "Face for displaying line numbers in backtrace buffer."
  :group 'geben-highlighting-faces)

(defcustom geben-backtrace-mode-hook nil
  "*Hook running at when GEBEN's backtrace buffer is initialized."
  :group 'geben
  :type 'hook)

(defun geben-dbgp-backtrace ()
  "Display backtrace."
  (unless (geben-dbgp-in-session)
    (error "GEBEN is out of debugging session."))
  (let ((buf (get-buffer-create geben-backtrace-buffer-name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (buffer-disable-undo)
      (erase-buffer)
      (dotimes (i (length geben-dbgp-current-stack))
	(let* ((stack (second (nth i geben-dbgp-current-stack)))
	       (fileuri (geben-dbgp-regularize-fileuri (cdr (assq 'filename stack))))
	       (lineno (cdr (assq 'lineno stack)))
	       (where (cdr (assq 'where stack))))
	  (insert (format "%s:%s %s\n"
			  (propertize fileuri 'face "geben-backtrace-fileuri")
			  (propertize lineno 'face "geben-backtrace-lineno")
			  where))
	  (put-text-property (save-excursion (forward-line -1) (point))
			     (point)
			     'geben-stack-frame
			     (list :fileuri fileuri :lineno lineno))))
      (setq buffer-read-only t)
      (geben-backtrace-mode)
      (goto-char (point-min)))
    (geben-dbgp-display-window buf)))

(defvar geben-backtrace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'geben-backtrace-mode-mouse-goto)
    (define-key map "\C-m" 'geben-backtrace-mode-goto)
    (define-key map "q" 'geben-backtrace-mode-quit)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "?" 'geben-backtrace-mode-help)
    map)
  "Keymap for `geben-backtrace-mode'")
    
(defun geben-backtrace-mode ()
  "Major mode for GEBEN's backtrace output.
The buffer commands are:
\\{geben-backtrace-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map geben-backtrace-mode-map)
  (setq major-mode 'geben-backtrace-mode)
  (setq mode-name "GEBEN backtrace")
  (set (make-local-variable 'revert-buffer-function)
       (lambda (a b) nil))
  (and (fboundp 'font-lock-defontify)
       (add-hook 'change-major-mode-hook 'font-lock-defontify nil t))
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'geben-backtrace-mode-hook)
    (run-hooks 'geben-backtrace-mode-hook)))

(defalias 'geben-backtrace-mode-mouse-goto 'geben-backtrace-mode-goto)
(defun geben-backtrace-mode-goto (&optional event)
  (interactive (list last-nonmenu-event))
  (let ((stack-frame
	 (if (or (null event)
		 (not (listp event)))
	     ;; Actually `event-end' works correctly with a nil argument as
	     ;; well, so we could dispense with this test, but let's not
	     ;; rely on this undocumented behavior.
	     (get-text-property (point) 'geben-stack-frame)
	   (with-current-buffer (window-buffer (posn-window (event-end event)))
	     (save-excursion
	       (goto-char (posn-point (event-end event)))
	       (get-text-property (point) 'geben-stack-frame)))))
	same-window-buffer-names
	same-window-regexps)
    (when stack-frame
      (geben-dbgp-indicate-current-line (plist-get stack-frame :fileuri)
					(plist-get stack-frame :lineno)
					t))))

(defun geben-backtrace-mode-quit ()
  "Quit and bury the backtrace mode buffer."
  (interactive)
  (quit-window)
  (geben-where))

(defun geben-where ()
  "Move to the current breaking point."
  (interactive)
  (if geben-dbgp-current-stack
      (let* ((stack (second (car geben-dbgp-current-stack)))
	     (fileuri (geben-dbgp-regularize-fileuri (cdr (assq 'filename stack))))
	     (lineno (cdr (assq 'lineno stack))))
	(geben-dbgp-indicate-current-line fileuri lineno t))
    (when (interactive-p)
      (message "GEBEN is not started."))))

(defun geben-backtrace-mode-help ()
  "Display description and key bindings of `geben-backtrace-mode'."
  (interactive)
  (describe-function 'geben-backtrace-mode))

;;--------------------------------------------------------------
;; context
;;--------------------------------------------------------------

(defface geben-context-category-face
  '((((class color))
     :background "purple"
     :foreground "white"
     :bold t))
  "Face used to highlight context category name."
  :group 'geben-highlighting-faces)

(defface geben-context-variable-face
  '((t :inherit 'font-lock-variable-name-face))
  "Face used to highlight variable name."
  :group 'geben-highlighting-faces)

(defface geben-context-type-face
  '((t :inherit 'font-lock-type-face))
  "Face used to highlight type name."
  :group 'geben-highlighting-faces)
  
(defface geben-context-class-face
  '((t :inherit 'font-lock-constant-face))
  "Face used to highlight type name."
  :group 'geben-highlighting-faces)
  
(defface geben-context-string-face
  '((t :inherit 'font-lock-string-face))
  "Face used to highlight string value."
  :group 'geben-highlighting-faces)
  
(defface geben-context-constant-face
  '((t :inherit 'font-lock-constant-face))
  "Face used to highlight numeric value."
  :group 'geben-highlighting-faces)
  
(defvar geben-dbgp-context-names-alist nil
  "Context names alist.
KEY is a context name string.
VALUE is a context id.")

(defvar geben-dbgp-context-variables nil
  "Context variables.
The structure is: ((context-id . ((old . (name type value))
                                  (new . (name type value)))))")

(defvar geben-dbgp-context-tid 0
  "Transaction id to which the current context variables belong.")

(defvar geben-dbgp-context-expanded-variables nil
  "Context variables in expanded state.")

(defvar geben-dbgp-context-where nil)
(defvar geben-dbgp-context-depth nil)
(defvar geben-dbgp-context-loading nil)
(defvar geben-dbgp-prop-tree-fill-children-hook 'geben-dbgp-context-fill-tree-children)

;; context list accessors

(defmacro geben-dbgp-ctx-get-list (cid)
  "Get context list for the context id CID."
  `(assq ,cid geben-dbgp-context-variables))

(defmacro geben-dbgp-ctx-get-old-list (cid)
  "Get previous context list for the context id CID."
  `(cdr (assq 'old (geben-dbgp-ctx-get-list ,cid))))

(defmacro geben-dbgp-ctx-get-new-list (cid)
  "Get the current context list for the context id CID."
  `(cdr (assq 'new (geben-dbgp-ctx-get-list ,cid))))

(defmacro geben-dbgp-ctx-update-list (cid list)
  "Update the current context list for the context id CID with LIST."
  `(let* ((clist (geben-dbgp-ctx-get-list ,cid))
	  (old (assq 'new clist)))
     (setcdr clist (list (cons 'old (cdr old))
			 (cons 'new ,list)))))

;; context property list accessors

(defmacro geben-dbgp-prop-has-children (property)
  "Check whether PROPERTY has any children."
  `(equal "1" (xml-get-attribute ,property 'children)))

(defun geben-dbgp-prop-typeinfo (property)
  "Get type information of PROPERTY to display it in the context buffer."
  (let ((type (if (xml-get-attribute property 'type)
		  (intern (xml-get-attribute property 'type))
		nil))
	typeinfo)
    (setq typeinfo
	  (cond
	   ((null type) nil)
	   ((member type '(int float))
	    (list :type type
		  :type-visiblep nil
		  :value-face 'geben-context-constant-face))
	   ((eq type 'bool)
	    (list :type type
		  :type-visiblep nil
		  :value-face 'geben-context-constant-face
		  :value-formatter 'geben-dbgp-prop-format-bool))
	   ((eq type 'string)
	    (list :type type
		  :type-visiblep nil
		  :value-face 'geben-context-string-face))
	   ((member type '(array hash))
	    (list :type type
		  :type-visiblep nil
		  :name-formatter 'geben-dbgp-prop-format-array-name
		  :value-face 'default))
	   ((eq type 'null)
	    (list :type type
		  :type-visiblep nil
		  :value-face 'geben-context-constant-face
		  :value-formatter (lambda (value) "null")))
	   ((eq type 'resource)
	    (list :type type
		  :type-visiblep t
		  :value-face 'geben-context-constant-face))
	   ((eq type 'object)
	    (list :type (if (xml-get-attribute property 'classname)
			    (intern (xml-get-attribute property 'classname))
			  type)
		  :type-visiblep t
		  :type-face 'geben-context-class-face
		  :value-face 'default))
	   ((eq type 'uninitialized)
	    (list :type 'undef
		  :type-visiblep t
		  :type-face 'geben-context-type-face
		  :value-face 'default))
	   (t
	    (list :type type
		  :type-visiblep t
		  :type-face 'geben-context-type-face
		  :value-face 'default))))
    typeinfo))

(defun geben-dbgp-prop-format-bool (value)
  "Format VALUE in the debuggee language expression."
  (let ((bool (if (equal "0" value) nil t)))
    (if bool "true" "false")))

(defun geben-dbgp-prop-format-array-name (property)
  "Format array element name in the debuggee language expression."
  (format "%s[%s]"
	  (propertize (xml-get-attribute property 'name)
		      'face 'geben-context-variable-face)
	  (propertize (xml-get-attribute property 'numchildren)
		      'face 'geben-context-constant-face)))

(defun geben-dbgp-prop-get-attribute (property sym)
  "Get attribute SYM from PROPERTY."
  ;; DBGp specs specifies property attributes of context_get and
  ;; property_get commands. But some debugger engines have values not
  ;; as attributes but child elements."
  (let ((node (car (xml-get-children property sym))))
    (if (consp node)
	(geben-dbgp-decode-string (xml-node-children node)
				  (xml-get-attribute node 'encoding)
				  'utf-8)
      (xml-get-attribute property sym))))

(defmacro geben-dbgp-prop-get-name (property)
  "Get name attribute value from PROPERTY."
  `(geben-dbgp-prop-get-attribute ,property 'name))
	
(defmacro geben-dbgp-prop-get-fullname (property)
  "Get fullname attribute value from PROPERTY."
  `(geben-dbgp-prop-get-attribute ,property 'fullname))

(defun geben-dbgp-prop-get-value (property)
  "Get value from PROPERTY."
  (let ((node (car (xml-get-children property 'value))))
    (if (consp node)
	(geben-dbgp-decode-string (xml-node-children node)
				  (xml-get-attribute node 'encoding)
				  'utf-8)
      (geben-dbgp-decode-string (xml-node-children property)
				(xml-get-attribute property 'encoding)
				'utf-8))))

;; context property tree widget

(defun geben-dbgp-prop-tree-open (tree)
  "Expand TREE."
  (let ((marker (widget-get tree :from)))
    (when (markerp marker)
      (with-current-buffer (marker-buffer marker)
	(goto-char marker)
	(call-interactively 'widget-button-press)
	(unless (widget-get tree :open)
	  (call-interactively 'widget-button-press))))))

(defun geben-dbgp-prop-tree-expand-p (tree)
  "A tree widget callback function to indicate whether TREE is able to expand."
  (or (geben-dbgp-prop-tree-has-complete-children tree)
      (and (run-hook-with-args 'geben-dbgp-prop-tree-fill-children-hook
			       tree)
	   nil)))

(defun geben-dbgp-prop-tree-expand (tree)
  "A tree widget callback function to create child list of TREE."
  (mapcar #'geben-dbgp-prop-tree-create-node
	  (xml-get-children (widget-get tree :property) 'property)))

(defun geben-dbgp-prop-tree-has-complete-children (tree)
  "Determine whether TREE has complete child nodes.
Child nodes can be short for :property property of TREE."
  (let* ((property (widget-get tree :property))
	 (children (xml-get-children property 'property))
	 (numchildren (and children
			   (string-to-number (xml-get-attribute property 'numchildren)))))
    (and children
	 (<= numchildren (length children)))))

(defun geben-dbgp-prop-tree-create-node (property)
  "Create nodes which represent PROPERTY."
  (let* ((typeinfo (geben-dbgp-prop-typeinfo property))
	 (value (geben-dbgp-prop-get-value property))
	 tag)
    (let ((formatter (plist-get typeinfo :name-formatter)))
      (setq tag 
	    (if formatter
		(funcall formatter property)
	      (propertize (geben-dbgp-prop-get-name property)
			  'face 'geben-context-variable-face))))
    (when (plist-get typeinfo :type-visiblep)
      (setq tag (concat tag
			(format "(%s)" (propertize
					(symbol-name (plist-get typeinfo :type))
					'face (plist-get typeinfo :type-face))))))
    (let ((formatter (plist-get typeinfo :value-formatter)))
      (when (or value formatter)
	(setq tag (format "%-32s %s" tag
			  (propertize (if formatter
					  (funcall formatter value)
					value)
				      'face (plist-get typeinfo :value-face))))))
    (if (geben-dbgp-prop-has-children property)
	(list 'tree-widget
	      :tag tag
	      :property property
	      :expander 'geben-dbgp-prop-tree-expand
	      :expander-p 'geben-dbgp-prop-tree-expand-p)
      (list 'item :tag (concat "   " tag)))))
  
(defun geben-dbgp-prop-tree-context-id (tree)
  "Get context id to which TREE belongs."
  (when tree
    (let ((cid (widget-get tree :context-id)))
      (or cid
	  (geben-dbgp-prop-tree-context-id (widget-get tree :parent))))))

;; context functions

(defun geben-dbgp-context-update (depth &optional no-select-p)
  "Update the context buffer with context of a stack DEPTH.
If NO-SELECT-P is nil, the context buffer will be selected
after updating."
  (let ((buf (get-buffer geben-context-buffer-name)))
    (when (and buf
	       (or (null no-select-p)
		   (get-buffer-window buf)) ;; only when the buffer is visible
	       geben-dbgp-context-variables)
      (with-current-buffer buf
	(setq geben-dbgp-context-depth depth
	      geben-dbgp-context-where
	      (xml-get-attribute (nth depth geben-dbgp-current-stack) 'where)
	      geben-dbgp-context-loading t))
      ;; Remain the current tid.
      ;; It is possible that the current context proceeds by step_in or
      ;; other continuous commands while retrieving variables.
      ;; To avoid mixing variables with multi context, remain something at here,
      ;; tid, and check the value in the retrieving process.
      (setq geben-dbgp-context-tid geben-dbgp-tid)
      (geben-dbgp-context-update-loop geben-dbgp-tid
				      depth
				      (mapcar (lambda (context)
						(cdr context))
					      geben-dbgp-context-names-alist)
				      no-select-p))))

(defun geben-dbgp-context-update-1 (cmd msg err)
  (when (get-buffer geben-context-buffer-name)
    (geben-dbgp-ctx-update-list (geben-dbgp-cmd-param-arg cmd "-c")
				(xml-get-children msg 'property))))

(defun geben-dbgp-context-update-loop (tid-save depth context-id-list no-select-p)
  (geben-dbgp-cmd-sequence
   (geben-dbgp-command-context-get (car context-id-list) depth)
   `(lambda (cmd msg err)
      (when (and (not err)
		 (eq ,tid-save geben-dbgp-context-tid))
	(geben-dbgp-context-update-1 cmd msg err)
	(let ((context-id-list (cdr (quote ,context-id-list))))
	  (if context-id-list
	      (geben-dbgp-context-update-loop (quote ,tid-save)
					      (quote ,depth)
					      context-id-list
					      (quote ,no-select-p))
	    (let ((buf (geben-dbgp-context-fill-buffer)))
	      (when (and buf
			 (not (quote ,no-select-p)))
		(geben-dbgp-display-window buf)))))))))

(defun geben-dbgp-context-fill-buffer ()
  "Fill the context buffer with locally stored context list."
  (let ((buf (get-buffer geben-context-buffer-name)))
    (when buf
      (with-current-buffer buf
	(let ((inhibit-read-only t)
	      (inhibit-modification-hooks t))
	  (widen)
	  (erase-buffer)
	  (dolist (context-name geben-dbgp-context-names-alist)
	    (let ((old (geben-dbgp-ctx-get-old-list (cdr context-name)))
		  (new (geben-dbgp-ctx-get-new-list (cdr context-name))))
	      (apply 'widget-create
		     'tree-widget
		     :tag (car context-name)
		     :context-id (cdr context-name)
		     :open t
		     (mapcar #'geben-dbgp-prop-tree-create-node new))))
	  (widget-setup))
	(goto-char (point-min))
	(setq geben-dbgp-context-loading nil)))
    buf))

(defun geben-dbgp-context-fill-tree-children (tree &optional tid-save)
  (let ((tid-save (or tid-save
		      geben-dbgp-context-tid))
	(completed (geben-dbgp-prop-tree-has-complete-children tree)))
    (when (eq geben-dbgp-context-tid tid-save)
      (with-current-buffer (get-buffer geben-context-buffer-name)
	(setq geben-dbgp-context-loading (not completed)))
      (if completed
	  (geben-dbgp-prop-tree-open tree)
	(geben-dbgp-context-fill-tree-children-1 tree tid-save)))))

(defun geben-dbgp-context-fill-tree-children-1 (tree tid-save)
  (let* ((property (widget-get tree :property))
	 (children (xml-get-children property 'property)))
    (with-current-buffer (get-buffer geben-context-buffer-name)
      ;; -- comment on :property-page property --
      ;; debugger engine may lack of PAGESIZE in property message(bug).
      ;; so the following code doesn't rely on PAGESIZE but uses own
      ;; :property-page widget property.
      (let* ((nextpage (if (widget-get tree :property-page)
			   (1+ (widget-get tree :property-page))
			 (if children 1 0)))
	     (args (list :depth geben-dbgp-context-depth
			 :context-id (geben-dbgp-prop-tree-context-id tree)
			 :name (geben-dbgp-prop-get-fullname property)
			 :page nextpage)))
	(widget-put tree :property-page nextpage)
	(when (string< "" (xml-get-attribute property 'key))
	  (plist-put args :key (xml-get-attribute property 'key)))
	(geben-dbgp-cmd-sequence
	 (geben-dbgp-command-property-get args)
	 `(lambda (cmd msg err)
	    (unless err
	      (geben-dbgp-context-append-tree-children (quote ,tid-save)
						       (quote ,tree)
						       (car (xml-get-children msg 'property)))
	      (geben-dbgp-context-fill-tree-children (quote ,tree)
						     (quote ,tid-save)))))))))

(defun geben-dbgp-context-append-tree-children (tid-save tree property)
  (when (eq geben-dbgp-context-tid tid-save)
    (let ((tree-prop (widget-get tree :property)))
      (nconc (or (cddr tree-prop)
		 tree-prop)
	     (cddr property)))))

(defun geben-dbgp-context-display (depth)
  "Display context variables in the context buffer."
  (unless (geben-dbgp-in-session)
    (error "GEBEN is out of debugging session."))
  (let ((buf (get-buffer geben-context-buffer-name)))
    (when (or (< depth 0)
	      (< (length geben-dbgp-current-stack) (1+ depth)))
      (error "GEBEN context display: invalid depth: %S" depth))
    (unless buf
      (setq buf (get-buffer-create geben-context-buffer-name))
      (with-current-buffer buf
	(geben-context-mode)))
    (unless geben-dbgp-context-variables
      (setq geben-dbgp-context-variables
	    (mapcar (lambda (context)
		      (list (cdr context)))
		    geben-dbgp-context-names-alist)))
    (geben-dbgp-context-update depth)))

;; context mode

(defcustom geben-context-mode-hook nil
  "*Hook running at when GEBEN's context buffer is initialized."
  :group 'geben
  :type 'hook)

(defvar geben-context-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'widget-forward)
    (define-key map "S-\t" 'widget-backward)
    ;;(define-key map "\C-m" 'geben-context-mode-expand)
    ;;(define-key map "e" 'geben-context-mode-edit)
    (define-key map "r" 'geben-dbgp-context-refresh)
    (define-key map "q" 'geben-context-mode-quit)
    (define-key map "p" 'widget-backward)
    (define-key map "n" 'widget-forward)
    (define-key map "?" 'geben-context-mode-help)
    map)
  "Keymap for `geben-context-mode'")

(defalias 'geben-context-mode-quit 'geben-backtrace-mode-quit)

(defun geben-context-mode ()
  "Major mode for GEBEN's context output.
The buffer commands are:
\\{geben-context-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map geben-context-mode-map)
  (setq major-mode 'geben-context-mode)
  (setq mode-name "GEBEN context")
  (set (make-local-variable 'revert-buffer-function)
       (lambda (a b) nil))
  (and (fboundp 'font-lock-defontify)
       (add-hook 'change-major-mode-hook 'font-lock-defontify nil t))
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'geben-context-mode-hook)
    (run-hooks 'geben-context-mode-hook))
  (buffer-disable-undo)
  (make-local-variable 'geben-dbgp-context-where)
  (make-local-variable 'geben-dbgp-context-depth)
  (make-local-variable 'geben-dbgp-context-loading)
  (set (make-local-variable 'tree-widget-theme) "geben")
  (setq header-line-format
	(list
	 "Where: "
	 'geben-dbgp-context-where
	 "   "
	 '(geben-dbgp-context-loading "(loading...)")
	 ))
  (setq buffer-read-only t))

(defun geben-dbgp-context-refresh ()
  "Refresh the context buffer."
  (interactive)
  (let ((buf (get-buffer geben-context-buffer-name)))
    (when (and buf
	       (buffer-live-p buf)
	       (geben-dbgp-in-session))
      (let (depth)
	(with-current-buffer buf
	  (setq depth geben-dbgp-context-depth))
	(geben-dbgp-context-update depth)))))

(defun geben-context-mode-help ()
  "Display description and key bindings of `geben-context-mode'."
  (interactive)
  (describe-function 'geben-context-mode))

;;--------------------------------------------------------------
;; source hash
;;--------------------------------------------------------------

(defvar geben-dbgp-source-hash (make-hash-table :test #'equal)
  "Hash table of source files.
Key is a fileuri to a source file being debugged.
Value is a cons of (remotep . filepath).")

(defmacro geben-dbgp-source-make (fileuri remotep local-path)
  "Create a new source object.
A source object forms a property list with three properties
:fileuri, :remotep and :local-path."
  `(list :fileuri ,fileuri :remotep ,remotep :local-path ,local-path))

(defun geben-dbgp-cleanup-file (source)
  (let ((buf (find-buffer-visiting (or (plist-get source :local-path) ""))))
    (when buf
      (with-current-buffer buf
	(when (and (boundp 'geben-mode)
		   (symbol-value 'geben-mode))
	  (geben-mode 0))
	;;	  Not implemented yet
	;; 	  (and (buffer-modified-p buf)
	;; 	       (switch-to-buffer buf)
	;; 	       (yes-or-no-p "Buffer is modified. Save it?")
	;; 	       (geben-write-file-contents this buf))
	(when (and geben-close-mirror-file-after-finish
		   (plist-get source :remotep))
	  (set-buffer-modified-p nil)
	  (kill-buffer buf))))))

;;--------------------------------------------------------------
;; redirect
;;--------------------------------------------------------------

(defvar geben-dbgp-redirect-stdout-current nil)
(defvar geben-dbgp-redirect-stderr-current nil)
(defvar geben-dbgp-redirect-combine-current nil)

(defcustom geben-dbgp-redirect-stdout :redirect
  "*If non-nil, GEBEN redirects the debuggee script's STDOUT.
If the value is \`:redirect', then STDOUT goes to both GEBEN and
default destination.
If the value is \`:intercept', then STDOUT never goes to the
regular destination but to GEBEN."
  :group 'geben
  :type '(choice (const :tag "Disable" nil)
		 (const :tag "Redirect" :redirect)
		 (const :tag "Intercept" :intercept))
  :set (lambda (sym value)
	 (setq geben-dbgp-redirect-stdout value
	       geben-dbgp-redirect-stdout-current value)))

(defcustom geben-dbgp-redirect-stderr :redirect
  "*If non-nil, GEBEN redirects the debuggee script's STDERR.
If the value is \`:redirect', then STDERR goes to both GEBEN and
default destination.
If the value is \`:intercept', then STDERR never goes to the
regular destination but to GEBEN."
  :group 'geben
  :type '(choice (const :tag "Disable" nil)
		 (const :tag "Redirect" :redirect)
		 (const :tag "Intercept" :intercept))
  :set (lambda (sym value)
	 (setq geben-dbgp-redirect-stderr value
	       geben-dbgp-redirect-stderr-current value)))

(defcustom geben-dbgp-redirect-combine t
  "*If non-nil, redirection of STDOUT and STDERR go to same buffer.
Or to each own buffer."
  :group 'geben
  :type 'boolean
  :set (lambda (sym value)
	 (setq geben-dbgp-redirect-combine value
	       geben-dbgp-redirect-combine-current value)))

(defcustom geben-dbgp-redirect-coding-system 'utf-8-dos
  "*Coding system for decoding redirect content."
  :group 'geben
  :type 'coding-system)

(defcustom geben-dbgp-redirect-buffer-init-hook nil
  "*Hook running at when a redirection buffer is created."
  :group 'geben
  :type 'hook)

(defvar geben-dbgp-redirect-bufferp nil)

(defun geben-dbgp-init-redirects ()
  "Initialize redirection related variables."
  (when geben-dbgp-redirect-stdout-current
    (geben-dbgp-command-stdout geben-dbgp-redirect-stdout-current))
  (when geben-dbgp-redirect-stderr-current
    (geben-dbgp-command-stderr geben-dbgp-redirect-stderr-current)))

(defun geben-dbgp-redirect-stream (type encoding content)
  "Print redirected string to specific buffers."
  (let ((bufname (geben-dbgp-redirect-buffer-name type)))
    (when bufname
      (let* ((buf (or (get-buffer bufname)
		      (progn
			(with-current-buffer (get-buffer-create bufname)
			  (set (make-local-variable 'geben-dbgp-redirect-bufferp) t)
			  (setq buffer-undo-list t)
			  (run-hook-with-args 'geben-dbgp-redirect-buffer-init-hook)
			  (current-buffer)))))
	     (outwin (display-buffer buf))
	     save-pos)
	(with-current-buffer buf
	  (setq save-pos (and (eq (point) (point-max))
			      (point)))
	  (save-excursion
	    (goto-char (point-max))
	    (insert (decode-coding-string
		     (if (string= "base64" encoding)
			 (base64-decode-string content)
		       content)
		     geben-dbgp-redirect-coding-system))))
	(unless save-pos
	  (save-selected-window
	    (select-window outwin)
	    (goto-char (point-max))))))))

(defun geben-dbgp-redirect-buffer-name (type)
  "Select buffer name for a redirection type."
  (when (or (and (eq type :stdout) geben-dbgp-redirect-stdout-current)
	    (and (eq type :stderr) geben-dbgp-redirect-stderr-current))
    (cond
     (geben-dbgp-redirect-combine-current
      geben-redirect-combine-buffer-name)
     ((eq :stdout type)
      geben-redirect-stdout-buffer-name)
     (t
      geben-redirect-stderr-buffer-name))))

(defun geben-dbgp-redirect-buffer-existp ()
  "Check whether any redirection buffer exists."
  (let (name)
    (or (and (setq name (geben-dbgp-redirect-buffer-name :stdout))
	     (get-buffer name))
	(and (setq name (geben-dbgp-redirect-buffer-name :stderr))
	     (get-buffer name)))))

(defun geben-dbgp-dynamic-property-bufferp (buf)
  (when (buffer-live-p buf)
    (or (eq buf (get-buffer geben-context-buffer-name))
	(eq buf (get-buffer (geben-dbgp-redirect-buffer-name :stdout)))
	(eq buf (get-buffer (geben-dbgp-redirect-buffer-name :stderr))))))

(defun geben-dbgp-dynamic-property-buffer-visiblep ()
  "Check whether any window displays any property buffer."
  (condition-case nil
      (and (mapc (lambda (buf)
		   (and (buffer-live-p buf)
			(get-buffer-window buf)
			(error nil)))
		 (list (get-buffer geben-context-buffer-name)
		       (geben-dbgp-redirect-buffer-existp)))
	   nil)
    (error t)))

  
;;--------------------------------------------------------------
;; breakpoints
;;--------------------------------------------------------------

(defvar geben-dbgp-breakpoint-types '(:line :call :return :exception :conditional)
  "Breakpoint types supported by the current debugger engine.")

(defvar geben-dbgp-breakpoints nil
  "Break point list")

(defface geben-breakpoint-face
  '((((class color))
     :foreground "white"
     :background "red1")
    (t :inverse-video t))
  "Face used to highlight various names.
This includes element and attribute names, processing
instruction targets and the CDATA keyword in a CDATA section.
This is not used directly, but only via inheritance by other faces."
  :group 'geben-highlighting-faces)

(defcustom geben-show-breakpoints-debugging-only t
  "*Specify breakpoint markers visibility.
If the value is nil, GEBEN will always display breakpoint markers.
If non-nil, displays the markers while debugging but hides after
debugging is finished."
  :group 'geben
  :type 'boolean)

(defun geben-dbgp-store-breakpoint-types (cmd msg err)
  (setq geben-dbgp-breakpoint-types nil)
  (when (and (not err)
	     (equal "1" (xml-get-attribute msg 'supported)))
    (let ((types (car (xml-node-children msg))))
      (dolist (type (split-string (or types "") " "))
	(cond
	 ((string= type "line")
	  (add-to-list 'geben-dbgp-breakpoint-types :line))
	 ((string= type "call")
	  (add-to-list 'geben-dbgp-breakpoint-types :call))
	 ((string= type "return")
	  (add-to-list 'geben-dbgp-breakpoint-types :return))
	 ((string= type "exception")
	  (add-to-list 'geben-dbgp-breakpoint-types :exception))
	 ((string= type "conditional")
	  (add-to-list 'geben-dbgp-breakpoint-types :conditional))
	 ((string= type "watch")
	  (add-to-list 'geben-dbgp-breakpoint-types :watch)))))
    (when geben-dbgp-xdebug-p
      ;; Xdebug 2.0.3 supports the following types but they aren't
      ;; included in the response. Push them in the list manually.
      (add-to-list 'geben-dbgp-breakpoint-types :exception)
      (add-to-list 'geben-dbgp-breakpoint-types :conditional)))
  (unless geben-dbgp-breakpoint-types
    ;; Some debugger engines are buggy;
    ;; they don't return breakpoint types correctly.
    ;; To them put all of types to the list.
    (setq geben-dbgp-breakpoint-types '(:line :call :return :exception :conditional :watch))))

;; breakpoint object manipulators

(defun geben-dbgp-bp= (lhs rhs)
  "Return t if two breakpoint object point same thing."
  (and (eq (plist-get lhs :type)
	   (plist-get rhs :type))
       (equal (plist-get lhs :fileuri)
	      (plist-get rhs :fileuri))
       (equal (plist-get lhs :lineno)
	      (plist-get rhs :lineno))
       (equal (plist-get lhs :function)
	      (plist-get rhs :function))
       (equal (plist-get lhs :exception)
	      (plist-get rhs :exception))
       (equal (plist-get lhs :expression)
	      (plist-get rhs :expression))))

(defun geben-dbgp-bp-make (type &rest params)
  "Create a new line breakpoint object."
  (let ((bp (append (list :type type) params)))
    (mapc (lambda (prop)
	    (when (stringp (plist-get bp prop))
	      (plist-put bp prop (string-to-number (plist-get bp prop)))))
	  '(:lineno :hit-value))
    (when (and (plist-get params :fileuri)
	       (plist-get params :lineno)
	       (not (plist-get params :overlay)))
      (geben-dbgp-bp-setup-overlay bp))
    (let ((name (plist-get params :function)))
      (when (and name
		 geben-dbgp-xdebug-p
		 (string-match "[:->]" name))
	(plist-put bp :class (replace-regexp-in-string "^\\([^:-]+\\).*" "\\1" name))
	(plist-put bp :method (replace-regexp-in-string "^.*[:>]+" "" name))))
    (unless (plist-get params :state)
      (plist-put bp :state "enabled"))
    bp))

(defun geben-dbgp-bp-setup-overlay (bp)
  "Create an overlay for a breakpoint BP."
  (geben-dbgp-bp-finalize bp)
  (let* ((local-path (plist-get bp :local-path))
	 (overlay (and (stringp local-path)
		       (find-buffer-visiting local-path)
		       (geben-overlay-make-line (plist-get bp :lineno)
						(find-buffer-visiting local-path)))))
    (when overlay
      (geben-overlay-put overlay 'face 'geben-breakpoint-face)
      (geben-overlay-put overlay 'evaporate t)
      (geben-overlay-put overlay 'bp bp)
      (geben-overlay-put overlay 'modification-hooks '(geben-dbgp-bp-overlay-modified))
      (geben-overlay-put overlay 'insert-in-front-hooks '(geben-dbgp-bp-overlay-inserted-in-front))
      (plist-put bp :overlay overlay)))
  bp)

(defun geben-dbgp-bp-hide-overlays ()
  "Hide breakpoint overlays."
  (mapc (lambda (bp)
	  (let ((overlay (plist-get bp :overlay)))
	    (and (geben-overlayp overlay)
		 (geben-overlay-livep overlay)
		 (geben-overlay-put overlay 'face nil))))
	geben-dbgp-breakpoints))

(defun geben-dbgp-bp-overlay-modified (overlay afterp beg end &optional len)
  "A callback function invoked when inside of an overlay is modified.
With this callback GEBEN tracks displacements of line breakpoints."
  (when afterp
    (save-excursion
      (save-restriction
	(widen)
	(let* ((lineno-from (progn (goto-char (geben-overlay-start overlay))
				   (geben-what-line)))
	       (lineno-to (progn (goto-char (geben-overlay-end overlay))
				 (geben-what-line)))
	       (lineno lineno-from))
	  (goto-line lineno)
	  (while (and (looking-at "[ \t]*$")
		      (< lineno lineno-to))
	    (forward-line)
	    (incf lineno))
	  (if (< lineno-from lineno)
	      (plist-put (geben-overlay-get overlay 'bp) :lineno lineno))
	  (goto-line lineno)
	  (beginning-of-line)
	  (geben-overlay-move overlay (point) (save-excursion
						(forward-line)
						(point))))))))

(defun geben-dbgp-bp-overlay-inserted-in-front (overlay afterp beg end &optional len)
  "A callback function invoked when text in front of an overlay is modified.
With this callback GEBEN tracks displacements of line breakpoints."
  (if afterp
      (save-excursion
	(goto-line (progn (goto-char (geben-overlay-start overlay))
			  (geben-what-line)))
	(geben-overlay-move overlay (point) (save-excursion
					      (forward-line)
					      (point))))))

(defun geben-dbgp-bp-lineno-find (fileuri lineno)
  "Find a line breakpoint placed at LINENO in a file FILEURI."
  (let ((tmpbp (geben-dbgp-bp-make :line
				   :fileuri fileuri :lineno lineno :overlay t)))
    (find-if (lambda (bp)
	       (geben-dbgp-bp= bp tmpbp))
	     geben-dbgp-breakpoints)))

(defun geben-dbgp-bp-find (id-or-obj)
  "Find a breakpoint.
id-or-obj should be either a breakpoint id or a breakpoint object."
  (find-if 
   (if (stringp id-or-obj)
       (lambda (bp)
	 (string= (plist-get bp :id) id-or-obj))
     (lambda (bp)
       (geben-dbgp-bp= id-or-obj bp)))
   geben-dbgp-breakpoints))
  
(defun geben-dbgp-bp-add (bp)
  "Add a breakpoint BP to `geben-dbgp-breakpoints'.
This function removes same breakpoints as BP from `geben-dbgp-breakpoints'
before proceeding."
  (geben-dbgp-bp-remove bp)
  (add-to-list 'geben-dbgp-breakpoints bp t))

(defun geben-dbgp-bp-remove (id-or-obj)
  "Remove breakpoints having specific breakpoint id or same meaning objects."
  (setq geben-dbgp-breakpoints
	(if (stringp id-or-obj)
	    (remove-if (lambda (bp)
			 (when (string= (plist-get bp :id) id-or-obj)
			   (geben-dbgp-bp-finalize bp)))
		       geben-dbgp-breakpoints)
	  (remove-if (lambda (bp)
		       (when (geben-dbgp-bp= id-or-obj bp)
			 (geben-dbgp-bp-finalize bp)))
		     geben-dbgp-breakpoints))))

(defun geben-dbgp-bp-finalize (bp)
  "Finalize a breakpoint object."
  (when (geben-overlayp (plist-get bp :overlay))
    (geben-overlay-delete (plist-get bp :overlay)))
  bp)

(defun geben-dbgp-bp-find-file-hook ()
  "A callback function invoked when emacs visits a new file.
GEBEN may place overlay markers if there are line breakpoints in
the file."
  (when (or (geben-dbgp-in-session)
	    (not geben-show-breakpoints-debugging-only))
    (let ((buf (current-buffer)))
      (mapc (lambda (bp)
	      (and (plist-get bp :lineno)
		   (eq (find-buffer-visiting (or (plist-get bp :local-path) "")) buf)
		   (geben-dbgp-bp-setup-overlay bp)))
	    geben-dbgp-breakpoints))))

(add-hook 'find-file-hooks 'geben-dbgp-bp-find-file-hook)

;; breakpoint functions

(defun geben-dbgp-restore-breakpoints ()
  "Restore breakpoints against new DBGp session."
  (let (overlay)
    (mapc (lambda (bp)
	    (plist-put bp :id nil)
	    ;; User may edit code since previous debugging session
	    ;; so that lineno breakpoints set before may moved.
	    ;; The followings try to adjust breakpoint line to
	    ;; nearly what user expect.
	    (if (and (setq overlay (plist-get bp :overlay))
		     (geben-overlayp overlay)
		     (geben-overlay-livep overlay)
		     (eq (geben-overlay-buffer overlay)
			 (find-buffer-visiting (or (plist-get bp :local-path) ""))))
		(with-current-buffer (geben-overlay-buffer overlay)
		  (save-excursion
		    (plist-put bp :lineno (progn (goto-char (geben-overlay-start overlay))
						 (geben-what-line))))))
	    (geben-dbgp-command-breakpoint-set bp))
	  geben-dbgp-breakpoints)))

;; breakpoint list

(defface geben-breakpoint-fileuri
  '((t (:inherit geben-backtrace-fileuri)))
  "Face used to highlight fileuri in breakpoint list buffer."
  :group 'geben-highlighting-faces)

(defface geben-breakpoint-lineno
  '((t (:inherit geben-backtrace-lineno)))
  "Face for displaying line numbers in breakpoint list buffer."
  :group 'geben-highlighting-faces)

(defface geben-breakpoint-function
  '((t (:inherit font-lock-function-name-face)))
  "Face for displaying line numbers in breakpoint list buffer."
  :group 'geben-highlighting-faces)

(defun geben-dbgp-breakpoint-sort-pred (a b)
  (if (and (stringp (plist-get a :id))
	   (equal (plist-get a :id)
		  (plist-get b :id)))
      nil
    (let ((type-rank '(:line 1
		       :call 2
		       :return 3
		       :exception 4
		       :conditional 5
		       :watch 6))
	  ax bx cmp)
      (setq cmp (- (plist-get type-rank (plist-get a :type))
		   (plist-get type-rank (plist-get b :type))))
      (if (not (zerop cmp))
	  (< cmp 0)
	(case (plist-get a :type)
	  (:line
	   (setq ax (plist-get a :fileuri))
	   (setq bx (plist-get b :fileuri))
	   (or (string< ax bx)
	       (and (string= ax bx)
		    (< (plist-get a :lineno)
		       (plist-get b :lineno)))))
	  (:call
	   (string< (plist-get a :function)
		    (plist-get b :function)))
	  (:return
	   (string< (plist-get a :function)
		    (plist-get b :function)))
	  (:exception
	   (string< (plist-get a :exception)
		    (plist-get b :exception)))
	  (:conditional
	   (or (string< (plist-get a :fileuri)
			(plist-get b :fileuri))
	       (progn
		 (setq ax (plist-get a :lineno)
		       bx (plist-get b :lineno))
		 (if (null ax)
		     (not (null ax))
		   (if (null ax)
		       nil
		     (< ax bx))))
	       (string< (plist-get a :expression)
			(plist-get b :expression))))
	  (:watch
	   (string< (plist-get a :expression)
		    (plist-get b :expression))))))))

(defun geben-dbgp-breakpoint-list (displayp)
  "Display breakpoint list.
The breakpoint list buffer is under `geben-breakpoint-list-mode'.
Key mapping and other information is described its help page."
  (if (not (geben-dbgp-in-session))
      (geben-dbgp-breakpoint-list-1 geben-dbgp-breakpoints displayp)
    (geben-dbgp-cmd-sequence
     (geben-dbgp-send-command "breakpoint_list")
     `(lambda (cmd msg err)
	(geben-dbgp-create-breakpoints cmd msg err)
	(geben-dbgp-breakpoint-list-1 geben-dbgp-breakpoints ,displayp)))))

(defun geben-dbgp-create-breakpoints (cmd msg err)
  "Create breakpoint objects according to the result of `breakpoint_list'."
  (unless err
    (dolist (msg-bp (xml-get-children msg 'breakpoint))
      (let* ((id (xml-get-attribute msg-bp 'id))
	     (bp (geben-dbgp-bp-find id)))
	(unless bp
	  (let* ((type (intern-soft (concat ":" (xml-get-attribute msg-bp 'type))))
		 (fileuri (xml-get-attribute msg-bp 'filename))
		 (lineno (xml-get-attribute msg-bp 'lineno))
		 (function (xml-get-attribute msg-bp 'function))
		 (class (xml-get-attribute msg-bp 'class))
		 (method function)
		 (exception (xml-get-attribute msg-bp 'exception))
		 (expression (xml-get-attribute msg-bp 'expression))
		 (state (xml-get-attribute msg-bp 'state))
		 (local-path (and fileuri
				  (or (geben-dbgp-get-local-path-of fileuri)
				      (geben-temp-path-for-fileuri fileuri)))))
	    (when (stringp lineno)
	      (setq lineno (string-to-number lineno))
	      (when (floatp lineno) ;; debugger engine may return invalid number.
		(setq lineno 1)))
	    (when class
	      (setq function (format "%s::%s" (or function "") class)))
	    (when expression
	      (setq expression (base64-decode-string expression)))
	    (geben-dbgp-bp-add
	     (setq bp (geben-dbgp-bp-make type
					  :id id
					  :fileuri fileuri
					  :lineno lineno
					  :class class
					  :method method
					  :function function
					  :exception exception
					  :expression expression
					  :state state
					  :local-path local-path)))))
	(when bp
	  (plist-put bp :hit-count (string-to-number (xml-get-attribute msg-bp 'hit_count)))
	  (plist-put bp :hit-value (string-to-number (xml-get-attribute msg-bp 'hit_value))))))))

(defun geben-dbgp-breakpoint-list-1 (breakpoints displayp)
  (let ((buf (get-buffer-create geben-breakpoint-list-buffer-name))
	pos)
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (buffer-disable-undo)
      (erase-buffer)
      (if (or (not (listp breakpoints))
	      (zerop (length breakpoints)))
	  (insert "No breakpoints.\n")
	(setq breakpoints (sort (copy-list breakpoints)
				#'geben-dbgp-breakpoint-sort-pred))
	(mapc (lambda (bp)
		(insert "  ")
		(insert (format "%-11s"
				(or (case (plist-get bp :type)
				      (:line "Line")
				      (:exception "Exception")
				      (:call "Call")
				      (:return "Return")
				      (:conditional "Conditional")
				      (:watch "Watch"))
				    "Unknown")))
		(if (geben-dbgp-in-session)
		    (insert (format "%2s/%-2s  "
				    (or (plist-get bp :hit-count) "?")
				    (let ((hit-value (plist-get bp :hit-value)))
				      (cond
				       ((null hit-value) "?")
				       ((zerop hit-value) "*")
				       (t hit-value)))))
		  (insert " "))
		(when (plist-get bp :function)
		  (insert (propertize (plist-get bp :function)
				      'face 'geben-breakpoint-function))
		  (insert " "))
		(when (plist-get bp :exception)
		  (insert (propertize (plist-get bp :exception)
				      'face 'geben-breakpoint-function))
		  (insert " "))
		(when (plist-get bp :expression)
		  (insert (format "\"%s\" " (plist-get bp :expression))))
		(when (plist-get bp :fileuri)
		  (insert (format "%s:%s"
				  (propertize (plist-get bp :fileuri)
					      'face 'geben-breakpoint-fileuri)
				  (propertize (format "%s" (or (plist-get bp :lineno) "*"))
					      'face 'geben-breakpoint-lineno))))
		(insert "\n")
		(put-text-property (save-excursion (forward-line -1) (point))
				   (point)
				   'geben-bp bp))
	      breakpoints))
      (setq buffer-read-only t)
      (geben-breakpoint-list-mode)
      (setq header-line-format
	    (concat "  Type        "
		    (if (geben-dbgp-in-session) "Hits  " "")
		    "Property"))
      (goto-char (point-min)))
    (when displayp
      (geben-dbgp-display-window buf))))

(defun geben-dbgp-breakpoint-list-refresh ()
  (when (and (eq 'break geben-dbgp-current-status)
	     (get-buffer geben-breakpoint-list-buffer-name))
    (geben-dbgp-breakpoint-list nil)))

;; breakpoint list mode

(defcustom geben-breakpoint-list-mode-hook nil
  "*Hook running at when GEBEN's breakpoint list buffer is initialized."
  :group 'geben
  :type 'hook)

(defvar geben-breakpoint-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'geben-breakpoint-list-mode-mouse-goto)
    (define-key map "\C-m" 'geben-breakpoint-list-mode-goto)
    (define-key map "d" 'geben-breakpoint-list-mark-delete)
    (define-key map "u" 'geben-breakpoint-list-unmark)
    (define-key map "x" 'geben-breakpoint-list-execute)
    (define-key map "q" 'geben-breakpoint-list-mode-quit)
    (define-key map "r" 'geben-breakpoint-list-refresh)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "?" 'geben-breakpoint-list-mode-help)
    map)
  "Keymap for `geben-breakpoint-list-mode'")
    
(defun geben-breakpoint-list-mode ()
  "Major mode for GEBEN's breakpoint list.
The buffer commands are:
\\{geben-breakpoint-list-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map geben-breakpoint-list-mode-map)
  (setq major-mode 'geben-breakpoint-list-mode)
  (setq mode-name "GEBEN breakpoints")
  (set (make-local-variable 'revert-buffer-function)
       (lambda (a b) nil))
  (and (fboundp 'font-lock-defontify)
       (add-hook 'change-major-mode-hook 'font-lock-defontify nil t))
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'geben-breakpoint-list-mode-hook)
    (run-hooks 'geben-breakpoint-list-mode-hook)))

(defun geben-breakpoint-list-mark-delete ()
  "Add deletion mark."
  (interactive)
  (let ((buffer-read-only nil))
    (beginning-of-line)
    (delete-char 1)
    (insert ?D)
    (forward-line 1)))

(defun geben-breakpoint-list-unmark ()
  "Remove deletion mark."
  (interactive)
  (let ((buffer-read-only nil))
    (beginning-of-line)
    (delete-char 1)
    (insert " ")
    (forward-line 1)))

(defun geben-breakpoint-list-execute ()
  "Execute breakpoint deletion."
  (interactive)
  (let (candidates)
    (save-excursion
      (goto-char (point-min))
      (let ((buffer-read-only nil))
	(while (re-search-forward "^D" nil t)
	  (add-to-list 'candidates (get-text-property (point) 'geben-bp)))))
    (dolist (bp candidates)
      (let ((bid (plist-get bp :id)))
	(if (and (geben-dbgp-in-session)
		 bid)
	    (geben-dbgp-cmd-sequence
	     (geben-dbgp-send-command "breakpoint_remove" (cons "-d" bid))
	     `(lambda (cmd msg err)
		(when err
		  ;; it should a stray breakpoint; remove it from bp hash table.
		  (geben-dbgp-bp-remove ,bid))))
	  (setq geben-dbgp-breakpoints
		(delete-if (lambda (bp1)
			     (geben-dbgp-bp= bp bp1))
			   geben-dbgp-breakpoints)))))
    (when candidates
      (geben-dbgp-breakpoint-list t))))

(defun geben-breakpoint-list-mode-goto (&optional event)
  "Move to the set point of the selected breakpoint."
  (interactive (list last-nonmenu-event))
  (let ((bp
         (if (or (null event)
		 (not (listp event)))
             ;; Actually `event-end' works correctly with a nil argument as
             ;; well, so we could dispense with this test, but let's not
             ;; rely on this undocumented behavior.
             (get-text-property (point) 'geben-bp)
           (with-current-buffer (window-buffer (posn-window (event-end event)))
             (save-excursion
               (goto-char (posn-point (event-end event)))
	       (get-text-property (point) 'geben-bp)))))
        same-window-buffer-names
        same-window-regexps)
    (let ((fileuri (plist-get bp :fileuri))
	  (lineno (plist-get bp :lineno)))
      (when (and fileuri lineno)
	(geben-dbgp-indicate-current-line fileuri lineno t)))))

(defun geben-breakpoint-list-mode-quit ()
  "Quit and bury the breakpoint list mode buffer."
  (interactive)
  (quit-window)
  (geben-where))

(defun geben-breakpoint-list-refresh ()
  "Refresh breakpoint list."
  (interactive)
  (geben-dbgp-breakpoint-list-refresh))

(defun geben-breakpoint-list-mode-help ()
  "Display description and key bindings of `geben-breakpoint-list-mode'."
  (interactive)
  (describe-function 'geben-breakpoint-list-mode))

;;------------------------------------------------------------------------
;; DBGp protocol handler
;;------------------------------------------------------------------------

(defun geben-dbgp-entry (msg)
  "Analyze MSG and dispatch to a specific handler."
  (geben-dbgp-update-session-status msg)
  (case (xml-node-name msg)
    ('connect
     t)
    ('init
     (geben-dbgp-handle-init msg))
    ('response
     (geben-dbgp-handle-response msg))
    ('stream
     (geben-dbgp-handle-stream msg))
    ('otherwise
     ;;mada
     (message "unknown protocol: %S" msg))))

(defun geben-dbgp-reset ()
  "Reset GEBEN session."
  (setq gud-last-frame nil)
  (cond
   ((boundp 'gud-overlay-arrow-position)
    (setq gud-overlay-arrow-position nil))
   ((boundp 'overlay-arrow-position)
    (setq overlay-arrow-position nil)))
  (maphash (lambda (fileuri source)
	     (geben-dbgp-cleanup-file source))
	   geben-dbgp-source-hash)
  (when geben-show-breakpoints-debugging-only
    (geben-dbgp-bp-hide-overlays))
  (geben-dbgp-reset-session-variables)
  (ignore-errors
    (geben-delete-directory-tree (geben-temp-dir))))

(defun geben-dbgp-reset-session-variables ()
  "Initialize session variables."
  (geben-dbgp-set-initial-message nil)
  (setq geben-dbgp-current-stack nil)
  (setq geben-dbgp-context-names-alist nil)
  (setq geben-dbgp-context-variables nil)
  (clrhash geben-dbgp-cmd-hash)
  (clrhash geben-dbgp-source-hash))
  
(defun geben-dbgp-handle-init (msg)
  "Handle a init message."
  (run-hooks 'geben-session-starting-hook)
  (add-hook 'geben-dbgp-disconnect-hook #'geben-dbgp-reset)
  (geben-dbgp-set-initial-message msg)
  (geben-dbgp-init-features)
  (geben-dbgp-init-redirects)
  (geben-dbgp-restore-breakpoints)
  (geben-dbgp-prepare-source-file (xml-get-attribute msg 'fileuri))
  (geben-dbgp-command-context-names)
  (geben-dbgp-command-step-into))

(defun geben-dbgp-set-initial-message (msg)
  (setq geben-dbgp-init-info msg)
  (when msg
    (setq geben-dbgp-xdebug-p
	  (and (member "Xdebug" (geben-flatten geben-dbgp-init-info)) t))
    (setq geben-dbgp-target-language
	  (let ((lang (xml-get-attribute msg 'language)))
	    (when (string< "" lang)
	      (intern (concat ":" (downcase lang))))))))
  
(defun geben-dbgp-handle-response (msg)
  "Handle a response message."
  (let* ((tid (geben-dbgp-tid-of msg))
	 (cmd (geben-dbgp-cmd-get tid))
	 (err (ignore-errors (xml-get-children msg 'error))))
    (if err
	(message "Command error: %s"
		 (third (car-safe (xml-get-children (car err) 'message))))
      (let* ((operand (replace-regexp-in-string
		       "_" "-" (xml-get-attribute msg 'command)))
	     (func-name (concat "geben-dbgp-response-" operand))
	     (func (intern-soft func-name)))
	(if (and cmd (functionp func))
	    (funcall func cmd msg)
	  (unless (functionp func)
	    (message "%s is not defined" func-name)))))
    (geben-dbgp-cmd-remove tid msg err)
    (geben-dbgp-handle-status msg err)))

(defun geben-dbgp-handle-stream (msg)
  "Handle a stream message."
  (let ((type (case (intern-soft (xml-get-attribute msg 'type))
		('stdout :stdout)
		('stderr :stderr)))
	(encoding (xml-get-attribute msg 'encoding))
	(content (car (last msg)))
	bufname buf outwin)
    (geben-dbgp-redirect-stream type encoding content)))

(defun geben-dbgp-handle-status (msg err)
  "Handle status code in a response message."
  (let ((status (xml-get-attribute msg 'status)))
    (cond
     ((equal status "stopping")
      (if (geben-dbgp-in-session)
	  (geben-dbgp-command-stop)
	;;(geben-dbgp-command-stop)))
	(gud-basic-call ""))) ;; for bug of Xdebug 2.0.3 with stop command,
					; stopping state comes after stopped state.
     ((equal status "stopped")
      (gud-basic-call "")
      (run-hooks 'geben-session-finished-hook)
      (message "GEBEN debugging session is finished."))
     ((equal status "break")
      (unless err
	(geben-dbgp-cmd-sequence
	 (geben-dbgp-command-stack-get)
	 (lambda (&rest arg)
	   (geben-dbgp-context-update 0 t))))))))

;;; command sending

(defun geben-send-raw-command (fmt &rest arg)
  "Send a command string to a debugger engine.
The command string will be built up with FMT and ARG with a help of
the string formatter function `format'."
  (let ((cmd (apply #'format fmt arg)))
    (gud-basic-call cmd)))

(defun geben-dbgp-send-command (operand &rest params)
  "Send a command to a debugger engine.
This function automatically inserts a transaction ID which is
required for each dbgp command by the protocol specification."
  (when (geben-dbgp-in-session)
    (let ((cmd (geben-dbgp-cmd-make operand params))
	  (tid (geben-dbgp-next-tid)))
      (geben-dbgp-cmd-store tid cmd)
      (gud-basic-call (geben-dbgp-cmd-expand tid cmd))
      tid)))

;;;
;;; command/response handlers
;;;

;; step_into

(defun geben-dbgp-command-step-into ()
  "Send \`step_into\' command."
  (geben-dbgp-send-command "step_into"))

(defun geben-dbgp-response-step-into (cmd msg)
  "A response message handler for \`step_into\' command."
  nil)

;; step_over

(defun geben-dbgp-command-step-over ()
  "Send \`step_over\' command."
  (geben-dbgp-send-command "step_over"))

(defun geben-dbgp-response-step-over (cmd msg)
  "A response message handler for \`step_over\' command."
  nil)

;; step_out
(defun geben-dbgp-response-step-out (cmd msg)
  "A response message handler for \`step_out\' command."
  nil)

(defun geben-dbgp-command-step-out ()
  "Send \`step_out\' command."
  (geben-dbgp-send-command "step_out"))

;; run

(defun geben-dbgp-command-run ()
  "Send \`run\' command."
  (geben-dbgp-send-command "run"))

(defun geben-dbgp-response-run (cmd msg)
  "A response message handler for \`run\' command."
  nil)

;;; stop

(defun geben-dbgp-command-stop ()
  "Send \`stop\' command."
  (geben-dbgp-send-command "stop"))

(defun geben-dbgp-response-stop (cmd msg)
  "A response message handler for \`stop\' command."
  nil)

;; context

(defun geben-dbgp-command-context-names (&optional depth)
  (geben-dbgp-send-command "context_names"
			   (and (numberp depth)
				(cons "-d" depth))))

(defun geben-dbgp-response-context-names (cmd msg)
  (setq geben-dbgp-context-names-alist
	(mapcar (lambda (context)
		  (let ((name (xml-get-attribute context 'name))
			(id (xml-get-attribute context 'id)))
		    (cons name (string-to-number id))))
		(xml-get-children msg 'context))))

;;; breakpoint_set

(defun geben-dbgp-command-breakpoint-set (bp)
  "Send \`breakpoint_set\' command."
  (if (not (geben-dbgp-in-session))
      (geben-dbgp-bp-add bp)
    (let ((obp (geben-dbgp-bp-find bp)))
      (if (and obp
	       (plist-get obp :id))
	  (geben-dbgp-send-command "breakpoint_update"
				   (cons "-d" (plist-get obp :id))
				   (cons "-h" (or (plist-get bp :hit-value)
						  0))
				   (cons "-o" ">="))
	(let ((params
	       (remove nil
		       (list
			(cons "-t"
			      (substring (symbol-name (plist-get bp :type)) 1))
			(and (plist-get bp :fileuri)
			     (cons "-f" (plist-get bp :fileuri)))
			(and (plist-get bp :lineno)
			     (cons "-n" (plist-get bp :lineno)))
			(and (plist-get bp :class)
			     geben-dbgp-xdebug-p
			     (cons "-a" (plist-get bp :class)))
			(and (plist-get bp :function)
			     (if (and geben-dbgp-xdebug-p
				      (plist-get bp :method))
				 (cons "-m" (plist-get bp :method))
			       (cons "-m" (plist-get bp :function))))
			(and (plist-get bp :exception)
			     (cons "-x" (plist-get bp :exception)))
			(cons "-h" (or (plist-get bp :hit-value) 0))
			(cons "-o" ">=")
			(cons "-s" (or (plist-get bp :state)
				       "enabled"))
			(cons "-r" (or (plist-get bp :run-once)
				       0))
			(and (plist-get bp :expression)
			     (cons "--"
				   (base64-encode-string
				    (plist-get bp :expression))))))))
	  (when params
	    (apply 'geben-dbgp-send-command "breakpoint_set" params)))))))

(defun geben-dbgp-response-breakpoint-set (cmd msg)
  "A response message handler for \`breakpoint_set\' command."
  (let* ((type (intern (concat ":" (geben-dbgp-cmd-param-arg cmd "-t"))))
	 (id (xml-get-attribute msg 'id))
	 (fileuri (geben-dbgp-cmd-param-arg cmd "-f"))
	 (lineno (geben-dbgp-cmd-param-arg cmd "-n"))
	 (function (geben-dbgp-cmd-param-arg cmd "-m"))
	 (class (geben-dbgp-cmd-param-arg cmd "-a"))
	 (method function)
	 (exception (geben-dbgp-cmd-param-arg cmd "-x"))
	 (expression (geben-dbgp-cmd-param-arg cmd "--"))
	 (hit-value (geben-dbgp-cmd-param-arg cmd "-h"))
	 (state (geben-dbgp-cmd-param-arg cmd "-s"))
	 (local-path (and fileuri
			  (or (geben-dbgp-get-local-path-of fileuri)
			      (geben-temp-path-for-fileuri fileuri))))
	 bp)
    (when expression
      (setq expression (base64-decode-string expression)))
    (geben-dbgp-bp-add
     (setq bp (geben-dbgp-bp-make type
				  :id id
				  :fileuri fileuri
				  :lineno lineno
				  :class class
				  :method method
				  :function function
				  :exception exception
				  :expression expression
				  :hit-value hit-value
				  :local-path local-path
				  :state state))))
  (geben-dbgp-breakpoint-list-refresh))

(defun geben-dbgp-response-breakpoint-update (cmd msg)
  "A response message handler for `breakpoint_update' command."
  (let* ((id (geben-dbgp-cmd-param-arg cmd "-d"))
	 (bp (geben-dbgp-bp-find id)))
    (when bp
      (plist-put bp :hit-value (geben-dbgp-cmd-param-arg cmd "-h"))
      (geben-dbgp-breakpoint-list-refresh))))

;;; breakpoint_remove

(defun geben-dbgp-command-breakpoint-remove (&optional fileuri path lineno)
  "Send `breakpoint_remove' command."
  (setq path (or path
		 (buffer-file-name (current-buffer))))
  (when (stringp path)
    (setq lineno (or lineno
		     (and (get-file-buffer path)
			  (with-current-buffer (get-file-buffer path)
			    (geben-what-line)))))
    (setq fileuri (or fileuri
		      (geben-dbgp-find-fileuri path)
		      (concat "file://" (file-truename path))))
    (when (and fileuri lineno)
      (let* ((bp (geben-dbgp-bp-lineno-find fileuri lineno))
	     (bid (and bp (plist-get bp :id))))
	(when bp
	  (if (geben-dbgp-in-session)
	      (geben-dbgp-cmd-sequence
	       (geben-dbgp-send-command "breakpoint_remove" (cons "-d" bid))
	       `(lambda (cmd msg err)
		  (when err
		    ;; it should a stray breakpoint; remove it from bp hash table.
		    (geben-dbgp-bp-remove ,bid))))
	    (geben-dbgp-bp-remove bp)))))))

(defun geben-dbgp-response-breakpoint-remove (cmd msg)
  "A response message handler for \`breakpoint_remove\' command."
  (let* ((id (geben-dbgp-cmd-param-arg cmd "-d"))
	 (bp (geben-dbgp-bp-find id)))
    (geben-dbgp-bp-remove id)
    (geben-dbgp-breakpoint-list-refresh)))

(defun geben-dbgp-command-breakpoint-list ()
  "Send `breakpoint_list' command."
  (geben-dbgp-send-command "breakpoint_list"))

(defun geben-dbgp-response-breakpoint-list (cmd msg)
  "A response message handler for \`breakpoint_list\' command."
  t)

;;; stack_get

(defun geben-dbgp-command-stack-get ()
  "Send \`stack_get\' command."
  (geben-dbgp-send-command "stack_get"))

(defun geben-dbgp-response-stack-get (cmd msg)
  "A response message handler for \`stack_get\' command."
  (setq geben-dbgp-current-stack (xml-get-children msg 'stack))
  (let* ((stack (car-safe geben-dbgp-current-stack))
	 (fileuri (xml-get-attribute stack 'filename))
	 (lineno (xml-get-attribute stack'lineno)))
    (when (and fileuri lineno)
      (geben-dbgp-indicate-current-line fileuri lineno))))

;;; eval

(defun geben-dbgp-command-eval (exp)
  "Send \`eval\' command."
  (geben-dbgp-send-command
   "eval"
   (format "-- {%s}" (base64-encode-string exp))))

(defun geben-dbgp-response-eval (cmd msg)
  "A response message handler for \`eval\' command."
  (message "result: %S" 
	   (geben-dbgp-decode-value (car-safe (xml-get-children msg 'property)))))

(defun geben-dbgp-decode-value (prop)
  "Decode a VALUE passed by debugger engine."
  (let ((type (xml-get-attribute prop 'type))
	result)
    (setq result
	  (cond
	   ((or (string= "array" type)
		(string= "object" type))
	    (mapcar (lambda (value)
		      (geben-dbgp-decode-value value))
		    (xml-get-children prop 'property)))
	   ((string= "null" type)
	    nil)
	   (t
	    (let ((value (car (last prop))))
	      (assert (stringp value))
	      (when (string= "base64" (xml-get-attribute prop 'encoding))
		(setq value (base64-decode-string value)))
	      (if (string= "string" type)
		  (decode-coding-string value 'utf-8)
		(string-to-number value))))))
    (let ((name (xml-get-attribute prop 'name)))
      (if (string< "" name)
	  (cons name result)
	result))))
	   
;;; source

(defun geben-dbgp-regularize-fileuri (fileuri)
  ;; for bug of Xdebug 2.0.3 and below:
  (replace-regexp-in-string "%28[0-9]+%29%20:%20runtime-created%20function$" ""
			    fileuri))
  
(defun geben-dbgp-command-source (fileuri)
  "Send source command.
FILEURI is a uri of the target file of a debuggee site."
  (geben-dbgp-send-command "source" (cons "-f"
					  (geben-dbgp-regularize-fileuri fileuri))))


(defun geben-dbgp-response-source (cmd msg)
  "A response message handler for \`source\' command."
  (let* ((fileuri (geben-dbgp-cmd-param-arg cmd "-f"))
	 ;; (decode-coding-string (base64-decode-string (third msg)) 'undecided)))))
	 (local-path (geben-temp-path-for-fileuri fileuri)))
    (when local-path
      (geben-temp-store local-path (base64-decode-string (third msg)))
      (puthash fileuri (geben-dbgp-source-make fileuri t local-path) geben-dbgp-source-hash)
      (geben-visit-file local-path))))

(defun geben-dbgp-command-feature-get (feature)
  "Send \`feature_get\' command."
  (geben-dbgp-send-command "feature_get" (cons "-n" feature)))

(defun geben-dbgp-response-feature-get (cmd msg)
  "A response message handler for \`feature_get\' command."
  (and t nil))

(defun geben-dbgp-command-feature-set (feature value)
  "Send \`feature_get\' command."
  (geben-dbgp-send-command "feature_set"
			   (cons "-n" feature)
			   (cons "-v" (format "%S" (eval value)))))

(defun geben-dbgp-response-feature-set (cmd msg)
  "A response message handler for \`feature_get\' command."
  (and t nil))

;;; redirect

(defun geben-dbgp-command-stdout (mode)
  "Send `stdout' command."
  (let ((m (plist-get '(nil 0 :disable 0 :redirect 1 :intercept 2) mode)))
    (when (and m)
      (geben-dbgp-send-command "stdout" (cons "-c" m)))))

(defun geben-dbgp-response-stdout (cmd msg)
  "A response message handler for `stdout' command."
  (setq geben-dbgp-redirect-stdout-current
	(case (geben-dbgp-cmd-param-arg cmd "-c")
	  (0 nil)
	  (1 :redirect)
	  (2 :intercept))))

(defun geben-dbgp-command-stderr (mode)
  "Send `stderr' command."
  (let ((m (plist-get '(nil 0 :disable 0 :redirect 1 :intercept 2) mode)))
    (when (and m)
      (geben-dbgp-send-command "stderr" (cons "-c" m)))))

(defun geben-dbgp-response-stderr (cmd msg)
  "A response message handler for `stderr' command."
  (setq geben-dbgp-redirect-stderr-current
	(case (geben-dbgp-cmd-param-arg cmd "-c")
	  (0 nil)
	  (1 :redirect)
	  (2 :intercept))))

;; context

(defun geben-dbgp-command-context-get (context-id &optional depth)
  (geben-dbgp-send-command "context_get"
			   (cons "-c" context-id)
			   (and depth
				(cons "-d" depth))))

(defun geben-dbgp-response-context-get (cmd msg)
  t)

;; property

(defun geben-dbgp-command-property-get (&rest args)
  (apply 'geben-dbgp-send-command "property_get"
	 (mapcar (lambda (key)
		   (let ((arg (plist-get (car args) key)))
		     (when arg
		       (cons (geben-dbgp-cmd-param-for key) arg))))
		 '(:depth :context-id :name :max-data-size :type :page :key :address))))

(defun geben-dbgp-response-property-get (cmd msg)
  t)

;;;

(defun geben-dbgp-prepare-source-file (fileuri)
  "Prepare source file to be in the local machine.
If the counter-file of FILEURI is already known by the current
debugging session, do nothing.  
If `geben-always-use-mirror-file-p' is non-nil or not exists locally, fetch
the file from remote site using \`source\' command then stores in
a GEBEN's temporal directory tree."
  (setq fileuri (geben-dbgp-regularize-fileuri fileuri))
  (unless (geben-dbgp-get-local-path-of fileuri)
    (let ((local-path (geben-temp-path-for-fileuri fileuri)))
      (if (or geben-always-use-mirror-file-p
	      (not (file-exists-p local-path)))
	  ;; haven't fetched remote source yet; fetch it.
	  (geben-dbgp-command-source fileuri)
	;; don't know why but the temporal copy of the remote's source exists.
	(let ((source (geben-dbgp-source-make fileuri t local-path)))
	  (puthash fileuri source geben-dbgp-source-hash)
	  (geben-visit-file (plist-get source :local-path)))))))

(defun geben-dbgp-find-fileuri (local-path)
  "Find fileuri for PATH."
  (block finder
	(maphash (lambda (key source)
		   (when (string= (plist-get source :local-path) local-path)
		     (return-from finder key)))
		   geben-dbgp-source-hash)))
	     
(defun geben-dbgp-get-fileuri-of (local-path)
  (or (geben-dbgp-find-fileuri local-path)
      (let* ((temp-dir (geben-temp-dir))
	     (temp-len (length temp-dir)))
	(concat "file://"
		(if (and (< temp-len (length local-path))
			 (string= temp-dir (substring local-path 0 temp-len)))
		    (substring local-path
			       (- temp-len
				  (if (string< "" (file-name-nondirectory temp-dir)) 0 1)))
		  local-path)))))
  
(defun geben-dbgp-get-local-path-of (fileuri &optional markp)
  "Try to get local path equivalent to FILEURI.
If MARKP is non-nil, this function creates and stores new source
object, which contains information about a source file, to
`geben-dbgp-source-hash'."
  (let ((source (gethash fileuri geben-dbgp-source-hash)))
    (if source
	(plist-get source :local-path)
      ;; not konwn for the current session.
      (let ((local-path (geben-make-local-path fileuri)))
	(when (and (not geben-always-use-mirror-file-p)
		   (file-exists-p local-path))
	  (when (and markp
		     (not (gethash fileuri geben-dbgp-source-hash)))
	    (puthash fileuri (geben-dbgp-source-make fileuri nil local-path) geben-dbgp-source-hash))
	  local-path)))))

;; gud

(defcustom geben-dbgp-command-line "debugclient -p 9000"
  "*Command line string to execute DBGp client."
  :type 'string
  :group 'gud)

(defcustom geben-dbgp-process-hook 'geben-dbgp-entry
  "*Hook running at each dbgp protocol message.
Each hook functions is called with one argument XML which is a
XMLized dbgp protocol message."
  :type 'hook
  :group 'geben)

(defvar geben-dbgp-disconnect-hook nil
  "Hook running at when the session connection is disconnected.")

(defun geben-dbgp-process-chunk (xml)
  "Process a DBGp response chunk."
  (run-hook-with-args 'geben-dbgp-process-hook (car-safe xml)))

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the filter.
(defun geben-dbgp-marker-filter (string)
  "Process DBGp response STRING.
Parse STRING, find xml chunks, convert them to xmlized lisp objects
and call `geben-dbgp-process-chunk' with each chunk."
  (setq gud-marker-acc (concat gud-marker-acc (delete ?\r string)))
  (let (xml-list disconnectp
		 (output ""))
    (flet ((parse-xml (str)
                      (with-temp-buffer
                        (insert str)
                        (ignore-errors (xml-parse-region (point-min) (point-max)))))
           (xmlize (offset)
                   (if (string-match "<\\?xml" gud-marker-acc offset)
		       (let* ((beg (match-beginning 0))
			      (end (and (string-match "^\\((cmd)\\|<\\?xml\\)" gud-marker-acc (1+ beg))
					(match-beginning 0))))
			 (if (null end)
			     beg
			   (let ((xml (parse-xml (substring gud-marker-acc beg end))))
			     (when xml
			       (add-to-list 'xml-list xml t))
			     (xmlize end))))
		     (when (string-match "^Disconnect\\n*" gud-marker-acc offset)
		       (setq disconnectp t)
		       (match-end 0)))))
      (setq output
            (let ((acc-pos (xmlize 0)))
              ;; Does the remaining text look like it might end with the
              ;; beginning of another marker?  If it does, then keep it in
              ;; gud-marker-acc until we receive the rest of it.  Since we
              ;; know the full marker regexp above failed, it's pretty simple to
              ;; test for marker starts.
              (if acc-pos
                  (prog1
                      ;; Everything before the potential marker start can be output.
                      (substring gud-marker-acc 0 acc-pos)
                    (setq gud-marker-acc
                          (substring gud-marker-acc acc-pos)))
                ;; Everything after, we save, to combine with later input.
                (prog1
                    gud-marker-acc
                  (setq gud-marker-acc "")))))
      (mapc #'geben-dbgp-process-chunk xml-list))
    (when disconnectp
      (run-hooks 'geben-dbgp-disconnect-hook))
    output))

(defun geben-dbgp-find-file (path)
  "Visit debuggee file specified by PATH.
After visited it invokes `geben-after-visit-hook'."
  (let ((buffer (or (find-buffer-visiting path)
		    (and (file-exists-p path)
			 (find-file-noselect path)))))
    (when buffer
      (prog1
	  (geben-dbgp-display-window buffer)
	(run-hook-with-args 'geben-after-visit-hook buffer)))))

(defun geben-dbgp-massage-args (file args)
  args)

(defun geben-dbgp-indicate-current-line (fileuri lineno &optional display-bufferp)
  "Display indication marker at the current breaking point.
if DISPLAY-BUFFERP is non-nil, the buffer contains the breaking point
will be displayed in a window."
  (let ((local-path (geben-dbgp-get-local-path-of
		     (geben-dbgp-regularize-fileuri fileuri) t)))
    (if local-path
	(prog1
	    (geben-dbgp-indicate-current-line-1 local-path lineno)
	  (when display-bufferp
	    (gud-display-frame)))
      (geben-dbgp-cmd-sequence
       (geben-dbgp-command-source fileuri)
       `(lambda (cmd msg err)
	  (when (not err)
	    (geben-dbgp-indicate-current-line-1
	     (geben-dbgp-get-local-path-of ,fileuri) ,lineno)
	    (gud-display-frame))))
      nil)))

(defun geben-dbgp-indicate-current-line-1 (local-path lineno)
  "Display current debugging position marker."
  (let ((lineno-1 (cond
		   ((numberp lineno)
		    lineno)
		   ((stringp lineno)
		    (string-to-number lineno)))))
    (when lineno-1
      (when (floatp lineno-1)
	(setq lineno-1 1)) ;; restrict to integer
      (setq gud-last-frame
	    (cons local-path lineno-1))
      (message "stopped: %s(%S)"
	       (file-name-nondirectory local-path) lineno-1))))

(defun geben-dbgp-buffer-killed()
  (geben-dbgp-reset)
  (message "GEBEN is terminated."))

(defun geben-dbgp (&optional command-line)
  "Run a DBGp client program.
If the optional argument COMMAND-LINE is nil, the value of
`geben-dbgp-command-line' is used."
  (interactive "P")
  (save-window-excursion
    (when (and gud-comint-buffer
	       (buffer-name gud-comint-buffer))
      (kill-buffer gud-comint-buffer))
    (gud-common-init geben-dbgp-command-line 'geben-dbgp-massage-args
		     'geben-dbgp-marker-filter 'geben-dbgp-find-file)
    (with-current-buffer gud-comint-buffer
      (rename-buffer geben-process-buffer-name t)
      (and (fboundp 'set-process-query-on-exit-flag)
	   (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil))
      (add-hook 'kill-buffer-hook 'geben-dbgp-buffer-killed nil t))

    (set (make-local-variable 'gud-minor-mode) 'geben)
    ;;  (gud-def gud-break  "b %l"         "\C-b" "Set breakpoint at current line.")
    ;;  (gud-def gud-remove "d %l"         "\C-d" "Remove breakpoint at current line")
    ;;  (gud-def gud-step   "s"            "\C-s" "Step one source line with display.")
    ;;  (gud-def gud-next   "n"            "\C-n" "Step one line (skip functions).")
    ;;  (gud-def gud-cont   "c"            "\C-r" "Continue with display.")
    ;;  (gud-def gud-finish "finish"       "\C-f" "Finish executing current function.")
    ;;  (gud-def gud-up     "up %p"        "<" "Up N stack frames (numeric arg).")
    ;;  (gud-def gud-down   "down %p"      ">" "Down N stack frames (numeric arg).")
    ;;  (gud-def gud-print  "%e"           "\C-p" "Evaluate perl expression at point.")
    (setq comint-prompt-regexp "^(cmd) ")
    (setq paragraph-start comint-prompt-regexp)
    (run-hooks 'geben-mode-hook))
  (message "Waiting for debug server to connect."))

;;-------------------------------------------------------------
;;  miscellaneous functions
;;-------------------------------------------------------------

;; temporary directory

(defun geben-temp-dir ()
  "Get a temporary directory path for a GEBEN session."
  (let ((base-dir (file-truename (expand-file-name "emacs-geben"
						   geben-temporary-file-directory))))
    (unless (file-exists-p base-dir)
      (make-directory base-dir t)
      (set-file-modes base-dir 1023))
    (expand-file-name (format "%d" (emacs-pid)) base-dir)))

(defun geben-temp-path-for-fileuri (fileuri)
  "Generate path string from FILEURI to store files temporarily."
  (let ((path (geben-make-local-path fileuri)))
    (if path
	(expand-file-name (substring path 1) (geben-temp-dir))
      fileuri)))

(defun geben-temp-store (path source)
  "Store temporary file."
  (make-directory (file-name-directory path) t)
  (ignore-errors
    (with-current-buffer (or (find-buffer-visiting path)
			     (create-file-buffer path))
      (widen)
      (erase-buffer)
      (font-lock-mode 0)
      (let ((encoding (detect-coding-string source t)))
	(unless (eq 'undecided encoding)
	  (set-buffer-file-coding-system encoding))
	(insert (decode-coding-string source encoding)))
      (with-temp-message ""
	(write-file path)
	(kill-buffer (current-buffer))))
    t))

(defun geben-delete-directory-tree (base-path)
  "Delete directory tree."
  (if (file-directory-p base-path)
      (progn
	(mapc (lambda (name)
		(let ((fullpath (expand-file-name name base-path)))
		  (cond
		   ((equal name ".") t)
		   ((equal name "..") t)
		   ((or (file-symlink-p fullpath)
			(file-regular-p fullpath))
		    (delete-file fullpath))
		   ((file-directory-p fullpath)
		    (geben-delete-directory-tree fullpath)))))
	      (directory-files base-path nil nil t))
	(delete-directory base-path))))

;; path

(defun geben-make-local-path (fileuri)
  "Make a path string correspond to FILEURI."
  (when (string-match "^\\(file\\|https?\\):/+" fileuri)
    (let ((path (substring fileuri (1- (match-end 0)))))
      (setq path (or (and (eq system-type 'windows-nt)
			  (require 'url-util)
			  (url-unhex-string path))
		     path))
      (if (string= "" (file-name-nondirectory path))
	  (expand-file-name (geben-generate-default-file-name) path)
	path))))

(defun geben-generate-default-file-name ()
  (case geben-dbgp-target-language
    (:php "index.php")
    (:python "index.py")
    (:perl "index.pl")
    (:ruby "index.rb")
    (t "index.html")))

;; source code file

(defun geben-visit-file (path)
  "Visit to a local source code file."
  (when (file-exists-p path)
    (let ((buf (find-file-noselect path)))
      (geben-dbgp-display-window buf)
      (run-hook-with-args 'geben-after-visit-hook buf)
      buf)))

(defun geben-enter-geben-mode (buf)
  (geben-mode 1))

;; utility

(defun geben-flatten (x)
  "Make cons X to a flat list."
  (flet ((rec (x acc)
	      (cond ((null x) acc)
		    ((atom x) (cons x acc))
		    (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun geben-what-line (&optional pos)
  "Get the number of the line in which POS is located.
If POS is omitted, then the current position is used."
  (save-restriction
    (widen)
    (save-excursion
      (if pos (goto-char pos))
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

;;
;; interactive commands
;;

;;;###autoload
(defun geben (&optional quit)
  "Start GEBEN, a PHP source level debugger.
Prefixed with \\[universal-argument], GEBEN quits immediately.

GEBEN communicates with script servers, located anywhere local or
remote, in DBGp protocol (e.g. PHP with Xdebug extension)
to help you debugging your script with some valuable features:
 - continuation commands like \`step in\', \`step out\', ...
 - a kind of breakpoints like \`line no\', \`function call\' and
   \`function return\'.
 - evaluation
 - stack dump
 - etc.

The script servers should be DBGp protocol enabled.
Ask to your script server administrator about this setting up
issue.

The variable `geben-dbgp-command-line' is a command line to
execute a DBGp protocol client command. GEBEN communicates with
script servers through this command.

Once you've done these setup operation correctly, run GEBEN first
and your script on your script server second. After some
negotiation GEBEN will display your script's entry source code.
The debugging session is started.

In the debugging session the source code buffers are under the
minor mode  `geben-mode'. Key mapping and other information is
described its help page."
  (interactive "P")
  (if quit
      (and gud-comint-buffer
	   (buffer-name gud-comint-buffer)
	   (kill-buffer gud-comint-buffer))
    (geben-dbgp)))

;;-------------------------------------------------------------
;;  geben-mode
;;-------------------------------------------------------------

(defvar geben-mode-map nil)
(unless geben-mode-map
  (setq geben-mode-map (make-sparse-keymap "geben"))
  ;; control
  (define-key geben-mode-map " " 'geben-step-again)
  (define-key geben-mode-map "g" 'geben-run)
  ;;(define-key geben-mode-map "G" 'geben-Go-nonstop-mode)
  (define-key geben-mode-map "t" 'geben-set-redirect)
  ;;(define-key geben-mode-map "T" 'geben-Trace-fast-mode)
  ;;(define-key geben-mode-map "c" 'geben-continue-mode)
  ;;(define-key geben-mode-map "C" 'geben-Continue-fast-mode)

  ;;(define-key geben-mode-map "f" 'geben-forward) not implemented
  ;;(define-key geben-mode-map "f" 'geben-forward-sexp)
  ;;(define-key geben-mode-map "h" 'geben-goto-here)

  ;;(define-key geben-mode-map "I" 'geben-instrument-callee)
  (define-key geben-mode-map "i" 'geben-step-into)
  (define-key geben-mode-map "o" 'geben-step-over)
  (define-key geben-mode-map "r" 'geben-step-out)

  ;; quitting and stopping
  (define-key geben-mode-map "q" 'geben-stop)
  ;;(define-key geben-mode-map "Q" 'geben-top-level-nonstop)
  ;;(define-key geben-mode-map "a" 'abort-recursive-edit)
  (define-key geben-mode-map "v" 'geben-display-context)

  ;; breakpoints
  (define-key geben-mode-map "b" 'geben-set-breakpoint-line)
  (define-key geben-mode-map "B" 'geben-breakpoint-menu)
  (define-key geben-mode-map "u" 'geben-unset-breakpoint-line)
  (define-key geben-mode-map "\C-cb" 'geben-breakpoint-list)
  ;;(define-key geben-mode-map "B" 'geben-next-breakpoint)
  ;;(define-key geben-mode-map "x" 'geben-set-conditional-breakpoint)
  ;;(define-key geben-mode-map "X" 'geben-set-global-break-condition)

  ;; evaluation
  (define-key geben-mode-map "e" 'geben-eval-expression)
  ;;(define-key geben-mode-map "\C-x\C-e" 'geben-eval-last-sexp)
  ;;(define-key geben-mode-map "E" 'geben-visit-eval-list)

  ;; views
  (define-key geben-mode-map "w" 'geben-where)
  ;;(define-key geben-mode-map "v" 'geben-view-outside) ;; maybe obsolete??
  ;;(define-key geben-mode-map "p" 'geben-bounce-point)
  ;;(define-key geben-mode-map "P" 'geben-view-outside) ;; same as v
  ;;(define-key geben-mode-map "W" 'geben-toggle-save-windows)

  ;; misc
  (define-key geben-mode-map "?" 'geben-mode-help)
  (define-key geben-mode-map "d" 'geben-backtrace)

  ;;(define-key geben-mode-map "-" 'negative-argument)

  ;; statistics
  ;;(define-key geben-mode-map "=" 'geben-temp-display-freq-count)

  ;; GUD bindings
  (define-key geben-mode-map "\C-c\C-s" 'geben-step-into)
  (define-key geben-mode-map "\C-c\C-n" 'geben-step-over)
  (define-key geben-mode-map "\C-c\C-c" 'geben-run)

  (define-key geben-mode-map "\C-x " 'geben-set-breakpoint-line)
  (define-key geben-mode-map "\C-c\C-d" 'geben-unset-breakpoint-line)
  (define-key geben-mode-map "\C-c\C-t" 'geben-set-breakpoint-line)
  (define-key geben-mode-map "\C-c\C-l" 'geben-where))

;;;###autoload
(define-minor-mode geben-mode
  "Minor mode for debugging source code with GEBEN.
The geben-mode buffer commands:
\\{geben-mode-map}"
  nil " *debugging*" geben-mode-map
  (setq buffer-read-only geben-mode))
  
(add-hook 'kill-emacs-hook
	  (lambda ()
	    (geben-dbgp-reset)))

(defun geben-mode-help ()
  "Display description and key bindings of `geben-mode'."
  (interactive)
  (describe-function 'geben-mode))

(defvar geben-step-type :step-into
  "Step command of what `geben-step-again' acts.
This value remains the last step command type either
`:step-into' or `:step-out'.")

(defun geben-step-again ()
  "Do either `geben-step-into' or `geben-step-over' what the last time called.
Default is `geben-step-into'."
  (interactive)
  (case geben-step-type
    (:step-over (geben-step-over))
    (:step-into (geben-step-into))
    (t (geben-step-into))))
     
(defun geben-step-into ()
  "Step into the definition of the function or method about to be called.
If there is a function call involved it will break on the first
statement in that function"
  (interactive)
  (setq geben-step-type :step-into)
  (geben-dbgp-command-step-into))

(defun geben-step-over ()
  "Step over the definition of the function or method about to be called.
If there is a function call on the line from which the command
is issued then the debugger engine will stop at the statement
after the function call in the same scope as from where the
command was issued"
  (interactive)
  (setq geben-step-type :step-over)
  (geben-dbgp-command-step-over))

(defun geben-step-out ()
  "Step out of the current scope.
It breaks on the statement after returning from the current
function."
  (interactive)
  (geben-dbgp-command-step-out))

(defun geben-run ()
  "Start or resumes the script.
It will break at next breakpoint, or stops at the end of the script."
  (interactive)
  (geben-dbgp-command-run))

(defun geben-stop ()
  "End execution of the script immediately."
  (interactive)
  (geben-dbgp-command-stop))

(defun geben-breakpoint-menu (arg)
  "Set a breakpoint interactively.
Script debugger engine may support a kind of breakpoints, which
will be stored in the variable `geben-dbgp-breakpoint-types'
after a debugging session is started.

This command asks you a breakpoint type and its options.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-breakpoint-menu] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-breakpoint-menu]), \
this command will also ask a
hit-value interactively.
"
  (interactive "P")
  (let ((candidates (remove nil
			    (mapcar
			     (lambda (x)
			       (if (member (car x) geben-dbgp-breakpoint-types) x))
			     '((:line . "l)Line")
			       (:call . "c)Call")
			       (:return . "r)Return")
			       (:exception . "e)Exception")
			       (:conditional . "d)Conditional")
			       (:watch . "w)Watch"))))))
    (when (null candidates)
      (error "No breakpoint type is supported by the debugger engine."))
    (let* ((c (read-char (concat "Breakpoint type: "
				 (mapconcat
				  (lambda (x)
				    (cdr x))
				  candidates " "))))
	   (x (find-if (lambda (x)
			 (eq c (elt (cdr x) 0)))
		       candidates))
	   (fn (and x
		    (intern-soft (concat "geben-set-breakpoint-"
					 (substring (symbol-name (car x)) 1))))))
      (unless x
	(error "Cancelled"))
      (if (fboundp fn)
	  (call-interactively fn)
	(error (concat (symbol-name fn) " is not implemented."))))))

(defun geben-set-breakpoint-common (hit-value cmd)
  (setq hit-value (if (and (not (null hit-value))
			   (listp hit-value))
		      (if (fboundp 'read-number)
			  (read-number "Number of hit to break: ")
			(string-to-number
			 (read-string "Number of hit to break: ")))
		    hit-value))
  (plist-put cmd :hit-value (if (and (numberp hit-value)
				     (<= 0 hit-value))
				hit-value
			      0))
  (geben-dbgp-command-breakpoint-set cmd))

(defun geben-set-breakpoint-line (fileuri lineno &optional hit-value)
  "Set a breakpoint at the current line.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-line] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-line]), \
this command will also ask a
hit-value interactively."
  (interactive (list nil nil current-prefix-arg))
  (let ((local-path (if fileuri
			(geben-dbgp-get-local-path-of fileuri)
		      (buffer-file-name (current-buffer)))))
    (geben-set-breakpoint-common hit-value
				 (geben-dbgp-bp-make
				  :line
				  :fileuri (or fileuri
					       (geben-dbgp-find-fileuri local-path)
					       (geben-dbgp-find-fileuri (file-truename local-path))
					       (geben-dbgp-get-fileuri-of (file-truename local-path)))
				  :lineno (or (numberp lineno)
					      (geben-what-line))
				  :local-path local-path
				  :overlay t))))

(defvar geben-set-breakpoint-call-history nil)
(defvar geben-set-breakpoint-fileuri-history nil)
(defvar geben-set-breakpoint-exception-history nil)
(defvar geben-set-breakpoint-condition-history nil)

(defun geben-set-breakpoint-call (name &optional fileuri hit-value)
  "Set a breakpoint to break at when entering function/method named NAME.
For a class method, specify NAME like \"MyClass::MyMethod\".
For an instance method, do either like \"MyClass::MyMethod\" or
\"MyClass->MyMethod\".
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-call] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-call]),
this command will also ask a
hit-value interactively."
  (interactive (list
		(read-string "Name: " ""
			     'geben-set-breakpoint-call-history)
		(unless (member geben-dbgp-target-language '(:php :ruby))
		  ;; at this present some debugger engines' implementation is buggy:
		  ;; some requires fileuri and some don't accept it.
		  (read-string "fileuri: "
			       (geben-dbgp-get-fileuri-of
				(file-truename (buffer-file-name (current-buffer))))
			       'geben-set-breakpoint-fileuri-history))
		current-prefix-arg))
  (when (string< "" name)
    (geben-set-breakpoint-common hit-value
				 (geben-dbgp-bp-make :call
						     :function name
						     :fileuri fileuri))))

(defun geben-set-breakpoint-return (name &optional fileuri hit-value)
  "Set a breakpoint to break after returned from a function/method named NAME.
For a class method, specify NAME like \"MyClass::MyMethod\".
For an instance method, do either like \"MyClass::MyMethod\" or
\"MyClass->MyMethod\".
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-return] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-return]),
this command will also ask a
hit-value interactively."
  (interactive (list
		(read-string "Name: " ""
			     'geben-set-breakpoint-call-history)
		(unless (member geben-dbgp-target-language '(:php :ruby))
		  ;; at this present some debugger engines' implementations are buggy:
		  ;; some requires fileuri and some don't accept it.
		  (read-string "fileuri: "
			       (geben-dbgp-get-fileuri-of
				(file-truename (buffer-file-name (current-buffer))))
			       'geben-set-breakpoint-fileuri-history))
		current-prefix-arg))
  (when (string< "" name)
    (geben-set-breakpoint-common hit-value
				 (geben-dbgp-bp-make :return
						     :function name
						     :fileuri fileuri))))

(defun geben-set-breakpoint-exception (name &optional hit-value)
  "Set a breakpoint to break at when an exception named NAME is occurred.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-exception] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-exception]),
this command will also ask a
hit-value interactively."
  (interactive (list
		(read-string "Exception type: "
			     "Exception"
			     'geben-set-breakpoint-exception-history)
		current-prefix-arg))
  (geben-set-breakpoint-common hit-value
			       (geben-dbgp-bp-make :exception
						   :exception name)))
   
(defun geben-set-breakpoint-conditional (expr fileuri &optional lineno hit-value)
  "Set a breakpoint to break at when the expression EXPR is true in the file FILEURI.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-conditional] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-conditional]),
this command will also ask a
hit-value interactively."
  (interactive (list
		(read-string "Expression: " ""
			     'geben-set-breakpoint-condition-history)
		(geben-dbgp-get-fileuri-of
		 (file-truename (buffer-file-name (current-buffer))))
		(read-string "Line number to evaluate (blank means entire file): "
			     (number-to-string (geben-what-line)))
		current-prefix-arg))
  
  (geben-set-breakpoint-common hit-value
			       (geben-dbgp-bp-make :conditional
						   :expression expr
						   :fileuri fileuri
						   :lineno (and (stringp lineno)
								(string-match "^[0-9]+$" lineno)
								(string-to-number lineno)))))

(defun geben-set-breakpoint-watch (expr &optional hit-value)
  "Set a breakpoint to break on write of the variable or address.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-conditional] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-conditional]),
this command will also ask a
hit-value interactively."
  (interactive (list
		(read-string "Expression: " ""
			     'geben-set-breakpoint-condition-history)
		current-prefix-arg))
  (geben-set-breakpoint-common hit-value
			       (geben-dbgp-bp-make :watch
						   :expression expr)))

(defun geben-unset-breakpoint-line ()
  "Clear a breakpoint set at the current line."
  (interactive)
  (geben-dbgp-command-breakpoint-remove))

(defun geben-breakpoint-list ()
  "Display breakpoint list.
The breakpoint list buffer is under `geben-breakpoint-list-mode'.
Key mapping and other information is described its help page."
  (interactive)
  (geben-dbgp-breakpoint-list t))

(defvar geben-eval-history nil)

(defun geben-eval-expression (expr)
  "Evaluate a given string EXPR within the current execution context."
  (interactive
   (progn
     (list (read-from-minibuffer "Eval: "
				 nil nil nil 'geben-eval-history))))
  (geben-dbgp-command-eval expr))

(defun geben-open-file (fileuri)
  "Open a debugger server side file specified by FILEURI.
FILEURI forms like as \`file:///path/to/file\'."
  (interactive (list (read-string "Open file: " "file://")))
  (geben-dbgp-command-source fileuri))

(defun geben-backtrace ()
  "Display backtrace list.
The backtrace list buffer is under `geben-backtrace-mode'.
Key mapping and other information is described its help page."
  (interactive)
  (geben-dbgp-backtrace))

(defun geben-set-redirect (target &optional arg)
  "Set the debuggee script's output redirection mode.
This command enables you to redirect the debuggee script's output to GEBEN.
You can select redirection target from \`stdout', \`stderr' and both of them.
Prefixed with \\[universal-argument], you can also select redirection mode
from \`redirect', \`intercept' and \`disabled'."
  (interactive (list (case (read-char "Redirect: o)STDOUT e)STRERR b)Both")
		       (?o :stdout)
		       (?e :stderr)
		       (?b :both))
		     current-prefix-arg))
  (unless target
    (error "Cancelled"))
  (let ((mode (if arg
		  (case (read-char "Mode: r)Redirect i)Intercept d)Disable")
		    (?r :redirect)
		    (?i :intercept)
		    (?d :disable))
		:redirect)))
    (unless mode
      (error "Cancelled"))
    (when (memq target '(:stdout :both))
      (geben-dbgp-command-stdout mode))
    (when (memq target '(:stderr :both))
      (geben-dbgp-command-stderr mode))))

(defun geben-display-context (&optional depth)
  (interactive (list (cond
		      ((null current-prefix-arg) 0)
		      ((numberp current-prefix-arg)
		       current-prefix-arg)
		      ((listp current-prefix-arg)
		       (if (fboundp 'read-number)
			   (read-number "Depth: " 0)
			 (string-to-number (read-string "Depth: " "0"))))
		      (t nil))))
  (geben-dbgp-context-display (or depth 0)))

(provide 'geben)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; geben.el ends here
