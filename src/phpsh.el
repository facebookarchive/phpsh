;open all files read-only
;(add-hook 'find-file-hook '(lambda () (setq buffer-read-only t)))

;do not split the window when loading files in geben
(setq geben-display-window-function 'switch-to-buffer)

(defun make-all-frames-visible ()
  (dolist (frame (frame-list))
    (if (not (eq (frame-visible-p frame) t))
        (make-frame-visible frame)
      nil)))

(add-hook 'geben-session-starting-hook
          '(lambda ()
             (set-face-background 'default active-bg)
             (set-face-background 'fringe "grey75")
             (set-face-foreground 'fringe "magenta")))

(add-hook 'geben-session-finished-hook
          '(lambda ()
             (set-face-background 'default inactive-bg)
             (set-face-background 'fringe inactive-bg)
             (set-face-foreground 'fringe inactive-bg)))

(setq blink-matching-paren t)
(setq blink-matching-paren-on-screen t)
(show-paren-mode 1)
(global-font-lock-mode 1)
(setq case-fold-search t)
(set-fringe-mode (quote (nil . 0)))

(setq auto-mode-alist (append '(("\\.php\\'" . c-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.phpt$" . c-mode)) auto-mode-alist))
