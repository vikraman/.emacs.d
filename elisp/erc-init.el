;;; erc-init.el --- Emacs Prelude: ERC mode configuration.
;;
;;; Commentary:

;;; Code:

(require 'use-package)

(use-package erc

  :config
  ;; Interpret mIRC-style color commands in IRC chats
  (setq erc-interpret-mirc-color t)

  ;; Kill buffers for channels after /part
  (setq erc-kill-buffer-on-part t)

  ;; Kill buffers for private queries after quitting the server
  (setq erc-kill-queries-on-quit t)

  ;; Kill buffers for server messages after quitting the server
  (setq erc-kill-server-buffer-on-quit t)

  ;; open query buffers in the current window
  (setq erc-query-display 'buffer)

  ;; set erc-fill-column based on buffer size
  (make-variable-buffer-local 'erc-fill-column)
  (add-hook 'window-configuration-change-hook
            '(lambda ()
               (save-excursion
                 (walk-windows
                  (lambda (w)
                    (let ((buffer (window-buffer w)))
                      (set-buffer buffer)
                      (when (eq major-mode 'erc-mode)
                        (setq erc-fill-column (- (window-width w) 2)))))))))

  ;; static left align for nicks
  (setq erc-fill-function 'erc-fill-static)
  (setq erc-fill-static-center 24)

  ;; misc stuff
  (setq erc-prompt ">"
        erc-nick '("vikraman" "vikraman_"))

  ;; update modules
  (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules)))

  ;; autoaway setup
  (setq erc-auto-discard-away t)
  (setq erc-autoaway-idle-seconds 600)
  (defvar erc-autoaway-use-emacs-idle t)

  ;; utf-8 always and forever
  (setq erc-server-coding-system '(utf-8 . utf-8))

  ;; always reconnect
  (setq erc-server-auto-reconnect t)
  (setq erc-server-reconnect-attempts t))

(use-package erc-log

  :init
  (progn
    (setq erc-log-channels-directory "~/.erc/logs/")
    (if (not (file-exists-p erc-log-channels-directory))
        (mkdir erc-log-channels-directory t))

    (erc-log-enable)

    ;; erc-view-log
    (add-to-list 'auto-mode-alist
                 `(,(format "%s/.*\\.log"
                            (regexp-quote
                             (expand-file-name erc-log-channels-directory)))
                   . erc-view-log-mode))

    (add-hook 'erc-view-log-mode-hook 'turn-on-auto-revert-tail-mode)
    (setq erc-save-buffer-on-part t)
    ;; FIXME - this advice is wrong and is causing problems on Emacs exit
    ;; (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
    ;;   (save-some-buffers t (lambda () (when (eq major-mode 'erc-mode) t))))
    ))

(use-package erc-spelling

  :init (erc-spelling-mode t))

(use-package erc-autoaway

  :config (add-to-list 'erc-modules 'autoaway))
(use-package erc-desktop-notifications

  :config (add-to-list 'erc-modules 'notifications))
(use-package erc-image

  :config (add-to-list 'erc-modules 'image))
(use-package erc-tweet

  :config (add-to-list 'erc-modules 'tweet))
(use-package erc-youtube

  :config (add-to-list 'erc-modules 'youtube))
(use-package erc-colorize

  :config (add-to-list 'erc-modules 'colorize))
(use-package erc-crypt)
(use-package erc-view-log)
(use-package erc-goodies

  :init
  (progn
    (erc-truncate-mode +1)
    ;; exclude boring stuff from tracking
    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                    "324" "329" "332" "333" "353" "477"))
    (setq erc-current-nick-highlight-type 'nick)
    (setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))
    (setq erc-track-use-faces t)
    (setq erc-track-faces-priority-list '(erc-error-face
                                          erc-current-nick-face
                                          erc-keyword-face
                                          erc-nick-msg-face
                                          erc-direct-msg-face
                                          erc-dangerous-host-face
                                          erc-notice-face
                                          erc-prompt-face))
    (setq erc-track-priority-faces-only 'all)
    (erc-track-mode t))
  :config
  (add-to-list 'erc-modules 'smiley)
  (add-to-list 'erc-modules 'move-to-prompt)
  (add-to-list 'erc-modules 'keep-place)
  (add-to-list 'erc-modules 'irccontrols))
(use-package erc-speedbar)
(use-package sr-speedbar)

;; TODO: mark emacs frame as urgent
;; (add-hook 'erc-server-PRIVMSG-functions (lambda (proc parsed) (x-urgent) nil))
;; (add-hook 'erc-text-matched-hook (lambda (match-type nickuserhost msg) (x-urgent) nil))

;; urgency hint for Emacs frame
(defun x-urgency-hint (frame arg &optional source)
  "Set the x-urgency hint for the FRAME to ARG:

- If arg is nil, unset the urgency.
- If arg is any other value, set the urgency from SOURCE."
  (let* ((wm-hints (append (x-window-property
                            "WM_HINTS" frame "WM_HINTS"
                            source nil t) nil))
         (flags (car wm-hints)))
                                        ; (message flags)
    (setcar wm-hints
            (if arg
                (logior flags #x00000100)
              (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun x-urgent (&optional arg)
  "Mark the current Emacs frame as requiring urgent attention.
With a prefix argument ARG which does not equal a boolean value
of nil, remove the urgency flag (which might or might not change
display, depending on the window manager)."
  (interactive "P")
  (let (frame (car (car (cdr (current-frame-configuration)))))
    (x-urgency-hint frame (not arg))))

(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (load "~/.emacs.d/.erc-auth")
  (when (y-or-n-p "Do you want to start IRC? ")
    (start-irc-with-auth))
  (sr-speedbar-open)
  (erc-speedbar-browser))

(defun filter-server-buffers ()
  (delq nil
        (mapcar
         (lambda (x) (and (erc-server-buffer-p x) x))
         (buffer-list))))

(defun stop-irc ()
  "Disconnects from all irc servers."
  (interactive)
  (dolist (buffer (filter-server-buffers))
    (message "Server buffer: %s" (buffer-name buffer))
    (with-current-buffer buffer
      (erc-quit-server "Asta la vista"))))

(provide 'erc-init)

;;; erc-init.el ends here
