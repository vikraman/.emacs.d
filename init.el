;;; init.el --- emacs init
;;; Commentary:

;;; Code:

;; Setup packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Setup `use-package'

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq inhibit-default-init t)

;; Emacs customizations

;; Shorter yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Remove annoying defaults
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

(set-default 'tags-case-fold-search nil)

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Better defaults
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(transient-mark-mode 1)
(delete-selection-mode 1)

;; Put autosave files (ie #foo#) and backup files (ie foo~) into a cache dir
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.cache/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.cache/backups/"))))

;; Put session backups into the cache directory
(setq auto-save-list-file-prefix "~/.emacs.cache/auto-save-list/.saves-")

;; utf-8
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; unicode fonts
(use-package unicode-fonts
  :ensure t
  :config (unicode-fonts-setup))

;; Ending newline
(setq require-final-newline t)

;; Disable the damn tabs
(setq-default indent-tabs-mode nil)

;; Do not fsync on save since I'm on an ssd
(setq write-region-inhibit-fsync t)

;; Browser
(setq browse-url-browser-function 'browse-url-xdg-open)

;; Buffer switching
(defun switch-to-previous-buffer ()
  "Switch to last active buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key [C-M-tab] 'switch-to-previous-buffer)

;; Clipboard integration
(setq mouse-drag-copy-region nil)
(setq select-enable-primary t)
(setq select-enable-clipboard t)
(setq select-active-regions t)
(global-set-key [mouse-2] 'mouse-yank-primary)

;; Global zoom and bindings
(define-globalized-minor-mode
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))
(defun global-text-scale-adjust (inc) (interactive)
       "Adjust global zoom"
       (text-scale-set 1)
       (kill-local-variable 'text-scale-mode-amount)
       (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
       (global-text-scale-mode 1)
       )
(global-set-key (kbd "M-0")
                '(lambda () (interactive)
                   (global-text-scale-adjust (- text-scale-mode-amount))
                   (global-text-scale-mode -1)))
(global-set-key (kbd "M-+")
                '(lambda () (interactive) (global-text-scale-adjust 1)))
(global-set-key (kbd "M--")
                '(lambda () (interactive) (global-text-scale-adjust -1)))

;; Misc keybindings
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; When running as a daemon, set $DISPLAY
(when (daemonp)
  (if (null (getenv "DISPLAY"))
      (setenv "DISPLAY" ":0.0")))

;; Set frame title
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; Welcome message
(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message "\
;;   _____ _   _ _    _   ______
;;  / ____| \\ | | |  | | |  ____|
;; | |  __|  \\| | |  | | | |__   _ __ ___   __ _  ___ ___
;; | | |_ | . ` | |  | | |  __| | '_ ` _ \\ / _` |/ __/ __|
;; | |__| | |\\  | |__| | | |____| | | | | | (_| | (__\\__ \\
;;  \\_____|_| \\_|\\____/  |______|_| |_| |_|\\__,_|\\___|___/


")

;; (defconst animate-n-steps 5)
;; (defun emacs-reloaded ()
;;   "Animate Emacs."
;;   (animate-string (substring (emacs-version) 0 16) 0 0)
;;   (figlet-figletify-region 1 (buffer-size)))

;; Confirm on exit
(setq confirm-kill-emacs #'yes-or-no-p)

;; Set PATH from shell
(setenv "EMACS" "1")
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;; Theme
(use-package solarized-theme
  :ensure t
  :if (display-graphic-p)
  :config
  (load-theme 'solarized-dark t))

(use-package zenburn-theme
  :ensure t
  :if (not (display-graphic-p))
  :config (load-theme 'zenburn t))

;; Use the Source Code Pro font
(setq default-frame-alist '((font . "Source Code Pro SemiBold-10")))

;; Use different custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Custom packages

(use-package dart-init :defer t :load-path "elisp/")
(use-package haskell-init :load-path "elisp/")
(use-package mu4e-init :load-path "elisp/")
(use-package sp-init :load-path "elisp/")
(use-package tex-init :defer t :load-path "elisp/")
(use-package utils :load-path "elisp/")

(provide 'init)
;;; init.el ends here
