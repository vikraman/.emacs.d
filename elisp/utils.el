;;; utils.el -- utils
;;; Commentary:

;;; Code:

(require 'use-package)

;; ace-jump-mode
(use-package ace-jump-mode
  :ensure t
  :config (ace-jump-mode-enable-mark-sync)
  :bind (("C-." . ace-jump-mode)
         ("C-," . ace-jump-mode-pop-mark)))

;; auto-fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; company-mode
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (add-hook 'after-init-hook 'global-company-mode))

;; TeX
(load "tex-init")

;; ag -- the silver searcher
(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-window 't)
  (setq ag-reuse-buffers 't))

;; css-mode
(use-package css-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook (setq css-indent-offset 2)))

;; dired
(use-package dired+ :ensure t)
(use-package dired-details
  :ensure t
  :init (dired-details-install))
(use-package dired-details+ :ensure t)

;; edit-server
(use-package edit-server
  :ensure t
  :if (and (window-system) (daemonp))
  :init (edit-server-start))

;; expand-region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; agda2-mode
(use-package agda2
  :config
  (add-hook 'agda2-mode-hook
            '(lambda ()
               (setq agda2-include-dirs '("." "/usr/share/agda-stdlib"))))
  (setq agda2-highlight-set-faces nil)
  (defface agda2-highlight-primitive-type-face
    '((t (:foreground "dark cyan")))
    "The face used for primitive types (like Set and Prop)."
    )
  (defface agda2-highlight-datatype-face
    '((t (:foreground "dark cyan")))
    "The face used for datatypes."
    )
  (defface agda2-highlight-function-face
    '((t (:foreground "dark cyan")))
    "The face used for functions."
    )
  (defface agda2-highlight-postulate-face
    '((t (:foreground "dark cyan")))
    "The face used for postulates."
    )
  (defface agda2-highlight-primitive-face
    '((t (:foreground "dark cyan")))
    "The face used for primitive functions."
    )
  (defface agda2-highlight-record-face
    '((t (:foreground "dark cyan")))
    "The face used for record types."
    ))

;; vc-darcs
(use-package vc-darcs
  :ensure t
  :config
  (add-to-list 'vc-handled-backends 'DARCS)
  (add-hook 'find-file-hooks 'vc-darcs-find-file-hook))

;; dart
(load "dart-init")

;; extempore
(autoload 'extempore-mode "~/extempore/extras/extempore.el" "" t)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))

;; font-lock
(use-package font-lock
  :init (global-font-lock-mode 1))

;; geiser
(use-package geiser :ensure t)

;; haskell
(load "haskell-init")

;; highlight-indent
;; (autoload 'highlight-indentation-mode "highlight-indentation" nil t)
;; (autoload 'highlight-indentation-current-column-mode "highlight-indentation" nil t)

;; ido
(use-package ido
  :init (progn
          (ido-mode 1)
          (ido-everywhere 1))
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-save-directory-list-file "~/.emacs.cache/ido.last")
  (setq ido-use-filename-at-point 'guess))

(use-package ido-ubiquitous
  :ensure t
  :init (ido-ubiquitous-mode 1))

;; ispell
(use-package ispell
  :if (executable-find "aspell")
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")))

;; flyspell
(use-package flyspell
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'message-mode-hook 'flyspell-mode)
  (add-hook 'magit-log-edit-mode-hook 'flyspell-prog-mode))

;; langtool
(use-package langtool
  :ensure t
  :config
  (setq langtool-language-tool-jar "~/.emacs.d/LanguageTool-2.8/languagetool-commandline.jar"))

;; js2-mode
(use-package js2-mode
  :ensure t
  :config
  (setq js2-basic-offset 2
        js2-bounce-indent-p t))

;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; flycheck-haskell
(use-package flycheck-haskell
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; flycheck-color-mode
(use-package flycheck-color-mode-line
  :ensure t
  :init (flycheck-color-mode-line-mode))

;; guru-mode
(use-package guru-mode
  :ensure t
  :init (guru-global-mode)
  :config (setq guru-warn-only t))

;; idris-mode
(use-package idris-mode
  :ensure t)

;; magit
(use-package magit
  :ensure t)

(use-package magit-filenotify
  :ensure t
  :config (add-hook 'magit-status-mode-hook #'magit-filenotify-mode))

;; makefile
(add-hook 'makefile-mode-hook (progn (setq indent-tabs-mode t)))

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode)))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

;; pretty-mode
(use-package pretty-mode
  :ensure t
  :init (global-pretty-mode))

;; projectile
(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :config (setq projectile-enable-caching t))

;; quack
(use-package quack :ensure t)

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; saveplace
(use-package saveplace
  :ensure t
  :config
  (setq save-place-file "~/.emacs.cache/saveplace")
  (setq-default save-place t))

;; scss-mode
(use-package scss-mode
  :ensure t
  :config
  (setq scss-sass-options
	'("--style" "compressed" "--cache-location" "'/tmp/.sass-cache'")))


;; undo-tree
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode t)
  :config
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '((".*" . "~/.emacs.cache/undo/"))))

;; whitespace
(use-package whitespace
  :ensure t
  :init (global-whitespace-mode t)
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs empty trailing lines-tail space-before-tab))
  (add-hook 'before-save-hook 'whitespace-cleanup))

;; scala
(use-package scala-mode2 :ensure t)
(use-package sbt-mode :ensure t)

(defun scalariver ()
  "Send current buffer to scalariver and replace with response."
  (require 'request)
  (request
   "http://127.0.0.1:8098/"
   :type "POST"
   :parser 'buffer-string
   :data `(("source" . ,(buffer-string))
           ("url" . "http://127.0.0.1:8098")
           ("stdin" . "true")
           ("stdout" . "true")
           ("forceOutput" . "true")
           ("quiet" . "true"))
   :success (function* (lambda (&key data &allow-other-keys)
                         (when data
                           (with-current-buffer (current-buffer)
                             (erase-buffer)
                             (insert data))
                           )))
   :error (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                       (message "Got error: %S" error-thrown)))
   :complete (lambda (&rest _) (message "Formatted!"))
   :status-code '((400 . (lambda (&rest _) (message "Got 400."))))
   :timeout 5
   :sync t
   ))
(defun scalariver-buffer ()
  "Apply scalariver to the current buffer if it is in scala-mode."
  (interactive)
  (let ((column (current-column))
        (line (line-number-at-pos)))
    (when (string=
           "scala-mode"
           (buffer-local-value 'major-mode (current-buffer)))
      (scalariver))
    (goto-char (point-min))
    (forward-line (1- line))
    (goto-char (+ column (point)))))

;; (add-hook 'scala-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "M-.") 'sbt-find-definitions)
;;              (local-set-key (kbd "C-x '") 'sbt-run-previous-command)
;;              (add-hook 'before-save-hook 'scalariver-buffer)
;;              ))
(add-hook 'sbt-mode-hook
          '(lambda ()
             (setq compilation-skip-threshold 1)
             (local-set-key (kbd "C-a") 'comint-bol)
             (local-set-key (kbd "M-RET") 'comint-accumulate)
             ))

(use-package ensime
  :ensure t
  :config
  (setq ensime-sbt-perform-on-save "compile")
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  (add-hook 'ensime-source-buffer-saved-hook 'ensime-format-source))

;; slime
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program (executable-find "sbcl"))
  (setq slime-contribs '(slime-fancy)))

;; show-paren-mode
(use-package paren
  :init (show-paren-mode t)
  :config (setq show-paren-style 'expression))

;; smart-mode-line
(use-package smart-mode-line
  :ensure t
  :init (progn
	  (setq sml/no-confirm-load-theme t)
	  (sml/setup)
	  (sml/apply-theme 'respectful)))

;; smartparens
(load "sp-init")

;; smex
(use-package smex
  :ensure t
  :init (smex-initialize)
  :config (setq smex-save-file "~/.emacs.cache/smex-items")
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command)))

;; enh-ruby-mode
(use-package enh-ruby-mode
  :ensure t
  :mode "\\.rb$"
  :interpreter "ruby"
  :config (setq enh-ruby-deep-indent-param nil))

;; web-mode
(use-package web-mode
  :ensure t
  :mode "\\.html?\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; yasnippet
(use-package yasnippet
  :ensure t
  :init (progn
	  (setq yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
	  (yas-global-mode t)))

(provide 'utils)
;;; utils.el ends here
