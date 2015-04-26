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
  (add-hook 'after-init-hook 'global-company-mode)
  (add-to-list 'company-backends 'company-ghc))

(defun use-packages (names)
  "Use-package on list of NAMES."
  (pcase names
    (`(,p . ,ps)
     (progn
       (eval `(use-package ,p :ensure t :defer t))
       (use-packages ps)))
    (_ t)
    ))

(use-packages '(company-auctex
                company-cabal
                company-c-headers
                company-ghc
                company-ghci
                company-go
                company-inf-ruby
                company-irony
                company-math
                company-coq
                company-restclient
                company-quickhelp
                slime-company
                ))

;; ag -- the silver searcher
(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-window 't)
  (setq ag-reuse-buffers 't))

;; conf-mode
(add-hook 'conf-mode-hook
          '(lambda () (setq conf-assignment-column 12)))

;; css-mode
(use-package css-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook (setq css-indent-offset 2)))

;; dired
(use-package dired+ :ensure t)
(use-package dired-details
  :ensure t
  :config (dired-details-install))
(use-package dired-details+ :ensure t)

;; edit-server
(use-package edit-server
  :ensure t
  :if (and (window-system) (daemonp))
  :config (edit-server-start))

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

;; coq
(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
(autoload 'run-coq "inferior-coq" "Run an inferior Coq process." t)
(autoload 'run-coq-other-window "inferior-coq"
  "Run an inferior Coq process in a new window." t)
(autoload 'run-coq-other-frame "inferior-coq"
  "Run an inferior Coq process in a new frame." t)

;; proofgeneral
(custom-set-faces
   '(proof-locked-face
     ((t (:background "gray11")))
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
  :config (global-font-lock-mode 1))

;; geiser
(use-package geiser :ensure t)

;; diff-hl-mode
(use-package diff-hl
  :ensure t
  :defer t
  :config (diff-hl-mode))

;; highlight-indent
;; (autoload 'highlight-indentation-mode "highlight-indentation" nil t)
;; (autoload 'highlight-indentation-current-column-mode "highlight-indentation" nil t)

;; helm
(use-package helm
  :ensure t
  :bind
  (("C-x b" . helm-mini)
   ("C-x C-b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-x C-d" . helm-browse-project))
  :config
  (use-package helm-config)
  (use-package helm-command
    :bind (("M-x" . helm-M-x))
    :config (setq helm-M-x-fuzzy-match t
                  helm-M-x-requires-pattern 2))
  (helm-mode 1)
  (helm-autoresize-mode t)
  (helm-adaptive-mode t)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-ff-file-name-history-use-recentf t
        helm-quick-update t)
  (bind-keys :map helm-map
             ("<tab>" . helm-execute-persistent-action)
             ("C-i" . helm-execute-persistent-action)
             ("C-z" . helm-select-action))
  (use-package helm-ring
    :bind (("M-y" . helm-show-kill-ring)))
  (use-package helm-projectile
    :ensure t :defer t)
  (use-package helm-swoop
    :ensure t :defer t
    :bind
    (("M-i" . helm-swoop)
     ("M-I" . helm-swoo-back-to-last-point)
     ("C-c M-i" . helm-multi-swoop)
     ("C-x M-i" . helm-multi-swoop-all))
    :config
    (bind-keys :map isearch-mode-map
               ("M-i" . helm-swoop-from-isearch))
    (bind-keys :map helm-swoop-map
               ("M-i" . helm-multi-swoop-all-from-helm-swoop)
               ("C-r" . helm-previous-line)
               ("C-s" . helm-next-line))
    (bind-keys :map helm-multi-swoop-map
               ("C-r" . helm-previous-line)
               ("C-s" . helm-next-line))
    )
  (use-package helm-ag :ensure t :defer t)
  (use-package helm-ls-git :ensure t :defer t))

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

;; golden-ratio
(use-package golden-ratio
  :ensure t
  :config
  (setq golden-ratio-auto-scale t)
  (golden-ratio-mode 1))

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
  :config
  (global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

;; flycheck-color-mode
(use-package flycheck-color-mode-line
  :ensure t
  :config (flycheck-color-mode-line-mode))

;; guru-mode
(use-package guru-mode
  :ensure t
  :config
  (guru-global-mode)
  (setq guru-warn-only t))

;; idris-mode
(use-package idris-mode
  :ensure t)

;; magit
(use-package magit
  :ensure t
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))

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

;; nyan-mode
(use-package nyan-mode
  :ensure t
  :config (nyan-mode))

;; paradox
(use-package paradox
  :ensure t
  :defer t
  :config (setq paradox-github-token t
                paradox-execute-asynchronously nil))

;; pretty-mode
(use-package pretty-mode
  :ensure t
  :config (global-pretty-mode))

;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm
        projectile-enable-caching t
        projectile-switch-project-action 'helm-projectile)
  (helm-projectile-on))

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
  :config
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '((".*" . "~/.emacs.cache/undo/"))))

;; whitespace
(use-package whitespace
  :ensure t
  :config
  (global-whitespace-mode t)
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
  :defer t
  :config
  (setq ensime-sbt-perform-on-save "compile")
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

;; slime
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program (executable-find "sbcl"))
  (setq slime-contribs '(slime-fancy)))

;; show-paren-mode
(use-package paren
  :config
  (show-paren-mode t)
  (setq show-paren-style 'expression))

;; smart-mode-line
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (sml/setup))

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

;; writegood-mode
(use-package writegood-mode
  :ensure t
  :defer t)

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
  (yas-global-mode t))

(provide 'utils)
;;; utils.el ends here
