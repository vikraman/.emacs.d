;;; utils.el -- utils
;;; Commentary:

;;; Code:

;; ace-jump-mode
(require 'ace-jump-mode)

;; auto-complete
;; (require 'auto-complete)
;; (ac-config-default)
;; ;; Store the completion history in the cache directory
;; (setq ac-comphist-file "~/.emacs.cache/ac-comphist.dat")
;; (setq-default ac-sources '(ac-source-semantic
;;                            ac-source-yasnippet
;;                            ac-source-abbrev
;;                            ac-source-words-in-buffer
;;                            ac-source-words-in-same-mode-buffers
;;                            ac-source-files-in-current-dir
;;                            ))

;; (setq ac-delay 0.0)
;; (setq ac-auto-show-menu 0.0)
;; (setq ac-show-menu-immediately-on-auto-complete t)
;; (setq ac-quick-help-delay 1.0)
;; (setq ac-menu-height 3)
;; (setq ac-auto-start 2)
;; (setq ac-use-menu-map t)

;; (add-to-list 'ac-modes 'web-mode)
;; (add-to-list 'ac-modes 'scss-mode)

;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;; Enable auto-complete globally
;; (global-auto-complete-mode t)

;; (eval-after-load 'flyspell
;;   '(eval-after-load 'auto-complete
;;      '(ac-flyspell-workaround)))

;; ac-ispell
;; (custom-set-variables
;;  '(ac-ispell-requires 4)
;;  '(ac-ispell-fuzzy-limit 4))

;; (eval-after-load 'auto-complete
;;   '(lambda ()
;;      (ac-ispell-setup)
;;      (ac-ispell-ac-setup)))

;; auto-fill-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; company-mode
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 2)
(add-hook 'after-init-hook 'global-company-mode)

;; TeX
(load "tex-init")

;; ag -- the silver searcher
(require 'ag)
(setq ag-highlight-search t)
(setq ag-reuse-window 't)
(setq ag-reuse-buffers 't)

;; cedet
;(require 'cedet)
;(when (featurep 'cedet)
;  (global-ede-mode 1)
;  (setq semantic-load-turn-everything-on t)
;  (semantic-load-enable-gaudy-code-helpers)
;  (semantic-load-enable-excessive-code-helpers)
;  (semantic-load-enable-all-exuberent-ctags-support)
;  (global-semanticdb-minor-mode 1)
;  (global-srecode-minor-mode 1)
;  (setq semanticdb-default-save-directory "~/.emacs.cache/semanticdb")
;  (setq semanticdb-default-system-save-directory "~/.emacs.cache/semanticdb"))

;; autopair
;; (require 'autopair)
;; (when (featurep 'autopair)
;;   (autopair-global-mode))

;; css-mode
(add-hook 'css-mode-hook
          (setq css-indent-offset 2))

;; dired
(require 'dired+)
(require 'dired-details)
(dired-details-install)
(require 'dired-details+)

;; edit-server
(when (and
       (require 'edit-server nil t)
       (daemonp))
  (edit-server-start))

;; erc
(setq  erc-server-coding-system '(utf-8 . utf-8))

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; agda2-mode
(require 'agda2)
(add-hook 'agda2-mode-hook
          '(lambda ()
             (setq agda2-include-dirs '("." "/usr/share/agda-stdlib"))
             ))
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
)

;; vc-darcs
(add-to-list 'vc-handled-backends 'DARCS)
(autoload 'vc-darcs-find-file-hook "vc-darcs")
(add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

;; dart
(load "dart-init")

;; extempore
(autoload 'extempore-mode "~/extempore/extras/extempore.el" "" t)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))

;; font-lock
(global-font-lock-mode 1)

;; haskell
(load "haskell-init")

;; highlight-indent
;; (autoload 'highlight-indentation-mode "highlight-indentation" nil t)
;; (autoload 'highlight-indentation-current-column-mode "highlight-indentation" nil t)

;; ido
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(setq ido-save-directory-list-file "~/.emacs.cache/ido.last")
(setq ido-use-filename-at-point 'guess)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; ispell
(when (executable-find "aspell")
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")))
(defun c/enable-flyspell ()
  (flyspell-mode 1))
(defun c/enable-flyspell-prog ()
  (flyspell-prog-mode))
(add-hook 'text-mode-hook 'c/enable-flyspell)
(add-hook 'message-mode-hook 'c/enable-flyspell)
(add-hook 'magit-log-edit-mode-hook 'c/enable-flyspell)

;; langtool
(require 'langtool)
(setq langtool-language-tool-jar "~/.emacs.d/LanguageTool-2.8/languagetool-commandline.jar")

;; js2-mode
(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 )

;; fill-column-indicator
;; (require 'fill-column-indicator)
;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode 1)

;; flycheck
(require 'flycheck)
(when (featurep 'flycheck)
  (global-flycheck-mode))
(add-to-list 'load-path "~/.emacs.d/flycheck-haskell")
(load "flycheck-haskell")
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; flycheck-color-mode
(require 'flycheck-color-mode-line)
(when (featurep 'flycheck-color-mode-line)
  (flycheck-color-mode-line-mode))

;; guru-mode
(require 'guru-mode)
(guru-global-mode t)
(setq guru-warn-only t)

;; idris-mode
(add-to-list 'load-path "~/.emacs.d/idris-mode")
(require 'idris-mode)

;; magit
(add-hook 'magit-status-mode-hook #'magit-filenotify-mode)

;; makefile-mode
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

;; markdown-mode
(require 'markdown-mode)
(when (featurep 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  )

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; predictive
(add-to-list 'load-path "~/.emacs.d/predictive")
(require 'predictive)
(set-default 'predictive-auto-add-to-dict t)
(setq predictive-auto-learn t
      predictive-add-to-dict-ask nil
      predictive-use-auto-learn-cache nil
      predictive-which-dict t)

;; pretty-mode
(require 'pretty-mode)
(global-pretty-mode t)

;; projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; saveplace
(require 'saveplace)
(when (featurep 'saveplace)
  (setq save-place-file "~/.emacs.cache/saveplace")
  (setq-default save-place t))

;; scss-mode
(setq scss-sass-options '("--style" "compressed" "--cache-location" "'/tmp/.sass-cache'"))

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
(setq undo-tree-visualizer-relative-timestamps t)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '((".*" . "~/.emacs.cache/undo/")))

;; whitespace
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail space-before-tab))
(when (featurep 'whitespace)
  (global-whitespace-mode +1))
(add-hook 'before-save-hook 'whitespace-cleanup)

;; scala
(require 'scala-mode2)
;; (require 'sbt-mode)
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

;; goodbye, memory hungry monster
(add-to-list 'load-path "~/.emacs.d/ensime/")
;; (setq ensime-completion-style 'auto-complete)
(setq ensime-sbt-perform-on-save "compile")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; (add-hook 'ensime-source-buffer-saved-hook 'ensime-format-source)

;; make ensime work remotely
;; (defun ensime-stackoverflow-connect (host port)
;;   (interactive (list
;;                 (read-from-minibuffer "Host: " ensime-default-server-host)
;;                 (read-from-minibuffer "Port: " (format "%d" ensime-default-port)
;;                                       nil t)))
;;   (let ((c (ensime-connect host port))
;;         (config (ensime-config-load "~/devel/gmantra/ixchel/.ensime")))
;;     (ensime-set-config c config)
;;     (setq ensime-buffer-connection c))
;;   )

;; slime
(setq inferior-lisp-program (executable-find "sbcl"))
(setq slime-contribs '(slime-fancy))

;; show-paren-mode
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; smart-mode-line
(require 'smart-mode-line)
(sml/setup)
(sml/apply-theme 'respectful)

;; smartparens
(load "sp-init")

;; smex
(require 'smex)
(smex-initialize)
(setq smex-save-file "~/.emacs.cache/smex-items")
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; enh-ruby-mode
(add-to-list 'load-path "~/.emacs.d/enhanced-ruby-mode")
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(when (featurep 'enh-ruby-mode)
  (setq enh-ruby-deep-indent-paren nil))

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            ))

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
(yas-global-mode 1)

(provide 'utils)
;;; utils.el ends here
