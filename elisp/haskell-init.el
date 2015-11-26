;;; haskell-init.el -- haskell
;;; Commentary:

;;; Code:

(require 'use-package)

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook
            '(lambda ()
               (turn-on-haskell-simple-indent)
               (haskell-auto-insert-module-template)
               (interactive-haskell-mode)))
  (setq haskell-stylish-on-save t
        haskell-tags-on-save nil
        haskell-notify-p t
        haskell-literate-default 'tex
        haskell-process-type 'cabal-repl
        haskell-process-args-cabal-repl '("--ghc-options=-ferror-spans" "--with-ghc=ghci-ng")
        haskell-process-path-ghci "ghci-ng"
        haskell-process-args-ghci '("-ferror-spans")
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-process-use-presentation-mode t
        haskell-process-suggest-haskell-docs-imports t
        haskell-interactive-mode-eval-mode 'haskell-mode))

(use-package flycheck-haskell
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(use-package ghc
  :ensure t
  :config (add-hook 'haskell-mode-hook 'ghc-init))

(use-package shm
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  (setq shm-use-presentation-mode t
        shm-auto-insert-skeletons t
        shm-auto-insert-bangs t))

(use-package hindent
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode)
  (setq hindent-style "gibiansky"))

(provide 'haskell-init)
;;; haskell-init.el ends here
