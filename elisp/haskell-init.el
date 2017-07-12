;;; haskell-init.el -- haskell
;;; Commentary:

;;; Code:

(require 'use-package)

(use-package intero
  :ensure t
  :config
  (use-package button-lock
    :ensure t
    :config
    (use-package liquid-types
      :load-path "liquid-tip.el/"))
  (use-package flycheck-liquidhs
    :load-path "flycheck-liquidhs.el/")
  (setq haskell-stylish-on-save t)
  (add-hook 'haskell-mode-hook
            '(lambda ()
               (intero-mode)
               ;; (ghc-init)
               (flycheck-select-checker 'haskell-liquid)
               (liquid-types-mode))))

(use-package hindent
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode)
  (setq hindent-style "johan-tibell"))

(provide 'haskell-init)
;;; haskell-init.el ends here
