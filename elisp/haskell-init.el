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
  (use-package ghc
    :ensure t)
  (setq intero-package-version "0.1.16")
  (add-hook 'haskell-mode-hook
            '(lambda ()
               (intero-mode)
               (ghc-init)
               (flycheck-select-checker 'haskell-liquid)
               (liquid-types-mode))))

(provide 'haskell-init)
;;; haskell-init.el ends here
