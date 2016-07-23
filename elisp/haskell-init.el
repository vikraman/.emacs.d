;;; haskell-init.el -- haskell
;;; Commentary:

;;; Code:

(require 'use-package)

(use-package intero
  :ensure t
  :config
  (setq intero-package-version "0.1.16")
  (add-hook 'haskell-mode-hook 'intero-mode))

(provide 'haskell-init)
;;; haskell-init.el ends here
