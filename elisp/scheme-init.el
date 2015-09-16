;;; scheme-init.el -- scheme
;;; Commentary:

;;; Code:

(require 'use-package)

;; geiser
(use-package geiser
  :ensure t
  :config
  (use-package quack
    :ensure t
    :config
    (setq quack-smart-open-paren-p t))
  (setq geiser-mode-start-repl-p t
        geiser-repl-history-filename "~/.emacs.cache/geiser-history"))

(provide 'scheme-init)
;;; scheme-init.el ends here
