;;; scheme-init.el -- scheme
;;; Commentary:

;;; Code:

(require 'use-package)

;; geiser
(use-package geiser
  :ensure t
  :config
  (use-package quack
    :ensure t)
  (use-package sp-init
    :config
    (smartparens-strict-mode +1))
  (setq geiser-mode-start-repl-p t
        geiser-repl-history-filename "~/.emacs.cache/geiser-history"))

;; racket-mode
(use-package racket-mode
  :ensure t
  :init
  (progn
    (delete '("\\.rkt\\'" . scheme-mode) auto-mode-alist)
    (delete '("\\.rktd\\'" . scheme-mode) auto-mode-alist)
    (add-to-list 'auto-mode-alist '("\\.rkt[dl]?\\'" . racket-mode)))
  :config
  (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
  (put 'union-case 'racket-indent-function 2)
  (put 'fresh 'racket-indent-function 1)
  (put 'do 'racket-indent-function 0)
  (put 'conde 'racket-indent-function 0)
  (put 'syntax-id-rules 'racket-indent-function 1))

(provide 'scheme-init)
;;; scheme-init.el ends here
