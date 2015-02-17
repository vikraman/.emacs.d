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

(require 'use-package)
(setq inhibit-default-init t)

(provide 'init)
;;; init.el ends here
