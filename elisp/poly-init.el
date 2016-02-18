;;; poly-init.el -- polymode
;;; Commentary:

;;; Code:

(require 'use-package)

(use-package polymode
  :ensure t
  :config
  (defcustom pm-host/TeX
    (pm-bchunkmode "TeX" :mode 'TeX-mode)
    "TeX host"
    :group 'hostmodes
    :type 'object)
  (defcustom pm-inner/TeX+agda2
    (pm-hbtchunkmode "TeX agda2"
                     :mode 'agda2-mode
                     :head-reg "^[ \t]*\\\\begin{code}[ \t]*$"
                     :tail-reg "^[ \t]*\\\\end{code}[ \t]*$"
                     :head-mode 'host
                     :tail-mode 'host
                     :adjust-face nil)
    "TeX with agda chunk"
    :group 'innermodes
    :type 'object)
  (defcustom pm-poly/TeX+agda2
    (pm-polymode-one "TeX"
                     :hostmode 'pm-host/TeX
                     :innermode 'pm-inner/TeX+agda2)
    "TeX with agda mode"
    :group 'polymodes
    :type 'object)
  (define-polymode poly-TeX+agda2-mode pm-poly/TeX+agda2)
  (add-to-list 'auto-mode-alist '("\\.lagda\\'" . poly-TeX+agda2-mode)))

(provide 'poly-init)
;;; poly-init.el ends here
