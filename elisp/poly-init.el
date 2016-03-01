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
  (add-to-list 'auto-mode-alist '("\\.lagda\\'" . poly-TeX+agda2-mode))
  (defcustom pm-inner/TeX+haskell
    (pm-hbtchunkmode "TeX haskell"
                     :mode 'haskell-mode
                     :head-reg "^[ \t]*\\\\begin{lstlisting}[ \t]*$"
                     :tail-reg "^[ \t]*\\\\end{lstlisting}[ \t]*$"
                     :head-mode 'host
                     :tail-mode 'host
                     :adjust-face nil)
    "TeX with haskell chunk"
    :group 'innermodes
    :type 'object)
  (defcustom pm-poly/TeX+haskell
    (pm-polymode-one "TeX"
                     :hostmode 'pm-host/TeX
                     :innermode 'pm-inner/TeX+haskell)
    "TeX with agda mode"
    :group 'polymodes
    :type 'object)
  (define-polymode poly-TeX+haskell-mode pm-poly/TeX+haskell))

(provide 'poly-init)
;;; poly-init.el ends here
