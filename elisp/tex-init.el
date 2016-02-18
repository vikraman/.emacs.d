;;; tex-init.el -- TeX
;;; Commentary:

;;; Code:

(require 'use-package)

;; auctex

(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-view-program-list '(("Zathura" "zathura %o")))
  (setq-default TeX-view-program-selection '((output-pdf "Zathura")))
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (set-fill-column 120)
              (auto-fill-mode)
              (flyspell-mode)
              (LaTeX-math-mode)
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)
              (setq TeX-PDF-mode t
                    TeX-engine 'xetex)))
  (add-to-list 'TeX-command-list
               '("Nomenclature" "makeindex %s.nlo -s nomencl.ist -o %s.nls"
                 (lambda (name command file)
                   (TeX-run-compile name command file)
                   (TeX-process-set-variable file 'TeX-command-next TeX-command-default))
                 nil t :help "Create nomenclature file"))
  :bind (("C-<tab>" . TeX-complete-symbol)
         ("C-c C-t x" . TeX-toggle-escape)))

(defun TeX-toggle-escape nil
  "Toggle Shell Escape."
  (interactive)
  (setq LaTeX-command
        (if (string= LaTeX-command "latex") "latex -shell-escape"
          "latex"))
  (message (concat "shell escape "
                   (if (string= LaTeX-command "latex -shell-escape")
                       "enabled"
                     "disabled"))))

(provide 'tex-init)
;;; tex-init.el ends here
