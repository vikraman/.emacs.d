;;; tex-init.el -- TeX
;;; Commentary:

;;; Code:

;; auctex
(require 'tex)
;; (require 'auto-complete-auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-view-program-list '(("Zathura" "zathura %o")))
(setq-default TeX-view-program-selection '((output-pdf "Zathura")))
(setq-default TeX-master nil)

(defun TeX-toggle-escape nil (interactive)
       "Toggle Shell Escape"
       (setq LaTeX-command
             (if (string= LaTeX-command "latex") "latex -shell-escape"
               "latex"))
       (message (concat "shell escape "
                        (if (string= LaTeX-command "latex -shell-escape")
                            "enabled"
                          "disabled"))
                ))

(add-hook 'LaTeX-mode-hook
          (lambda()
            (local-set-key [C-tab] 'TeX-complete-symbol)
            (auto-fill-mode)
            (flyspell-mode)
            (LaTeX-math-mode)
            (turn-on-reftex)
            (setq reftex-plug-into-AUCTeX t)
            (setq TeX-PDF-mode t)
            (local-set-key (kbd "C-c C-t x") 'TeX-toggle-escape)))

;; nomenclature for latex
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
                '("Nomenclature" "makeindex %s.nlo -s nomencl.ist -o %s.nls"
                  (lambda (name command file)
                    (TeX-run-compile name command file)
                    (TeX-process-set-variable file 'TeX-command-next TeX-command-default))
                  nil t :help "Create nomenclature file")))

(provide 'tex-init)
;;; tex-init.el ends here
