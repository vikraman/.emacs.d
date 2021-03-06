;;; agda-init.el -- agda
;;; Commentary:

;;; Code:

(require 'use-package)

;; agda2-mode
(use-package agda2
  :bind (("M-p" . agda2-previous-goal)
         ("M-n" . agda2-next-goal))
  :config
  ;; missing letters
  ;; https://en.wikipedia.org/wiki/Mathematical_Alphanumeric_Symbols#Latin_letters
  (setq-default agda-input-user-translations
                `(("Mih" . ("ℎ"))
                  ("McB" . ("𝓑"))
                  ("McE" . ("𝓔"))
                  ("McF" . ("ℱ"))
                  ("McH" . ("𝓗"))
                  ("McI" . ("𝓘"))
                  ("McL" . ("𝓛"))
                  ("McM" . ("𝓜"))
                  ("McR" . ("𝓡"))
                  ("Mce" . ("𝓮"))
                  ("Mcg" . ("𝓰"))
                  ("Mco" . ("𝓸"))
                  ("MfC" . ("ℭ"))
                  ("MfH" . ("ℌ"))
                  ("MfI" . ("ℑ"))
                  ("MfR" . ("ℜ"))
                  ("MfZ" . ("ℨ"))))
  (add-hook 'agda2-mode-hook
            '(lambda ()
               (setq agda2-highlight-level 'interactive
                     agda2-highlight-face-groups 'default-faces
                     agda2-program-args '("--sharing"))))
  (let ((base03    "#002b36")
        (base02    "#073642")
        (base01    "#586e75")
        (base00    "#657b83")
        (base0     "#839496")
        (base1     "#93a1a1")
        (base2     "#eee8d5")
        (base3     "#fdf6e3")
        (yellow    "#b58900")
        (orange    "#cb4b16")
        (red       "#dc322f")
        (magenta   "#d33682")
        (violet    "#6c71c4")
        (blue      "#268bd2")
        (cyan      "#2aa198")
        (green     "#859900"))
    (custom-set-faces
     `(agda2-highlight-keyword-face ((t (:foreground ,orange))))
     `(agda2-highlight-string-face ((t (:foreground ,magenta))))
     `(agda2-highlight-number-face ((t (:foreground ,violet))))
     `(agda2-highlight-symbol-face ((((background ,base3)) (:foreground ,base01))))
     `(agda2-highlight-primitive-type-face ((t (:foreground ,blue))))
     `(agda2-highlight-bound-variable-face ((t nil)))
     `(agda2-highlight-inductive-constructor-face ((t (:foreground ,green))))
     `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,yellow))))
     `(agda2-highlight-datatype-face ((t (:foreground ,blue))))
     `(agda2-highlight-field-face ((t (:foreground ,red))))
     `(agda2-highlight-function-face ((t (:foreground ,blue))))
     `(agda2-highlight-module-face ((t (:foreground ,violet))))
     `(agda2-highlight-postulate-face ((t (:foreground ,blue))))
     `(agda2-highlight-primitive-face ((t (:foreground ,blue))))
     `(agda2-highlight-record-face ((t (:foreground ,blue))))
     `(agda2-highlight-dotted-face ((t nil)))
     `(agda2-highlight-operator-face ((t nil)))
     `(agda2-highlight-error-face ((t (:foreground ,red :underline t))))
     `(agda2-highlight-unsolved-meta-face ((t (:background ,base03 :foreground ,yellow))))
     `(agda2-highlight-unsolved-constraint-face ((t (:background ,base03 :foreground ,yellow))))
     `(agda2-highlight-termination-problem-face ((t (:background ,orange :foreground ,base03))))
     `(agda2-highlight-incomplete-pattern-face ((t (:background ,orange :foreground ,base03))))
     `(agda2-highlight-typechecks-face ((t (:background ,cyan :foreground ,base03)))))))

(provide 'agda-init)
;;; agda-init.el ends here
