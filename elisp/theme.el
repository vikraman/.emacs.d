;;; theme.el -- theme for emacs
;;; Commentary:
;;; Code:

;; Custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized/")

;; Load and use the solarized dark theme
(setq frame-background-mode 'dark)
;; (setq solarized-termcolors 256)
(load-theme 'solarized t)
(custom-set-faces
 '(show-paren-match
   ((t (:foreground nil ,@fmt-bold ,@bg-base02))))
 )

;; Use the Source Code Pro font
(setq default-frame-alist '((font . "Source Code Pro SemiBold-10")))

(provide 'theme)
;;; theme.el ends here
