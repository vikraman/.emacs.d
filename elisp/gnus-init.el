;; gnus-init.el -- gnus configuration
;;; Commentary:

;;; Code:
;; Setup email / news
;; (setq imap-shell-program "/usr/libexec/dovecot/imap")
;; (setq gnus-select-method '(nnimap "Mail" (nnimap-stream shell)))
(setq-default gnus-select-method '(nntp "news.gmane.org"))

(provide 'gnus-init)
;;; gnus-init.el ends here
