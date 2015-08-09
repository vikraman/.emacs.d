;;; mu4e-init.el -- mu4e configuration
;;; Commentary:

;;; Code:

(require 'mu4e)

;; make this the default mua
(setq mail-user-agent 'mu4e-user-agent)

;; update every 10 minutes
(setq mu4e-update-interval (* 10 60))

;; sending email
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "localhost")

;; my maildir
(setq mu4e-maildir "~/Mail")

;; my email config
(setq mu4e-user-mail-address-list
      '("vikraman.choudhury@gmail.com"
        "vikraman@indiana.edu"
        "vikraman@umail.iu.edu"
        "vikraman@gentoo.org"
        "vikraman@iitk.ac.in"
        "vikraman@cse.iitk.ac.in"
        )
      mu4e-compose-signature "Vikraman")
(setq user-mail-address "vikraman.choudhury@gmail.com"
      user-full-name  "Vikraman Choudhury")

;; multiple accounts
(defvar my-mu4e-account-alist
  '(("gmail"
     (user-mail-address "vikraman.choudhury@gmail.com"))
    ("indiana"
     (user-mail-address "vikraman@indiana.edu"))
    ("umail"
     (user-mail-address "vikraman@umail.iu.edu"))
    ("vikraman"
     (user-mail-address "vikraman@vikraman.org"))
    ("git"
     (user-mail-address "git@vikraman.org"))
    ("gentoo"
     (user-mail-address "vikraman@gentoo.org"))
    ("iitk"
     (user-mail-address "vikraman@iitk.ac.in"))
    ("iitkcse"
     (user-mail-address "vikraman@cse.iitk.ac.in"))))

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (message "No email account found"))
    ))
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

;; show related emails and skip duplicates
(setq mu4e-headers-include-related t
      mu4e-headers-skip-duplicates t)

;; my bookmarked queries
(add-to-list 'mu4e-bookmarks
             '("NOT flag:list AND flag:unread" "Unread non-list messages" ?b))
(add-to-list 'mu4e-bookmarks
             '("NOT flag:list" "Non-list messages" ?l))
(add-to-list 'mu4e-bookmarks
             '("flag:flagged" "Flagged messages" ?f))
(add-to-list 'mu4e-bookmarks
             '("to:git@vikraman.org OR to:noreply.github.com" "Git messages" ?g))

;; make search queries return more results
(setq mu4e-headers-results-limit 1024)

;; use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;; save attachments to tmpdir
(setq mu4e-attachment-dir "/tmp")

;; pgp/mime using mml
(setq mml2015-use 'epg)
(add-hook 'mu4e-compose-mode-hook
          (defun my-setup-mml-hook ()
            (mml-secure-message-sign)))

;; inline pgp using epa
(add-hook 'mu4e-view-mode-hook
          (defun my-view-mode-hook ()
            (epa-mail-mode)))

;; attempt to show images when viewing messages
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; to view html in browser
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; html2text for html emails
(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(provide 'mu4e-init)
;;; mu4e-init.el ends here
