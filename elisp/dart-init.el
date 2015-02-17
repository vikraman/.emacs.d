;;; dart-init.el -- dart
;;; Commentary:

;;; Code:

(require 'use-package)

(use-package dart-mode
  :ensure t
  :defer t
  :init (require 'cl)
  :config
  (add-hook 'before-save-hook #'dartformat-before-save))

(defun dart--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun dart--line-column-to-point (line column)
  (save-excursion
    (dart--goto-line line)
    (forward-char (1- column))
    (point)))

(defun dart--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current
buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in dart--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (dart--goto-line (- from line-offset))
                (incf line-offset len)
                (dart--delete-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in dart--apply-rcs-patch")))))))))

(defun dartformat--process-errors (filename tmpfile errbuf)
  ;; Convert the dartformat stderr to something understood by the compilation mode.
  (with-current-buffer errbuf
    (goto-char (point-min))
    (insert "dartformat errors:\n")
    (while (search-forward-regexp (concat "^\\(" (regexp-quote tmpfile) "\\):") nil t)
      (replace-match (file-name-nondirectory filename) t t nil 1))
    (compilation-mode)
    (display-buffer errbuf)))

(defalias 'dart--kill-whole-line
  (if (fboundp 'kill-whole-line)
      #'kill-whole-line
    #'kill-entire-line))

;; Delete the current line without putting it in the kill-ring.
(defun dart--delete-whole-line (&optional arg)
  ;; Emacs uses both kill-region and kill-new, Xemacs only uses
  ;; kill-region. In both cases we turn them into operations that do
  ;; not modify the kill ring. This solution does depend on the
  ;; implementation of kill-line, but it's the only viable solution
  ;; that does not require to write kill-line from scratch.
  (flet ((kill-region (beg end)
                      (delete-region beg end))
         (kill-new (s) ()))
    (dart--kill-whole-line arg)))

(defun dartformat ()
  "Formats the current buffer according to the dartformat tool."
  (interactive)
  (let ((tmpfile (make-temp-file "dartformat" nil ".dart"))
        (patchbuf (get-buffer-create "*Dartformat patch*"))
        (errbuf (get-buffer-create "*Dartformat Errors*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (with-current-buffer errbuf
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-current-buffer patchbuf
      (erase-buffer))
    (write-region nil nil tmpfile)
    ;; We're using errbuf for the mixed stdout and stderr output. This
    ;; is not an issue because dartformat -w does not produce any stdout
    ;; output in case of success.
    (if (zerop (call-process "dartformat" nil errbuf nil "-l" "120" "-w" tmpfile))
        (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
            (progn
              (kill-buffer errbuf)
              (message "Buffer is already dartformated"))
          (dart--apply-rcs-patch patchbuf)
          (kill-buffer errbuf)
          (message "Applied dartformat"))
      (message "Could not apply dartformat. Check errors for details")
      (dartformat--process-errors (buffer-file-name) tmpfile errbuf))
    (kill-buffer patchbuf)
    (delete-file tmpfile)))

(defun dartformat-before-save ()
  (interactive)
  (when (eq major-mode 'dart-mode) (dartformat)))

(provide 'dart-init)
;;; dart-init.el ends here
