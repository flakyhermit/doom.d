;;; me.el -*- lexical-binding: t; -*-

;; Definitions
;; journal -----------------------
(defvar journal-directory nil
  "My personal journal directory.")

(defun my-journal-daily (title)
  "Create a 6PM journal entry using TITLE."
  (interactive "M\Enter the title: ")
  (let ((filename nil)
        (new-flag nil)
        (title (with-syntax-table text-mode-syntax-table
                 (capitalize title))))
    ;; Check if file already exists, assing if it does
    (setq filename
          (seq-find (lambda (filename)
                      (string-match (format-time-string "%F") filename))
                    (directory-files journal-directory)))
    ;; If it doesn't,...
    (unless filename
      (setq new-flag t) ;; Set the flag
      ;; ...construct the new name
      (setq filename
            (concat
             (format-time-string "%F_")
             (string-join (split-string title " ") "_")
                      ".gpg")))
    (setq filepath (concat journal-directory "/" filename))
    ;; Write data into file in the background
    (with-temp-file filepath
      (if new-flag
          (insert "\n" (format-time-string "%A, %d %B %Y"))
        (insert-file-contents filepath))
      (goto-char (point-max))
      (insert "\n# " title "\n\n"))
    ;; Open file in buffer
    (find-file filepath)
    (goto-char (point-max))
    (message filename)))

;; Configuration
(setq journal-directory (concat (getenv "DROPBOX") "/Notes/6_PMs"))
(global-set-key (kbd "C-c j s") #'my-journal-daily)
