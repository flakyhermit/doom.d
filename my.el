;;; me.el -*- lexical-binding: t; -*-

;; Definitions
;; journal -----------------------
(defcustom journal-directory nil
  "My personal journal directory.")
(defcustom project-directory nil
  "My projects directory")

(defun remove-punctuation (string)
  "Remove punctuations from STRING"
  (replace-regexp-in-string "[,.:]" "" string))

(defun my-new-project (title &optional nogit)
  "Create a new project with TITLE.
Prefix argument NOGIT prevents a git repository being initialized in the project directory."
  ;; Check if project existss
  (interactive "M\Enter project name: \nP")
  (save-excursion
  (let ((dir (expand-file-name title project-directory)))
    (unless (file-exists-p dir)
      (make-directory dir))
    (find-file dir)
    (unless nogit
      (magit-call-git "init" (magit-convert-filename-for-git
                              (expand-file-name dir))))
    (call-interactively #'find-file))))

(defun my-journal-daily (title &optional arg)
  "Create a 6PM journal entry using TITLE."
  (interactive "M\Enter the title: \np")
  (message "%d" arg)
  (let ((filename nil)
        (new-flag nil)
        (time (if (= arg 4)
                  (org-read-date nil 'to-time)
                (encode-time (decode-time))))
        (timestring nil)
        (title (with-syntax-table text-mode-syntax-table
                 (capitalize title))))
    ;; Check if file already exists, assing if it does
    (setq timestring (format-time-string "%F" time))
    (setq filename
          (seq-find (lambda (filename)
                      (string-match timestring filename))
                    (directory-files journal-directory)))
    ;; If it doesn't,...
    (unless filename
      (setq new-flag t) ;; Set the flag
      ;; ...construct the new name
      (setq filename
            (concat
             timestring
             "_"
             (string-join (split-string (remove-punctuation title) " ") "_")
                      ".gpg")))
    (setq filepath (concat journal-directory "/" filename))
    ;; Write data into file in the background
    (with-temp-file filepath
      (if new-flag
          (insert "\n" (format-time-string "%A, %d %B %Y" time))
        (apply #'encode-time (decode-time))
        (insert-file-contents filepath))
      (goto-char (point-max))
      (insert "\n# " title "\n\n"))
    ;; Open file in buffer
    (find-file filepath)
    (goto-char (point-max))
    (message filename)))

;; Configuration
(setq journal-directory (concat (getenv "DROPBOX") "/Notes/6_PMs"))
(setq project-directory (expand-file-name "Projects" (getenv "HOME")))

(global-set-key (kbd "C-c j s") #'my-journal-daily)
(map! :leader "p n" #'my-new-project)
