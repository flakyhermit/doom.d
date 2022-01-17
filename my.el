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

(defun my-journal-daily (&optional arg)
  "Create a 6PM journal entry."
  (interactive "p")
  (message "%d" arg)
  (let ((filename nil)
        (new-flag nil)
        (time (if (= arg 4)
                  (org-read-date nil 'to-time)
                (encode-time (decode-time))))
        (timestring nil)
        (title ""))
    ;; Check if file already exists, assing if it does
    (setq timestring (format-time-string "%F" time))
    (setq filename
          (seq-find (lambda (filename)
                      (string-match timestring filename))
                    (directory-files journal-directory)))
    ;; If it doesn't,...
    (unless filename
      (setq new-flag t) ;; Set the flag
      ;; read title
      (setq title (with-syntax-table text-mode-syntax-table
                    (capitalize (read-string "Enter the title: " nil nil ""))))
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
          (progn
            (insert "\n" (format-time-string "%A, %d %B %Y" time))
            (goto-char (point-max))
            (insert "\n# " title "\n\n"))
        (apply #'encode-time (decode-time))
        (insert-file-contents filepath)))
    ;; Open file in buffer
    (find-file filepath)
    (goto-char (point-max))
    (message filename)))

;; Configuration
(setq journal-directory (concat (getenv "DROPBOX") "/Notes/journal"))
(setq project-directory (expand-file-name "Projects" (getenv "HOME")))

(global-set-key (kbd "C-c j s") #'my-journal-daily)
(map! :leader "p n" #'my-new-project)

;; org-mode enhancements ---------
(defun my-org-roam-copy-text ()
  "Copy the text content of a roam note in the current buffer"
  (interactive)
  ;; Check if buffer has a roam note
  (when (string=
         (file-name-directory (buffer-file-name))
         (concat org-roam-directory "/"))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^-----" nil t)
          (progn
            (forward-line 1)
            (kill-ring-save (point) (point-max))
            (message "Text copied to clipboard"))
        (message "Incorrectly formatted file")))))

(defun my-find-file-in-notes ()
  "Emacs find-file in the org directory."
  (interactive)
  (let ((default-directory org-directory))
    (call-interactively #'find-file)))

(defun my-find-org-note ()
  "Find a note in the org directory root."
  (interactive)
  (find-file
   (read-file-name "Org file: " org-directory "gtd.org" t nil
                   (lambda (filename)
                     (when (string-match-p ".org$" filename) t)))))

(map! :leader "n f" #'my-find-org-note)

;; Utility functions for text manipulation
(defun my-util-format-comma-space ()
  (interactive)
  (save-excursion
    (let ((beg (region-beginning))
          (end (region-end)))
    (goto-char beg)
    (while (re-search-forward ",[^ ]" end)
      (goto-char (+ 1 (match-beginning 0)))
      (insert " ")
      (setq end (+ 1 end))))))
