;;; client.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Jewel James
;;
;; Author: Jewel James <https://github.com/monk>
;; Maintainer: Jewel James <flakyhermit@protonmail.ch>
;; Created: August 08, 2021
;; Modified: August 08, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/monk/client
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar url-root "localhost")
(defvar url-port)
(defvar joplin-token)
(defvar joplin-folder-id)
(defvar joplin-archive-id)

(defun joplin--request (url)
  "Send a request at URL and return the response as a hash table."
  (let ((response-string nil) error-string)
    (with-current-buffer (url-retrieve-synchronously url :silent t)
      (setq error-string (buffer-substring-no-properties (point-min) (line-end-position)))
      (if (string-match "200 OK" error-string)
          (progn
            (goto-char (point-max))
            (setq response-string
                  (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position)))
                (json-parse-string response-string))
        (error "Error with the request: %s" error-string)))))
    ;; (message response-string)

(defun joplin--get-notes (folder-id)
  "Get all notes from the folder with FOLDER-ID."
  (let ((endpoint (concat "/folders/" folder-id "/notes"))
        (url nil)
        (data-hash nil))
    (setq url (joplin--make-url endpoint "fields=id,title,created_time,body&order_by=created_time&order_dir=DESC"))
    (setq data-hash (joplin--request url))
    ;; (message "%S" data-hash)
    data-hash))

(defun joplin--make-url (endpoint &optional options)
  "Compute the API url for the given ENDPOINT. OPTIONS are extra query parameters."
  (let ((url nil)
        (base (format "http://%s:%d" url-root url-port)))
    (setq url (concat base "/" endpoint "?token=" joplin-token
                      (if options
                          (format "\&%s" options)
                        nil)))
    url))

(defun joplin--note-change-parent (note-id parent-id)
  "Change the parent of note with NOTE-ID to PARENT-ID."
  (let ((url (joplin--make-url (concat "/notes/" note-id)))
        (url-request-method "PUT")
        (url-request-data (format "{ \"parent_id\": \"%s\" }" parent-id)))
    (url-retrieve-synchronously url)))

(defun joplin--delete-note (note-id)
  "Change the parent of note with NOTE-ID to PARENT-ID."
  (let ((url (joplin--make-url (concat "/notes/" note-id)))
        (url-request-method "DELETE"))
    (url-retrieve-synchronously url)))

(defun joplin--markdown-to-org (text)
  "Convert TEXT in Joplin custom Markdown to Org."
  (with-temp-file ".temp"
    (insert text))
  (my-link-convert-function
   (shell-command-to-string
    "pandoc --wrap=none -f markdown -t org < .temp")))

;; {Simple node link} {Simple node with: alternate text}
;; {r: Reference link}
(defun my-link-convert-function (str)
  (let ((output)
        (titles (org-roam-db-query [:select [title id] :from nodes])))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      ;; Find the node
      (require 'org-roam)
      (while (re-search-forward "{\\(r:\\)?\\([^\n:]+\\)\\(:\\)? ?\\([^\n]+?\\)}" nil t)
        (let ((result nil) (lowest 1))
          (dolist (el titles lowest)
            ;; Calculate Levenshtein distance
            (let* ((title (downcase (car el)))
                   (title-len (length title))
                   (match (downcase (match-string 2))) (ratio))
              (setq ratio
                    (/ (float (string-distance title match))
                       (max title-len (length match))))
              ;; Calculate ratio
              (when (< ratio lowest)
                ;; If can't, pick the closest match
                (setq lowest ratio)
                (setq result el))))
          (when (< lowest 0.4)
            ;; Add the link
            (let ((id (car (cdr result)))
                  (file (car result)))
              (if (match-string 1)
                  ;; Ref replace
                  (progn
                    (setq citekey
                          (org-roam-db-query [:select [ref type]
                                              :from refs
                                              :where (= node-id $s1)] id))
                    (when citekey
                      (replace-match (format "cite:%s" (caar citekey)))))
                ;; Normal replace
                (if (match-string 3)
                    (replace-match (format "[[id:%s][%s]]"
                                           (car (cdr result)) (match-string 4)))
                  (replace-match (format "[[id:%s][%s]]"
                                         (car (cdr result)) (car result))))
                )))))
      (setq output (buffer-string)))
    output))

;; End-user interactive functions
(defun my-joplin-notes-archive ()
  "Archive imported fleeting notes."
  (interactive)
  (let ((notes-array (gethash "items" (joplin--get-notes joplin-folder-id))))
    (mapc (lambda (note-hash)
            (let ((id (gethash "id" note-hash)))
              (joplin--note-change-parent id joplin-archive-id)))
          notes-array)))

(defun my-joplin-notebook-clear ()
  (interactive)
  (let ((notes-array nil)
        (notebook-id joplin-notebook-id-journal)
        (note-body nil))
    (setq notes-array (gethash "items" (joplin--get-notes notebook-id)))
    (mapc (lambda (note-hash)
            (joplin--delete-note (gethash "id" note-hash))) notes-array)))


(defun my-joplin-notebook-import (notebook-id target-folder)
  "Convert Joplin notes to md files"
  (interactive)
  (let ((notes-array nil)
        (note-body nil))
    (setq notes-array (gethash "items" (joplin--get-notes notebook-id)))
    (unless (file-directory-p target-folder)
      (make-directory target-folder))
    (mapc (lambda (note-hash)
            (setq note-body (gethash "body" note-hash))
            (setq note-title
                  (replace-regexp-in-string "[\s/]" "_" (string-trim (gethash "title" note-hash))))
            (setq note-date
                  (format-time-string "%F" (/ (gethash "created_time" note-hash) 1000)))
            (setq filename (concat note-date "_" note-title))
            (with-temp-file
                (concat target-folder "/" filename ".md")
              (insert note-body))) notes-array)))

(defun my-joplin-import-journal ()
  (interactive)
  (my-joplin-notebook-import joplin-notebook-id-journal "~/Dropbox/Notes/journal/test"))

(defun my-joplin-notes-import ()
  "Convert Joplin notes to org-roam."
  (interactive)
  (let ((notes-array nil)
        (note-body nil))
    (setq notes-array (gethash "items" (joplin--get-notes joplin-folder-id)))
    (mapc (lambda (note-hash)
            (setq note-body (joplin--markdown-to-org (gethash "body" note-hash)))
            ;; Roam capture
            ;; Temporary capture template
            (let ((org-roam-capture-templates
                   `(("j" "new note"
                      plain ,(concat "\nLabels: %?\n\n" note-body)
                      :if-new (file+head
                               "%<%Y%m%d%H%M%S>-${slug}.org"
                               "#+title: ${title}\n#+created: %U\n")
                      :unnarrowed t)))
                  (org-roam-capture-new-node-hook '(org-roam-link-replace-all)))
              (org-roam-capture- :goto nil :keys "j"
                                 :node (org-roam-node-create :title (gethash "title" note-hash)))))
          notes-array)))

;; Configuration
(setq url-port 41184)
(setq url-root "localhost")
(setq joplin-token "008fe8b4c90b8f8168040c183feb97f1d1f8eed9f35f753752351f844fa1fb189d8aa9024d82c5b4eb54d842d5522a89befb5495c810371d9e1c000020c7419c")
;; Set folder-id for 'Fleeting Notes'
(setq joplin-folder-id "d1ad212972dc4a13a3b0bd14c303dcfa")
(setq joplin-notebook-id-journal "a7979765fa0a42c88344292cac4f07ce")
(setq joplin-archive-id "9dd4213766d44ecaa516bae474c482fa")
(provide 'joplin-api-calls)
;;; joplin-api-calls.el ends here

