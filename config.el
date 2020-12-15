;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jewel James"
      user-mail-address "flakyhermit@protonmail.ch")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
 (setq doom-font (font-spec :family "JetBrains Mono" :size 19 :weight 'regular)
       doom-variable-pitch-font (font-spec :family "Iosevka Etoile" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; My configurations
;; (add-load-path! "../.emacs.d/")
;; Set default frame dimensions
(add-to-list 'default-frame-alist '(height . 32))
(add-to-list 'default-frame-alist '(width . 110))

;; Global keybindings
(map! "C-x k" #'kill-this-buffer)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/Notes/org")
(setq org-global-refile-targets '(("~/Dropbox/Notes/org/emacs.org" :maxlevel . 1)
			          ("~/Dropbox/Notes/org/gtd.org" :maxlevel . 2)))
(setq org-return-follows-link t
      org-todo-keywords '((sequence "TODO(t)" "ACTV(a!)" "REFL(r)" "|" "HOLD(h)" "CANC(c)" "DONE(d)"))
      org-inbox-file "~/Dropbox/Notes/org/inbox.org"
      org-agenda-files '("~/Dropbox/Notes/org")
      org-refile-targets org-global-refile-targets
      org-archive-location (concat org-directory "/archive/%s_archive::")
      org-startup-with-inline-images t
      org-indent-indentation-per-level 1
    ;; org-adapt-indentation nil
      org-hide-emphasis-markers t
      org-capture-templates
      `(("t" "Add a random capture to GTD" entry
	(file+olp ,(concat org-directory "/gtd.org") "Inbox")
	"* %?\n")
	("T" "Just a THOUGHT" entry
	(file ,(concat org-directory "/inbox.org"))
	"* %?\n")
	("Q" "A QUOTE" entry
	(file ,(concat org-directory "/quotes.org"))
	"* %?\n\n")
	("b" "Add a BLOG post IDEA" entry
	(file ,(concat org-directory "/blog-post-ideas.org"))
	"* %?\n")
	("p" "A project IDEA" entry
	(file ,(concat org-directory "/projects.org"))
	"* %?\n")
	("B" "Add a BOOK to the 'considering' list" entry
	(file+olp ,(concat org-directory "/lists/books.org") "Considering")
	"* <book%?\n")
	("k" "Add a BOOK to read with Krys" entry
	(file+olp ,(concat org-directory "/lists/books.org") "Shelved" "Fiction")
	"* <book%? :krys:\n")
	("r" "Add an ARTICLE to read later" checkitem
	(file+olp+datetree ,(concat org-directory "/lists/read-later.org"))
	"- [ ] %:annotation %?\n")
	("v" "Add a word to the vocabulary list" plain
	(file+headline ,(concat org-directory "/vocabulary.org") ,(format-time-string "%F"))
	"<voc%?\n")
	("e" "An Emacs customization idea" entry
	(file+headline ,(concat org-directory "/emacs.org") "To-do")
	"* TODO %? \n\n")))
(map! :leader "X" #'counsel-org-capture)
(map! :leader "A" #'org-agenda)
(map! :map mode-specific-map "a" #'org-agenda)
(map! :map mode-specific-map "c" #'counsel-org-capture)
(defun org-refile-global ()
  "Refile to the global refile target list"
  (interactive)
  (let ((org-refile-targets org-global-refile-targets))
    (org-refile)))
(map! "C-c 0 C-w" #'org-refile-global)
(add-hook! org-capture #'yas-expand)
(add-hook! org-capture (lambda () (insert "Cuck yeah")))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; olivetti -----------------
(setq olivetti-body-width 120)
(map! :map ctl-x-map "t o" olivetti-mode)

(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((org-log-done . time)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
