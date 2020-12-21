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
 (setq doom-font (font-spec :family "Input Mono Narrow" :size 19 :weight 'regular)
       doom-variable-pitch-font (font-spec :family "Input Mono Narrow" :size 22))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-sourcerer)

;; Global settings
;; Scratch buffer message
(setq initial-scratch-message (concat ";; \n;; Emacs loaded in " (emacs-init-time) "\n;; -----------------------------------\n;; Howdy Jewel! Welcome to Emacs.\n;; Today is " (format-time-string "%d %B, %A")  "\n"))
;; (add-load-path! "../.emacs.d/")
;; Set default frame dimensions
(add-to-list 'default-frame-alist '(height . 36))
(add-to-list 'default-frame-alist '(width . 120))

;; Global keybindings
(map! "C-x k" #'kill-this-buffer)
(map! "<f9>" #'+term/toggle)

;; Important hooks

;; Custom loads
(load! "me.el")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'absolute)

(display-battery-mode 1)
(beacon-mode 1)

;; Major mode configurations
;; evil ---------------------
;; Move the cursor to the new window when splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; org -----------------------
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/Notes/org")
(setq org-ellipsis " ▼ ")
(setq org-global-refile-targets '(("~/Dropbox/Notes/org/emacs.org" :maxlevel . 1)
                                  ("~/Dropbox/Notes/org/gtd.org" :maxlevel . 2)))
(after! org
  (setq org-return-follows-link t
        org-todo-keywords '((sequence "TODO(t)" "ACTV(a!)" "REFL(r)" "|" "HOLD(h)" "CANC(c)" "DONE(d)"))
        org-inbox-file "~/Dropbox/Notes/org/inbox.org"
        org-agenda-files '("~/Dropbox/Notes/org")
        org-agenda-span 'week
        org-refile-targets org-global-refile-targets
        org-archive-location (concat org-directory "/archive/%s_archive::")
        org-startup-with-inline-images t
        org-indent-indentation-per-level 1
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
          ("e" "Emacs")
          ("ed" "A DOOM EMACS tip or customizaiton idea" entry
           (file+olp+datetree ,(concat org-directory "/doom.org"))
           "* %?\n")
          ("ee" "An Emacs customization idea" entry
           (file+headline ,(concat org-directory "/emacs.org") "To-do")
           "* TODO %? \n\n")
          ("v" "Add a word to the vocabulary list" plain
           (file+headline ,(concat org-directory "/vocabulary.org") ,(format-time-string "%F"))
           "<voc%?\n"))))
(map! :leader "X" #'counsel-org-capture)
(map! :leader "A" #'org-agenda)
(map! :map mode-specific-map "a" #'org-agenda)
(map! :map mode-specific-map "c" #'counsel-org-capture)
(add-to-list 'org-modules 'org-habit)
(defun org-refile-global ()
  "Refile to the global refile target list"
  (interactive)
  (let ((org-refile-targets org-global-refile-targets))
    (org-refile)))
(map! "C-c 0 C-w" #'org-refile-global)
(add-hook! org-capture-mode #'yas-expand)
(doom-themes-set-faces nil
  '(org-document-title :height 150))

;; org-roam ------------------
(setq org-roam-directory (concat org-directory "/knowledgebase")
      org-roam-capture-templates `(("d" "default"
                                    plain #'org-roam-capture--get-point
                                   "\n- References :: \n- Tags :: %?\n\n"
                                   :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                   :head "#+title: ${title}\n#+created: %U\n"
                                   :unnarrowed t)))
(setq org-roam-buffer-position 'bottom
      org-roam-buffer-height 12)
(add-hook! org-roam-mode #'org-roam-bibtex-mode)
(add-hook 'org-mode-hook #'prose-mode)
(doom-themes-set-faces nil
  '(org-roam-link :inherit 'org-link :underline nil))

;; markdown-mode --------------
(add-hook 'markdown-mode-hook #'prose-mode)
(doom-themes-set-faces nil
  '(markdown-header-face-1 :height 150 :inherit 'markdown-header-face))

(define-minor-mode prose-mode
  "Visual tweaks for editing prose"
  :lighter " prose"
  (if (eq prose-mode t)
      (progn
        (display-line-numbers-mode -1)
        (mixed-pitch-mode 1)
        (setq line-spacing 3)
        (setq left-margin-width 2)
        (set-window-buffer nil (window-buffer)))
    (progn
      (display-line-numbers-mode 1)
      (mixed-pitch-mode -1)
      (kill-local-variable 'line-spacing)
      (kill-local-variable 'left-margin-width)
      (set-window-buffer nil (window-buffer)))))

;; org-roam-bibtex ------------
(setq orb-templates
  '(("r" "ref" plain (function org-roam-capture--get-point) ""
     :file-name "references/${citekey}"
     :head "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_tags: ref\n#+created: %U\n" ; <--
     :unnarrowed t)))
(define-key mode-specific-map (kbd "n a") 'orb-note-actions)
(define-key mode-specific-map (kbd "n f") 'orb-find-non-ref-file)
(map! :leader "n r F" #'orb-find-non-ref-file)

;; deft -----------------------
(setq deft-directory org-directory
      deft-recursive t
      deft-auto-save-interval -1.0
      deft-extensions '("org")
      deft-default-extension "org")
(define-key global-map (kbd "<f8>") 'deft)
(define-key mode-specific-map (kbd "f") 'deft-find-file)
(map! :leader "f o" #'deft-find-file)

;; Minor modes
;; olivetti
(setq olivetti-body-width 120)
(map! :leader :desc "Olivetti mode" "t o"  #'olivetti-mode)
(add-hook 'olivetti-mode-hook (lambda () (hide-mode-line-mode 'toggle)))
;; yasnippets
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))
;; multi-term
(setq multi-term-dedicated-window-height 12)

;; Custom faces
;; (custom-theme-set-faces 'doom-Iosvkem ())

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
