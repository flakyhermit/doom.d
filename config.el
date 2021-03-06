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
 (setq doom-font (font-spec :family "Iosevka SS04" :size 19 :weight 'regular)
       doom-variable-pitch-font (font-spec :family "Iosevka Etoile" :size 24))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `wdoom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-Iosvkem)

;; Global settings
;; Set environment variables
(setenv "DROPBOX" "~/Dropbox")
(setenv "ORG" (concat (getenv "DROPBOX") "/Notes/org"))

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Scratch buffer message
(setq initial-scratch-message (concat ";; Emacs loaded in " (emacs-init-time) "\n"
                                      ";; This is the *scartch* buffer \n"))
;; (add-load-path! "../.emacs.d/")
;; Set default frame dimensions
(add-to-list 'default-frame-alist '(height . 32))
(add-to-list 'default-frame-alist '(width . 118))

;; Global keybindings
;; Emacs native
(global-set-key (kbd "C-x p") #'mark-paragraph)
(map! :leader "f f"  #'find-file)
(map! "C-x k" #'kill-this-buffer)
(map! :leader "k" #'kill-this-buffer)
(map! :leader "P" #'projectile-find-file)
(map! "<f9>" #'+term/toggle)
(map! :leader "SPC" #'projectile-find-file)
(map! :leader "a" #'evil-switch-to-windows-last-buffer)
(map! :leader "l" #'projectile-find-file)
(map! :leader "f x" #'crux-open-with)
(map! :leader "f r" #'crux-recentf-find-file)
(map! :leader "," #'consult-buffer)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(display-time-mode 1)
(beacon-mode 1)
(display-battery-mode 1)

;; Facilitate local wordnut lookup
(setq +lookup-dictionary-prefer-offline t)
(set-popup-rule! "^\\*" :height 0.4)
(set-popup-rule! "^\\*WordNut*" :height 0.2)

;; File operations
(setq delete-by-moving-to-trash t)
;; Configure backups
(setq make-backup-files t)
;; Encryption key
(setq epa-file-encrypt-to '("754A49D9075B89B3"))

;; Major mode configurations
;; evil ---------------------
;; Move the cursor to the new window when splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; ivy ----------------------
(after! ivy
  (setq ivy-use-selectable-prompt t
        ivy-height 10)
  (amx-mode 1))

;; vertico ------------------
;; (vertico-mode 1)
;; (defun +consult/project-search ()
;;   (interactive)
;;   (consult-ripgrep (doom-project-root)))
;; (defun +consult/directory-search ()
;;   (interactive)
;;   (consult-ripgrep default-directory))
;; (map! :leader "s p" #'+consult/project-search)
;; (map! :leader "s d" #'+consult/directory-search)
;; (map! :leader "s s" #'consult-line)
;; ;; Vertico additives
;; (marginalia-mode 1)
;; (setq completion-styles '(orderless)
;;       completion-category-defaults nil
;;       completion-category-overrides '((file (styles . (partial-completion)))))

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; hydra ---------------------
;; (defhydra+ +hydra/window-nav ()
;; ("w" other-window))
(defhydra hydra-move ()
  "scroll"
  ("u" evil-scroll-up)
  ("d" evil-scroll-down)
  ("f" evil-scroll-page-down)
  ("b" evil-scroll-page-up)
  ("n" (lambda () (evil-next-line 3)))
  ("p" (lambda () (evil-previous-line 3)))
  ("gg" evil-goto-first-line)
  ("G" evil-goto-line))
(define-key evil-motion-state-map (kbd "M-u") 'hydra-move/body)
(define-key evil-motion-state-map (kbd "M-w") '+hydra/window-nav/body)
;; (map! :leader "w" #'+hydra/window-nav/body)

;; projectile ----------------
(setq projectile-switch-project-action #'projectile-find-file)

;; magit ---------------------
(map! :leader "g c a" #'magit-commit-amend)

;; org -----------------------
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (getenv "ORG"))
(defmacro org-path (filename)
    (expand-file-name filename org-directory))
(setq org-ellipsis " › ")
(setq org-global-refile-targets '(((org-path "emacs.org") :maxlevel . 1)
                                  ((org-path "gtd.org"):maxlevel . 2)))
(after! org
  (setq org-agenda-files `(,(org-path "inbox.org")
                           ,(org-path "gtd.org"))
        org-agenda-span 'week
        org-agenda-custom-commands '(("r" "Stuff to add to ROAM" tags-todo "roam")
                                     ("A" "Add to anki" tags-todo "anki")))

  (setq org-todo-keywords '((sequence "TODO(t)" "ACTV(a!)" "|" "HOLD(h)" "CANC(c)" "DONE(d)"))
        org-refile-targets org-global-refile-targets
        org-archive-location (concat org-directory "/archive/%s_archive::"))

  (setq org-startup-with-inline-images t
        org-return-follows-link t
        org-indent-indentation-per-level 1
        org-hide-emphasis-markers t
        org-id-method 'ts
        org-capture-templates
        `(("t" "Add a random capture to GTD" entry
           (file+olp ,(org-path "gtd.org") "Inbox")
           "* %?\n")
          ("W" "Web capture" entry  (file ,(org-path "clippings.org"))
           "* %^{Title}\nSource: %:annotation \nCaptured: %U\n#+begin_quote\n%i\n#+end_quote\n%?"
           :prepend t :empty-lines-before 1)
          ("L" "Web capture link" entry (file ,(org-path "clippings.org"))
           "* %:annotation \nCaptured: %U\n" :prepend t :immediate-finish t :empty-lines-before 1)
          ("Q" "A QUOTE" entry
           (file ,(org-path "quotes.org"))
           "* %?\n\n")
          ("i" "Add a BLOG post IDEA" entry
           (file+headline ,(org-path "blog.org") "Posts")
           "* %?\nCaptured On: %U")
          ("b" "Add a BLOG journal entry" entry
           (file+olp+datetree ,(org-path "blog.org") "Journal")
           "* %?\n")
          ("g" "Today's gratitude journal" item
           (file+datetree ,(org-path "gratitude.org"))
           "- %?\n")
          ("b" "Add a BLOG journal entry" entry
           (file+olp+datetree ,(org-path "blog.org") "Journal")
           "* %?\n")
          ("p" "A project IDEA" entry
           (file+headline ,(org-path "projects.org") "Ideas")
           "* %?\n")
          ("B" "Add a BOOK to the 'considering' list" entry
           (file+olp ,(org-path "lists/books.org") "Considering")
           "* <book%?\n")
          ("r" "Add an ARTICLE to read later" checkitem
           (file+olp+datetree ,(org-path "lists/read-later.org"))
           "- [ ] %:annotation %?\n")
          ("e" "Emacs")
          ("ed" "A DOOM EMACS tip or customizaiton idea" entry
           (file+olp+datetree ,(org-path "emacs/doom.org"))
           "* %?\n")
          ("ee" "An Emacs customization idea" entry
           (file+headline ,(org-path "emacs/emacs.org") "To-do")
           "* TODO %? \n\n")
          ("eo" "An ORG mode tip/customization" entry
           (file+headline ,(org-path "emacs/org.org") "To-do")
           "* TODO %? \n\n")
          ("v" "Add a word to the vocabulary list" plain
           (file+headline ,(org-path "vocabulary.org") ,(format-time-string "%F"))
           "<voc%?\n")))
  (set-popup-rule! "^\\*Org Agenda*" :height 0.6))
(map! :leader "X" #'org-capture)
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
  '(term :size 15))
;; (doom-themes-set-faces nil
;;   '(org-document-title :weight 'bold))
(setq org-superstar-headline-bullets-list "ᗒ")
(setq org-superstar-item-bullet-alist
  '((?* . ?-)
    (?+ . ?➤)
    (?- . ?»)))
;; LaTeX export configuration
(setq org-latex-compiler "xelatex"
      org-latex-pdf-process (list (concat "latexmk -"
                                          org-latex-compiler
                                          " -recorder -synctex=1 -bibtex -bibtex-cond %b")))
(setq org-latex-default-packages-alist
      '(("" "graphicx" t)
        ("" "grffile" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("" "hyperref" nil)))
;; There's a local `org-latex-classes' set in .dir-locals.el in the org-directory

;; org-roam ------------------
(setq org-roam-directory (org-path "knowledgebase")
      org-roam-capture-templates `(("d" "default"
                                    plain "\nLabels: %?"
                                   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                      "#+title: ${title}\n#+created: %U\n")
                                   :unnarrowed t)
                                   ("r" "reference"
                                    plain "%?"
                                   :if-new (file+head "references/%<%Y%m%d%H%M%S>-${slug}.org"
                                                      "#+title: ${title}\n#+roam_key: ${ref}\n#+created: %U\n")
                                   :unnarrowed t))
      org-roam-capture-ref-templates (list (nth 1 org-roam-capture-templates)))

(setq org-roam-buffer-position 'right
      org-roam-db-update-method 'immediate
      org-roam-tag-sources '(prop last-directory)
      +org-roam-open-buffer-on-find-file nil)
(add-hook! org-roam-mode #'org-roam-bibtex-mode)
(add-hook 'org-mode-hook #'prose-mode)
;; Custom functions
(defun org-roam-switch-db ()
  "Switch between multiple org-roam knowledgebases"
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let ((org-roam-directory-list '("~/Dropbox/Notes/org/journal" "~/Dropbox/Notes/org/knowledgebase")))
    (cd (completing-read "Pick database: " org-roam-directory-list))
    (find-file "index.org")
    (org-roam-db-build-cache)
    (org-roam-find-file)))
(defun org-roam-capture-ref ()
  (interactive)
  (org-roam-capture nil "r"))
;; Misc
(doom-themes-set-faces nil
  '(org-roam-link :inherit 'org-link :underline nil))
(map! :leader
      (:prefix-map ("r" . "roam")
      :desc "org-roam" "r" #'org-roam-buffer-toggle
      :desc "org-roam-node-insert" "i" #'org-roam-node-insert
      ;; :desc "org-roam-insert-immediate" "I" #'org-roam-insert-immediate
      :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
      :desc "org-roam-node-find" "f" #'org-roam-node-find
      :desc "org-roam-ref-find" "F" #'org-roam-ref-find
      :desc "org-roam-show-graph" "g" #'org-roam-show-graph
      :desc "org-roam-capture" "c" #'org-roam-capture
      :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today
      :desc "org-roam-dailies-find-today" "t" #'org-roam-dailies-find-today
      (:prefix ("d" . "dailies")
       :desc "Yesterday" "y" #'org-roam-dailies-find-yesterday
       :desc "Today" "t" #'org-roam-dailies-find-today
       :desc "Tomorrow" "m" #'org-roam-dailies-find-tomorrow
       :desc "Arbitary date" "d" #'org-roam-dailies-find-date)))
(map! :leader
      (:prefix-map ("r" . "roam")
       :desc "Switch database" "s" #'org-roam-switch-db))
;; markdown-mode --------------
(add-hook 'markdown-mode-hook #'prose-mode)
(doom-themes-set-faces nil
  '(markdown-header-face-1 :height 150 :inherit 'markdown-header-face))
;; (setq markdown-command "pandoc -t html --css ~/.emacs.d/mdhtmlstyle.css input.md -o output.pdf")

;; mixed-pitch-mode ------------
(after! mixed-pitch
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-todo)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-drawer)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-done)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-column)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-column-title)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-indent)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-date))

(define-minor-mode prose-mode
  "Visual tweaks for editing prose"
  :lighter " prose"
  (if (eq prose-mode t)
      (progn
        (display-line-numbers-mode -1)
        (mixed-pitch-mode 1)
        (setq line-spacing 4)
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
     :file-name "references/%<%Y%m%d%H%M%S>-${citekey}"
     :head "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_tags: ref\n#+created: %U\n\n* ${title}\n%?"
     :unnarrowed t)))
(define-key mode-specific-map (kbd "n a") 'orb-note-actions)
(define-key mode-specific-map (kbd "n f") 'orb-find-non-ref-file)
;; (map! :leader "r F" #'orb-find-non-ref-file)
(map! :leader "r o" #'orb-note-actions)

(use-package! org-ref
  :after org)

;; org-ref --------------------
(setq org-ref-notes-directory (org-path "knowledgebase/references")
      org-ref-default-bibliography (expand-file-name "references.bib" org-ref-notes-directory)
      org-ref-pdf-directory "~/Dropbox/Zotero/") ;; TODO Not required now. Fix this later.

;; helm-bibtex ----------------
(setq bibtex-completion-pdf-field "file"
      bibtex-completion-bibliography org-ref-default-bibliography
      bibtex-completion-library-path '("~/Dropbox/Zotero/"))
(map! :leader "r h" #'helm-bibtex)

;; ;; deft -----------------------
;; (setq deft-directory org-directory
;;       deft-recursive t
;;       deft-auto-save-interval -1.0
;;       deft-extensions '("org")
;;       deft-default-extension "org")
;; (define-key global-map (kbd "<f8>") 'deft)
;; (define-key mode-specific-map (kbd "f") 'deft-find-file)
;; (map! :leader "f o" #'deft-find-file)

;; Minor modes
;; olivetti
(setq olivetti-body-width 120)
(map! :leader :desc "Olivetti mode" "t o"  #'olivetti-mode)
(add-hook 'olivetti-mode-hook (lambda () (hide-mode-line-mode 'toggle)))
;; yasnippets
(add-to-list 'yas-snippet-dirs (expand-file-name "snippets" (doom-dir)))
(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))
;; multi-term
(setq multi-term-dedicated-window-height 12)

;; Custom loads
(load! "my.el")
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
