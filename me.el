;;; me.el -*- lexical-binding: t; -*-
(defun my/journal-daily()
  "Create a new 6-PM journal entry"
  (interactive)
  (setq filepath (concat "~/Dropbox/Notes/6_PMs/" (format-time-string "%F")))
  (with-temp-file filepath
    (insert "\n# " (format-time-string "%A, %d %B %Y") "\n\n"))
  (find-file filepath)
  (markdown-mode)
  (goto-char (point-max)))
(global-set-key (kbd "C-c j s") #'my/journal-daily)

(setq gcpp-file "~/Dropbox/Notes/org/knowledgebase/references/20210114174332-gcpp_takshashila.org")
(after! org
  (push '("g" "GCPP Takshashila") org-capture-templates)
  (push '("gt" "Todo" entry
          (file+headline gcpp-file "Things to-do")
          "* TODO %?\n") org-capture-templates))
