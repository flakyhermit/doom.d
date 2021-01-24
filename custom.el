(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/Dropbox/Notes/org/knowledgebase/references/20210114174332-gcpp_takshashila.org" "/home/monk/Dropbox/Notes/org/archlinux.org" "/home/monk/Dropbox/Notes/org/bike.org" "/home/monk/Dropbox/Notes/org/blog-post-ideas.org" "/home/monk/Dropbox/Notes/org/contacts.org" "/home/monk/Dropbox/Notes/org/cv.org" "/home/monk/Dropbox/Notes/org/devices.org" "/home/monk/Dropbox/Notes/org/diabetes.org" "/home/monk/Dropbox/Notes/org/doom.org" "/home/monk/Dropbox/Notes/org/emacs-article.org" "/home/monk/Dropbox/Notes/org/emacs.org" "/home/monk/Dropbox/Notes/org/events.org" "/home/monk/Dropbox/Notes/org/facts.org" "/home/monk/Dropbox/Notes/org/finances.org" "/home/monk/Dropbox/Notes/org/gtd.org" "/home/monk/Dropbox/Notes/org/higher-studies.org" "/home/monk/Dropbox/Notes/org/hindi.org" "/home/monk/Dropbox/Notes/org/idioms.org" "/home/monk/Dropbox/Notes/org/inbox.org" "/home/monk/Dropbox/Notes/org/income-generation-ideas.org" "/home/monk/Dropbox/Notes/org/key-issues.org" "/home/monk/Dropbox/Notes/org/kindle-support.org" "/home/monk/Dropbox/Notes/org/latex-test.org" "/home/monk/Dropbox/Notes/org/listen.org" "/home/monk/Dropbox/Notes/org/long-term-goals.org" "/home/monk/Dropbox/Notes/org/minecraft.org" "/home/monk/Dropbox/Notes/org/observations.org" "/home/monk/Dropbox/Notes/org/podcasts.org" "/home/monk/Dropbox/Notes/org/projects.org" "/home/monk/Dropbox/Notes/org/purchases.org" "/home/monk/Dropbox/Notes/org/pvc-diy.org" "/home/monk/Dropbox/Notes/org/quotes.org" "/home/monk/Dropbox/Notes/org/resources.org" "/home/monk/Dropbox/Notes/org/self-improvement.org" "/home/monk/Dropbox/Notes/org/thoughts.org" "/home/monk/Dropbox/Notes/org/typing-test.org" "/home/monk/Dropbox/Notes/org/vocabulary.org" "/home/monk/Dropbox/Notes/org/watch.org"))
 '(safe-local-variable-values
   '((org-latex-classes
      ("article" "\\RequirePackage{fix-cm}
\\PassOptionsToPackage{svgnames}{xcolor}
\\documentclass[10pt]{article}
\\usepackage{fontspec}
\\setmainfont{IBM Plex Serif}
\\setsansfont[Scale=MatchLowercase]{Cabin}
\\setmonofont[Scale=MatchLowercase]{IBM Plex Mono}
\\usepackage{sectsty}
\\allsectionsfont{\\sffamily}
\\usepackage{enumitem}
\\setlist[description]{style=unboxed,font=\\sffamily\\bfseries}
\\usepackage{listings}
\\lstset{frame=single,aboveskip=1em,
	framesep=.5em,backgroundcolor=\\color{AliceBlue},
	rulecolor=\\color{LightSteelBlue},framerule=1pt}
\\usepackage{xcolor}
\\newcommand\\basicdefault[1]{\\scriptsize\\color{Black}\\ttfamily#1}
\\lstset{basicstyle=\\basicdefault{\\spaceskip1em}}
\\lstset{literate=
	    {§}{{\\S}}1
	    {©}{{\\raisebox{.125ex}{\\copyright}\\enspace}}1
	    {«}{{\\guillemotleft}}1
	    {»}{{\\guillemotright}}1
	    {Á}{{\\'A}}1
	    {Ä}{{\\\"A}}1
	    {É}{{\\'E}}1
	    {Í}{{\\'I}}1
	    {Ó}{{\\'O}}1
	    {Ö}{{\\\"O}}1
	    {Ú}{{\\'U}}1
	    {Ü}{{\\\"U}}1
	    {ß}{{\\ss}}2
	    {à}{{\\`a}}1
	    {á}{{\\'a}}1
	    {ä}{{\\\"a}}1
	    {é}{{\\'e}}1
	    {í}{{\\'i}}1
	    {ó}{{\\'o}}1
	    {ö}{{\\\"o}}1
	    {ú}{{\\'u}}1
	    {ü}{{\\\"u}}1
	    {¹}{{\\textsuperscript1}}1
            {²}{{\\textsuperscript2}}1
            {³}{{\\textsuperscript3}}1
	    {ı}{{\\i}}1
	    {—}{{---}}1
	    {’}{{'}}1
	    {…}{{\\dots}}1
            {⮠}{{$\\hookleftarrow$}}1
	    {␣}{{\\textvisiblespace}}1,
	    keywordstyle=\\color{DarkGreen}\\bfseries,
	    identifierstyle=\\color{DarkRed},
	    commentstyle=\\color{Gray}\\upshape,
	    stringstyle=\\color{DarkBlue}\\upshape,
	    emphstyle=\\color{Chocolate}\\upshape,
	    showstringspaces=false,
	    columns=fullflexible,
	    keepspaces=true}
\\usepackage[a4paper,margin=1in]{geometry}
\\usepackage{parskip}
\\makeatletter
\\renewcommand{\\maketitle}{%
  \\begingroup\\parindent0pt
  \\sffamily
  \\huge{\\bfseries\\@title}\\par\\bigskip
  \\Large{\\bfseries\\@author}\\par\\medskip
  \\normalsize\\@date\\par\\bigskip
  \\endgroup\\@afterindentfalse\\@afterheading}
\\makeatother
[DEFAULT-PACKAGES]
\\hypersetup{linkcolor=Blue,urlcolor=DarkBlue,
  citecolor=DarkRed,colorlinks=true}
\\AtBeginDocument{\\renewcommand{\\UrlFont}{\\ttfamily}}
[PACKAGES]
[EXTRA]"
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}" . "\\paragraph*{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
      ("report" "\\documentclass[11pt]{report}"
       ("\\part{%s}" . "\\part*{%s}")
       ("\\chapter{%s}" . "\\chapter*{%s}")
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
      ("book" "\\documentclass[11pt]{book}"
       ("\\part{%s}" . "\\part*{%s}")
       ("\\chapter{%s}" . "\\chapter*{%s}")
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
     (org-log-into-drawer . t)
     (org-refile-targets)
     (eval progn
           (flyspell-mode 1)
           (markdown-toggle-markup-hiding t))
     (org-log-done . time))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-face-1 ((t (:height 150 :inherit markdown-header-face))))
 '(org-document-title ((t (:height 150))))
 '(org-roam-link ((t (:inherit org-link :underline nil)))))
