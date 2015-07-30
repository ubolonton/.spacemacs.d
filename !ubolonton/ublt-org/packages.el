;;; packages.el --- ublt-org Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq ublt-org-packages
    '(
      org
      ox-reveal
      ))

;; List of packages to exclude.
(setq ublt-org-excluded-packages '())

(defun ublt-org//set-up-basics ()
  (setq
   ;; This makes it less noisy and more pleasant to read. Messes up table
   ;; formatting though.
   org-hide-emphasis-markers t
   ;; Fontify code blocks
   org-src-fontify-natively t
   ;; This allows nicely theming headings as section separators.
   org-fontify-whole-heading-line t
   ;; Show all headlines by default
   org-startup-folded t
   ;; Don't show empty lines in collapsed view
   org-cycle-separator-lines 0
   ;; Don't interpret _ and ^ as sub/super-scripts without quoting with {}.
   org-use-sub-superscripts '{}
   org-export-with-sub-superscripts '{}
   ;; Place tags right after headings instead of trying to right-align.
   org-tags-column 0

   ;; Heading visual indentation
   org-indent-indentation-per-level 2
   ;; List additional (on top of 2) indentation
   org-list-indent-offset 1
   ;; Different list levels should use different bullets
   org-list-demote-modify-bullet '(("-" . "+")
                                   ("+" . "-")
                                   ("1." . "1)")
                                   ("1)" . "1."))
   ;; Allow using alphabetical bullets
   org-alphabetical-lists t
   ;; 2 lines to terminate lists
   org-empty-line-terminates-plain-lists nil
   ;;
   org-list-use-circular-motion t

   ;; Intelligent (dwim) bindings
   org-special-ctrl-a/e t
   org-special-ctrl-k t

   ;; Don't split current heading, create a new one
   org-M-RET-may-split-line nil
   ;; Create new heading after the current content
   org-insert-heading-respect-content t
   ;; Disallow editing folded content
   org-catch-invisible-edits 'error

   ;; Don't use isearch there, normal isearch is enough
   org-goto-auto-isearch nil
   ;; Don't use `outline-path-completion'. `ido-imenu' is better
   org-goto-interface 'outline

   ;; Whole path instead of level-by-level navigation
   org-outline-path-complete-in-steps nil
   org-refile-use-outline-path t

   ;; Don't number headlines
   org-export-with-section-numbers nil

   org-completion-use-ido t
   org-read-date-prefer-future 'time
   org-indirect-buffer-display 'current-window)

  (use-package org-indent
    :defer t
    :init (add-hook 'org-mode-hook #'org-indent-mode)))

;; Task management, GTD.
(defun ublt-org//set-up-GTD ()
  (setq
   org-todo-keywords
   '((sequence "TODO(t)" "STARTED(s!)"
               "WAITING(w@)" "PAUSED(p)"
               "|"
               "DONE(d@)" "CANCELLED(c@)"))

   ;; TODO: Add more colors
   org-todo-keyword-faces
   '(("TODO" :foreground "#F86155" :weight normal)
     ("PAUSED" :foreground "Firebrick" :weight normal)
     ("STARTED" :foreground "DarkGoldenRod" :weight normal)
     ("CANCELLED" :foreground "SeaGreen" :weight normal)
     ("DONE" :foreground "LightGreen" :weight normal))

   org-use-fast-todo-selection t

   ;; Allow S-<left> and S-<right> to change state without logging the change
   org-treat-S-cursor-todo-selection-as-state-change nil

   ;; Use with `org-toggle-ordered-property'
   org-enforce-todo-dependencies t

   ;; org-stuck-projects '("+PROJECT/-DONE" ("DONE") ("*") "")
   ;; org-stuck-projects '("" nil nil "")

   org-tag-alist '((:startgroup . nil)
                   ("@Home" . ?h) ("@Work" . ?w) ("@Road" . ?o)
                   (:endgroup . nil)
                   ("Emacs" . e)
                   ("Conkeror" . c)
                   ("Movie" . m)
                   ("Book" . b))

   ;; Log a lot
   org-log-done 'note
   org-log-reschedule 'note
   org-log-repeat 'time
   org-log-redeadline t
   org-log-note-clock-out t)

  (use-package org-clock
    :defer t
    :config
    (defun ublt-org//clock-in-to-next (kw)
      (when (equal kw "TODO")
        "STARTED"))

    (setq
     org-clock-history-length 24
     org-clock-in-resume t

     org-clock-into-drawer t
     org-clock-out-remove-zero-time-clocks t
     org-clock-out-when-done t
     org-clock-persistent t

     org-clock-in-switch-to-state 'ublt-org//clock-in-to-next)

    (org-clock-persistence-insinuate))

  (use-package org-agenda
    :defer t
    :config
    (defun ublt-org//verify-refile-target ()
      (not (member (nth 2 (org-heading-components)) org-done-keywords)))

    (setq
     org-agenda-dim-blocked-tasks t
     org-agenda-start-on-weekday nil
     org-agenda-skip-scheduled-if-deadline-is-shown nil
     org-agenda-skip-deadline-if-done nil
     org-agenda-skip-scheduled-if-done nil
     org-agenda-span 'week

     org-agenda-time-grid '((daily today require-timed)
                            "-----------------------"
                            (list 0700 0800
                                  1000 1100 1200
                                  1400 1500 1600 1700 1800 1900
                                  2100 2200 2300))

     ;; org-scheduled-past-days 365

     org-agenda-log-mode-items '(closed clock state)

     org-agenda-tags-column 0
     org-agenda-window-setup 'current-window
     org-agenda-restore-windows-after-quit t

     org-agenda-persistent-filter t
     org-agenda-persistent-marks t

     ;; TODO: Maybe more
     org-agenda-files '("~/org/gtd/someday.org"
                        "~/org/gtd/tasks.org"
                        "~/org/gtd/projects.org"
                        "~/org/gtd/journal.org")

     ;; TODO: Use another file, as this is meant for notes not actually tasks
     org-default-notes-file "~/org/gtd/someday.org"

     ;; TODO: Restructure
     org-refile-targets '((("~/org/gtd/someday.org") . (:maxlevel . 1))
                          (("~/org/gtd/tasks.org") . (:maxlevel . 1))
                          (("~/org/gtd/projects.org") . (:maxlevel . 2))
                          (("~/org/work/fram/work-notes.org") . (:maxlevel . 3)))

     org-refile-allow-creating-parent-nodes 'confirm
     org-refile-target-verify-function 'ublt-org//verify-refile-target

     ;; org-columns-default-format "%TODO %50ITEM %TAGS %CATEGORY"

     ;; TODO: More
     org-agenda-custom-commands
     '(("P" "Projects summary"
        ((tags "PROJECT"
               ((org-agenda-todo-list-sublevels nil)))))
       ("p" "Projects details"
        ((tags "PROJECT")))
       ("w" "Work"
        ((agenda "")
         (tags-todo "SCHEDULED=\"\""))
        ((org-agenda-files '("~/org/work/fram/work-notes.org"))
         (org-agenda-ndays 1)
         (org-agenda-sorting-strategy
          '((agenda todo-state-up time-up priority-down)))
         (org-deadline-warning-days 0)
         (org-agenda-prefix-format
          '((agenda  . "%i %-9:c%?-12t%-12s")
            (tags  . "%i %-9:c%?-12t")))))
       ("D" "Daily action list"
        (
         ;; Tasks with a date (deadline, scheduled)
         (agenda "")
         ;; Unscheduled tasks
         (tags-todo "SCHEDULED=\"\""))
        ((org-agenda-files '("~/org/gtd/tasks.org"
                             "~/org/gtd/projects.org"))
         (org-agenda-ndays 2)
         (org-agenda-sorting-strategy
          '((agenda todo-state-up time-up priority-down)))
         (org-deadline-warning-days 0)
         (org-agenda-prefix-format
          '((agenda  . "%i %-9:c%?-12t%-12s")
            (tags  . "%i %-9:c%?-12t")))))
       ("c" "Tasks in progress (current)"
        ((todo "STARTED")))
       ("S" "\"Someday\" task list"
        ((todo "TODO" ((org-agenda-files '("~/org/gtd/someday.org"))
                       (org-agenda-sorting-strategy
                        '((todo . (category-down))))
                       (org-agenda-prefix-format
                        '((todo . "%-9:c")))))))))

    ;; Try to align tags dynamically.
    (defun ublt-org//org-agenda-align-tags ()
      (let ((org-agenda-tags-column (- 4 (window-width))))
        (org-agenda-align-tags)))
    (add-hook 'org-agenda-finalize-hook #'ublt-org//org-agenda-align-tags))

  ;; Task creation (capturing/remembering)
  (use-package org-capture
    :defer t
    :config
    (setq
     org-capture-templates
     '(
       ("n" "Note or todo to be reviewed at day's end"
        entry (file "~/org/gtd/daily.org")
        (file "~/org/gtd/templates/quick-note.org")
        :empty-lines-before 1)
       ;; TODO: Capture link sent or clipboard content sent from other
       ;; places

       ;; Handle both internal invocation and external protocol handling
       ;; org-protocol://capture://l/<url>/<title or " ">[/description]

       ("x" "Capture stuff sent from external sources")
       ("xl" "Link to check"
        entry (file "~/org/gtd/daily.org")
        (file "~/org/gtd/templates/quick-link.org")
        :empty-lines-before 1)
       ("xt" "Talk to watch"
        entry (file+headline "~/org/gtd/someday.org" "Talks")
        (file "~/org/gtd/templates/someday-link.org")
        :empty-lines-before 1)
       ("xa" "Article to read"
        entry (file+headline "~/org/gtd/someday.org" "Articles")
        (file "~/org/gtd/templates/someday-link.org")
        :empty-lines-before 1)
       ("xp" "Paper to read"
        entry (file+headline "~/org/gtd/someday.org" "Papers")
        (file "~/org/gtd/templates/someday-link.org")
        :empty-lines-before 1)
       ("xb" "Book to read"
        entry (file+headline "~/org/gtd/someday.org" "Books")
        (file "~/org/gtd/templates/someday-link.org")
        :empty-lines-before 1)
       ("xm" "Movie to watch"
        entry (file+headline "~/org/gtd/someday.org" "Movies")
        (file "~/org/gtd/templates/someday-link.org")
        :empty-lines-before 1)

       ("t" "Task to review at the end of today"
        entry (file "~/org/gtd/daily.org")
        (file "~/org/gtd/templates/quick-task.org")
        :empty-lines-before 1)

       ("T" "Task that starts now"
        entry (file "~/org/gtd/daily.org")
        (file "~/org/gtd/templates/immediate-task.org")
        :empty-lines-before 1
        ;; FIX: Clocking in doesn't seem to work, sometimes
        :clock-in t)


       ;; TODO: Break this down by type
       ("s" "Someday, do this (task or project)!"
        entry (file+headline "~/org/gtd/someday.org" "Uncategorized")
        (file "~/org/gtd/templates/quick-task.org")
        :empty-lines-before 1)

       ("P" "New project to start soon (not someday)"
        entry (file+headline "~/org/gtd/projects.org" "Projects")
        (file "~/org/gtd/templates/quick-project.org")
        :empty-lines-before 1)

       ;; i Idea to be reviewed at day's end :IDEA:
       ;; n Note to be reviewed at day's end (saw, listened something)
       ;; t Task to be reviewed at day's end
       ;; T Task that will be started immediately
       ;; s Task for some day
       ;; d Daily review entry
       ;; w Weekly review entry
       ;; p Task belonging to a project (use file+function)
       ;; P New project
       ;; rt Talk review
       ;; rb Book review
       ;; rm Movie review



       ;; ("d" "Diary"
       ;;  entry (file "~/org/gtd/journal.org")
       ;;  "** %U %^{Diary} :Diary:%^g\n%i%?")
       ("i" "Idea"
        entry (file "~/org/gtd/journal.org")
        "** %U %^{Thought} :Thought:%^g\n%i%?")
       ("r" "Review"
        entry (file "~/org/gtd/journal.org")
        "** %t Daily Review :Coach:\n%[~/org/gtd/templates/daily-review.txt]\n" )
       ;; TODO: Add a project's task template that asks for a project
       ;; then put it under that project
       )))

  ;; Integration with Conkeror for capturing links.
  (use-package org-protocol
    :defer t
    :config
    ;; FIX: Distinguish Conkeror from other sources
    (defvar ublt-org--org-capture-external? nil)
    ;; Set the flag before capturing.
    (defadvice org-protocol-capture (around set-external-flag activate)
      (setq ublt-org--org-capture-external? t)
      (condition-case err ad-do-it
        (error (setq ublt-org--org-capture-external? nil)
               (signal (car err) (cdr err)))))
    ;; Unset it afterward.
    (defadvice org-capture-finalize (around unset-external-flag activate)
      (unwind-protect ad-do-it
        (setq ublt-org--org-capture-external? nil)))
    ;; Kill Conkeror's current buffer if capture was confirmed. Switch
    ;; to Conkeror regardless.
    (defun ublt-org//org-capture-back-to-conkeror ()
      (when ublt-org--org-capture-external?
        (start-process
         "wmctrl" nil "wmctrl" "-x" "-a" "conkeror")
        (unless org-note-abort
          (start-process
           "conkeror" nil "conkeror" "-f" "kill-current-buffer"))))
    (add-hook 'org-capture-after-finalize-hook
              #'ublt-org//org-capture-back-to-conkeror)))

(defun ublt-org//get-string-from-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defvar ublt-org--latex-classes-dir
  (file-name-directory load-file-name))

(defun ublt-org//set-up-pdf-export ()
  (use-package ox-latex
    :defer t
    :config
    ;; Use minted for code highlighting in exported pdf files.
    ;; http://joat-programmer.blogspot.nl/2013/07/org-mode-version-8-and-pdf-export-with.html
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (setq org-latex-listings 'minted
          org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode %f"
                                  "xelatex -shell-escape -interaction nonstopmode %f")
          org-latex-toc-command ""
          ;; Don't use `inputenc', `fontenc' (xelatex `fontspec' is
          ;; supposed to handle unicode better)
          org-latex-default-packages-alist
          '(("" "fixltx2e" nil)
            ("" "graphicx" t)
            ("" "longtable" nil)
            ("" "float" nil)
            ("" "wrapfig" nil)
            ("" "rotating" nil)
            ("normalem" "ulem" t)
            ("" "amsmath" t)
            ("" "textcomp" t)
            ("" "marvosym" t)
            ("" "wasysym" t)
            ("" "amssymb" t)
            ("" "hyperref" nil)
            "\\tolerance=1000"))
    (add-to-list 'org-latex-classes
                 `("ublt-org-article"
                   ,(concat ublt-org--latex-classes-dir "article.tex")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

(defun ublt-org/post-init-org ()
  (ublt-org//set-up-basics)
  (ublt-org//set-up-GTD)
  (ublt-org//set-up-pdf-export))

(defun ublt-org/init-ox-reveal ()
  (use-package ox-reveal
    :defer t
    :config
    (setq org-reveal-root "reveal.js/"
          org-reveal-transition "linear"
          org-reveal-transition-speed "fast"
          org-reveal-history t
          org-reveal-control t
          org-reveal-progress t
          org-reveal-rolling-links nil)))
