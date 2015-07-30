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
      ))

;; List of packages to exclude.
(setq ublt-org-excluded-packages '())

(defun ublt-org/post-init-org ()
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
