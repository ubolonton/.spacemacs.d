;;; packages.el --- ublt-git Layer packages File for Spacemacs
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
(setq ublt-git-packages
    '(
      magit
      git-commit
      ))

;; List of packages to exclude.
(setq ublt-git-excluded-packages '())

(defun ublt-git/post-init-magit ()
  (setq
   ;; magit status buffer should not be a pop-up (in the sense of not
   ;; being volatile or temporary like helm buffer). This is important
   ;; for small screens.
   magit-status-buffer-switch-function 'switch-to-buffer

   ;; Show refined diffs for the highlighted hunk.
   magit-diff-refine-hunk t

   ;; Show whitespace errors only in status buffers.
   magit-diff-paint-whitespace 'status

   ;; Use repo base names in buffer names
   magit-status-buffer-name-format   "*magit: %b*"
   magit-refs-buffer-name-format     "*magit-refs: %b*"
   magit-log-buffer-name-format      "*magit-log: %b*"
   magit-cherry-buffer-name-format   "*magit-cherry: %b*"
   magit-reflog-buffer-name-format   "*magit-reflog: %b*"
   magit-process-buffer-name-format  "*magit-process: %b*"
   magit-stashes-buffer-name-format  "*magit-stashes: %b*"
   magit-stash-buffer-name-format    "*magit-stash: %b*"
   magit-diff-buffer-name-format     "*magit-diff: %b*"
   magit-revision-buffer-name-format "*magit-rev: %b*"))

(defun ublt-git/post-init-git-commit ()
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))
