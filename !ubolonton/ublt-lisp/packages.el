;;; packages.el --- ublt-lisp Layer packages File for Spacemacs
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
(setq ublt-lisp-packages
    '(
      company
      elisp-slime-nav
      ))

;; List of packages to exclude.
(setq ublt-lisp-excluded-packages '())

;; XXX: Most of these should be in spacemacs.

(defun ublt-lisp/init-company ()
  (use-package company
    :defer t
    :config (add-hook 'ielm-mode-hook #'company-mode)))

(defun ublt-lisp/init-elisp-slime-nav ()
  (use-package elisp-slime-nav
    :defer t
    :init (progn
            (add-hook 'ielm-mode-hook #'elisp-slime-nav-mode)
            (evil-leader/set-key-for-mode 'inferior-emacs-lisp-mode
              "mgg" 'elisp-slime-nav-find-elisp-thing-at-point
              "mhh" 'elisp-slime-nav-describe-elisp-thing-at-point))))
