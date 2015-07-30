;;; packages.el --- ublt Layer packages File for Spacemacs
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
(setq ublt-packages
    '(
      help+
      help-mode+
      help-fns+
      flycheck
      ))

;; List of packages to exclude.
(setq ublt-excluded-packages '())

(defun ublt/init-help+ ()
  (use-package help+))

(defun ublt/init-help-mode+ ()
  (use-package help-mode+))

(defun ublt/init-help-fns+ ()
  (use-package help-fns+))

(defun ublt/post-init-flycheck ()
  (setq flycheck-display-errors-delay 0))
