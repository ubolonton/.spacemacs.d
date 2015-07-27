;;; packages.el --- ublt-editing Layer packages File for Spacemacs
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
(setq ublt-editing-packages
    '(
      whole-line-or-region
      htmlize
      company
      ))

;; List of packages to exclude.
(setq ublt-editing-excluded-packages '())

(defun ublt-editing/init-whole-line-or-region ()
  (use-package whole-line-or-region
    :defer t))

(defun ublt-editing/post-init-htmlize ()
  (setq htmlize-html-charset "utf-8"))

(defun ublt-editing/post-init-company ()
  (setq company-tooltip-flip-when-above t
        company-tooltip-align-annotations t
        company-selection-wrap-around t))
