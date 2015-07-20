;;; packages.el --- ublt-js Layer packages File for Spacemacs
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
(setq ublt-js-packages
    '(
      js2-mode
      flycheck
      company-tern
      ))

;; List of packages to exclude.
(setq ublt-js-excluded-packages '())

(defun ublt-js/post-init-js2-mode ()
  (setq js2-highlight-level 3
        js2-basic-offset 2))

(defun ublt-js/post-init-flycheck ()
  (setq-default flycheck-jshintrc "~/.jshint.json"))

(defun ublt-js/post-init-company-tern ()
  (setq company-tern-property-marker " ."
        company-tern-meta-as-single-line t))
