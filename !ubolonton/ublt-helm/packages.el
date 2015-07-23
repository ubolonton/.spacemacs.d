;;; packages.el --- ublt-helm Layer packages File for Spacemacs
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
(setq ublt-helm-packages
    '(
      helm
      helm-swoop
      ))

;; List of packages to exclude.
(setq ublt-helm-excluded-packages '(smex))

(defun ublt-helm/post-init-helm ()
  (setq helm-candidate-separator "────────────────────────────────────────────────────────────────────────────────")
  (use-package helm-locate
    :defer t
    :config (progn
              (case system-type
                ('gnu/linux (setq helm-locate-command "locate %s -e -A --regex %s")))
              (setq helm-locate-fuzzy-match nil)))
  (use-package helm-imenu
    :defer t
    :config (setq helm-imenu-delimiter " "
                  helm-imenu-execute-action-at-once-if-one nil))
  (use-package helm-org
    :defer t
    :config (setq helm-org-headings-fontify t)))

(defun ublt-helm/post-init-helm-swoop ()
  (setq helm-swoop-use-line-number-face t))
