;;; packages.el --- ublt-appearance Layer packages File for Spacemacs
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
(setq ublt-appearance-packages
    '(
      woman
      htmlize
      face-remap
      prog-mode
      ))

;; List of packages to exclude.
(setq ublt-appearance-excluded-packages '())

(use-package prog-mode
  :defer t
  :config (progn
            (defun ublt-appearance//add-watch-words ()
              (font-lock-add-keywords
               nil '(("\\<\\(FIXME\\|TODO\\|FIX\\|XXX\\|HACK\\|REFACTOR\\|NOCOMMIT\\|NTA\\)"
                      1 font-lock-warning-face t))))
            (add-hook 'prog-mode-hook #'ublt-appearance//add-watch-words)))

(defun ublt-appearance/init-woman ()
  (use-package woman
    :defer t
    :config (setq woman-fill-column 80
                  woman-fill-frame t
                  woman-default-indent 7)))

(defun ublt-appearance/post-init-htmlize ()
  (setq htmlize-css-name-prefix "htmlize-"
        htmlize-html-major-mode 'html-mode))

(defun ublt-appearance/init-face-remap ()
  (defun ublt-appearance//enable-variable-pitch-mode ()
    (variable-pitch-mode +1))
  (use-package face-remap
    :defer t
    :init (when ublt-appearance-use-variable-pitch-mode
            (dolist (hook '(help-mode-hook
                            apropos-mode-hook
                            Info-mode-hook
                            Man-mode-hook
                            woman-mode-hook))
              (add-hook hook #'ublt-appearance//enable-variable-pitch-mode)))))
