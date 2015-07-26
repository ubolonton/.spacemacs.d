;;; config.el --- ublt-helm functions File for Spacemacs
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

(defvar ublt-helm--exit-other-window-p nil)

(defun ublt-helm/maybe-exit-minibuffer-other-window ()
  (interactive)
  ;; We cannot use `let' because the action is executed after exiting
  ;; the minibuffer, not during.
  (when ublt-helm-enable-exit-other-window
    (setq ublt-helm--exit-other-window-p t))
  (call-interactively 'helm-maybe-exit-minibuffer))

;;; For some reason combining them into a single around advice didn't work.
(defadvice helm-execute-selection-action-1 (before ublt-helm/maybe-other-window activate)
  (when (and ublt-helm-enable-exit-other-window
             ublt-helm--exit-other-window-p)
    (when (= (count-windows) 1)
      (split-window-horizontally))
    (other-window 1)))

(defadvice helm-execute-selection-action-1 (around ublt-helm/maybe-other-window-cleanup activate)
  (unwind-protect ad-do-it
    (setq ublt-helm--exit-other-window-p nil)))

(defun ublt-helm//enable-follow-mode (pkg-source-pairs)
  (dolist (pkg-source pkg-source-pairs)
    (let ((pkg (car pkg-source))
          (source (cdr pkg-source)))
      (eval-after-load pkg
        `(helm-attrset 'follow 1 ,source)))))
