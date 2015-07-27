;;; Swap windows
;;; `http://sites.google.com/site/steveyegge2/my-dot-emacs-file'
(defun ublt/swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))



;;; Often we want to switch back-n-forth between 2 buffers.
(defun ublt/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))



;; Browse URL/link at point.
(if (functionp 'org-open-at-point-global)
    (defalias 'ublt/browse-url-at-point 'org-open-at-point-global)
  (defun ublt/browse-url-at-point ()
    (interactive)
    (case major-mode
      ('org-mode (call-interactively 'org-open-at-point))
      (t (call-interactively 'browse-url-at-point)))))
