;;; Apparently this works much better than dired-do-async-shell-command and
;; nohup trickeries
(defun ublt-dired/open-native ()
  "Open marked files (or the file the cursor is on) from dired."
  (interactive)
  (let* ((files (dired-get-marked-files t current-prefix-arg))
         (n (length files)))
    (when (or (<= n 3)
              (y-or-n-p (format "Open %d files?" n)))
      (dolist (file files)
        (call-process (case system-type
                        ('darwin "open")
                        ('gnu/linux "xdg-open"))
                      nil 0 nil file)))))
