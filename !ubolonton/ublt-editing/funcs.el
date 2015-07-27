(defun ublt-editing/duplicate-line (prefix)
  "Copies/cuts/duplicates whole line if no region is selected."
  (interactive "p")
  ;; FIX: This looks dirty
  (call-interactively 'whole-line-or-region-kill-ring-save)
  (call-interactively 'whole-line-or-region-yank))

(defun ublt-editing/unfill-paragraph ()
  "Does the inverse of `fill-paragraph', by calling it with
`fill-column' set to a large number."
  (interactive)
  (let ((fill-column (point-max)))
    (call-interactively 'fill-paragraph)))

(defun ublt-editing/cleanup-buffer ()
  (interactive)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))



(defun ublt-editing/toggle-letter-case ()
  "Toggles the letter case of current symbol or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”.
See `http://ergoemacs.org/emacs/modernization_upcase-word.html.'
"
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'symbol)))
        (setq p1 (car bds) p2 (cdr bds))))
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps"))
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "title case"))
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps"))
         (t (put this-command 'state "all lower")))))
    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2)
      (put this-command 'state "title case")
      (echo "Title Case"))
     ((string= "title case" (get this-command 'state))
      (upcase-region p1 p2)
      (put this-command 'state "all caps")
      (echo "ALL CAPS"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2)
      (put this-command 'state "all lower")
      (echo "all lower")))))
