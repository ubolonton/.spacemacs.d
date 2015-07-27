(defvar ublt-appearance-use-variable-pitch-mode t
  "Whether to use `variable-pitch-mode' as much as possible.")

(setq-default
 ;; This affects how lines are wrapped (we want wrapping at word
 ;; boundary not in the middle of a word).
 word-wrap t
 ;; Truncate lines by default. Only certain modes (mostly prose modes) should
 ;; wrap lines.
 truncate-lines t)
