;; Clean up before saving.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setq
 x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)

 ;; Save clipboard's content into kill-ring before replacing it
 save-interprogram-paste-before-kill t

 ;; Avoid putting consecutive duplicates into `kill-ring'
 kill-do-not-save-duplicates t)

;; Typing/deleting deletes selected text if any.
(delete-selection-mode +1)

(set-selection-coding-system 'utf-8)

(setq-default tab-width 4)
