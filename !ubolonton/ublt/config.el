;; Clean up before saving.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;
(setq save-place-limit 3000)

;; Remember more states.
(dolist (var '(log-edit-comment-ring
               Info-history-list
               Info-search-history))
  (add-to-list 'savehist-additional-variables var))

(setq bookmark-default-file "~/.emacs.d/.bookmarks")

;; Unified diffs (context and +/-).
(setq diff-switches "-u")

;;; Open files with certain extensions using an external program
;;; (opening a large PDF file can hang Emacs).
(defvar ublt/find-file-externally-extensions
  '("pdf" "xls" "xlsx" "doc" "docx"))
(defadvice find-file (around open-externally activate)
  (let ((file-name (ad-get-arg 0)))
    (if (member (file-name-extension file-name) ublt/find-file-externally-extensions)
        (call-process (case system-type
                        ('darwin "open")
                        ('gnu/linux "xdg-open"))
                      nil 0 nil file-name)
      ad-do-it)))
