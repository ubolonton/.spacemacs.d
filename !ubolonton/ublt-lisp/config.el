(defface ublt-lisp-paren
  '((((class color) (background dark))
     (:foreground "grey40"))
    (((class color) (background light))
     (:background "grey60")))
  "Face used to dim (Lisp) parentheses."
  :group 'personal)

(dolist (mode '(scheme-mode
                emacs-lisp-mode
                lisp-mode
                clojure-mode
                clojurescript-mode))
  (font-lock-add-keywords mode '(("(\\|)" . 'ublt-lisp-paren))))

(dolist (hook '(scheme-mode-hook
                emacs-lisp-mode-hook
                lisp-mode-hook
                ielm-mode-hook
                clojure-mode-hook
                clojurescript-mode-hook
                cider-repl-mode-hook))
  (add-hook hook (lambda () (rainbow-delimiters-mode -1)) t))
