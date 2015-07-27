;; Helper to define keys
(defun ublt/define-keys (key-map &rest ps)
  "Define key binding pairs for KEY-MAP."
  (declare (indent 1))
  (let ((i 0))
    (while (< i (length ps))
      (if (= (mod i 2) 0)
          (let ((src (elt ps i))
                (dst (elt ps (1+ i))))
            (define-key key-map
              (read-kbd-macro src) (if (stringp dst)
                                       (read-kbd-macro dst)
                                     dst))))
      (setq i (+ i 2)))))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<\\(ublt/define-keys\\) \\(.*\\)\\>"
    (1 font-lock-keyword-face)
    (2 font-lock-variable-name-face))
   ("\\<\\(ublt/undefine-keys\\) \\(.*\\)\\>"
    (1 font-lock-keyword-face)
    (2 font-lock-variable-name-face))
   ("\\<\\(ublt/keys\\) *\\(.*\\) *\\_<\\(.*\\)\\>"
    (1 font-lock-keyword-face)
    (2 font-lock-constant-face)
    (3 font-lock-variable-name-face)))
 'append)

(defun ublt/undefine-keys (key-map keys)
  (declare (indent 1))
  (dolist (key keys)
    (define-key key-map (read-kbd-macro key) nil)))

(defmacro ublt/keys (package map &rest mappings)
  (declare (indent 2))
  `(eval-after-load ,package
     (quote (progn
              (ublt/define-keys ,map
                ,@mappings)
              (message "Updated keymap `%s'" ',map)))))



;;; These are translated so that they can be pervasive, as most modes
;;; rebind them (syntactically override), instead of remapping the
;;; original command (semantically override). NTA: Many of these are
;;; now handled system-wide by `autokey', but are kept here just in case
(ublt/define-keys key-translation-map
  ;; OSX conventions
  "s-c"    "M-w"                        ;   copy
  ;; "s-x"    "C-w"                        ; ✂ cut
  "s-v"    "C-y"                        ;   paste
  ;; "s-V"    "M-y"                        ;   paste cycle
  "s-s"    "C-x C-s"                    ;   save
  "s-S"    "C-x s"                      ;   save some buffers
  "s-z"    "C-_"                        ; ↺ undo
  "s-Z"    "M-_"                        ; ↻ redo
  "s-a"    "C-x h"                      ;   mark all
  "s-o"    "C-x C-f"                    ;   open file

  ;; Other super-based translations
  "s-t"    "M-."                        ; push reference
  "s-T"    "M-,"                        ; pop reference

  ;; XXX: ⬅➡ WTF Unicode. There's no RIGHTWARDS BLACK ARROW
  ;; Movement keys (right hand)
  "M-c"    "<up>"                       ; ⬆
  "M-t"    "<down>"                     ; ⬇
  "M-h"    "<left>"                     ; ⬅
  "M-n"    "<right>"                    ; ➡
  "M-C"    "<prior>"                    ; ▲ scroll up
  "M-T"    "<next>"                     ; ▼ scroll down
  "M-H"    "C-<up>"                     ; ⬆ paragraph up
  "M-N"    "C-<down>"                   ; ⬇ paragraph down
  "M-g"    "C-<left>"                   ; ⬅ word left
  "M-r"    "C-<right>"                  ; ➡ word right
  "M-G"    "M-<"                        ; ⇱ buffer home
  "M-R"    "M->"                        ; ⇲ buffer end
  "C-s-t"  "M-<next>"                   ; ▲ other window scroll up
  "C-s-c"  "M-<prior>"                  ; ▼ other window scroll down

  ;; Deletion (left hand)
  "M-e"    "DEL"                        ; ⌫
  "M-u"    "<kp-delete>"                ; ⌦
  "M-."    "M-DEL"                      ; ⌫ delete word
  "M-p"    "M-<kp-delete>"              ; ⌦ delete word

  ;; "Help" `autokey'
  ;; "C-<backspace>" "M-DEL"
  ;; "C-<delete>"    "M-<kp-delete>"
  "<backspace>"   "DEL"
  "<delete>"      "<kp-delete>"
  ;; "<home>"        "C-a"
  ;; "<end>"         "C-e"
  "C-<home>"      "M-<"
  "C-<end>"       "M->"

  "M-i"    "C-k"
  ;; "M-d"    "C-a"
  ;; "M-D"    "C-e"

  ;; Nut!!! But seriously much more effective
  "M-SPC"  "C-SPC"
  "C-SPC"  "M-SPC"

  "C-M-h"  "M-<left>"                ; ⬅ list (except for org-mode)
  "C-M-n"  "M-<right>"               ; ➡ list (except for org-mode)
  "C-M-c"  "M-<up>"                  ; ⤂ paredit splice-kill, org up
  "C-M-t"  "M-<down>"                ; ⤃ paredit splice-kill, org down

  "M-M"    "C-M-u"                      ; ⬉( up list
  "M-V"    "C-M-d"                      ; ⬊( down list

  "M-m"    "M-p"                        ; ⬁ special (history, errors)
  "M-v"    "M-n"                        ; ⬂ special (history, errors)

  "s-4"    "C-x 4"                      ; do something other window
  ;; "s-r"    "C-x r"
  ;; "s-R"    "C-x r j"

  ;; "s-P"    "C-c p"                      ; projectile

  "M-F"    "s-<escape>"                 ; evil's motion state
  "M-f"    "<escape>"                   ; evil's normal state
  )



;;; See the configuration files for zsh (`.zshrc'), readline
;;; (`.inputrc'), Konsole (`default.keytab')
;; (define-key local-function-key-map "[25~" (kbd "<backspace>"))
;; (define-key local-function-key-map "[26~" (kbd "C-<backspace>"))
;; (define-key local-function-key-map "[27~" (kbd "C-<delete>"))

(ublt/define-keys global-map
  ;; Windows manipulation
  "s-1"           'delete-other-windows
  "s-2"           'split-window-vertically
  "s-3"           'split-window-horizontally
  "s-0"           'delete-window
  "s-)"           'bury-buffer
  "s-w"           'other-window
  "s-W"           'ublt/swap-windows

  ;; Utilities, super-
  "s-i"           'helm-semantic-or-imenu
  "s-d"           'helm-command-prefix
  "s-D"           'eproject-ido-imenu
  "s-H"           'ido-switch-buffer
  "s-h"           'helm-mini
  "s-f"           'helm-swoop
  "s-F"           'helm-projectile
  ;; "s-F"           'projectile-find-file
  "s-g"           'magit-status
  "s-G"           'find-grep
  ;; "s-G"           'projectile-grep
  "s-m"           'ace-jump-mode
  "s-M"           'ace-jump-char-mode
  "s-r"           'org-capture
  "s-R"           'org-agenda
  "s-n"           'ublt/switch-to-last-buffer
  "s-b"           'ublt/browse-url-at-point
  "s-B"           'zeal-at-point
  "s-p"           'pop-global-mark
  "s-<backspace>" 'ublt/toggle-alpha
  "s-<return>"    'ublt/toggle-fullscreen
  ;; "s-/"           'find-file-in-project
  "s-/"           'variable-pitch-mode
  "s-\\"          'align-regexp
  "s-u"           'revert-buffer        ; ⟲
  "s-k"           'kill-this-buffer
  ;; "s-l"           'goto-line
  "s-l"           'helm-swoop
  "s-C"           'ublt-editing/duplicate-line
  "s-+"           'text-scale-increase
  "s-="           'text-scale-increase
  "s--"           'text-scale-decrease

  ;; These should be translated
  "s-["           'backward-page
  "s-]"           'forward-page

  ;; "M-w"           'whole-line-or-region-kill-ring-save
  ;; "C-y"           'whole-line-or-region-yank

  ;; "C-_"           'undo-tree-undo
  ;; "M-_"           'undo-tree-redo

  ;; Line/region movement
  "M-s-h"         'textmate-shift-left
  "M-s-n"         'textmate-shift-right
  "M-s-c"         'move-text-up
  "M-s-t"         'move-text-down
  "M-s-˙"         'textmate-shift-left  ; OS X
  "M-s-˜"         'textmate-shift-right ; OS X
  "M-s-ç"         'move-text-up    ; OS X
  "M-s-†"         'move-text-down  ; OS X

  "M-s-=" 'evil-numbers/inc-at-pt
  "M-s--" 'evil-numbers/dec-at-pt

  "C-M-r"         'highlight-symbol-next
  "C-M-g"         'highlight-symbol-prev

  ;; Deletion
  "<kp-delete>"   'delete-char
  "M-<kp-delete>" 'kill-word
  "M-I"           'whole-line-or-region-kill-region

  ;; "C-="           'text-scale-increase

  "M-Q"           'ublt-editing/unfill-paragraph

  ;; ;; Toggling
  ;; "<f9> l"        'global-linum-mode
  ;; "<f9> <f9>"     'ublt/toggle-fonts
  ;; "<f9> <f12>"    'ublt/toggle-line-wrap

  ;; ;; ido-mode
  ;; "C-x C-d"       'ido-dired
  ;; "C-x d"         'ido-list-directory
  ;; "C-x C-i"       'ido-imenu

  ;; Ubuntu
  "M-<f4>"        'kmacro-start-macro-or-insert-counter ; F3 is taken by xbindkeys

  ;; Right, use C-u if you want digit args
  "M-5"           'query-replace
  "M-%"           'query-replace-regexp
  "M-6"           'ublt-editing/toggle-letter-case
  ;; "M-7"           'ublt/toggle-cua-rect
  ;; "M-8"           'ublt/cycle-prose-region
  ;; "M-9"           'mark-enclosing-sexp
  "M-7"           'er/expand-region
  "M-4"           'er/expand-region
  "s-O"           'er/expand-region

  ;; ;; TODO: more pervasive
  ;; "C-a"           'ublt/back-to-indentation-or-line-beginning

  ;; Misc
  "C-c C-x C-o"   'org-clock-out
  "M-l"           'move-to-window-line-top-bottom
  "M-b"           'hippie-expand        ; more convenient here
  "M-B"           nil                   ; use yas-minor-mode-map
  ;; "C-z"           nil                   ; who needs suspend-frame?
  "S-s-SPC"       'whitespace-mode
  ;; "M-x"           'helm-M-x          ; C-x C-m for the original
  "C-h C-a"       'apropos-command
  ;; "C-x C-b"       'ido-switch-buffer     ; Because it's to easy to mis-press
  ;; "C-x C-b"       'helm-mini       ; Because it's to easy to mis-press
  ;; "C-x b"         'helm-mini
  ;; "C-x <return>"  'term
  "C-x B"         'ibuffer
  "C-S-s"         'ublt/isearch-other-window

  "M-<left>"      'backward-list
  "M-<right>"     'forward-list

  ;; ;; XXX: Multimedia keys
  ;; "<XF86Forward>" 'emms-next
  ;; "<XF86Back>"    'emms-previous
  ;; "<XF86Reload>"  'emms-pause

  ;; ;; FIX: Still have to override mode-specific bindings. There must
  ;; ;; be something better
  ;; "M-TAB"         'company-complete

  ;; ;; TODO: Something (were upcase/downcase region)
  ;; "C-x C-u"       'nil
  ;; "C-x C-l"       'nil

  ;; ;; TODO: Rearrange the s- combos
  ;; "C-c p f"       'projectile-find-file

  "M-a"           'hs-toggle-hiding

  "M-o"           'isearch-occur

  ;; "C-<f10>"       'menu-bar-mode
  "C-s"           'isearch-forward-regexp
  "C-r"           'isearch-backward-regexp
  ;; FIX
  "C-M-s"         'isearch-forward
  "C-M-r"         'isearch-backward

  ;; ;; FIX
  ;; "C-c r"         'revert-buffer
  ;; "C-c y"         'bury-buffer

  ;; FIX
  ;; "C-x m"         'eshell
  ;; "C-x C-m"       'shell

  ;; "C-x C-f"       'helm-find-files

  ;; ;; FIX
  ;; "C-c e"         'esk-eval-and-replace
  ;; "M-j"           'join-line

  ;; "C-c g"         'magit-status
  "C-c n"         'ublt-editing/cleanup-buffer

  "s-<mouse-4>"   'text-scale-increase
  "s-<mouse-5>"   'text-scale-decrease
  "S-<mouse-4>"   'backward-paragraph
  "S-<mouse-5>"   'forward-paragraph
  "M-<mouse-4>"   'scroll-down-line
  "M-<mouse-5>"   'scroll-up-line)

(ublt/keys 'org org-mode-map
  "s-i" 'helm-org-in-buffer-headings)

(ublt/keys 'helm helm-map
  "s-h"         'minibuffer-keyboard-quit
  "s-<return> " 'minibuffer-keyboard-quit
  "C-x h"       'helm-mark-all
  "C-f"         'helm-follow-mode
  "s-n"         'helm-select-action
  "s-w"         'ublt-helm/maybe-exit-minibuffer-other-window)
