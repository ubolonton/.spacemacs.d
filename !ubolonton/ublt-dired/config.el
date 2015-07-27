(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")
(setq-default dired-omit-mode t)

(setq
 ;; Smart search, defaulting to file names only.
 dired-isearch-filenames 'dwim

 ;; Offer the other window's path as default when copying.
 dired-dwim-target t

 ;; Don't hide symlink targets.
 dired-hide-details-hide-symlink-targets nil

 ;; Better guesses for compressed files.
 dired-guess-shell-alist-user
 (list
  (list "\\.t\\(ar\\.bz2\\|bz\\)\\'"
        "tar xvjf"
        "bunzip2 -c * | tar xvf -"
        ;; Extract files into a separate subdirectory
        '(concat "mkdir " (file-name-sans-extension file)
                 "; bunzip2 -c * | tar -C "
                 (file-name-sans-extension file) " -xvf -")
        ;; Optional decompression.
        "bunzip2"))
)
