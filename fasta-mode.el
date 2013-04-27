(defvar nuc-mode-hook nil
  "*Hook to setup `nuc-mode'.")

(defvar nuc-mode-load-hook nil
  "*Hook to run when `nuc-mode' is loaded.")

(defvar nuc-setup-on-load nil
  "*If not nil setup nuc mode on load by running `dna-`add-hook's'.")

;;;###autoload
(define-minor-mode nuc-mode
  "This turns on and off a minor mode for nucleic acid sequences."
  :lighter " nuc"
  :keymap nuc-mode-map)

(defun nuc-mode ()
  "Minor mode for editing nucleotide sequences.

This mode also customizes isearch to search over line
breaks.  Use \\[universal-argument] number as a prefix to
`dna-forward-base' to move that many bases.  This skips line
breaks and spaces.

`dna-color-bases-region' disables `font-lock-mode'
automaticly as they cant work together.
\\[dna-color-bases-region] turns `font-lock-mode' back on.

\\{nuc-mode-map}"
  (interactive)
  ;;
  (kill-all-local-variables)
  (setq mode-name "dna")
  (setq major-mode 'nuc-mode)
  (use-local-map nuc-mode-map)
  ;;
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(dna-font-lock-keywords))
  ;;
  (make-local-variable 'dna-valid-base-regexp)
  (make-local-variable 'dna-sequence-start-regexp)
  (make-local-variable 'dna-cruft-regexp)
  (make-local-variable 'dna-isearch-case-fold-search)
  ;;
  (run-hooks 'nuc-mode-hook))



;;; Setup functions
(defun dna-find-file-func ()
  "Invoke `nuc-mode' if the buffer look like a sequence.
and another mode is not active.
This function is added to `find-file-hooks'."
  (if (and (eq major-mode 'fundamental-mode)
           (looking-at dna-sequence-start-regexp))
    (nuc-mode)))

;;;###autoload
(defun dna-add-hooks ()
  "Add a default set of dna-hooks.
These hooks will activate `nuc-mode' when visiting a file
which has a dna-like name (.fasta or .gb) or whose contents
looks like dna.  It will also turn enable fontification for `nuc-mode'."
  (add-hook 'nuc-mode-hook 'turn-on-font-lock)
  (add-hook 'find-file-hooks 'dna-find-file-func)
  (add-to-list
   'auto-mode-alist
   '("\\.\\(fasta\\|fa\\|exp\\|ace\\|gb\\)\\'" . nuc-mode)))



;; done loading
(run-hooks 'nuc-mode-load-hook)
