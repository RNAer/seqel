;;; fasta-mode -- a major mode for editing fasta files

(require 'seq)

(defvar fasta-mode-hook nil
  "*Hook to setup `fasta-mode'.")


(defvar fasta-mode-map
 (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    (define-key map "\C-cp"     'fasta-position)
    (define-key map "\C-cp"     'fasta-beg-of-seq)
    (define-key map "\C-cp"     'fasta-end-of-seq)
    (define-key map "\C-cp"     'fasta-mark-seq)
    (define-key map "\C-cp"     'fasta-position)
    map)
 "The local keymap for `fasta-mode'")

;;;###autoload
(define-derived-mode fasta-mode text-mode "fasta"
  "Major mode for editing sequences in fasta format.

This mode also customizes isearch to search over line
breaks.  Use \\[universal-argument] number as a prefix to
`fasta-forward-base' to move that many bases.  This skips line
breaks and spaces.

`dna-color-bases-region' disables `font-lock-mode'
automaticly as they cant work together.
\\[dna-color-bases-region] turns `font-lock-mode' back on.

\\{nuc-mode-map}"
  ;; This runs the normal hook change-major-mode-hook, then gets rid of
  ;; the buffer-local variables of the major mode previously in effect.
  (kill-all-local-variables)
  ;; (setq mode-name "fasta")
  ;; (setq major-mode 'fasta-mode)
  (use-local-map fasta-mode-map)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(dna-font-lock-keywords))

  (make-local-variable 'dna-valid-base-regexp)
  (make-local-variable 'dna-sequence-start-regexp)
  (make-local-variable 'dna-cruft-regexp)
  (make-local-variable 'dna-isearch-case-fold-search)

  (run-hooks 'fasta-mode-hook))


;;; Setup functions
(defun fasta-find-file ()
  "Invoke `fasta-mode' if the buffer look like a fasta.
and another mode is not active.
This function is added to `find-file-hooks'."
  (if (and (eq major-mode 'fundamental-mode)
           (looking-at "^\\(>\\)"))
    (fasta-mode)))


;;;###autoload
(defun fasta-add-hooks ()
  "Add a default set of fasta-hooks.
These hooks will activate `fasta-mode' when visiting a file
which has a fasta-like name (.fasta or .fa) or whose contents
looks like fasta.  It will also turn enable fontification for `fasta-mode'."
  (add-hook 'fasta-mode-hook 'turn-on-font-lock)
  (add-hook 'find-file-hooks 'fasta-find-file)
  (add-to-list
   'auto-mode-alist
   '("\\.\\(fasta\\|fa\\|fna\\|faa\\)\\'" . fasta-mode)))


(defvar fasta-seq-type
  "Return the type of sequence, either protein or nucleic acid.")

(defvar fasta-font-lock-keywords
  '(("^\\(>\\)\\([-_.|a-zA-Z0-9]+\\)\\([ \t]+.*\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     (3 font-lock-comment-face nil t))))


(defun fasta-position ()
  "Return the position of point in the current sequence.")

(defun fasta-beg ()
  "Move the point the beginning of the current fasta record.

Return the point of the beginning of the fasta record if the beginning
is found; otherwise, do not move point and return nil."
  (interactive)
  (let ((pos nil))
    (save-excursion
      (end-of-line) ; in case point is at the beginning of the fasta record.
      (if (re-search-backward "^>" nil t)
          (setq pos (match-beginning 0))))
    (cond (pos (goto-char pos))
          ((called-interactively-p 'interactive)
           (error "The beginnning of the fasta record is not found!!!")
           nil))))
    ;; ;; skip white spaces
    ;; (while (looking-at-p seq-space-regexp)
    ;;   (forward-char))))

(defun fasta-end ()
  "Move the point the end of the current fasta record.

Return the point of the end of the fasta record."
  (interactive)
  (let ((old  (point)))
    (end-of-line) ; in case the point is at the regexp
    (if (re-search-forward "^>" nil t)
        (progn (forward-line -1)
               (goto-char (line-end-position)))
      (goto-char (point-max)))))
  ;; ;; skip white spaces
  ;; (while (looking-back seq-space-regexp)
  ;;   (backward-char)))

(defun fasta-mark ()
  "Put point at the beginning of the fasta record and mark the end."
  (interactive)
  (fasta-end)
  (push-mark)
  (fasta-beg))

(defun fasta-format (&optional width)
  "Format the current sequence to contain WIDTH chars per line.

The default width is 80. The white spaces inside will also be removed."
  (interactive "P")
  (or width (setq width 80))
  (save-excursion
    (fasta-mark)
    (forward-line)
    (let ((beg (point)))
      ;; remove seq-spaces
      (goto-char beg)
      (while (re-search-forward seq-space-regexp (region-end) t)
        (replace-match "" nil nil))
      ;; insert newlines
      (goto-char beg)
      ;; (message "%d %d %d" width (point) (region-end))
      (while (< (point) (- (region-end) width))
        (forward-char width)
        (insert-char ?\n)))))

(defun fasta-next ()
  "Move to the beginning of the next fasta record."
  (interactive)
  (let ((old (point))
        (pos nil))
    (save-excursion
      (end-of-line) ; in case the point is at the regexp
      (if (re-search-forward "^>" nil t)
          (setq pos (match-beginning 0))))
    (cond (pos ;; move only when the beginning of next fasta is found.
           (goto-char pos))
          ((called-interactively-p 'interactive)
           (message "This is the last sequence in the file!")
           nil))))

(defun fasta-last ()
  (interactive)
  (while (fasta-next)))


(defun fasta-prev ()
  "Move to the beginning of the previous sequence.

Return nil and do not move if it's already the first sequences."
  (interactive)
  (let ((pos nil))
    (save-excursion
      (end-of-line)
      (setq pos (re-search-backward "^>" (point-min) t 2)))
    (cond (pos ;; move only when the beginning of the seq is found.
           (goto-char pos)) ; return the beg point of the prev fasta
          ((called-interactively-p 'interactive)
           ;; only echo the message when called interactively
           (message "This is the first sequence in the file!")
           nil)))) ; return nil

(defun fasta-first ()
  (interactive)
  (while (fasta-prev)))


(defun fasta-delete-next ()
  (interactive)
  (save-excursion
    (if (fasta-next)
        (delete-region (point) (or (fasta-next)
                                   (point-max)))
      (message "This is the last sequence"))))

(defun fasta-delete-prev ()
  (interactive)
  (save-excursion
    (if (fasta-prev)
        (delete-region (point) (1+ (fasta-end)))
      (message "This is the first sequence"))))

;;; column manipulations
(defun fasta-delete-column ()
  (interactive)
  (save-excursion
    (let ((original (point))
          (column   (current-column)))
      (fasta-first)
      (loop do
            (forward-line) ; move to the sequence region
            (if (or (< (move-to-column column) column)
                    (eq (char-after) ?\n))  ; if at the end of the line.
                (error "This sequence at line %d does not have this column!!!"
                       (line-number-at-pos)))
            (delete-char 1)
            while (fasta-next)))))


(defun fasta-insert-column (char)
  (interactive "sInsert string:")
  (save-excursion
    (let ((original (point))
          (column   (current-column)))
      (fasta-first)
      (loop do
            (forward-line)
            (if (< (move-to-column column) column)
                (error "This sequence at line %d does not have this column!!!"
                       (line-number-at-pos)))
            (insert char)
            while (fasta-next)))))


(defun fasta-mark-column-toggle ()
  (interactive)
  (save-excursion
    (let* ((original (point))
           (column   (current-column))
           (to-face     (if (eq (get-char-property (point) 'face) 'highlight)
                            nil
                          'highlight)))
      (loop do
            (forward-line)
            (if (< (move-to-column column) column)
                (error "This sequence at line %d does not have this column!!!"
                       (line-number-at-pos)))
            (silent-put-text-property (point) (1+ (point)) 'face to-face)
            while (fasta-next)))))


(provide 'fasta-mode)

;; done loading
(run-mode-hooks)



;;; fasta-mode.el ends here
