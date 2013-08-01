;;; fasta-mode -- a major mode for editing fasta files

(require 'seq)

(defvar fasta-mode-hook nil
  "*Hook to setup `fasta-mode'.")


(defvar fasta-mode-map
 (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    (define-key map "\C-cp"     'fasta-position)
    (define-key map "\C-\M-a"   'fasta-beg)
    (define-key map "\C-\M-e"   'fasta-end)
    (define-key map "\C-cm"     'fasta-mark-seq)
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
  (setq mode-name "fasta")
  (setq major-mode 'fasta-mode)
  (use-local-map fasta-mode-map)
  ;; (set-syntax-table fasta-mode-syntax-table)

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




(defvar fasta-font-lock-keywords
  '(("^\\(>\\)\\([-_.|a-zA-Z0-9]+\\)\\([ \t]+.*\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     (3 font-lock-comment-face nil t))))


(defvar fasta-record-regexp "^>.*$"
  "Fasta label that delimits records.")


;;;###autoload
(defun fasta-beg (count)
  "Move the point the beginning of the fasta record.

Return the point of the beginning of the fasta record if the beginning
is found; otherwise, do not move point and return nil."
  (interactive "p")
  (while (and (> count 0)
              (re-search-backward fasta-record-regexp nil t))
    (setq count (1- count)))
  count)

;;;###autoload
(defun fasta-end (count)
  "Move forward to the next end fasta record.

Return the point of the end of the fasta record."
  (interactive "p")
  (let ((pos (point))  found)
    (and (looking-at-p fasta-record-regexp)
         (setq count (1+ count)))
    (while (and (re-search-forward fasta-record-regexp nil t)
                (> count 0))
      (setq count (1- count)))
    (if (> count 0)
        (progn (goto-char (point-max))
               (setq count (1- count)))
      (beginning-of-line)))
  count)

;;;###autoload
(defun fasta-last ()
  (interactive)
  (while (fasta-end 1)))

;;;###autoload
(defun fasta-first ()
  (interactive)
  (while (fasta-beg 1)))


(defun fasta-count ()
  (interactive)
  (let ((total 0))
    (save-excursion
      (goto-char (point-max))
      (while (eq (fasta-beg 1) 0)
        (setq total (1+ total))))
    (if (called-interactively-p 'interactive)
        (message "Total %d sequences" total))
    total))

(defun fasta-mark ()
  "Put point at the beginning of the fasta record and mark the end."
  (interactive)
  (fasta-end 1)
  (push-mark)
  (fasta-beg 1))


;;;###autoload
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


(defun fasta-delete-next ()
  (interactive)
  (save-excursion
    (if (fasta-end 1)
        (delete-region (point) (or (fasta-end 1)
                                   (point-max)))
      (message "This is the last sequence"))))

(defun fasta-delete-prev ()
  (interactive)
  (save-excursion
    (if (fasta-prev)
        (delete-region (point) (1+ (fasta-end)))
      (message "This is the first sequence"))))


(defun fasta-check ()
  "Check the validity of the fasta format.

It will check: 1. The file is not empty;
               2. No fasta record is empty.
It will run whenever fasta-mode is enabled."
  (interactive)
  (let (pos)
    (save-excursion
      (fasta-first)
      (loop do
            (setq pos (line-end-position))
            (fasta-end)
            (while (looking-back seq-space-regexp)
              (backward-char))
            ;; (message "%d %d" pos (point))
            (if (eq pos (point))
                (error "The fasta record is empty at line %d" (line-number-at-pos)))
            while (fasta-next)))))


(defun fasta-position ()
  "Return the position of point in the current sequence.

It will not count white spaces and seq gaps. The count starts
at zero."
  (interactive)
  (let ((pos   (point))
        (count 0))
    (save-excursion
      (if (fasta-beg)
          (error "The start of the fasta record is not found!!!"))
      (end-of-line)
      (while (< (point) pos)
        (if (not (looking-at-p seq-cruft-regexp))
            (setq count (1+ count)))
        (forward-char)))
    (message "%d" count)
    count))


(defun fasta-seq-length ()
  "The length of current sequence."
  (interactive)
  (let ()
    (save-excursion
      (fasta-end)
      (fasta-position))))


(defvar fasta-seq-type
  "Return the type of sequence, either protein or nucleic acid."
  (let ((pro-uniq (set-difference pro-aa nuc-base))
        (nuc-uniq (set-difference nuc-base pro-aa))
        pos  current)
    (save-excursion
      (fasta-first)
      (loop do
            (forward-line) ; move to the sequence region
            (setq pos (point))
            (fasta-end)
            (while (> (point) pos)
              (setq current (char-before))
              (cond ((memq current pro-uniq)
                     'pro)
                    ((memq current nuc-uniq)
                     'nuc)
                    (t nil))
              (backward-char))
            while (fasta-next)))))


;;;###autoload
(defun fasta-rc ()
  "Reverse complement current fasta sequence."
  (interactive)
  (fasta-mark)
  (if nuc-mode
      (if (dna-p)
          (dna-reverse-complement)
        (rna-reverse-complement))))


;;; column manipulations
(defun fasta-column-action (snippet)
  "A function called by other column manipulation functions.

SNIPPET is a piece of code that does some specific manipulation
at the current column. See `fasta-insert-column' for an example
of usage."
  (save-excursion
    (let ((original (point))
          (column   (current-column)))
      (fasta-first)
      (loop do
            (forward-line) ; move to the sequence region
            (if (or (< (move-to-column column) column)
                    (equal (char-after) ?\n)     ; if at the end of the line
                    (equal (point) (point-max))) ; or at the end of buffer.
                (error "This sequence at line %d does not have this column!!!"
                       (line-number-at-pos)))
            ;; (delete-char 1)
            (eval snippet)
            while (fasta-next)))))


(defun fasta-delete-column ()
  "Delete current column."
  (interactive)
  (fasta-column-action `(delete-char 1)))


(defun fasta-insert-column (str)
  "Insert a string STR at the column."
  (interactive "sInsert string:")
  (fasta-column-action `(insert ,str)))


(defun fasta-mark-column (&optional to-face)
  "Mark the current column with the face TO-FACE.

If TO-FACE is not a face, mark with highlight face by default.
Thus \\[fasta-mark-column] will mark with highlight face;
and C-u \\[fasta-mark-column] will unmark the column."
  (interactive "p")
  (princ to-face)
  (cond ((equal to-face 1) ; without C-u
         (setq to-face 'highlight))
        ((numberp to-face)
         (setq to-face nil)))
  (fasta-column-action
   `(silent-put-text-property (point) (1+ (point)) 'face to-face)))




(provide 'fasta-mode)


;;; fasta-mode.el ends here
