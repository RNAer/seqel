;;; fasta-mode -- a major mode for editing fasta files

(require 'seq)

(defvar fasta-mode-hook nil
  "*Hook to setup `fasta-mode'.")

(defvar fasta-setup-on-load nil
  "*If not nil setup dna mode on buffer load.")

(defvar fasta-mode-map
  ;; use `make-keymap' if there are lots of keybindings
  (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    (define-key map "\C-cp"     'fasta-position)
    (define-key map "\C-cp"     'fasta-position)
    map)
 "The local keymap for `fasta-mode'")

(let ((equivs
       '((fasta-forward  . forward-paragraph)
         (fasta-backward . backward-paragraph)
         (fasta-mark     . mark-paragraph))))
  (dolist (x equivs)
    (substitute-key-definition (cdr x)
                               (car x)
                               fasta-mode-map
                               (current-global-map))))


(defvar fasta-font-lock-keywords
  '(("^\\(>\\)\\([-_.|a-zA-Z0-9]+\\)\\(.*\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     (3 font-lock-comment-face))))


(defvar fasta-record-regexp "^>.*$"
  "Fasta label that delimits records.")


;;;###autoload
(define-derived-mode fasta-mode text-mode "fasta"
  "Major mode for editing sequences in fasta format.

￼Special commands:
￼\\{fasta-mode-map}
  \\{nuc-mode-map}
  \\{pro-mode-map}"
  ;; This runs the normal hook change-major-mode-hook, then gets rid of
  ;; the buffer-local variables of the major mode previously in effect.
  ;; (kill-all-local-variables)
  ;; (setq mode-name "fasta")
  ;; (setq major-mode 'fasta-mode)
  ;; (use-local-map fasta-mode-map)
  ;; The above are automatically done if the mode is defined using
  ;; `define-derived-mode'.
  ;; the variable automatically becomes buffer-local when set
  (setq font-lock-defaults '(fasta-font-lock-keywords))
  (flyspell-mode -1)
  ;; (set-syntax-table fasta-mode-syntax-table)
  (run-hooks 'fasta-mode-hook))


(defun fasta-find-file ()
  "Invoke `fasta-mode' if the buffer look like a fasta.
and another mode is not active.
This function is added to `find-file-hooks'."
  (save-excursion
    (goto-char (point-min))
    (if (and (eq major-mode 'fundamental-mode)
             (looking-at fasta-record-regexp))
        (fasta-mode))))


;;;###autoload
(if fasta-setup-on-load
    (add-hook 'find-file-hook 'fasta-find-file))


;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.\\(fasta\\|fa\\|fna\\|faa\\)\\'" . fasta-mode))


;;;###autoload
(defun fasta-backward (count)
  "Move the point the beginning of the fasta record.

It works in the style of `backward-paragraph'. COUNT need to be positive number;
otherwise point will not move. The number of fasta record remain to be moved is
returned."
  (interactive "p")
  (while (and (> count 0)
              (re-search-backward fasta-record-regexp nil t))
    (setq count (1- count)))
  count)

;;;###autoload
(defun fasta-forward (count)
  "Move forward to the end fasta record.

It works in the style of `forward-paragraph'. COUNT need to be positive number;
otherwise point will not move. The number of fasta record remain to be moved is
returned.."
  (interactive "p")
  (and (looking-at fasta-record-regexp)
       (setq count (1+ count)))
  (while (and (> count 0)
              (re-search-forward fasta-record-regexp nil t))
    (setq count (1- count)))
  (if (> count 0)
      (if (/= (point) (point-max))
          (progn (goto-char (point-max))
                 (setq count (1- count))))
    (beginning-of-line))
  count)

;;;###autoload
(defun fasta-last ()
  "Go to the beginning of last fasta record"
  (interactive)
  (while (= (fasta-forward 1) 0))
  (fasta-backward 1))

;;;###autoload
(defun fasta-first ()
  "Go to the beginning of first fasta record"
  (interactive)
  (while (= (fasta-backward 1) 0)))

;;;###autoload
(defun fasta-count ()
  (interactive)
  (let ((total 0))
    (save-excursion
      (goto-char (point-max))
      (while (= (fasta-backward 1) 0)
        (setq total (1+ total))))
    (if (called-interactively-p 'interactive)
        (message "Total %d sequences" total))
    total))


(defun fasta-mark (&optional whole)
  "Put point at the beginning of the sequence and mark the end.

If a prefix arg is provided or WHOLE is t, then put the point at
the beginning of the fasta entry instead of the sequence."
  (interactive "P")
  (fasta-forward 1)
  (push-mark)
  (fasta-backward 1)
  (or whole
      (forward-line)))


;;;###autoload
(defun fasta-format (&optional width)
  "Format the current sequence to contain WIDTH chars per line.

By default, each sequence is one line if WIDTH is nil. The white spaces inside
will also be removed."
  (interactive "P")

  (save-excursion
    (let (beg end)
      (fasta-forward 1)
      (backward-char)
      (setq end (point-marker))
      (fasta-backward 1)
      (setq beg (line-beginning-position 2)) ; the beg of next line
      ;; remove seq-spaces
      (goto-char beg)
      (while (re-search-forward seq-space-regexp end t)
        ;; (setq count (1+ count))
        (replace-match "" nil nil))
      ;; insert newlines
      (if width
          (progn
            (and (< width 0)
                 (error "Width cannot be a negative number"))
            (goto-char (- end width))
            (setq end (point-marker))
            (goto-char beg)
            ;; (message "%d %d %d" width (point) (region-end))
            (while (< (point) end)
              (forward-char width)
              (insert-char ?\n)))))))


(defun fasta-delete-next ()
  (interactive)
  (save-excursion
    (if (fasta-forward 1)
        (delete-region (point) (or (> (fasta-forward 1) 0)
                                   (point-max)))
      (message "This is the last sequence"))))


(defun fasta-position ()
  "Return the position of point in the current sequence.

It will not count white spaces and seq gaps. The count starts
at zero."
  (interactive)
  (if (looking-at fasta-record-regexp)
      (error "Point is not in the sequence region"))
  (let ((pos   (point))
        (count 0))
    (save-excursion
      (if (> (fasta-backward 1) 0)
          (error "The start of the fasta record is not found"))
      (end-of-line)
      (while (< (point) pos)
        (if (not (looking-at seq-cruft-regexp))
            (setq count (1+ count)))
        (forward-char)))
    (if (called-interactively-p 'interactive)
        (message "Position %d" count))
    count))


(defun fasta-relative-position ()
  "The point position counted from the beginning of the record."
  (interactive)
  (if (looking-at fasta-record-regexp)
      (error "Point is not in the sequence region"))
  (let ((pos (point)))
    (save-excursion
      (fasta-backward 1)
      (forward-line)
      (if (called-interactively-p 'interactive)
          (message "Position %d." (- pos (point)))
        (- pos (point))))))


(defun fasta-seq-length ()
  "Return the length of current sequence."
  (interactive)
  (let (length)
    (save-excursion
      (fasta-forward 1)
      (setq length (fasta-position)))
    (if (called-interactively-p 'interactive)
        (message "Length %d" length))
    length))


(defun fasta-seq-type ()
  "Return the type of sequence, either protein or nucleic acid."
  (interactive)
  (let ((pro-uniq (set-difference pro-aa nuc-base))
        (nuc-uniq (set-difference nuc-base pro-aa))
        (atugc    '(?a ?t ?u ?g ?c ?A ?T ?U ?G ?C))
        (smallest 100000)
        (count    0)
        (atugc-p  t)
        pos  current type)
    (save-excursion
      (goto-char (point-max))
      (setq pos (point))
      (setq type (catch 'seq-type
                   (while (> (fasta-backward 1) 0)
                     ;; (message "%d" (line-number-at-pos))
                     (forward-line) ; move to the sequence region
                     (while (< (point) pos)
                       (setq current (char-after))
                       (cond ((memq current pro-uniq)
                              (throw 'seq-type 'pro))
                             ((memq current nuc-uniq)
                              (throw 'seq-type 'nuc)))
                       (or (not atugc-p)
                           (memq current seq-gap)
                           (memq current seq-space)
                           (if (memq current atugc) (setq count (1+ count)))
                           (setq atugc-p nil))
                       (forward-char)
                       ;; (message "%c %S" current atugc-p)
                       (if (or (> count smallest) (/= 1 (fasta-backward 1)))
                           (if atugc-p
                               (throw 'seq-type 'nuc)
                             (throw 'seq-type 'pro))))
                     (fasta-backward 1)
                     (setq pos (point)))))
    type)))


;;;###autoload
(defun fasta-rc (is-rna)
  "Reverse complement current fasta sequence if it is nuc sequence."
  (interactive "P")
  (princ is-rna)
  (save-excursion
    (fasta-mark)
    (let ((beg (region-beginning))
          (end (region-end)))
      (if nuc-mode
          (nuc-reverse-complement beg end is-rna)))))



;;; column manipulations
(defun fasta--column-action (snippet &optional n-p)
  "A function called by other column manipulation functions.

SNIPPET is a piece of code that does some specific manipulation
at the current column. N-P is a boolean to indicate whether the
column is allowed at the end of line. See `fasta-insert-column'
and `fasta-mark-column' for an example of usage."
  (save-excursion
    (condition-case ex
        (let ((column   (current-column)))
          (fasta-first)
          (loop do
                (forward-line) ; move to the sequence region
                (if (or (< (move-to-column column) column)
                        (and n-p (eolp)))
                    (error "This sequence at line %d does not have this column"
                           (line-number-at-pos)))
                (eval snippet)
                while (and (= (fasta-forward 1) 0) (not (eobp)))))
      ;; return to the original state if error is met.
      ('error
       (primitive-undo 1 buffer-undo-list)
       (eval ex)))))


(defun fasta-delete-column ()
  "Delete current column."
  (interactive)
  (fasta--column-action `(delete-char 1) t))


(defun fasta-insert-column (str)
  "Insert a string STR at the column."
  (interactive "sInsert string:")
  (fasta--column-action `(insert ,str)))


(defun fasta-mark-column (&optional to-face)
  "Mark the current column with the face TO-FACE.

If TO-FACE is not a face, mark with highlight face by default.
Thus \\[fasta-mark-column] will mark with highlight face;
and C-u \\[fasta-mark-column] will unmark the column."
  (interactive "p")
  ;; (princ to-face)
  (cond ((equal to-face 1) ; without C-u
         (setq to-face 'highlight))
        ((numberp to-face)
         (setq to-face nil)))
  (fasta--column-action
   `(silent-put-text-property (point) (1+ (point)) 'face to-face) t))


(defun fasta-2-stockholm ()
  ""
  (interactive))

(defun fasta-align ()
  ""
  (interactive))

(provide 'fasta-mode)


;;; fasta-mode.el ends here
