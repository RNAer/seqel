;;; fasta-mode -- a major mode for editing fasta files

(require 'seq)
(require 'nuc-mode)
(require 'pro-mode)
(require 'genetic-code)

(defvar fasta-mode-hook nil
  "*Hook to setup `fasta-mode'.")

;;;###autoload
(defvar fasta-setup-on-load nil
  "*If not nil, set up fasta mode on buffer load.")

(defvar fasta-mode-map
  ;; use `make-keymap' if there are lots of keybindings
  (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    (define-key map "\C-ca"     'fasta-first)
    (define-key map "\C-cc"     'fasta-count)
    (define-key map "\C-cd"     'fasta-delete)
    (define-key map "\C-ce"     'fasta-last)
    (define-key map "\C-cf"     'fasta-format)
    (define-key map "\C-cl"     'fasta-seq-length)
    (define-key map "\C-cm"     'fasta-mark)
    (define-key map "\C-cp"     'fasta-position)
    (define-key map "\C-cr"     'fasta-rc)
    (define-key map "\C-c\C-d"  'fasta-delete-column)
    (define-key map "\C-c\C-i"  'fasta-insert-column)
    (define-key map "\C-c\C-h"  'fasta-highlight-column)
    (define-key map "\C-c\C-p"  'fasta-paint-column)
    (define-key map "\C-c\C-s"  'fasta-summary-column)
    map)
 "The local keymap for `fasta-mode'")

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.\\(fasta\\|fa\\|fna\\|faa\\)\\'" . fasta-mode))

;; map the paragraph key bindings to corresponding fasta functions
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
     (3 font-lock-comment-face)))
  "Expressions to hilight in `fasta-mode'.")


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

It works in the style of `backward-paragraph'. COUNT need to be positive integer.
Return current point if it moved over COUNT of records; otherwise return nil."
  (interactive "p")
  (if (> count 0)
      (re-search-backward fasta-record-regexp nil 'move-to-point-min count)
    (error "The parameter count should be positive integer.")))

;;;###autoload
(defun fasta-forward (count)
  "Move forward to the end fasta record.

It works in the style of `forward-paragraph'. Count need to be positive integer.
Return current point if it moved over COUNT of records; otherwise return nil."
  (interactive "p")
  (if (looking-at fasta-record-regexp)
      (setq count (1+ count)))
  (if (< count 1)
      (error "The parameter count should be positive integer."))
  (if (re-search-forward fasta-record-regexp nil 'move-to-point-max count)
      (progn (beginning-of-line) (point))
    nil))


;;;###autoload
(defun fasta-last ()
  "Go to the beginning of last fasta record."
  (interactive)
  ;; (while (fasta-forward 1))
  (goto-char (point-max))
  (fasta-backward 1))

;;;###autoload
(defun fasta-first ()
  "Go to the beginning of first fasta record."
  (interactive)
  ;; (while (fasta-backward 1)))
  (goto-char (point-min))
  (or (looking-at fasta-record-regexp)
      (fasta-forward 1)))

;;;###autoload
(defun fasta-count ()
  (interactive)
  (let ((total 0))
    (save-excursion
      (goto-char (point-max))
      (while (fasta-backward 1)
        (setq total (1+ total))))
    (if (called-interactively-p 'interactive)
        (message "Total %d sequences." total))
    total))


(defun fasta-mark (&optional whole)
  "Put point at the beginning of the sequence and mark the end.

If a prefix arg is provided or WHOLE is t, then put the point at
the beginning of the fasta entry instead of the sequence."
  (interactive "P")
  (push-mark) ; mark current position
  (fasta-forward 1)
  (push-mark nil nil t)
  (fasta-backward 1)
  (or whole
      (forward-line)))

(defun fasta--format (width)
  "Format the current sequence to contain WIDTH chars per line.

By default, each sequence is one line (WIDTH is nil). The white spaces inside
will also be removed."
  (and width  (< width 1)
       (error "Width should be nil or positive integer"))
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
          (goto-char beg)
          ;; (message "%d %d %d" width (point) (region-end))
          (forward-char width)
          (while (< (point) end)
            (insert-char ?\n)
            (forward-char width))))))

;;;###autoload
(defun fasta-format (&optional width)
  "Format the current sequence to contain WIDTH chars per line.

It is just a wrapper around `fasta--format'."
  (interactive "P")
  (save-excursion
    (fasta--format width)))


(defun fasta-format-all (&optional width)
  "Format all fasta sequences in the buffer.

It calls `fasta--format' on each fasta records."
  (interactive "P")
  (save-excursion
    (goto-char (point-max))
    (while (fasta-backward 1)
      (fasta--format width)
      (fasta-backward 1))))


(defun fasta-delete ()
  "Delete current fasta entry."
  (interactive)
  (save-excursion
    (fasta-mark t)
    (kill-region (region-beginning) (region-end))))


(defun fasta-position ()
  "Return the position of point in the current sequence.

It will not count white spaces and seq gaps. The count starts
at zero."
  (interactive)
  (if (looking-at fasta-record-regexp)
      (error "Point is not in the sequence region"))
  (let ((pos (point))
        (count 0))
    (save-excursion
      (or (fasta-backward 1)
          (error "The start of the fasta record is not found"))
      (end-of-line)
      (while (< (point) pos)
        (or (looking-at seq-cruft-regexp)
            (setq count (1+ count)))
        (forward-char)))
    (if (called-interactively-p 'interactive)
        (message "Position %d" count))
    count))


(defun fasta-geo-position ()
  "The point position counted from the beginning of the record.

It will count all the characters."
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
      (backward-char)
      (setq length (fasta-position)))
    (if (called-interactively-p 'interactive)
        (message "Length %d" length))
    length))


(defun fasta-seq-type ()
  "Return the type of sequence, either 'pro (protein), 'nuc (nucleic acid) or
nil (undetermined).

It will search all the sequence residues for unique nuc base and unique
protein amino acid IUPAC code. If found, the type is determined. Or if
more than (1000) number or all of them are \"atugc\", then it is determined
to be nuc"
  (interactive)
  (let ((pro-uniq (set-difference pro-aa nuc-base))
        (nuc-uniq (set-difference nuc-base pro-aa))
        (atugc    '(?a ?t ?u ?g ?c ?A ?T ?U ?G ?C))
        (smallest 1000)
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
                       (and (> count smallest)
                            atugc-p
                            (throw 'seq-type 'nuc))
                     (fasta-backward 1)
                     (setq pos (point))))))
      (and atugc-p
           (setq type 'nuc)))
    type))

(defun fasta--rc (is-rna)
  "Reverse complement current fasta sequence if it is nuc sequence.

If IS-RNA is nil, then assume the sequence is RNA; otherwise, DNA."
  (condition-case err
      (progn (fasta-mark)
             (let ((beg (region-beginning))
                   (end (region-end)))
               (if nuc-mode  ; if nuc-mode is enabled
                   (nuc-reverse-complement beg (1- end) is-rna)
                 (error "nuc mode is not enabled"))))
    ('error (primitive-undo 1 buffer-undo-list)
            (error "%s" err))))

;;;###autoload
(defun fasta-rc (is-rna)
  "Reverse complement current fasta sequence if it is nuc sequence.

If IS-RNA is nil, then assume the sequence is RNA; otherwise, DNA.
It is just a wrapper on `fasta--rc'."
  (interactive "P")
  (save-excursion
    (fasta--rc is-rna)))

(defun fasta-rc-all (is-rna)
  "Reverse complement every fasta sequences in the buffer.

It calls `fasta--rc' on each fasta record."
  (interactive "P")
  (save-excursion
    (goto-char (point-max))
    (while (fasta-backward 1)
      (fasta--rc is-rna)
      (fasta-backward 1))))


(defun fasta--translate ()
  "Translate the current fasta sequence to amino acids."
  (fasta-mark)
  (if nuc-mode  ; if nuc-mode is enabled
      (nuc-translate (region-beginning) (region-end))
    (error "nuc mode is not enabled")))

;;;###autoload
(defun fasta-translate ()
  "Translate the current fasta sequence to amino acids.

It is just a wrapper on `fasta--translate'."
  (interactive)
  (save-excursion
    (fasta--translate)))

(defun fasta-translate-all ()
  "Translate the every fasta sequence in the buffer to amino acids.

It calls `fasta--translate' on each fasta record."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (fasta-backward 1)
      (fasta--translate)
      (fasta-backward 1))))


(defun fasta-weight ()
  "Calculate the molecular weight of the current protein entry."
  (interactive)
  (save-excursion
    (fasta-mark)
    (if pro-mode  ; if pro-mode is enabled
        (pro-weight (region-beginning) (region-end))
      (error "pro mode is not enabled"))))



;;; column manipulations
(defmacro fasta--column-action (&rest fn)
  "A function called by other column manipulation functions.

FN is a piece of code that does some specific manipulation
at the current column. See `fasta-insert-column'
and `fasta-mark-column' for an example of usage."
  `(save-excursion
     (condition-case err
         (let ((column (current-column))
               pos)
           (goto-char (point-max))
           (setq pos (point-marker))
           (while (fasta-backward 1)
             (forward-line) ; move to the sequence region
             (if (< (move-to-column column) column)
                 (error "This sequence at line %d does not have enough columns at %d"
                        (line-number-at-pos) column))
             ,@fn
             (or (> pos (point))
                 (error "This sequence at line %d does not have enough columns at %d"
                        (line-number-at-pos) column))
             (fasta-backward 1)
             (setq pos (point-marker))))
       ;; return to the original state if error is met.
       ('error ; the single quote is dispensable
        (primitive-undo 1 buffer-undo-list)
        (error "%s" err)))))


(defun fasta-delete-column (&optional n)
  "Delete current column."
  (interactive "p")
  (fasta--column-action (delete-char n)))


(defun fasta-insert-column (str)
  "Insert a string STR at the column."
  (interactive "sInsert string: ")
  (fasta--column-action (insert str)))


(defun fasta-highlight-column (&optional to-face)
  "Highlight the current column with the face TO-FACE.

If TO-FACE is not a face, mark with highlight face by default.
Thus \\[fasta-highlight-column] will mark with highlight face;
and C-u \\[fasta-highlight-column] will unmark the column."
  (interactive "p")
  ;; (princ to-face)
  (cond ((equal to-face 1) ; without C-u
         (setq to-face 'highlight))
        ((numberp to-face)
         (setq to-face nil)))
  (fasta--column-action
   (silent-put-text-property (point) (1+ (point)) 'font-lock-face to-face)))


(defun fasta-paint-column (&optional case)
  "Paint the current column according their aa or nuc bases.

By default, lower and upper cases are painted in the same colors.
C-u \\[fasta-paint-column] honors the cases"
  (interactive "P")
  (let ((current-char '(char-after)) to-face)
    (or case
        (setq current-char (list 'upcase current-char)))
    (cond (nuc-mode
           (setq to-face (list 'format "base-face-%c" current-char)))
          (pro-mode
           (setq to-face (list 'format "aa-face-%c" current-char)))
          (t
           (error "Unknown seq type")))
    (fasta--column-action
     (silent-put-text-property (point) (1+ (point))
                               'font-lock-face
                               (intern (eval to-face))))))


(defun fasta-summary-column (&optional case)
  "Summary of the column.

If CASE is nil, the summary will be case insensitive."
  (interactive "P")
  (let ((my-hash (make-hash-table :test 'equal))
        count char)
    (fasta--column-action (setq char (char-after))
                          (or case (setq char (upcase char)))
                          (setq count (gethash char my-hash))
                          (if count
                              (puthash char (1+ count) my-hash)
                            (puthash char 1 my-hash)))
    (if (called-interactively-p 'interactive)
        (maphash (lambda (x y) (princ (format "%c:%d " x y))) my-hash)
      my-hash)))

(provide 'fasta-mode)


;;; fasta-mode.el ends here
