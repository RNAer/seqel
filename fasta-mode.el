;;; fasta-mode -- a major mode for editing fasta files

;;; license: GPLv3

;;; Author: Zech Xu <zhenjiang dot xu at gmail dot com>


(require 'seq)
(require 'nuc-mode)
(require 'pro-mode)
(require 'genetic-code)



(defvar fasta-mode-hook nil
  "*Hook to setup `fasta-mode'.")


(defvar fasta-mode-map
  ;; use `make-keymap' instead of `make-sparse-keymap'
  ;; if there are lots of keybindings
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

;; this slows down the mode loading
(defvar fasta-font-lock-keywords
  '(("^\\(>\\)\\([-_.|a-zA-Z0-9]+\\)\\(.*\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     (3 font-lock-comment-face)))
  "Expressions to highlight in `fasta-mode'.")


(defvar fasta-record-regexp "^>.*$"
  "Fasta label that delimits records.")


;;;###autoload
(define-derived-mode fasta-mode text-mode "fasta"
  "Major mode for editing sequences in fasta format.

Special commands:
\\{fasta-mode-map}
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
  ;; (setq font-lock-defaults '(fasta-font-lock-keywords))
  (flyspell-mode -1)
  ;; (set-syntax-table fasta-mode-syntax-table)
  (run-hooks 'fasta-mode-hook))


(defvar fasta-setup-on-load t
  "If not nil, set up fasta mode on buffer load by guessing buffer content.")

(defun fasta-find-file ()
  "Invoke `fasta-mode' if the buffer look like a fasta.

Only if the major mode is `fundermental. This function is added to
`find-file-hooks'."
  (save-excursion
    (goto-char (point-min))
    (if (and (eq major-mode 'fundamental-mode)
             (looking-at fasta-record-regexp))
        (fasta-mode))))

;;;###autoload
(defun fasta-guess-on-load ()
  "Whether to enable `fasta-mode' by guessing buffer content."
  (interactive)
  (if fasta-setup-on-load
      (progn (remove-hook 'find-file-hook 'fasta-find-file)
             (setq fasta-setup-on-load nil)
             (message "Turned off fasta format guessing on load"))
    (progn (add-hook 'find-file-hook 'fasta-find-file)
           (setq fasta-setup-on-load t)
           (message "Turned on fasta format guessing on load"))))


;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.\\(fasta\\|fa\\|fna\\|faa\\)\\'" . fasta-mode))



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
  "Count the number of fasta records in the buffer."
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
  (fasta-mark t)
  (delete-region (region-beginning) (region-end)))


(defun fasta-position ()
  "Return the position of point in the current sequence.

It will not count white spaces and seq gaps. The count starts
at zero. See also `fasta-position-ali'."
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


(defun fasta-position-ali ()
  "Return position counted from the beginning of the current sequence.

The difference from `fasta-position' is that this function will
count all the characters."
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
  (let ((pro-uniq pro-aa)
        (nuc-uniq nuc-base)
        (atugc    '(?a ?t ?u ?g ?c ?A ?T ?U ?G ?C))
        (smallest 1000)
        (count    0)
        (atugc-only-p  t)
        current)
    (mapc (lambda (x) (setq pro-uniq (remq x pro-uniq))) nuc-base)
    (mapc (lambda (x) (setq nuc-uniq (remq x nuc-uniq))) pro-aa)
    (save-excursion
      (goto-char (point-min))
      (catch 'seq-type
        (while (setq current (char-after))
          ;; (message "%d" (line-number-at-pos))
          ;; move from ">" line to the sequence region
          (if (looking-at fasta-record-regexp) (forward-line))
          (cond ((memq current pro-uniq)
                 (pro-mode)
                 (throw 'seq-type 'pro))
                ((memq current nuc-uniq)
                 (nuc-mode)
                 (throw 'seq-type 'nuc)))

          (or (not atugc-only-p)
              (memq current seq-gap)
              (memq current seq-space)
              (if (memq current atugc) (setq count (1+ count)))
              (setq atugc-only-p nil))

          ;; have enough AUGCTs to call it a nuc sequence
          (and (> count smallest)
               atugc-only-p
               (nuc-mode)
               (throw 'seq-type 'nuc))

          (forward-char))
        ;; went thru all the sequences
        (and atugc-only-p
             (nuc-mode)
             (throw 'seq-type 'nuc))))))


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
    ((debug error)
     (primitive-undo 1 buffer-undo-list)
     ;; get the original error message
     (error "%s" (error-message-string err)))))

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
  (condition-case err
      (progn (fasta-mark)
             (if nuc-mode  ; if nuc-mode is enabled
                 (nuc-translate (region-beginning) (region-end))
               (error "nuc mode is not enabled")))
    ((debug error)
     (primitive-undo 1 buffer-undo-list)
     ;; get the original error message
     (error "%s" (error-message-string err)))))

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


(defun fasta--paint (&optional case)
  "Paint current fasta sequence by their residue identity.

By default, lower and upper cases are painted in the same colors.
C-u \\[fasta-paint] honors the cases."
  (condition-case err
      (progn (fasta-mark)
             (cond (nuc-mode
                    (nuc-paint (region-beginning) (region-end) case))
                   (pro-mode
                    (pro-paint (region-beginning) (region-end) case))
                   (t
                    (error "Unknown seq type"))))
    ((debug error)
     (primitive-undo 1 buffer-undo-list)
     ;; get the original error message
     (error "%s" (error-message-string err)))))

(defun fasta-unpaint ()
  "Unpaint current sequence.

It calls `seq-unpaint'."
  (interactive)
  (save-excursion
    (fasta-mark)
    (seq-unpaint (region-beginning) (region-end))))

(defun fasta-paint (&optional case)
  "Paint current sequence.

It is just a wrapper around `fasta--paint'."
  (interactive "P")
  (save-excursion
    (fasta--paint case)))

(defun fasta-unpaint-all ()
  "Unpaint all the sequences.

It calls `seq-unpaint' on each fasta record."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (fasta-backward 1)
      (fasta-mark)
      (seq-unpaint (region-beginning) (region-end))
      (fasta-backward 1))))

(defun fasta-paint-all (&optional case)
  "Paint all sequences.

It calls `fasta--paint' on each fasta record."
  (interactive "P")
  (save-excursion
    (goto-char (point-max))
    (while (fasta-backward 1)
      (fasta--paint case)
      (fasta-backward 1))))

;;; column manipulations
(defmacro fasta--column-action (&rest fn)
  "A macro called by other column manipulation functions.

FN is a piece of code that does some specific manipulation
at the current column of all fasta records. See `fasta-insert-column'
and `fasta-delete-column' for an example of usage."
  `(save-excursion
     (let ((column (current-column))
           pos ln)
       (condition-case err
           (progn (goto-char (point-max))
                  (setq pos (point-marker))
                  (while (fasta-backward 1)
                    (forward-line) ; move to the sequence region
                    (setq ln (line-number-at-pos))
                    (if (< (move-to-column column) column)
                        (signal 'end-of-col-err '(move-to-column)))
                    ,@fn
                    (or (> pos (point))
                        (signal 'end-of-col-err '((> pos (point)))))
                    (fasta-backward 1)
                    (setq pos (point-marker))))
       ;; return to the original state if error is met.
       (end-of-col-err ; the single quote is dispensable
        (primitive-undo 1 buffer-undo-list)
        (error "%s Line %d does not have enough columns at %d."
                 (error-message-string err)
                 ln column))))))


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
   (put-text-property (point) (1+ (point)) 'font-lock-face to-face)))


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
     (put-text-property (point) (1+ (point))
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


;; this can slow the loading of a large fasta file
(add-hook 'fasta-mode-hook 'fasta-seq-type)


(provide 'fasta-mode)


;;; fasta-mode.el ends here
