;;; fasta-mode -- a major mode for editing fasta files  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Zech Xu

;; Author: Zech Xu
;; Keywords: DNA, RNA, protein
;; License: BSD-3

;;; Commentary:

;;

;;; Code:


(require 'bioseq)
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
    (define-key map "\C-cc"     'fasta-count)
    (define-key map "\C-cd"     'fasta-delete)
    (define-key map "\C-ca"     'fasta-first)
    (define-key map "\C-cz"     'fasta-last)
    (define-key map "\C-cf"     'fasta-forward)
    (define-key map "\C-cb"     'fasta-backward)
    (define-key map "\C-cl"     'fasta-length)
    (define-key map "\C-cm"     'fasta-mark)
    (define-key map "\C-cp"     'fasta-position)
    (define-key map "\C-cw"     'fasta-weight)
    (define-key map "\C-cr"     'fasta-rc-all)
    (define-key map "\C-ct"     'fasta-translate-all)
    (define-key map "\C-c\C-d"  'fasta-column-delete)
    (define-key map "\C-c\C-i"  'fasta-column-insert)
    (define-key map "\C-c\C-h"  'fasta-column-highlight)
    (define-key map "\C-c\C-p"  'fasta-column-paint)
    (define-key map "\C-c\C-s"  'fasta-column-summary)
    map)
  "The local keymap for `fasta-mode'")

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.\\(fasta\\|fa\\|fna\\|faa\\|aln\\)\\'" . fasta-mode))

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
  "Major mode for editing biological sequences in fasta format.

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
  (setq font-lock-defaults '(fasta-font-lock-keywords))
  ;; (set-syntax-table fasta-mode-syntax-table)
  (run-hooks 'fasta-mode-hook))


(defvar fasta-setup-on-load t
  "If not nil, set up fasta mode on buffer load by guessing buffer content.")

(defun fasta-find-file ()
  "Invoke `fasta-mode' if the buffer look like a fasta.

Only if the major mode is `fundermental'. This function is added to
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
  "Move the point to the beginning of the fasta record.

It works in the style of `backward-paragraph'. COUNT need to be positive integer.
Return current point if it moved over COUNT of records; otherwise return nil."
  (interactive "p")
  (entry-backward count fasta-record-regexp))

;;;###autoload
(defun fasta-forward (count)
  "Move forward to the end fasta record.

It works in the style of `forward-paragraph'. Count need to be positive integer.
Return current point if it moved over COUNT of records; otherwise return nil."
  (interactive "p")
  (entry-forward count fasta-record-regexp))


;;;###autoload
(defun fasta-last ()
  "Go to the beginning of last fasta record."
  (interactive)
  (entry-last fasta-record-regexp))

;;;###autoload
(defun fasta-first ()
  "Go to the beginning of first fasta record."
  (interactive)
  (entry-first fasta-record-regexp))

;;;###autoload
(defun fasta-count ()
  "Count the number of fasta sequences in the buffer."
  (interactive)
  (entry-count fasta-record-regexp))


;;;###autoload
(defun fasta-mark (&optional include-header)
  "Put point at the beginning of the sequence and mark the end.

If a prefix arg is provided or INCLUDE-HEADER is t, then put the point at
the beginning of the fasta entry instead of the sequence."
  (interactive "P")
  (if (fasta-forward 1)
      (backward-char))
  (push-mark nil nil t)
  (fasta-backward 1)
  (or include-header
      (forward-line)))

(defun fasta--format (width)
  "Format the current sequence to contain WIDTH chars per line.

By default, each sequence is one line (if WIDTH is nil). The
white spaces will all be removed."
  (and width  (< width 1)
       (error "Width should be nil or positive integer"))
  (save-restriction
    (fasta-mark)
    (message "Formating the sequence...")
    (narrow-to-region (point) (mark))
    ;; remove bioseq-spaces
    (while (re-search-forward "\\s-+" nil t)
      ;; (setq count (1+ count))
      (replace-match "" nil nil))
    (if width
        ;; insert newlines
        (progn (goto-char (point-min))
               ;; (message "%d %d %d" width (point) (region-end))
               (dotimes (i (/ (- (point-max) (point-min)) width))
                 (forward-char width)
                 (insert-char ?\n))))))

;;;###autoload
(defun fasta-format (&optional width)
  "Format the current sequence to contain WIDTH chars per line.

It is just a wrapper around `fasta--format'. This can take >10
seconds for long sequences (> 5 M base pairs)."
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
  (fasta-mark 'include-header)
  (delete-region (region-beginning) (region-end)))


(defun fasta-position ()
  "Return the position of point in the current sequence.

It will not count white spaces and sequence gaps. See also
`fasta-position-ali'."
  (interactive)
  (if (looking-at fasta-record-regexp)
      (error "Point is not in the sequence region"))
  (let ((pos (point))
        (count 0))
    (save-excursion
      (or (fasta-backward 1)
          (error "The start of the fasta record is not found"))
      (forward-line 1)
      (dotimes (i (- pos (point)))
        (or (gethash (char-after) bioseq-cruft-set)
            (setq count (1+ count)))
        (forward-char)))
    (if (called-interactively-p 'interactive)
        (message "Position %d" count))
    count))


(defun fasta-length ()
  "Return the length of current sequence."
  (interactive)
  (let (length)
    (save-excursion
      (fasta-forward 1)
      (backward-char)
      (setq length (fasta-position)))
    (if (called-interactively-p 'interactive)
        (message "Sequence length %d" length))
    length))


(defun fasta--rc ()
  "Reverse complement current DNA/RNA sequence."
  (condition-case err
      (progn (fasta-mark)
             (let ((beg (region-beginning))
                   (end (region-end)))
               (if nuc-mode  ; if nuc-mode is enabled
                   ;; (print (buffer-substring beg end))
                   (nuc-reverse-complement beg end)
                 (error "nuc mode is not enabled"))))
    ((debug error)
     (primitive-undo 1 buffer-undo-list)
     ;; get the original error message
     (error "%s" (error-message-string err)))))

;;;###autoload
(defun fasta-rc ()
  "Reverse complement current DNA/RNA sequence.

It is just a wrapper on `fasta--rc'."
  (interactive)
  (save-excursion
    (fasta--rc))
  (message "Reverse complemented the current sequence."))

(defun fasta-rc-all ()
  "Reverse complement every DNA/RNA sequence in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (fasta-backward 1)
      (fasta--rc)
      (fasta-backward 1)))
  (message "Reverse complemented all the sequences in the buffer."))


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
  "Translate every DNA/RNA sequence in the buffer to amino acids.

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
      (error "pro mode is not enabled. `fasta-weight' is only for protein sequence"))))


(defun fasta-summary ()
  "Print the frequencies of characters in the fasta sequence.

See also `bioseq-summary', `nuc-summary', `pro-summary'."
  (interactive)
  (save-excursion
    (fasta-mark)
    (if pro-mode
        (pro-summary (point) (mark))
      (nuc-summary (point) (mark)))))

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

It calls `bioseq-unpaint'."
  (interactive)
  (save-excursion
    (fasta-mark)
    (bioseq-unpaint (region-beginning) (region-end))))

(defun fasta-paint (&optional case)
  "Paint current sequence.

It is just a wrapper around `fasta--paint'."
  (interactive "P")
  (save-excursion
    (fasta--paint case)))

(defun fasta-unpaint-all ()
  "Unpaint all the sequences.

It calls `bioseq-unpaint' on each fasta record."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (fasta-backward 1)
      (fasta-mark)
      (bioseq-unpaint (region-beginning) (region-end))
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
           pos line)
       (condition-case err
           (progn (goto-char (point-max))
                  (while (fasta-backward 1)
                    (forward-line) ; move to the sequence region
                    (setq line (line-number-at-pos))
                    (if (< (move-to-column column) column)
                        (signal 'end-of-col-err '(move-to-column)))
                    ,@fn
                    (fasta-backward 1)))
       ;; return to the original state if error is met.
       (end-of-col-err ; the single quote is dispensable
        (primitive-undo 1 buffer-undo-list)
        (error "Abort: line %d is shorter than the column number (%d)."
               line column))))))


(defun fasta-column-delete (&optional n)
  "Delete current column."
  (interactive "p")
  (fasta--column-action (delete-char n)))


(defun fasta-column-insert (str)
  "Insert a string STR at the column."
  (interactive "sInsert string: ")
  (fasta--column-action (insert str)))


(defun fasta-column-highlight (&optional to-face)
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


(defun fasta-column-paint (&optional case)
  "Paint the current column according their aa or nuc bases.

By default, lower and upper cases are painted in the same colors.
C-u \\[fasta-paint-column] honors the cases"
  (interactive "P")
  (let ((current-char '(char-after)) to-face)
    (or case
        (setq current-char (list 'upcase current-char)))
    (cond (nuc-mode
           (setq to-face (list 'format "nuc-base-face-%c" current-char)))
          (pro-mode
           (setq to-face (list 'format "pro-aa-face-%c" current-char)))
          (t
           (error "Unknown sequence type. Please specify protein or nucletide")))
    (fasta--column-action
     (with-silent-modifications
       (put-text-property (point) (1+ (point))
                          'font-lock-face
                          (intern (eval to-face)))))))


(defun fasta-column-summary (&optional case)
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


(defun fasta-bioseq-type (&optional threshold)
  "Enable the minor mode for either protein or nucleic acid.

It will search the first 100 sequence residues (or the current
sequence record, whichever is smaller) for unique nucletide base
and unique protein amino acid IUPAC code. If found, the minor
mode of `nuc-mode' or `pro-mode' will be enabled. If it is
ambiguous, enable `nuc-mode' by default."
  (let ((pro-aa-uniq '(?E ?F ?I ?J ?L ?P ?Q ?Z ?e ?f ?i ?j ?l ?p ?q ?z))
        current)
    (save-mark-and-excursion
        (catch 'bioseq-type
          (fasta-mark)
          ;; 100 sequence residues or the current sequence length
          (dotimes (i (min (or threshold 100) (- (region-end) (region-beginning))))
            (setq current (char-after))
            (cond ((memq current pro-aa-uniq)
                   (pro-mode)
                   (throw 'bioseq-type 'pro))
                  ((= ?U (upcase current))
                   (nuc-mode)
                   (throw 'bioseq-type 'nuc)))
            (forward-char))
        ;; if no uniq char found for pro or nuc sequences; enable nuc-mode by default
        (nuc-mode)
        (throw 'bioseq-type 'nuc)))))


(add-hook 'fasta-mode-hook
          (lambda ()
            (fasta-bioseq-type)
            ;; this can slow the loading of a large fasta file
            ;; disable these minor modes
            (flyspell-mode -1)
            (linum-mode -1)))


(provide 'fasta-mode)


;;; fasta-mode.el ends here
