;;; seqel-fasta-mode.el --- A major mode for editing fasta files.

;; Copyright (C) 2021  Zech Xu

;; Author: Zech Xu
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; License: BSD-3
;; URL: https://github.com/RNAer/seqel

;;; Commentary:

;; Major mode to edit fasta files

;;; Code:


(require 'seqel)
(require 'seqel-nuc-mode)
(require 'seqel-pro-mode)


(defvar seqel-fasta-mode-hook nil
  "*Hook to setup `seqel-fasta-mode'.")


(defvar seqel-fasta-mode-map
  ;; use `make-keymap' instead of `make-sparse-keymap'
  ;; if there are lots of keybindings
  (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    (define-key map "\C-c\C-c"   'seqel-fasta-count)
    (define-key map "\C-c\C-d"   'seqel-fasta-delete)
    (define-key map "\C-c\C-a"   'seqel-fasta-first)
    (define-key map "\C-c\C-z"   'seqel-fasta-last)
    (define-key map "\C-c\C-f"   'seqel-fasta-forward) ; it also binds to M-}
    (define-key map "\C-c\C-b"   'seqel-fasta-backward); it also binds to M-{
    (define-key map "\C-c\C-m"   'seqel-fasta-mark)    ; it also binds to M-h
    (define-key map "\C-c\C-l"   'seqel-fasta-length)
    (define-key map "\C-c\C-p"   'seqel-fasta-position)
    (define-key map "\C-c\C-w"   'seqel-fasta-weight)
    (define-key map "\C-c\C-r"   'seqel-fasta-rc-all)
    (define-key map "\C-c\C-t"   'seqel-fasta-translate-all)
    (define-key map "\C-c\C-vd"  'seqel-fasta-column-delete)
    (define-key map "\C-c\C-vi"  'seqel-fasta-column-insert)
    (define-key map "\C-c\C-vh"  'seqel-fasta-column-highlight)
    (define-key map "\C-c\C-vp"  'seqel-fasta-column-paint)
    (define-key map "\C-c\C-vs"  'seqel-fasta-column-summary)
    map)
  "The local keymap for `seqel-fasta-mode'.")

;; map the paragraph key bindings to corresponding fasta functions
(let ((equivs
       '((seqel-fasta-forward  . forward-paragraph)
         (seqel-fasta-backward . backward-paragraph)
         (seqel-fasta-mark     . mark-paragraph))))
  (dolist (x equivs)
    (substitute-key-definition (cdr x)
                               (car x)
                               seqel-fasta-mode-map
                               (current-global-map))))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.\\(fasta\\|fa\\|fna\\|faa\\|aln\\)\\'" . seqel-fasta-mode))

;; this slows down the mode loading
(defvar seqel-fasta-font-lock-keywords
  '(("^\\(>\\)\\([-_.|a-zA-Z0-9]+\\)\\(.*\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     (3 font-lock-comment-face)))
  "Expressions to highlight in `seqel-fasta-mode'.")


(defvar seqel-fasta-record-regexp "^>.*$"
  "Fasta label that delimits records.")


;;;###autoload
(define-derived-mode seqel-fasta-mode text-mode "fasta"
  "Major mode for editing biological sequences in fasta format.

Special commands:
\\{seqel-fasta-mode-map}
\\{seqel-nuc-mode-map}
\\{seqel-pro-mode-map}"
  ;; This runs the normal hook change-major-mode-hook, then gets rid of
  ;; the buffer-local variables of the major mode previously in effect.
  ;; (kill-all-local-variables)
  ;; (setq mode-name "fasta")
  ;; (setq major-mode 'seqel-fasta-mode)
  ;; (use-local-map seqel-fasta-mode-map)
  ;; The above are automatically done if the mode is defined using
  ;; `define-derived-mode'.
  ;; the variable automatically becomes buffer-local when set
  (setq font-lock-defaults '(seqel-fasta-font-lock-keywords))
  ;; (set-syntax-table fasta-mode-syntax-table)
  (run-hooks 'seqel-fasta-mode-hook))


(defvar seqel-fasta-setup-on-load t
  "If not nil, set up fasta mode on buffer load by guessing buffer content.")

(defun seqel-fasta-find-file ()
  "Invoke `seqel-fasta-mode' if the buffer look like a fasta.

Only if the major mode is `fundermental'.  This function is added to
`find-file-hooks'."
  (save-excursion
    (goto-char (point-min))
    (if (and (eq major-mode 'fundamental-mode)
             (looking-at seqel-fasta-record-regexp))
        (seqel-fasta-mode))))

;;;###autoload
(defun seqel-fasta-guess-on-load ()
  "Whether to enable `seqel-fasta-mode' by guessing buffer content."
  (interactive)
  (if seqel-fasta-setup-on-load
      (progn (remove-hook 'find-file-hook 'seqel-fasta-find-file)
             (setq seqel-fasta-setup-on-load nil)
             (message "Turned off fasta format guessing on load"))
    (progn (add-hook 'find-file-hook 'seqel-fasta-find-file)
           (setq seqel-fasta-setup-on-load t)
           (message "Turned on fasta format guessing on load"))))


(defun seqel-fasta-backward (count)
  "Move the point to the beginning of the fasta record.

It works in the style of `backward-paragraph'.  COUNT need to be
positive integer.  Return current point if it moved over COUNT of
records; otherwise return nil."
  (interactive "p")
  (seqel-entry-backward count seqel-fasta-record-regexp))

;;;###autoload
(defun seqel-fasta-forward (count)
  "Move forward to the end fasta record.

It works in the style of `forward-paragraph'.  Count need to be
positive integer.  Return current point if it moved over COUNT of
records; otherwise return nil."
  (interactive "p")
  (seqel-entry-forward count seqel-fasta-record-regexp))


;;;###autoload
(defun seqel-fasta-last ()
  "Go to the beginning of last fasta record."
  (interactive)
  (seqel-entry-last seqel-fasta-record-regexp))

;;;###autoload
(defun seqel-fasta-first ()
  "Go to the beginning of first fasta record."
  (interactive)
  (seqel-entry-first seqel-fasta-record-regexp))

;;;###autoload
(defun seqel-fasta-count ()
  "Count the number of fasta sequences in the buffer."
  (interactive)
  (seqel-entry-count seqel-fasta-record-regexp))


;;;###autoload
(defun seqel-fasta-mark (&optional include-header)
  "Put point at the beginning of the sequence and mark the end.

If a prefix arg is provided or INCLUDE-HEADER is t, then put the point at
the beginning of the fasta entry instead of the sequence."
  (interactive "P")
  (if (seqel-fasta-forward 1)
      (backward-char))
  (push-mark nil nil t)
  (seqel-fasta-backward 1)
  (or include-header
      (forward-line)))

(defun seqel-fasta--format (width)
  "Format the current sequence to contain WIDTH chars per line.

By default, each sequence is one line (if WIDTH is nil).  The
white spaces will all be removed."
  (and width  (< width 1)
       (error "Width should be nil or positive integer"))
  (save-restriction
    (seqel-fasta-mark)
    (message "Formating the sequence...")
    (narrow-to-region (point) (mark))
    ;; remove seqel-spaces
    (while (re-search-forward "\\s-+" nil t)
      ;; (setq count (1+ count))
      (replace-match "" nil nil))
    (if width
        ;; insert newlines
        (progn (goto-char (point-min))
               ;; (message "%d %d %d" width (point) (region-end))
               (dotimes (i (/ (- (point-max) (point-min)) width))
                 (forward-char width)
                 (insert-char ?\n)))))
  (message "done"))

;;;###autoload
(defun seqel-fasta-format (&optional width)
  "Format the current sequence to contain WIDTH chars per line.

It wraps around `seqel-fasta--format'.  This can take ~10
seconds for long sequences (> 5M base pairs).  If WIDTH is
nil, each fasta sequence will be formatted into a single line."
  (interactive "P")
  (save-excursion
    (seqel-fasta--format width)))


(defun seqel-fasta-format-all (&optional width)
  "Format all fasta sequences in the buffer.

It calls `seqel-fasta--format' on each fasta records.
Optional argument WIDTH is to set a row width; if nil,
each fasta sequence will be formatted into a single line."
  (interactive "P")
  (save-excursion
    (goto-char (point-max))
    (while (seqel-fasta-backward 1)
      (seqel-fasta--format width)
      (seqel-fasta-backward 1))))


(defun seqel-fasta-delete ()
  "Delete current fasta entry."
  (interactive)
  (seqel-fasta-mark 'include-header)
  (delete-region (region-beginning) (region-end)))


(defun seqel-fasta-position ()
  "Return the position of point in the current sequence.

It will not count white spaces and sequence gaps."
  (interactive)
  (if (looking-at seqel-fasta-record-regexp)
      (error "Point is not in the sequence region!"))
  (let ((pos (point))
        (count 0))
    (save-excursion
      (or (seqel-fasta-backward 1)
          (error "The start of the fasta record is not found!"))
      (forward-line 1)
      (dotimes (i (- pos (point)))
        (or (gethash (char-after) seqel-cruft-set)
            (setq count (1+ count)))
        (forward-char)))
    (if (called-interactively-p 'interactive)
        (message "Position %d" count))
    count))


(defun seqel-fasta-length ()
  "Return the length of current sequence."
  (interactive)
  (let (length)
    (save-excursion
      (seqel-fasta-forward 1)
      (backward-char)
      (setq length (seqel-fasta-position)))
    (if (called-interactively-p 'interactive)
        (message "Sequence length %d" length))
    length))


(defun seqel-fasta--rc ()
  "Reverse complement current DNA/RNA sequence."
  (condition-case err
      (progn (seqel-fasta-mark)
             (let ((beg (region-beginning))
                   (end (region-end)))
               (if seqel-nuc-mode  ; if seqel-nuc-mode is enabled
                   ;; (print (buffer-substring beg end))
                   (seqel-nuc-reverse-complement beg end)
                 (error "The nuc mode is not enabled"))))
    ((debug error)
     (primitive-undo 1 buffer-undo-list)
     ;; get the original error message
     (error "%s" (error-message-string err)))))

;;;###autoload
(defun seqel-fasta-rc ()
  "Reverse complement current DNA/RNA sequence.

It wraps on `seqel-fasta--rc'."
  (interactive)
  (save-excursion
    (seqel-fasta--rc))
  (message "Reverse complemented the current sequence."))

(defun seqel-fasta-rc-all ()
  "Reverse complement every DNA/RNA sequence in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (seqel-fasta-backward 1)
      (seqel-fasta--rc)
      (seqel-fasta-backward 1)))
  (message "Reverse complemented all the sequences in the buffer."))


(defun seqel-fasta--translate ()
  "Translate the current fasta sequence to amino acids."
  (condition-case err
      (progn (seqel-fasta-mark)
             (if seqel-nuc-mode  ; if seqel-nuc-mode is enabled
                 (seqel-nuc-translate (region-beginning) (region-end))
               (error "The nuc mode is not enabled")))
    ((debug error)
     (primitive-undo 1 buffer-undo-list)
     ;; get the original error message
     (error "%s" (error-message-string err)))))

;;;###autoload
(defun seqel-fasta-translate ()
  "Translate the current fasta sequence to amino acids.

It is just a wrapper on `seqel-fasta--translate'."
  (interactive)
  (save-excursion
    (seqel-fasta--translate)))

(defun seqel-fasta-translate-all ()
  "Translate every DNA/RNA sequence in the buffer to amino acids.

It calls `seqel-fasta--translate' on each fasta record."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (seqel-fasta-backward 1)
      (seqel-fasta--translate)
      (seqel-fasta-backward 1))))


(defun seqel-fasta-weight ()
  "Calculate the molecular weight of the current protein entry."
  (interactive)
  (save-excursion
    (seqel-fasta-mark)
    (if seqel-pro-mode  ; if seqel-pro-mode is enabled
        (seqel-pro-weight (region-beginning) (region-end))
      (error "The pro mode is not enabled.  `seqel-fasta-weight' is only for protein sequence"))))


(defun seqel-fasta-summary ()
  "Print the frequencies of characters in the fasta sequence.

See also `seqel-summary', `seqel-nuc-summary', `seqel-pro-summary'."
  (interactive)
  (save-excursion
    (seqel-fasta-mark)
    (if seqel-pro-mode
        (seqel-pro-summary (point) (mark))
      (seqel-nuc-summary (point) (mark)))))

(defun seqel-fasta--paint (&optional case)
  "Paint current fasta sequence by their residue identity.

By default, lower and upper cases are painted in the same colors.
If CASE is not nil, this function honors the case."
  (condition-case err
      (progn (seqel-fasta-mark)
             (cond (seqel-nuc-mode
                    (seqel-nuc-paint (region-beginning) (region-end) case))
                   (seqel-pro-mode
                    (seqel-pro-paint (region-beginning) (region-end) case))
                   (t
                    (error "Unknown seq type"))))
    ((debug error)
     (primitive-undo 1 buffer-undo-list)
     ;; get the original error message
     (error "%s" (error-message-string err)))))

(defun seqel-fasta-unpaint ()
  "Unpaint current sequence.

It calls `seqel-unpaint'."
  (interactive)
  (save-excursion
    (seqel-fasta-mark)
    (seqel-unpaint (region-beginning) (region-end))))

(defun seqel-fasta-paint (&optional case)
  "Paint current sequence.

It is just a wrapper around `seqel-fasta--paint'.  Optional argument
CASE is set to non-nil to paint with case sensitivity."
  (interactive "P")
  (save-excursion
    (seqel-fasta--paint case)))

(defun seqel-fasta-unpaint-all ()
  "Unpaint all the sequences.

It calls `seqel-unpaint' on each fasta record."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (seqel-fasta-backward 1)
      (seqel-fasta-mark)
      (seqel-unpaint (region-beginning) (region-end))
      (seqel-fasta-backward 1))))

(defun seqel-fasta-paint-all (&optional case)
  "Paint all sequences.

It calls `seqel-fasta--paint' on each fasta record.  Optional argument
CASE is set to non-nil to paint with case sensitivity."
  (interactive "P")
  (save-excursion
    (goto-char (point-max))
    (while (seqel-fasta-backward 1)
      (seqel-fasta--paint case)
      (seqel-fasta-backward 1))))

;;; column manipulations
(defmacro seqel-fasta--column-action (&rest fn)
  "A macro called by other column manipulation functions.

FN is a piece of code that does some specific manipulation
at the current column of all fasta records.  See `fasta-insert-column'
and `seqel-fasta-delete-column' for an example of usage."
  `(save-excursion
     (let ((column (current-column))
           pos line)
       (condition-case err
           (progn (goto-char (point-max))
                  (while (seqel-fasta-backward 1)
                    (forward-line) ; move to the sequence region
                    (setq line (line-number-at-pos))
                    (if (< (move-to-column column) column)
                        (signal 'error '(move-to-column)))
                    ,@fn
                    (seqel-fasta-backward 1)))
       ;; return to the original state if error is met.
       (error
        (primitive-undo 1 buffer-undo-list)
        (error "Abort: line %d is shorter than the column number (%d)"
               line column))))))


(defun seqel-fasta-column-delete (&optional n)
  "Delete current column(s) for all sequences.

Delete the current column by default, unless the optional
argument N is set to delete the number of chars starting from the
cursor."
  (interactive "p")
  (seqel-fasta--column-action (delete-char n)))


(defun seqel-fasta-column-insert (str)
  "Insert a string STR at the column."
  (interactive "sInsert string: ")
  (seqel-fasta--column-action (insert str)))


(defun seqel-fasta-column-highlight (&optional to-face)
  "Highlight the current column with the face TO-FACE.

It can be used to highlight/un-highlight the current column in
sequence alignment fasta file.  Interactively, TO-FACE is raw
prefix argument.  If TO-FACE is omitted or nil, mark the column
with highlight face by default; otherwise, unmark the column."
  (interactive "P")
  ;; (princ to-face)
  (or to-face ; without C-u
      (setq to-face 'highlight))
  (seqel-fasta--column-action
   (put-text-property (point) (1+ (point)) 'font-lock-face to-face)))


(defun seqel-fasta-column-paint (&optional case)
  "Paint the current column according their aa or nuc bases.

CASE is raw prefix argument.  If it is omitted or nil, lower and
upper cases are painted in the same colors; otherwise, it honors
the case."
  (interactive "P")
  (let ((current-char '(char-after)) to-face)
    (or case
        (setq current-char (list 'upcase current-char)))
    (cond (seqel-nuc-mode
           (setq to-face (list 'format "nuc-base-face-%c" current-char)))
          (seqel-pro-mode
           (setq to-face (list 'format "pro-aa-face-%c" current-char)))
          (t
           (error "Unknown sequence type.  Please specify protein or nucletide")))
    (seqel-fasta--column-action
     (with-silent-modifications
       (put-text-property (point) (1+ (point))
                          'font-lock-face
                          (intern (eval to-face)))))))


(defun seqel-fasta-column-summary (&optional case)
  "Summary of the column.

If CASE is nil, the summary will be case insensitive."
  (interactive "P")
  (let ((my-hash (make-hash-table :test 'equal))
        count char)
    (seqel-fasta--column-action (setq char (char-after))
                          (or case (setq char (upcase char)))
                          (setq count (gethash char my-hash))
                          (if count
                              (puthash char (1+ count) my-hash)
                            (puthash char 1 my-hash)))
    (if (called-interactively-p 'interactive)
        (maphash (lambda (x y) (princ (format "%c:%d " x y))) my-hash)
      my-hash)))


(defun seqel-fasta-bioseq-type (&optional threshold)
  "Enable the minor mode for either protein or nucleic acid.

It will search the first THRESHOLD number of sequence
residues (100 if THRESHOLD is nil), or the current sequence
record (whichever is smaller) for unique nucletide base and
unique protein amino acid IUPAC code.  If found, the proper minor
mode (variable `seqel-nuc-mode' or variable `seqel-pro-mode') will be
enabled.  If it is ambiguous, enable variable `seqel-nuc-mode' by
default."
  (let ((pro-aa-uniq '(?E ?F ?I ?J ?L ?P ?Q ?Z ?e ?f ?i ?j ?l ?p ?q ?z))
        current)
    (save-mark-and-excursion
        (catch 'bioseq-type
          (seqel-fasta-mark)
          ;; 100 sequence residues or the current sequence length
          (dotimes (i (min (or threshold 100) (- (region-end) (region-beginning))))
            (setq current (char-after))
            (cond ((memq current pro-aa-uniq)
                   (seqel-pro-mode)
                   (throw 'bioseq-type 'pro))
                  ((= ?U (upcase current))
                   (seqel-nuc-mode)
                   (throw 'bioseq-type 'nuc)))
            (forward-char))
        ;; if no uniq char found for pro or nuc sequences; enable seqel-nuc-mode by default
        (seqel-nuc-mode)
        (throw 'bioseq-type 'nuc)))))


(add-hook 'seqel-fasta-mode-hook
          (lambda ()
            (seqel-fasta-bioseq-type)
            ;; these modes can slow the loading of a large fasta file;
            ;; disable these minor modes
            (flyspell-mode -1)
            (linum-mode -1)))


(provide 'seqel-fasta-mode)


;;; seqel-fasta-mode.el ends here
