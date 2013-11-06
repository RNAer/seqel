;;; genbank-mode -- a major mode to edit genbank files

(require 'seq)
(require 'nuc-mode)
(require 'pro-mode)

(defvar genbank-mode-hook nil
  "*Hook to setup `genbank-mode'.")

(defvar genbank-mode-map
  ;; use `make-keymap' if there are lots of keybindings
  (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    (define-key map "\C-ca"     'genbank-first)
    (define-key map "\C-cc"     'genbank-count)
    (define-key map "\C-cd"     'genbank-delete)
    (define-key map "\C-ce"     'genbank-last)
    (define-key map "\C-cf"     'genbank-format)
    (define-key map "\C-cl"     'genbank-seq-length)
    ;; (define-key map "\C-cm"     'genbank-mark)
    (define-key map "\C-cp"     'genbank-position)
    (define-key map "\C-cr"     'genbank-rc)
    (define-key map "\C-c\C-d"  'genbank-delete-column)
    (define-key map "\C-c\C-i"  'genbank-insert-column)
    (define-key map "\C-c\C-h"  'genbank-highlight-column)
    (define-key map "\C-c\C-p"  'genbank-paint-column)
    (define-key map "\C-c\C-s"  'genbank-summary-column)
    map)
 "The local keymap for `genbank-mode'")


(defvar genbank-font-lock-keywords
  '(("^\\(LOCUS\\) +\\([-_.a-zA-Z_0-9]+\\)" ;; are '-_.' allowed?
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))

    ("^\\(VERSION\\) +\\([-_.a-zA-Z_0-9]+\\) +\\([Gg][Ii]\\):\\([0-9]+\\)" ;; are '-_.' allowed?
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     (3 font-lock-keyword-face)
     (4 font-lock-function-name-face))

    ;; more genbank keywords...
    "ORIGIN" "ACCESSION" "AUTHORS" "BASE COUNT" "DEFINITION"
    "FEATURES" "JOURNAL" "KEYWORDS" "MEDLINE" "NID "
    "ORGANISM" "REFERENCE" "SEGMENT" "SOURCE" "TITLE"
    "DBLINK" "PUBMED" "REMARK" "COMMENT" "CONSRTM"

    ;; line numbers...
    ("^[ \t]*\\([0-9]+\\)"
     (1 font-lock-string-face)))
  "Expressions to hilight in `genbank-mode'.")


;;;###autoload
(define-derived-mode genbank-mode text-mode "genbank"
  "Major mode for editing sequences in genbank format.

Special commands:
￼\\{genbank-mode-map}
  \\{nuc-mode-map}
  \\{pro-mode-map}"
  ;; This runs the normal hook change-major-mode-hook, then gets rid of
  ;; the buffer-local variables of the major mode previously in effect.
  ;; (kill-all-local-variables)
  ;; (setq mode-name "genbank")
  ;; (setq major-mode 'genbank-mode)
  ;; (use-local-map genbank-mode-map)
  ;; The above are automatically done if the mode is defined using
  ;; `define-derived-mode'.
  ;; the variable automatically becomes buffer-local when set
  (setq font-lock-defaults '(genbank-font-lock-keywords))
  (flyspell-mode -1)
  ;; (set-syntax-table genbank-mode-syntax-table)
  (run-hooks 'genbank-mode-hook))


;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.\\(genbank\\|gb\\)\\'" . genbank-mode))


(defun genbank-mark (&optional whole)
  "Put point at the beginning of the sequence and mark the end.

If a prefix arg is provided or WHOLE is t, then put the point at
the beginning of the genbank entry instead of the sequence."
  (interactive "P")
  (genbank-forward 1)
  (push-mark)
  (genbank-backward 1)
  (or whole
      (forward-line)))


(defun genbank-position ()
  "Return the position of point in the current sequence.

It will not count white spaces and seq gaps. The count starts
at zero."
  (interactive)
  (if (looking-at genbank-record-regexp)
      (error "Point is not in the sequence region"))
  (let ((pos   (point))
        (count 0))
    (save-excursion
      (if (> (genbank-backward 1) 0)
          (error "The start of the genbank record is not found"))
      (end-of-line)
      (while (< (point) pos)
        (if (not (looking-at seq-cruft-regexp))
            (setq count (1+ count)))
        (forward-char)))
    (if (called-interactively-p 'interactive)
        (message "Position %d" count))
    count))


(defun genbank-seq-length ()
  "Return the length of current sequence."
  (interactive)
  (let (length)
    (save-excursion
      (genbank-forward 1)
      (backward-char)
      (setq length (genbank-position)))
    (if (called-interactively-p 'interactive)
        (message "Length %d" length))
    length))


;;;###autoload
(defun genbank-rc (is-rna)
  "Reverse complement current genbank sequence if it is nuc sequence."
  (interactive "P")
  (save-excursion
    (genbank-mark)
    (let ((beg (region-beginning))
          (end (region-end)))
      (if nuc-mode  ; if nuc-mode is enabled
          (nuc-reverse-complement beg (1- end) is-rna)
        (error "nuc mode is not enabled")))))

;;;###autoload
(defun genbank-2-fasta ()
  (interactive))


(provide 'genbank-mode)


;;; genbank-mode.el ends here
