;;; genbank-mode -- a major mode to edit genbank files

;;; license: BSD-3

;;; Author: Zech Xu <zhenjiang dot xu at gmail dot com>

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
    ;; (define-key map "\C-cm"     'genbank-mark)
    (define-key map "\C-cp"     'genbank-position)
    map)
 "The local keymap for `genbank-mode'")


(defvar genbank-font-lock-keywords
  '(("^\\(LOCUS\\) +\\([-_.a-zA-Z_0-9]+\\)" ;; are '-_.' allowed?
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))

    ;; are '-_.' allowed?
    ("^\\(VERSION\\) +\\([-_.a-zA-Z_0-9]+\\) +\\([Gg][Ii]\\):\\([0-9]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     (3 font-lock-keyword-face)
     (4 font-lock-function-name-face))

    ;; more genbank keywords...
    ;; will be font-lock-keyword-face by default
    "DEFINITION" "ACCESSION" "DBLINK" "ORIGIN"
    "KEYWORDS" "SOURCE" "REFERENCE" "FEATURES"
    "COMMENT"

    ("^ +\\(ORGANISM\\)" . font-lock-doc-face)
    ("^ +\\(AUTHORS\\)"  . font-lock-doc-face)
    ("^ +\\(TITLE\\)"    . font-lock-doc-face)
    ("^ +\\(JOURNAL\\)"  . font-lock-doc-face)
    ("^ +\\(PUBMED\\)"   . font-lock-doc-face)

    ;; line numbers at the beginning of sequence...
    ("^[ \t]*\\([0-9]+\\)"
     (1 font-lock-string-face)))
  "Expressions to highlight in `genbank-mode'.")


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


(defvar genbank-record-regexp "^LOCUS[ \t].*$"
  "Genbank records always start with \"LOCUS\".")


;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.\\(genbank\\|gb\\)\\'" . genbank-mode))


(defun genbank-forward (count)
  "Move forward to the end genbank record.

It works in the style of `forward-paragraph'. COUNT needs to be positive integer.
Return current point if it moved over COUNT of records; otherwise return nil."
  (interactive "p")
  (entry-forward count genbank-record-regexp))

(defun genbank-backward (count)
  "Move the point the beginning of the genbank record.

It works in the style of `backward-paragraph'. COUNT needs to be positive integer.
Return current point if it moved over COUNT of records; otherwise return nil."
  (interactive "p")
  (entry-backward count genbank-record-regexp))

(defun genbank-first ()
  "Go to the beginning of first genbank record."
  (interactive)
  (entry-first genbank-record-regexp))

(defun genbank-last ()
  "Go to the beginning of last genbank record."
  (interactive)
  (entry-last genbank-record-regexp))

(defun genbank-count ()
  (interactive)
  (entry-count genbank-record-regexp))


(defun genbank-mark (&optional whole)
  "Put point at the beginning of the sequence and mark the end.

If a prefix arg is provided or WHOLE is t, then put the point at
the beginning of the genbank entry instead of the sequence."
  (interactive "P")
  (if (genbank-forward 1)
      (backward-char))
  (push-mark nil nil t)
  (genbank-backward 1)
  (or whole
      (progn (re-search-forward "^ORIGIN *$" nil)
             (forward-line))))


(defun genbank-delete ()
  "Delete the current genbank record."
  (interactive)
  (genbank-mark 'whole)
  (delete-region (region-beginning) (region-end)))


;;;###autoload
(defun genbank-2-fasta ()
  "Convert current genbank record to fasta format"
  (interactive)
  (genbank-mark))


(provide 'genbank-mode)


;;; genbank-mode.el ends here
