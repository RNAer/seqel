;;; seqel-genbank-mode.el --- A major mode to edit genbank files

;; Copyright (C) 2021  Zech Xu

;; Author: Zech Xu
;; Version: 1.0
;; License: BSD-3
;; URL: https://github.com/RNAer/seqel

;;; Commentary:

;; Major mode to edit genbank files

;;; Code:


(require 'seqel)
(require 'seqel-nuc-mode)
(require 'seqel-pro-mode)

(defvar seqel-genbank-mode-hook nil
  "*Hook to setup `seqel-genbank-mode'.")

(defvar seqel-genbank-mode-map
  ;; use `make-keymap' if there are lots of keybindings
  (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    (define-key map "\C-c\C-a"  'seqel-genbank-first)
    (define-key map "\C-c\C-z"  'seqel-genbank-last)
    (define-key map "\C-c\C-c"  'seqel-genbank-count)
    (define-key map "\C-c\C-d"  'seqel-genbank-delete)
    (define-key map "\C-c\C-f"  'seqel-genbank-forward) ; it also binds to M-}
    (define-key map "\C-c\C-b"  'seqel-genbank-backward); it also binds to M-}
    (define-key map "\C-c\C-m"  'seqel-genbank-mark)    ; it also binds to M-h
    map)
 "The local keymap for `seqel-genbank-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.\\(genbank\\|gb\\|gbk\\)\\'" . seqel-genbank-mode))

;; map the paragraph key bindings to corresponding functions
(let ((equivs
       '((seqel-genbank-forward  . forward-paragraph)
         (seqel-genbank-backward . backward-paragraph)
         (seqel-genbank-mark     . mark-paragraph))))
  (dolist (x equivs)
    (substitute-key-definition (cdr x)
                               (car x)
                               seqel-genbank-mode-map
                               (current-global-map))))

(defvar seqel-genbank-font-lock-keywords
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
  "Expressions to highlight in `seqel-genbank-mode'.")


;;;###autoload
(define-derived-mode seqel-genbank-mode text-mode "genbank"
  "Major mode for editing sequences in genbank format.

Special commands:
￼\\{seqel-genbank-mode-map}
  \\{seqel-nuc-mode-map}
  \\{seqel-pro-mode-map}"
  ;; This runs the normal hook change-major-mode-hook, then gets rid of
  ;; the buffer-local variables of the major mode previously in effect.
  ;; (kill-all-local-variables)
  ;; (setq mode-name "genbank")
  ;; (setq major-mode 'seqel-genbank-mode)
  ;; (use-local-map seqel-genbank-mode-map)
  ;; The above are automatically done if the mode is defined using
  ;; `define-derived-mode'.
  ;; the variable automatically becomes buffer-local when set
  (setq font-lock-defaults '(seqel-genbank-font-lock-keywords))
  (flyspell-mode -1)
  ;; (set-syntax-table genbank-mode-syntax-table)
  (run-hooks 'seqel-genbank-mode-hook))


(defvar seqel-genbank-record-regexp "^LOCUS[ \t]+"
  "Genbank records always start with \"LOCUS\".")

(defvar seqel-genbank-record-end "^//[ \t]*"
  "Genbank records always end with \"//\".")


(defun seqel-genbank-forward (count)
  "Move forward to the end genbank record.

It works in the style of `forward-paragraph'.  COUNT needs to be
positive integer.  Return current point if it moved over COUNT of
records; otherwise return nil."
  (interactive "p")
  (seqel-entry-forward count seqel-genbank-record-regexp))

(defun seqel-genbank-backward (count)
  "Move the point the beginning of the genbank record.

It works in the style of `backward-paragraph'.  COUNT needs to be
positive integer.  Return current point if it moved over COUNT of
records; otherwise return nil."
  (interactive "p")
  (seqel-entry-backward count seqel-genbank-record-regexp))

(defun seqel-genbank-first ()
  "Go to the beginning of first genbank record."
  (interactive)
  (seqel-entry-first seqel-genbank-record-regexp))

(defun seqel-genbank-last ()
  "Go to the beginning of last genbank record."
  (interactive)
  (seqel-entry-last seqel-genbank-record-regexp))

(defun seqel-genbank-count ()
  "Count the number of genbank records in the file."
  (interactive)
  (seqel-entry-count seqel-genbank-record-regexp))


(defun seqel-genbank-mark (&optional whole)
  "Put point at the beginning of the sequence and mark the end.

If a prefix arg is provided or WHOLE is t, then put the point at
the beginning of the genbank entry instead of the sequence."
  (interactive "P")
  (if (seqel-genbank-forward 1)
      (backward-char))
  (seqel-entry-backward 1 seqel-genbank-record-end)
  (forward-line)
  (push-mark nil nil t)
  (seqel-genbank-backward 1)
  (or whole
      (progn (re-search-forward "^ORIGIN *$" nil)
             (forward-line))))


(defun seqel-genbank-delete ()
  "Delete the current genbank record."
  (interactive)
  (seqel-genbank-mark 'whole)
  (delete-region (region-beginning) (region-end)))


(defun seqel-genbank--2-fasta ()
  "Convert current genbank record to fasta format."
  (let (str seq)
    (seqel-genbank-mark)
    (setq str (buffer-substring-no-properties (region-beginning) (region-end)))
    (if seqel-nuc-mode
        (setq seq (mapcan (lambda (i) (if (gethash i seqel-nuc-alphabet-set) (list (upcase i)))) str))
      (setq seq (mapcan (lambda (i) (if (gethash i seqel-pro-alphabet-set) (list (upcase i)))) str)))
    (seqel-genbank-mark 'whole)
    (if (re-search-forward seqel-genbank-record-regexp nil t)
        (replace-match ">" nil nil))
    (forward-line)
    (delete-region (region-beginning) (region-end))
    (insert (concat seq))
    (insert "\n")))

(defun seqel-genbank-2-fasta ()
  "Convert current genbank record to fasta format."
  (interactive)
  (save-excursion
    (seqel-genbank--2-fasta)))

(defun seqel-genbank-2-fasta-all ()
  "Convert all genbank records to fasta format."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (seqel-genbank-backward 1)
      (seqel-genbank--2-fasta))))

(provide 'seqel-genbank-mode)


;;; seqel-genbank-mode.el ends here
