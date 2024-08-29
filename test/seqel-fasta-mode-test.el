;;; seqel-fasta-mode-test.el --- Tests for seqel-fasta-mode.el  -*- lexical-binding: t; -*-

(require 'seqel-fasta-mode)


(ert-deftest seqel-fasta-forward-test ()
  :tags '(fasta-mode)
  (let ((cases '(("" 1 1)
                 (">a
acgu

>b
AcGu
"
                  1 4))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (nth 0 test))
        (goto-char (point-min))
        (setq current-prefix-arg (nth 1 test))
        (call-interactively 'seqel-fasta-forward)
        ;; set its value back to avoid side effects for other functions
        (setq current-prefix-arg nil)
        (should (equal (line-number-at-pos) (nth 2 test)))
        (delete-region (point-min) (point-max))))))

(ert-deftest seqel-fasta-backward-test ()
  :tags '(fasta-mode)
  (let ((cases '(("" 1 1)
                 (">a
acgu

>b
AcGu
"
                  2 1))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (nth 0 test))
        (setq current-prefix-arg (nth 1 test))
        (call-interactively 'seqel-fasta-backward)
        ;; set its value back to avoid side effects for other functions
        (setq current-prefix-arg nil)
        (should (equal (line-number-at-pos) (nth 2 test)))
        (delete-region (point-min) (point-max))))))

(ert-deftest seqel-fasta-first-test ()
  :tags '(fasta-mode)
  (let ((cases '(""
                 ">a
acgu

>b
AcGu
")))
    (with-temp-buffer
      (dolist (test cases)
        (insert test)
        (call-interactively 'seqel-fasta-first)
        (should (equal (point-min) (point)))
        (delete-region (point-min) (point-max))))))

(ert-deftest seqel-fasta-last-test ()
  :tags '(fasta-mode)
  (let ((cases '(("" . 1)
                 (">a
acgu

>b
AcGu
"
                  . 4))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (goto-char (point-min))
        (call-interactively 'seqel-fasta-last)
        (should (equal (line-number-at-pos) (cdr test)))
        (delete-region (point-min) (point-max))))))

(ert-deftest seqel-fasta-count-test ()
  :tags '(fasta-mode)
  (let ((cases '(("" . 0)
                 (">a
acgu

>b
AcGu
"
                  . 2))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (should (equal (call-interactively 'seqel-fasta-count) (cdr test)))
        (delete-region (point-min) (point-max))))))

(ert-deftest seqel-fasta-mark-test ()
  :tags '(seqel-fasta-mark)
  (let ((cases '(("" 1 1)
                 (">a
acgu

>b
AcGu
"
                  13 18))))
        (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (seqel-fasta-mark)
        (should (equal (region-beginning) (nth 1 test)))
        (should (equal (region-end) (nth 2 test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest seqel-fasta-format-test ()
  :tags '(fasta-mode)
    (let ((cases '((
">seq_name b
augc 	tAUGCT
augct

>seq_name a
augctt"

5

">seq_name b
augct
AUGCT
augct

>seq_name a
augctt")))
          tmp)
    (with-temp-buffer
      (dolist (test cases)
        (insert (nth 0 test))
        (goto-char (point-min))
        (setq current-prefix-arg (nth 1 test))
        (call-interactively 'seqel-fasta-format)
        (setq tmp (buffer-string))
        (should (equal tmp (nth 2 test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest seqel-fasta-format-all-test ()
  :tags '(fasta-mode)
    (let ((cases '((
">seq_name b
augc 	tAUGCT
augct

>seq_name a
augctt"

5

">seq_name b
augct
AUGCT
augct

>seq_name a
augct
t")))
          tmp)
    (with-temp-buffer
      (dolist (test cases)
        (insert (nth 0 test))
        (setq current-prefix-arg (nth 1 test))
        (call-interactively 'seqel-fasta-format-all)
        (setq tmp (buffer-string))
        (should (equal tmp (nth 2 test)))
        (delete-region (point-min) (point-max))))))

(ert-deftest seqel-fasta-delete-test ()
  :tags '(fasta-mode)
  (let ((cases '(("" . "")
                 (">seq_name a
augc 	tAUGCT
augct
>seq_name b
a" . "
>seq_name b
a"))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (goto-char (point-min))
        (call-interactively 'seqel-fasta-delete)
        (setq tmp (buffer-string))
        (should (equal tmp (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest seqel-fasta-length-test ()
  :tags '(fasta-mode)
  (let ((cases '((
">seq_name a
augc 	tAUGCT
augct
>seq_name b
a" . 15))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (goto-char (point-min))
        (should (equal (call-interactively 'seqel-fasta-length) (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest seqel-fasta-position-test ()
  :tags '(fasta-mode)
  (let ((cases '((
">seq_name a
augc 	tAUGCT
augct"
19 4))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (nth 0 test))
        (goto-char (nth 1 test))
        (should (equal (call-interactively 'seqel-fasta-position) (nth 2 test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest seqel-fasta-rc-test ()
  :tags '(fasta-mode)
  (let ((cases '((
">seq_name a
ugc 	AUGC
augc" .
">seq_name a
gcau
GCAU	 gca")))
        tmp)
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (seqel-nuc-mode 1)
        (call-interactively 'seqel-fasta-rc)
        (setq tmp (buffer-string))
        (should (equal tmp (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest seqel-fasta-translate-test ()
  :tags '(fasta-mode)
   (let ((cases '((
">seq_name a
at gc
ag
" .
">seq_name a
MQ
")
                  (
">seq_name a
at gc
" .
">seq_name a
Mc
")))
        tmp)
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (seqel-nuc-mode 1)
        (call-interactively 'seqel-fasta-translate)
        (setq tmp (buffer-string))
        (should (equal tmp (cdr test)))
        (delete-region (point-min) (point-max))))))

(ert-deftest seqel-fasta-weight-test ()
  :tags '(fasta-mode)
   (let ((cases '((
">seq_name a
GKVKVGVNG FGRIGRLVTR AAFNSGKVDI
VAINDPFIDL NYMVYMFQYD STHGKFHGTV
" . 6505.0) )))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (seqel-pro-mode 1)
        (should (equal (fround (call-interactively 'seqel-fasta-weight)) (cdr test)))
        (delete-region (point-min) (point-max))))))

(ert-deftest seqel-fasta-column-delete-test ()
  :tags '(fasta-mode)
  (let ((cases '((1   ; should case: test the buffer after deletion
                  ">seq_name a\naugctAUGCTaugct\n\n>seq_name b\naugct\n"
                  17
                  ">seq_name a\naugcAUGCTaugct\n\n>seq_name b\naugc\n")
                 (2   ; should-error case: the 2nd seq don't have any nuc in that column to delete
                  ">seq_name a\naugctAUGCTaugct\n>seq_name b\naugct"
                  18)))
        tmp   type-test)
    (with-temp-buffer
      (dolist (test cases)
        (setq type-test (nth 0 test))
        (insert (nth 1 test))
        (goto-char (nth 2 test))
        (cond ((= type-test 1)
               (call-interactively 'seqel-fasta-column-delete)
               (setq tmp (buffer-string))
               (should (equal tmp (nth 3 test))))
              ((= type-test 2)
               (should-error (call-interactively 'seqel-fasta-column-delete))))
         (delete-region (point-min) (point-max))))))


(ert-deftest seqel-fasta-column-insert-test ()
  :tags '(fasta-mode)
    (let ((cases '((">seq_name a
augctAUGCTaugct

>seq_name b
augct"
                    18
                    "A"
                    ">seq_name a
augctAAUGCTaugct

>seq_name b
augctA")))
          tmp)
    (with-temp-buffer
      (dolist (test cases)
        (insert (nth 0 test))
        (goto-char (nth 1 test))
        (seqel-fasta-column-insert (nth 2 test))
        (setq tmp (buffer-string))
        (should (equal tmp (nth 3 test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest seqel-fasta-column-summary-test ()
  :tags '(fasta-mode)
  (let ((cases '((
">seq_name a
augctAUGCTaugct
>seq_name b
aUgct
>seq_name c
ag
>seq_name d
aa
>seq_name e
ac"
14  ((?U . 2) (?G . 1) (?A . 1) (?C . 1))))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (nth 0 test))
        (goto-char (nth 1 test))
        ;; (princ (nth 2 test))
        ;; (princ (seqel-hash-alist (nth 2 test)))
        (should (seqel-hash-equal
                 (seqel-fasta-column-summary)
                 (seqel-hash-alist (nth 2 test))))
        (delete-region (point-min) (point-max))))))


(ert-deftest seqel-fasta-bioseq-type-test ()
  :tags '(fasta-mode)
  (let ((cases '((">seq1
A" . seqel-nuc-mode)
                 (">seq1
AUCG" . seqel-nuc-mode)
                 (">seq1
AAAAAAEFZ" . seqel-pro-mode))))
    (dolist (test cases)
      (with-temp-buffer
        (insert (car test))
        (seqel-fasta-bioseq-type)
        ;; return true if the mode is active
        (should (and (symbolp (cdr test)) (symbol-value (cdr test))))
        ;; disable both modes to start with clean buffer
        (seqel-nuc-mode -1)
        (seqel-pro-mode -1)))))

(ert-deftest seqel-fasta-bioseq-type-test-error ()
  :tags '(fasta-mode)
  (let ((cases '((">seq1
A" . seqel-pro-mode)
                 (">seq1
AUCG" . seqel-pro-mode)
                 (">seq1
AEFZ" . seqel-nuc-mode))))
    (dolist (test cases)
      (with-temp-buffer
        (insert (car test))
        (seqel-fasta-bioseq-type)
        (princ (and (symbolp (cdr test)) (symbol-value (cdr test))))
        (should (not (and (symbolp (cdr test)) (symbol-value (cdr test)))))
        (seqel-nuc-mode -1)
        (seqel-pro-mode -1)))))


(provide 'seqel-fasta-mode-test)
