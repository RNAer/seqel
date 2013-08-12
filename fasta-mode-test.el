;;; nuc-mode-test.el --- a unit test for nuc-mode.el

(require 'fasta-mode)
(require 'ert)


(ert-deftest fasta-count-test ()
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
        (should (equal (fasta-count) (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest fasta-formt-test ()
  :tags '(fasta-mode)
    (let ((cases '((
">seq_name b
augc 	tAUGCT
augct

>seq_name a
augct"

5

">seq_name b
augct
AUGCT
augct
>seq_name a
augct")))
          tmp)
    (with-temp-buffer
      (dolist (test cases)
        (insert (nth 0 test))
        (goto-char (point-min))
        (setq current-prefix-arg (nth 1 test))
        (call-interactively 'fasta-format)
        (setq tmp (buffer-string))
        (should (equal tmp (nth 2 test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest fasta-seq-length-test ()
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
        (should (equal (call-interactively 'fasta-seq-length) (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest fasta-seq-type-test ()
  :tags '(fasta-mode)
  (let ((cases '((">seq_name a
augctAUGCT
>seq_name b
augct" . nuc))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (should (equal (fasta-seq-type) (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest fasta-position-test ()
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
        (should (equal (call-interactively 'fasta-position) (nth 2 test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest fasta-rc-test ()
  :tags '(fasta-mode)
  (let ((cases '((
">seq_name a
augc 	AUGC
augc" .
">seq_name a
gcau
GCAU	 gcau")))
        tmp)
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (goto-char (point-min))
        (nuc-mode 1)
        (call-interactively 'fasta-rc)
        (setq tmp (buffer-string))
        (should (equal tmp (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest fasta-delete-column-test ()
  :tags '(fasta-mode)
  (let ((cases '((1   ; should case
                  ">seq_name a
augctAUGCTaugct

>seq_name b
augct"
                  17
">seq_name a
augcAUGCTaugct

>seq_name b
augc")
                 (2   ; should-error case
                  ">seq_name a
augctAUGCTaugct

>seq_name b
augct" 18)))
        tmp   type-test)
    (with-temp-buffer
      (dolist (test cases)
        (setq type-test (nth 0 test))
        (insert (nth 1 test))
        (goto-char (nth 2 test))
        (cond ((= type-test 1)
               (call-interactively 'fasta-delete-column)
               (setq tmp (buffer-string))
               (should (equal tmp (nth 3 test))))
              ((= type-test 2)
               (should-error (call-interactively 'fasta-delete-column))))
         (delete-region (point-min) (point-max))))))


(ert-deftest fasta-insert-column-test ()
  :tags '(fasta-mode)
    (let ((cases '((
">seq_name a
augctAUGCTaugct

>seq_name b
augct"
18  "A"
">seq_name a
augctAAUGCTaugct

>seq_name b
augctA")))
          tmp)
    (with-temp-buffer
      (dolist (test cases)
        (insert (nth 0 test))
        (goto-char (nth 1 test))
        ;; (call-interactively 'fasta-insert-column)
        (fasta-insert-column (nth 2 test))
        (setq tmp (buffer-string))
        (should (equal tmp (nth 3 test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest fasta-2-stockholm-test ()
  :tags '(fasta-mode))


(ert-deftest fasta-summary-column-test ()
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
        (princ (nth 2 test))
        (princ (hash-alist (nth 2 test)))
        (should (hash-equal
                 (fasta-summary-column)
                 (hash-alist (nth 2 test))))
        (delete-region (point-min) (point-max))))))

