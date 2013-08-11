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
        (should (equal tmp (nth 2 test)))))))


(ert-deftest fasta-seq-length-test ()
  :tags '(fasta-mode))

(ert-deftest fasta-seq-type-test ()
  :tags '(fasta-mode))

(ert-deftest fasta-position-test ()
  :tags '(fasta-mode)
  (let ((cases '((
">seq_name b
augc 	tAUGCT
augct"
19 4))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (nth 0 test))
        (goto-char (nth 1 test))
        (should (equal (call-interactively 'fasta-position) (nth 2 test)))))))


(ert-deftest fasta-complement-test ()
  :tags '(fasta-mode))


(ert-deftest fasta-rc-test ()
  :tags '(fasta-mode))


(ert-deftest fasta-delete-column-test ()
  :tags '(fasta-mode))

(ert-deftest fasta-insert-column-test ()
  :tags '(fasta-mode))

(ert-deftest fasta-2-stockholm-test ()
  :tags '(fasta-mode))

(ert-deftest fasta-align-test ()
  :tags '(fasta-mode))

(ert-deftest fasta-paint-test ()
  :tags '(fasta-mode))

(ert-deftest fasta-summary-test ()
  :tags '(fasta-mode))
