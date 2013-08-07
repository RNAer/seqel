;;; nuc-mode-test.el --- a unit test for nuc-mode.el

(require 'nuc-mode)
(require 'ert)

(ert-deftest nuc-whr-test ()
  :tags '(nuc-mode)
  (let ((cases '(("TGATTCAAGCATTCGATC" . 1.6)
                 ("GGGTGCCCCCAAAATATT" . 7.25))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test)) ; prepare the buffer
        (should (equal (nuc-whr (point-min) (point-max))
                       (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest nuc-p-test ()
  :tags '(nuc-mode))

(ert-deftest rna-p-test ()
  :tags '(nuc-mode))

(ert-deftest dna-p-test ()
  :tags '(nuc-mode))

(ert-deftest 2rna-test ()
  :tags '(nuc-mode))

(ert-deftest 2dna-test ()
  :tags '(nuc-mode))

(ert-deftest nuc-paint-region-test ()
  :tags '(nuc-mode))

(ert-deftest nuc-complement-test ()
  :tags '(nuc-mode))

(ert-deftest nuc-rc-test ()
  :tags '(nuc-mode))

(ert-deftest nuc-base-summary-test ()
  :tags '(nuc-mode))
