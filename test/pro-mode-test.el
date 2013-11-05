;;; pro-mode-test.el --- a unit test for pro-mode.el

(require 'pro-mode)
(require 'seq)
(require 'ert)

(ert-deftest pro-weight-test ()
  :tags '(pro-mode)
  (let ((cases '(("TGATTCAAGCATTCGATC" . 1.6)
                 ("GGGTGCCCCCAAAATATT" . 7.25))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test)) ; prepare the buffer
        (should (equal (pro-weight (point-min) (point-max))
                       (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest pro-count-test ()
  :tags '(pro-mode))

(ert-deftest pro-1-2-3-test ()
  :tags '(pro-mode))

(ert-deftest pro-3-2-1-test ()
  :tags '(pro-mode)
>sequence1
AlaCysAspGluPheGlyHisIleLysAsxXaaGlx

>sequence2
AsnProGlnArgSerThrValTrpTyr***

>sequence1
ACDEFGHIKBXZ

>sequence2
NPQRSTVWY*)


(ert-deftest pro-summary-test ()
  :tags '(pro-mode))


;; pro-mode-test.el ends here
