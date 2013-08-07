;;; seq-test.el --- a unit test for seq package

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
