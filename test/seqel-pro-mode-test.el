(require 'seqel-pro-mode)

(ert-deftest seqel-pro-weight-test ()
  :tags '(pro-mode)
  (let ((cases '(("ACDEFGHIKLMNPQRSTVWY" . 2378)
                 ("Aa" . 142)
                 ("Ax" . 163)
                 ("A" . 71))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test)) ; prepare the buffer
        (should (equal (round (call-interactively 'seqel-pro-weight))
                       (cdr test)))
        (delete-region (point-min) (point-max))))))

(ert-deftest seqel-pro-weight-error-test ()
  :tags '(pro-mode)
  (let ((cases '("?ACDEFGHIKLMNPQRSTVWY")))
    (with-temp-buffer
      (dolist (test cases)
        (insert test) ; prepare the buffer
        (should-error (call-interactively 'seqel-pro-weight))
        (delete-region (point-min) (point-max))))))


(ert-deftest seqel-pro-count-test ()
  :tags '(pro-mode)
  (let ((cases '(("ABCDEFGHIJKLMNPQRSTVWXYZabcdefghijklmnpqrstvwxyz" . 48)
                 ("12345"    .       nil)))
        exp)
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (set-mark (point-min))
        (goto-char (point-max))
        (setq exp (cdr test))
        (if exp
            (should (equal (call-interactively 'seqel-pro-count) exp))
          (should-error (call-interactively 'seqel-pro-count)))

        (delete-region (point-min) (point-max))))))


(ert-deftest seqel-pro-3-2-1-2-3-test ()
  :tags '(pro-mode)
  (let ((cases '(("AlaCysAspGluPheGlyHisIleLysAsxXaaGlx" . "ACDEFGHIKBXZ")
                 ("AsnProGlnArgSerThrValTrpTyr" . "NPQRSTVWY"))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (set-mark (point-min))
        (goto-char (point-max))
        (call-interactively 'seqel-pro-3-2-1)
        (should (equal (buffer-string) (cdr test)))

        (set-mark (point-min))
        (goto-char (point-max))
        (call-interactively 'seqel-pro-1-2-3)
        (should (equal (buffer-string) (car test)))

        (delete-region (point-min) (point-max))))))

(ert-deftest seqel-pro-summary-test ()
  :tags '(pro-mode)
  (let ((cases '(("ABCa" .
                  ((?A . 1) (?B . 1) (?C . 1) (?a . 1)))))
        obs)
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (setq obs (seqel-pro-summary (point-min) (point-max)))
        (maphash (lambda (k v) (if (= 0 (gethash k obs)) (remhash k obs))) obs)
        (should (seqel-hash-equal obs (seqel-hash-alist (cdr test))))
        (delete-region (point-min) (point-max))))))
