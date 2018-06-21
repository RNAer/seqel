;;; pro-mode-test.el --- a unit test for pro-mode.el

(require 'pro-mode)
(require 'seq)
(require 'ert)

(ert-deftest pro-weight-test ()
  :tags '(pro-mode)
  (let ((cases '(("ACDEFGHIKLMNPQRSTVWY" . 2378)
                 ("Aa" . 142)
                 ("Ax" . 163)
                 ("A" . 71))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test)) ; prepare the buffer
        (should (equal (round (call-interactively 'pro-weight))
                       (cdr test)))
        (delete-region (point-min) (point-max))))))

(ert-deftest pro-weight-error-test ()
  :tags '(pro-mode)
  (let ((cases '("?ACDEFGHIKLMNPQRSTVWY")))
    (with-temp-buffer
      (dolist (test cases)
        (insert test) ; prepare the buffer
        (should-error (call-interactively 'pro-weight))
        (delete-region (point-min) (point-max))))))


(ert-deftest pro-count-test ()
  :tags '(pro-mode)
  (let ((cases '(("ABCDEFGHIJKLMNPQRSTVWXYZabcdefghijklmnpqrstvwxyz" . 48)
                 ("12345"    .       nil))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (set-mark (point-min))
        (goto-char (point-max))
        (should (equal (call-interactively 'pro-count)
                       (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest pro-3-2-1-2-3-test ()
  :tags '(pro-mode)
  (let ((cases '(("AlaCysAspGluPheGlyHisIleLysAsxXaaGlx" . "ACDEFGHIKBXZ")
                 ("AsnProGlnArgSerThrValTrpTyr" . "NPQRSTVWY"))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (set-mark (point-min))
        (goto-char (point-max))
        (call-interactively 'pro-3-2-1)
        (should (equal (buffer-string) (cdr test)))

        (set-mark (point-min))
        (goto-char (point-max))
        (call-interactively 'pro-1-2-3)
        (should (equal (buffer-string) (car test)))

        (delete-region (point-min) (point-max))))))

(ert-deftest pro-summary-test ()
  :tags '(nuc-mode)
  (let ((cases '(("ABCa" .
                  ((?A . 1) (?B . 1) (?C . 1) (?a . 1))))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (should
         ;; `equal' can compare hash tables
         (hash-equal (region-summary (point-min) (point-max) pro-aa-regexp)
                (hash-alist (cdr test))))
        (delete-region (point-min) (point-max))))))


;; pro-mode-test.el ends here
