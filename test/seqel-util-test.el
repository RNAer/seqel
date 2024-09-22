;;; seqel-test.el   -*- lexical-binding: t; -*-

(require 'seqel-util)

(ert-deftest seqel-summary-test ()
  :tags '(util)
  (let ((cases '(("a1..- " .
                  ((?a . 1) (?1 . 1) (?. . 2) (?- . 1) (? . 1)))))
        obs)
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (setq obs (seqel-summary (point-min) (point-max) ))
        (maphash (lambda (k v) (if (= 0 (gethash k obs)) (remhash k obs))) obs)
        (should
         ;; `equal' can not compare hash tables
         (seqel-hash-equal obs (seqel-hash-alist (cdr test))))
        (delete-region (point-min) (point-max))))))


(ert-deftest seqel-isearch-mangle-str-test ()
  :tags '(util)
  (let ((cases '(("mR" . "m[\t\n .-]*R")
                 ("aTGc" . "a[\t\n .-]*T[\t\n .-]*G[\t\n .-]*c"))))
    (dolist (test cases)
      (should
       (equal (seqel-isearch-mangle-str (car test))
              (cdr test))))))


(provide 'seqel-util-test)
