(require 'seq)

(ert-deftest seq-summary-test ()
  :tags '(seq)
  (let ((cases '(("a1..- " .
                  ((?a . 1) (?1 . 1) (?. . 2) (?- . 1) (? . 1)))))
        obs)
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (setq obs (seq-summary (point-min) (point-max) ))
        (maphash (lambda (k v) (if (= 0 (gethash k obs)) (remhash k obs))) obs)
        (should
         ;; `equal' can not compare hash tables
         (hash-equal obs (hash-alist (cdr test))))
        (delete-region (point-min) (point-max))))))


(ert-deftest seq-isearch-mangle-str-test ()
  :tags '(seq)
  (let ((cases '(("mR" . "m[\t\n .-]*R")
                 ("aTGc" . "a[\t\n .-]*T[\t\n .-]*G[\t\n .-]*c"))))
    (dolist (test cases)
      (should
       (equal (seq-isearch-mangle-str (car test))
              (cdr test))))))

