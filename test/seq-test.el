(require 'seq)

(ert-deftest bioseq-summary-test ()
  :tags '(seq)
  (let ((cases '(("a1..- " .
                  ((?a . 1) (?1 . 1) (?. . 2) (?- . 1) (? . 1)))))
        obs)
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (setq obs (bioseq-summary (point-min) (point-max) ))
        (maphash (lambda (k v) (if (= 0 (gethash k obs)) (remhash k obs))) obs)
        (should
         ;; `equal' can not compare hash tables
         (hash-equal obs (hash-alist (cdr test))))
        (delete-region (point-min) (point-max))))))


(ert-deftest bioseq-isearch-mangle-str-test ()
  :tags '(seq)
  (let ((cases '(("mR" . "m[	- .-]*R")
                 ("aTGc" . "a[	- .-]*T[	- .-]*G[	- .-]*c"))))
    (dolist (test cases)
      (princ (bioseq-isearch-mangle-str (car test)))
      (should
       (equal (bioseq-isearch-mangle-str (car test))
              (cdr test))))))

