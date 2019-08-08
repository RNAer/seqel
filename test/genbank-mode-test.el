;;; nuc-mode-test.el --- a unit test for nuc-mode.el

(require 'genbank-mode)
(require 'ert)


(ert-deftest genbank-count-test ()
  :tags '(genbank-mode)
  (let ((cases '(("" . 0)
                 ("LOCUS       ABCM01000001          510929 bp    DNA     linear   BCT 20-JUN-2007
//

LOCUS       ABCM01000001          510929 bp    DNA     linear   BCT 20-JUN-2007
//
"
                  . 2))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (should (equal (genbank-count) (cdr test)))
        (delete-region (point-min) (point-max))))))


(provide 'genbank-mode-test)
;; genbank-mode-test.el ends here
