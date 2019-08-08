;;; nuc-mode-test.el --- a unit test for nuc-mode.el

(require 'genbank-mode)
(require 'ert)


(ert-deftest genbank-count-test ()
  :tags '(fasta-mode)
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


(ert-deftest genbank-count-test ()
  :tags '(fasta-mode)
  (let ((cases '(("LOCUS       ABCM01000001          510929 bp    DNA     linear   BCT 20-JUN-2007
ORIGIN
        1 tacggtaaca gaagtacttt ctaaacacgg tattgaccag cgtgtgctca accccgacaa
       61 tctcaaggca ccggccggtt atgcctttga actgaaagag accacgacga cagctggcaa

//

LOCUS       ABCM01000002          510929 bp    DNA     linear   BCT 20-JUN-2007
//
        1 tacggtaaca gaagtacttt ctaaacacgg tattgaccag cgtgtgctca accccgacaa
       61 tctcaaggca ccggccggtt atgcctttga actgaaagag accacgacga

"
                  .
                  ">ABCM01000001
tacggtaaca gaagtacttt ctaaacacgg tattgaccag cgtgtgctca accccgacaa
tctcaaggca ccggccggtt atgcctttga actgaaagag accacgacga cagctggcaa
>ABCM01000002
tacggtaaca gaagtacttt ctaaacacgg tattgaccag cgtgtgctca accccgacaa
tctcaaggca ccggccggtt atgcctttga actgaaagag accacgacga
"))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (should (equal (genbank-count) (cdr test)))
        (delete-region (point-min) (point-max))))))


;; genbank-mode-test.el ends here
