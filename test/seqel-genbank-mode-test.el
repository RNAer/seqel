;;; seqel-genbank-mode-test.el --- Tests for seqel-genbank-mode. -*- lexical-binding: t; -*-

(require 'seqel-genbank-mode)


(ert-deftest seqel-genbank-forward-test ()
  :tags '(genbank-mode)
  (let ((cases '(("" 1 1)
                 ("LOCUS       ABCM01000001          510929 bp    DNA     linear   BCT 20-JUN-2007
//

LOCUS       ABCM01000001          510929 bp    DNA     linear   BCT 20-JUN-2007
//
"
                1  4))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (nth 0 test))
        (goto-char (point-min))
        (setq current-prefix-arg (nth 1 test))
        (call-interactively 'seqel-genbank-forward)
        (should (equal (line-number-at-pos) (nth 2 test)))
        (delete-region (point-min) (point-max))))))

(ert-deftest seqel-genbank-backward-test ()
  :tags '(genbank-mode)
  (let ((cases '(("" 1 1)
                 ("LOCUS       ABCM01000001          510929 bp    DNA     linear   BCT 20-JUN-2007
//

LOCUS       ABCM01000001          510929 bp    DNA     linear   BCT 20-JUN-2007
//
"
                  1 4))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (nth 0 test))
        (setq current-prefix-arg (nth 1 test))
        (call-interactively 'seqel-genbank-backward)
        (should (equal (line-number-at-pos) (nth 2 test)))
        (delete-region (point-min) (point-max))))))

(ert-deftest seqel-genbank-first-test ()
  :tags '(genbank-mode)
  (let ((cases '(""
                  "LOCUS       ABCM01000001          510929 bp    DNA     linear   BCT 20-JUN-2007
//

LOCUS       ABCM01000001          510929 bp    DNA     linear   BCT 20-JUN-2007
//
")))
    (with-temp-buffer
      (dolist (test cases)
        (insert test)
        (call-interactively 'seqel-genbank-first)
        (should (equal (point-min) (point)))
        (delete-region (point-min) (point-max))))))

(ert-deftest seqel-genbank-last-test ()
  :tags '(genbank-mode)
  (let ((cases '(("" . 1)
                 ("LOCUS       ABCM01000001          510929 bp    DNA     linear   BCT 20-JUN-2007
//

LOCUS       ABCM01000001          510929 bp    DNA     linear   BCT 20-JUN-2007
//
" . 4))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (call-interactively 'seqel-genbank-last)
        (should (equal (line-number-at-pos) (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest seqel-genbank-count-test ()
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
        (should (equal (seqel-genbank-count) (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest seqel-genbank-delete-test ()
  :tags '(genbank-mode)
  (let ((cases '(("" . "")
                 ("LOCUS       ABCM01000001          510929 bp    DNA     linear   BCT 20-JUN-2007
//
LOCUS       ABCM01000002          510929 bp    DNA     linear   BCT 20-JUN-2007
//
"
                  . "LOCUS       ABCM01000002          510929 bp    DNA     linear   BCT 20-JUN-2007
//
"))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (goto-char (point-min))
        (call-interactively 'seqel-genbank-delete)
        (setq tmp (buffer-string))
        (should (equal tmp (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest seqel-genbank-2-fasta-test ()
  :tags '(genbank-mode)
  (let ((cases '(; ("" . "")
                 ("LOCUS       ABCM01000001          510929 bp    DNA     linear   BCT 20-JUN-2007
ORIGIN
        1 tacggtaaca gaagtacttt ctaaacacgg tattgaccag cgtgtgctca accccgacaa
       61 tctcaagg
//
"
                  . ">ABCM01000001          510929 bp    DNA     linear   BCT 20-JUN-2007
TACGGTAACAGAAGTACTTTCTAAACACGGTATTGACCAGCGTGTGCTCAACCCCGACAATCTCAAGG
"))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (goto-char (point-min))
        (call-interactively 'seqel-genbank-2-fasta)
        (setq tmp (buffer-string))
        (should (equal tmp (cdr test)))
        (delete-region (point-min) (point-max))))))


(provide 'seqel-genbank-mode-test)
