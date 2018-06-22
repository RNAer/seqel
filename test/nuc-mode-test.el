;;; nuc-mode-test.el --- a unit test for nuc-mode.el

(require 'nuc-mode)
(require 'seq)
(require 'genetic-code)
(require 'ert)

(ert-deftest nuc-move-test ()
  ;; the tags is used to group the tests together.
  :tags '(nuc-mode)
  (let ((cases '(("ATGC" 1 2)
                 (" ATGC" 1 3)
                 ("A TGC" 2 4))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (nth 0 test))
        (goto-char 0)
        ;; set the numeric prefix
        (setq current-prefix-arg (nth 1 test))
        (call-interactively 'nuc-move-forward)
        (should (equal (point) (nth 2 test)))
        ;; important to clean up the buffer
        (delete-region (point-min) (point-max))))))

(ert-deftest nuc-delete-test ()
  ;; the tags is used to group the tests together.
  :tags '(nuc-mode)
  ;; seq, set initial poin position (1-based), func args, expected res
  (let ((cases '(("ATGC" 1 1 "TGC")
                 (" ATGC" 1 1 "TGC")
                 ("ATGC" 3 -1 "AGC")
                 ("A TG C" 1 2 "G C"))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (nth 0 test))
        (goto-char (nth 1 test))
        ;; set the numeric prefix
        (setq current-prefix-arg (nth 2 test))
        (call-interactively 'nuc-delete-forward)
        (should (equal (buffer-string) (nth 3 test)))
        ;; important to clean up the buffer
        (delete-region (point-min) (point-max))))))

(ert-deftest nuc-whr-test ()
  ;; the tags is used to group the tests together.
  :tags '(nuc-mode)
  (let ((cases '(("TGATTCAAGCATTCGATC" . 1.6)
                 ("GGGTGCCCCCAAAATATT" . 7.25))))
    (with-temp-buffer ; create a temp buffer
      (dolist (test cases)
        (insert (car test))
        ;; create the mark region
        (set-mark (point-min))
        (goto-char (point-max))
        (should (equal (call-interactively 'nuc-whr)
                       (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest nuc-count-test ()
  :tags '(nuc-mode)
  (let ((cases '(("acgutmrwsykvhdbnACGUTMRWSYKVHDBN" . 32)
                 ("abc12345"    .       nil))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (set-mark (point-min))
        (goto-char (point-max))
        (should (equal (call-interactively 'nuc-count)
                       (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest nuc-rna-p-test ()
  :tags '(nuc-mode)
  (let ((cases '(("acgumrwsykvhdbnACGUMRWSYKVHDBN" . t)
                 ("abc12345"    .       nil)
                 ("acgtmrwsykvhdbnACGTMRWSYKVHDBN" . nil))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (set-mark (point-min))
        (goto-char (point-max))
        (if (cdr test)
            (should (call-interactively 'nuc-rna-p))
          (should-not (call-interactively 'nuc-rna-p)))
        (delete-region (point-min) (point-max))))))


(ert-deftest nuc-dna-p-test ()
  :tags '(nuc-mode)
  (let ((cases '(("acgumrwsykvhdbnACGUMRWSYKVHDBN" . nil)
                 ("abc12345"    .       nil)
                 ("acgtmrwsykvhdbnACGTMRWSYKVHDBN" . t))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (set-mark (point-min))
        (goto-char (point-max))
        (if (cdr test)
            (should (call-interactively 'nuc-dna-p))
          (should-not (call-interactively 'nuc-dna-p)))
        (delete-region (point-min) (point-max))))))


(ert-deftest nuc-2rna-test ()
  :tags '(nuc-mode)
  (let ((cases '(("acgtmrwsykvhdbnACGTMRWSYKVHDBN" . "acgumrwsykvhdbnACGUMRWSYKVHDBN"))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (set-mark (point-min))
        (goto-char (point-max))
        (call-interactively 'nuc-2rna)
        (should (equal (cdr test) (buffer-string)))
        (delete-region (point-min) (point-max))))))


(ert-deftest nuc-2dna-test ()
  :tags '(nuc-mode)
  (let ((cases '(("acgumrwsykvhdbnACGUMRWSYKVHDBN" . "acgtmrwsykvhdbnACGTMRWSYKVHDBN"))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (set-mark (point-min))
        (goto-char (point-max))
        (call-interactively 'nuc-2dna)
        (should (equal (cdr test) (buffer-string)))
        (delete-region (point-min) (point-max))))))


(ert-deftest nuc-complement-test ()
  :tags '(nuc-mode)
  (let ((cases '(("Atg C" . "Tac G")
                 ("acgtmrwsykvhdbnACGTMRWSYKVHDBN" . "tgcakywsrmbdhvnTGCAKYWSRMBDHVN")
                 ("acgumrwsykvhdbnACGUMRWSYKVHDBN" . "ugcakywsrmbdhvnUGCAKYWSRMBDHVN"))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (set-mark (point-min))
        (goto-char (point-max))
        (call-interactively 'nuc-complement)
        (should (equal (buffer-string) (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest nuc-rc-test ()
  :tags '(nuc-mode)
  ;; (should, test case, result)
  (let ((cases '(("A1 a" . "t 1T")
                 ("acgtmrwsykvhdbnACGTMRWSYKVHDBN" . "NVHDBMRSWYKACGTnvhdbmrswykacgt")
                 ("acgumrwsykvhdbnACGUMRWSYKVHDBN" . "NVHDBMRSWYKACGUnvhdbmrswykacgu"))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (set-mark (point-min))
        (goto-char (point-max))
        (call-interactively 'nuc-rc)
        (should (equal (buffer-string) (cdr test)))
        ;; check the cursor has not moved
        ;; (should (equal (point) (point-max)))
        (delete-region (point-min) (point-max))))))


(ert-deftest nuc-summary-test ()
  :tags '(nuc-mode)
  (let ((cases '(("acgtmrwsykvhdbnACGTMRWSYKVHDBN.- " .
                  ((?a . 1) (?c . 1) (?g . 1) (?t . 1)
                   (?m . 1) (?r . 1) (?w . 1) (?s . 1)
                   (?y . 1) (?k . 1) (?v . 1) (?h . 1)
                   (?d . 1) (?b . 1) (?n . 1)
                   (?A . 1) (?C . 1) (?G . 1) (?T . 1)
                   (?M . 1) (?R . 1) (?W . 1) (?S . 1)
                   (?Y . 1) (?K . 1) (?V . 1) (?H . 1)
                   (?D . 1) (?B . 1) (?N . 1))))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (should
         ;; `equal' can compare hash tables
         (hash-equal (region-summary (point-min) (point-max) nuc-base-regexp)
                (hash-alist (cdr test))))
        (delete-region (point-min) (point-max))))))


(ert-deftest nuc-translate-test ()
  :tags '(nuc-mode)
  ;; http://in-silico.net/tools/biology/sequence_conversion
  (let ((cases '(("accatttcm mtc" . "TISX"))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (set-mark (point-min))
        (goto-char (point-max))
        (call-interactively 'nuc-translate)
        (should (equal (buffer-string) (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest seq-isearch-mangle-str-test ()
  :tags '(nuc-mode)
  (let ((cases '(
                 ("mR" . "[ac][\t\n .-]*[AG]")
                 ("aTGc" . "[a][\t\n .-]*[T][\t\n .-]*[G][\t\n .-]*[c]"))))
    (dolist (test cases)
      (should
       (equal (seq-isearch-mangle-str (car test))
              (cdr test))))))


(provide 'nuc-mode-test)

;;; nuc-mode-test.el ends here
