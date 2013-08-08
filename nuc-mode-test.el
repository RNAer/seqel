;;; nuc-mode-test.el --- a unit test for nuc-mode.el

(require 'nuc-mode)
(require 'ert)


(ert-deftest nuc-whr-test ()
  :tags '(nuc-mode)
  (let ((cases '(("TGATTCAAGCATTCGATC" . 1.6)
                 ("GGGTGCCCCCAAAATATT" . 7.25))))
    (with-temp-buffer
      (dolist (test cases)
        (insert (car test))
        (set-mark (point-min))
        (goto-char (point-max))
        (should (equal (call-interactively 'nuc-whr)
                       (cdr test)))
        (delete-region (point-min) (point-max))))))


(ert-deftest nuc-p-test ()
  :tags '(nuc-mode))

(ert-deftest rna-p-test ()
  :tags '(nuc-mode))

(ert-deftest dna-p-test ()
  :tags '(nuc-mode))

(ert-deftest 2rna-test ()
  :tags '(nuc-mode))

(ert-deftest 2dna-test ()
  :tags '(nuc-mode))

(ert-deftest nuc-paint-region-test ()
  :tags '(nuc-mode))

(ert-deftest nuc-complement-test ()
  :tags '(nuc-mode))

(ert-deftest nuc-rc-test ()
  :tags '(nuc-mode)
  (let ((cases '((should "acgtmrwsykvhdbnACGTMRWSYKVHDBN" "NVHDBMRSWYKACGTnvhdbmrswykacgt" nil)
                 (should "acgumrwsykvhdbnACGUMRWSYKVHDBN" "NVHDBMRSWYKACGUnvhdbmrswykacgu" t)))
        tmp
        func)
    (with-temp-buffer
      (dolist (test cases)
        (insert (nth 1 test))
        (set-mark (point-min))
        (goto-char (point-max))
        (if (nth 3 test)
            (setq current-prefix-arg '(4)))
        (call-interactively 'nuc-rc)
        (setq tmp (buffer-string))
        (eval `(,(nth 0 test) (equal tmp (nth 2 test))))
        (delete-region (point-min) (point-max))))))

(ert-deftest nuc-base-summary-test ()
  :tags '(nuc-mode))
