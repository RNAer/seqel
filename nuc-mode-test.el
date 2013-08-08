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
  (let ((cases '((1 "acgtmrwsykvhdbnACGTMRWSYKVHDBN" nil "NVHDBMRSWYKACGTnvhdbmrswykacgt")
                 (1 "acgumrwsykvhdbnACGUMRWSYKVHDBN" t   "NVHDBMRSWYKACGUnvhdbmrswykacgu")
                 (2 "acug" nil)     ; error cases
                 (2 "actg" t)))
        type-test
        tmp
        func)
    (with-temp-buffer
      (dolist (test cases)
        (setq type-test (nth 0 test))
        (insert (nth 1 test))
        (set-mark (point-min))
        (goto-char (point-max))
        (if (nth 2 test)
            (setq current-prefix-arg '(4))
          (setq current-prefix-arg nil))
        (cond ((equal type-test 1)
               (call-interactively 'nuc-rc)
               (setq tmp (buffer-string))
               (should (equal tmp (nth 3 test))))
              ((equal type-test 2)
               (should-error (call-interactively 'nuc-rc))))
        (delete-region (point-min) (point-max))))))

(ert-deftest nuc-base-summary-test ()
  :tags '(nuc-mode))
