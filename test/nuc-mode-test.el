;;; nuc-mode-test.el --- a unit test for nuc-mode.el

(require 'nuc-mode)
(require 'seq)
(require 'genetic-code)
(require 'ert)


(ert-deftest nuc-whr-test ()
  ;; the tags is used to group the tests together.
  :tags '(nuc-mode)
  (let ((cases '(("TGATTCAAGCATTCGATC" . 1.6)
                 ("GGGTGCCCCCAAAATATT" . 7.25))))
    (with-temp-buffer ; create a temp buffer
      (dolist (test cases)
        (insert (car test))
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
  (let ((cases '((1 "acgtmrwsykvhdbnACGTMRWSYKVHDBN" nil "tgcakywsrmbdhvnTGCAKYWSRMBDHVN") ; for should
                 (1 "acgumrwsykvhdbnACGUMRWSYKVHDBN" t   "ugcakywsrmbdhvnUGCAKYWSRMBDHVN")
                 (2 "acug" nil)     ; for should-error
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
        (cond ((= type-test 1)
               (call-interactively 'nuc-complement)
               (setq tmp (buffer-string))
               (should (equal tmp (nth 3 test))))
              ((= type-test 2)
               (should-error (call-interactively 'nuc-complement))))
        (delete-region (point-min) (point-max))))))


(ert-deftest nuc-rc-test ()
  :tags '(nuc-mode)
  ;; (should/should-error, test case, DNA/RNA, result)
  (let ((cases '((1 "acgtmrwsykvhdbnACGTMRWSYKVHDBN" nil "NVHDBMRSWYKACGTnvhdbmrswykacgt") ; for should
                 (1 "acgumrwsykvhdbnACGUMRWSYKVHDBN" t   "NVHDBMRSWYKACGUnvhdbmrswykacgu")
                 (2 "acug" nil)     ; for should-error
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
        (cond ((= type-test 1)
               (call-interactively 'nuc-rc)
               (setq tmp (buffer-string))
               (should (equal tmp (nth 3 test))))
              ((= type-test 2)
               (should-error (call-interactively 'nuc-rc))))
        (delete-region (point-min) (point-max))))))

;; (defun foo ()
;;   (let ((cases '(("acgt" .
;;                   ((?a . 1) (?c . 1) (?g . 1) (?t . 1))))))
;;     (with-temp-buffer
;;       (dolist (test cases)
;;         (insert (car test))
;;         (maphash (lambda (x y) (princ (format "%c:%d " x y) t))
;;                  (region-summary (point-min) (point-max) nuc-base-regexp))
;;         (maphash (lambda (x y) (princ (format "%c:%d " x y) t))
;;                  (hash-alist (cdr test)))
;;         (delete-region (point-min) (point-max))))))

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
         (equal (region-summary (point-min) (point-max) nuc-base-regexp)
                (hash-alist (cdr test))))
        (delete-region (point-min) (point-max))))))


(ert-deftest nuc-translate-test ()
  :tags '(nuc-mode)
  ;; http://in-silico.net/tools/biology/sequence_conversion
  (let ((cases '(("accatttcm mtc" . "TISX")))
        nuc tmp)
    (with-temp-buffer
      (dolist (test cases)
        (setq nuc (car test))
        (insert nuc)
        (set-mark (point-min))
        (goto-char (point-max))
        (call-interactively 'nuc-translate)
        (setq tmp (buffer-string))
        (should (equal tmp (cdr test)))
        (delete-region (point-min) (point-max))))))

(provide 'nuc-mode-test)

;;; nuc-mode-test.el ends here
