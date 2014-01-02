;;; unit-test.el --- run the unit-test in the test/ dir


(require 'nuc-mode-test)
(ert '(tag nuc-mode))

(require 'pro-mode-test)
(ert '(tag pro-mode))

(require 'fasta-mode-test)
(ert '(tag fasta-mode))
