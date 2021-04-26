;;; unit-test.el --- run the unit-test interactively in emacs

(load-file "seqel-test.el")
(ert '(tag seq))

(load-file "seqel-nuc-mode-test.el")
(ert '(tag nuc-mode))

(load-file "seqel-pro-mode-test.el")
(ert '(tag pro-mode))

(load-file "seqel-fasta-mode-test.el")
(ert '(tag fasta-mode))

(load-file "seqel-genbank-mode-test.el")
(ert '(tag genbank-mode))
