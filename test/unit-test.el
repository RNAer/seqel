;;; unit-test.el --- run the unit-test interactively in emacs

(load-file "bioseq-test.el")
(ert '(tag seq))

(load-file "nuc-mode-test.el")
(ert '(tag nuc-mode))

(load-file "pro-mode-test.el")
(ert '(tag pro-mode))

(load-file "fasta-mode-test.el")
(ert '(tag fasta-mode))

(load-file "genbank-mode-test.el")
(ert '(tag genbank-mode))
