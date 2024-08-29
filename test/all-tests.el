;;; all-tests.el --- all the tests.  -*- lexical-binding: t; -*-

;; If the directory happens to have both compiled and uncompiled
;; version, prefer to use the newer (typically the uncompiled) version.
(setq load-prefer-newer t)

(require 'seqel-test)
(require 'seqel-nuc-mode-test)
(require 'seqel-pro-mode-test)
(require 'seqel-fasta-mode-test)
(require 'seqel-genbank-mode-test)

;; (load-file "seqel-test.el")
;; (ert '(tag seq))

;; (load-file "seqel-nuc-mode-test.el")
;; (ert '(tag nuc-mode))

;; (load-file "seqel-pro-mode-test.el")
;; (ert '(tag pro-mode))

;; (load-file "seqel-fasta-mode-test.el")
;; (ert '(tag fasta-mode))

;; (load-file "seqel-genbank-mode-test.el")
;; (ert '(tag genbank-mode))
