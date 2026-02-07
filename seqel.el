;;; seqel.el --- biological sequence manipulation with emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Zech Xu

;; Author: Zech Xu
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))
;; License: BSD-3
;; URL: https://github.com/RNAer/seqel

;;; Commentary:

;; Entry point for the seqel package.  Loads fasta and genbank modes
;; for biological sequence manipulation.

;;; Code:

(require 'seqel-fasta-mode)

(require 'seqel-genbank-mode)


(provide 'seqel)

;;; seqel.el ends here
