;;; pro-mode.el --- a minor mode for editing protein sequences   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Zech Xu

;; Author: Zech Xu
;; Version: 1.0
;; Keywords: DNA, RNA, protein
;; License: BSD-3

;;; Commentary:
;;  * It should be not enabled with nuc-mode at the same time.


;;; Code:


(require 'bioseq)


(defvar pro-aa-alist
  ;; put a question mark before the char will evaluate it to digit ascii code
  '((?A  "Ala"  71.09   ?a)
    (?B  "Asx"  114     ?n ?d)  ; Asn Asp
    (?C  "Cys"  103.15  ?c)
    (?D  "Asp"  115.09  ?d)
    (?E  "Glu"  129.12  ?e)
    (?F  "Phe"  147.18  ?f)
    (?G  "Gly"  57.05   ?g)
    (?H  "His"  137.14  ?h)
    (?I  "Ile"  113.16  ?i)
    (?J  "Xle"  113.16  ?i ?l)  ;Leu Ile
    (?K  "Lys"  128.17  ?k)
    (?L  "Leu"  113.16  ?l)
    (?M  "Met"  131.19  ?m)
    (?N  "Asn"  114.11  ?n)
    (?P  "Pro"  97.12   ?p)
    (?Q  "Gln"  128.14  ?q)
    (?R  "Arg"  156.19  ?r)
    (?S  "Ser"  87.08   ?s)
    (?T  "Thr"  101.11  ?t)
    (?V  "Val"  99.14   ?v)
    (?W  "Trp"  186.21  ?w)
    (?X  "Xaa"  92      ?.)  ; unknown aa; set it weight to average weight
    (?Y  "Tyr"  163.18  ?y)
    (?Z  "Glx"  128     ?e ?q)) ; Glu Gln
  "*A association list of 1-letter, 3-letter IUPAC codes and molecular weights.

This is the molecular weight with H2O subtracted.

For each inner list, the first element is allowed AA; the second
element is the three-letter code of the first, and the last is
the molecular weight.  for the first.")

;;;;; END OF USER CUSTOMIZABLE VARIABLES


(defvar pro-alphabet-set
  (let ((alphabet-set (make-hash-table :test 'eq :size (* 2 (length pro-aa-alist)))))
    (dolist (l pro-aa-alist)
      (puthash (downcase (car l)) t alphabet-set)
      (puthash (car l) t alphabet-set))
    alphabet-set)
  "The set of all legal alphabets in protein sequences.

This is a hash table: keys are char (including both lower case
and upper case) and values are `t'. It serves like a set object
similar in Python language.")


(defvar pro-aa-acidic "DE")
(defvar pro-aa-basic "RHK")
(defvar pro-aa-hydrophobic "AHILMFVPGWY")
(defvar pro-aa-hydrophilic "")
(defvar pro-aa-amphipathic "")


(defvar pro-aa-mw
  (let ((mw-vec (make-vector 256 nil))
        aa mw)
    (dolist (element pro-aa-alist)
      (setq aa (car element))
      (setq mw (nth 2 element))
      (aset mw-vec aa mw)
      (aset mw-vec (downcase aa) mw))
    mw-vec)
  "A vector of amino acid molecular weights in Dalton.")

(defvar pro-aa-1-vec
  (let ((vec (make-vector 256 nil))
        aa1 aa3)
    (dolist (element pro-aa-alist)
      (setq aa1 (car element))
      (setq aa3 (nth 1 element))
      (aset vec aa1 aa3)
      (aset vec (downcase aa1) aa3))
    vec)
  "A vector of 3-letter amino acid codes.

It is used to convert 1-letter codes to 3-letter codes.")

(defvar pro-aa-3-hash
  (let ((my-hash (make-hash-table :test 'equal :size (length pro-aa-alist))))
    (dolist (element pro-aa-alist)
      (puthash (nth 1 element) (car element) my-hash))
    my-hash)
  "A hash table with 3-letter code as key and 1-letter code as value.

It is used to convert 3-letter codes to 1-letter codes.")


(defun pro-weight (beg end)
  "Return molecular weight of the region BEG and END or the current line."
  (interactive-region-or-line)
  (let ((sum-mw 0) (times (- end beg)) char mw)
    (save-excursion
      (goto-char beg)
      (dotimes (x times)
        (setq char (char-after))
        (setq mw (aref pro-aa-mw char))
        (cond (mw
               (setq sum-mw (+ sum-mw mw)))
              ((not (gethash char bioseq-cruft-set))
               (error "Ambiguous or illegal char %s at position line %d column %d"
                      char (line-number-at-pos) (current-column))))
        (forward-char)))
    (message "The molecular weight is %.2f" sum-mw)
    sum-mw))


(defun pro-1-2-3 (beg end)
  "Convert 1-letter IUPAC code to 3-letter IUPAC code.

BEG and END defines the region to operate on."
  (interactive-region-or-line)
  (condition-case err
      (let ((times (- end beg)) char)
        (goto-char beg)
        (dotimes (x times)
          (setq char (char-after))
          (cond ((gethash char pro-alphabet-set)
                 (insert (aref pro-aa-1-vec (char-after)))
                 (delete-char 1))
                (t
                 (error "Ambiguous or illegal amino acid letter %c at line %d column %d"
                        char (line-number-at-pos) (current-column))))))
    ((debug error)
     (primitive-undo 1 buffer-undo-list)
     (error "%s" (error-message-string err)))))


(defun pro-3-2-1 (beg end)
  "Convert 3-letter IUPAC code to 1-letter IUPAC code.

Currently it only converts 3-letter codes without any characters
separating them."
  (interactive-region-or-line)
  (condition-case err
      (let ((times (/ (- end beg) 3))
            code letter)
        (goto-char beg)
        (dotimes (x times)
          (setq code (buffer-substring (point) (+ 3 (point))))
          (setq letter (gethash code pro-aa-3-hash))
          (if letter
              (insert-char letter)
            (error "Unknown 3-letter amino acid code '%s' at position %d"
                   code (point)))
          (delete-char 3)))
    ;; return to the original state if error is met.
    ((debug error)
     (primitive-undo 1 buffer-undo-list)
     (error "%s" (error-message-string err)))))


;;;###autoload
(defun pro-move-forward (count)
  "Move forward COUNT number of amino acids.

See `nuc-move-forward'"
  (interactive "p")
  (bioseq-forward-char count pro-alphabet-set))

(defun pro-move-backward (count)
  "Move backward COUNT number of amino acides, similar to `pro-move-forward'."
  (interactive "p")
  ;; (proceed-char-repeatedly count 'backward-char))
  (bioseq-forward-char (- count) pro-alphabet-set))

;;; delete
(defun pro-delete-forward (count)
  "Delete COUNT number of amino acids starting from the point.

See also `nuc-delete-forward'."
  (interactive "p")
  (let ((pos (point)))
    (bioseq-forward-char count pro-alphabet-set)
    (delete-region pos (point))))


(defun pro-delete-backward (count)
  "Delete backward COUNT number of AA from the point.

See also `nuc-delete-backward'."
  (interactive "p")
  (let ((pos (point)))
    (bioseq-forward-char (- count) pro-alphabet-set)
    (delete-region pos (point))))


(defun pro-count (beg end)
  "Count the amino acid in the region or in the current line).

Return the count if the region contains only legal amino acid
characters, including `pro-alphabet-set', `bioseq-cruft-set';
otherwise return nil and report the location of the invalid
characters in the echo region."
  (interactive-region-or-line)
  (let ((length (bioseq-count beg end pro-alphabet-set)))
    (and length
         (called-interactively-p 'interactive)
         (message "Amino acid count: %d" length))
    length))


(defalias 'pro-p 'pro-count)


(defun pro-summary (beg end)
  "Summarize the frequencies of amino acids in the region BEG and END or the current line.

See also `bioseq-summary'."
  (interactive-region-or-line)
  (bioseq-summary beg end pro-alphabet-set))

;; define aa faces belonging to pro-aa-face group
(defvar pro-aa-colors
  (bioseq--zip #'(lambda (x y) (cons (car x) y)) pro-aa-alist color-pairs-cycle)
  "Background and foreground colors for each IUPAC bases.

This is a list of lists.  For each inner list, it contains 3 atoms:
a nuc base in char type, hex-code colors for foreground and background")


(mapc (lambda (elem)
        (let ((l (format "%c" (nth 0 elem)))
              (f (nth 2 elem))
              (b (nth 1 elem)))
          (eval (macroexpand `(def-char-face ,l ,b ,f "pro-aa-face")))))
      pro-aa-colors)

;;;###autoload
(defun pro-paint (beg end &optional case)
  "Color the protein sequence from BEG to END.

If the CASE is nil, upcase and lowercase base chars will be colored the same;
otherwise, not.  See `bioseq-paint' for details."
  (interactive "r\nP")
  (if (not (use-region-p))
      (setq beg (line-beginning-position)
            end (line-end-position)))
  (bioseq-paint beg end "pro-aa-face" case))

;;;###autoload
(defalias 'pro-unpaint 'bioseq-unpaint
  "Uncolor the region of protein sequence.

This is an alias to `bioseq-unpaint'.")


(defvar pro-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-f"  'pro-move-forward)
    (define-key map "\C-c\C-b"  'pro-move-backward)
    (define-key map "\C-c\C-w"  'pro-weight)
    ;; (define-key map "\C-c\C-s"  'pro-summary)
    map)
  "Keymap for `pro-mode'.")


(define-minor-mode pro-mode
  "Protein mode

It should be not enabled with `nuc-mode' at the same time."
  :init-value nil
  ;; the name, a string, to show in the modeline
  :lighter " protein"
  :keymap pro-mode-map
  :global t)


(provide 'pro-mode)

;;; pro-mode.el ends here
