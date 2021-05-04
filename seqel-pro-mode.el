;;; seqel-pro-mode.el --- A minor mode for editing protein sequences

;; Copyright (C) 2021  Zech Xu

;; Author: Zech Xu
;; Version: 1.0
;; License: BSD-3
;; URL: https://github.com/RNAer/seqel

;;; Commentary:

;;  A minor mode that provides collection of functions for editing protein sequences.


;;; Code:


(require 'seqel)


(defvar seqel-pro-mode-hook nil
  "*Hook to setup `seqel-pro-mode'.")


(defvar seqel-pro-aa-alist
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


(defvar seqel-pro-alphabet-set
  (let ((alphabet-set (make-hash-table :test 'eq :size (* 2 (length seqel-pro-aa-alist)))))
    (dolist (l seqel-pro-aa-alist)
      (puthash (downcase (car l)) t alphabet-set)
      (puthash (car l) t alphabet-set))
    alphabet-set)
  "The set of all legal alphabets in protein sequences.

This is a hash table: keys are char (including both lower case
and upper case) and values are t. It serves like a set object
similar in Python language.")


(defvar seqel-pro-aa-acidic "DE")
(defvar seqel-pro-aa-basic "RHK")
(defvar seqel-pro-aa-hydrophobic "AHILMFVPGWY")
(defvar seqel-pro-aa-hydrophilic "")
(defvar seqel-pro-aa-amphipathic "")


(defvar seqel-pro-aa-mw
  (let ((mw-vec (make-vector 256 nil))
        aa mw)
    (dolist (element seqel-pro-aa-alist)
      (setq aa (car element))
      (setq mw (nth 2 element))
      (aset mw-vec aa mw)
      (aset mw-vec (downcase aa) mw))
    mw-vec)
  "A vector of amino acid molecular weights in Dalton.")

(defvar seqel-pro-aa-1-vec
  (let ((vec (make-vector 256 nil))
        aa1 aa3)
    (dolist (element seqel-pro-aa-alist)
      (setq aa1 (car element))
      (setq aa3 (nth 1 element))
      (aset vec aa1 aa3)
      (aset vec (downcase aa1) aa3))
    vec)
  "A vector of 3-letter amino acid codes.

It is used to convert 1-letter codes to 3-letter codes.")

(defvar seqel-pro-aa-3-hash
  (let ((my-hash (make-hash-table :test 'equal :size (length seqel-pro-aa-alist))))
    (dolist (element seqel-pro-aa-alist)
      (puthash (nth 1 element) (car element) my-hash))
    my-hash)
  "A hash table with 3-letter code as key and 1-letter code as value.

It is used to convert 3-letter codes to 1-letter codes.")


(defun seqel-pro-weight (beg end)
  "Return molecular weight of the region BEG and END or the current line."
  (interactive (seqel-region-or-line))
  (let ((sum-mw 0) (times (- end beg)) char mw)
    (save-excursion
      (goto-char beg)
      (dotimes (x times)
        (setq char (char-after))
        (setq mw (aref seqel-pro-aa-mw char))
        (cond (mw
               (setq sum-mw (+ sum-mw mw)))
              ((not (gethash char seqel-cruft-set))
               (error "Ambiguous or illegal char %s at position line %d column %d"
                      char (line-number-at-pos) (current-column))))
        (forward-char)))
    (message "The molecular weight is %.2f" sum-mw)
    sum-mw))


(defun seqel-pro-1-2-3 (beg end)
  "Convert 1-letter IUPAC code to 3-letter IUPAC code.

BEG and END defines the region to operate on."
  (interactive (seqel-region-or-line))
  (condition-case err
      (let ((times (- end beg)) char)
        (goto-char beg)
        (dotimes (x times)
          (setq char (char-after))
          (cond ((gethash char seqel-pro-alphabet-set)
                 (insert (aref seqel-pro-aa-1-vec (char-after)))
                 (delete-char 1))
                (t
                 (error "Ambiguous or illegal amino acid letter %c at line %d column %d"
                        char (line-number-at-pos) (current-column))))))
    ((debug error)
     (primitive-undo 1 buffer-undo-list)
     (error "%s" (error-message-string err)))))


(defun seqel-pro-3-2-1 (beg end)
  "Convert 3-letter IUPAC code to 1-letter IUPAC code.

Currently it only converts 3-letter codes without any characters
separating them.

Interactively, BEG and END are the begin and end of the active
region or the current line if no region is active."
  (interactive (seqel-region-or-line))
  (condition-case err
      (let ((times (/ (- end beg) 3))
            code letter)
        (goto-char beg)
        (dotimes (x times)
          (setq code (buffer-substring (point) (+ 3 (point))))
          (setq letter (gethash code seqel-pro-aa-3-hash))
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
(defun seqel-pro-move-forward (count)
  "Move forward COUNT number of amino acids.

See `seqel-nuc-move-forward'"
  (interactive "p")
  (seqel-forward-char count seqel-pro-alphabet-set))

(defun seqel-pro-move-backward (count)
  "Move backward COUNT number of amino acides, similar to `seqel-pro-move-forward'."
  (interactive "p")
  ;; (proceed-char-repeatedly count 'backward-char))
  (seqel-forward-char (- count) seqel-pro-alphabet-set))

;;; delete
(defun seqel-pro-delete-forward (count)
  "Delete COUNT number of amino acids starting from the point.

See also `nuc-delete-forward'."
  (interactive "p")
  (let ((pos (point)))
    (seqel-forward-char count seqel-pro-alphabet-set)
    (delete-region pos (point))))


(defun seqel-pro-delete-backward (count)
  "Delete backward COUNT number of AA from the point.

See also `seqel-nuc-delete-backward'."
  (interactive "p")
  (let ((pos (point)))
    (seqel-forward-char (- count) seqel-pro-alphabet-set)
    (delete-region pos (point))))


(defun seqel-pro-count (beg end)
  "Count the amino acid in the region or in the current line).

Return the count if the region contains only legal amino acid
characters, including `seqel-pro-alphabet-set', `seqel-cruft-set';
otherwise return nil and report the location of the invalid
characters in the echo region.

Interactively, BEG and END are the begin and end of the active
region or the current line if no region is active."
  (interactive (seqel-region-or-line))
  (let ((length (seqel-count beg end seqel-pro-alphabet-set)))
    (and length
         (called-interactively-p 'interactive)
         (message "Amino acid count: %d" length))
    length))


(defalias 'seqel-pro-p 'seqel-pro-count)


(defun seqel-pro-summary (beg end)
  "Summarize the frequencies of amino acids in the region or the current line.

Interactively, BEG and END are the begin and end of the active
region or the current line if no region is active.

See also `seqel-summary'."
  (interactive (seqel-region-or-line))
  (seqel-summary beg end seqel-pro-alphabet-set))

;; define aa faces belonging to pro-aa-face group
(defvar seqel-pro-aa-colors
  (seqel--zip #'(lambda (x y) (cons (car x) y)) seqel-pro-aa-alist seqel-color-pairs-cycle)
  "Background and foreground colors for each IUPAC bases.

This is a list of lists.  For each inner list, it contains 3 atoms:
a nuc base in char type, hex-code colors for foreground and background")


(mapc (lambda (elem)
        (let ((l (format "%c" (nth 0 elem)))
              (f (nth 2 elem))
              (b (nth 1 elem)))
          (eval (macroexpand `(seqel--def-char-face ,l ,b ,f "pro-aa-face")))))
      seqel-pro-aa-colors)

;;;###autoload
(defun seqel-pro-paint (beg end &optional case)
  "Color the protein sequence from BEG to END.

If the CASE is nil, upcase and lowercase base chars will be colored the same;
otherwise, not.  See `seqel-paint' for details."
  (interactive "r\nP")
  (if (not (use-region-p))
      (setq beg (line-beginning-position)
            end (line-end-position)))
  (seqel-paint beg end "pro-aa-face" case))

;;;###autoload
(defalias 'seqel-pro-unpaint 'seqel-unpaint
  "Uncolor the region of protein sequence.

This is an alias to `seqel-unpaint'.")


(defvar seqel-pro-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-p\C-f"  'seqel-pro-move-forward)
    (define-key map "\C-c\C-p\C-b"  'seqel-pro-move-backward)
    (define-key map "\C-c\C-p\C-w"  'seqel-pro-weight)
    (define-key map "\C-c\C-p\C-s"  'seqel-pro-summary)
    map)
  "Keymap for 'seqel-pro-mode' minor mode.")


(define-minor-mode seqel-pro-mode
  "Protein mode

It should be not enabled with `nuc-mode' at the same time."
  :init-value nil
  ;; the name, a string, to show in the modeline
  :lighter " protein"
  :keymap seqel-pro-mode-map
  (setq-local seqel-isearch-p t)
  (run-hooks 'seqel-pro-mode-hook))


(provide 'seqel-pro-mode)

;;; seqel-pro-mode.el ends here
