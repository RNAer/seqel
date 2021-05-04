;;; seqel-nuc-mode.el --- A minor mode for editing nucleic acid sequences

;; Copyright (C) 2021  Zech Xu

;; Author: Zech Xu
;; Version: 1.0
;; License: BSD-3
;; URL: https://github.com/RNAer/seqel

;;; Commentary:

;; A minor mode that provides collection of functions for editing DNA and RNA sequences.

;;

;;; Code:

(require 'seqel)
(require 'seqel-genetic-code)

;;;;;; USER CUSTOMIZABLE VARIABLES START HERE

(defvar seqel-nuc-mode-hook nil
  "*Hook to setup `seqel-nuc-mode'.")


(defvar seqel-nuc--base-alist
  '((?a  ?t ?a)
    (?t  ?a ?t)
    (?u  ?a ?u)
    (?c  ?g ?c)
    (?g  ?c ?g)
    (?m  ?k ?a ?c)
    (?y  ?r ?c ?t)
    (?r  ?y ?a ?g)
    (?w  ?w ?a ?t)
    (?s  ?s ?c ?g)
    (?k  ?m ?g ?t)
    (?v  ?b ?a ?c ?g)
    (?b  ?v ?c ?g ?t)
    (?h  ?d ?a ?c ?t)
    (?d  ?h ?a ?g ?t)
    (?n  ?n ?a ?t ?g ?c)     ; this and above are IUPAC codes.
    (?x  ?x ?a ?t ?g ?c))
  ;; Traditionally, the doc string starting with an asterisk (*) indicates
  ;; the variable is customizable.
  "*A association list showing the degeneracies and complements of the bases.

For each inner list, the first element is allowed nuc bases; the second element
is the complement of the first, and the rest is the degenerated bases
for the first.  Only for lowercase, as the upcased will be added automatically.")

;;;;; END OF USER CUSTOMIZABLE VARIABLES

(defvar seqel-nuc-base-alist
  (append (mapcar (lambda (x)
                    (mapcar #'upcase x))
                  seqel-nuc--base-alist)
          seqel-nuc--base-alist)
  "Similar to `seqel-nuc--base-alist', just with uppercase bases added.")


(defvar seqel-nuc-alphabet-set
  (let ((alphabet-set (make-hash-table :test 'eq :size (length seqel-nuc-base-alist))))
    (dolist (l seqel-nuc-base-alist)
      (puthash (car l) t alphabet-set))
    alphabet-set)
  "The set of all legal alphabets in DNA or RNA sequences.

This is a hash table: keys are char and values are t. It serves
like a set object similar in Python language.")


(defvar seqel-nuc-base-degeneracy
  (let ((d-vec (make-vector 256 nil)))
    (dolist (element seqel-nuc-base-alist)
      (aset d-vec (car element) (cddr element)))
  d-vec)
  "A vector of degeneracies (list type) for each upper and lower case valid bases defined in `seqel-nuc-base-alist'.")

(defvar seqel-nuc-dna-base-complement
  (let ((c-vec (vconcat (number-sequence 0 256))))  ; all alphabets chars are < 256
    (dolist (element seqel-nuc-base-alist)
      (aset c-vec (car element) (nth 1 element)))
    c-vec)
  "A vector of complements of upper and lower case bases.

For example, 'seqel-nuc-dna-base-complement[A]' returns 'U'.  see also
`seqel-nuc-rna-base-complement'.")

(defvar seqel-nuc-rna-base-complement
  ;; make a copy of the vector; otherwise it would change it in place.
  (let ((c-vec (copy-sequence seqel-nuc-dna-base-complement)))
    (aset c-vec ?a ?u)
    (aset c-vec ?A ?U)
    (aset c-vec ?u ?a)
    (aset c-vec ?U ?A)
    c-vec)
  "A vector of upper and lower case bases and their complements.

For example, 'seqel-nuc-rna-base-complement[A]' returns 'T'.  see also
`seqel-nuc-dna-base-complement'.")


;;;###autoload
(defun seqel-nuc-move-forward (count)
  "Move forward COUNT bases.  Move backward if negative.

Skip `seqel-cruft-regexp' but stop on the illegal base
and report how many bases the point have been moved by.
COUNT can be either positive or negative, indicating the
moving direction.  Return the number of bases that are moved thru.
See `seqel-forward-char'"
  (interactive "p")
  (seqel-forward-char count seqel-nuc-alphabet-set))

;;;###autoload
(defun seqel-nuc-move-backward (count)
  "Move backward COUNT bases, similar to `seqel-nuc-move-forward'.

See also `seqel-forward-char'.  `(seqel-nuc-move-backward -1)'
and `(seqel-nuc-move-forward 1)' are equvialent."
  (interactive "p")
  ;; (proceed-char-repeatedly count 'backward-char))
  (seqel-forward-char (- count) seqel-nuc-alphabet-set))

;;; delete
(defun seqel-nuc-delete-forward (count)
  "Delete COUNT number of bases starting from the point.

See also `seqel-nuc-delete-backward' and `seqel-nuc-move-forward'."
  (interactive "p")
  (let ((pos (point)))
    (seqel-forward-char count seqel-nuc-alphabet-set)
    (delete-region pos (point))))

(defun seqel-nuc-delete-backward (count)
  "Delete backward COUNT number of bases from the point.

See `seqel-nuc-delete-forward'."
  (interactive "p")
  (let ((pos (point)))
    (seqel-forward-char (- count) seqel-nuc-alphabet-set)
    (delete-region pos (point))))


(defun seqel-nuc-count (beg end)
  "Count the number of nucleotides between BEG and END.

Check if each char is legal base.  Return the count if the region
contains only legal nucleic acid characters, which includes
`seqel-nuc-alphabet-set', `seqel-cruft-set'; otherwise return nil and
report the location of the invalid characters in the echo region.
This function calls `seqel-count'.  `seqel-nuc-p' is an alias of this
function.

Interactively, BEG and END are the begin and end of the active
region or the current line if no region is active."
  (interactive (seqel-region-or-line))
  (let ((length (seqel-count beg end seqel-nuc-alphabet-set)))
    (and length
         (called-interactively-p 'interactive)
         (message "Base count: %d" length))
    length))

(defalias 'seqel-nuc-p 'seqel-nuc-count
  "Check the validity of the region as nucleotide sequence.

This is an alias of `seqel-nuc-count'.")

(defun seqel-nuc-rna-p (beg end)
  "Return the point of 'u' or 'U' if any is found; otherwise return nil.

Interactively, BEG and END are the begin and end of the active
region or the current line if no region is active.  See also
`seqel-nuc-dna-p' and `seqel-nuc-p'."
  (interactive (seqel-region-or-line))
  (let ((case-fold-search t))           ; enable case insensitive search
    (save-excursion
      (goto-char beg)
      (search-forward "u" end t))))
      ;; (re-search-forward "[uU]" end t))))

(defun seqel-nuc-dna-p (beg end)
  "Return the point of 't' or 'T' if any is found; otherwise return nil.

Interactively, BEG and END are the begin and end of the active
region or the current line if no region is active.

See also `seqel-nuc-rna-p' and `seqel-nuc-p'."
  (interactive (seqel-region-or-line))
  (let ((case-fold-search t))
    (save-excursion
      (goto-char beg)
      (search-forward "t" end t))))


(defun seqel-nuc-base-complement-lookup (base)
  "Look up the complement of the BASE and print a message.

See `seqel-nuc-dna-base-complement'."
  (interactive "cComplement of base:")
  (if (equal base ?u) (setq base ?t))
  ;; use aref or elt
  (message "Complement of '%c' is '%c'." base (aref seqel-nuc-dna-base-complement base)))


(defun seqel-nuc-complement (beg end &optional reverse)
  "Complement a region (or a line) of bases from BEG to END.

Complement a region of the buffer by replacing nucleotide char
base by base.  Non-base char will be passed over unchanged.  Also
reverse the sequence of the region if REVERSE is not nil.

Interactively, BEG and END are the begin and end of the active
region or the current line if no region is active."
  (interactive (seqel-region-or-line))
  (let ((complement-vector seqel-nuc-dna-base-complement)
        (is-rna (seqel-nuc-rna-p beg end))
        (sequence (buffer-substring-no-properties beg end)))
    (if is-rna
        (setq complement-vector seqel-nuc-rna-base-complement))
    (if reverse
        (setq sequence (nreverse sequence)))
    (delete-region beg end)
    (dotimes (i (- end beg))
      ;; replace the sequence inplace. this is the fastest and also
      ;; requires least mem. It reduces 12s to 2s and 200mb to 30mb
      ;; for a genome of 5M bp.
      (aset sequence i (aref complement-vector (aref sequence i))))
    (insert sequence)))

;;;###autoload
(defun seqel-nuc-reverse-complement (beg end)
  "Reverse complement a region of DNA or RNA sequence.

Interactively, BEG and END are the begin and end of the active
region or the current line if no region is active.

See also `seqel-nuc-complement'."
  (interactive (seqel-region-or-line))
  (seqel-nuc-complement beg end t)
  (if (called-interactively-p 'interactive)
      (message "Reverse complemented the selected region")))


(defalias 'seqel-nuc-rc 'seqel-nuc-reverse-complement)

(defvar seqel-nuc--u2t
  (let ((c-vec (vconcat (number-sequence 0 256))))  ; all alphabets chars are < 256
    (aset c-vec ?u ?t)
    (aset c-vec ?U ?T)
    c-vec)
  "A vector used to replace U/u with T/t.")


(defvar seqel-nuc--t2u
  (let ((c-vec (vconcat (number-sequence 0 256))))  ; all alphabets chars are < 256
    (aset c-vec ?t ?u)
    (aset c-vec ?T ?U)
    c-vec)
  "A vector used to replace T/t with U/u.")

;;;###autoload
(defun seqel-nuc-2rna (beg end &optional negate)
  "Convert the region or the current line to RNA.

It basically converts 't' -> 'u' and 'T' -> 'U'.  Interactively,
BEG and END are the begin and end of the active region or the
current line if no region is active.  If NEGATE is not nil,
convert to DNA.  See also `seqel-nuc-2dna'."
  (interactive (seqel-region-or-line))
  (let ((sequence (buffer-substring-no-properties beg end))
        (replace-vector (if negate seqel-nuc--u2t seqel-nuc--t2u)))
    (delete-region beg end)
    (dotimes (i (- end beg))
      ;; replace the sequence inplace. this is the fastest and also
      ;; requires least mem. It reduces 12s to <2s and 200mb to 30mb
      ;; for a genome of 5M bp.
      (aset sequence i (aref replace-vector (aref sequence i))))
    (insert sequence)))


(defun seqel-nuc-2dna (beg end)
  "Convert the region or current line to DNA.

It basically converts 'u' -> 't' and 'U' -> 'T'.  Interactively,
BEG and END are the begin and end of the active region or the
current line if no region is active.  See also `seqel-nuc-2rna'."
  (interactive (seqel-region-or-line))
  (seqel-nuc-2rna beg end t))

(defun seqel-nuc-summary (beg end)
  "Print the frequencies of bases in the region or the current line.


Interactively, BEG and END are the begin and end of the active
region or the current line if no region is active.  See also
`seqel-summary'."
  (interactive (seqel-region-or-line))
  (seqel-summary beg end seqel-nuc-alphabet-set))


(defun seqel-nuc-isearch-mangle-str-degeneracy (str)
  "Mangle the string STR into a regexp to search over cruft in sequence.

Inserts a regexp between each base which matches sequence
formatting cruft.  For example, if `seqel-cruft-regexp' is '[ ]',
the search string 'at' would be transformed into '[a][ ]*[t]';
and 'mR' will be transformed to '[ac][ ]*[AG]'."
  ;; (mapconcat 'identity (split-string str "" t) (concat seqel-cruft-regexp "*")))
  (let (degenerate-str-list degeneracies)
    ;; 'ar' will return as ("[a]", "[ag]")
    (setq degenerate-str-list
          (mapcar #'(lambda (x)
                      (setq degeneracies (aref seqel-nuc-base-degeneracy x))
                      (if (not degeneracies)
                          (error "%c is not a valid nuc base character!" x))
                      (concat "["  (mapconcat 'char-to-string degeneracies "") "]"))
                  (string-to-list str)))
    (mapconcat 'identity degenerate-str-list (concat seqel-cruft-regexp "*"))))

(defun seqel-nuc-isearch-forward (pattern &optional bound noerror)
  "Search forward for PATTERN.

BOUND and NOERROR passes to function `re-search-forward'."
  (let ((string (seqel-nuc-isearch-mangle-str-degeneracy pattern)))
    (re-search-forward string bound noerror)))

(defun seqel-nuc-isearch-backward (pattern &optional bound noerror)
  "Search backward for PATTERN.

BOUND and NOERROR passes to function `re-search-backward'."
  (let ((string (seqel-nuc-isearch-mangle-str-degeneracy pattern)))
    (re-search-backward string bound noerror)))

(defun seqel-nuc--isearch-search-fun ()
  "This function will be assigned to `isearch-search-fun-function'."
  (if seqel-isearch-p
      (if isearch-forward 'seqel-nuc-isearch-forward 'seqel-nuc-isearch-backward)
    (isearch-search-fun-default)))


;; weighted homopolymer rate (WHR)
(defun seqel-nuc-whr (beg end)
  "Calculate weighted homopolymer rate of the selected sequence.

A homopolymer is a sequence of identical bases, like AAAA or TTTTTTTT.
The weighted homopolymer rate (WHR) of a sequence is a measure of
the frequency of homopolymers in the sequence.

Interactively, BEG and END are the begin and end of the active
region or the current line if no region is active."
  (interactive (seqel-region-or-line))
  (let ((n  1.0)  (ni 0) (nis 0)
        old cur)
    (save-excursion
      (goto-char beg)
      (while (and (/= (point) end))
        (setq cur (char-after))
        (cond ((not old)
               (setq ni 1))
              ((/= old cur)
               (setq n (1+ n))
               (setq nis (+ nis (expt ni 2)))
               (setq ni 1))
              ((= old cur)
               (setq ni (1+ ni))))
        ;; (message "%c %d %d %d" cur ni nis n)
        (setq old cur)
        (forward-char)))
    (setq nis (+ nis (expt ni 2)))
    ;; (message "%d %d" nis n)
    ;; (princ (/ nis n))
    (/ nis n)))


;;; Per base colors
(defvar seqel-nuc-base-colors
    (seqel--zip #'(lambda (x y) (cons (car x) y)) seqel-nuc-base-alist seqel-color-pairs-cycle)

  "Background and foreground colors for each IUPAC bases.

This is a list of lists.  For each inner list, it contains 3
atoms: a nuc base in char type, hex-code colors for foreground
and background")


(mapc (lambda (elem)
        (let ((l (format "%c" (nth 0 elem)))
              (f (nth 2 elem))
              (b (nth 1 elem)))
          (eval (macroexpand `(seqel--def-char-face ,l ,b ,f "nuc-base-face")))))
      seqel-nuc-base-colors)


;;;###autoload
(defun seqel-nuc-paint (beg end &optional case)
  "Color the nucleic acid region BEG to END.

If CASE is nil, upcase and lowercase base chars will be colored the same;
otherwise, not.  See `seqel-paint' for details.

Interactively, BEG and END are the begin and end of the active
region or the current line if no region is active."
  (interactive "r\nP")
  (if (not (use-region-p))
      (setq beg (line-beginning-position)
            end (line-end-position)))
  (seqel-paint beg end "nuc-base-face" case))

;;;###autoload
(defalias 'seqel-nuc-unpaint 'seqel-unpaint
  "Uncolor the nucleotide sequence region.

It is an alias to `seqel-unpaint'.")


(defvar seqel-nuc-translation-table nil
  "Define the translation table.

This is list.  Its car is an int indication which genetic table it
is.  Its cdr is a hash table with codons as keys and encoded
amino acids as values.  This variable is set by
`seqel-nuc-set-translation-table'.")


(defun seqel-nuc-set-translation-table (n)
  "Set translation table to N.

By default, this function set the table 1 as the translation table.
Interactively, N is set to numeric prefix argument."
  (interactive "p")
  (let (table)
    (or (setq table (seqel-genetic-code-table n))
        (error "The translation table %d does not exist" n))
    (setq seqel-nuc-translation-table `(,n . ,(seqel-hash-alist table)))
    (message "Set to translation table %d" n)))


(defun seqel-nuc-decode (codon)
  "Return the list of amino acid(s) that are coded by the CODON.

CODON must be uppercase string of 3 DNA letters.  Example: (seqel-nuc-decode
\"TCM\") should return (83) and (seqel-nuc-decode \"MAT\") should
return (72 78) for translation table 1."
  (interactive (list (read-from-minibuffer
                (format "The amino acid(s) of condon in genetic table %d: " (car seqel-nuc-translation-table)))))
  (let ((table (cdr seqel-nuc-translation-table))
        degenerated-codon aas aa)
    (if (stringp codon)
        (setq codon (string-to-list codon)))
    (dolist (first-base (aref seqel-nuc-base-degeneracy (nth 0 codon)))
      (dolist (second-base (aref seqel-nuc-base-degeneracy (nth 1 codon)))
        (dolist (third-base (aref seqel-nuc-base-degeneracy (nth 2 codon)))
          (setq degenerated-codon (format "%c%c%c" first-base second-base third-base))
          (setq aa (car (gethash degenerated-codon table)))
          (or aa (error "Codon %s (after degeneration) is not recognized!" degenerated-codon))
          (or (memq aa aas)
              (setq aas (cons aa aas))))))
    (if (called-interactively-p 'interactive)
        ;; apply is used to concat list of char to a string
        (message "%s encodes %s" codon (apply #'string aas)))
    aas))

;;;###autoload
(defun seqel-nuc-translate (beg end)
  "Translate the nucleotides to protein using current translation table.

The ambiguous codon will be handled correctly: if it is mapped to
multiple amino acids, 'X' will be the output.

Interactively, BEG and END are the begin and end of the active
region or the current line if no region is active.

This function translates DNA of 9K for ~6 sec (over 80% of the
time is for `seqel-nuc-decode') and ~15 MB mem.  It is not super fast,
but it is very rare you need to translate a sequence over 10K
long."
  (interactive (seqel-region-or-line))
  (let* (;; check valid of the sequence and count the number of bases
         (n (seqel-nuc-count beg end))
         (x (% n 3))
         (len (- n x))
         str seq codon aa)
    (goto-char beg)
    ;; ask for confirmation if translate long sequence
    (if (or (< n 10000) (y-or-n-p (format "Translate %d nucleotides (long sequence will take a while)? " n)))
        (progn
          (message "translating %d nucleotides to %d amino acids..." len (/ n 3))
          (seqel-nuc-move-forward len)
          (setq str (buffer-substring-no-properties beg (point)))
          ;; seq is a list of all legal nuc char
          (setq seq (mapcan (lambda (i) (if (gethash i seqel-nuc-alphabet-set) (list (upcase i)))) str))
          (delete-region beg (point))
          (insert (concat
                   (mapcar (lambda (i) (let ((j (* i 3)) aas)
                                         (setq aas (seqel-nuc-decode (list (nth j seq)
                                                                     (nth (1+ j) seq)
                                                                     (nth (+ j 2) seq))))
                                         (if (> (length aas) 1)
                                             ?X
                                           (car aas))))
                           (number-sequence 0  (1- (/ n 3)))))))))
  (message "done."))


(defvar seqel-nuc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n C-f") 'seqel-nuc-move-forward)
    (define-key map (kbd "C-c C-n C-b") 'seqel-nuc-move-backward)
    (define-key map (kbd "C-c C-n C-r") 'seqel-nuc-rc)
    (define-key map (kbd "C-c C-n C-s") 'seqel-nuc-summary)
    (define-key map (kbd "C-c C-n C-t") 'seqel-nuc-translate)
    map)
  "Keymap for the 'seqel-nuc-mode' minor mode.")


(define-minor-mode seqel-nuc-mode
  "Nucleic acid mode.

It should not be enabled with `pro-mode' at the same time."
  ;; the fun auto defines a buffer local variable `seqel-nuc-mode'
  ;; and this key value set its initial value
  :init-value nil
  ;; the name, a string, to show in the modeline
  :lighter " nucleotide"
  :keymap seqel-nuc-mode-map
  ;; set the translation table to 1 if it is nil
  (or seqel-nuc-translation-table (seqel-nuc-set-translation-table 1))
  (setq-local seqel-isearch-p t)
  (setq isearch-search-fun-function 'seqel-nuc--isearch-search-fun)
  (run-hooks 'seqel-nuc-mode-hook))


(provide 'seqel-nuc-mode)

;;; seqel-nuc-mode.el ends here
