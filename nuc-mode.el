;;; nuc-mode.el --- a minor mode for editing nucleic acid sequences

;;; license: BSD-3

;;; Author: Zech Xu <zhenjiang dot xu at gmail dot com>

;;; Commentary:
;;  * A collection of functions for editing DNA and RNA sequences.
;;  * It should not be enabled with pro-mode at the same time.
;;  * Borrows a few ideas/lines of code from dna-mode.el

(require 'seq)
(require 'genetic-code)

;;;;;; USER CUSTOMIZABLE VARIABLES START HERE

(defvar nuc--base-alist
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
for the first. Only for lowercase, as the upcased will be added automatically.")

;;;;; END OF USER CUSTOMIZABLE VARIABLES

(defvar nuc-base-alist
  (append (mapcar (lambda (x)
                    (mapcar #'upcase x))
                  nuc--base-alist)
          nuc--base-alist)
  "Similar to `nuc--base-alist', just with uppercase bases added.")


(defvar nuc-alphabet-set
  (let ((alphabet-set (make-hash-table :test 'eq :size (length nuc-base-alist))))
    (dolist (l nuc-base-alist)
      (puthash (car l) t alphabet-set))
    alphabet-set)
  "The set of all legal alphabets in DNA or RNA sequences.

This is a hash table: keys are char and values are `t'. It serves
like a set object similar in Python language.")


(defvar nuc-base-regexp
    (regexp-opt (mapcar (lambda (i) (char-to-string (car i))) nuc-base-alist))
  "A regexp that matches a valid nucleotide base symbol defined
in `nuc-base-alist'.")


(defvar nuc-base-degeneracy
  (let ((d-vec (make-vector 256 nil)))
    (dolist (element nuc-base-alist)
      (aset d-vec (car element) (cddr element)))
  d-vec)
  "A vector of degeneracies (list type) for each upper and lower
case valid bases defined in `nuc-base-alist'.")

(defvar dna-base-complement
  (let ((c-vec (vconcat (number-sequence 0 256))))  ; all alphabets chars are < 256
    (dolist (element nuc-base-alist)
      (aset c-vec (car element) (nth 1 element)))
    c-vec)
  "A vector of complements of upper and lower case bases.
 dna-base-complement[base] returns the complement of the base. see also
`rna-base-complement'.")

(defvar rna-base-complement
  ;; make a copy of the vector; otherwise it would change it in place.
  (let ((c-vec (copy-sequence dna-base-complement)))
    (aset c-vec ?a ?u)
    (aset c-vec ?A ?U)
    (aset c-vec ?u ?a)
    (aset c-vec ?U ?A)
    c-vec)
  "A vector of upper and lower case bases and their complements.
 rna-base-complement[base] returns the complement of the base. see also
`dna-base-complement'.")


;;;###autoload
(defun nuc-move-forward (count)
  "Move forward COUNT bases. Move backward if negative.

Skip `seq-cruft-regexp' but stop on the illegal base
and report how many bases the point have been moved by.
COUNT can be either positive or negative, indicating the
moving direction. Return the number of bases that are moved thru.
See `seq-forward-char'"
  (interactive "p")
  (seq-forward-char count nuc-alphabet-set))

;;;###autoload
(defun nuc-move-backward (count)
  "Move backward COUNT bases, similar to `nuc-move-forward'.

See also `seq-forward-char'. `(nuc-move-backward -1)'
and `(nuc-move-forward 1)' are equvialent."
  (interactive "p")
  ;; (proceed-char-repeatedly count 'backward-char))
  (seq-forward-char (- count) nuc-alphabet-set))

;;; delete
(defun nuc-delete-forward (count)
  "Delete COUNT number of bases starting from the point.

See also `nuc-delete-backward' and `nuc-move-forward'."
  (interactive "p")
  (let ((pos (point)))
    (seq-forward-char count nuc-alphabet-set)
    (delete-region pos (point))))

(defun nuc-delete-backward (count)
  "Delete backward COUNT number of bases from the point.

See `nuc-delete-forward'."
  (interactive "p")
  (let ((pos (point)))
    (seq-forward-char (- count) nuc-alphabet-set)
    (delete-region pos (point))))


(defun nuc-count (beg end)
  "Count the number of bases in the region or in the current line.

Check if each char is legal base. Return the count if the region
contains only legal nucleic acid characters, which includes
`nuc-base-regexp', `seq-cruft-regexp'; otherwise return nil and
report the location of the invalid characters in the echo region.
This function calls `seq-count'. `nuc-p' is an alias of this function."
  (interactive-region-or-line)
  (let ((length (seq-count beg end nuc-base-regexp)))
    (and length
         (called-interactively-p 'interactive)
         (message "Base count: %d" length))
    length))

(defalias 'nuc-p 'nuc-count
  "Check the validity of the region as nucleotide sequence.

This is an alias of `nuc-count'.")

(defun nuc-rna-p (beg end)
  "Return the point of 'u' or 'U' if any is found; otherwise return nil.

See also `nuc-dna-p' and `nuc-p'."
  (interactive-region-or-line)
  (let ((case-fold-search t))           ; enable case insensitive search
    (save-excursion
      (goto-char beg)
      (search-forward "u" end t))))
      ;; (re-search-forward "[uU]" end t))))

(defun nuc-dna-p (beg end)
  "Return the point of 't' or 'T' if any is found; otherwise return nil.

See also `nuc-rna-p' and `nuc-p'."
  (interactive-region-or-line)
  (let ((case-fold-search t))
    (save-excursion
      (goto-char beg)
      (search-forward "t" end t))))


(defun nuc-base-complement-lookup (base)
  "Look up the complement of the BASE and print a message.

See `dna-base-complement'."
  (interactive "cComplement of base:")
  (if (equal base ?u) (setq base ?t))
  ;; use aref or elt
  (message "Complement of '%c' is '%c'." base (aref dna-base-complement base)))


(defun nuc-complement (beg end &optional reverse)
  "Complement a region (or a line) of bases from BEG to END.

Complement a region of the buffer by replacing nucleotide char
base by base. Non-base char will be passed over unchanged."
  (interactive-region-or-line)
  (let ((complement-vector dna-base-complement)
        (is-rna (nuc-rna-p beg end))
        (sequence (buffer-substring-no-properties beg end)))
    (if is-rna
        (setq complement-vector rna-base-complement))
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
(defun nuc-reverse-complement (beg end)
  "Reverse complement a region of DNA or RNA sequence.

See also `nuc-complement'."
  (interactive-region-or-line)
  (nuc-complement beg end t)
  (if (called-interactively-p 'interactive)
      (message "Reverse complemented the selected region")))


(defalias 'nuc-rc 'nuc-reverse-complement)

(defvar nuc--u2t
  (let ((c-vec (vconcat (number-sequence 0 256))))  ; all alphabets chars are < 256
    (aset c-vec ?u ?t)
    (aset c-vec ?U ?T)
    c-vec)
  "A vector used to replace U/u with T/t.")


(defvar nuc--t2u
  (let ((c-vec (vconcat (number-sequence 0 256))))  ; all alphabets chars are < 256
    (aset c-vec ?t ?u)
    (aset c-vec ?T ?U)
    c-vec)
  "A vector used to replace T/t with U/u")

;;;###autoload
(defun nuc-2rna (beg end &optional negate)
  "Convert the region or the current line to RNA.

It basically converts 't' -> 'u' and 'T' -> 'U'.
See also `nuc-2dna'."
  (interactive-region-or-line)
  (let ((sequence (buffer-substring-no-properties beg end))
        (replace-vector (if negate nuc--u2t nuc--t2u)))
    (delete-region beg end)
    (dotimes (i (- end beg))
      ;; replace the sequence inplace. this is the fastest and also
      ;; requires least mem. It reduces 12s to <2s and 200mb to 30mb
      ;; for a genome of 5M bp.
      (aset sequence i (aref replace-vector (aref sequence i))))
    (insert sequence)))


(defun nuc-2dna (beg end)
  "Convert the region or current line to DNA.

It basically converts 'u' -> 't' and 'U' -> 'T'.
See also `nuc-2rna'."
  (interactive-region-or-line)
  (nuc-2rna beg end t))

(defun nuc-summary (beg end)
  "Print the frequencies of bases in the region or the current line.

See also `region-summary'."
  (interactive-region-or-line)
  (region-summary beg end nuc-base-regexp))


(defun nuc-seq-isearch-mangle-str-degeneracy (str)
  "Mangle the string STR into a regexp to search over cruft in sequence.

Inserts a regexp between each base which matches sequence
formatting cruft.  For example, if `seq-cruft-regexp' is '[ ]',
the search string 'at' would be transformed into '[a][ ]*[t]';
and 'mR' will be transformed to '[ac][ ]*[AG]'."
  ;; (mapconcat 'identity (split-string str "" t) (concat seq-cruft-regexp "*")))
  (let (degenerate-str-list  degeneracies)
    ;; 'ar' will return as ("[a]", "[ag]")
    (setq degenerate-str-list
          (mapcar #'(lambda (x)
                      (setq degeneracies (aref nuc-base-degeneracy x))
                      (if (not degeneracies)
                          (error "%c is not a valid nuc base character!" x))
                      (concat "["  (mapconcat 'char-to-string degeneracies "") "]"))
                  (string-to-list str)))
    (mapconcat 'identity degenerate-str-list (concat seq-cruft-regexp "*"))))


;; weighted homopolymer rate (WHR)
(defun nuc-whr (beg end)
  "Calculate weighted homopolymer rate of the selected sequence.

A homopolymer is a sequence of identical bases, like AAAA or TTTTTTTT.
The weighted homopolymer rate (WHR) of a sequence is a measure of
the frequency of homopolymers in the sequence. "
  (interactive-region-or-line)
  (let ((n  1.0)  (ni 0) (nis 0)
        old cur)
    (save-excursion
      (goto-char beg)
      (while (and (/= (point) end) (looking-at nuc-base-regexp))
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
(defvar nuc-base-colors
    (mapcar* #'(lambda (x y) (cons (car x) y)) nuc-base-alist color-pairs-cycle)

  "Background and foreground colors for each IUPAC bases.

This is a list of lists.  For each inner list, it contains 3
atoms: a nuc base in char type, hex-code colors for foreground
and background")


;; define base faces belonging to base-face group
(loop for elem in nuc-base-colors
      for f = (nth 1 elem)
      for b = (nth 2 elem)
      for l = (format "%c" (nth 0 elem)) do
      (eval (macroexpand `(def-char-face ,l ,b ,f "nuc-base-face"))))


;;;###autoload
(defun nuc-paint (beg end &optional case)
  "Color the nucleic acid region BEG to END.

If CASE is nil, upcase and lowercase base chars will be colored the same;
otherwise, not. See `seq-paint' for details."
  (interactive "r\nP")
  (if (not (use-region-p))
      (setq beg (line-beginning-position)
            end (line-end-position)))
  (seq-paint beg end "nuc-base-face" case))

;;;###autoload
(defalias 'nuc-unpaint 'seq-unpaint
  "Uncolor the nucleotide sequence region.

It is an alias to `seq-unpaint'.")


(defvar nuc-translation-table nil
  "Define the translation table.

This is list. Its car is an int indication which genetic table it
is.  Its cdr is a hash table with codons as keys and encoded
amino acids as values. This variable is set by
`nuc-set-translation-table'.")


(defun nuc-decode (codon)
  "Return the list of amino acid(s) that are coded by the CODON.

CODON must be uppercase string of 3 DNA letters. Example: (nuc-decode
\"TCM\") should return (83) and (nuc-decode \"MAT\") should
return (72 78) for translation table 1."
  (interactive (list (read-from-minibuffer
                (format "The amino acide of condon in genetic table %d: " (car nuc-translation-table)))))
  (let ((table (cdr nuc-translation-table))
        degenerated-codon aas aa)
    (if (stringp codon)
        (setq codon (string-to-list codon)))
    (dolist (first-base (aref nuc-base-degeneracy (nth 0 codon)))
      (dolist (second-base (aref nuc-base-degeneracy (nth 1 codon)))
        (dolist (third-base (aref nuc-base-degeneracy (nth 2 codon)))
          (setq degenerated-codon (format "%c%c%c" first-base second-base third-base))
          (setq aa (car (gethash degenerated-codon table)))
          (or aa (error "Codon %s (after degeneration) is not recognized." degenerated-codon))
          (or (memq aa aas)
              (setq aas (cons aa aas))))))
    (if (called-interactively-p 'interactive)
        ;; apply is used to concat list of char to a string
        (message "%s encodes %s" codon (apply #'string aas)))
    aas))


(defun nuc-set-translation-table (n)
  "Set translation table to N.

By default, this function set the table 1 as the translation table.
For example, run `C-u 2 M-x nuc-set-translation-table' to set it to table 2."
  (interactive "p")
  (let (table)
    (or (setq table (get-translation-table n))
        (error "The translation table %d does not exist" n))
    (setq nuc-translation-table `(,n . ,(hash-alist table)))
    (message "Set to translation table %d" n)))


;;;###autoload
(defun nuc-translate (beg end)
  "Translate the DNA/RNA seq to protein seq using current translation table.

The ambiguous codon will be handled correctly: if it is mapped to multiple
amino acids, 'X' will be output.

This function translates DNA of 9K for ~6 sec (over 80% of the
time is for `nuc-decode') and ~15 MB mem."
  (interactive-region-or-line)
  (let* (;; check valid of the sequence and count the number of bases
         (n (nuc-count beg end))
         (x (% n 3))
         str seq codon aa)
    (goto-char beg)
    (message "translating %d nucleotides to %d amino acids..." (- n x) (/ n 3))
    (setq str (buffer-substring-no-properties beg (- end x)))
    (setq seq (mapcan (lambda (i) (if (gethash i nuc-alphabet-set) (list (upcase i)))) str))
    (delete-region beg (- end x))
    (insert (concat
             (mapcar (lambda (i) (let ((j (* i 3)) aas)
                                   (setq aas (nuc-decode (list (nth j seq)
                                                          (nth (1+ j) seq)
                                                          (nth (+ j 2) seq))))
                                   (if (> (length aas) 1)
                                       ?X
                                     (car aas))))
                     (number-sequence 0  (1- (/ n 3))))))))


(defvar nuc-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "\C-cf"     'nuc-move-forward)
    (define-key map "\C-cb"     'nuc-move-backward)
    (define-key map "\C-c\C-r"  'nuc-rc)
    ;; (define-key map "\C-c\C-#"  'nuc-summary)
    (define-key map "\C-c\C-t"  'nuc-translate)
    map)
  "Keymap for `nuc-mode'.")

(define-minor-mode nuc-mode
  "Nucleic acid mode.

It should not be enabled with `pro-mode' at the same time."
  ;; the fun auto defines a buffer local variable `nuc-mode'
  ;; and this key value set its initial value
  :init-value nil
  ;; the name, a string, to show in the modeline
  :lighter " nucleotide"
  :keymap nuc-mode-map
  :global t
  ;; set the translation table to 1 if it is nil
  (or nuc-translation-table (nuc-set-translation-table 1)))


(provide 'nuc-mode)

;;; nuc-mode.el ends here
