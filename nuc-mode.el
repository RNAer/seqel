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

(defvar nuc-base
  (mapcar #'car nuc-base-alist)
  "All the bases that are allowed.

This is a list of chars. Only lower cases.")


(defvar nuc-base-regexp
    (regexp-opt (mapcar #'char-to-string nuc-base))
  "A regexp that matches a valid nucleotide base symbol defined
in `nuc-base-alist'.")


(defvar nuc-base-degeneracy
  (let ((d-vec (make-vector 256 nil)))
    (dolist (element nuc-base-alist)
      (aset d-vec (car element) (cddr element)))
  d-vec)
  "A vector of degeneracies for each upper and lower case valid bases
defined in `nuc-base-alist'.")

(defvar dna-base-complement
  (let ((c-vec (make-vector 256 nil)))  ; all alphabets chars are < 256
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
    (aset c-vec ?t nil)
    (aset c-vec ?T nil)
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
See `proceed-char-repeatedly'"
  (interactive "p")
  (proceed-char-repeatedly count #'forward-char nuc-base-regexp))

;;;###autoload
(defun nuc-move-backward (count)
  "Move backward COUNT bases, similar to `nuc-move-forward'.

See also `proceed-char-repeatedly'."
  (interactive "p")
  ;; (proceed-char-repeatedly count 'backward-char))
  (proceed-char-repeatedly (- count) #'forward-char nuc-base-regexp))

;;; delete
(defun nuc-delete-forward (count)
  "Delete COUNT number of bases starting from the point.

Similar to `nuc-move-forward' (just delete instead of move)."
  (interactive "p")
  (proceed-char-repeatedly count #'delete-char nuc-base-regexp))

(defun nuc-delete-backward (count)
  "Delete backward COUNT number of bases from the point.

Similar to `nuc-move-forward' (just delete backward instead of move
forward). See `nuc-delete-forward' and `proceed-char-repeatedly'."
  (interactive "p")
  (proceed-char-repeatedly (- count) #'delete-char nuc-base-regexp))


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
  (let ((c-base (or (elt dna-base-complement base)
                    ??)))     ; return '?' if there's no complement base
    (message "Complement of '%c' is '%c'." base c-base)))


(defun nuc-complement (beg end)
  "Complement a region (or a line ) of bases from BEG to END.

Complement a region of the buffer by replacing nucleotide char
base by base. Non-base char will be passed over unchanged."
  (interactive-region-or-line)
  (let* ((complement-vector dna-base-complement)
         (is-rna (nuc-rna-p beg end)))
    (if is-rna
        (setq complement-vector rna-base-complement))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (setq base (following-char))
        (setq c-base (aref complement-vector base))
        (insert (or c-base base))
        (delete-char 1)))))


;;;###autoload
(defun nuc-reverse-complement (beg end)
  "Reverse complement a region of DNA or RNA sequence.

See also `nuc-complement'."
  (interactive-region-or-line)
  (let ((region (buffer-substring-no-properties beg end)))
    (save-excursion
      (delete-region beg end)
      (insert (nreverse region))
      (nuc-complement beg end))
    (if (called-interactively-p 'interactive)
      (message "Reverse complemented the selected region"))))


(defalias 'nuc-rc 'nuc-reverse-complement)


;;;###autoload
(defun nuc-2rna (beg end)
  "Convert the region or the current line to RNA.

It basically converts 't' -> 'u' and 'T' -> 'U'.
Similar to `nuc-2dna."
  (interactive-region-or-line)
  (save-excursion
    (goto-char beg)
    (while (search-forward "t" end t)  ;(case-fold-search t)
      (replace-match "u" nil t))))

(defun nuc-2dna (beg end)
  "Convert the region or current line to DNA.

It basically converts 'u' -> 't' and 'U' -> 'T'.
Similar to `nuc-2rna'."
  (interactive-region-or-line)
  (save-excursion
    (goto-char beg)
    (while (search-forward "u" end t)   ;(case-fold-search t)
      (replace-match "t" nil t))))


(defun nuc-summary (beg end)
  "Print the frequencies of bases in the region or the current line.

See also `region-summary'."
  (interactive-region-or-line)
  (let ((my-hash (region-summary beg end nuc-base-regexp)))
    (maphash (lambda (x y) (princ (format "%c:%d " x y) t))
             my-hash)))


(defun seq-isearch-mangle-str (str)
  "Mangle the string STR into a regexp to search over cruft in sequence.

Inserts a regexp between each base which matches sequence formatting cruft.
For example, if `seq-cruft-regexp' is '[ ]', the search string 'acgt' would be
transformed into 'a[ ]*c[ ]*g[ ]*t'."
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
    (print degenerate-str-list)
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
  (let ((colp (setcdr (last color-pairs) color-pairs))
        (n (length nuc-base))
        tmp)
    (dotimes (i n)
      (setq tmp (cons (cons (nth i nuc-base) (nth i colp)) tmp)))
    tmp)
  ;; Alternatively,
  ;; (mapcar* #'cons
  ;;          nuc-base
  ;;          ;; this is circular list
  ;;          (setcdr (last color-pairs) color-pairs))
  "Background and foreground colors for each IUPAC bases.

This is a list of lists. For each inner list, it contains 3 atoms:
a nuc base in char type, hex-code colors for foreground and background")


;; define base faces belonging to base-face group
(loop for elem in nuc-base-colors
      for f = (nth 1 elem)
      for b = (nth 2 elem)
      for l = (format "%c" (nth 0 elem)) do
      (eval (macroexpand `(def-char-face ,l ,b ,f "base-face"))))


;;;###autoload
(defun nuc-paint (beg end &optional case)
  "Color the nucleic acid region BEG to END.

If CASE is nil, upcase and lowercase base chars will be colored the same;
otherwise, not. See `seq-paint' for details."
  (interactive "r\nP")
  (if (not (use-region-p))
      (setq beg (line-beginning-position)
            end (line-end-position)))
  (seq-paint beg end "base-face" case))

;;;###autoload
(defalias 'nuc-unpaint 'seq-unpaint
  "Uncolor the nucleotide sequence region.

It is an alias to `seq-unpaint'.")


(defvar translation-table nil
  "Define the translation table.

This is hash table with codons as keys. It is set by
`nuc-set-translation-table'.")


(defun nuc-decode (codons)
  "Return the list of amino acid(s) that are coded by the CODONS.

Example: (nuc-decode '(?t ?c ?m) should return (83) and (nuc-decode '(?m ?a ?t)) should
return (72 78) for translation table 1."
  (let ((case-fold-search t)
        codon aas aa)
    (dolist (first (aref nuc-base-degeneracy (nth 0 codons)))
      (if (memq first '(?u ?U)) (setq first ?t))
      (dolist (second (aref nuc-base-degeneracy (nth 1 codons)))
        (if (memq second '(?u ?U)) (setq second ?t))
        (dolist (third (aref nuc-base-degeneracy (nth 2 codons)))
          (if (memq third '(?u ?U)) (setq third ?t))
          (setq codon (format "%c%c%c" first second third))
          (setq aa (car (gethash (upcase codon) translation-table)))
          (or aa (error "codon %s is not recognized." codon))
          (or (memq aa aas)
              (setq aas (cons aa aas))))))
    aas))


(defun nuc-set-translation-table (n)
  "Set translation table to N.

By default, this function set the table 1 as the translation table.
For example, run `C-u 2 M-x nuc-set-translation-table' to set it to table 2."
  (interactive "p")
  (let (table)
    (or (setq table (get-translation-table n))
        (error "The translation table %d does not exist" n))
    (setq translation-table (hash-alist table))
    (message "Set to translation table %d" n)))


;;;###autoload
(defun nuc-translate (beg end)
  "Translate the nuc seq to protein seq using current translation table.

The ambiguous codon will be handled correctly: if it is mapped to multiple
amino acids, 'X' will be output; if it is mapped to a single amino acid,
then it will be translated into the amino acid."
  (interactive-region-or-line)
  (let (;; check valid of the sequence and
        ;; count the number of bases
        (n (nuc-count beg end))
        codon aa)
      (goto-char beg)
      (if n
        (progn
          (setq n (/ n 3))
          (message "%d nucleotides to %d amino acids" (* 3 n) n)
          (while (> n 0)
            (if (looking-at nuc-base-regexp)
                (setq codon (cons (char-after) codon)))
            (if (equal (length codon) 3)
                (progn (setq aa (nuc-decode (nreverse codon)))
                       (if (> (length aa) 1)
                           (setq aa ?X)
                         (setq aa (car aa)))
                       (insert-char aa)
                       (setq n (1- n))
                       (setq codon nil)))
            (delete-char 1))))))


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
  (or translation-table (nuc-set-translation-table 1)))


(provide 'nuc-mode)

;;; nuc-mode.el ends here
