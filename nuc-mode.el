;;; nuc-mode.el --- a minor mode for editing nucleic acid sequences
;;
;;; Commentary:
;; * A collection of functions for editing DNA and RNA sequences.


;;;;;; USER CUSTOMIZABLE VARIABLES START HERE
(require 'seq)

(defvar nuc-base-alist
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
  "*A association list showing the degeneracies and complements of the bases.

For each inner list, the first element is allowed nuc bases; the second element
is the complement of the first, and the rest is the degenerated bases
for the first. Only for lowercase, as the upcased will be added automatically.")

;;;;; END OF USER CUSTOMIZABLE VARIABLES

(defvar nuc-base
  (mapcar #'car nuc-base-alist)
  "All the bases that are allowed.

This is a list of chars. All are in lower cases.")


(defvar nuc-base-regexp
    (regexp-opt (mapcar #'char-to-string
                        (append nuc-base
                                (mapcar #'upcase nuc-base))))
  "A regexp that matches a valid nucleotide base symbol defined
in `nuc-base-alist'.")


(defvar nuc-base-degeneracy
  (let ((d-vec (make-vector 256 nil)))
    (dolist (element nuc-base-alist)
      (aset d-vec (car element) (cddr element))
      (aset d-vec
            (upcase (car element))
            (mapcar 'upcase (cddr element))))
  d-vec)
  "A vector of degeneracies for each upper and lower case valid bases
defined in `nuc-base-alist'.")

(defvar dna-base-complement
  (let ((c-vec (make-vector 256 nil)))  ; all alphabets chars are < 256
    (dolist (element nuc-base-alist)
      (aset c-vec (car element) (nth 1 element))
      (aset c-vec (upcase (car element)) (upcase (nth 1 element))))
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
  "Move backward COUNT bases, similar to `nuc-move-forward'. See also
 `proceed-char-repeatedly'."
  (interactive "p")
  ;; (proceed-char-repeatedly count 'backward-char))
  (proceed-char-repeatedly (- count) #'forward-char nuc-base-regexp))

;;; delete
(defun nuc-delete-forward (count)
  "Delete COUNT number of bases starting from the point, similar to
`nuc-move-forward' (just use delete instead of move)."
  (interactive "p")
  (proceed-char-repeatedly count #'delete-char nuc-base-regexp))

(defun nuc-delete-backward (count)
  "Delete backward COUNT number of bases from the point, similar to
`nuc-move-forward' (just use delete backward instead of move forward).
See `nuc-delete-forward' and `proceed-char-repeatedly'."
  (interactive "p")
  (proceed-char-repeatedly (- count) #'delete-char nuc-base-regexp))


(defun nuc-p (beg end)
  "Test if the region between BEG and END (or the line) is a legal nucleotide
acid sequence. Return the count if the region
 contains only legal nucleic acid characters, including
 `nuc-base-regexp', `seq-cruft-regexp'; otherwise return nil and
 report the location of the invalid characters in the echo region.
This function calls `seq-p'."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (seq-p beg end nuc-base-regexp))

(defalias 'nuc-count 'nuc-p)

(defun rna-p (beg end)
  "Return the point of 'u' or 'U' if they are found; otherwise return nil.
See also `dna-p' and `nuc-p'."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((case-fold-search t))           ; enable case insensitive search
    (save-excursion
      (goto-char beg)
      (search-forward "u" end t))))
      ;; (re-search-forward "[uU]" end t))))

(defun dna-p (beg end)
  "Return the point of 't' or 'T' if they are found; otherwise return nil.
See also `rna-p' and `nuc-p'."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((case-fold-search t))
    (save-excursion
      (goto-char beg)
      (search-forward "t" end t))))


(defun base-complement-lookup (base)
  "Look up the complement of the BASE and print a message.
See `dna-base-complement'."
  (interactive "cComplement of base:")
  (if (equal base ?u) (setq base ?t))
  (let ((c-base (or (elt dna-base-complement base)
                    ??)))     ; return '?' if there's no complement base
    (message "Complement of '%c' is '%c'." base c-base)))


(defun nuc-complement (beg end &optional is-rna)
  "Complement a region of bases from BEG to END.

Complement a region of the buffer by
inserting the complements, base by base, and by deleting the region.
Non-base char are passed over unchanged. By default, it will complement
to DNA sequence unless IS-RNA is true. C-u \\[nuc-complement] will complement
as RNA while as DNA without C-u"
  (interactive "r\nP")
  (let* ((t-exist (dna-p beg end))
         (u-exist (rna-p beg end))
         (complement-vector
          (cond ((and t-exist is-rna)
                 (error "T (at %d) exist!" t-exist))
                ((and u-exist is-rna) rna-base-complement)
                ((and t-exist (not is-rna)) dna-base-complement)
                ((and u-exist (not is-rna))
                 (error "U (at %d) exist!" u-exist))))
          base c-base)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (setq base (following-char))
        (setq c-base (aref complement-vector base))
        (insert (if c-base c-base base))
        (delete-char 1)))))


;;;###autoload
(defun nuc-reverse-complement (beg end &optional is-rna)
  "Reverse complement a region of DNA (unless IS-RNA is true) from BEG to END.
Works by deleting the region and inserting bases reversed
and complemented, base by base while entering non-bases in the order
found. This function has some code redundancy with
`nuc-complement'."
  (interactive "r\nP")
  (let* ((t-exist (dna-p beg end))
         (u-exist (rna-p beg end))
         (complement-vector
          (cond ((and t-exist is-rna)
                 (error "T (at %d) exist!" t-exist))
                ((and u-exist is-rna) rna-base-complement)
                ((and t-exist (not is-rna)) dna-base-complement)
                ((and u-exist (not is-rna))
                 (error "U (at %d) exist!" u-exist))))
         (str-len (- end beg))
         (old-pos (point))
         base c-base)
    (goto-char end)
    (dotimes (x str-len)
      (let (current-char)
        (save-excursion
          (goto-char (- end x 1))
          (setq base (following-char)))
        (setq c-base (aref complement-vector base))
        (insert (if c-base c-base base))))
    (delete-region beg end)
    (goto-char old-pos)
    (if (/= old-pos end) (push-mark end nil t))))


(defalias 'nuc-rc 'nuc-reverse-complement)


;;;###autoload
(defun 2rna (beg end)
  "Convert to RNA. It basically convert 'u' to 't' ('U' to 'T') in the
sequence."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (goto-char beg)
    (while (search-forward "t" end t)
      (replace-match "u" nil t))))

(defun 2dna (beg end)
  "Convert to DNA. Similar to `2rna'."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (goto-char beg)
    (while (search-forward "u" end t)
      (replace-match "t" nil t))))


(defun nuc-base-summary (beg end)
  "Summarize the frequencies of bases in the region BEG and END or the current line.

See also `region-summary'."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (region-summary beg end nuc-base-regexp))



(defun seq-isearch-mangle-str (str)
  "Mangle the string STR into a regexp to search over cruft in sequence.
Inserts a regexp between each base which matches sequence formatting cruft.
For example, if `seq-cruft-regexp' is '[ ]', the search string 'acgt' would be
transformed into 'a[ ]*c[ ]*g[ ]*t' and the search string containing IUPAC code
such as 'acrt' would be transformed into '[a][ ]*[c][ ]*[ag][ ]*[t]."
  ;; (mapconcat 'identity (split-string str "" t) (concat seq-cruft-regexp "*")))
  (let (degenerate-str-list  tmp)
    ;; 'ar' will return as ("[a]", "[ag]")
    (setq degenerate-str-list
          (mapcar #'(lambda (x)
                      (setq tmp (assoc x nuc-base-degeneracy))
                      (if (not tmp)
                          (error "%c is not a valid nuc base character!" x))
                      (concat "["  (mapconcat 'char-to-string (cdr tmp) "") "]"))
                  (string-to-list str)))
    ;; (print degenerate-str-list)
    (mapconcat 'identity degenerate-str-list (concat seq-cruft-regexp "*"))))


;; weighted homopolymer rate (WHR)
(defun nuc-whr (beg end)
  ""
)


;;; Per base colors

(defvar nuc-base-colors
  (mapcar* #'cons
           (append nuc-base (mapcar #'upcase nuc-base))
           (setcdr (last color-pairs) color-pairs))
  "Background and foreground colors for each IUPAC bases.

This is a list of lists. For each inner list, it contains 3 atoms:
a nuc base in char type, hex-code colors for foreground and background")


;; define base faces belonging to base-face group
(let ((letcol-alist nuc-base-colors))
  (loop for elem in letcol-alist
        for f = (nth 1 elem)
        for b = (nth 2 elem)
        for l = (format "%c" (nth 0 elem)) do
        (eval (macroexpand `(def-char-face ,l ,b ,f "base-face")))))


;;;###autoload
(defun nuc-base-paint-region (beg end &optional case)
  "Color the nucleic acid region BEG to END.

If CASE is nil, upcase and lowercase base chars will be colored the same;
otherwise, not. See `paint-seq-region' for details."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (paint-seq-region beg end "base-face" case))

;;;###autoload
(defalias 'unpaint-nuc-base-region 'unpaint-seq-region)


(defvar nuc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cf"     'nuc-move-forward)
    (define-key map "\C-cb"     'nuc-move-backward)
    (define-key map "\C-c\C-d"   'nuc-delete-forward)
    (define-key map "\C-cd"     'nuc-detete-backward)
    (define-key map "\C-cp"     'nuc-base-paint-region)
    (define-key map "\C-cr"     'nuc-rc)
    (define-key map "\C-c#"     'nuc-base-summary)
    map)
  "Keymap for `nuc-mode'.")

(define-minor-mode nuc-mode
  ;; the fun auto defines a buffer local variable `nuc-mode'
  ;; and this key value set its initial value
  :init-value nil
  "Nucleic acid mode"
  ;; the name, a string, to show in the modeline
  :lighter " nuc"
  ;; keymap
  :keymap nuc-mode-map
  :global t)

(provide 'nuc-mode)

;;; nuc-mode.el ends here
