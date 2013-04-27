;;; nuc-mode.el --- a minor mode for editing nucleic acid sequences
;;
;;; Commentary:
;; * A collection of functions for editing DNA and RNA sequences.

;;; Installation:
;; --------------------
;; Here are two suggested ways for installing this package.
;; You can choose to autoload it when needed, or load it
;; each time emacs is started.  Put one of the following
;; sections in your .emacs:
;;
;; ---Autoload:
;;  (autoload 'dna-mode "dna-mode" "Major mode for dna" t)
;;  (add-to-list 'magic-mode-alist '("^>\\|ID\\|LOCUS\\|DNA" . dna-mode))
;;  (add-to-list
;;     'auto-mode-alist
;;     '("\\.\\(fasta\\|fa\\|exp\\|ace\\|gb\\)\\'" . dna-mode))
;;  (add-hook 'dna-mode-hook 'turn-on-font-lock)

;;;;;; USER CUSTOMIZABLE VARIABLES START HERE
;; valid characters in the sequences
(defvar seq-gap "-."
  "*Chars that represent a gap")

(defvar seq-space " \t\n"
  "*Chars that represent cruft which may appear between bases.
 It will be skipped during moving and search and anything involving counting bases.")

(defvar nuc-other "x"
  "*Other chars that can possibly exist in a sequence. It should be in lower
case as the upper case will be added automatically. Please modify
`nuc-degeneracy-list' and `dna-complement-list' accordingly")

(defvar nuc-degeneracy-list
  '((?a  ?a)
    (?c  ?c)
    (?g  ?g)
    (?t  ?t)
    (?u  ?u)
    (?m  ?a ?c)
    (?y  ?c ?t)
    (?r  ?a ?g)
    (?w  ?a ?t)
    (?s  ?c ?g)
    (?k  ?g ?t)
    (?v  ?a ?c ?g)
    (?b  ?c ?g ?t)
    (?h  ?a ?c ?t)
    (?d  ?a ?g ?t)
    (?n  ?a ?t ?g ?c)
    (?x  ?a ?t ?g ?c))
  "*A association list showing the degeneracy of the bases. Only for lowercase,
as the upcased will be added automatically.")

(defvar dna-complement-list
  '((?n . ?n) (?x . ?x) ; identity
    (?t . ?a) (?a . ?t) (?c . ?g) (?g . ?c)           ; single
    (?m . ?k) (?r . ?y) (?w . ?w) (?s . ?s) (?y . ?r) (?k . ?m) ; double
    (?v . ?b) (?b . ?v) (?h . ?d) (?d . ?h))  ; triple
  "*List of bases and their complements. Bases should be lowercase,
as the upcased will be added when the vector is made.")

;;;;; END OF USER CUSTOMIZABLE VARIABLES

(defvar nuc-iupac-base "acgtumrwsykvhdbn"
  "All char for a single base, following IUPAC code. It should be in lower case
as the upper case will be added automatically.")

(defvar nuc-base-regexp
  (let ((nuc-base (concat nuc-iupac-base nuc-other)))
    (regexp-opt (mapcar #'char-to-string
                        (concat nuc-base (upcase nuc-base)))))
  "A regexp that matches a valid nucleotide base (following IPUAC code plus
the symbol defined in `nuc-other'.")

(defvar seq-cruft-regexp
  (regexp-opt (mapcar #'char-to-string
                      (concat seq-gap seq-space)))
  "A regexp that matches cruft.")

(defvar nuc-degeneracy
  (let ((nuc-degen nuc-degeneracy-list))
    (dolist (element nuc-degeneracy-list)
      (setq nuc-degen (append nuc-degen (list (mapcar 'upcase element)))))
  nuc-degen)
  "This is association list with keys being IUPAC code and values being the
bases the code represents. This includes uppercase bases into
`nuc-degeneracy-list'.")

(defvar dna-base-complement
  (let ((c-vec (make-vector 256 nil)))  ; all alphabets chars are < 256
    (dolist (element dna-complement-list)
      (aset c-vec (car element) (cdr element))
      (aset c-vec (upcase (car element)) (upcase (cdr element))))
    c-vec)
  "A vector of complements of upper and lower case bases.
 dna-base-complement[base] returns the complement of the base. see also
`rna-base-complement'.")

(defvar rna-base-complement
  (let ((c-vec dna-base-complement))
    (aset c-vec ?a ?u)
    (aset c-vec ?A ?U)
    (aset c-vec ?t nil)
    (aset c-vec ?T nil)
    c-vec)
  "A vector of upper and lower case bases and their complements.
 rna-base-complement[base] returns the complement of the base. see also
`dna-base-complement'.")


(defun proceed-char-repeatedly (count func legal-char-regexp)
  "Run the FUNC function for COUNT times repeatedly from the point on,
if the next char belongs to LEGAL-CHAR-REGEXP. COUNT can be either positive
or negative integer, indicating the proceeding direction."
  (let ((direction (if (< count 0) -1 1)))
    (fset 'looking (if (< count 0) 'looking-back 'looking-at-p))
    (dotimes (x (abs count))
      (while (looking seq-cruft-regexp)
        (funcall func direction))
      (if (looking legal-char-regexp)
          (funcall func direction)
        (error "Illegal char found! Moved %d bases" (* direction x))))
    count))

;;; move
(defun nuc-move-forward (count)
  "Move forward COUNT bases. Move backward if negative.
Skip `seq-cruft-regexp' but stop on the illegal base
and report how many bases the point have been moved by.
COUNT can be either positive or negative, indicating the
moving direction. Return the number of bases that are moved thru.
See `proceed-char-repeatedly'"
  (interactive "p")
  (proceed-char-repeatedly count 'forward-char nuc-base-regexp))

(defun nuc-move-backward (count)
  "Move backward COUNT bases, similar to `nuc-move-forward'. See also
 `proceed-char-repeatedly'."
  (interactive "p")
  ;; (proceed-char-repeatedly count 'backward-char))
  (proceed-char-repeatedly (- count) 'forward-char nuc-base-regexp))

;;; delete
(defun nuc-delete-forward (count)
  "Delete COUNT number of bases starting from the point, similar to
`nuc-move-forward' (just use delete instead of move)."
  (interactive "p")
  (proceed-char-repeatedly count 'delete-char nuc-base-regexp))

(defun nuc-delete-backward (count)
  "Delete backward COUNT number of bases from the point, similar to
`nuc-move-forward' (just use delete backward instead of move forward).
See `nuc-delete-forward' and `proceed-char-repeatedly'."
  (interactive "p")
  (proceed-char-repeatedly (- count) 'delete-char nuc-base-regexp))


(defun seq-p (beg end legal-char-regexp)
  "Test if the region between BEG and END is a legal
 sequence defined by LEGAL-CHAR-REGEXP and `seq-cruft-regexp'.
 Return the count if the region contains only legal characters;
 otherwise return nil and
 report the location of the invalid characters."
  (let ((count 0) (legal-p nil))
    (save-excursion
      (goto-char beg)
      ;; (point) will not equal `end' if invalid char is met.
      ;; Using forward-char to check char one-by-one has the advantage of
      ;; negligible memory requirement.
      (setq legal-p (dotimes (x (- end beg) (= (point) end))
                      (cond ((looking-at-p legal-char-regexp)
                             (forward-char)
                             (setq count (1+ count)))
                            ((looking-at-p seq-cruft-regexp)
                             (forward-char))
                            (t (setq x end) ; end the while loop
                               (message "Bad base '%c' found at position %d,%d"
                                        (following-char)
                                        (line-number-at-pos)
                                        (current-column)))))))
    (if legal-p count)))

(defun nuc-p (beg end)
  "Test if the region between BEG and END (or the line) is a legal nucleotide
acid sequence. Return the count if the region
 contains only legal nucleic acid characters, including
 `nuc-base-regexp', `seq-cruft-regexp'; otherwise return nil and
 report the location of the invalid characters."
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
  (let ((c-base (or (elt dna-base-complement base)
                    ??)))     ; return '?' if there's no complement base
        (message "Complement of '%c' is '%c'." base c-base)))

(defun nuc-complement (beg end)
  "Complement a region of bases from BEG to END. If no active region,
complement the current line.Complement a region of the buffer by deleting it and
inserting the complements, base by base. Non-base (see `nuc-complement-list' are
passed over unchanged.

   The cool thing is that the point will remain in the same position and the
highlighted region is still highlighted after the complement."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((deactivate-mark nil)    ; required to keep the marked region hilighted
        (regionTypeP (= (point) beg))
        (oldPoint (point))
        complement-vector r-string r-length r-base r-cbase)
    (setq complement-vector
          (cond ((and (rna-p beg end) (dna-p beg end))
                 (error "both U (at %d) and T (at %d) exist!"
                        (rna-p beg end) (dna-p beg end))
                 ((rna-p beg end) rna-base-complement)
                 (t dna-base-complement))))

    (setq r-string (buffer-substring-no-properties beg end))
    (setq r-length (length r-string))
    (delete-region beg end)
    (dotimes (r-point r-length)
      (setq r-base (aref r-string r-point))
      (setq r-cbase (aref complement-vector r-base))
      (insert (if r-cbase r-cbase r-base)))
    (if regionTypeP
        (push-mark end nil t))                 ; keep the region marked
    (goto-char oldPoint)))      ; keep the point at the same previous position

;;;###autoload
(defun 2rna (beg end)
  "Convert to RNA. It basically convert 'u' to 't' ('U' to 'T') in the
sequence."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((case-replace t)
        (case-fold-search t))
    (save-excursion
      (replace-string "t" "u" nil beg end))))

(defun 2dna (beg end)
  "Convert to DNA. Similar to `2rna'."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((case-replace t)                ; these 2 variables preserve case
        (case-fold-search t))           ; in replace-string
    (save-excursion
      (replace-string "u" "t" nil beg end))))

(defun region-summary (beg end)
  "Count and print the number of all the characters in the region. Use hash
table to create dictionary-like data type."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((myHash (make-hash-table :test 'equal))
        currentChar
        currentCount)
    (save-excursion
      (goto-char beg)
      (dotimes (x (- end beg))
        (setq currentChar (char-after))
        (setq currentCount (gethash currentChar myHash))
        (if currentCount
            (puthash currentChar (1+ currentCount) myHash)
          (puthash currentChar 1 myHash))
        (forward-char)))
    (maphash (lambda (x y) (princ (format "%c:%d " x y))) myHash)))

(defun nuc-reverse-complement (beg end)
  "Reverse complement a region of dna from BEG to END.
Works by deleting the region and inserting bases reversed
and complemented, while entering non-bases in the order
found. This function has some code redundancy with
`nuc-reverse-complement'."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((deactivate-mark nil)          ; required to keep the marked region hilighted
        (regionTypeP (= (point) end))
        (oldPoint (point))
        complement-vector r-string r-length r-base r-cbase)
    (setq complement-vector
          (cond ((and (rna-p) (dna-p))
                 (error "both U (or u at pos %d) and T (or t at pos %d) exist!"
                        (rna-p) (dna-p))
                 ((rna-p) rna-base-complement)
                 (t dna-base-complement))))

    (setq r-string (buffer-substring-no-properties beg end))
    ;; reverse r-string
    (setq r-string (apply 'string (reverse (string-to-list r-string))))
    (setq r-length (length r-string))
    (delete-region beg end)
    (dotimes (r-point r-length)
      (setq r-base (aref r-string r-point))
      (setq r-cbase (nuc-complement-base r-base))
      (insert (if r-cbase r-cbase r-base)))
    (if regionTypeP
        (push-mark end nil t))  ; keep the region marked
    (goto-char (+ beg (- end oldPoint)))))      ; keep the point at the same previous position

(defalias 'nuc-rc 'nuc-reverse-complement)


;;;;;; isearch motif

(defun seq-isearch-mangle-str (str)
  "Mangle the string STR into a regexp to search over cruft in sequence.
Inserts a regexp between each base which matches sequence formatting cruft.
For example, if `seq-cruft-regexp' is '[ ]', the search string 'acgt' would be
transformed into 'a[ ]*c[ ]*g[ ]*t' and the search string containing IUPAC code
such as 'acrt' would be transformed into '[a][ ]*[c][ ]*[ag][ ]*[t]."
  ;; (mapconcat 'identity (split-string str "" t) (concat seq-cruft-regexp "*")))
  (let ((char-list (string-to-list str))
        degenerate-str-list)
    ;; 'ar' will return as ("[a]", "[ag]")
    (setq degenerate-str-list
          (mapcar '(lambda (x)
                     (let ((just-a-var))
                       (setq just-a-var (assoc x nuc-degeneracy))
                       (if (not just-a-var)
                           (error "%c is not a valid IUPAC code in nuc-degeneracy!" x))
                       (concat "["  (mapconcat 'char-to-string
                                               (cdr just-a-var) "")
                               "]")))   ; end of lambda
                  char-list))
    ;; (print char-list)
    ;; (print degenerate-str-list)
    (mapconcat 'identity degenerate-str-list (concat seq-cruft-regexp "*"))))

;; (defun seq-isearch-transform-string ()
;;   (interactive)
;;   (let* ((string (seq-isearch-mangle-str isearch-string)))
;;     (setq isearch-string string
;;           isearch-message (mapconcat 'isearch-text-char-description string ""))
;;     (isearch-search-and-update)))

;; (define-key isearch-mode-map (kbd "C-c C-t") 'seq-isearch-transform-string)


(defun seq-isearch-forward (motif &optional bound noerror)
  "Search forward for MOTIF."
  (let ((string (seq-isearch-mangle-str motif)))
    (re-search-forward string bound noerror)))

(defun seq-isearch-backward (motif &optional bound noerror)
  "Search backward for MOTIF."
  (let ((string (seq-isearch-mangle-str motif)))
    (re-search-backward string bound noerror)))

(defadvice isearch-message-prefix (after seq-isearch-ismp)
  "Set the isearch prompt string to show dna search is active.
This serves as a warning that the string is being mangled."
  (setq ad-return-value (concat "MOTIF " ad-return-value)))

(defvar seq-isearch-p t)

(defun toggle-seq-isearch ()
  (interactive)
  (cond (seq-isearch-p
         (setq seq-isearch-p nil)
         (message "motif isearch is off")
         (ad-disable-advice 'isearch-message-prefix 'around 'seq-isearch-ismp))
        (t (setq seq-isearch-p t)
           (message "motif isearch is on")
           (ad-enable-advice 'isearch-message-prefix 'around 'seq-isearch-ismp)))
  ;; in case there are other advices.
  (ad-activate 'isearch-message-prefix))

(defun seq-isearch-search-fun ()
  "Set to `isearch-search-fun-function' when `nuc-mode' is
  enabled."
  (if seq-isearch-p
      (if isearch-forward 'seq-isearch-forward 'seq-isearch-backward)
    (isearch-search-fun-default)))

(setq isearch-search-fun-function 'seq-isearch-search-fun)

;;;;;;;;;;;;;;;;; paint
;;; Per base colors
(defun dna-base-color-make-faces (beg end &optional force)
  "Build a face to display bases with.  FORCE remakes the faces."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (when (or (not (facep 'dna-face-t)) force)
    (let ((base-list '("a" "c" "g" "t"))
          base
          base-face)
      (while base-list
        (setq base (car base-list))
        (setq base-face (intern (concat "dna-base-face-" base)))
        (make-face base-face)
        (set-face-foreground
         base-face (symbol-value (intern (concat "dna-base-color-" base))))
        (setq base-list (cdr base-list))))))


(defvar dna-color-bases-auto t
  "Automaticly deactivate option `font-lock-mode' when `dna-color-bases' is run.
See dna-color-bases for details.")
;; (setq dna-color-bases-auto t)

(defun dna-color-bases-region (beg end)
  "Color the bases in the region BEG to END.
NOTE: The function `font-lock-mode' will undo the work of this
function if activated.  Disable it before using this
function.  If `dna-color-bases-auto' is set then option `font-lock-mode'
is deactivated automatically."
  (interactive "r")
  (if (and dna-color-bases-auto font-lock-mode)
    (font-lock-mode -1))
  (if font-lock-mode
    (error "Font-lock-mode is on -- deactivate it"))
  (save-excursion
    (let (c)
      (goto-char s)
      (while (< s e)
        (setq c (downcase (char-after s)))
        (cond
         ((eq c ?a)
          (set-text-properties s (+ s 1) '(face dna-base-face-a)))
         ((eq c ?c)
          (set-text-properties s (+ s 1) '(face dna-base-face-c)))
         ((eq c ?g)
          (set-text-properties s (+ s 1) '(face dna-base-face-g)))
         ((eq c ?t)
          (set-text-properties s (+ s 1) '(face dna-base-face-t)))
         (t nil))
        (setq s (+ s 1))))))

(defun uncolor-region (beg end)
  "Uncolor the bases from BEG to END or the current line."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (remove-text-properties beg end '(face nil)))


(provide 'nuc-mode)

;;; nuc-mode.el ends here
