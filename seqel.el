;;; seqel.el --- Util functions and variables

;; Copyright (C) 2021  Zech Xu

;; Author: Zech Xu
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))
;; License: BSD-3
;; URL: https://github.com/RNAer/seqel

;;; Commentary:

;; Utility functions and variables for both nucleotide and protein minor modes

;;; Code:


(require 'color)


;; valid characters as alignment gaps in the sequences
(defvar seqel-gap
  '(?- ?.)
  "*Chars of '.' and '-' that represent a gap.")

(defvar seqel-space
  '(?  ?\t ?\n ?\r)
  "*Chars of whitespaces that may appear in sequences.

It will be skipped during moving and search and anything
involving counting.")

(defvar seqel-cruft-regexp
  (regexp-opt (mapcar #'char-to-string
                      (concat seqel-gap seqel-space)))
  "A regexp that matches cruft, including `seqel-gap' and `seqel-space'.")

(defvar seqel-cruft-set
  (let ((alphabet-set (make-hash-table :test 'eq
                                       :size (+ (length seqel-gap)
                                                (length seqel-space)))))
    (mapc (lambda (i) (puthash i t alphabet-set)) seqel-gap)
    (mapc (lambda (i) (puthash i t alphabet-set)) seqel-space)
    alphabet-set)
  "The set of alphabets for `seqel-gap' and `seqel-space' in sequences.

This is a hash table: keys are char and values are t. It serves
like a set object similar in Python language.")


(defun seqel-region-or-line ()
  "Return (BEG END) positions of region (if active) or current line."
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (list (line-beginning-position) (line-end-position))))


(defun seqel-forward-char (count legal-alphbet-set)
  "Move the cursor for COUNT times repeatedly from the point on.

The next char has to belong to LEGAL-ALPHBET-SET to be
 counted.  The `seqel-cruft-regexp' char will be skipped, i.e., not
 counted.  Return the actual count of legal char.  COUNT can be
 either positive or negative integer - if it is positive, move
 forward; if it is negative, move backward; if zero, don't move."

  (let ((direction (if (< count 0) -1 1))
        (fetch-char (if (< count 0) 'char-before 'char-after)))
    (dotimes (x (abs count))
      (while (gethash (funcall fetch-char) seqel-cruft-set)
        (forward-char direction))
      (if (gethash (funcall fetch-char) legal-alphbet-set)
          (forward-char direction)
        (error "Failed! Moved %d bases.  You have illegal char here!" (* direction x))))
    count))


(defun seqel-summary (beg end &optional legal-char-set)
  "Count the number of all the characters in the region.

Summarize the sequence marked between BEG and END.  Ignore char
that does not belong to LEGAL-CHAR-SET.  Use hash table to create
dictionary-like data type.  Return the hash table.  This is
fast (only 2 seconds for 5M base pairs)."
  (interactive (seqel-region-or-line))
  (let (my-hash size char count)
    (if legal-char-set
        (progn (setq my-hash (make-hash-table :test 'equal :size (hash-table-count legal-char-set)))
               (maphash (lambda (k v) (puthash k 0 my-hash)) legal-char-set))
      ;; any char is allowed if legal char is not provided
      (progn (setq my-hash (make-hash-table :test 'equal :size 255))
             (mapc (lambda (i) (puthash i 0 my-hash)) (number-sequence 0 255))))
    (save-excursion
      (goto-char beg)
      (dotimes (x (- end beg))
        (setq char (char-after))
        (if (or (not legal-char-set)
                (gethash char legal-char-set))
            (puthash char (1+ (gethash char my-hash)) my-hash))
        (forward-char))
      ;; print out the summary table
      (maphash (lambda (x y) (or (= y 0) (princ (format "%c:%d " x y) t))) my-hash)
      my-hash)))


(defun seqel-count (beg end &optional legal-char-set)
  "Count the chars that belong to LEGAL-CHAR-REGEXP.

Count the residues of sequence region marked between BEG and
END.  Chars of `seqel-cruft-regexp' will be skipped.  Return the
count if the region contains only legal characters (if
LEGAL-CHAR-SET is provided); otherwise return nil and report the
location of the invalid characters.  This function is used by
`nuc-count' and `seqel-pro-count'."
  (let ((count 0) char)
    (save-excursion
      (goto-char beg)
      (dotimes (i (- end beg))
        (setq char (char-after))
        (cond ((gethash char legal-char-set) (setq count (1+ count)))
              ;; allow any char if legal-char-set is not provided
              ((and legal-char-set
                    (not (gethash char seqel-cruft-set)))
               (error "Bad char '%c' found at line %d column %d"
                      char (line-number-at-pos) (current-column))))
        (forward-char)))
    count))

(defun seqel-color-gradient-hsl (start stop step-number &optional s l)
  "Return a list with (STEP-NUMBER + 1) number of colors in hex code.

START and STOP should are the start and ending hue in the color gradient
to create.  And S and L are saturation and lightness.

For example, \"(seqel-color-gradient-hsl 0 0.333 20)\" will produce color
gradient from red to yellow to green. Please be aware that there is a
`color-gradient' function defined in color.el, which produces color
gradient in RGB scales (for the example here, it will create gradient
from red to green without yellow) besides other differences."
  (let* ((incremental (/ (- stop start) step-number)))
    (or s (setq s -1))
    (or l (setq l 128))
    (mapcar #'(lambda (rgb) (format "#%02x%02x%02x"
                                    (nth 0 rgb)
                                    (nth 1 rgb)
                                    (nth 2 rgb)))
            (mapcar #'(lambda (hue) (color-hsl-to-rgb hue s l))
                    (number-sequence start stop incremental)))))


(defvar seqel-color-pairs
  '(("#ffffff" "#000000")  ; white    on black
    ("#ff0000" "#000000")  ; red
    ("#00ff00" "#000000")  ; green
    ("#00ffff" "#000000")  ; cyan
    ("#ff00ff" "#000000")  ; magenta
    ("#ffff00" "#000000")  ; yellow
    ("#ff6600" "#000000")  ; orange
    ("#0066ff" "#000000")  ; ~blue
    ("#000000" "#00ff00")  ; on green
    ("#ff0000" "#00ff00")
    ("#ffff00" "#00ff00")
    ("#000000" "#00aaff")  ; on ~blue
    ("#ffffff" "#00aaff")
    ("#00ff00" "#00aaff")
    ("#000000" "#ff0000")  ; on red
    ("#00ff00" "#ff0000")
    ("#ffffff" "#ff0000")
    ("#000000" "#ff00ff")  ; on magenta
    ("#ffffff" "#ff00ff")
    ("#000000" "#ffff00")  ; on yellow
    ("#ff0000" "#ffff00")
    ("#000000" "#00ffff")  ; on cyan
    ("#ff0000" "#00ffff")
    ("#000000" "#ffffff")  ; black    on white
    ("#ff0000" "#ffffff")  ; red
    ("#0000ff" "#ffffff")  ; blue
    ("#ff00ff" "#ffffff")  ; magenta
    ("#00ffff" "#ffffff")  ; cyan
    )
  "Color pairs that pass WCAG AAA test.

The first one is the text color and the second is the background.")

(defvar seqel-color-pairs-cycle
  (setcdr (last seqel-color-pairs) seqel-color-pairs)
    "Color pairs that pass WCAG AAA test.

The first one is the text color and the second is the background.")


(defmacro seqel--def-char-face (letter backgrnd foregrnd grp)
  "A macro used to define faces.

This will define a face named GRP-LETTER for character LETTER
that belongs to the face group named GRP, with BACKGRND as
background and FOREGRND as foreground colors."
  `(defface ,(intern (concat grp "-" letter))
     '((((type tty) (class color))
        (:background ,backgrnd :foreground ,foregrnd))
       (((type tty) (class color)) (:inverse-video t))
       (((class color) (background dark))
        (:background ,backgrnd :foreground ,foregrnd))
       (((class color) (background light))
        (:background ,backgrnd :foreground ,foregrnd))
       (t (:background "gray")))
     ,(concat "Face for marking up " (upcase letter) "'s")))


(defun seqel-paint (beg end face-group &optional case)
  "Color the sequences in the region BEG to END.

If CASE is nil, upcase and lowercase chars will be colored the same;
otherwise, not.  FACE-GROUP decides which face groups ('base-face' or
'aa-face') to use.

TODO: this is slow for long sequences."
  (save-excursion
    (let (char face)
      (goto-char beg)
      (dotimes (i (- end beg))
        (setq char (char-after))
        ;; skip whitespaces and gap symbols
        (if (not (gethash char seqel-cruft-set))
            (progn (if case
                       (setq face (format "%s-%c" face-group char))
                     ;; let upcase base use the color of lowercase base color
                     (setq face (format "%s-%c" face-group (upcase char))))
                   ;; use font-lock-face instead of face for font-lock-mode is enabled
                   (with-silent-modifications
                     (put-text-property (+ beg i) (+ beg i 1) 'font-lock-face (intern face)))))
        (forward-char)))))

(defun seqel-unpaint (beg end)
  "Uncolor the sequences from BEG to END or the current line."
  (interactive (seqel-region-or-line))
  (with-silent-modifications
    (remove-text-properties beg end '(font-lock-face nil))))


;;;;;; isearch pattern

;; (defun bioseq-isearch-transform-string ()
;;   (interactive)
;;   (let* ((string (seqel-isearch-mangle-str isearch-string)))
;;     (setq isearch-string string
;;           isearch-message (mapconcat 'isearch-text-char-description string ""))
;;     (isearch-search-and-update)))

;; (define-key isearch-mode-map (kbd "C-c C-t") 'bioseq-isearch-transform-string)


(defun seqel-isearch-mangle-str (str)
  "Mangle the string STR into a regexp to search over cruft in sequence.

Inserts a regexp between each base which matches sequence
formatting cruft, namely, you don't need to worry about if there
is any spaces separating between 'A' and 'T' if you'd like to
find all the 'AT's in the sequence.  More technically, if
`seqel-cruft-regexp' is '[ ]', the search string 'acgt' would be
transformed into 'a[ ]*c[ ]*g[ ]*t'."
  (mapconcat 'identity (split-string str "" 'omit-empty) (concat seqel-cruft-regexp "*")))


(defun seqel-isearch-forward (pattern &optional bound noerror)
  "Search forward for PATTERN.

BOUND and NOERROR passes to function `re-search-forward'."
  (let ((string (seqel-isearch-mangle-str pattern)))
    (re-search-forward string bound noerror)))

(defun seqel-isearch-backward (pattern &optional bound noerror)
  "Search backward for PATTERN.

BOUND and NOERROR passes to function `re-search-backward'."
  (let ((string (seqel-isearch-mangle-str pattern)))
    (re-search-backward string bound noerror)))

(defvar seqel-isearch-p nil
  "Whether biological sequence pattern isearch is enabled.")

(defun seqel-toggle-isearch ()
  "Toggle the sequence isearch."
  (interactive)
  (cond (seqel-isearch-p
         (setq seqel-isearch-p nil)
         (message "bio sequence pattern isearch is off"))
        (t (setq-local seqel-isearch-p t)
           (message "bio sequence pattern isearch is on"))))

(defun seqel--isearch-search-fun ()
  "This function will be assigned to `isearch-search-fun-function'."
  (if seqel-isearch-p
      (if isearch-forward 'seqel-isearch-forward 'seqel-isearch-backward)
    (isearch-search-fun-default)))

(setq isearch-search-fun-function 'seqel--isearch-search-fun)


(defun seqel-entry-forward (count entry-regexp)
  "Move forward to the beginning of next entry.

It works in the style of `forward-paragraph'.  COUNT needs to be
positive integer.  ENTRY-REGEXP defines the boundary of
entries.  Return current point if it moved over COUNT of entries;
otherwise return nil.  See also `seqel-fasta-forward'."
  (if (looking-at entry-regexp)
      (setq count (1+ count)))
  (if (< count 1)
      (error "The parameter COUNT should be positive integer!"))
  (if (re-search-forward entry-regexp nil 'move-to-point-max count)
      (progn (beginning-of-line) (point))
    nil))


(defun seqel-entry-backward (count entry-regexp)
  "Move the point to the beginning of previous entry.

It works in the style of `backward-paragraph'.  COUNT needs to be
positive integer.  ENTRY-REGEXP defines the boundary of entries.
Return current point if it moved over COUNT of entries; otherwise
return nil.  See also `seqel-fasta-backward'."
  (if (> count 0)
      (re-search-backward entry-regexp nil 'move-to-point-min count)
    (error "The argument COUNT should be positive integer!")))


(defun seqel-entry-last (entry-regexp)
  "Go to the beginning of last entry.

ENTRY-REGEXP defines the boundary of entries."
  ;; (while (seqel-entry-forward 1))
  (goto-char (point-max))
  (seqel-entry-backward 1 entry-regexp))


(defun seqel-entry-first (entry-regexp)
  "Go to the beginning of first entry.

ENTRY-REGEXP defines the boundary of entries."
  ;; (while (seqel-entry-backward 1)))
  (goto-char (point-min))
  (or (looking-at entry-regexp)
      (seqel-entry-forward 1 entry-regexp)))


(defun seqel-entry-count (entry-regexp)
  "Count the number of entries in the buffer.

ENTRY-REGEXP defines the boundary of entries."
  (let ((total 0))
    (save-excursion
      (goto-char (point-max))
      (while (seqel-entry-backward 1 entry-regexp)
        (setq total (1+ total))))
    (message "Total %d sequences." total)
    total))


(defun seqel-hash-alist (alist)
  "Convert association list ALIST to a hash table and return it.

The car will be the key and the cdr will be the value.  If
there are multiple items with the same car, error will be
reported."
  (let ((my-hash (make-hash-table :test 'equal :size (length alist))))
    (dolist (entry alist)
      (if (gethash (car entry) my-hash)
          (error "The same key already exists!"))
      (puthash (car entry) (cdr entry) my-hash))
    my-hash))

(defun seqel-hash-equal (hash1 hash2)
  "Compare 2 hash tables HASH1 and HASH2 to see whether they are equal."
  (and (= (hash-table-count hash1)
          (hash-table-count hash2))
       (catch 'flag
         (maphash (lambda (x y)
                    ;; (message "%c" x)
                    (or (equal (gethash x hash2) y)
                        (throw 'flag nil)))
                  hash1)
         (throw 'flag t))))


(defun seqel--zip (function &rest args)
  "Apply FUNCTION to successive cars of all ARGS.

Return the list of results.  This is similar to the Python zip function."
  ;; If no list is exhausted,
  (if (not (memq nil args))
      ;; apply function to cars.
      (cons (apply function (mapcar 'car args))
            (apply 'seqel--zip function
                   ;; Recurse for rest of elements.
                   (mapcar 'cdr args)))))


(provide 'seqel)

;;; seqel.el ends here
