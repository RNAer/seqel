;;; seq.el --- util functions and variables shared by nuc-mode.el and pro-mode.el.

;;; license: BSD-3

;;; Author: Zech Xu <zhenjiang dot xu at gmail dot com>


(require 'color)


;; valid characters as alignment gaps in the sequences
(defvar bioseq-gap
  '(?- ?.)
  "*Chars of '.' and '-' that represent a gap.")

(defvar bioseq-space
  '(?  ?\t ?\n ?\r ?\v ?\f)
  "*Chars that represent all kinds of spaces which may appear between bases or amino acids.

It will be skipped during moving and search and anything involving counting.")

(defvar bioseq-cruft-regexp
  (regexp-opt (mapcar #'char-to-string
                      (concat bioseq-gap bioseq-space)))
  "A regexp that matches cruft, including `bioseq-gap' and `bioseq-space'.")

(defvar bioseq-cruft-set
  (let ((alphabet-set (make-hash-table :test 'eq
                                       :size (+ (length bioseq-gap)
                                                (length bioseq-space)))))
    (mapc (lambda (i) (puthash i t alphabet-set)) bioseq-gap)
    (mapc (lambda (i) (puthash i t alphabet-set)) bioseq-space)
    alphabet-set)
  "The set of alphabets for `bioseq-gap' and `bioseq-space' in sequences.

This is a hash table: keys are char and values are `t'. It serves
like a set object similar in Python language.")


;; define a new error symbol
(put 'end-of-col-err
     'error-conditions
     ;; This error has three condition names:
     ;; 1) end-of-col-err, the narrowest classification;
     ;; 2) my-own-errors, which we imagine is a wider classification;
     ;; 3) and error, which is the widest of all.
     '(error my-own-errors end-of-col-err))
(put 'end-of-col-err
     'error-message
     "End of column error")

(defmacro interactive-region-or-line ()
  `(interactive
    (if (use-region-p) ; mark-active
        (list (region-beginning) (region-end))
      (list (line-beginning-position) (line-end-position)))))


(defun bioseq-forward-char (count legal-alphbet-set)
  "Move the cursor for COUNT times repeatedly from the point on.

The next char has to belong to LEGAL-ALPHBET-SET to be
 counted. The `bioseq-cruft-regexp' char will be skipped, i.e., not
 counted. Return the actual count of legal char. COUNT can be
 either positive or negative integer - if it is positive, move
 forward; if it is negative, move backward; if zero, don't move."

  (let ((direction (if (< count 0) -1 1))
        (fetch-char (if (< count 0) 'char-before 'char-after)))
    (dotimes (x (abs count))
      (while (gethash (funcall fetch-char) bioseq-cruft-set)
        (forward-char direction))
      (if (gethash (funcall fetch-char) legal-alphbet-set)
          (forward-char direction)
        (error "Failed! Moved %d bases. You have illegal char here." (* direction x))))
    count))


(defun bioseq-summary (beg end &optional legal-char-set)
  "Count the number of all the characters in the region.

Ignore char that does not belong to LEGAL-CHAR-SET. Use hash
table to create dictionary-like data type. Return the hash table.
This is fast (only 2 seconds for 5M base pairs)."
  (interactive-region-or-line)
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


(defun bioseq-count (beg end &optional legal-char-set)
  "Count the chars that belong to LEGAL-CHAR-REGEXP.

Chars of `bioseq-cruft-regexp' will be skipped. Return the count if
the region contains only legal characters; otherwise return nil and
report the location of the invalid characters. This function is used
by `nuc-count' and `pro-count'."
  (let ((count 0) char)
    (save-excursion
      (goto-char beg)
      (dotimes (i (- end beg))
        (setq char (char-after))
        (cond ((gethash char legal-char-set) (setq count (1+ count)))
              ;; allow any char if legal-char-set is not provided
              ((and legal-char-set
                    (not (gethash char bioseq-cruft-set)))
               (error "Bad char '%c' found at line %d column %d"
                      char (line-number-at-pos) (current-column))))
        (forward-char)))
    count))

(defun color-gradient-hsl (start stop step-number &optional s l)
  "Return a list with (STEP-NUMBER + 1) number of colors in hex code.

START and STOP should are the start and ending hue in the color gradient
to create. And S and L are saturation and lightness.

For example, \"(color-gradient-hsl 0 0.333 20)\" will produce color
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


(defvar color--pairs
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

(defvar color-pairs-cycle
  (setcdr (last color--pairs) color--pairs)
    "Color pairs that pass WCAG AAA test.

The first one is the text color and the second is the background.")


(defmacro def-char-face (letter backgrnd foregrnd grp)
  "A macro used to define faces.

This will define a face named GRP-LETTER that belongs to the
face group named GRP, with BACKGRND as background and FOREGRND
as foreground colors."
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


(defun bioseq-paint (beg end face-group &optional case)
  "Color the sequences in the region BEG to END.

If CASE is nil, upcase and lowercase chars will be colored the same;
otherwise, not. FACE-PREFIX decides which face groups ('base-face' or
'aa-face') to use.

TODO: this is slow for long sequences."
  (save-excursion
    (let (char face)
      (goto-char beg)
      (dotimes (i (- end beg))
        (setq char (char-after))
        ;; skip whitespaces and gap symbols
        (if (not (gethash char bioseq-cruft-set))
            (progn (if case
                       (setq face (format "%s-%c" face-group char))
                     ;; let upcase base use the color of lowercase base color
                     (setq face (format "%s-%c" face-group (upcase char))))
                   ;; use font-lock-face instead of face for font-lock-mode is enabled
                   (with-silent-modifications
                     (put-text-property (+ beg i) (+ beg i 1) 'font-lock-face (intern face)))))
        (forward-char)))))

(defun bioseq-unpaint (beg end)
  "Uncolor the sequences from BEG to END or the current line."
  (interactive-region-or-line)
  (with-silent-modifications
    (remove-text-properties beg end '(font-lock-face nil))))


;;;;;; isearch pattern

;; (defun bioseq-isearch-transform-string ()
;;   (interactive)
;;   (let* ((string (bioseq-isearch-mangle-str isearch-string)))
;;     (setq isearch-string string
;;           isearch-message (mapconcat 'isearch-text-char-description string ""))
;;     (isearch-search-and-update)))

;; (define-key isearch-mode-map (kbd "C-c C-t") 'bioseq-isearch-transform-string)


(defun bioseq-isearch-mangle-str (str)
  "Mangle the string STR into a regexp to search over cruft in sequence.

Inserts a regexp between each base which matches sequence
formatting cruft, namely, you don't need to worry about if there
is any spaces separating between 'A' and 'T' if you'd like to
find all the 'AT's in the sequence.  More technically, if
`bioseq-cruft-regexp' is '[ ]', the search string 'acgt' would be
transformed into 'a[ ]*c[ ]*g[ ]*t'."
  (mapconcat 'identity (split-string str "" 'omit-empty) (concat bioseq-cruft-regexp "*")))


(defun bioseq-isearch-forward (pattern &optional bound noerror)
  "Search forward for PATTERN."
  (let ((string (bioseq-isearch-mangle-str pattern)))
    (re-search-forward string bound noerror)))

(defun bioseq-isearch-backward (pattern &optional bound noerror)
  "Search backward for PATTERN."
  (let ((string (bioseq-isearch-mangle-str pattern)))
    (re-search-backward string bound noerror)))

(defadvice isearch-message-prefix (after bioseq-isearch-ismp)
  "Modify the isearch prompt string to show seq search is active.

This serves as a warning that the string is being mangled."
  (setq ad-return-value (concat "SEQ " ad-return-value)))

(defvar bioseq-isearch-p nil
  "Whether sequence pattern isearch is enabled")

(defun bioseq-toggle-isearch ()
  "Toggle the sequence isearch."
  (interactive)
  (cond (bioseq-isearch-p
         (setq bioseq-isearch-p nil)
         (message "sequence pattern isearch is off")
         (ad-disable-advice 'isearch-message-prefix 'after 'bioseq-isearch-ismp))
        (t (setq bioseq-isearch-p t)
           (message "sequence pattern isearch is on")
           (ad-enable-advice 'isearch-message-prefix 'after 'bioseq-isearch-ismp)))
  ;; in case there are other advices.
  (ad-activate 'isearch-message-prefix))

(defun bioseq-isearch-search-fun ()
  "Set to `isearch-search-fun-function'."
  (if bioseq-isearch-p
      (if isearch-forward 'bioseq-isearch-forward 'bioseq-isearch-backward)
    (isearch-search-fun-default)))

(setq isearch-search-fun-function 'bioseq-isearch-search-fun)


(defun entry-forward (count entry-regexp)
  "Move forward to the beginning of next entry.

It works in the style of `forward-paragraph'. Count need to be positive integer.
Return current point if it moved over COUNT of entries; otherwise return nil."
  (if (looking-at entry-regexp)
      (setq count (1+ count)))
  (if (< count 1)
      (error "The parameter COUNT should be positive integer."))
  (if (re-search-forward entry-regexp nil 'move-to-point-max count)
      (progn (beginning-of-line) (point))
    nil))


(defun entry-backward (count entry-regexp)
  "Move the point to the beginning of previous entry.

It works in the style of `backward-paragraph'. COUNT need to be positive integer.
Return current point if it moved over COUNT of entries; otherwise return nil."
  (if (> count 0)
      (re-search-backward entry-regexp nil 'move-to-point-min count)
    (error "The argument COUNT should be positive integer.")))

;;;###autoload
(defun entry-last (entry-regexp)
  "Go to the beginning of last entry."
  (interactive)
  ;; (while (entry-forward 1))
  (goto-char (point-max))
  (entry-backward 1 entry-regexp))

;;;###autoload
(defun entry-first (entry-regexp)
  "Go to the beginning of first entry."
  (interactive)
  ;; (while (entry-backward 1)))
  (goto-char (point-min))
  (or (looking-at entry-regexp)
      (entry-forward 1 entry-regexp)))

;;;###autoload
(defun entry-count (entry-regexp)
  "Count the number of entries in the buffer."
  (interactive)
  (let ((total 0))
    (save-excursion
      (goto-char (point-max))
      (while (entry-backward 1 entry-regexp)
        (setq total (1+ total))))
    (message "Total %d sequences." total)
    total))


(defun hash-alist (alist)
  "Convert association list to a hash table and return it.

The car will be the key and the cdr will be the value. If
there are multiple items with the same car, error will be
reported."
  (let ((my-hash (make-hash-table :test 'equal :size (length alist))))
    (dolist (entry alist)
      (if (gethash (car entry) my-hash)
          (error "repeat hashing"))
      (puthash (car entry) (cdr entry) my-hash))
    my-hash))

(defun hash-equal (hash1 hash2)
  "Compare two hash tables to see whether they are equal."
  (and (= (hash-table-count hash1)
          (hash-table-count hash2))
       (catch 'flag
         (maphash (lambda (x y)
                    ;; (message "%c" x)
                    (or (equal (gethash x hash2) y)
                        (throw 'flag nil)))
                  hash1)
         (throw 'flag t))))


(defun bioseq--zip (function &rest args)
  "Apply FUNCTION to successive cars of all ARGS.

Return the list of results. This is similar to the Python zip function."
  ;; If no list is exhausted,
  (if (not (memq nil args))
      ;; apply function to cars.
      (cons (apply function (mapcar 'car args))
            (apply 'bioseq--zip function
                   ;; Recurse for rest of elements.
                   (mapcar 'cdr args)))))



(provide 'bioseq)
