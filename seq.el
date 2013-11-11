;;; seq.el --- functions and variables shared by nuc-mode.el and
;;; aa-mode.el.

(require 'color)


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


;; valid characters as alignment gaps in the sequences
(defvar seq-gap
  '(?- ?.)
  "*Chars that represent a gap.")

(defvar seq-space
  '(?  ?\t ?\n)
  "*Chars that represent cruft which may appear between bases or amino acid.

It will be skipped during moving and search and anything involving counting.")

(defvar seq-space-regexp
  (regexp-opt (mapcar #'char-to-string seq-space))
  "A regexp that matches white spaces.")

(defvar seq-cruft-regexp
  (regexp-opt (mapcar #'char-to-string
                      (concat seq-gap seq-space)))
  "A regexp that matches cruft, including `seq-gap' and `seq-space'.")


(defun proceed-char-repeatedly (count func legal-char-regexp)
  "Run the FUNC function for COUNT times repeatedly from the point on.

The next char has to belong to LEGAL-CHAR-REGEXP; otherwise, stop and report
error. The `seq-cruft-regexp' char will be skipped, i.e., not counted.
 Return the actual count of legal char. COUNT can be either positive
or negative integer, indicating the proceeding direction."
  (let ((direction (if (< count 0) -1 1)))
    (fset 'looking (if (< count 0) 'looking-back 'looking-at))
    (dotimes (x (abs count))
      (while (looking seq-cruft-regexp)
        (funcall func direction))
      (if (looking legal-char-regexp)
          (funcall func direction)
        (error "Illegal char found! Moved %d bases" (* direction x))))
    count))


(defun region-summary (beg end &optional legal-char-regexp)
  "Count the number of all the characters in the region.

Ignore char that does not belong to LEGAL-CHAR-REGEXP. Use hash
table to create dictionary-like data type. Return the hash table."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((my-hash (make-hash-table :test 'equal))
        char count)
    ;; any char is allowed if legal char is not provided
    (if (not legal-char-regexp)
        (setq legal-char-regexp "."))
    (save-excursion
      (goto-char beg)
      (dotimes (x (- end beg))
        (if (looking-at legal-char-regexp)
            (progn (setq char (char-after))
                   (setq count (gethash char my-hash))
                   (if count
                       (puthash char (1+ count) my-hash)
                     (puthash char 1 my-hash))))
        (forward-char)))
    my-hash))


(defun seq-count (beg end &optional legal-char-regexp)
  "Count the chars that belong to LEGAL-CHAR-REGEXP.

Chars of `seq-cruft-regexp' will be skipped. Return the count if
the region contains only legal characters; otherwise return nil and
report the location of the invalid characters. This function is used
by `nuc-count' and `pro-count'."
  (let ((count 0) (legal-p t))
    ;; allow any char if legal-char-regexp is not provided
    (or legal-char-regexp
        (setq legal-char-regexp "."))
    (save-excursion
      (goto-char beg)
      ;; (point) will not equal `end' if invalid char is met.
      ;; Using forward-char to check char one-by-one has the advantage of
      ;; negligible memory requirement.
      (while (< (point) end)
        (cond ((looking-at seq-cruft-regexp)
               (forward-char))
              ((looking-at legal-char-regexp)
               (forward-char)
               (setq count (1+ count)))
              (t (message "Bad char '%c' found at position %d,%d"
                          (char-after)
                          (line-number-at-pos)
                          (current-column))
                 (setq legal-p nil)
                 (goto-char end)))))
    (if legal-p count)))

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
                    (loop for i from start to stop by incremental
                          collect i)))))


(defvar color-pairs
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

(defun seq-paint (beg end face-prefix &optional case)
  "Color the sequences in the region BEG to END.

If CASE is nil, upcase and lowercase chars will be colored the same;
otherwise, not. FACE-PREFIX decides which face groups ('base-face' or
'aa-face') to use."
  (save-excursion
    (let (char face)
      (goto-char beg)
      (while (< (point) end)
        (setq char (char-after))
        (if case
            (setq face (format "%s-%c" face-prefix char))
          ;; let upcase base use the color of lowercase base color
          (setq face (format "%s-%c" face-prefix (upcase char))))
        (if (facep face)
            ;; use font-lock-face instead of face for font-lock-mode is enabled
            (silent-put-text-property beg (+ beg 1)
                                      'font-lock-face
                                      (intern face))
          (error "Face '%s' does not exist." face))
        (forward-char)))))

(defun seq-unpaint (beg end)
  "Uncolor the sequences from BEG to END or the current line."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (remove-text-properties beg end '(font-lock-face nil)))


;;;;;; isearch motif


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
  "Modify the isearch prompt string to show seq search is active.

This serves as a warning that the string is being mangled."
  (setq ad-return-value (concat "MOTIF " ad-return-value)))

(defvar seq-isearch-p nil)

(defun seq-toggle-isearch ()
  "Toggle the sequence isearch."
  (interactive)
  (cond (seq-isearch-p
         (setq seq-isearch-p nil)
         (message "motif isearch is off")
         (ad-disable-advice 'isearch-message-prefix 'after 'seq-isearch-ismp))
        (t (setq seq-isearch-p t)
           (message "motif isearch is on")
           (ad-enable-advice 'isearch-message-prefix 'after 'seq-isearch-ismp)))
  ;; in case there are other advices.
  (ad-activate 'isearch-message-prefix))

(defun seq-isearch-search-fun ()
  "Set to `isearch-search-fun-function'."
  (if seq-isearch-p
      (if isearch-forward 'seq-isearch-forward 'seq-isearch-backward)
    (isearch-search-fun-default)))

(setq isearch-search-fun-function 'seq-isearch-search-fun)


(defun hash-alist (alist)
  "Convert association list to a hash table and return it.

The car will be the key and the cdr will be the value. If
there are multiple items with the same car, error will be
reported."
  (let ((my-hash (make-hash-table :test 'equal)))
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

(provide 'seq)
