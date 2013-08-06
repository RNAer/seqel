;;; seq.el --- functions and variables shared by nuc-mode.el and
;; aa-mode.el.

(require 'color)

;; valid characters in the sequences
(defvar seq-gap "-."
  "*Chars that represent a gap")

(defvar seq-space " \t\n"
  "*Chars that represent cruft which may appear between bases.
 It will be skipped during moving and search and anything involving counting bases.")

(defvar seq-space-regexp
  (regexp-opt (mapcar #'char-to-string seq-space))
  "A regexp that matches white spaces.")

(defvar seq-cruft-regexp
  (regexp-opt (mapcar #'char-to-string
                      (concat seq-gap seq-space)))
  "A regexp that matches cruft.")


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


(defun reverse-region-by-char (beg end)
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((str-len (- end beg))
        (old-pos (point)))
    ;; (message "%d and %d" end old-pos)
    (goto-char end)
    (dotimes (x str-len)
      (let (current-char)
        (save-excursion
          (goto-char (- end x 1))
          (setq current-char (following-char)))
        (insert current-char)))
    (delete-region beg end)
    (goto-char old-pos)
    (if (/= old-pos end) (push-mark end nil t))))


(defun region-summary (beg end &optional legal-char-regexp)
  "Count and print the number of all the characters in the region.
Ignore char that does not belong to LEGAL-CHAR-REGEXP. Use hash
table to create dictionary-like data type."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((my-hash (make-hash-table :test 'equal))
        char count)
    ;; match any char if legal char is not provided
    (if (not legal-char-regexp)
        (setq legal-char-regexp "."))
    (save-excursion
      (goto-char beg)
      (dotimes (x (- end beg))
        (if (looking-at-p legal-char-regexp)
            (progn (setq char (char-after))
                   (setq count (gethash char my-hash))
                   (if count
                       (puthash char (1+ count) my-hash)
                     (puthash char 1 my-hash))))
        (forward-char)))
    (maphash (lambda (x y) (princ (format "%c:%d " x y))) my-hash)))


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
                            (t (setq x end) ; end the dotimes loop
                               (message "Bad base '%c' found at position %d,%d"
                                        (following-char)
                                        (line-number-at-pos)
                                        (current-column)))))))
    (if legal-p count)))

(defun color-gradient-hsl (start stop step-number &optional s l)
  "Return a list with (STEP-NUMBER + 1) colors from START to STOP in hex code.

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

(defun paint-seq-region (beg end face-prefix &optional case)
  "Color the sequences in the region BEG to END.

If CASE is nil, upcase and lowercase chars will be colored the same;
otherwise, not. FACE-PREFIX decides what kind of face group to use."
  (save-excursion
    (let (char face)
      (goto-char beg)
      (while (< beg end)
        (setq char (char-after beg))
        (if case
            (setq face (format "%s-%c" face-prefix char))
          ;; let upcase base use the color of lowercase base color
          (setq face (format "%s-%c" face-prefix (downcase char))))
        (if (facep face)
            (silent-put-text-property beg (+ beg 1) 'face (intern face)))
        (setq beg (+ beg 1))))))

(defun unpaint-seq-region (beg end)
  "Uncolor the sequences from BEG to END or the current line."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (remove-text-properties beg end '(face nil)))


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
  "Set the isearch prompt string to show seq search is active.
This serves as a warning that the string is being mangled."
  (setq ad-return-value (concat "MOTIF " ad-return-value)))

(defvar seq-isearch-p nil)

(defun toggle-seq-isearch ()
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
  "Set to `isearch-search-fun-function' when `nuc-mode' is
  enabled."
  (if seq-isearch-p
      (if isearch-forward 'seq-isearch-forward 'seq-isearch-backward)
    (isearch-search-fun-default)))

(setq isearch-search-fun-function 'seq-isearch-search-fun)

(provide 'seq)
