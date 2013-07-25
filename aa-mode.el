;;; aa-mode.el --- a minor mode for editing protein sequences
;;
;;; Commentary:
;; * A collection of functions for editing protein sequences.

;;; Installation:
;; --------------------
;; Here are two suggested ways for installing this package.
;; You can choose to autoload it when needed, or load it
;; each time emacs is started.  Put one of the following
;; sections in your .emacs:

;;;;;; USER CUSTOMIZABLE VARIABLES START HERE

(require 'seq)

(defvar aa-other ""
  "*Other chars that can possibly exist in a sequence. It should be in lower
case as the upper case will be added automatically. Please modify
`nuc-degeneracy-list' and `dna-complement-list' accordingly")

;;;;; END OF USER CUSTOMIZABLE VARIABLES

(defvar aa-iupac "acdefghiklmnpqrstvwybjxz"
  "All char for a single base, following IUPAC code. It should be in lower case
as the upper case will be added automatically.")
(defvar aa-iupac-3
  '((?a . Ala)
    (?b . Asx)
    (?c . Cys)
    (?d . Asp)
    (?e . Glu)
    (?f . Phe)
    (?g . Gly)
    (?h . His)
    (?i . Ile)
    (?j . Xle)
    (?k . Lys)
    (?l . Leu)
    (?m . Met)
    (?n . Asn)
    (?p . Pro)
    (?q . Gln)
    (?r . Arg)
    (?s . Ser)
    (?t . Thr)
    (?v . Val)
    (?w . Trp)
    (?x . Xaa)
    (?y . Tyr)
    (?z . Glx))
  "*Three letter IUPAC code of AA.")

(defvar aa-acidic "de")
(defvar aa-basic "rhk")
(defvar aa-hydrophobic "ahilmfvpgwy")
(defvar aa-hydrophilic "")
(defvar aa-amphipathic "")

(defvar aa-mw
  (let ((mw-vec (make-vector 256 nil)))
    (aset mw-vec ?a 71.09)
    (aset mw-vec ?c 103.15)
    (aset mw-vec ?d 115.09)
    (aset mw-vec ?e 129.12)
    (aset mw-vec ?f 147.18)
    (aset mw-vec ?g 57.05)
    (aset mw-vec ?h 137.14)
    (aset mw-vec ?i 113.16)
    (aset mw-vec ?k 128.17)
    (aset mw-vec ?l 113.16)
    (aset mw-vec ?m 131.19)
    (aset mw-vec ?n 114.11)
    (aset mw-vec ?p 97.12)
    (aset mw-vec ?q 128.14)
    (aset mw-vec ?r 156.19)
    (aset mw-vec ?s 87.08)
    (aset mw-vec ?t 101.11)
    (aset mw-vec ?v 99.14)
    (aset mw-vec ?w 186.21)
    (aset mw-vec ?y 163.18)
  mw-vec)
  "*AA molecular weights in Dalton in vector.")

(defvar aa-regexp
  (let ((aa (concat aa-iupac aa-other)))
    (regexp-opt (mapcar #'char-to-string
                        (concat aa (upcase aa)))))
  "A regexp that matches a valid nucleotide base (following IPUAC code plus
the symbol defined in `aa-other'.")

(defun aa-mw-region (beg end)
  "Return molecular weight of the region BEG and END or the current line."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((sum-mw 0))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (setq current (aref aa-mw (downcase (char-after))))
        (cond (current
               (setq sum-mw (+ sum-mw current)))
              ((not (looking-at-p seq-cruft-regexp))
               (error "Ambiguous or illegal char at position %d, %d"
                      (line-number-at-pos) (current-column))))
        (forward-char)))
    (message "The molecular weight is %.2f" sum-mw)
    sum-mw))

;; define aa faces belonging to aa-face group
(let ((letcol-alist aa-colors))
  (loop for elem in letcol-alist
        for l = (format "%s" (nth 0 elem))
        for b = (format "%s" (nth 1 elem))
        for f = (format "%s" (nth 2 elem))
        do
        (eval (macroexpand `(def-char-face "aa" ,l ,b ,f "aa-face")))))


;;;###autoload
(defun aa-move-forward (count)
  "Move forward COUNT bases. Move backward if negative.
Skip `seq-cruft-regexp' but stop on the illegal base
and report how many bases the point have been moved by.
COUNT can be either positive or negative, indicating the
moving direction. Return the number of bases that are moved thru.
See `proceed-char-repeatedly'"
  (interactive "p")
  (proceed-char-repeatedly count 'forward-char aa-regexp))

(defun aa-move-backward (count)
  "Move backward COUNT bases, similar to `aa-move-forward'. See also
 `proceed-char-repeatedly'."
  (interactive "p")
  ;; (proceed-char-repeatedly count 'backward-char))
  (proceed-char-repeatedly (- count) 'forward-char aa-regexp))

;;; delete
(defun aa-delete-forward (count)
  "Delete COUNT number of bases starting from the point, similar to
`aa-move-forward' (just use delete instead of move)."
  (interactive "p")
  (proceed-char-repeatedly count 'delete-char aa-regexp))

(defun aa-delete-backward (count)
  "Delete backward COUNT number of bases from the point, similar to
`aa-move-forward' (just use delete backward instead of move forward).
See `aa-delete-forward' and `proceed-char-repeatedly'."
  (interactive "p")
  (proceed-char-repeatedly (- count) 'delete-char aa-regexp))


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

(defun aa-p (beg end)
  "Test if the region between BEG and END (or the line) is a legal
protein sequence. Return the count if the region
 contains only legal nucleic acid characters, including
 `aa-regexp', `seq-cruft-regexp'; otherwise return nil and
 report the location of the invalid characters in the echo region."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (seq-p beg end aa-regexp))

(defalias 'aa-count 'aa-p)


(defun aa-summary (beg end)
  "Summarize the frequencies of AA in the region BEG and END or the current line.

See also `region-summary'."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (region-summary beg end aa-regexp))


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


;;;###autoload
(defun paint-aa-region (beg end &optiona case)
  "Color the bases in the region BEG to END or the current line."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (let ((case-fold-search t)
          c)
      (goto-char beg)
      (while (< beg end)
        (setq c (char-after beg))
        (cond
         ((char-equal c ?a)
          (silent-put-text-property beg (+ beg 1) 'face 'base-face-a))
         ((char-equal c ?c)
          (silent-put-text-property beg (+ beg 1) 'face 'base-face-c))
         ((char-equal c ?g)
          (silent-put-text-property beg (+ beg 1) 'face 'base-face-g))
         ((or (char-equal c ?t) (char-equal c ?u))
          (silent-put-text-property beg (+ beg 1) 'face 'base-face-t))
         (t nil))
        (setq beg (+ beg 1))))))


(define-minor-mode aa-mode
  "Nucleic acid mode"
  ;; the name, a string, to show in the modeline
  :lighter " aa"
  ;; keymap
  :keymap nil
  :global t)

(provide 'aa-mode)

;;; nuc-mode.el ends here
