;;; aa-mode.el --- a minor mode for editing protein sequences
;;; It should be not enabled with nuc-mode at the same time.

;;;;;; USER CUSTOMIZABLE VARIABLES START HERE

(require 'seq)

(defvar pro-aa--alist
  '((?a  "Ala"  71.09)
    (?b  "Asx"  nil)  ; Asn Asp
    (?c  "Cys"  103.15)
    (?d  "Asp"  115.09)
    (?e  "Glu"  129.12)
    (?f  "Phe"  147.18)
    (?g  "Gly"  57.05)
    (?h  "His"  137.14)
    (?i  "Ile"  113.16)
    (?j  "Xle"  nil)  ;Leu Ile
    (?k  "Lys"  128.17)
    (?l  "Leu"  113.16)
    (?m  "Met"  131.19)
    (?n  "Asn"  114.11)
    (?p  "Pro"  97.12 )
    (?q  "Gln"  128.14)
    (?r  "Arg"  156.19)
    (?s  "Ser"  87.08 )
    (?t  "Thr"  101.11)
    (?v  "Val"  99.14 )
    (?w  "Trp"  186.21)
    (?x  "Xaa"  nil)  ; unknown aa
    (?y  "Tyr"  163.18)
    (?z  "Glx"  nil)) ; Glu Gln
  "*A association list of one-letter, three-letter IUPAC code of AA and their molecular weight.

For each inner list, the first element is allowed AA; the second element
is the three-letter code of the first, and the last is the molecular weight.
for the first. Only for lowercase, as the upcased will be added automatically.")

;;;;; END OF USER CUSTOMIZABLE VARIABLES

(defvar pro-aa-alist
  (append (mapcar (lambda (x)
                    (setcar x (upcase (car x)))
                    x)
                  pro-aa--alist)
          pro-aa--alist)
  "Similar to `pro-aa--alist', just with upcase bases added.")


(defvar pro-aa
  (mapcar #'car pro-aa-alist)
  "All char for a single base, following IUPAC code. It should be in lower case
as the upper case will be added automatically.")

(defvar pro-aa-acidic "de")
(defvar pro-aa-basic "rhk")
(defvar pro-aa-hydrophobic "ahilmfvpgwy")
(defvar pro-aa-hydrophilic "")
(defvar pro-aa-amphipathic "")


(defvar pro-aa-mw
  (let ((mw-vec (make-vector 256 nil)))
    (dolist (element pro-aa-alist)
      (aset mw-vec (car element) (nth 2 element)))
    mw-vec)
  "A vector of AA molecular weights in Dalton.")

(defvar pro-aa-1-vec
  (let ((vec (make-vector 256 nil)))
    (dolist (element pro-aa-alist)
      (aset vec (car element) (nth 1 element)))
    vec)
  "A vector of 3-letter amino acid codes.")

(defvar pro-aa-3-hash
  (let ((my-hash (make-hash-table :test 'equal)))
    (dolist (element pro-aa--alist)
      (puthash (nth 1 element) (car element) my-hash))
    my-hash)
  "A hash table with 3-letter code as key and 1-letter code as value.")

(defvar pro-aa-regexp
  (regexp-opt (mapcar #'char-to-string pro-aa))
  "A regexp that matches a valid 1-letter amino acid code in `pro-aa'.")


(defun pro-weight (beg end)
  "Return molecular weight of the region BEG and END or the current line."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((sum-mw 0))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (setq current (aref pro-aa-mw (downcase (char-after))))
        (cond (current
               (setq sum-mw (+ sum-mw current)))
              ((not (looking-at seq-cruft-regexp))
               (error "Ambiguous or illegal char at position %d, %d"
                      (line-number-at-pos) (current-column))))
        (forward-char)))
    (if (called-interactively-p 'interactive)
        (message "The molecular weight is %.2f" sum-mw))
    sum-mw))


(defun pro-1-2-3 (beg end)
  "Convert 1-letter IUPAC code to 3-letter IUPAC code"
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (condition-case err
      (let ((times (- end beg)))
        (goto-char beg)
        (dotimes (x times)
          (if (looking-at pro-aa-regexp)
              (progn (insert (aref pro-aa-1-vec (char-after)))
                     (delete-char 1))
            (forward-char))))
    (error (primitive-undo 1 buffer-undo-list)
           (message "Failed: %s" err))))


(defun pro-3-2-1 (beg end)
  "Convert 3-letter IUPAC code to 1-letter IUPAC code.

Currently it only converts 3-letter codes without any characters
between them."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (condition-case err
      (let ((times (/ (- end beg) 3))
            code letter)
        (goto-char beg)
        (dotimes (x times)
          (setq code (buffer-substring (point) (+ 3 (point))))
          (setq letter (gethash code pro-aa-3-hash))
          (if letter
              (insert-char letter)
            (error "Unknown code '%s' at position %d" code (point)))
          (delete-char 3)))
    ;; return to the original state if error is met.
    (error (primitive-undo 1 buffer-undo-list)
           (message "Failed: %s" err))))


;;;###autoload
(defun pro-move-forward (count)
  "Move forward COUNT bases. Move backward if negative.
Skip `seq-cruft-regexp' but stop on the illegal base
and report how many bases the point have been moved by.
COUNT can be either positive or negative, indicating the
moving direction. Return the number of bases that are moved thru.
See `proceed-char-repeatedly'"
  (interactive "p")
  (proceed-char-repeatedly count #'forward-char pro-aa-regexp))

(defun pro-move-backward (count)
  "Move backward COUNT bases, similar to `pro-aa-move-forward'. See also
 `proceed-char-repeatedly'."
  (interactive "p")
  ;; (proceed-char-repeatedly count 'backward-char))
  (proceed-char-repeatedly (- count) #'forward-char pro-aa-regexp))

;;; delete
(defun pro-delete-forward (count)
  "Delete COUNT number of bases starting from the point, similar to
`pro-aa-move-forward' (just use delete instead of move)."
  (interactive "p")
  (proceed-char-repeatedly count #'delete-char pro-aa-regexp))

(defun pro-delete-backward (count)
  "Delete backward COUNT number of bases from the point, similar to
`pro-aa-move-forward' (just use delete backward instead of move forward).
See `pro-aa-delete-forward' and `proceed-char-repeatedly'."
  (interactive "p")
  (proceed-char-repeatedly (- count) #'delete-char pro-aa-regexp))


(defun pro-count (beg end)
  "Test if the region between BEG and END (or the line) is a legal
protein sequence. Return the count if the region
 contains only legal nucleic acid characters, including
 `pro-aa-regexp', `seq-cruft-regexp'; otherwise return nil and
 report the location of the invalid characters in the echo region."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (seq-p beg end pro-aa-regexp))

(defalias 'pro-p 'pro-count)


(defun pro-summary (beg end)
  "Summarize the frequencies of AA in the region BEG and END or the current line.

See also `region-summary'."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (region-summary beg end pro-aa-regexp))


;;;;;; isearch motif

(defun seq-isearch-mangle-str (str)
  "Mangle the string STR into a regexp to search over cruft in sequence.
Inserts a regexp between each base which matches sequence formatting cruft.
For example, if `seq-cruft-regexp' is '[ ]', the search string 'acgt' would be
transformed into 'a[ ]*c[ ]*g[ ]*t' and the search string containing IUPAC code
such as 'acrt' would be transformed into '[a][ ]*[c][ ]*[ag][ ]*[t]."
  ;; (mapconcat 'identity (split-string str "" t) (concat seq-cruft-regexp "*")))
    (mapconcat 'char-to-string str (concat seq-cruft-regexp "*")))


;; define aa faces belonging to pro-aa-face group
(defvar pro-aa-colors
  (mapcar* #'cons
           pro-aa
           (setcdr (last color-pairs) color-pairs))
  "Background and foreground colors for each IUPAC bases.

This is a list of lists. For each inner list, it contains 3 atoms:
a nuc base in char type, hex-code colors for foreground and background")

(let ((letcol-alist pro-aa-colors))
  (loop for elem in letcol-alist
        for f = (nth 1 elem)
        for b = (nth 2 elem)
        for l = (format "%c" (nth 0 elem)) do
        (eval (macroexpand `(def-char-face ,l ,b ,f "aa-face")))))


;;;###autoload
(defun pro-paint (beg end &optiona case)
  "Color the nucleic acid region BEG to END.

If CASE is nil, upcase and lowercase base chars will be colored the same;
otherwise, not. See `paint-seq-region' for details."
  (interactive
   (if (use-region-p) ; (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (paint-seq-region beg end "aa-face" case))

;;;###autoload
(defalias 'pro-unpaint 'unpaint-seq-region)


(defvar pro-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cf"     'pro-move-forward)
    (define-key map "\C-cb"     'pro-move-backward)
    (define-key map "\C-cw"     'pro-weight)
    (define-key map "\C-c1"     'pro-1-2-3)
    (define-key map "\C-c3"     'pro-3-2-1)
    (define-key map "\C-c\c-#"  'pro-summary)
    map)
  "Keymap for `pro-mode'.")


(define-minor-mode pro-mode
  "Protein mode

It should be not enabled with `nuc-mode' at the same time."
  :init-value nil
  ;; the name, a string, to show in the modeline
  :lighter " pro"
  :keymap pro-mode-map
  :global t)

(provide 'pro-mode)

;;; nuc-mode.el ends here
