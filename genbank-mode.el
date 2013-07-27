


(defvar genbank-font-lock-keywords
  '(("^\\(LOCUS\\) +\\([-_.a-zA-Z_0-9]+\\)" ;; are '-_.' allowed?
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))

    ("^\\(VERSION\\) +\\([-_.a-zA-Z_0-9]+\\) +\\([Gg][Ii]\\):\\([0-9]+\\)" ;; are '-_.' allowed?
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     (3 font-lock-keyword-face)
     (4 font-lock-function-name-face))

    ;; more genbank keywords...
    "ORIGIN" "ACCESSION" "AUTHORS" "BASE COUNT" "DEFINITION"
    "FEATURES" "JOURNAL" "KEYWORDS" "MEDLINE" "NID"
    "ORGANISM" "REFERENCE" "SEGMENT" "SOURCE" "TITLE"
    "DBLINK" "PUBMED" "REMARK" "COMMENT" "CONSRTM"

    ;; line numbers...
    ("^[ \t]*\\([0-9]+\\)"
     (1 font-lock-string-face)))
  "Expressions to hilight in `genbank-mode'.")
