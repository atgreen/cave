;;; yaml.lisp — Minimal YAML parser for Cave workflow files
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Handles the subset of YAML needed for .cave/workflows/*.yml:
;;; - Maps (key: value), nested maps (indentation-based)
;;; - Lists (- item), inline lists ([a, b, c])
;;; - Strings (quoted and unquoted), integers
;;; - Block scalars (key: |  and  key: >, with -/+ chomping) — needed for
;;;   multi-line `run:` steps and `env:`-style blocks, GitHub-Actions style
;;; - Comments (#), except inside block-scalar bodies (which are taken verbatim,
;;;   so shell `#` comments in a `run: |` survive)
;;;
;;; Produces nested alists. Lists become Lisp lists. Lines are carried internally
;;; as (INDENT CONTENT BLOCKVAL) triples; BLOCKVAL is the pre-resolved string for
;;; a block-scalar key line, else NIL.

(in-package #:cave)

(defun yaml-parse (input)
  "Parse a YAML string into nested alists/lists."
  (let ((lines (yaml-preprocess input)))
    (multiple-value-bind (result _remaining)
        (yaml-parse-block lines 0)
      (declare (ignore _remaining))
      result)))

(defun yaml-strip-comment (line)
  "Remove # comments (but not inside quotes)."
  (let ((in-quote nil)
        (quote-char nil))
    (loop for i from 0 below (length line)
          for ch = (char line i)
          do (cond
               ((and (not in-quote) (or (char= ch #\') (char= ch #\")))
                (setf in-quote t quote-char ch))
               ((and in-quote (char= ch quote-char))
                (setf in-quote nil))
               ((and (not in-quote) (char= ch #\#))
                (return-from yaml-strip-comment (subseq line 0 i)))))
    line))

(defun yaml-indent (line)
  "Count leading spaces in LINE."
  (loop for ch across line
        while (char= ch #\Space)
        count t))

(defun yaml-block-scalar-indicator (content)
  "If CONTENT is a `key: |`/`key: >` block-scalar header (with optional -/+
chomping), return (VALUES key indicator), else NIL."
  (let ((c (yaml-find-colon content)))
    (when c
      (let ((rest (string-trim '(#\Space) (subseq content (1+ c)))))
        (when (member rest '("|" "|-" "|+" ">" ">-" ">+") :test #'string=)
          (values (string-trim '(#\Space) (subseq content 0 c)) rest))))))

(defun yaml-apply-chomp (joined chomp)
  "Apply YAML block chomping: :strip removes trailing newlines, :keep keeps them,
:clip (default) leaves exactly one (or none if empty)."
  (ecase chomp
    (:keep joined)
    (:strip (string-right-trim '(#\Newline) joined))
    (:clip (let ((s (string-right-trim '(#\Newline) joined)))
             (if (zerop (length s)) "" (concatenate 'string s (string #\Newline)))))))

(defun yaml-collect-block-scalar (raw n start key-indent indicator)
  "Assemble a block scalar from RAW lines [START, n). KEY-INDENT is the header's
indentation; the body is the run of more-indented (and blank) lines. INDICATOR is
|/>/|-/etc. Returns (VALUES assembled-string next-index)."
  (let ((block-indent nil) (collected nil) (i start))
    (loop while (< i n)
          for rl = (aref raw i)
          for trimmed = (string-trim '(#\Space #\Tab #\Return) rl)
          do (cond
               ((zerop (length trimmed))            ; blank line — part of block
                (push "" collected) (incf i))
               ((> (yaml-indent rl) key-indent)     ; deeper than header — body
                (when (null block-indent) (setf block-indent (yaml-indent rl)))
                (push rl collected) (incf i))
               (t (return))))                        ; dedent — block ends
    (let* ((bi (or block-indent (1+ key-indent)))
           (body (mapcar (lambda (l)
                           (string-right-trim
                            '(#\Return)
                            (if (>= (length l) bi) (subseq l bi) (string-left-trim '(#\Space) l))))
                         (nreverse collected)))
           (literal (char= (char indicator 0) #\|))
           (chomp (cond ((find #\- indicator) :strip)
                        ((find #\+ indicator) :keep)
                        (t :clip)))
           (joined (if literal
                       (format nil "~{~A~^~%~}" body)
                       ;; folded (>) — join consecutive non-empty lines with a
                       ;; space; a blank line becomes a newline.
                       (with-output-to-string (s)
                         (loop with prev-blank = t
                               for l in body
                               for blank = (zerop (length l))
                               do (cond (blank (write-char #\Newline s) (setf prev-blank t))
                                        (t (unless prev-blank (write-char #\Space s))
                                           (write-string l s) (setf prev-blank nil))))))))
      (values (yaml-apply-chomp joined chomp) i))))

(defun yaml-preprocess (input)
  "Split INPUT into (INDENT CONTENT BLOCKVAL) line triples: strip comments and
drop blank lines, EXCEPT a `key: |`/`key: >` header swallows its verbatim block
body into BLOCKVAL (comments preserved). Returns the list of triples."
  (let* ((raw (coerce (uiop:split-string input :separator '(#\Newline)) 'vector))
         (n (length raw))
         (result nil)
         (i 0))
    (loop while (< i n)
          do (let* ((rawline (aref raw i))
                    (stripped (yaml-strip-comment rawline))
                    (indent (yaml-indent stripped))
                    (content (string-trim '(#\Space #\Tab) stripped)))
               (cond
                 ((uiop:emptyp content) (incf i))
                 (t (multiple-value-bind (key indicator)
                        (yaml-block-scalar-indicator content)
                      (if key
                          (multiple-value-bind (body next-i)
                              (yaml-collect-block-scalar raw n (1+ i) indent indicator)
                            (push (list indent (concatenate 'string key ": |") body) result)
                            (setf i next-i))
                          (progn (push (list indent content nil) result)
                                 (incf i))))))))
    (nreverse result)))

(defun yaml-parse-block (lines min-indent)
  "Parse a block of YAML lines at >= MIN-INDENT.
   Returns (VALUES parsed-value remaining-lines)."
  (when (null lines)
    (return-from yaml-parse-block (values nil nil)))
  (let ((first-content (second (first lines))))
    (cond
      ;; List item
      ((and (>= (car (first lines)) min-indent)
            (uiop:string-prefix-p "- " first-content))
       (yaml-parse-list lines min-indent))
      ;; Map entry (contains ":")
      ((and (>= (car (first lines)) min-indent)
            (yaml-map-line-p first-content))
       (yaml-parse-map lines min-indent))
      ;; Scalar
      (t (values (yaml-parse-scalar first-content) (rest lines))))))

(defun yaml-map-line-p (content)
  "Check if CONTENT looks like a map entry (key: value)."
  (let ((colon-pos (yaml-find-colon content)))
    (and colon-pos (> colon-pos 0))))

(defun yaml-find-colon (content)
  "Find the position of the first unquoted colon followed by space or end."
  (let ((in-quote nil)
        (quote-char nil))
    (loop for i from 0 below (length content)
          for ch = (char content i)
          do (cond
               ((and (not in-quote) (or (char= ch #\') (char= ch #\")))
                (setf in-quote t quote-char ch))
               ((and in-quote (char= ch quote-char))
                (setf in-quote nil))
               ((and (not in-quote) (char= ch #\:)
                     (or (= i (1- (length content)))
                         (char= (char content (1+ i)) #\Space)))
                (return-from yaml-find-colon i))))))

(defun yaml-parse-map (lines min-indent)
  "Parse a YAML map. Returns (VALUES alist remaining-lines)."
  (let ((result nil)
        (remaining lines))
    (loop
      (when (or (null remaining)
                (< (car (first remaining)) min-indent))
        (return))
      (let* ((line (first remaining))
             (indent (car line))
             (content (second line))
             (blockval (third line)))
        (when (< indent min-indent)
          (return))
        (unless (yaml-map-line-p content)
          (return))
        (let* ((colon-pos (yaml-find-colon content))
               (key (string-trim '(#\Space) (subseq content 0 colon-pos)))
               (rest-val (string-trim '(#\Space)
                                       (subseq content (min (+ colon-pos 2)
                                                            (length content))))))
          (setf remaining (rest remaining))
          (cond
            ;; Pre-resolved block scalar (key: | / key: >)
            (blockval
             (push (cons key blockval) result))
            ;; Inline value present
            ((and (not (uiop:emptyp rest-val))
                  (not (string= rest-val "")))
             (push (cons key (yaml-parse-scalar rest-val)) result))
            ;; Block value (next lines at deeper indent)
            (t
             (multiple-value-bind (val rest-lines)
                 (yaml-parse-block remaining (1+ indent))
               (push (cons key val) result)
               (setf remaining rest-lines)))))))
    (values (nreverse result) remaining)))

(defun yaml-parse-list (lines min-indent)
  "Parse a YAML list. Returns (VALUES list remaining-lines)."
  (let ((result nil)
        (remaining lines))
    (loop
      (when (or (null remaining)
                (< (car (first remaining)) min-indent))
        (return))
      (let* ((line (first remaining))
             (content (second line)))
        (unless (uiop:string-prefix-p "- " content)
          (return))
        (let ((item-content (string-trim '(#\Space) (subseq content 2))))
          (setf remaining (rest remaining))
          (cond
            ;; List item is a map (- key: value)
            ((yaml-map-line-p item-content)
             ;; Re-parse as a map entry from this point
             (let* ((item-indent (+ (car line) 2))
                    ;; Build virtual lines: the item-content + any deeper lines.
                    ;; Preserve the dash line's BLOCKVAL (third line) for the
                    ;; `- run: |` on-the-dash case.
                    (virtual-lines (list (list item-indent item-content (third line))))
                    (deeper nil))
               ;; Collect lines deeper than the dash
               (loop while (and remaining (> (car (first remaining)) (car line)))
                     do (push (first remaining) deeper)
                        (setf remaining (rest remaining)))
               (setf virtual-lines (append virtual-lines (nreverse deeper)))
               (multiple-value-bind (val _rest)
                   (yaml-parse-map virtual-lines item-indent)
                 (declare (ignore _rest))
                 (push val result))))
            ;; Simple scalar item
            (t
             (push (yaml-parse-scalar item-content) result))))))
    (values (nreverse result) remaining)))

(defun yaml-parse-scalar (value)
  "Parse a scalar YAML value."
  (cond
    ((uiop:emptyp value) nil)
    ;; Inline list [a, b, c]
    ((and (char= (char value 0) #\[)
          (char= (char value (1- (length value))) #\]))
     (let ((inner (string-trim '(#\Space) (subseq value 1 (1- (length value))))))
       (if (uiop:emptyp inner)
           nil
           (mapcar (lambda (s) (yaml-parse-scalar (string-trim '(#\Space) s)))
                   (yaml-split-commas inner)))))
    ;; Quoted string
    ((or (and (char= (char value 0) #\")
              (char= (char value (1- (length value))) #\"))
         (and (char= (char value 0) #\')
              (char= (char value (1- (length value))) #\')))
     (subseq value 1 (1- (length value))))
    ;; Boolean
    ((member value '("true" "yes") :test #'string-equal) t)
    ((member value '("false" "no") :test #'string-equal) nil)
    ;; Integer
    ((every #'digit-char-p value)
     (parse-integer value))
    ;; Plain string
    (t value)))

(defun yaml-split-commas (str)
  "Split STR by commas, respecting quotes."
  (let ((result nil)
        (current (make-string-output-stream))
        (in-quote nil)
        (quote-char nil))
    (loop for ch across str
          do (cond
               ((and (not in-quote) (char= ch #\,))
                (push (get-output-stream-string current) result)
                (setf current (make-string-output-stream)))
               ((and (not in-quote) (or (char= ch #\") (char= ch #\')))
                (setf in-quote t quote-char ch)
                (write-char ch current))
               ((and in-quote (char= ch quote-char))
                (setf in-quote nil)
                (write-char ch current))
               (t (write-char ch current))))
    (push (get-output-stream-string current) result)
    (nreverse result)))
