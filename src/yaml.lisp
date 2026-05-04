;;; yaml.lisp — Minimal YAML parser for Cave workflow files
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Handles the subset of YAML needed for .cave/workflows/*.yml:
;;; - Maps (key: value), nested maps (indentation-based)
;;; - Lists (- item), inline lists ([a, b, c])
;;; - Strings (quoted and unquoted), integers
;;; - Comments (#)
;;;
;;; Produces nested alists. Lists become Lisp lists.

(in-package #:cave)

(defun yaml-parse (input)
  "Parse a YAML string into nested alists/lists."
  (let ((lines (yaml-preprocess input)))
    (multiple-value-bind (result _remaining)
        (yaml-parse-block lines 0)
      (declare (ignore _remaining))
      result)))

(defun yaml-preprocess (input)
  "Split input into lines, strip comments, drop blank lines.
   Returns list of (indent . content) cons cells."
  (let ((result nil))
    (dolist (raw-line (uiop:split-string input :separator '(#\Newline)))
      (let* ((line (yaml-strip-comment raw-line))
             (indent (yaml-indent line))
             (content (string-trim '(#\Space #\Tab) line)))
        (unless (uiop:emptyp content)
          (push (cons indent content) result))))
    (nreverse result)))

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

(defun yaml-parse-block (lines min-indent)
  "Parse a block of YAML lines at >= MIN-INDENT.
   Returns (VALUES parsed-value remaining-lines)."
  (when (null lines)
    (return-from yaml-parse-block (values nil nil)))
  (let ((first-content (cdr (first lines))))
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
             (content (cdr line)))
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
             (content (cdr line)))
        (unless (uiop:string-prefix-p "- " content)
          (return))
        (let ((item-content (string-trim '(#\Space) (subseq content 2))))
          (setf remaining (rest remaining))
          (cond
            ;; List item is a map (- key: value)
            ((yaml-map-line-p item-content)
             ;; Re-parse as a map entry from this point
             (let* ((item-indent (+ (car line) 2))
                    ;; Build virtual lines: the item-content + any deeper lines
                    (virtual-lines (cons (cons item-indent item-content) nil))
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
