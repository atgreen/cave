;;; expr.lisp — GitHub-Actions ${{ }} expression language
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; A faithful evaluator for GitHub Actions' small, pure expression DSL — NOT a
;;; scripting language: literals (string/number/bool/null), the operators
;;; ! && || == != < <= > >=, property/index access (a.b, a[b], a.*), and a fixed
;;; function table (contains, startsWith, endsWith, format, join, toJSON,
;;; fromJSON, hashFiles, success/failure/always/cancelled). No arithmetic, no
;;; assignment, no user functions.
;;;
;;; The grammar is parsed with iparse (an instaparse port). Values use Lisp
;;; types with two sentinels so GHA's null-vs-false distinction survives:
;;;   true  -> T          false -> :false          null -> NIL
;;;   number -> double     string -> string
;;;   object -> hash-table (string keys)   array -> vector
;;;
;;; CONTEXT is a hash-table mapping the root names (github, env, secrets, steps,
;;; matrix, needs, runner, job, vars, inputs) to such values.

(in-package #:cave)

(iparse:defparser %gha-expr-parse "
expr = <ws> orx <ws>
orx = andx (<ws> <'||'> <ws> andx)*
andx = notx (<ws> <'&&'> <ws> notx)*
notx = bang / cmp
bang = <'!'> <ws> notx
cmp = postfix (<ws> cmpop <ws> postfix)*
cmpop = '==' / '!=' / '<=' / '>=' / '<' / '>'
postfix = atom trailer*
<trailer> = star / member / index
star = <'.'> '*'
member = <'.'> name
index = <'['> <ws> expr <ws> <']'>
<atom> = funcall / group / boollit / nulllit / numlit / strlit / name
group = <'('> <ws> expr <ws> <')'>
funcall = name <ws> <'('> <ws> arglist? <ws> <')'>
arglist = expr (<ws> <','> <ws> expr)*
name = #'[A-Za-z_][A-Za-z0-9_-]*'
numlit = #'-?[0-9]+(\\.[0-9]+)?'
strlit = #\"'([^']|'')*'\"
boollit = 'true' / 'false'
nulllit = 'null'
<ws> = #'[ \\t\\r\\n]*'
")

;;; ---------- value helpers ----------

(defun %gha-truthy (v)
  "GHA truthiness: null, false, empty string, and 0 are falsy."
  (cond ((null v) nil)
        ((eq v :false) nil)
        ((stringp v) (plusp (length v)))
        ((numberp v) (/= v 0))
        (t t)))

(defun %gha-bool (x) (if x t :false))

(defun %gha-array-p (x)
  "True for a GHA array (a vector that is NOT a string — strings are vectors too)."
  (typep x '(and vector (not string))))

(defun %gha-number (v)
  "Coerce V to a number per GHA rules (NaN -> :nan)."
  (cond ((numberp v) v)
        ((null v) 0)                       ; null -> 0
        ((eq v :false) 0)
        ((eq v t) 1)
        ((stringp v)
         (let ((s (string-trim '(#\Space) v)))
           (cond ((zerop (length s)) 0)     ; '' -> 0
                 ((member s '("true") :test #'string-equal) 1)
                 ((member s '("false") :test #'string-equal) 0)
                 (t (or (ignore-errors
                         (let ((*read-eval* nil))
                           (let ((n (read-from-string s nil nil)))
                             (and (numberp n) n))))
                        :nan)))))
        (t :nan)))

(defun %gha-= (a b)
  "GHA loose equality."
  (cond
    ;; same object identity for hash/vector
    ((and (or (hash-table-p a) (%gha-array-p a)) (eq a b)) t)
    ((or (hash-table-p a) (%gha-array-p a) (hash-table-p b) (%gha-array-p b))
     (eq a b))
    ((and (stringp a) (stringp b)) (string-equal a b))
    (t (let ((na (%gha-number a)) (nb (%gha-number b)))
         (and (numberp na) (numberp nb) (= na nb))))))

(defun %gha-compare (op a b)
  "Apply comparison OP (string) to A and B, returning T/:false."
  (%gha-bool
   (cond
     ((string= op "==") (%gha-= a b))
     ((string= op "!=") (not (%gha-= a b)))
     (t (let ((na (%gha-number a)) (nb (%gha-number b)))
          (if (and (numberp na) (numberp nb))
              (cond ((string= op "<") (< na nb))
                    ((string= op "<=") (<= na nb))
                    ((string= op ">") (> na nb))
                    ((string= op ">=") (>= na nb)))
              nil))))))

(defun %gha-member (obj key)
  "Property access obj.KEY (KEY a string)."
  (cond ((hash-table-p obj) (gethash key obj))
        (t nil)))

(defun %gha-index (obj idx)
  "Index access obj[IDX]."
  (cond ((hash-table-p obj) (gethash (if (stringp idx) idx (%gha-to-string idx)) obj))
        ((%gha-array-p obj)
         (let ((n (%gha-number idx)))
           (when (and (integerp n) (>= n 0) (< n (length obj))) (aref obj n))))
        (t nil)))

(defun %gha-star (obj)
  "Object/array filter `.*` — returns a vector of the values."
  (cond ((hash-table-p obj)
         (let ((vs nil)) (maphash (lambda (k v) (declare (ignore k)) (push v vs)) obj)
              (coerce (nreverse vs) 'vector)))
        ((%gha-array-p obj) obj)
        (t #())))

(defun %gha-to-string (v)
  "Stringify a value for interpolation into a larger string."
  (cond ((null v) "")                       ; null -> empty
        ((eq v :false) "false")
        ((eq v t) "true")
        ((stringp v) v)
        ((integerp v) (princ-to-string v))
        ((numberp v) (if (= v (truncate v))
                         (princ-to-string (truncate v))
                         (princ-to-string v)))
        ((or (hash-table-p v) (%gha-array-p v)) (%to-json v))
        (t (princ-to-string v))))

(defun %gha-unquote (raw)
  "Turn a 'single-quoted' literal token into its string value ('' -> ')."
  (let ((inner (subseq raw 1 (1- (length raw)))))
    (%string-replace-all inner "''" "'")))

;;; ---------- functions ----------

(defun %gha-call (name args ctx)
  "Dispatch a GHA built-in function (case-insensitive)."
  (let ((fn (string-downcase name)))
    (flet ((arg (i) (nth i args)))
      (cond
        ((string= fn "contains")
         (let ((hay (arg 0)) (needle (arg 1)))
           (%gha-bool
            (cond ((%gha-array-p hay) (some (lambda (e) (%gha-= e needle)) hay))
                  (t (search (string-downcase (%gha-to-string needle))
                             (string-downcase (%gha-to-string hay))))))))
        ((string= fn "startswith")
         (%gha-bool (let ((s (string-downcase (%gha-to-string (arg 0))))
                          (p (string-downcase (%gha-to-string (arg 1)))))
                      (and (<= (length p) (length s)) (string= p (subseq s 0 (length p)))))))
        ((string= fn "endswith")
         (%gha-bool (let ((s (string-downcase (%gha-to-string (arg 0))))
                          (p (string-downcase (%gha-to-string (arg 1)))))
                      (and (<= (length p) (length s)) (string= p (subseq s (- (length s) (length p))))))))
        ((string= fn "format")
         (%gha-format (%gha-to-string (arg 0)) (cdr args)))
        ((string= fn "join")
         (let* ((arr (arg 0)) (sep (if (cdr args) (%gha-to-string (arg 1)) ",")))
           (if (%gha-array-p arr)
               (format nil (concatenate 'string "~{~A~^" sep "~}")
                       (map 'list #'%gha-to-string arr))
               (%gha-to-string arr))))
        ((string= fn "tojson") (%to-json (arg 0)))
        ((string= fn "fromjson") (handler-case (com.inuoe.jzon:parse (%gha-to-string (arg 0)))
                                   (error () nil)))
        ((string= fn "hashfiles") "")        ; TODO: hash workspace files
        ((string= fn "success") (%gha-status-p ctx :success))
        ((string= fn "failure") (%gha-status-p ctx :failure))
        ((string= fn "cancelled") (%gha-status-p ctx :cancelled))
        ((string= fn "always") t)
        (t nil)))))

(defun %json-escape (s)
  (with-output-to-string (out)
    (loop for ch across s do
      (case ch
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (t (write-char ch out))))))

(defun %to-json (v)
  "Serialize a GHA value (with our T/:false/NIL sentinels) to a JSON string."
  (cond ((eq v t) "true")
        ((eq v :false) "false")
        ((null v) "null")
        ((stringp v) (concatenate 'string "\"" (%json-escape v) "\""))
        ((integerp v) (princ-to-string v))
        ((numberp v) (%gha-to-string v))
        ((%gha-array-p v)
         (format nil "[~{~A~^,~}]" (map 'list #'%to-json v)))
        ((hash-table-p v)
         (let ((pairs nil))
           (maphash (lambda (k val)
                      (push (format nil "\"~A\":~A" (%json-escape (princ-to-string k)) (%to-json val)) pairs))
                    v)
           (format nil "{~{~A~^,~}}" (nreverse pairs))))
        (t "null")))

(defun %gha-format (fmt args)
  "format(): replace {0},{1},… ; {{ -> { ; }} -> }."
  (with-output-to-string (s)
    (loop with i = 0 with n = (length fmt)
          while (< i n)
          for ch = (char fmt i) do
            (cond
              ((and (char= ch #\{) (< (1+ i) n) (char= (char fmt (1+ i)) #\{))
               (write-char #\{ s) (incf i 2))
              ((and (char= ch #\}) (< (1+ i) n) (char= (char fmt (1+ i)) #\}))
               (write-char #\} s) (incf i 2))
              ((char= ch #\{)
               (let ((close (position #\} fmt :start i)))
                 (if close
                     (let ((idx (ignore-errors (parse-integer fmt :start (1+ i) :end close))))
                       (write-string (%gha-to-string (and idx (nth idx args))) s)
                       (setf i (1+ close)))
                     (progn (write-char ch s) (incf i)))))
              (t (write-char ch s) (incf i))))))

(defun %gha-status-p (ctx which)
  "Status functions read job.status (default 'success')."
  (let* ((job (gethash "job" ctx))
         (status (and (hash-table-p job) (gethash "status" job)))
         (st (if (stringp status) (string-downcase status) "success")))
    (%gha-bool (ecase which
                 (:success (string= st "success"))
                 (:failure (string= st "failure"))
                 (:cancelled (string= st "cancelled"))))))

;;; ---------- evaluator ----------

(defun %gha-ev (node ctx)
  "Evaluate a parse NODE against CTX."
  (cond
    ((stringp node) node)
    ((null node) nil)
    (t (case (car node)
         (:expr (%gha-ev (second node) ctx))
         (:orx (%gha-ev-or (cdr node) ctx))
         (:andx (%gha-ev-and (cdr node) ctx))
         (:notx (%gha-ev (second node) ctx))
         (:bang (%gha-bool (not (%gha-truthy (%gha-ev (second node) ctx)))))
         (:cmp (%gha-ev-cmp (cdr node) ctx))
         (:postfix (%gha-ev-postfix (cdr node) ctx))
         (:group (%gha-ev (second node) ctx))
         (:funcall (%gha-ev-funcall (cdr node) ctx))
         (:name (gethash (second node) ctx))
         (:numlit (let ((s (second node)))
                    (if (find #\. s) (float (read-from-string s) 1d0)
                        (parse-integer s))))
         (:strlit (%gha-unquote (second node)))
         (:boollit (if (string= (second node) "true") t :false))
         (:nulllit nil)
         (t nil)))))

(defun %gha-ev-or (operands ctx)
  (loop for (n . rest) on operands
        for v = (%gha-ev n ctx)
        when (or (%gha-truthy v) (null rest)) return v))

(defun %gha-ev-and (operands ctx)
  (loop for (n . rest) on operands
        for v = (%gha-ev n ctx)
        when (or (not (%gha-truthy v)) (null rest)) return v))

(defun %gha-ev-cmp (children ctx)
  (if (= (length children) 1)
      (%gha-ev (first children) ctx)
      (let ((acc (%gha-ev (first children) ctx)))
        (loop for (opnode operand) on (cdr children) by #'cddr
              do (setf acc (%gha-compare (second opnode) acc (%gha-ev operand ctx))))
        acc)))

(defun %gha-ev-postfix (children ctx)
  (let ((val (%gha-ev (first children) ctx)))
    (dolist (tr (cdr children) val)
      (setf val
            (case (car tr)
              (:member (%gha-member val (second (second tr))))
              (:index (%gha-index val (%gha-ev (second tr) ctx)))
              (:star (%gha-star val))
              (t val))))))

(defun %gha-ev-funcall (children ctx)
  (let* ((name (second (first children)))
         (arglist (find-if (lambda (c) (and (consp c) (eq (car c) :arglist))) children))
         (args (when arglist (mapcar (lambda (a) (%gha-ev a ctx)) (cdr arglist)))))
    (%gha-call name args ctx)))

;;; ---------- public API ----------

(defun %unwrap-tree (node)
  "iparse returns metaobject-wrapped nodes; flatten to plain (tag child…) lists."
  (cond ((iparse/util:metaobject-p node) (%unwrap-tree (iparse/util:metaobject-value node)))
        ((consp node) (mapcar #'%unwrap-tree node))
        (t node)))

(defun eval-gha-expression (expr-string ctx)
  "Parse and evaluate a single GHA expression (the text inside ${{ }}) against
CTX (a hash-table). Returns the typed value (T / :false / NIL / number / string
/ hash-table / vector). On parse/eval error returns NIL."
  (handler-case
      (let ((tree (%unwrap-tree (%gha-expr-parse expr-string))))
        (if (and tree (consp tree))
            (%gha-ev tree ctx)
            nil))
    (error () nil)))

(defun gha-expression-true-p (expr-string ctx)
  "Evaluate EXPR-STRING and return its truthiness — for `if:` conditions."
  (%gha-truthy (eval-gha-expression expr-string ctx)))

(defun interpolate-gha (string ctx)
  "Replace every ${{ expr }} in STRING with the stringified value of EXPR
evaluated against CTX. Non-expression text is left untouched."
  (if (or (null string) (not (search "${{" string)))
      string
      (with-output-to-string (out)
        (loop with i = 0 with n = (length string)
              while (< i n) do
                (let ((start (search "${{" string :start2 i)))
                  (if (null start)
                      (progn (write-string string out :start i) (setf i n))
                      (let ((end (search "}}" string :start2 (+ start 3))))
                        (write-string string out :start i :end start)
                        (if (null end)
                            (progn (write-string string out :start start) (setf i n))
                            (let ((expr (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                     (subseq string (+ start 3) end))))
                              (write-string (%gha-to-string (eval-gha-expression expr ctx)) out)
                              (setf i (+ end 2)))))))))))
