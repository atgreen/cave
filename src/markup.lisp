;;; markup.lisp — rendering and classifying repo content
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Markdown rendering (with the camo image proxy for external images),
;;; CODEOWNERS parsing, and the Linguist language tables. None of this
;;; shells out to git — that's git.lisp's job.

(in-package #:cave)

;; Extension/filename -> (display-name . color). Colors follow GitHub Linguist.
(defparameter *language-by-ext*
  '(("lisp" "Common Lisp" . "#3fb68b") ("lsp" "Common Lisp" . "#3fb68b")
    ("cl" "Common Lisp" . "#3fb68b") ("asd" "Common Lisp" . "#3fb68b")
    ("py" "Python" . "#3572A5") ("go" "Go" . "#00ADD8")
    ("js" "JavaScript" . "#f1e05a") ("mjs" "JavaScript" . "#f1e05a")
    ("cjs" "JavaScript" . "#f1e05a") ("jsx" "JavaScript" . "#f1e05a")
    ("ts" "TypeScript" . "#3178c6") ("tsx" "TypeScript" . "#3178c6")
    ("rs" "Rust" . "#dea584") ("c" "C" . "#555555") ("h" "C" . "#555555")
    ("cc" "C++" . "#f34b7d") ("cpp" "C++" . "#f34b7d") ("cxx" "C++" . "#f34b7d")
    ("hpp" "C++" . "#f34b7d") ("java" "Java" . "#b07219")
    ("rb" "Ruby" . "#701516") ("sh" "Shell" . "#89e051") ("bash" "Shell" . "#89e051")
    ("html" "HTML" . "#e34c26") ("htm" "HTML" . "#e34c26")
    ("css" "CSS" . "#563d7c") ("scss" "SCSS" . "#c6538c")
    ("md" "Markdown" . "#083fa1") ("markdown" "Markdown" . "#083fa1")
    ("yml" "YAML" . "#cb171e") ("yaml" "YAML" . "#cb171e")
    ("json" "JSON" . "#292929") ("nix" "Nix" . "#7e7eff")
    ("lua" "Lua" . "#000080") ("php" "PHP" . "#4F5D95")
    ("swift" "Swift" . "#F05138") ("kt" "Kotlin" . "#A97BFF")
    ("scala" "Scala" . "#c22d40") ("hs" "Haskell" . "#5e5086")
    ("ex" "Elixir" . "#6e4a7e") ("exs" "Elixir" . "#6e4a7e")
    ("clj" "Clojure" . "#db5855") ("vue" "Vue" . "#41b883")
    ("svelte" "Svelte" . "#ff3e00") ("sql" "SQL" . "#e38c00")
    ("pl" "Perl" . "#0298c3") ("pm" "Perl" . "#0298c3")
    ("vim" "Vim Script" . "#199f4b") ("dockerfile" "Dockerfile" . "#384d54")))

(defparameter *language-by-name*
  '(("makefile" "Makefile" . "#427819") ("gnumakefile" "Makefile" . "#427819")
    ("dockerfile" "Dockerfile" . "#384d54") ("containerfile" "Dockerfile" . "#384d54")))

(defun %file-basename-ext (path)
  "Return (VALUES basename ext-lowercase) for PATH; EXT is NIL for a dotfile or
   a name with no extension."
  (let* ((slash (position #\/ path :from-end t))
         (base (if slash (subseq path (1+ slash)) path))
         (dot (position #\. base :from-end t)))
    (values base (when (and dot (plusp dot) (< (1+ dot) (length base)))
                   (string-downcase (subseq base (1+ dot)))))))

(defun file-language-info (path)
  "Return (VALUES name color) for PATH's language, or NIL when unrecognized."
  (multiple-value-bind (base ext) (%file-basename-ext path)
    (let ((by-name (cdr (assoc (string-downcase base) *language-by-name* :test #'equal))))
      (if by-name
          (values (car by-name) (cdr by-name))
          (let ((by-ext (and ext (cdr (assoc ext *language-by-ext* :test #'equal)))))
            (when by-ext (values (car by-ext) (cdr by-ext))))))))

(defun language-color (name)
  "Hex color for a language display NAME (from the Linguist-style tables), or NIL."
  (flet ((scan (table)
           (loop for e in table
                 when (equal (cadr e) name) return (cddr e))))
    (or (scan *language-by-ext*) (scan *language-by-name*))))


(defun autolink-issue-refs (html ref-base)
  "Link #123 occurrences in HTML text to REF-BASE/issues/123, GitHub-style.
Skips markup (inside tags), text already inside <a>/<code>/<pre>, numeric
character entities (&#39;), and #123abc word-glued forms."
  (let ((out (make-string-output-stream))
        (i 0) (n (length html)) (skip 0))
    (flet ((tag-name (tag)
             (let* ((s (string-downcase tag))
                    (end (or (position-if
                              (lambda (c) (member c '(#\Space #\Tab #\Newline #\>)))
                              s :start 1)
                             (length s))))
               (subseq s 1 end))))
      (loop while (< i n)
            do (let ((ch (char html i)))
                 (cond
                   ((char= ch #\<)
                    (let* ((end (or (position #\> html :start i) (1- n)))
                           (tag (subseq html i (1+ end)))
                           (name (tag-name tag)))
                      (write-string tag out)
                      (cond ((member name '("a" "code" "pre") :test #'equal)
                             (incf skip))
                            ((member name '("/a" "/code" "/pre") :test #'equal)
                             (setf skip (max 0 (1- skip)))))
                      (setf i (1+ end))))
                   ((and (char= ch #\#) (zerop skip)
                         (< (1+ i) n) (digit-char-p (char html (1+ i)))
                         (or (zerop i)
                             (let ((prev (char html (1- i))))
                               (not (or (alphanumericp prev) (char= prev #\&))))))
                    (let ((end (or (position-if-not #'digit-char-p html :start (1+ i)) n)))
                      (if (and (< end n) (alphanumericp (char html end)))
                          (progn (write-char ch out) (incf i))
                          (let ((num (subseq html (1+ i) end)))
                            (format out "<a href=\"~A/issues/~A\">#~A</a>"
                                    ref-base num num)
                            (setf i end)))))
                   (t (write-char ch out) (incf i))))))
    (get-output-stream-string out)))

(defun render-markdown (markdown-string &key raw-base-url issue-ref-base)
  "Render Markdown to sanitized HTML string.
   Uses cl-commonmark (CommonMark 0.31.2 + GFM tables), so the input is
   parsed exactly as GitHub renders it — no preprocessing workarounds needed.
   RAW-BASE-URL when provided rewrites relative image src to absolute URLs
   before sanitization (the sanitizer strips relative src as protocol-less)."
  (let* ((raw-html (cl-commonmark:markdown-to-html markdown-string
                                                   :extensions '(:tables)))
         ;; Rewrite relative URLs BEFORE sanitization so they have a protocol
         (rewritten (if raw-base-url
                        (rewrite-relative-img-src raw-html raw-base-url)
                        raw-html)))
    ;; Proxy external images through camo so rendering never leaks the viewer's
    ;; IP to a third-party host (and mixed-content over HTTPS is fixed).
    (let ((final (camoify-img-src (sanitize-html:sanitize rewritten))))
      ;; Issue-ref autolinking runs last, on sanitized HTML, so the links it
      ;; injects (digits + our own path only) can't be stripped or abused.
      (if issue-ref-base
          (autolink-issue-refs final issue-ref-base)
          final))))

(defun rewrite-relative-img-src (html base-url)
  "Rewrite relative src= in <img> tags to use BASE-URL prefix."
  (let ((result html)
        (pos 0))
    (loop
      (let ((img-pos (search "<img " result :start2 pos)))
        (unless img-pos (return result))
        (let ((src-pos (search "src=\"" result :start2 img-pos)))
          (unless src-pos (return result))
          (let* ((url-start (+ src-pos 5))
                 (url-end (position #\" result :start url-start))
                 (url (subseq result url-start url-end)))
            ;; Skip absolute URLs (http://, https://, //, data:)
            (if (or (uiop:string-prefix-p "http://" url)
                    (uiop:string-prefix-p "https://" url)
                    (uiop:string-prefix-p "//" url)
                    (uiop:string-prefix-p "data:" url))
                (setf pos (1+ url-end))
                ;; Rewrite relative URL. Strip a leading "./" or "/" so the
                ;; raw handler sees a clean path; without this, ![](./foo.png)
                ;; produces ?path=./foo.png and 404s.
                (let* ((clean (cond ((uiop:string-prefix-p "./" url) (subseq url 2))
                                    ((uiop:string-prefix-p "/" url) (subseq url 1))
                                    (t url)))
                       (new-url (format nil "~A~A" base-url clean)))
                  (setf result (concatenate 'string
                                            (subseq result 0 url-start)
                                            new-url
                                            (subseq result url-end)))
                  (setf pos (+ url-start (length new-url) 1))))))))))

(defun camo-sig (image-url)
  "HMAC-SHA256 hex of IMAGE-URL keyed by the instance secret."
  (let ((mac (ironclad:make-mac :hmac
              (sb-ext:string-to-octets (or (config-value :secret-key) "")
                                       :external-format :utf-8)
              :sha256)))
    (ironclad:update-mac mac (sb-ext:string-to-octets image-url :external-format :utf-8))
    (ironclad:byte-array-to-hex-string (ironclad:produce-mac mac))))

(defun camo-url (image-url)
  "Signed local proxy URL for an external IMAGE-URL — hides the viewer's IP from
the remote host and fixes mixed-content (the proxy serves over the site's TLS)."
  (format nil "/-/camo/~A/~A"
          (camo-sig image-url)
          (ironclad:byte-array-to-hex-string
           (sb-ext:string-to-octets image-url :external-format :utf-8))))

(defun camoify-img-src (html)
  "Rewrite external <img src=\"http(s)://…\"> in HTML through the camo proxy.
Own-origin images (cave's own :base-url) are left untouched: camo exists to hide
the viewer's IP from THIRD-PARTY hosts and to fix mixed content, neither of which
applies to images cave itself serves. Proxying them would also make cave fetch
its own public URL from inside its container — which can't hairpin back, so the
camo fetch 502s and the image breaks."
  (let ((result html) (pos 0)
        (own (let ((b (config-value :base-url "")))
               (and (stringp b) (plusp (length b)) b))))
    (loop
      (let ((img-pos (search "<img " result :start2 pos)))
        (unless img-pos (return result))
        (let ((src-pos (search "src=\"" result :start2 img-pos)))
          (unless src-pos (return result))
          (let* ((url-start (+ src-pos 5))
                 (url-end (position #\" result :start url-start)))
            (unless url-end (return result))
            (let ((url (subseq result url-start url-end)))
              (if (and (or (uiop:string-prefix-p "http://" url)
                           (uiop:string-prefix-p "https://" url))
                       (not (and own (uiop:string-prefix-p own url))))
                  (let ((new-url (camo-url url)))
                    (setf result (concatenate 'string
                                              (subseq result 0 url-start)
                                              new-url
                                              (subseq result url-end)))
                    (setf pos (+ url-start (length new-url) 1)))
                  (setf pos (1+ url-end))))))))))

(defun parse-codeowners (text)
  "Parse CODEOWNERS TEXT into a list of (pattern . owner-tokens), in file order."
  (loop for line in (uiop:split-string text :separator '(#\Newline))
        for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
        unless (or (uiop:emptyp trimmed) (uiop:string-prefix-p "#" trimmed))
          collect (let ((parts (remove "" (uiop:split-string trimmed
                                                             :separator '(#\Space #\Tab))
                                       :test #'equal)))
                    (cons (first parts) (rest parts)))))

(defun codeowners-match-p (pattern path)
  "A practical subset of gitignore-style matching for a CODEOWNERS PATTERN."
  (let ((p pattern))
    (cond
      ((string= p "*") t)
      ((uiop:string-prefix-p "/" p) (codeowners-match-p (subseq p 1) path))
      ((uiop:string-suffix-p "/" p) (uiop:string-prefix-p p path))
      ((uiop:string-prefix-p "*." p) (uiop:string-suffix-p (subseq p 1) path))
      ((find #\/ p) (or (string= p path)
                        (uiop:string-prefix-p (concatenate 'string p "/") path)))
      (t (or (string= p (file-namestring path))
             (uiop:string-suffix-p (concatenate 'string "/" p) path))))))

(defun codeowners-for-path (rules path)
  "Owner tokens for PATH per RULES (last matching rule wins, GitHub semantics)."
  (let ((owners nil))
    (dolist (rule rules owners)
      (when (codeowners-match-p (car rule) path)
        (setf owners (cdr rule))))))

(defun monaco-language-id (filename)
  "Map a filename to a Monaco editor language identifier."
  (let ((ext (pathname-type (pathname filename)))
        (base (pathname-name (pathname filename))))
    (cond
      ((member ext '("lisp" "cl" "asd" "lsp") :test #'equalp) "lisp")
      ((member ext '("js" "mjs") :test #'equalp) "javascript")
      ((member ext '("ts" "tsx") :test #'equalp) "typescript")
      ((equalp ext "py") "python")
      ((equalp ext "rb") "ruby")
      ((member ext '("c" "h") :test #'equalp) "c")
      ((member ext '("cpp" "cc" "cxx" "hpp") :test #'equalp) "cpp")
      ((equalp ext "go") "go")
      ((equalp ext "rs") "rust")
      ((equalp ext "java") "java")
      ((equalp ext "sql") "sql")
      ((equalp ext "css") "css")
      ((equalp ext "html") "html")
      ((member ext '("md" "markdown") :test #'equalp) "markdown")
      ((equalp ext "json") "json")
      ((member ext '("yml" "yaml") :test #'equalp) "yaml")
      ((member ext '("sh" "bash" "zsh") :test #'equalp) "shell")
      ((equalp ext "xml") "xml")
      ((string-equal base "Makefile") "makefile")
      ((string-equal base "Dockerfile") "dockerfile")
      (t "plaintext"))))
