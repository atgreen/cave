;;; git.lisp — Git CLI integration
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

;;; Shell out to git for all operations. Simple and correct.

(defun git-run (repo-path &rest args)
  "Run a git command in REPO-PATH. Returns (VALUES output error-output exit-code)."
  (let ((cmd (append (list "git" "-C" (namestring repo-path)) args)))
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program cmd
                          :output '(:string :stripped t)
                          :error-output '(:string :stripped t)
                          :ignore-error-status t)
      (values output error-output exit-code))))

(defun git-branches (repo-path)
  "List branches in a bare repo. Returns list of branch name strings."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "branch" "--format=%(refname:short)")
    (declare (ignore _err))
    (when (zerop exit-code)
      (remove-if #'uiop:emptyp
                 (uiop:split-string output :separator '(#\Newline))))))

(defun git-default-branch (repo-path)
  "Get the default branch (HEAD target) of a bare repo."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "symbolic-ref" "--short" "HEAD")
    (declare (ignore _err))
    (when (zerop exit-code) output)))

(defun git-log (repo-path &key (branch nil) (limit 20))
  "Get recent commits. Returns list of plists (:hash :short-hash :author :date :subject)."
  (let* ((format-str "%H%n%h%n%an%n%ai%n%s%n---")
         (args (list "log" (format nil "--format=~A" format-str)
                     (format nil "-~A" limit)))
         (args (if branch (append args (list branch)) args)))
    (multiple-value-bind (output _err exit-code)
        (apply #'git-run repo-path args)
      (declare (ignore _err))
      (when (zerop exit-code)
        (parse-git-log output)))))

(defun parse-git-log (output)
  "Parse git log output in our custom format."
  (let ((entries nil)
        (lines (uiop:split-string output :separator '(#\Newline))))
    (loop while (>= (length lines) 5)
          do (let ((hash (pop lines))
                   (short-hash (pop lines))
                   (author (pop lines))
                   (date (pop lines))
                   (subject (pop lines)))
               ;; Skip separator
               (when (and lines (equal (first lines) "---"))
                 (pop lines))
               (push (list :hash hash
                           :short-hash short-hash
                           :author author
                           :date date
                           :subject subject)
                     entries)))
    (nreverse entries)))

(defun git-file-tree (repo-path &key (ref "HEAD"))
  "Get the file tree at REF. Returns list of (:mode :type :hash :name)."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "ls-tree" "-r" "--name-only" ref)
    (declare (ignore _err))
    (when (zerop exit-code)
      (remove-if #'uiop:emptyp
                 (uiop:split-string output :separator '(#\Newline))))))

(defun git-diff (repo-path base-ref head-ref)
  "Get diff between two refs."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "diff" base-ref head-ref)
    (declare (ignore _err))
    (when (zerop exit-code) output)))

(defun git-diff-stat (repo-path base-ref head-ref)
  "Get diff stat between two refs."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "diff" "--stat" base-ref head-ref)
    (declare (ignore _err))
    (when (zerop exit-code) output)))

(defun git-commit-count (repo-path &key (branch nil))
  "Count commits on a branch (or all if nil)."
  (let ((args (if branch
                  (list "rev-list" "--count" branch)
                  (list "rev-list" "--count" "--all"))))
    (multiple-value-bind (output _err exit-code)
        (apply #'git-run repo-path args)
      (declare (ignore _err))
      (when (zerop exit-code)
        (parse-integer output :junk-allowed t)))))

(defun git-repo-empty-p (repo-path)
  "Check if a repo has any commits."
  (multiple-value-bind (_out _err exit-code)
      (git-run repo-path "rev-parse" "HEAD")
    (declare (ignore _out _err))
    (/= exit-code 0)))

(defun git-tree (repo-path &key (ref "HEAD") (path ""))
  "Get structured directory listing at PATH under REF.
   Returns list of (:mode :type :hash :name) plists, directories first."
  (let ((target (if (uiop:emptyp path) ref (format nil "~A:~A" ref path))))
    (multiple-value-bind (output _err exit-code)
        (git-run repo-path "ls-tree" target)
      (declare (ignore _err))
      (when (zerop exit-code)
        (let ((entries nil))
          (dolist (line (uiop:split-string output :separator '(#\Newline)))
            (unless (uiop:emptyp line)
              ;; Format: <mode> SP <type> SP <hash> TAB <name>
              (let* ((tab-pos (position #\Tab line))
                     (meta (subseq line 0 tab-pos))
                     (name (subseq line (1+ tab-pos)))
                     (parts (uiop:split-string meta :separator '(#\Space))))
                (when (= (length parts) 3)
                  (push (list :mode (first parts)
                              :type (second parts)
                              :hash (third parts)
                              :name name)
                        entries)))))
          ;; Sort: directories first, then alphabetical
          (sort (nreverse entries)
                (lambda (a b)
                  (let ((a-dir (equal (getf a :type) "tree"))
                        (b-dir (equal (getf b :type) "tree")))
                    (if (eq a-dir b-dir)
                        (string< (getf a :name) (getf b :name))
                        a-dir)))))))))

(defun git-blob (repo-path ref path)
  "Read file content at PATH under REF. Returns string or NIL."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "cat-file" "blob" (format nil "~A:~A" ref path))
    (declare (ignore _err))
    (when (zerop exit-code) output)))

(defun git-blob-size (repo-path ref path)
  "Get file size in bytes at PATH under REF. Returns integer or NIL."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "cat-file" "-s" (format nil "~A:~A" ref path))
    (declare (ignore _err))
    (when (zerop exit-code)
      (parse-integer output :junk-allowed t))))

(defun git-blob-binary-p (content)
  "Check if CONTENT appears to be binary (contains null bytes)."
  (and content (position (code-char 0) content)))

(defun git-readme-path (repo-path &key (ref "HEAD"))
  "Find a README file in the root tree. Returns filename or NIL."
  (let ((tree (git-tree repo-path :ref ref)))
    (when tree
      (let ((candidates (remove-if-not
                         (lambda (entry)
                           (and (equal (getf entry :type) "blob")
                                (let ((name (string-downcase (getf entry :name))))
                                  (or (string= name "readme.md")
                                      (string= name "readme")
                                      (string= name "readme.txt")
                                      (string= name "readme.org")
                                      (string= name "readme.rst")))))
                         tree)))
        ;; Prefer .md, then no extension, then others
        (or (find "readme.md" candidates
                  :key (lambda (e) (string-downcase (getf e :name))) :test #'string=)
            (find "readme" candidates
                  :key (lambda (e) (string-downcase (getf e :name))) :test #'string=)
            (first candidates))))))

(defun render-markdown (markdown-string)
  "Render Markdown to sanitized HTML string."
  (let ((raw-html (with-output-to-string (s)
                    (3bmd:parse-string-and-print-to-stream markdown-string s))))
    (sanitize-html:sanitize raw-html)))

(defun file-language (filename)
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
