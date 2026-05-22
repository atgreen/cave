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

(defun git-tags (repo-path)
  "List tags in a bare repo. Returns list of tag name strings, newest first."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "tag" "--sort=-creatordate")
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

(defun git-show-commit (repo-path hash)
  "Get a single commit's metadata. Returns plist or NIL."
  (let ((format-str "%H%n%h%n%an%n%ae%n%ai%n%s%n%b"))
    (multiple-value-bind (output _err exit-code)
        (git-run repo-path "show" "--no-patch" (format nil "--format=~A" format-str) hash)
      (declare (ignore _err))
      (when (zerop exit-code)
        (let ((lines (uiop:split-string output :separator '(#\Newline))))
          (when (>= (length lines) 6)
            (list :hash (pop lines)
                  :short-hash (pop lines)
                  :author (pop lines)
                  :author-email (pop lines)
                  :date (pop lines)
                  :subject (pop lines)
                  :body (string-trim '(#\Newline #\Space)
                                     (format nil "~{~A~^~%~}" lines)))))))))

(defun git-format-patch (repo-path hash)
  "Get a single commit as a git format-patch (email-style patch). Returns string."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "format-patch" "--stdout" "-1" hash)
    (declare (ignore _err))
    (when (zerop exit-code) output)))

(defun git-commit-diff (repo-path hash)
  "Get the diff for a single commit. Returns raw diff text."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "diff-tree" "-p" "--root" hash)
    (declare (ignore _err))
    (when (zerop exit-code) output)))

(defun git-commit-stat (repo-path hash)
  "Get the diff stat for a single commit."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "diff-tree" "--stat" "--root" hash)
    (declare (ignore _err))
    (when (zerop exit-code) output)))

(defun git-merge-branch (repo-path source-branch target-branch &key squash)
  "Merge SOURCE-BRANCH into TARGET-BRANCH in a bare repo.
   If SQUASH is T, squash all commits into one.
   Uses a temporary worktree. Returns T on success, NIL on failure."
  (let ((tmpdir (format nil "/tmp/cave-merge-~A" (ironclad:byte-array-to-hex-string
                                                   (ironclad:random-data 8)))))
    (unwind-protect
         (let ((exit-code
                (progn
                  (multiple-value-bind (_o _e code)
                      (git-run repo-path "worktree" "add" tmpdir target-branch)
                    (declare (ignore _o _e))
                    (unless (zerop code) (return-from git-merge-branch nil)))
                  (if squash
                      ;; Squash merge: merge --squash then commit
                      (multiple-value-bind (_o _e code)
                          (uiop:run-program
                           (list "git" "-C" tmpdir "merge" "--squash" source-branch)
                           :output '(:string :stripped t)
                           :error-output '(:string :stripped t)
                           :ignore-error-status t)
                        (declare (ignore _o _e))
                        (unless (zerop code) (return-from git-merge-branch nil))
                        ;; Commit the squashed changes
                        (multiple-value-bind (_o2 _e2 code2)
                            (uiop:run-program
                             (list "git" "-C" tmpdir "commit" "--no-edit"
                                   "-m" (format nil "Squash merge ~A into ~A"
                                                source-branch target-branch))
                             :output '(:string :stripped t)
                             :error-output '(:string :stripped t)
                             :ignore-error-status t)
                          (declare (ignore _o2 _e2))
                          code2))
                      ;; Regular merge
                      (multiple-value-bind (_o _e code)
                          (uiop:run-program
                           (list "git" "-C" tmpdir "merge" "--no-edit" source-branch)
                           :output '(:string :stripped t)
                           :error-output '(:string :stripped t)
                           :ignore-error-status t)
                        (declare (ignore _o _e))
                        code)))))
           (zerop exit-code))
      (uiop:run-program (list "git" "-C" (namestring repo-path)
                              "worktree" "remove" "--force" tmpdir)
                         :ignore-error-status t
                         :output :string :error-output :string)
      (when (probe-file tmpdir)
        (uiop:delete-directory-tree (pathname tmpdir) :validate t :if-does-not-exist :ignore)))))

(defun inject-auth-token (url token)
  "Insert TOKEN into a URL: https://TOKEN@host/path. Returns URL unchanged if no token."
  (if token
      (let ((pos (search "://" url)))
        (if pos
            (format nil "~A://~A@~A"
                    (subseq url 0 pos)
                    token
                    (subseq url (+ pos 3)))
            url))
      url))

(defun git-clone-bare-from-url (url dest-path &key auth-token)
  "Clone a bare repo from a remote URL. Returns (VALUES success-p error-string)."
  (let ((effective-url (inject-auth-token url auth-token)))
    (multiple-value-bind (output err exit-code)
        (uiop:run-program (list "git" "clone" "--bare" effective-url
                                (namestring dest-path))
                          :output '(:string :stripped t)
                          :error-output '(:string :stripped t)
                          :ignore-error-status t)
      (declare (ignore output))
      (values (zerop exit-code) err))))

(defun repo-name-from-url (url)
  "Extract a repo name from a git URL. E.g. https://github.com/foo/bar.git → bar"
  (let* ((trimmed (string-right-trim '(#\/) url))
         (last-slash (position #\/ trimmed :from-end t))
         (basename (if last-slash (subseq trimmed (1+ last-slash)) trimmed)))
    (if (uiop:string-suffix-p basename ".git")
        (subseq basename 0 (- (length basename) 4))
        basename)))

(defun git-push-mirror (repo-path remote-url &optional auth-token)
  "Push all refs to a remote URL. Returns (VALUES success-p error-string)."
  (let ((url (inject-auth-token remote-url auth-token)))
    (multiple-value-bind (output err exit-code)
        (git-run repo-path "push" "--mirror" url)
      (declare (ignore output))
      (values (zerop exit-code) err))))

(defun git-pull-mirror (repo-path remote-url &optional auth-token)
  "Fetch all refs from a remote URL into a bare repo.
   Auto-detects default branch and updates HEAD. Returns (VALUES success-p error-string)."
  (let ((url (inject-auth-token remote-url auth-token)))
    (multiple-value-bind (output err exit-code)
        (git-run repo-path "fetch" "--prune" url "+refs/*:refs/*")
      (declare (ignore output))
      (when (zerop exit-code)
        ;; Auto-detect default branch: try remote HEAD, fall back to master/main
        (multiple-value-bind (remote-head _e rc)
            (git-run repo-path "remote" "show" url)
          (declare (ignore _e))
          (when (zerop rc)
            (let ((line (find-if (lambda (l) (search "HEAD branch:" l))
                                 (uiop:split-string remote-head :separator '(#\Newline)))))
              (when line
                (let ((branch (string-trim '(#\Space) (subseq line (+ (search ":" line) 1)))))
                  (when (and branch (not (uiop:emptyp branch)))
                    (git-run repo-path "symbolic-ref" "HEAD"
                             (format nil "refs/heads/~A" branch)))))))))
      (values (zerop exit-code) err))))

(defun git-delete-branch (repo-path branch)
  "Delete a branch in a bare repo."
  (multiple-value-bind (_out _err exit-code)
      (git-run repo-path "branch" "-D" branch)
    (declare (ignore _out _err))
    (zerop exit-code)))

(defun git-diff (repo-path base-ref head-ref)
  "Get diff between two refs."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "diff" base-ref head-ref)
    (declare (ignore _err))
    (when (zerop exit-code) output)))

(defun git-diff-merge-base (repo-path target-ref source-ref)
  "Get diff of changes introduced by source-ref relative to its merge-base with target-ref.
   Uses three-dot notation: target...source."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "diff" (format nil "~A...~A" target-ref source-ref))
    (declare (ignore _err))
    (when (zerop exit-code) output)))

(defun git-diff-stat (repo-path base-ref head-ref)
  "Get diff stat between two refs."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "diff" "--stat" base-ref head-ref)
    (declare (ignore _err))
    (when (zerop exit-code) output)))

(defun parse-hunk-header (line)
  "Parse @@ -old-start,old-count +new-start,new-count @@ from a hunk header.
   Returns (VALUES old-start new-start)."
  (let ((at-pos (position #\@ line :start 2)))
    (when at-pos
      (let* ((range (string-trim '(#\Space #\@) (subseq line 2 (+ at-pos 1))))
             (parts (uiop:split-string range :separator '(#\Space)))
             (old-part (first parts))
             (new-part (second parts)))
        (values
         (when old-part
           (parse-integer (subseq old-part 1) :junk-allowed t))
         (when new-part
           (parse-integer (subseq new-part 1) :junk-allowed t)))))))

(defun parse-diff (diff-text)
  "Parse unified diff text into a list of file diffs.
   Each file is a plist: (:filename :old-filename :lines).
   Each line in :lines is a plist with :type, :content, :old-line, :new-line."
  (when (and diff-text (not (uiop:emptyp diff-text)))
    (let ((files nil)
          (current-file nil)
          (current-lines nil)
          (old-line 0)
          (new-line 0))
      (dolist (line (uiop:split-string diff-text :separator '(#\Newline)))
        (cond
          ;; New file header
          ((and (>= (length line) 6) (string= "diff --" (subseq line 0 7)))
           (when current-file
             (push (list :filename (getf current-file :filename)
                         :old-filename (getf current-file :old-filename)
                         :lines (nreverse current-lines))
                   files))
           (setf current-lines nil old-line 0 new-line 0)
           (let* ((parts (uiop:split-string line :separator '(#\Space)))
                  (b-file (car (last parts)))
                  (filename (if (and (>= (length b-file) 2)
                                     (char= (char b-file 0) #\b)
                                     (char= (char b-file 1) #\/))
                                (subseq b-file 2)
                                b-file)))
             (setf current-file (list :filename filename :old-filename nil))))
          ;; Hunk header
          ((and (>= (length line) 3) (string= "@@" (subseq line 0 2)))
           (multiple-value-bind (os ns) (parse-hunk-header line)
             (setf old-line (or os 1) new-line (or ns 1)))
           (push (list :type :hunk :content line
                       :old-line nil :new-line nil)
                 current-lines))
          ;; Added line
          ((and (> (length line) 0) (char= (char line 0) #\+)
                (not (and (>= (length line) 3) (string= "+++" (subseq line 0 3)))))
           (push (list :type :add :content (subseq line 1)
                       :old-line nil :new-line new-line)
                 current-lines)
           (incf new-line))
          ;; Deleted line
          ((and (> (length line) 0) (char= (char line 0) #\-)
                (not (and (>= (length line) 3) (string= "---" (subseq line 0 3)))))
           (push (list :type :del :content (subseq line 1)
                       :old-line old-line :new-line nil)
                 current-lines)
           (incf old-line))
          ;; Meta lines
          ((or (and (>= (length line) 3) (string= "---" (subseq line 0 3)))
               (and (>= (length line) 3) (string= "+++" (subseq line 0 3)))
               (and (>= (length line) 5) (string= "index" (subseq line 0 5))))
           nil)
          ;; Context line
          (t
           (when current-file
             (push (list :type :context
                         :content (if (and (> (length line) 0)
                                           (char= (char line 0) #\Space))
                                      (subseq line 1)
                                      line)
                         :old-line old-line :new-line new-line)
                   current-lines)
             (incf old-line)
             (incf new-line)))))
      (when current-file
        (push (list :filename (getf current-file :filename)
                    :old-filename (getf current-file :old-filename)
                    :lines (nreverse current-lines))
              files))
      (nreverse files))))

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
        (git-run repo-path "ls-tree" "-l" target)
      (declare (ignore _err))
      (when (zerop exit-code)
        (let ((entries nil))
          (dolist (line (uiop:split-string output :separator '(#\Newline)))
            (unless (uiop:emptyp line)
              ;; Format with -l: <mode> SP <type> SP <hash> SP+ <size|-> TAB <name>
              (let* ((tab-pos (position #\Tab line))
                     (meta (subseq line 0 tab-pos))
                     (name (subseq line (1+ tab-pos)))
                     (parts (remove "" (uiop:split-string meta :separator '(#\Space)) :test #'equal)))
                (when (>= (length parts) 4)
                  (push (list :mode (first parts)
                              :type (second parts)
                              :hash (third parts)
                              :size (parse-integer (fourth parts) :junk-allowed t)
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

(defun git-blob-hash (repo-path ref path)
  "Get the git object hash for a blob at PATH under REF. Returns SHA string or NIL."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "rev-parse" (format nil "~A:~A" ref path))
    (declare (ignore _err))
    (when (zerop exit-code) (string-trim '(#\Newline #\Space) output))))

(defun git-blob (repo-path ref path)
  "Read file content at PATH under REF. Returns string or NIL."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "cat-file" "blob" (format nil "~A:~A" ref path))
    (declare (ignore _err))
    (when (zerop exit-code) output)))

(defun git-object-is-tree-p (repo-path ref path)
  "Return T if PATH under REF is a tree (directory)."
  (multiple-value-bind (output _err exit-code)
      (git-run repo-path "cat-file" "-t" (format nil "~A:~A" ref path))
    (declare (ignore _err))
    (and (zerop exit-code)
         (equal (string-trim '(#\Newline #\Space) output) "tree"))))

(defun git-blob-bytes (repo-path ref path)
  "Read file content at PATH under REF as raw octets. Returns byte vector or NIL."
  (let* ((cmd (format nil "git -C ~A cat-file blob ~A:~A"
                      (uiop:escape-sh-token (namestring repo-path))
                      (uiop:escape-sh-token ref)
                      (uiop:escape-sh-token path)))
         (process (uiop:launch-program cmd :output :stream
                                       :element-type '(unsigned-byte 8)
                                       :force-shell t))
         (stream (uiop:process-info-output process)))
    (unwind-protect
         (let ((bytes (handler-case
                          (let ((buf (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
                                (chunk (make-array 4096 :element-type '(unsigned-byte 8))))
                            (loop for n = (read-sequence chunk stream)
                                  while (plusp n)
                                  do (loop for i below n do (vector-push-extend (aref chunk i) buf)))
                            buf)
                        (error () nil))))
           (let ((exit (uiop:wait-process process)))
             (when (zerop exit) bytes)))
      (close stream))))

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

(defun git-blob-binary-check (repo-path ref path)
  "Check if blob at PATH under REF is binary.
Reads the blob as text; if UTF-8 decoding fails, it's binary.
Otherwise checks for null bytes."
  (handler-case
      (let ((content (git-blob repo-path ref path)))
        (git-blob-binary-p content))
    (error () t)))

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

(defun render-markdown (markdown-string &key raw-base-url)
  "Render Markdown to sanitized HTML string.
   RAW-BASE-URL when provided rewrites relative image src to absolute URLs
   before sanitization (the sanitizer strips relative src as protocol-less)."
  (let* ((3bmd-tables:*tables* t)
         (3bmd-code-blocks:*code-blocks* t)
         (raw-html (with-output-to-string (s)
                     (3bmd:parse-string-and-print-to-stream markdown-string s)))
         ;; Rewrite relative URLs BEFORE sanitization so they have a protocol
         (rewritten (if raw-base-url
                        (rewrite-relative-img-src raw-html raw-base-url)
                        raw-html)))
    (sanitize-html:sanitize rewritten)))

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
                ;; Rewrite relative URL
                (let ((new-url (format nil "~A~A" base-url url)))
                  (setf result (concatenate 'string
                                            (subseq result 0 url-start)
                                            new-url
                                            (subseq result url-end)))
                  (setf pos (+ url-start (length new-url) 1))))))))))

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
