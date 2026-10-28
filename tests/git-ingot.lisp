;;; git-ingot.lisp — the Ingot git backend answers as the git CLI does
;;;
;;; Builds a small repository with the git binary — two branches and a merge,
;;; an annotated and a lightweight tag, a binary file, a file without a final
;;; newline, a mode change, a deletion, a rename, an empty file, UTF-8, a
;;; multi-paragraph message with trailers, a subdirectory — then asks each
;;; operation of git.lisp twice, through its CLI body (%cli-...) and through
;;; Ingot (%ingot-..., src/git-ingot.lisp), and compares the answers. The
;;; writes (branches, tags, merges of every strategy, a committed file) are
;;; made on two copies of the repository, one per backend, and the copies are
;;; then compared through git itself.
;;;
;;;   make test-git-ingot        (a build with INGOT=1; git on PATH)
;;;
;;; One line per check; the Lisp exits 0 when every check passes, 1 otherwise.
;;;
;;; SPDX-License-Identifier: MIT

(unless (find-package :cave)
  (pushnew :cave-ingot *features*)
  (asdf:load-system :cave))

(in-package #:cl-user)

(defvar *failures* nil)
(defvar *checks* 0)

(defmacro check (name form)
  `(let ((ok (handler-case ,form
               (error (e) (format t "~&      ~a~%" e) nil))))
     (incf *checks*)
     (format t "~&~a ~a~%" (if ok "ok  " "FAIL") ,name)
     (unless ok (push ,name *failures*))
     ok))

(defun show-difference (a b)
  "Print the two answers; for two strings, the first line where they part."
  (if (and (stringp a) (stringp b))
      (let ((la (uiop:split-string a :separator '(#\Newline)))
            (lb (uiop:split-string b :separator '(#\Newline))))
        (loop for i from 0
              for x = (nth i la) for y = (nth i lb)
              while (or x y)
              unless (equal x y)
                do (format t "~&      line ~d~%      cli:   ~s~%      ingot: ~s~%" (1+ i) x y)
                   (return)))
      (format t "~&      cli:   ~s~%      ingot: ~s~%" a b)))

(defmacro same (name cli-form ingot-form &key (test '#'equal) (normalize '#'identity))
  "Check that the CLI and the Ingot answers agree, after NORMALIZE."
  `(check ,name
     (let ((a (funcall ,normalize ,cli-form))
           (b (funcall ,normalize ,ingot-form)))
       (or (funcall ,test a b)
           (progn (show-difference a b) nil)))))

(defun sh (&rest args)
  (uiop:run-program args :output '(:string :stripped t) :error-output :string))

(defun git (dir &rest args)
  (apply #'sh "git" "-C" (namestring dir) args))

(defun write-text (dir relative text)
  (let ((file (merge-pathnames relative dir)))
    (ensure-directories-exist file)
    (with-open-file (s file :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string text s))
    file))

(defun write-bytes (dir relative bytes)
  (let ((file (merge-pathnames relative dir)))
    (ensure-directories-exist file)
    (with-open-file (s file :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-sequence (coerce bytes '(vector (unsigned-byte 8))) s))
    file))

(defun hunk-header-only (text)
  "TEXT with the function context after each hunk's second @@ dropped (Ingot
prints none; git prints its funcname guess), and CRs left alone."
  (when text
    (with-output-to-string (out)
      (dolist (line (uiop:split-string text :separator '(#\Newline)))
        (if (and (uiop:string-prefix-p "@@ " line)
                 (search " @@" line :start2 3))
            (write-string (subseq line 0 (+ 3 (search " @@" line :start2 3))) out)
            (write-string line out))
        (terpri out)))))

(defun stat-shape (text)
  "A --stat text reduced to what both backends must agree on: each file's name
and change count, and the summary line; the +/- graph is not compared."
  (when text
    (let ((lines (remove "" (uiop:split-string text :separator '(#\Newline)) :test #'string=)))
      (mapcar (lambda (line)
                (let ((bar (position #\| line)))
                  (if bar
                      (let* ((count-part (string-trim " " (subseq line (1+ bar))))
                             (space (position #\Space count-part)))
                        (list (string-trim " " (subseq line 0 bar))
                              (if space (subseq count-part 0 space) count-part)))
                      line)))
              lines))))

(defun patch-shape (text)
  "A format-patch text with the hunk contexts dropped and the trailer (git's
version line) cut."
  (when text
    (let ((end (search (format nil "~%-- ~%") text)))
      (hunk-header-only (if end (subseq text 0 end) text)))))

(defun table-alist (table)
  "A hash table as a sorted alist, so two tables compare and print plainly."
  (let ((rows nil))
    (maphash (lambda (k v) (push (cons k v) rows)) table)
    (sort rows #'string< :key #'car)))

(defun as-string (octets-or-string)
  (if (stringp octets-or-string)
      octets-or-string
      (map 'string #'code-char octets-or-string)))

(defun advertised-refs (advertisement)
  "The (oid . name) pairs an advertisement lists, sorted, capabilities dropped."
  (let ((text (as-string advertisement)) (pairs nil) (at 0))
    (loop
      (let ((pos (search " " text :start2 at)))
        (unless pos (return))
        (when (and (>= pos 40)
                   (every (lambda (c) (digit-char-p c 16)) (subseq text (- pos 40) pos)))
          (let ((end (position-if (lambda (c) (or (char= c #\Newline) (char= c (code-char 0)))) text :start pos)))
            (push (cons (subseq text (- pos 40) pos) (subseq text (1+ pos) (or end (length text)))) pairs)))
        (setf at (1+ pos))))
    (sort pairs #'string< :key (lambda (p) (concatenate 'string (cdr p) (car p))))))

(defun pack-object-count (response scratch tag)
  "Index the pack RESPONSE carries (after its NAK line) with git; the number of
objects it holds, or NIL when git refuses it."
  (let* ((octets (if (stringp response) (map '(vector (unsigned-byte 8)) #'char-code response) response))
         (nak (format nil "0008NAK~%"))
         (file (merge-pathnames (format nil "~a.pack" tag) scratch)))
    (when (and (>= (length octets) 8)
               (string= nak (map 'string #'code-char (subseq octets 0 8))))
      (with-open-file (s file :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
        (write-sequence (subseq octets 8) s))
      (sh "git" "index-pack" (namestring file))
      (count-if (lambda (line) (and (> (length line) 41)
                                    (member (subseq line 41 (or (position #\Space line :start 41) (length line)))
                                            '("commit" "tree" "blob" "tag") :test #'string=)))
                (uiop:split-string (sh "git" "verify-pack" "-v" (namestring file)) :separator '(#\Newline))))))

(defun ref-state (bare)
  "What a copy of the repository looks like after the writes: branches and tags
with their subjects, parents' count and trees — the committer dates differ
between the copies, the rest must not."
  (list :branches (git bare "for-each-ref" "--format=%(refname) %(subject)" "refs/heads")
        :tags (git bare "for-each-ref" "--format=%(refname) %(*subject)" "refs/tags")
        :trees (git bare "for-each-ref" "--format=%(refname) %(tree)" "refs/heads")
        :parents (git bare "log" "--all" "--format=%s %p" "--no-walk=unsorted")))

;;; --- The repository -----------------------------------------------------

(defvar *scratch* (merge-pathnames (format nil "cave-git-ingot-~a/" (random 1000000000))
                                   (uiop:temporary-directory)))
(ensure-directories-exist *scratch*)
;; The committer only: `git tag -a` on the bare copies needs an identity, and
;; GIT_AUTHOR_* would override the author the operations pass with -c.
(setf (uiop:getenv "GIT_COMMITTER_NAME") "Test Person"
      (uiop:getenv "GIT_COMMITTER_EMAIL") "test@example.org")

(defvar *work* (merge-pathnames "work/" *scratch*))
(defvar *bare* (merge-pathnames "repo.git/" *scratch*))
(defvar *empty* (merge-pathnames "empty.git/" *scratch*))

(ensure-directories-exist *work*)
(git *work* "init" "-q" "-b" "main")
(git *work* "config" "user.name" "Test Person")
(git *work* "config" "user.email" "test@example.org")
(write-text *work* "README.md" (format nil "# Project~%~%A readme.~%"))
(write-text *work* "src/a.lisp" (format nil "(defun a () 1)~%"))
(write-text *work* "src/sub/b.lisp" (format nil "(defun b () 2)~%"))
(write-text *work* "bin/run.sh" (format nil "#!/bin/sh~%echo run~%"))
(sh "chmod" "+x" (namestring (merge-pathnames "bin/run.sh" *work*)))
(write-bytes *work* "image.bin" '(137 80 78 71 0 0 0 13 0 1 2 3 255 0 0 9))
(write-text *work* "nonl.txt" "one
two")
(git *work* "add" "-A")
(git *work* "commit" "-q" "-m" "Initial import

The first paragraph of the body
runs over two lines.

Signed-off-by: Test Person <test@example.org>
Co-Authored-By: Pair Programmer <pair@example.org>")
(write-text *work* "src/a.lisp" (format nil "(defun a () 1)~%(defun a2 () 2)~%"))
(git *work* "rm" "-q" "src/sub/b.lisp")
(write-text *work* "docs/notes.md" (format nil "Notes~%~%Fixes: #12 in prose, not a trailer.~%More prose.~%"))
(git *work* "add" "-A")
(git *work* "commit" "-q" "-m" "Change a, drop b, add notes

Fixes: #12 in prose, not a trailer.
More prose here so the block is not all trailers.")
(git *work* "tag" "-a" "v0.1" "-m" "Version 0.1")
(git *work* "checkout" "-q" "-b" "side")
(write-text *work* "nonl.txt" "one
two
three")
(write-text *work* "src/c.lisp" (format nil "(defun c () 3)~%"))
(git *work* "add" "-A")
(git *work* "commit" "-q" "-m" "Side work")
(git *work* "checkout" "-q" "main")
(write-text *work* "README.md" (format nil "# Project~%~%A readme, revised.~%~%With a section.~%"))
(git *work* "mv" "src/a.lisp" "src/a2.lisp")
(sh "chmod" "-x" (namestring (merge-pathnames "bin/run.sh" *work*)))
(git *work* "add" "-A")
(git *work* "commit" "-q" "-m" "Main work: revise, rename, chmod")
(git *work* "merge" "-q" "--no-ff" "--no-edit" "side")
(write-text *work* "empty.txt" "")
(write-text *work* "utf8.txt" (format nil "héllo wörld — ünïcode~%"))
(write-text *work* "nonl.txt" (format nil "one~%two~%three~%"))
(git *work* "add" "-A")
(git *work* "commit" "-q" "-m" "Empty and UTF-8 files; nonl gets its newline")
(git *work* "tag" "v0.2")
(git *work* "checkout" "-q" "-b" "conflict" "v0.1")
(write-text *work* "README.md" (format nil "# Project~%~%A readme, rewritten in conflict.~%"))
(git *work* "commit" "-q" "-am" "Conflicting edit")
(git *work* "checkout" "-q" "-b" "feature" "v0.1")
(write-text *work* "docs/feature.md" (format nil "A feature.~%"))
(git *work* "add" "-A")
(git *work* "commit" "-q" "-m" "Add the feature doc

Signed-off-by: Test Person <test@example.org>")
(git *work* "checkout" "-q" "main")
(git *work* "init" "-q" "--bare" (namestring *bare*))
(git *work* "push" "-q" (namestring *bare*) "--all")
(git *work* "push" "-q" (namestring *bare*) "--tags")
(git *bare* "symbolic-ref" "HEAD" "refs/heads/main")
(sh "git" "init" "-q" "--bare" "-b" "main" (namestring *empty*))

(defvar *commits* (uiop:split-string (git *bare* "rev-list" "--all") :separator '(#\Newline)))
(defvar *head* (git *bare* "rev-parse" "HEAD"))

(format t "~&repository at ~a: ~d commits~%" *bare* (length *commits*))

;;; --- Reads ----------------------------------------------------------------

(same "branches" (cave::%cli-git-branches *bare*) (cave::%ingot-git-branches *bare*))
(same "tags, newest first" (cave::%cli-git-tags *bare*) (cave::%ingot-git-tags *bare*))
(same "default branch" (cave::%cli-git-default-branch *bare*) (cave::%ingot-git-default-branch *bare*))
(same "empty? no" (cave::%cli-git-repo-empty-p *bare*) (cave::%ingot-git-repo-empty-p *bare*))
;; Not compared with the CLI: `git rev-parse HEAD` exits 0 on an unborn HEAD
;; with git 2.4x (printing "HEAD"), so the CLI body calls an empty repository
;; non-empty. Ingot answers the truth.
(check "empty? yes" (cave::%ingot-git-repo-empty-p *empty*))
(same "branches of an empty repository" (cave::%cli-git-branches *empty*) (cave::%ingot-git-branches *empty*))
(same "log of an empty repository" (cave::%cli-git-log *empty*) (cave::%ingot-git-log *empty*))
(dolist (ref (list "HEAD" "main" "side" "v0.1" "v0.2" (subseq *head* 0 7) "refs/heads/feature" "nonesuch" "src/a.lisp"))
  (same (format nil "rev-parse ~a" ref) (cave::%cli-git-rev-parse *bare* ref) (cave::%ingot-git-rev-parse *bare* ref))
  (same (format nil "has-commit-p ~a" ref) (cave::%cli-git-has-commit-p *bare* ref) (cave::%ingot-git-has-commit-p *bare* ref)))
(dolist (pair '(("v0.1" "main") ("main" "v0.1") ("side" "main") ("conflict" "main") ("nonesuch" "main") ("main" "main")))
  (same (format nil "is-ancestor-p ~a ~a" (first pair) (second pair))
        (cave::%cli-git-is-ancestor-p *bare* (first pair) (second pair))
        (cave::%ingot-git-is-ancestor-p *bare* (first pair) (second pair)))
  (same (format nil "commit-ancestor-p ~a ~a" (first pair) (second pair))
        (cave::%cli-git-commit-ancestor-p *bare* (first pair) (second pair))
        (cave::%ingot-git-commit-ancestor-p *bare* (first pair) (second pair)))
  (same (format nil "merge-base ~a ~a" (first pair) (second pair))
        (cave::%cli-git-merge-base *bare* (first pair) (second pair))
        (cave::%ingot-git-merge-base *bare* (first pair) (second pair))))
(same "commit-count --all" (cave::%cli-git-commit-count *bare*) (cave::%ingot-git-commit-count *bare*))
(same "commit-count main" (cave::%cli-git-commit-count *bare* :branch "main") (cave::%ingot-git-commit-count *bare* :branch "main"))
(same "commit-count side" (cave::%cli-git-commit-count *bare* :branch "side") (cave::%ingot-git-commit-count *bare* :branch "side"))
(same "tag-exists-p v0.1" (cave::%cli-git-tag-exists-p *bare* "v0.1") (cave::%ingot-git-tag-exists-p *bare* "v0.1"))
(same "tag-exists-p v9" (cave::%cli-git-tag-exists-p *bare* "v9") (cave::%ingot-git-tag-exists-p *bare* "v9"))

(same "log HEAD" (cave::%cli-git-log *bare* :limit 100) (cave::%ingot-git-log *bare* :limit 100))
(same "log side" (cave::%cli-git-log *bare* :branch "side") (cave::%ingot-git-log *bare* :branch "side"))
(same "log -2" (cave::%cli-git-log *bare* :limit 2) (cave::%ingot-git-log *bare* :limit 2))
(same "log -- src" (cave::%cli-git-log *bare* :branch "main" :limit 100 :path "src")
      (cave::%ingot-git-log *bare* :branch "main" :limit 100 :path "src"))
(same "log -- README.md" (cave::%cli-git-log *bare* :branch "main" :limit 100 :path "README.md")
      (cave::%ingot-git-log *bare* :branch "main" :limit 100 :path "README.md"))
(same "log -- src/sub/b.lisp" (cave::%cli-git-log *bare* :branch "main" :limit 100 :path "src/sub/b.lisp")
      (cave::%ingot-git-log *bare* :branch "main" :limit 100 :path "src/sub/b.lisp"))
(same "log of a missing branch" (cave::%cli-git-log *bare* :branch "nonesuch") (cave::%ingot-git-log *bare* :branch "nonesuch"))

(dolist (hash *commits*)
  (let ((short (subseq hash 0 7)))
    (same (format nil "show-commit ~a" short) (cave::%cli-git-show-commit *bare* hash) (cave::%ingot-git-show-commit *bare* hash))
    (same (format nil "trailers ~a" short) (cave::%cli-git-commit-trailers *bare* hash) (cave::%ingot-git-commit-trailers *bare* hash))
    (same (format nil "signature-info ~a" short)
          (multiple-value-list (cave::%cli-git-commit-signature-info *bare* hash))
          (multiple-value-list (cave::%ingot-git-commit-signature-info *bare* hash)))
    (same (format nil "commit-diff ~a" short) (cave::%cli-git-commit-diff *bare* hash) (cave::%ingot-git-commit-diff *bare* hash)
          :normalize #'hunk-header-only)
    (same (format nil "commit-stat ~a" short) (cave::%cli-git-commit-stat *bare* hash) (cave::%ingot-git-commit-stat *bare* hash)
          :normalize #'stat-shape)
    (let ((cli (cave::%cli-git-format-patch *bare* hash))
          (merge? (> (length (uiop:split-string (git *bare* "rev-list" "--parents" "-1" hash) :separator '(#\Space))) 2)))
      (cond
        ;; `format-patch -1 <merge>` prints the nearest non-merge ancestor's
        ;; patch; Ingot prints nothing for a merge
        (merge? (check (format nil "format-patch ~a (a merge: nothing)" short)
                  (equal (cave::%ingot-git-format-patch *bare* hash) "")))
        ;; format-patch detects renames and Ingot does not: the renamed file
        ;; is a deletion and a creation there
        ((search "rename from " cli)
         (check (format nil "format-patch ~a (a rename: delete and add)" short)
           (let ((patch (cave::%ingot-git-format-patch *bare* hash)))
             (and (search "deleted file mode" patch) (search "new file mode" patch)))))
        ;; format-patch encodes a binary change as a git binary patch; Ingot
        ;; says the files differ, as `git diff` does
        ((search "GIT binary patch" cli)
         (check (format nil "format-patch ~a (binary: the files differ)" short)
           (let ((patch (cave::%ingot-git-format-patch *bare* hash)))
             (and (search "Binary files " patch)
                  (equal (patch-shape (subseq cli 0 (search "diff --git" cli)))
                         (patch-shape (subseq patch 0 (search "diff --git" patch))))))))
        (t (same (format nil "format-patch ~a" short) cli (cave::%ingot-git-format-patch *bare* hash)
                 :normalize #'patch-shape))))))
(same "show-commit of nothing" (cave::%cli-git-show-commit *bare* "nonesuch") (cave::%ingot-git-show-commit *bare* "nonesuch"))
(same "commit-diff of nothing" (cave::%cli-git-commit-diff *bare* "nonesuch") (cave::%ingot-git-commit-diff *bare* "nonesuch"))

(same "diff main...side" (cave::%cli-git-diff-merge-base *bare* "main" "side") (cave::%ingot-git-diff-merge-base *bare* "main" "side")
      :normalize #'hunk-header-only)
(same "diff main...conflict" (cave::%cli-git-diff-merge-base *bare* "main" "conflict") (cave::%ingot-git-diff-merge-base *bare* "main" "conflict")
      :normalize #'hunk-header-only)
(same "diff main...feature" (cave::%cli-git-diff-merge-base *bare* "main" "feature") (cave::%ingot-git-diff-merge-base *bare* "main" "feature")
      :normalize #'hunk-header-only)

(same "release notes v0.2 (since v0.1)" (cave::%cli-git-release-notes *bare* "v0.2") (cave::%ingot-git-release-notes *bare* "v0.2"))
(same "release notes v0.1 (all history)" (cave::%cli-git-release-notes *bare* "v0.1") (cave::%ingot-git-release-notes *bare* "v0.1"))

(same "list-files HEAD" (cave::%cli-git-list-files *bare* "HEAD") (cave::%ingot-git-list-files *bare* "HEAD"))
(same "list-files v0.1" (cave::%cli-git-list-files *bare* "v0.1") (cave::%ingot-git-list-files *bare* "v0.1"))
(same "list-files, capped" (cave::%cli-git-list-files *bare* "HEAD" :limit 3) (cave::%ingot-git-list-files *bare* "HEAD" :limit 3))
(same "tree /" (cave::%cli-git-tree *bare*) (cave::%ingot-git-tree *bare*))
(same "tree src" (cave::%cli-git-tree *bare* :path "src") (cave::%ingot-git-tree *bare* :path "src"))
(same "tree src/sub at v0.1^" (cave::%cli-git-tree *bare* :ref "v0.1^" :path "src/sub") (cave::%ingot-git-tree *bare* :ref (git *bare* "rev-parse" "v0.1^") :path "src/sub"))
(same "tree of a file" (cave::%cli-git-tree *bare* :path "README.md") (cave::%ingot-git-tree *bare* :path "README.md"))
(same "tree of nothing" (cave::%cli-git-tree *bare* :path "nope") (cave::%ingot-git-tree *bare* :path "nope"))
(same "tree at a bad ref" (cave::%cli-git-tree *bare* :ref "nonesuch") (cave::%ingot-git-tree *bare* :ref "nonesuch"))
(dolist (path '("README.md" "image.bin" "nonl.txt" "empty.txt" "utf8.txt" "bin/run.sh" "src" "missing.txt"))
  ;; git-blob is for text: the two decode bytes that are not UTF-8 differently
  ;; (replacement characters against Latin-1), so the binary file is compared
  ;; as bytes only
  (unless (string= path "image.bin")
    (same (format nil "blob ~a" path) (cave::%cli-git-blob *bare* "HEAD" path) (cave::%ingot-git-blob *bare* "HEAD" path)))
  (same (format nil "blob-bytes ~a" path) (cave::%cli-git-blob-bytes *bare* "HEAD" path) (cave::%ingot-git-blob-bytes *bare* "HEAD" path)
        :test #'equalp)
  (same (format nil "blob-size ~a" path) (cave::%cli-git-blob-size *bare* "HEAD" path) (cave::%ingot-git-blob-size *bare* "HEAD" path))
  (same (format nil "blob-hash ~a" path) (cave::%cli-git-blob-hash *bare* "HEAD" path) (cave::%ingot-git-blob-hash *bare* "HEAD" path)))
(same "blob at side" (cave::%cli-git-blob *bare* "side" "nonl.txt") (cave::%ingot-git-blob *bare* "side" "nonl.txt"))
(same "language stats" (cave::%cli-git-language-stats *bare* "HEAD") (cave::%ingot-git-language-stats *bare* "HEAD"))
(let ((names (mapcar (lambda (e) (getf e :name)) (cave::%cli-git-tree *bare*))))
  (same "tree-last-commits /"
        (cave::%cli-git-tree-last-commits *bare* "HEAD" "" names)
        (cave::%ingot-git-tree-last-commits *bare* "HEAD" "" names)
        :normalize #'table-alist))
(let ((names (mapcar (lambda (e) (getf e :name)) (cave::%cli-git-tree *bare* :path "src"))))
  (same "tree-last-commits src"
        (cave::%cli-git-tree-last-commits *bare* "HEAD" "src" names)
        (cave::%ingot-git-tree-last-commits *bare* "HEAD" "src" names)
        :normalize #'table-alist))

(same "merge-conflicts main conflict"
      (multiple-value-list (cave::%cli-git-merge-conflicts *bare* "main" "conflict"))
      (multiple-value-list (cave::%ingot-git-merge-conflicts *bare* "main" "conflict")))
(same "merge-conflicts main feature"
      (multiple-value-list (cave::%cli-git-merge-conflicts *bare* "main" "feature"))
      (multiple-value-list (cave::%ingot-git-merge-conflicts *bare* "main" "feature")))
;; Not compared with the CLI: `git merge-tree` exits 1 for a branch that is
;; not there, which the CLI body reads as "conflicts, no files"; its docstring
;; asks for (NIL NIL) when the answer is indeterminate, and Ingot gives that.
(check "merge-conflicts main nonesuch: indeterminate, so no conflict"
  (equal (multiple-value-list (cave::%ingot-git-merge-conflicts *bare* "main" "nonesuch")) '(nil nil)))

;;; --- The smart HTTP protocol -------------------------------------------

(same "advertised refs" (cave::%cli-git-advertise-refs *bare*) (cave::%ingot-git-advertise-refs *bare*)
      :normalize #'advertised-refs)
(let ((request (map '(vector (unsigned-byte 8)) #'char-code
                    (format nil "0032want ~a~%00000009done~%" *head*))))
  (same "upload-pack: the pack of a clone"
        (pack-object-count (cave::%cli-git-upload-pack *bare* request) *scratch* "cli")
        (pack-object-count (cave::%ingot-git-upload-pack *bare* request) *scratch* "ingot"))
  (check "upload-pack: the pack holds every object"
    (= (pack-object-count (cave::%ingot-git-upload-pack *bare* request) *scratch* "ingot2")
       (parse-integer (git *bare* "rev-list" "--objects" "HEAD" "--count")))))

;;; --- Writes, one copy per backend ---------------------------------------

(defvar *cli-copy* (merge-pathnames "cli.git/" *scratch*))
(defvar *ingot-copy* (merge-pathnames "ingot.git/" *scratch*))
(sh "cp" "-r" (string-right-trim "/" (namestring *bare*)) (string-right-trim "/" (namestring *cli-copy*)))
(sh "cp" "-r" (string-right-trim "/" (namestring *bare*)) (string-right-trim "/" (namestring *ingot-copy*)))

(defmacro both (name (var) &body body)
  "Evaluate BODY with VAR bound to each copy, as the CLI then as Ingot, and
check the two answers agree."
  `(same ,name
         (let ((,var *cli-copy*)) (macrolet ((op (name &rest args) `(,(intern (format nil "%CLI-~a" name) :cave) ,@args))) ,@body))
         (let ((,var *ingot-copy*)) (macrolet ((op (name &rest args) `(,(intern (format nil "%INGOT-~a" name) :cave) ,@args))) ,@body))))

(both "create-branch topic from v0.1" (r) (multiple-value-list (op git-create-branch r "topic" "v0.1")))
(both "create-branch topic again fails" (r) (values (op git-create-branch r "topic" "v0.1")))
(both "create-branch from nothing fails" (r) (values (op git-create-branch r "topic2" "nonesuch")))
(both "create-branch with a bad name fails" (r) (values (op git-create-branch r "bad..name" "main")))
(both "delete-branch topic" (r) (op git-delete-branch r "topic"))
(both "delete-branch nonesuch fails" (r) (op git-delete-branch r "nonesuch"))
(both "create-tag lightweight" (r) (op git-create-tag r "t-light" "main"))
(both "create-tag annotated" (r) (op git-create-tag r "t-ann" "main" :message "An annotated tag"))
(both "create-tag that exists fails" (r) (op git-create-tag r "t-ann" "main"))
(both "create-tag at nothing fails" (r) (op git-create-tag r "t-none" "nonesuch"))
(same "the lightweight tags agree" (git *cli-copy* "rev-parse" "t-light") (git *ingot-copy* "rev-parse" "t-light"))
(same "the annotated tags agree, but for the tagger"
      (remove-if (lambda (l) (uiop:string-prefix-p "tagger " l)) (uiop:split-string (git *cli-copy* "cat-file" "-p" "t-ann") :separator '(#\Newline)))
      (remove-if (lambda (l) (uiop:string-prefix-p "tagger " l)) (uiop:split-string (git *ingot-copy* "cat-file" "-p" "t-ann") :separator '(#\Newline))))
(both "commit-file-on-branch" (r) (and (op git-commit-file-on-branch r "main" "fix/thing" "docs/fix.md" (format nil "fixed~%") "Fix the thing"
                                           :author-name "Fixer" :author-email "fixer@example.org")
                                       t))
(both "commit-file-on-branch onto an existing branch fails" (r)
  (op git-commit-file-on-branch r "main" "fix/thing" "docs/fix.md" "again" "Again"))
(both "commit-file-on-branch from a missing base fails" (r)
  (op git-commit-file-on-branch r "nonesuch" "fix/other" "docs/fix.md" "x" "X"))
(same "the committed files agree"
      (list (git *cli-copy* "log" "-1" "--format=%s|%an|%ae|%p" "fix/thing") (git *cli-copy* "show" "fix/thing:docs/fix.md"))
      (list (git *ingot-copy* "log" "-1" "--format=%s|%an|%ae|%p" "fix/thing") (git *ingot-copy* "show" "fix/thing:docs/fix.md")))
(both "merge feature into main (--no-ff)" (r) (op git-merge-branch r "feature" "main" :author "Merger <m@example.org>" :message "Merge the feature"))
(both "merge conflict into main fails" (r) (op git-merge-branch r "conflict" "main" :author "Merger <m@example.org>"))
(both "create-branch release from v0.1" (r) (values (op git-create-branch r "release" "v0.1")))
(both "squash side into release" (r) (op git-merge-branch r "side" "release" :strategy "squash" :author "Merger <m@example.org>" :message "Squash side"))
(both "squash again: nothing to commit" (r) (op git-merge-branch r "side" "release" :strategy "squash" :author "Merger <m@example.org>"))
(both "create-branch ff from v0.1" (r) (values (op git-create-branch r "ff" "v0.1")))
(both "fast-forward-only feature into ff" (r) (op git-merge-branch r "feature" "ff" :strategy "fast-forward-only"))
(both "fast-forward-only conflict into release fails" (r) (op git-merge-branch r "conflict" "release" :strategy "fast-forward-only"))
(both "merge of a missing branch fails" (r) (op git-merge-branch r "nonesuch" "main"))
(same "the copies agree after the writes" (ref-state *cli-copy*) (ref-state *ingot-copy*))
(same "main's merge commit: subject, author, parents"
      (git *cli-copy* "log" "-1" "--format=%s|%an|%ae|%P" "main") (git *ingot-copy* "log" "-1" "--format=%s|%an|%ae|%P" "main"))
(same "release's squash: one parent, side's tree changes"
      (list (git *cli-copy* "log" "--format=%s|%P" "release") (git *cli-copy* "ls-tree" "-r" "--name-only" "release"))
      (list (git *ingot-copy* "log" "--format=%s|%P" "release") (git *ingot-copy* "ls-tree" "-r" "--name-only" "release")))
(same "ff moved to feature" (git *cli-copy* "rev-parse" "ff") (git *ingot-copy* "rev-parse" "ff"))
;; last, since it leaves HEAD unborn: in a bare repository `git branch -D`
;; deletes the branch HEAD names, and so does Ingot
(both "delete-branch of HEAD's branch, as git allows in a bare repository" (r) (op git-delete-branch r "main"))
(same "both copies have no main now" (git *cli-copy* "for-each-ref" "refs/heads/main") (git *ingot-copy* "for-each-ref" "refs/heads/main"))

;;; --- The verdict ---------------------------------------------------------

(format t "~&~d checks, ~d failed~%" *checks* (length *failures*))
(when *failures*
  (format t "~&failed:~%~{  ~a~%~}" (reverse *failures*)))
(uiop:delete-directory-tree *scratch* :validate t :if-does-not-exist :ignore)
(uiop:quit (if *failures* 1 0))
