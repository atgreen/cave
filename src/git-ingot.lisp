;;; git-ingot.lisp — Git operations in-process, through Ingot
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; The Ingot backend for the operations of git.lisp. Ingot
;;; (https://gitlab.common-lisp.net/clo/ingot) is Git in Common Lisp: it reads
;;; and writes a repository's objects, packs and refs itself, so every function
;;; here answers without a git subprocess. There is no Landlock sandbox around
;;; these reads because there is no process to confine: the repository's bytes
;;; are parsed in the server image, by Lisp code that signals rather than
;;; crashes on a stranger's object.
;;;
;;; This file is compiled only when the :cave-ingot feature is set while
;;; cave.asd is read (make INGOT=1) and used only when cave.conf says
;;; (:git-backend :ingot). Each %INGOT-<name> is the Ingot form of git.lisp's
;;; <name>, which DEFINE-GIT-OPERATION dispatches to: the same arguments, the
;;; same answers (plists, strings, octets, NIL on failure where the CLI
;;; function answers NIL), so the rest of Cave never knows which backend
;;; answered. An operation with no Ingot form here — blame, range-diff, the
;;; signature verifications, every operation that reaches a remote — keeps
;;; going through the git binary whatever the backend.
;;;
;;; Where the two backends are known to answer differently:
;;;   - diffs: no rename detection (a rename is a delete and an add), and no
;;;     function context after a hunk's @@ header;
;;;   - `git log -- path` simplifies merge history; Ingot compares each commit
;;;     with its first parent only;
;;;   - a merge base is one best common ancestor; on a criss-cross history the
;;;     two may pick different ones;
;;;   - --stat graphs are not scaled to a terminal width.

(in-package #:cave)

;;; ------------------------------------------------------------------------
;;; Plumbing shared by the operations

(defmacro with-ingot-repository ((var repo-path) &body body)
  "Bind VAR to the Ingot repository at REPO-PATH and evaluate BODY. Answers NIL
when there is no repository there or Ingot signals a git-error (an object or
ref that is not there, a conflict), matching the NIL-on-failure contract of
the CLI functions."
  `(handler-case (let ((,var (ingot:open-repository ,repo-path))) ,@body)
     (ingot:git-error () nil)
     (file-error () nil)))

(defun %ingot-commit (repo revision)
  "The commit structure REVISION names in REPO (peeled through tags), or NIL."
  (let ((oid (ingot:resolve-commit repo revision)))
    (and oid (ingot:read-commit repo oid))))

(defun %ingot-tree-of (repo commit-oid)
  (ingot:commit-tree (ingot:read-commit repo commit-oid)))

(defun %ingot-short (oid)
  "The abbreviation git prints for OID (%h, `index` lines): seven characters."
  (if (and (stringp oid) (>= (length oid) 7)) (subseq oid 0 7) oid))

(defun %ingot-stripln (text)
  "TEXT without its last newline (CR LF or LF), as uiop's :stripped output
option leaves the CLI functions' answers; NIL stays NIL."
  (when text
    (let ((n (length text)))
      (cond ((and (>= n 2) (char= (char text (- n 1)) #\Newline) (char= (char text (- n 2)) #\Return))
             (subseq text 0 (- n 2)))
            ((and (>= n 1) (char= (char text (- n 1)) #\Newline))
             (subseq text 0 (- n 1)))
            (t text)))))

(defun %ingot-six-digit-mode (mode)
  "A tree entry's mode as diffs print it: six digits (\"40000\" -> \"040000\")."
  (if (and mode (< (length mode) 6))
      (concatenate 'string (make-string (- 6 (length mode)) :initial-element #\0) mode)
      mode))

(defun %ingot-zone-offset (zone)
  "The seconds east of UTC that ZONE (\"+0130\", \"-0400\") stands for; 0 when
ZONE is malformed."
  (or (ignore-errors
       (let ((sign (if (char= (char zone 0) #\-) -1 1))
             (hours (parse-integer zone :start 1 :end 3))
             (minutes (parse-integer zone :start 3 :end 5)))
         (* sign (+ (* hours 3600) (* minutes 60)))))
      0))

(defun %ingot-local-zone ()
  "The server's time zone as git writes it in an identity (\"+0000\")."
  (multiple-value-bind (s m h d mo y dow dst zone) (decode-universal-time (get-universal-time))
    (declare (ignore s m h d mo y dow))
    (let* ((east-minutes (round (* 60 (- (if dst 1 0) zone))))
           (sign (if (minusp east-minutes) "-" "+"))
           (abs-minutes (abs east-minutes)))
      (format nil "~a~2,'0d~2,'0d" sign (floor abs-minutes 60) (mod abs-minutes 60)))))

(defun %ingot-decoded-time (ident)
  "IDENT's time decoded in IDENT's own zone: (values second minute hour day
month year day-of-week), with Monday 0 as DECODE-UNIVERSAL-TIME counts."
  (let ((universal (+ (ingot:ident-time ident) 2208988800 (%ingot-zone-offset (ingot:ident-zone ident)))))
    (decode-universal-time universal 0)))

(defun %ingot-iso-date (ident)
  "IDENT's date as git's %ai prints it: 2026-10-27 14:03:22 -0400."
  (multiple-value-bind (s mi h d mo y) (%ingot-decoded-time ident)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d ~a" y mo d h mi s (ingot:ident-zone ident))))

(defun %ingot-rfc2822-date (ident)
  "IDENT's date as an email Date header: Tue, 27 Oct 2026 14:03:22 -0400."
  (multiple-value-bind (s mi h d mo y dow) (%ingot-decoded-time ident)
    (format nil "~a, ~d ~a ~4,'0d ~2,'0d:~2,'0d:~2,'0d ~a"
            (nth dow '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
            d (nth (1- mo) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
            y h mi s (ingot:ident-zone ident))))

(defun %ingot-subject-and-body (message)
  "MESSAGE split as git's %s and %b do: the subject is the first paragraph with
its lines joined by spaces, the body everything after the blank line that
ends it (trimmed)."
  (let* ((lines (ingot:split-lines message))
         (blank? (lambda (line) (zerop (length (string-trim '(#\Space #\Tab #\Return) line)))))
         (start (position-if-not blank? lines)))
    (if (null start)
        (values "" "")
        (let* ((rest (nthcdr start lines))
               (end (or (position-if blank? rest) (length rest)))
               (subject (format nil "~{~a~^ ~}"
                                (mapcar (lambda (line) (string-trim '(#\Space #\Tab #\Return) line))
                                        (subseq rest 0 end))))
               (body (format nil "~{~a~%~}" (nthcdr (1+ end) rest))))
          (values subject (string-trim '(#\Newline #\Space #\Return) body))))))

(defun %ingot-log-plist (commit)
  "A commit as git-log and git-tree-last-commits describe one."
  (list :hash (ingot:commit-oid commit)
        :short-hash (%ingot-short (ingot:commit-oid commit))
        :author (ingot:commit-author-name commit)
        :date (%ingot-iso-date (ingot:commit-author commit))
        :subject (values (%ingot-subject-and-body (ingot:commit-message commit)))))

(defun %ingot-reachable (repo starts)
  "A hash table of the ids of every commit reachable from the commit ids STARTS."
  (let ((seen (make-hash-table :test 'equal))
        (stack (copy-list starts)))
    (loop while stack
          do (let ((oid (pop stack)))
               (unless (gethash oid seen)
                 (setf (gethash oid seen) t)
                 (let ((commit (ignore-errors (ingot:read-commit repo oid))))
                   (when commit
                     (dolist (parent (ingot:commit-parents commit)) (push parent stack)))))))
    seen))

(defun %ingot-walk-newest-first (repo start function)
  "Call FUNCTION with each commit reachable from the commit id START, newest
committer date first — the order `git log` prints, down to the ties: the
parents join the back of the queue and the sort is stable, so commits of one
second come out first parent first — until FUNCTION answers :stop."
  (let ((seen (make-hash-table :test 'equal))
        (frontier (list (ingot:read-commit repo start))))
    (setf (gethash start seen) t)
    (loop while frontier
          do (setf frontier (stable-sort frontier #'> :key #'ingot:commit-committer-time))
             (let ((commit (pop frontier)))
               (dolist (parent (ingot:commit-parents commit))
                 (unless (gethash parent seen)
                   (setf (gethash parent seen) t)
                   (let ((object (ignore-errors (ingot:read-commit repo parent))))
                     (when object (setf frontier (append frontier (list object)))))))
               (when (eq (funcall function commit) :stop) (return))))))

(defun %ingot-object-size (repo oid)
  "The size in bytes of the object OID, or NIL when it is not there."
  (let ((object (ingot:find-object repo oid)))
    (and object (ingot:object-size object))))

(defun %ingot-identity (author)
  "An Ingot identity for AUTHOR (\"Name <email>\", or NIL for the cave-bot
fallback parse-git-author supplies), timed now in the server's zone."
  (multiple-value-bind (name email) (parse-git-author author)
    (ingot:make-identity name email :zone (%ingot-local-zone))))

;;; ------------------------------------------------------------------------
;;; Refs

(defun %ingot-git-branches (repo-path)
  (with-ingot-repository (repo repo-path)
    (mapcar #'car (ingot:branches repo))))

(defun %ingot-tag-creator-time (repo oid)
  "When the tag at OID was made: the tagger's time for an annotated tag, the
committer's for a lightweight one (git's creatordate)."
  (let ((object (ingot:find-object repo oid)))
    (if (null object)
        0
        (case (ingot:object-type object)
          (:tag (let ((tagger (ingot:tag-tagger (ingot:parse-tag (ingot:object-data object) oid))))
                  (if tagger (ingot:ident-time tagger) 0)))
          (:commit (ingot:commit-committer-time (ingot:parse-commit (ingot:object-data object) oid)))
          (t 0)))))

(defun %ingot-git-tags (repo-path)
  (with-ingot-repository (repo repo-path)
    (let ((tags (ingot:tags repo)))
      (mapcar #'car (stable-sort (copy-list tags) #'>
                                 :key (lambda (tag) (%ingot-tag-creator-time repo (cdr tag))))))))

(defun %ingot-git-default-branch (repo-path)
  (with-ingot-repository (repo repo-path)
    (ingot:default-branch repo)))

(defun %ingot-git-repo-empty-p (repo-path)
  (let ((head (with-ingot-repository (repo repo-path) (ingot:head-oid repo))))
    (null head)))

(defun %ingot-git-rev-parse (repo-path ref)
  (with-ingot-repository (repo repo-path)
    (ingot:resolve-commit repo ref)))

(defun %ingot-git-has-commit-p (repo-path commit)
  (and (%ingot-git-rev-parse repo-path commit) t))

(defun %ingot-git-is-ancestor-p (repo-path ancestor descendant)
  (let ((answer (with-ingot-repository (repo repo-path)
                  (let ((a (ingot:resolve-commit repo ancestor))
                        (d (ingot:resolve-commit repo descendant)))
                    (cond ((not (and a d)) :unknown)
                          ((ingot:ancestor? repo a d) :yes)
                          (t :no))))))
    (case answer (:yes t) (:no nil) (t :unknown))))

(defun %ingot-git-commit-ancestor-p (repo-path commit branch)
  (and (stringp commit) (plusp (length (string-trim " " commit)))
       (eq t (%ingot-git-is-ancestor-p repo-path (string-trim " " commit) branch))))

(defun %ingot-git-merge-base (repo-path a b)
  (with-ingot-repository (repo repo-path)
    (let ((a (ingot:resolve-commit repo a))
          (b (ingot:resolve-commit repo b)))
      (and a b (ingot:merge-base repo a b)))))

(defun %ingot-git-commit-count (repo-path &key (branch nil))
  (with-ingot-repository (repo repo-path)
    (if branch
        (let ((start (ingot:resolve-commit repo branch)))
          (and start (ingot:count-commits repo start)))
        (let ((tips (remove nil (mapcar (lambda (ref) (ingot:peel-to-commit repo (cdr ref)))
                                        (ingot:refs repo)))))
          (hash-table-count (%ingot-reachable repo tips))))))

(defun %ingot-git-tag-exists-p (repo-path tag-name)
  (and (with-ingot-repository (repo repo-path)
         (ingot:ref-value* repo (format nil "refs/tags/~A" tag-name)))
       t))

(defun %ingot-git-create-tag (repo-path tag-name target &key message)
  (and (with-ingot-repository (repo repo-path)
         (let* ((ref (format nil "refs/tags/~A" tag-name))
                ;; `git tag` points at what TARGET names, unpeeled: a branch's
                ;; commit, or an annotated tag's own object.
                (oid (ingot:resolve repo target)))
           (when (and oid (ingot:valid-ref-name? ref) (null (ingot:ref-value* repo ref)))
             (if message
                 (let* ((object (ingot:read-object repo oid))
                        (text (string-right-trim '(#\Newline #\Space) message))
                        (tag (format nil "object ~a~%type ~(~a~)~%tag ~a~%tagger ~a~%~%~a~%"
                                     oid (ingot:object-type object) tag-name
                                     (ingot:identity-string (%ingot-identity nil)) text)))
                   (ingot:update-ref! repo ref (ingot:write-object repo :tag (ingot:string-to-octets tag)) :old nil))
                 (ingot:update-ref! repo ref oid :old nil))
             t)))
       t))

(defun %ingot-git-delete-branch (repo-path branch)
  ;; As `git branch -D` in a bare repository: the branch HEAD names goes too
  ;; (there is no work tree to protect), leaving HEAD unborn; the callers guard
  ;; the default branch themselves.
  (and (with-ingot-repository (repo repo-path)
         (let ((ref (format nil "refs/heads/~A" branch)))
           (when (ingot:ref-value* repo ref)
             (ingot:delete-ref! repo ref)
             t)))
       t))

(defun %ingot-git-create-branch (repo-path new-branch from-ref)
  (handler-case
      (let* ((repo (ingot:open-repository repo-path))
             (ref (format nil "refs/heads/~A" new-branch))
             (from (ingot:resolve-commit repo from-ref)))
        (cond ((not (ingot:valid-ref-name? ref))
               (values nil (format nil "fatal: '~A' is not a valid branch name" new-branch)))
              ((ingot:ref-value* repo ref)
               (values nil (format nil "fatal: a branch named '~A' already exists" new-branch)))
              ((null from)
               (values nil (format nil "fatal: not a valid object name: '~A'" from-ref)))
              (t (ingot:update-ref! repo ref from :old nil)
                 (values t ""))))
    (ingot:git-error (e) (values nil (princ-to-string e)))
    (file-error (e) (values nil (princ-to-string e)))))

;;; ------------------------------------------------------------------------
;;; History

(defun %ingot-git-log (repo-path &key (branch nil) (limit 20) path)
  (with-ingot-repository (repo repo-path)
    (let ((start (ingot:resolve-commit repo (or branch "HEAD"))))
      (when start
        (mapcar #'%ingot-log-plist
                (ingot:log-commits repo start :limit limit
                                              :path (and path (plusp (length path)) path)))))))

(defun %ingot-git-show-commit (repo-path hash)
  (with-ingot-repository (repo repo-path)
    (let ((commit (%ingot-commit repo hash)))
      (when commit
        (multiple-value-bind (subject body) (%ingot-subject-and-body (ingot:commit-message commit))
          (list :hash (ingot:commit-oid commit)
                :short-hash (%ingot-short (ingot:commit-oid commit))
                :author (ingot:commit-author-name commit)
                :author-email (ingot:commit-author-email commit)
                :date (%ingot-iso-date (ingot:commit-author commit))
                :subject subject
                :body body))))))

(defun %ingot-git-commit-signature-info (repo-path commit-sha)
  (let ((scheme
          (with-ingot-repository (repo repo-path)
            (let* ((oid (ingot:resolve-commit repo commit-sha))
                   (object (and oid (ingot:find-object repo oid))))
              (when (and object (eq (ingot:object-type object) :commit))
                ;; the headers end at the first blank line; a signature block
                ;; quoted in the message is not a signature
                (let* ((text (ingot:octets-to-string (ingot:object-data object)))
                       (sep (search (format nil "~%~%") text))
                       (headers (if sep (subseq text 0 sep) text)))
                  (cond ((search "BEGIN SSH SIGNATURE" headers) :ssh)
                        ((search "BEGIN PGP SIGNATURE" headers) :gpg)
                        (t :none))))))))
    (if (member scheme '(:ssh :gpg)) (values t scheme) (values nil nil))))

(defun %ingot-trailer-line-p (line)
  "Whether LINE is shaped like a trailer: a token of letters, digits and
dashes, a colon, a value; or git's own `(cherry picked from commit ...)`."
  (or (uiop:string-prefix-p "(cherry picked from commit " line)
      (let ((colon (position #\: line)))
        (and colon (plusp colon)
             (every (lambda (c) (or (alphanumericp c) (char= c #\-)))
                    (string-right-trim " " (subseq line 0 colon)))))))

(defun %ingot-trailers (message)
  "The trailer block of MESSAGE as git's interpret-trailers finds it: the last
paragraph, after a blank line, when every line of it is a trailer or a
continuation — or when it holds a Signed-off-by or cherry-pick line and at
least a quarter of its lines are trailers. Answers the trailer lines."
  (let* ((lines (ingot:split-lines (string-right-trim '(#\Newline #\Space) message)))
         (blank? (lambda (line) (zerop (length (string-trim '(#\Space #\Tab #\Return) line)))))
         (blank (position-if blank? lines :from-end t)))
    (when (and blank (< (1+ blank) (length lines)))
      (let ((block (subseq lines (1+ blank)))
            (trailers 0) (others 0) (recognized nil))
        (dolist (line block)
          (cond ((%ingot-trailer-line-p line)
                 (incf trailers)
                 (when (or (uiop:string-prefix-p "Signed-off-by:" line)
                           (uiop:string-prefix-p "(cherry picked from commit " line))
                   (setf recognized t)))
                ((member (char line 0) '(#\Space #\Tab)))   ; a continuation of the line before
                (t (incf others))))
        (when (or (and (plusp trailers) (zerop others))
                  (and recognized (>= (* 3 trailers) others)))
          (remove-if-not #'%ingot-trailer-line-p block))))))

(defun %ingot-git-commit-trailers (repo-path commit-sha)
  (with-ingot-repository (repo repo-path)
    (let ((commit (%ingot-commit repo commit-sha)))
      (when commit
        (let ((result nil))
          (dolist (line (%ingot-trailers (ingot:commit-message commit)) (nreverse result))
            (let ((colon (position #\: line)))
              (when (and colon (plusp colon))
                (push (cons (string-trim " " (subseq line 0 colon))
                            (string-trim " " (subseq line (1+ colon))))
                      result)))))))))

(defun %ingot-nearest-tag (repo commit-oid)
  "The tag `git describe --tags --abbrev=0` would name for COMMIT-OID: the
closest tag reachable from it (the commit itself included); among tags at
the same distance, the newest."
  (let ((tags-at (make-hash-table :test 'equal)))
    (loop for (name . oid) in (ingot:tags repo)
          do (let ((commit (ingot:peel-to-commit repo oid)))
               (when commit (push (cons name oid) (gethash commit tags-at)))))
    (when (plusp (hash-table-count tags-at))
      (let ((seen (make-hash-table :test 'equal))
            (frontier (list commit-oid)))
        (setf (gethash commit-oid seen) t)
        (loop while frontier
              do (let ((found (loop for c in frontier append (gethash c tags-at))))
                   (when found
                     (return (car (first (sort found #'> :key (lambda (tag) (%ingot-tag-creator-time repo (cdr tag)))))))))
                 (setf frontier
                       (loop for c in frontier
                             append (loop for parent in (ingot:commit-parents (ingot:read-commit repo c))
                                          unless (gethash parent seen)
                                            do (setf (gethash parent seen) t)
                                            and collect parent))))))))

(defun %ingot-git-release-notes (repo-path tag)
  (with-ingot-repository (repo repo-path)
    (let ((tip (ingot:resolve-commit repo tag)))
      (when tip
        (let* ((first-parent (first (ingot:commit-parents (ingot:read-commit repo tip))))
               (prev (and first-parent (%ingot-nearest-tag repo first-parent)))
               (excluded (if prev
                             (%ingot-reachable repo (list (ingot:resolve-commit repo prev)))
                             (make-hash-table :test 'equal)))
               (lines nil))
          (%ingot-walk-newest-first
           repo tip
           (lambda (commit)
             (unless (or (gethash (ingot:commit-oid commit) excluded)
                         (> (length (ingot:commit-parents commit)) 1))   ; --no-merges
               (push (format nil "- ~a (~a)"
                             (values (%ingot-subject-and-body (ingot:commit-message commit)))
                             (%ingot-short (ingot:commit-oid commit)))
                     lines))))
          (when lines
            (let ((body (format nil "~{~a~^~%~}" (nreverse lines))))
              (if prev
                  (format nil "### Changes since ~A~%~%~A~%" prev body)
                  (format nil "### Changes~%~%~A~%" body)))))))))

;;; ------------------------------------------------------------------------
;;; Trees and blobs

(defun %ingot-entry-type (entry)
  "The type column of ls-tree for ENTRY: tree, blob, or commit for a submodule."
  (case (ingot:tree-entry-kind entry)
    (:tree "tree")
    (:submodule "commit")
    (t "blob")))

(defun %ingot-git-list-files (repo-path ref &key (limit 20000))
  (with-ingot-repository (repo repo-path)
    (let ((commit (ingot:resolve-commit repo ref)))
      (when commit
        (let ((files (mapcar #'car (ingot:tree-paths repo (%ingot-tree-of repo commit)))))
          (if (> (length files) limit) (subseq files 0 limit) files))))))

(defun %ingot-git-tree (repo-path &key (ref "HEAD") (path ""))
  (with-ingot-repository (repo repo-path)
    (let ((commit (ingot:resolve-commit repo ref)))
      (when commit
        (let ((entries (ingot:tree-at-path repo commit (or path ""))))
          (when entries
            (let ((rows (mapcar (lambda (entry)
                                  (let ((kind (ingot:tree-entry-kind entry)))
                                    (list :mode (%ingot-six-digit-mode (ingot:tree-entry-mode entry))
                                          :type (%ingot-entry-type entry)
                                          :hash (ingot:tree-entry-oid entry)
                                          :size (and (member kind '(:blob :executable :symlink))
                                                     (%ingot-object-size repo (ingot:tree-entry-oid entry)))
                                          :name (ingot:tree-entry-name entry))))
                                entries)))
              ;; directories first, then alphabetical — as git-tree sorts
              (sort rows (lambda (a b)
                           (let ((a-dir (equal (getf a :type) "tree"))
                                 (b-dir (equal (getf b :type) "tree")))
                             (if (eq a-dir b-dir)
                                 (string< (getf a :name) (getf b :name))
                                 a-dir)))))))))))

(defun %ingot-git-tree-last-commits (repo-path ref dir entry-names)
  (let ((result (make-hash-table :test 'equal)))
    (when (null entry-names) (return-from %ingot-git-tree-last-commits result))
    (with-ingot-repository (repo repo-path)
      (let* ((start (ingot:resolve-commit repo ref))
             (prefix (if (or (null dir) (string= dir "")) "" (concatenate 'string dir "/")))
             (remaining (make-hash-table :test 'equal)))
        (dolist (n entry-names) (setf (gethash n remaining) t))
        (when start
          (%ingot-walk-newest-first
           repo start
           (lambda (commit)
             ;; `git log --name-only` names no files for a merge commit, so a
             ;; merge never claims an entry
             (when (< (length (ingot:commit-parents commit)) 2)
               (let ((plist (list :hash (ingot:commit-oid commit)
                                  :short-hash (%ingot-short (ingot:commit-oid commit))
                                  :author (ingot:commit-author-name commit)
                                  :subject (values (%ingot-subject-and-body (ingot:commit-message commit)))
                                  :time (+ (ingot:commit-committer-time commit) 2208988800))))
                 (dolist (change (ingot:commit-diff repo (ingot:commit-oid commit)))
                   (let ((child (%tree-top-child (ingot:change-path change) prefix)))
                     (when (and child (gethash child remaining))
                       (setf (gethash child result) plist)
                       (remhash child remaining))))))
             (when (zerop (hash-table-count remaining)) :stop))))))
    result))

(defun %ingot-git-language-stats (repo-path ref)
  (with-ingot-repository (repo repo-path)
    (let ((commit (ingot:resolve-commit repo ref)))
      (when commit
        (let ((totals (make-hash-table :test 'equal)))   ; name -> (color . bytes)
          (loop for (path . entry) in (ingot:tree-paths repo (%ingot-tree-of repo commit))
                when (and (member (ingot:tree-entry-kind entry) '(:blob :executable :symlink))
                          (not (%vendored-or-generated-p path)))
                  do (multiple-value-bind (lang color) (file-language-info path)
                       (when lang
                         (let ((size (%ingot-object-size repo (ingot:tree-entry-oid entry))))
                           (when (and size (plusp size))
                             (let ((cur (gethash lang totals)))
                               (setf (gethash lang totals)
                                     (cons color (+ (if cur (cdr cur) 0) size)))))))))
          (let ((rows nil))
            (maphash (lambda (lang cc) (push (list lang (car cc) (cdr cc)) rows)) totals)
            (sort rows #'> :key #'third)))))))

(defun %ingot-git-blob-hash (repo-path ref path)
  (with-ingot-repository (repo repo-path)
    (let ((commit (ingot:resolve-commit repo ref)))
      (when commit
        (let ((entry (ingot:tree-entry-at repo (%ingot-tree-of repo commit) path)))
          (and entry (ingot:tree-entry-oid entry)))))))

(defun %ingot-git-blob-bytes (repo-path ref path)
  (with-ingot-repository (repo repo-path)
    (let ((commit (ingot:resolve-commit repo ref)))
      (when commit
        (values (ingot:blob-at repo commit path))))))

(defun %ingot-git-blob (repo-path ref path)
  ;; the CLI's :stripped output loses the file's last newline; so does this
  (let ((octets (%ingot-git-blob-bytes repo-path ref path)))
    (and octets (%ingot-stripln (ingot:octets-to-string octets)))))

(defun %ingot-git-blob-size (repo-path ref path)
  ;; `cat-file -s ref:path` sizes whatever object is at PATH, a tree included
  (with-ingot-repository (repo repo-path)
    (let ((commit (ingot:resolve-commit repo ref)))
      (when commit
        (let ((entry (ingot:tree-entry-at repo (%ingot-tree-of repo commit) path)))
          (and entry (%ingot-object-size repo (ingot:tree-entry-oid entry))))))))

;;; ------------------------------------------------------------------------
;;; Patches: the text git prints for a change, from Ingot's tree diff

(defun %ingot-lines-keeping-ends (text)
  "TEXT split into lines that keep their newline, so that a last line without
one differs from the same line with one, as it does for git."
  (let ((lines nil) (start 0) (n (length text)))
    (loop while (< start n)
          do (let ((eol (position #\Newline text :start start)))
               (push (subseq text start (if eol (1+ eol) n)) lines)
               (setf start (if eol (1+ eol) n))))
    (nreverse lines)))

(defun %ingot-hunks (old-text new-text &key (context 3))
  "The hunks of a unified diff of the two texts: a list of
(old-start old-count new-start new-count edits), each edit (op line) with op
:equal, :delete or :insert and the line keeping its newline when it had one."
  (let* ((edits (coerce (ingot:diff-lines (%ingot-lines-keeping-ends old-text)
                                          (%ingot-lines-keeping-ends new-text))
                        'simple-vector))
         (n (length edits))
         (old-at (make-array n)) (new-at (make-array n))
         (old 1) (new 1))
    (dotimes (i n)
      (setf (aref old-at i) old (aref new-at i) new)
      (ecase (first (svref edits i))
        (:equal (incf old) (incf new))
        (:delete (incf old))
        (:insert (incf new))))
    (let ((changes (loop for i below n unless (eq (first (svref edits i)) :equal) collect i))
          (hunks nil))
      (loop while changes
            do (let* ((first-change (pop changes))
                      (last-change first-change))
                 (loop while (and changes (<= (- (first changes) last-change) (* 2 context)))
                       do (setf last-change (pop changes)))
                 (let* ((start (max 0 (- first-change context)))
                        (end (min n (+ last-change context 1)))
                        (lines (loop for i from start below end collect (svref edits i))))
                   (push (list (aref old-at start)
                               (count-if (lambda (e) (member (first e) '(:equal :delete))) lines)
                               (aref new-at start)
                               (count-if (lambda (e) (member (first e) '(:equal :insert))) lines)
                               lines)
                         hunks))))
      (nreverse hunks))))

(defun %ingot-write-hunks (hunks out)
  (dolist (hunk hunks)
    (destructuring-bind (old-start old-count new-start new-count edits) hunk
      (flet ((range (start count)
               ;; git prints the line before the hunk when it holds no lines
               (cond ((= count 1) (format nil "~d" start))
                     ((zerop count) (format nil "~d,0" (1- start)))
                     (t (format nil "~d,~d" start count)))))
        (format out "@@ -~a +~a @@~%" (range old-start old-count) (range new-start new-count)))
      (dolist (edit edits)
        (let* ((line (second edit))
               (ends? (and (plusp (length line)) (char= (char line (1- (length line))) #\Newline))))
          (write-string (ecase (first edit) (:equal " ") (:delete "-") (:insert "+")) out)
          (write-string (if ends? (subseq line 0 (1- (length line))) line) out)
          (terpri out)
          (unless ends? (format out "\\ No newline at end of file~%")))))))

(defun %ingot-change-content (repo oid mode)
  "What a diff compares for the entry OID of MODE: the blob's text, :binary
for a blob that looks binary, or the pointer line of a submodule."
  (cond ((null oid) "")
        ((string= (%ingot-six-digit-mode mode) "160000")
         (format nil "Subproject commit ~a~%" oid))
        (t (let ((octets (ingot:read-blob repo oid)))
             (if (ingot:binary-octets? octets) :binary (ingot:octets-to-string octets))))))

(defun %ingot-change-patch (repo change &key full-binary-index)
  "The text `git diff` prints for CHANGE: the diff --git header, the mode and
index lines, and the hunks (or the one line that says two binaries differ).
FULL-BINARY-INDEX writes a binary change's index line with whole ids, as
format-patch does."
  (let* ((path (ingot:change-path change))
         (status (ingot:change-status change))
         (old-oid (ingot:change-old-oid change))
         (new-oid (ingot:change-new-oid change))
         (old-mode (%ingot-six-digit-mode (ingot:change-old-mode change)))
         (new-mode (%ingot-six-digit-mode (ingot:change-new-mode change))))
    (with-output-to-string (out)
      (format out "diff --git a/~a b/~a~%" path path)
      (ecase status
        (:added (format out "new file mode ~a~%" new-mode))
        (:deleted (format out "deleted file mode ~a~%" old-mode))
        ((:modified :type-changed :renamed)
         (unless (equal old-mode new-mode)
           (format out "old mode ~a~%new mode ~a~%" old-mode new-mode))))
      (unless (equal old-oid new-oid)
        (let* ((old (%ingot-change-content repo old-oid old-mode))
               (new (%ingot-change-content repo new-oid new-mode))
               (binary? (or (eq old :binary) (eq new :binary)))
               (whole? (and binary? full-binary-index))
               (old-name (if old-oid (format nil "a/~a" path) "/dev/null"))
               (new-name (if new-oid (format nil "b/~a" path) "/dev/null")))
          (flet ((id (oid) (let ((oid (or oid ingot:*zero-oid*))) (if whole? oid (%ingot-short oid)))))
            (format out "index ~a..~a~@[ ~a~]~%" (id old-oid) (id new-oid)
                    (and old-oid new-oid (equal old-mode new-mode) new-mode)))
          (if binary?
              (format out "Binary files ~a and ~a differ~%" old-name new-name)
              (let ((hunks (%ingot-hunks old new)))
                (when hunks
                  (format out "--- ~a~%+++ ~a~%" old-name new-name)
                  (%ingot-write-hunks hunks out)))))))))

(defun %ingot-change-counts (repo change)
  "Three values for CHANGE: lines added, lines deleted, and whether it is
binary (then the two counts are the sizes before and after)."
  (let* ((old-oid (ingot:change-old-oid change))
         (new-oid (ingot:change-new-oid change))
         (old (%ingot-change-content repo old-oid (ingot:change-old-mode change)))
         (new (%ingot-change-content repo new-oid (ingot:change-new-mode change))))
    (cond ((equal old-oid new-oid) (values 0 0 nil))
          ((or (eq old :binary) (eq new :binary))
           (values (if old-oid (%ingot-object-size repo old-oid) 0)
                   (if new-oid (%ingot-object-size repo new-oid) 0)
                   t))
          (t (let ((added 0) (deleted 0))
               (dolist (edit (ingot:diff-lines (%ingot-lines-keeping-ends old)
                                               (%ingot-lines-keeping-ends new)))
                 (case (first edit) (:insert (incf added)) (:delete (incf deleted))))
               (values added deleted nil))))))

(defun %ingot-stat (repo changes)
  "The --stat text for CHANGES: a line per file and the summary line, as git
prints them (the +/- graph is not scaled to a terminal width)."
  (let ((rows (mapcar (lambda (change)
                        (multiple-value-bind (added deleted binary?) (%ingot-change-counts repo change)
                          (list (ingot:change-path change) added deleted binary?)))
                      changes)))
    (with-output-to-string (out)
      (when rows
        (let ((name-width (reduce #'max rows :key (lambda (row) (length (first row)))))
              (count-width (reduce #'max rows
                                   :key (lambda (row) (if (fourth row) 3 (length (princ-to-string (+ (second row) (third row))))))))
              (files 0) (insertions 0) (deletions 0))
          (dolist (row rows)
            (destructuring-bind (path added deleted binary?) row
              (incf files)
              (if binary?
                  (format out " ~va | Bin ~d -> ~d bytes~%" name-width path added deleted)
                  (progn
                    (incf insertions added) (incf deletions deleted)
                    (format out " ~va | ~vd~@[ ~a~]~%" name-width path count-width (+ added deleted)
                            (and (plusp (+ added deleted))
                                 (concatenate 'string
                                              (make-string added :initial-element #\+)
                                              (make-string deleted :initial-element #\-))))))))
          (format out " ~d file~:p changed" files)
          (when (plusp insertions) (format out ", ~d insertion~:p(+)" insertions))
          (when (plusp deletions) (format out ", ~d deletion~:p(-)" deletions))
          (terpri out))))))

(defun %ingot-stat-summary (changes)
  "The --summary lines format-patch adds under the stat: files created and
deleted, and modes changed, in path order."
  (with-output-to-string (out)
    (dolist (change changes)
      (let ((old-mode (%ingot-six-digit-mode (ingot:change-old-mode change)))
            (new-mode (%ingot-six-digit-mode (ingot:change-new-mode change))))
        (case (ingot:change-status change)
          (:added (format out " create mode ~a ~a~%" new-mode (ingot:change-path change)))
          (:deleted (format out " delete mode ~a ~a~%" old-mode (ingot:change-path change)))
          (t (unless (equal old-mode new-mode)
               (format out " mode change ~a => ~a ~a~%" old-mode new-mode (ingot:change-path change)))))))))

(defun %ingot-commit-changes (repo hash)
  "Two values: the commit HASH names and the changes it makes against its
first parent — NIL for a merge commit, which `git diff-tree` leaves out."
  (let ((commit (%ingot-commit repo hash)))
    (when commit
      (values commit
              (and (< (length (ingot:commit-parents commit)) 2)
                   (ingot:commit-diff repo (ingot:commit-oid commit)))))))

(defun %ingot-git-commit-diff (repo-path hash)
  (with-ingot-repository (repo repo-path)
    (multiple-value-bind (commit changes) (%ingot-commit-changes repo hash)
      (when commit
        (if (> (length (ingot:commit-parents commit)) 1)
            ""
            ;; diff-tree prints the commit's id on a line of its own first
            (%ingot-stripln
             (with-output-to-string (out)
               (format out "~a~%" (ingot:commit-oid commit))
               (dolist (change changes) (write-string (%ingot-change-patch repo change) out)))))))))

(defun %ingot-git-commit-stat (repo-path hash)
  (with-ingot-repository (repo repo-path)
    (multiple-value-bind (commit changes) (%ingot-commit-changes repo hash)
      (when commit
        (if (> (length (ingot:commit-parents commit)) 1)
            ""
            (%ingot-stripln (format nil "~a~%~a" (ingot:commit-oid commit) (%ingot-stat repo changes))))))))

(defun %ingot-git-format-patch (repo-path hash)
  (with-ingot-repository (repo repo-path)
    (multiple-value-bind (commit changes) (%ingot-commit-changes repo hash)
      (when commit
        (if (> (length (ingot:commit-parents commit)) 1)
            ;; a merge has no patch of its own (`format-patch -1 <merge>` would
            ;; print the nearest non-merge ancestor's; this says so instead)
            ""
            (multiple-value-bind (subject body) (%ingot-subject-and-body (ingot:commit-message commit))
              (%ingot-stripln
               (with-output-to-string (out)
                 (format out "From ~a Mon Sep 17 00:00:00 2001~%" (ingot:commit-oid commit))
                 (format out "From: ~a <~a>~%" (ingot:commit-author-name commit) (ingot:commit-author-email commit))
                 (format out "Date: ~a~%" (%ingot-rfc2822-date (ingot:commit-author commit)))
                 (format out "Subject: [PATCH] ~a~%~%" subject)
                 (when (plusp (length body)) (format out "~a~%" body))
                 (format out "---~%~a~a~%" (%ingot-stat repo changes) (%ingot-stat-summary changes))
                 (dolist (change changes)
                   (write-string (%ingot-change-patch repo change :full-binary-index t) out))
                 (format out "-- ~%cave (ingot)~%~%")))))))))

(defun %ingot-git-diff-merge-base (repo-path target-ref source-ref)
  (with-ingot-repository (repo repo-path)
    (let ((target (ingot:resolve-commit repo target-ref))
          (source (ingot:resolve-commit repo source-ref)))
      (when (and target source)
        (let ((base (ingot:merge-base repo target source)))
          (when base
            (%ingot-stripln
             (with-output-to-string (out)
               (dolist (change (ingot:tree-diff repo (%ingot-tree-of repo base) (%ingot-tree-of repo source)))
                 (write-string (%ingot-change-patch repo change) out))))))))))

;;; ------------------------------------------------------------------------
;;; Merging and committing

(defun %ingot-git-merge-conflicts (repo-path target-branch source-branch)
  (let ((files (with-ingot-repository (repo repo-path)
                 (let ((target (ingot:resolve-commit repo target-branch))
                       (source (ingot:resolve-commit repo source-branch)))
                   (when (and target source)
                     (let* ((base (ingot:merge-base repo target source))
                            (base-tree (and base (%ingot-tree-of repo base))))
                       (multiple-value-bind (tree conflicts)
                           (ingot:merge-trees repo base-tree (%ingot-tree-of repo target) (%ingot-tree-of repo source))
                         (declare (ignore tree))
                         (if conflicts (sort (copy-list conflicts) #'string<) :clean))))))))
    (if (consp files) (values t files) (values nil nil))))

(defun %ingot-git-merge-branch (repo-path source-branch target-branch &key strategy author message)
  (let ((strategy (if (and (stringp strategy) (plusp (length strategy))) strategy "merge"))
        (message (let ((m (and (stringp message)
                               (string-trim '(#\Space #\Tab #\Newline #\Return) message))))
                   (and m (plusp (length m)) m)))
        (who (%ingot-identity author)))
    (and (with-ingot-repository (repo repo-path)
           (let* ((target-ref (format nil "refs/heads/~A" target-branch))
                  (source (ingot:ref-value* repo (format nil "refs/heads/~A" source-branch)))
                  (target (ingot:ref-value* repo target-ref)))
             (when (and source target)
               (cond
                 ((string= strategy "squash")
                  (let* ((base (ingot:merge-base repo source target))
                         (target-tree (%ingot-tree-of repo target)))
                    (multiple-value-bind (tree conflicts)
                        (ingot:merge-trees repo (and base (%ingot-tree-of repo base))
                                           target-tree (%ingot-tree-of repo source)
                                           :ours-label target-branch :theirs-label source-branch)
                      ;; a conflict, or nothing to commit, is a failure as it is for git
                      (when (and (null conflicts) tree (string/= tree target-tree))
                        (ingot:make-commit! repo :branch target-branch :tree tree :parents (list target)
                                                 :author who
                                                 :message (or message (format nil "Squash merge ~A into ~A"
                                                                              source-branch target-branch))
                                                 :expected-old target)
                        t))))
                 ((string= strategy "fast-forward-only")
                  (case (ingot:can-merge? repo source target)
                    (:already t)
                    (:fast-forward (ingot:update-ref! repo target-ref source :old target) t)
                    (t nil)))
                 (t
                  ;; "merge": a merge commit even where a fast-forward would do
                  (ingot:merge-branches! repo source-branch target-branch
                                         :author who :allow-fast-forward nil
                                         :message (or message (format nil "Merge ~A into ~A"
                                                                      source-branch target-branch)))
                  t)))))
         t)))

(defun %ingot-git-commit-file-on-branch (repo-path base-branch new-branch path content message
                                         &key (author-name "cave-bot") (author-email "cave-bot@localhost"))
  (with-ingot-repository (repo repo-path)
    (let ((new-ref (format nil "refs/heads/~A" new-branch)))
      ;; as `git worktree add -b`: the new branch must not exist, the base must
      (when (and (ingot:valid-ref-name? new-ref)
                 (null (ingot:ref-value* repo new-ref))
                 (ingot:ref-value* repo (format nil "refs/heads/~A" base-branch)))
        (ingot:make-commit! repo :branch new-branch :start-branch base-branch
                                 :author (ingot:make-identity author-name author-email :zone (%ingot-local-zone))
                                 :message message
                                 :actions (list (list :action :update :path path :content content)))))))

;;; ------------------------------------------------------------------------
;;; The smart HTTP protocol, read side

(defun %ingot-git-advertise-refs (repo-path)
  (with-ingot-repository (repo repo-path)
    (ingot:advertise-refs repo :upload-pack)))

(defun %ingot-git-upload-pack (repo-path request-body)
  (with-ingot-repository (repo repo-path)
    (ingot:upload-pack repo (if request-body
                                (coerce request-body '(simple-array (unsigned-byte 8) (*)))
                                (make-array 0 :element-type '(unsigned-byte 8))))))
