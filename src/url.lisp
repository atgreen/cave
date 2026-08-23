(in-package #:cave)

;;;; Internal URL construction helpers.
;;;;
;;;; One place to build in-app links so path shape and query-string encoding
;;;; stay consistent across views, notifications, and API responses. Before
;;;; this existed, every call site hand-rolled (format nil "/~A/~A...") and a
;;;; few forgot to url-encode the ?path= value (broken on spaces/#/?).

(defun repo-url (owner repo &rest segments)
  "Site path for a repo: (repo-url \"o\" \"r\" \"settings\") => \"/o/r/settings\".
SEGMENTS are inserted verbatim; use TREE-URL / BLOB-URL for ?path= links."
  (format nil "/~A/~A~{/~A~}" owner repo segments))

(defun issue-url (owner repo number)
  "Permalink to issue NUMBER in OWNER/REPO."
  (format nil "/~A/~A/issues/~A" owner repo number))

(defun pr-url (owner repo number)
  "Permalink to pull request NUMBER in OWNER/REPO."
  (format nil "/~A/~A/pulls/~A" owner repo number))

(defun tree-url (owner repo ref path)
  "Directory-listing URL. REF and PATH are url-encoded so branch names with
slashes and paths with spaces/# survive the round-trip."
  (format nil "/~A/~A/tree/~A?path=~A"
          owner repo (hunchentoot:url-encode ref) (hunchentoot:url-encode path)))

(defun blob-url (owner repo ref path)
  "File-view URL. REF and PATH are url-encoded (see TREE-URL)."
  (format nil "/~A/~A/blob/~A?path=~A"
          owner repo (hunchentoot:url-encode ref) (hunchentoot:url-encode path)))
