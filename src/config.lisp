;;; config.lisp — S-expression config parser for cave.conf
;;;
;;; SPDX-License-Identifier: MIT

(in-package #:cave)

(defvar *config* nil
  "The current Cave configuration, as a plist.")

(defvar *config-path* nil
  "Path to the loaded config file.")

(defun default-config ()
  "Return a plist of default configuration values."
  '(:http-port 8080
    :ssh-port 2222
    :data-dir "/var/lib/cave"
    :db-host "localhost"
    :db-port 5432
    :db-name "cave"
    :db-user "cave"
    :db-password ""
    :secret-key nil
    :smtp-host nil
    :smtp-port 587
    :smtp-user nil
    :smtp-password nil
    :smtp-from nil
    :base-url "http://localhost:8080"
    :log-level :info))

(defun load-config (path)
  "Load cave.conf from PATH. Returns the merged config plist."
  (setf *config-path* (pathname path))
  (let ((user-config (if (probe-file path)
                         (with-open-file (s path :direction :input)
                           (read s))
                         nil))
        (defaults (default-config)))
    ;; User config is a plist that overrides defaults
    (setf *config* (merge-plists defaults user-config))
    ;; Generate a secret key if none provided
    (unless (config-value :secret-key)
      (llog:warn "No :secret-key in config — generating random key. Set :secret-key in config." :config-path path)
      (setf (getf *config* :secret-key)
            (ironclad:byte-array-to-hex-string (ironclad:random-data 32))))
    *config*))

(defun config-value (key &optional default)
  "Get a config value by keyword KEY."
  (getf *config* key default))

(defun merge-plists (defaults overrides)
  "Merge OVERRIDES plist onto DEFAULTS plist."
  (let ((result (copy-list defaults)))
    (loop for (key value) on overrides by #'cddr
          do (setf (getf result key) value))
    result))

(defun data-dir (&rest subdirs)
  "Return a pathname under the configured data directory, joined with SUBDIRS."
  (let ((base (uiop:ensure-directory-pathname (config-value :data-dir))))
    (if subdirs
        (reduce (lambda (dir sub)
                  (uiop:ensure-directory-pathname
                   (merge-pathnames sub dir)))
                subdirs
                :initial-value base)
        base)))

(defun base-hostname ()
  "Extract just the hostname from :base-url (strip scheme and port)."
  (let* ((url (config-value :base-url "localhost"))
         ;; Strip scheme
         (no-scheme (if (search "://" url)
                        (subseq url (+ 3 (search "://" url)))
                        url))
         ;; Strip port
         (colon-pos (position #\: no-scheme)))
    (if colon-pos
        (subseq no-scheme 0 colon-pos)
        no-scheme)))

(defun repos-dir ()
  "Return the directory where bare git repos are stored."
  (data-dir "repos"))

(defun ensure-data-dirs ()
  "Create the data directory structure if it doesn't exist."
  (handler-case
      (progn
        (ensure-directories-exist (data-dir))
        (ensure-directories-exist (repos-dir))
        (ensure-directories-exist (data-dir "tmp")))
    (error (e)
      (format *error-output*
              "~&Cannot create data directory ~A~%  ~A~%~
               Set :data-dir in your cave.conf to a writable path, e.g.:~%  ~
               :data-dir \"./data\"~%"
              (config-value :data-dir) e)
      (uiop:quit 1))))
