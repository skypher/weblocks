(in-package :weblocks-test)

(defun remove-import-urls (urls)
  (loop for url in urls
       if (not (cl-ppcre:scan "-import\\.(?:\\d\\d*?\\.|)css$" url))
       collect url))

(defun make-versioned-regex (name type)
  "Used for checking potential local dependency path."
  (let ((dir (cond ((string= type "css") "stylesheets")
		   ((string= type "js") "scripts")))
	(pub-dir "pub"))
    (format nil "^/~A/~A/(?:vzn/~A\\.\\d\\d*?|~A)\\.~A$" pub-dir dir name name type)))

(deftestsuite versioning-suite (weblocks-suite print-upcase-suite)
  ())

(defparameter *temp-mod-record-folder* (princ-to-string (compute-public-files-path "weblocks" "pub/mod-record/test/temp-vzn")))
(defparameter *temp-version-folder* (princ-to-string (compute-public-files-path "weblocks" "pub/test/temp-vzn")))
(defparameter *temp-version-file* (concatenate 'string *temp-version-folder* "temp.test"))

(addtest versioning-test-1
  (cl-fad:delete-directory-and-files *temp-version-folder* :if-does-not-exist :ignore)
  (cl-fad:delete-directory-and-files *temp-mod-record-folder* :if-does-not-exist :ignore)
  (weblocks::with-file-write (stream *temp-version-file*)
      (write-string "test-text" stream))
  ;; version file initialization
  (ensure-same (weblocks::update-versioned-dependency-path *temp-version-file* "/www/temp.test")
	       (values (pathname (cl-ppcre:regex-replace "temp.test$" *temp-version-file* "vzn/temp.0.test"))
		       (pathname "/www/vzn/temp.0.test")))

  (sleep 1) ;; so that modified time of temp.test will change
  (weblocks::with-file-write (stream *temp-version-file*)
      (write-string "new-test-text" stream))
  (ensure-same (weblocks::update-versioned-dependency-path *temp-version-file* "/www/temp.test")
	       (values (pathname (cl-ppcre:regex-replace "temp.test$" *temp-version-file* "vzn/temp.1.test"))
		       (pathname "/www/vzn/temp.1.test")))

  ;; import rule versioning
  (let ((import-path (concatenate 'string *temp-version-folder* "import.css")))
    (weblocks::with-file-write (stream import-path)
      (write-string "@import url(/pub/stylesheets/table.css);
@import url(/pub/stylesheets/form.css);" stream))
    (weblocks::update-import-css-content import-path :version-types '(:stylesheet) :gzip-types nil)
    (ensure-same "^\\n@import url\\(/pub/stylesheets/vzn/table\\.\\d\\d*?\\.css\\);\\n@import url\\(/pub/stylesheets/vzn/form\\.\\d\\d*?\\.css\\);$"
		 (weblocks::slurp-file import-path)
		 :test #'cl-ppcre:scan)))

(addtest gzipping-test-1
  (cl-fad:delete-directory-and-files *temp-version-folder* :if-does-not-exist :ignore)
  (weblocks::with-file-write (stream *temp-version-file*)
    (write-string (make-sequence 'string 1000 :initial-element #\x) stream))
  (weblocks::create-gziped-dependency-file *temp-version-file*)
  (ensure (cl-fad:file-exists-p (concatenate 'string *temp-version-file* ".gz"))))
