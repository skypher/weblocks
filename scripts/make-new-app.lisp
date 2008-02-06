
(in-package :weblocks-scripts)

(export '(make-application))

(defparameter *app-name-placeholder* (ppcre:quote-meta-chars "{APPNAME}")
  "A placeholder located in template files.")

(defun attributize-name (name)
  "Convert a string or a symbol to a format suitable for
serialization (in particular for markup languages like HTML).

Ex:
\(attributize-name 'hello-world) => \"hello-world\"
\(attributize-name \"Hello world-ref\") => \"hello-world-ref\""
  (when (null name)
    (return-from attributize-name ""))
  (let ((namestr (etypecase name
		     (symbol (symbol-name name))
		     (string name)
		     (integer (format nil "~A" name)))))
    (string-downcase (substitute #\- #\Space namestr))))
(defun make-dir (pathname)
  "Create directory 'pathname'.

Borrowed from cl-darcs with permission of copyright owner."
  (with-simple-restart (ignore-error "Ignore ~A directory creation error." pathname)
    (multiple-value-bind (path created) (ensure-directories-exist pathname)
      (declare (ignore path))
      (unless created
        (error "Directory ~A already exists." pathname)))))

(defun copy-directory (source target &key excluding)
  "Copy all files and directories in 'source' to 'target'.
'source' and 'target' are pathnames designating directories, both of
which must exist.  'Excluding' is a list of files and directories
to exclude.

Symlinks will confuse the function.

Borrowed from cl-darcs with permission of copyright owner."
  (let* ((wild (make-pathname :directory '(:relative :wild-inferiors)
                              :name :wild
                              :type :wild
                              :version :wild))
         (source-wild (merge-pathnames wild source))
         (target-wild (merge-pathnames wild target))
         (excluding-wild (mapcar
                          (lambda (excluded) (merge-pathnames wild excluded))
                          excluding))

         (files (fad:list-directory (truename source))))
    (dolist (source-file files)
      (let ((target-file (translate-pathname source-file source-wild target-wild)))
        (cond
          ((some (lambda (excluded) (pathname-match-p source-file excluded)) excluding-wild)
           ;; File excluded - do nothing.
           )
          ((fad:directory-pathname-p source-file)
           (make-dir target-file)
           (copy-directory source-file target-file :excluding excluding))
          (t
           (copy-file source-file target-file)))))))

(defun asdf-system-directory (system-name)
  "Returns a directory of the asdf system file of system
'system-name'."
  (make-pathname :directory
		 (pathname-directory (truename (asdf:system-definition-pathname
						(asdf:find-system system-name))))))

(defun copy-file-replace (source target &optional match replacement)
  "Copies 'source' to 'target' replacing all instances of 'match' with
'replacement' in the target."
  (let (text)
    (with-open-file (stream source)
      (setf text (make-string (file-length stream)))
      (read-sequence text stream))
    (setf text (ppcre:regex-replace-all match text replacement))
    (with-open-file (stream target :direction :output :if-does-not-exist :create :if-exists nil)
      (write-sequence text stream))))

(defun make-application (name &optional target)
  "Creates a directory 'name' under directory 'target' and fills it
with files that allow to easily get started with a new Weblocks
application.

If 'target' isn't specified, 'make-applicaiton' uses
*default-pathname-defaults*.

'name' cannot be NIL, as it's being used in numerous places in the
generated files to specify the name of the newly created
application. Note, 'name' is expected to be a symbol naming the
application."
  ; ensure name is a symbol
  (assert (symbolp name))
  ; if target is missing, set default
  (unless target
    (setf target *default-pathname-defaults*))
  ; create a directory 'name' under 'target'
  (let* ((new-project-dir (merge-pathnames
			   (make-pathname :directory `(:relative ,(attributize-name name)))
			   (truename (pathname-as-directory target))))
	 (new-project-src-dir (merge-pathnames
			       (make-pathname :directory '(:relative "src"))
			       new-project-dir))
	 (new-project-conf-dir (merge-pathnames
				(make-pathname :directory '(:relative "conf"))
				new-project-dir))
	 (new-project-data-dir (merge-pathnames
				(make-pathname :directory '(:relative "data"))
				new-project-dir)))
    ; create necessary directories
    (ensure-directories-exist new-project-dir :verbose t)
    (ensure-directories-exist new-project-src-dir :verbose t)
    (ensure-directories-exist new-project-conf-dir :verbose t)
    (ensure-directories-exist new-project-data-dir :verbose t)
    ; copy weblocks public files
    (copy-directory (pathname-as-file
		     (truename (merge-pathnames (make-pathname :directory '(:relative "pub"))
						(asdf-system-directory :weblocks))))
		    new-project-dir)
    ; copy init-session.lisp
    (copy-file-replace (merge-pathnames
			(make-pathname :directory '(:relative "scripts" "new-app-templates")
				       :name "init-session" :type "lisp")
			(asdf-system-directory :weblocks-scripts))
		       (merge-pathnames
			(make-pathname :name "init-session" :type "lisp")
			new-project-src-dir)
		       *app-name-placeholder*
		       (attributize-name name))
    ; copy {APPNAME}.asd
    (copy-file-replace (merge-pathnames
			(make-pathname :directory '(:relative "scripts" "new-app-templates")
				       :name "{APPNAME}" :type "asd")
			(asdf-system-directory :weblocks-scripts))
		       (merge-pathnames
			(make-pathname :name (attributize-name name) :type "asd")
			new-project-dir)
			*app-name-placeholder*
		       (attributize-name name))
    ; copy {APPNAME}.lisp
    (copy-file-replace (merge-pathnames
			(make-pathname :directory '(:relative "scripts" "new-app-templates")
				       :name "{APPNAME}" :type "lisp")
			(asdf-system-directory :weblocks-scripts))
		       (merge-pathnames
			(make-pathname :name (attributize-name name) :type "lisp")
			new-project-dir)
			*app-name-placeholder*
		       (attributize-name name))
    ; copy stores.lisp
    (copy-file-replace (merge-pathnames
			(make-pathname :directory '(:relative "scripts" "new-app-templates")
				       :name "stores" :type "lisp")
			(asdf-system-directory :weblocks-scripts))
		       (merge-pathnames
			(make-pathname :name "stores" :type "lisp")
			new-project-conf-dir)
		       *app-name-placeholder*
		       (attributize-name name))
    ; tell user to add new app to asdf central registry
    (format t "~%~A has been created.~%" name)
    (format t "Please add '~A' to asdf:*central-registry* before you proceed.~%" new-project-dir)
    (format t "Happy hacking!")))
