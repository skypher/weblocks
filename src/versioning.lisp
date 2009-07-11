(in-package weblocks)

;;; This file contains utilities to check whether a file has been modified.
;;; This done by creating a shadow directory that contains the modification record
;;; for files in a user specified directory. The shadow directory has the same
;;; structure as the original directory and is under the orginal directory.
;;; Modification is detected by comparing last modified time of a file against its
;;; modification record.

;;; Versioning is used for dependencies (e.g. CSS JS files)
;;; It is turned on through version-dependency-types in application.lisp

;;; Versioning works like this: When modification of a file is detected, a new file
;;; is created with versioined name under the folder vzn, which is located under the
;;; same folder as the file itself. (e.g. ../pub/script/weblocks.js is copied to
;;; ../pub/script/vzn/weblocks.0.js) The path of the versioned file is then used for
;;; serving replies, bundling, gziping and etc. This means you can work on the
;;; original file without keep track of versions of a file at all.

;;; --JT jt@homejt.com

(defclass mod-record ()
  ((original-path :accessor original-path :initarg :original-path
		  :documentation "Path of the original unversioned file.")
   (mod-record-path :accessor mod-record-path :initarg :mod-record-path
		    :documentation "Path of the modification record of the file.")
   (last-mod-time :accessor last-mod-time :initarg :last-mod-time
		  :documentation "Last recorded modified time of the file.")
   (last-version :accessor last-version :initarg :last-version
		 :documentation "Last version of the file. (Currently implemented as an integer)"))
  (:documentation "Holds the modification record of a file."))

(defun make-record-path (original-path &key (system :weblocks)
			 (record-folder "mod-record/") (extension ".mod"))
  (let ((system-folder (asdf-system-directory system)))
    (merge-pathnames (concatenate 'string (relative-path original-path system-folder)
				  extension)
		     (merge-pathnames record-folder system-folder))))

(defun write-to-mod-record (mod-time version record-path)
  (write-to-file `',(cons mod-time version) record-path))

(defun make-versioned-path (path version)
  (let ((dir-list (pathname-directory path)))
    (assert dir-list)
    (make-pathname :directory (push-end "vzn" dir-list)
                   ;; SBCL doesn't like dots in the NAME while CCL
                   ;; barfs at them in the TYPE. Lispworks takes it
                   ;; both ways. Not sure how other Lisps handle this.
                   :name #+ccl(pathname-name path)
                   #-ccl(concatenate 'string (pathname-name path) "." (princ-to-string version))
                   :type #+ccl(concatenate 'string (princ-to-string version) "." (pathname-type path))
                   #-ccl(pathname-type path))))

(defun create-versioned-file (original-path version)
  (let ((new-path (make-versioned-path original-path version)))
    (copy-file original-path new-path :if-does-not-exist :ignore :if-exists :supersede)))

(defun get-mod-record (original-path &key (versioning-p nil))
  (let ((record-path (make-record-path original-path)))
    (if (cl-fad:file-exists-p record-path)
	(let* ((cell (read-from-file record-path))
	       (time (car cell))
	       (version (cdr cell)))
	  (make-instance 'mod-record :last-mod-time time
			 :mod-record-path record-path
			 :last-version version :original-path original-path))
	(let ((time (file-write-date original-path)))
	  (write-to-mod-record time 0 record-path)
	  (when versioning-p
	    (create-versioned-file original-path 0))
	  (make-instance 'mod-record :last-mod-time time
			 :mod-record-path record-path
			 :last-version 0 :original-path original-path)))))

(defun file-modified-p (mod-record)
  (with-slots (last-mod-time original-path) mod-record
    (not (= last-mod-time (file-write-date original-path)))))

(defun update-mod-record (mod-record &key (versioning-p nil))
  (with-slots (last-mod-time last-version original-path mod-record-path) mod-record
    (setf last-mod-time (file-write-date original-path))
    (when versioning-p
      (incf last-version)
      (create-versioned-file original-path last-version))
    (write-to-mod-record last-mod-time last-version mod-record-path)))
			 
(defvar *version-dependencies-lock* (bordeaux-threads:make-lock))

(defun update-versioned-dependency-path (original-path &optional other-path)
  "If the file has been modified, it is copied and renamed with the correct version number in the same directory. If the file has never being modified before, its name is kept the same."
  (bordeaux-threads:with-lock-held (*version-dependencies-lock*)
    (let ((mod-record (get-mod-record original-path :versioning-p t)))
      (when (file-modified-p mod-record) (update-mod-record mod-record :versioning-p t))
      (with-slots (last-version) mod-record
	(values (make-versioned-path original-path last-version)
		(make-versioned-path other-path last-version))))))


;;; Dealing with CSS import rules

(defun write-import-css (url stream)
  (write-char #\Newline stream)
  (write-string "@import url(" stream)
  (princ url stream)
  (write-string ");" stream))
	   
(defun extract-import-urls (string)
  (let (urls (start 0))
    (loop
       (multiple-value-bind (head tail) (cl-ppcre:scan "(?i)import url\(.*?\);" string :start start)
	 (if head
	     (progn
	       (push (subseq string (+ head 11) (- tail 2)) urls)
	       (setf start tail))
	     (return-from extract-import-urls urls))))))

(defun local-path-from-url (url &key (type :stylesheet))
  (let* ((name (pathname-name url))
	 (relative (public-file-relative-path type name))
	 (webapp (current-webapp))
	 (local (princ-to-string (merge-pathnames relative
						  (compute-webapp-public-files-path webapp)))))
    (when (cl-fad:file-exists-p local)
      (values local
	      (princ-to-string (merge-pathnames relative
						(maybe-add-trailing-slash (compute-webapp-public-files-uri-prefix webapp))))))))

(defun update-import-css-content (import-path &key (version-types (version-dependency-types* (current-webapp)))
				  (gzip-types (gzip-dependency-types* (current-webapp))))
  (let ((urls (extract-import-urls (slurp-file import-path))))
    (with-file-write (stream import-path)
      (dolist (url (nreverse urls))
	(multiple-value-bind (physical-path virtual-path) (local-path-from-url url)
	  (if physical-path
	      (progn
		(when (find :stylesheet version-types)
		  (multiple-value-setq (physical-path virtual-path)
		    (update-versioned-dependency-path physical-path virtual-path)))
		(when (find :stylesheet gzip-types)
		  (create-gziped-dependency-file physical-path))
		(write-import-css virtual-path stream))
	      (write-import-css url stream)))))))
