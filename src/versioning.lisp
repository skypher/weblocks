(in-package weblocks)

;;; This file contains utilities to check whether a file has been modified.
;;; Versioning of a modified file can be turned on 

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
			 (record-folder "mod-record/") (extention ".mod"))
  (let ((system-folder (asdf-system-directory system)))
    (merge-pathnames (concatenate 'string (relative-path original-path system-folder)
				  extention)
		     (merge-pathnames record-folder system-folder))))

(defun write-to-mod-record (mod-time version record-path)
  (write-to-file `',(cons mod-time version) record-path))

(defun make-versioned-name (original-name-without-extention version)
  (concatenate 'string original-name-without-extention "." (write-to-string version)))

(defun make-versioned-path (path version)
  (let ((dir-list (pathname-directory path)))
    (princ-to-string (make-pathname :directory (push-end "vzn" dir-list)
				    :name (make-versioned-name (pathname-name path) version)
				    :type (pathname-type path)))))

(defun get-mod-record (original-path &key (versioning-p nil))
  (let ((record-path (make-record-path original-path)))
    (if (cl-fad:file-exists-p record-path)
	(let* ((cell (read-from-file record-path))
	       (time (car cell))
	       (version (cdr cell)))
	  (make-instance 'mod-record :last-mod-time time
			 :last-version version :original-path original-path))
	(let ((time (file-write-date original-path)))
	  (write-to-mod-record time 0 record-path)
	  (when versioning-p
	    (copy-file original-path (make-versioned-path original-path 0)
		       :if-does-not-exist :ignore :if-exists :supersede))
	  (make-instance 'mod-record :last-mod-time time
			 :last-version 0 :original-path original-path)))))

(defun file-modified-p (mod-record)
  (with-slots (last-mod-time original-path) mod-record
    (not (= last-mod-time (file-write-date original-path)))))

(defun update-mod-record (mod-record &key (versioning-p nil))
  (with-slots (last-mod-time last-version original-path mod-record-path) mod-record
    (setf last-mod-time (file-write-date original-path))
    (when versioning-p
      (incf last-version)
      (copy-file original-path (make-versioned-path original-path last-version)
		 :if-does-not-exist :ignore :if-exists :supersede))
    (write-to-mod-record last-mod-time last-version mod-record-path)))
			 
(defvar *version-dependencies-lock* (bordeaux-threads:make-lock))

(defun update-versioned-path (original-path &optional other-path)
  "If the file has been modified, it is copied and renamed with the correct version number in the same directory. If the file has never being modified before, its name is kept the same."
  (bordeaux-threads:with-lock-held (*version-dependencies-lock*)
    (let ((mod-record (get-mod-record original-path :versioning-p t)))
      (when (file-modified-p mod-record) (update-mod-record mod-record :versioning-p t))
      (with-slots (last-version) mod-record
	(values (make-versioned-path original-path last-version)
		(make-versioned-path other-path last-version))))))