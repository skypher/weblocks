(in-package :weblocks)


(defclass bundle-tally ()
  ((last-bundle-id :accessor last-bundle-id :initarg :last-bundle-id
	    :documentation "The id of the last created bundle file.")
   (composition-list :accessor composition-list :initarg :composition-list
		     :documentation "A list of lists. Each list contains a bundle id and a list of files that the bundle is composed of.")
   (modified-p :accessor modified-p :initform nil
	       :documentation "Indicate if the tally has been modified.")
   (bundle-folder :accessor bundle-folder :initarg :bundle-folder
		  :documentation "Stores the path of the bundle folder for easier access.")
   (tally-path :reader tally-path :initarg :tally-path
	       :documentation "Stores the path of the tally file for easier access."))
  (:documentation "A record of the locations and composition files of all bundled files."))


(defun get-bundle-tally ()
  "Copy the tally file into a bundle-tally object"
  (let* ((bundle-folder (merge-pathnames "bundles/" (compute-webapp-public-files-path (current-webapp))))
	 (tally-path (merge-pathnames "tally" bundle-folder))
	 (file-data (when (cl-fad:file-exists-p tally-path)
		      (read-from-file tally-path)))
	 (last-bundle-id (if file-data (car file-data) 0))
	 (composition-list (cdr file-data)))
    (make-instance 'bundle-tally :last-bundle-id last-bundle-id :composition-list composition-list
		   :bundle-folder bundle-folder :tally-path tally-path)))


(defun store-bundle-tally (tally)
  "Save the bundle-tally object into the tally file"
  (when (modified-p tally)
    (write-to-file `',(cons (last-bundle-id tally) (composition-list tally))
		   (tally-path tally))))

(defun add-to-tally (bundle-name file-list tally)
  (with-slots (composition-list modified-p) tally
    (push (cons bundle-name file-list) composition-list)			      
    (setf modified-p t)))


(defun remove-from-tally (bundle-name tally)
  (with-slots (composition-list modified-p) tally
    (setf composition-list (remove-if #'(lambda (x) (string-equal (car x) bundle-name)) composition-list))
    (setf modified-p t)))

(defun create-bundle-file (file-list type tally)
  (let ((bundle-name (format nil "~A.~A" (incf (last-bundle-id tally)) type)))
    (add-to-tally bundle-name file-list tally)
    (merge-files file-list (merge-pathnames bundle-name (bundle-folder tally)))
    bundle-name))

(defun delete-bundle-file (bundle-name tally)
  (remove-from-tally bundle-name tally)
  (delete-file (merge-pathnames bundle-name (bundle-folder tally))))


;;; covers all cases because used after prune-dependencies
;;; otherwise it should be (and (set-difference lst1 lst2) (set-difference lst2 lst1))
(defun contain-same-strings (lst1 lst2)
  (if (= (length lst1) (length lst2))
      (null (set-difference lst1 lst2 :test #'string-equal))))

;;; If the same files have already been bundled, return the bundle-name
(defun find-bundle (file-list tally)
  (car (find-if #'(lambda (x) (contain-same-strings (cdr x) file-list))
		(composition-list tally))))

(defun bundle-modified-p (bundle-name file-list tally)
  (when (files-modified-p file-list)
    (delete-bundle-file bundle-name tally)
    t))

(defvar *bundle-dependencies-lock* (bordeaux-threads:make-lock))

(defun build-bundle (file-list type &key media)
  (bordeaux-threads:with-lock-held (*bundle-dependencies-lock*)
    (let* ((tally (get-bundle-tally))
	   (bundle-name (find-bundle file-list tally)))
      (when (or (null bundle-name)
		(bundle-modified-p bundle-name file-list tally))
	(setf bundle-name (create-bundle-file file-list type tally)))
      (store-bundle-tally tally)
      ;; make new dependency object for the bundle file
      (let ((physical-path (merge-pathnames bundle-name (bundle-folder tally)))
	    (virtual-path (merge-pathnames (format nil "bundles/~A" bundle-name)
					   (maybe-add-trailing-slash (compute-webapp-public-files-uri-prefix (current-webapp))))))
	(cond ((string-equal type "css")
	       (make-instance 'stylesheet-dependency
			      :url virtual-path :media media
			      :local-path physical-path))
	      ((string-equal type "js")
	       (make-instance 'script-dependency :url virtual-path
			      :local-path physical-path)))))))