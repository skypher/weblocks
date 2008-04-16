(in-package :blog)

(defclass user ()
  ((id :documentation "this is automatically assigned by cl-prevalence
       when we persist the post object")
   (name :accessor user-name
	 :initarg :name
	 :initform ""
	 :type string)))

(defclass post ()
  ((id)
   (short-text :accessor post-short-text
	       :initarg :short-text
	       :initform ""
	       :type string
	       :documentation "short text of the post, to be shown on
	       the main page of the blog")
   (text :accessor post-text
	 :initarg :text
	 :initform ""
	 :type string
	 :documentation "long text of the post, shown when the user
	 clicks on the link after the short text")
   (time :accessor post-time
	 :initarg :time
	 :initform (get-universal-time)
	 :documentation "time at which the post was created")
   (author :accessor post-author
	   :initarg :author
	   :initform nil
	   :type user)))

(defgeneric post-author-id (post)
  (:method ((post post))
    (when (post-author post)
      (object-id (post-author post)))))

(defgeneric post-author-name (post)
  (:method ((post post))
    (when (post-author post)
      (user-name (post-author post)))))

(defun post-formatted-time (post)
  (multiple-value-bind (second minute hour date month year day
			       daylight-p zone)
      (decode-universal-time (post-time post))
    (declare (ignore second daylight-p zone date))
    (format nil "~d-~d-~d ~d:~d" year month day hour minute)))

(defun all-users (&rest args)
  "return all objects of class USER.  ARGS is an added argument that
  is ignored (needed for use in dropdown lists in views)."
  (declare (ignore args))
  (find-persistent-objects (class-store 'user) 'user))

(defun all-posts (&rest args)
  "return all objects of class POST.  ARGS is an added argument that is
  ignored (needed for use in dropdown lists in views)."
  (declare (ignore args))
  (find-persistent-objects (class-store 'post) 'post))

(defun post-by-id (id)
  (first
   (remove-if-not
    (lambda (post)
      (when (slot-boundp post 'id)
	(= id (slot-value post 'id))))
    (all-posts))))
