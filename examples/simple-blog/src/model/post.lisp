;;;; mode: common-lisp; mode: paredit; mode: slime
(in-package :simple-blog)

(defclass post ()
  ((id)
   (title :accessor post-title
	  :initarg :title
	  :initform ""
	  :type string
	  :documentation "a title for the post, to be displayed on
	  both the blog page and on the post page.")
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
  (multiple-value-bind (sec min hr dom mo year dow daylight-p zone)
      (decode-universal-time (post-time post))
    (declare (ignore sec daylight-p zone dom))
    (format nil "~d-~d-~d ~d:~d" year mo dow hr min)))

(defun all-posts (&rest args)
  "return all objects of class POST.  Args is an added argument that
is ignored (needed for use in dropdown lists in views"
  (declare (ignore args))
  (find-persistent-objects (class-store 'post) 'post))

(defun post-by-id (id)
  (first
   (remove-if-not (lambda (post)
		    (when (slot-boundp post 'id)
		      (= id (slot-value post 'id))))
		  (all-posts))))

