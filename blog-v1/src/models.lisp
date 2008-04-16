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
