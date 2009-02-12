(in-package :blog)

(defwidget post-widget ()
  ((post :accessor post
	 :initarg :post
	 :initform nil)
   (mode :accessor mode
	 :initarg :mode
	 :initform :short
	 :documentation "The post can be displayed in two
	 versions, :SHORT and :FULL.")
   (short-view :accessor short-view
	       :initarg :short-view
	       :initform nil
	       :documentation "View to determine how the post is
	       displayed when in :SHORT mode.")
   (full-view :accessor full-view
	      :initarg :full-view
	      :initform nil
	      :documentation "View to determine how the post is
	       displayed when in :SHORT mode.")
   (on-select :accessor on-select
	      :initarg :on-select
	      :initform nil
	      :documentation "Function to be called when this post is
	      selected.  It accepts POST-WIDGET as argument."))
  (:documentation "widget to handle a blog post"))

(defgeneric post-action-select (post-widget)
  (:documentation "return an action that selects POST-WIDGET")
  (:method ((post-widget post-widget))
    (make-action
     (lambda (&rest args)
       (declare (ignore args))
       (setf (mode post-widget) :full)
       (safe-funcall (on-select post-widget) post-widget)))))

(defmethod render-widget-body ((obj post-widget) &key)
  (ecase (mode obj)
    (:short
     (when (short-view obj)
      (render-object-view (post obj) (short-view obj)
			  :widget obj
			  ;; after the fields of the POST object,
			  ;; display a link to see the full post
			  :fields-suffix-fn
			  (lambda (&rest args)
			    (declare (ignore args))
			    (when (on-select obj)
			      (render-link (post-action-select obj) "more"))))))
    (:full
     (when (full-view obj)
      (render-object-view (post obj) (full-view obj) :widget obj)))))
