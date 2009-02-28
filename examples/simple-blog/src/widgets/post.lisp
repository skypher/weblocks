(in-package :simple-blog)

(defwidget post-widget ()
  ((blog :accessor blog-widget
	 :initarg :blog) 
   (post :accessor post
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

(defmethod with-widget-header ((obj post-widget) body-fn &rest args &key
			       widget-prefix-fn widget-suffix-fn
			       &allow-other-keys)
  (with-html
    (:div :class (dom-classes obj) :id (dom-id obj)
	  :style (when (eql (mode obj) :short)
		   (format nil "margin-left: ~A%;" (random 60)))
	  (safe-apply widget-prefix-fn obj args)
	  (apply body-fn obj args)
	  (safe-apply widget-suffix-fn obj args))))

(defmethod render-widget-body ((obj post-widget) &key)
  (ecase (mode obj)
    (:short
     (when (short-view obj)
      (render-object-view (post obj) (short-view obj) :widget obj)))
    (:full
     (when (full-view obj)
      (render-object-view (post obj) (full-view obj) :widget obj)))))
