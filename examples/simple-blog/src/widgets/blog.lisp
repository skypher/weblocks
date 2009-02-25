(in-package :simple-blog)

(defwidget blog-widget ()
  ((current-post :accessor current-post
		 :initarg :current-post
		 :initform nil
		 :documentation "POST-WIDGET containing the current
		 post when the blog is in :POST mode")
   (posts :accessor posts
	  :initarg :posts
	  :initform (make-instance 'composite)
	  :documentation "composite widget that contains a POST-WIDGET
	  for each post of the blog")
   (mode :accessor mode
	 :initarg :mode
	 :initform :blog
	 :documentation "The blog can be in two modes, :BLOG
	 and :POST.  In :BLOG mode to display a list of posts, and
	 in :POST mode to display an individual post.")
   (post-short-view :accessor post-short-view
		    :initarg :post-short-view
		    :initform nil
		    :documentation "see SHORT-VIEW slot of POST-WIDGET")
   (post-full-view :accessor post-full-view
		   :initarg :post-full-view
		   :initform nil
		   :documentation "see FULL-VIEW slot of POST-WIDGET"))
  (:documentation "widget to handle a blog"))

(defgeneric blog-action-blog-mode (blog-widget)
  (:documentation "return an action that will switch BLOG-WIDGET into :BLOG
  mode")
  (:method ((blog-widget blog-widget))
    (make-action
     (lambda (&rest args)
       (declare (ignore args))
       (when (current-post blog-widget)
	 (setf (mode (current-post blog-widget)) :short))
       (setf (mode blog-widget) :blog)
       (reset-blog blog-widget)))))

(defgeneric blog-make-post-widget (blog-widget post)
  (:documentation "make a POST-WIDGET containing POST. (called by
  RESET-BLOG)")
  (:method ((blog-widget blog-widget) (post post))
    (make-instance 'post-widget
		   :blog blog-widget
		   :post post
		   :short-view (post-short-view blog-widget)
		   :full-view (post-full-view blog-widget)
		   :on-select (lambda (post-widget)
				(setf (current-post blog-widget) post-widget)
				(setf (mode blog-widget) :post)))))

(defgeneric reset-blog (blog-widget)
  (:documentation "Reset the list of post widgets from the posts in
  the database.  This function is called by BLOG-ACTION-BLOG-MODE.")
  (:method ((blog-widget blog-widget))
    (setf (composite-widgets (posts blog-widget))
	  (mapcar (lambda (post)
		    (blog-make-post-widget blog-widget post))
		  (all-posts)))))

(defmethod initialize-instance :after ((obj blog-widget) &key)
  (reset-blog obj))

(defgeneric render-blog (blog-widget mode)
  (:documentation "render a blog widget in mode MODE.  This function
  is called by RENDER-WIDGET-BODY."))

(defmethod render-blog ((blog-widget blog-widget) (mode (eql :blog)))
  (with-html (:h1 "Simple Blog"))
  (render-widget (posts blog-widget)))

(defmethod render-blog ((blog-widget blog-widget) (mode (eql :post)))
  (with-html
    (:h1
     ;; link to come back to the blog
     (render-link (blog-action-blog-mode blog-widget)
		  "Simple Blog")))
  (render-widget (current-post blog-widget)))

(defmethod with-widget-header ((obj blog-widget) body-fn &rest args)
  (with-html
    (:div :class (format nil "~A ~A-mode"
			 (dom-classes obj)
			 (remove #\: (write-to-string (mode obj) :case :downcase)))
	  :id (dom-id obj)
	  (apply body-fn obj args)
	  (when (eql (mode obj) :blog)
	    (htm
	     (:img :id "bubblehead"
		   :src "/pub/images/bubblehead.png"))))))

(defmethod render-widget-body ((obj blog-widget) &key)
  (render-blog obj (mode obj)))
