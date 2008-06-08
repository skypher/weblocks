
;;;; blog-v4: add a blog widget

;;;; One straightforward way to go is to have the BLOG-WIDGET contain
;;;; a list of POST-WIDGETs for each post, and one for the current
;;;; post.  The weblocks COMPOSITE widget can already handle a list of
;;;; widgets for us, so we'll use it again.

;;; src/specials.lisp
(in-package :blog)

(defvar *blog-title* "Blog")

;;; src/widgets/blog.lisp
(in-package :blog)

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
		   :post post
		   :short-view (post-short-view blog-widget)
		   :full-view (post-full-view blog-widget)
		   ;;; we'll add a new slot to POST-WIDGET for this
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
  (with-html (:h1 *blog-title*))
  (render-widget (posts blog-widget)))

(defmethod render-blog ((blog-widget blog-widget) (mode (eql :post)))
  (with-html
    (:h1
     ;; link to come back to the blog
     (render-link (blog-action-blog-mode blog-widget)
		  *blog-title*)))
  (render-widget (current-post blog-widget)))

(defmethod render-widget-body ((obj blog-widget) &key)
  (render-blog obj (mode obj)))

;;;; When we make the POST-WIDGET in the function
;;;; BLOG-MAKE-POST-WIDGET, we introduced a new :ON-SELECT initarg
;;;; which is a function that should be called when the post is
;;;; selected (to view it) to appropriately set the state of
;;;; BLOG-WIDGET.  So we'll now make the corresponding changes to
;;;; POST-WIDGET.

;;; src/widgets/post.lisp
(in-package :blog)

(defwidget post-widget ()
  (;; slots as before
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
   ;; new slot
   (on-select :accessor on-select
	      :initarg :on-select
	      :initform nil
	      :documentation "Function to be called when this post is
	      selected.  It accepts POST-WIDGET as argument."))
  (:documentation "widget to handle a blog post"))

;;; new function
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

;;;; Now we'll change the MAKE-BLOG-WIDGET function in the layout to
;;;; make a BLOG-WIDGET instead of a POST-WIDGET.  And provide two
;;;; specialized views POST-SHORT-VIEW and POST-FULL-VIEW to nicely
;;;; display the posts.

;;; modification in src/layout.lisp
(defun make-blog-widget ()
  (let ((composite
	 (make-instance
	  'composite
	  :widgets (list
		    (make-instance 'blog-widget
				   :post-short-view 'post-short-view
				   :post-full-view 'post-full-view)))))
    (push (lambda ()
	    (render-link (lambda (&rest args)
			   (declare (ignore args))
			   (answer composite))
			 "admin"))
	  (composite-widgets composite))
    composite))

;;;; For the views, we'll have POST-DATA-VIEW display properly
;;;; formatted time and the author name instead of "User" that is the
;;;; default when converting an object of class USER to a string.  We
;;;; can then inherit from this view and hide either the TEXT slot or
;;;; the SHORT-TEXT slot to define the short and full views.

;;; modification in src/views.lisp
(defview post-data-view (:type data :inherit-from '(:scaffold post))
  (author :reader #'post-author-name)
  (time :reader #'post-formatted-time))

(defview post-short-view (:type data :inherit-from 'post-data-view)
  (text :hidep t))

(defview post-full-view (:type data :inherit-from 'post-data-view)
  (short-text :hidep t))

;;; add in src/models.lisp
(defgeneric post-author-name (post)
  (:method ((post post))
    (when (post-author post)
      (user-name (post-author post)))))

(defun post-formatted-time (post)
  (multiple-value-bind (second minute hour date month year day
			       daylight-p zone)
      (decode-universal-time (post-time post))
    (declare (ignore second daylight-p zone date))
    ;; my format-foo is not very good so please improve if you can :)
    (format nil "~d-~d-~d ~d:~d" year month day hour minute)))




;;;; ChangeLog
blog-v4
	
	* blog.asd (blog): add new file

	* src/models.lisp (post-author-name, post-formatted-time): backend
	functions

	* src/layout.lisp (make-blog-widget): make a BLOG-WIDGET instead
	of POST-WIDGET
	(make-blog-widget): use new views for the post

	* src/views.lisp (post-data-view): modify to display formatted
	time, and user name instead of "User"
	(post-short-view, post-full-view): new views for used the two
	states POST-WIDGET

	* src/widgets/post.lisp (post-widget): add ON-SELECT slot so that
	BLOG-WIDGET can set a call-back
	(post-action-select): return an action that selects POST-WIDGET
	(render-widget-body): modify to add a link to see the full post
	[and call the ON-SELECT function if defined]

	* src/widgets/blog.lisp:
	(blog-action-blog-mode, blog-make-post-widget, reset-blog)
	(render-blog, initialize-instance, render-widget-body): new blog
	widget

	* src/specials.lisp (*blog-title*): blog title

blog-v3
	
	* blog.asd (blog): updated for new files

	* src/layout.lisp (make-blog-widget): create a composite widget
	with a post widget and a link
	(make-admin-page): add a link to MAKE-BLOG-WIDGET

	* src/models.lisp (all-posts, post-by-id): backend functions

	* src/widgets/post.lisp (post-widget): simple post widget
	(render-widget-body): specialized method to render the post

blog-v2
	
	* src/models.lisp (post-author-id, all-users): functions used by
	the views

	* src/views.lisp (post-form-view): override some fields - textarea
	for the texts, and dropdown list for the author

blog-v1:	
	
	* src/views.lisp (user-table-view, user-data-view, user-form-view)
	(post-table-view, post-data-view, post-form-view): scaffolded views
	for the gridedit interface

	* src/init-session.lisp (init-user-session): call MAKE-ADMIN-PAGE

	* src/layout.lisp (make-users-gridedit, make-posts-gridedit)
	(make-admin-page): add simple gridedit interface for the two
	models

	* src/models.lisp (user, post): USER and POST models
