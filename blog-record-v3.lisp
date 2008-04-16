
;;;; blog-v3: Widgets
;;;;
;;;; Now let's make some widgets to show and edit the blog.  The
;;;; simplest widget that we can probably make is to just present some
;;;; data using a data view.  We specialize on RENDER-WIDGET-BODY to
;;;; render the widget.

;;; src/widgets/post.lisp
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
	       displayed when in :SHORT mode."))
  (:documentation "widget to handle a blog post"))

(defmethod render-widget-body ((obj post-widget) &key)
  (ecase (mode obj)
    (:short
     (when (short-view obj)
      (render-object-view (post obj) (short-view obj) :widget obj)))
    (:full
     (when (full-view obj)
      (render-object-view (post obj) (full-view obj) :widget obj)))))

;;;; In the top of the admin page let's add a link that will display
;;;; the blog.  For now, we'll have the function MAKE-BLOG-WIDGET just
;;;; return a composite widget with a POST-WIDGET inside, and we'll
;;;; later put a BLOG-WIDGET instead.  Since we just want to check
;;;; that our home-made widget displays correctly, let's initialize it
;;;; with the POST that has ID=1 (create a post with the admin page).
;;;;
;;;; Note that I don't fully understand the continuation framework
;;;; yet, so I used DO-PAGE and ANSWER by trial and error and the
;;;; calls below work but I don't know if it's the best way to do
;;;; this.

;;; src/layout.lisp
(in-package :blog)

(defun make-users-gridedit ()
  (make-instance 'gridedit
		 :name 'users-grid
		 :data-class 'user
		 :view 'user-grid-view
		 :widget-prefix-fn (lambda (&rest args)
				     (declare (ignore args))
				     (with-html (:h1 "Users")))
		 :item-data-view 'user-data-view
		 :item-form-view 'user-form-view))

(defun make-posts-gridedit ()
  (make-instance 'gridedit
		 :name 'posts-grid
		 :data-class 'post
		 :widget-prefix-fn (lambda (&rest args)
				     (declare (ignore args))
				     (with-html (:h1 "Posts")))
		 :view 'post-grid-view
		 :item-data-view 'post-data-view
		 :item-form-view 'post-form-view))

(defun make-admin-page ()
  (make-instance 'composite
		 :widgets
		 (list (lambda ()
			 (render-link (lambda (&rest args)
					(declare (ignore args))
					(do-page (make-blog-widget)))
				      "view blog"))
		       (make-users-gridedit)
		       (lambda ()
			 ;; gridedit widgets were probably not
			 ;; intended to be put 2 on the same page, so
			 ;; I add an HR tag between the two
			 (with-html (:div (:hr :style "margin: 2em;"))))
		       (make-posts-gridedit))))

(defun make-blog-widget ()
  (let ((composite
	 (make-instance
	  'composite
	  :widgets (list
		    (make-instance 'post-widget
				   :short-view 'post-data-view
				   :full-view 'post-data-view
				   :post (post-by-id 0))))))
    (push (lambda ()
	    (render-link (lambda (&rest args)
			   (declare (ignore args))
			   (answer composite))
			 "admin"))
	  (composite-widgets composite))
    composite))

;;; added in src/models.lisp
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

;;;; Now you can go and check that the first post is indeed displayed
;;;; (you will need to restart your session or restart the blog
;;;; application), and so let's turn on to making a more useful
;;;; BLOG-WIDGET.


;;;; ChangeLog
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
	
	* src/views.lisp (user-grid-view, user-data-view, user-form-view)
	(post-grid-view, post-data-view, post-form-view): scaffolded views
	for the gridedit interface

	* src/init-session.lisp (init-user-session): call MAKE-ADMIN-PAGE

	* src/layout.lisp (make-users-gridedit, make-posts-gridedit)
	(make-admin-page): add simple gridedit interface for the two
	models

	* src/models.lisp (user, post): USER and POST models
