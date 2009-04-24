(in-package :simple-blog)

(defun make-main-page ()
  (make-navigation 'navigation
		   'main (make-blog-widget)
		   'admin (make-admin-page)))

(defun make-users-gridedit ()
  (make-instance 'gridedit
		 :name 'users-grid
		 :data-class 'user
		 :view 'user-table-view
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
		 :view 'post-table-view
		 :item-data-view 'post-data-view
		 :item-form-view 'post-form-view))

(defun make-admin-page ()
  (make-instance 'composite
		 :widgets
		 (list (make-users-gridedit)
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
		    (make-instance 'blog-widget
				   :post-short-view 'post-short-view
				   :post-full-view 'post-full-view)))))    
    composite))
