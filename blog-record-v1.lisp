;;;; blog-v1: adding models and a gridedit interface

;;;; We will work inside the blog/ directory, which now has a data/
;;;; directory (will store the posts and user data), pub/ (for
;;;; javascript scripts and css stylesheets), conf/ for configuration
;;;; of the store (by default a prevalence store that stores data in
;;;; xml files under data/), and src/ which will contain the lisp
;;;; source code of the blog.
;;;;
;;;; So let's start working by creating a post and a user class:

;;; src/models.lisp
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

;;;; Now we'll follow the weblocks-demo and play a little with the
;;;; data by using the gridedit widgets.
;;;;
;;;; Gridedit needs three views for its data, corresponding to the
;;;; grid view, the data view to view the details of an item, and the
;;;; form view to edit an item.
;;;;
;;;; We put one gridedit widget for the USER and one for the POST
;;;; class on the same page for convenience, by putting them inside a
;;;; composite widget.
;;;;
;;;; Each of the gridedits has three associated views that we'll
;;;; define thereafter.  A simple title is also added before the grid,
;;;; by using a lambda function and the :WIDGET-PREFIX-FN argument to
;;;; GRIDEDIT.

;;; src/layout.lisp
(in-package :blog)

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
			 ;; gridedit widgets were probably
			 ;; not intended to be put 2 on the
			 ;; same page, so I add an HR tag
			 ;; between the two
			 (with-html (:div (:hr :style "margin: 2em;"))))
		       (make-posts-gridedit))))


;;;; Now we need to modify the file src/init-session.lisp to call the
;;;; function MAKE-ADMIN-PAGE.

;;; src/init-session.lisp
(in-package :blog)

;; Define our application
(defwebapp 'blog
    :description "A web application based on Weblocks")

;; Set public files directory to blog/pub
(setf *public-files-path* (compute-public-files-path :blog))

(defun init-user-session (comp)
  (setf (composite-widgets comp)
	(make-admin-page)))

;;;; Finally, we need to define the views to be used in the GRIDEDIT.
;;;; The most straightforward way is to scaffold them based on the
;;;; USER and POST slots, see what the result is and modify the views
;;;; after.

;;; src/views.lisp
(in-package :blog)

(defview user-table-view (:type table :inherit-from '(:scaffold user)))
(defview user-data-view (:type data :inherit-from '(:scaffold user)))
(defview user-form-view (:type form :inherit-from '(:scaffold user)))

(defview post-table-view (:type table :inherit-from '(:scaffold post)))
(defview post-data-view (:type data :inherit-from '(:scaffold post)))
(defview post-form-view (:type form :inherit-from '(:scaffold post)))


;;;; For completeness, here is the modified blog.asd file.

;;; blog.asd
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:blog-asd
  (:use :cl :asdf))

(in-package :blog-asd)

(defsystem blog
    :name "blog"
    :version "0.0.1"
    :maintainer ""
    :author ""
    :licence ""
    :description "blog"
    :depends-on (:weblocks)
    :components ((:file "blog")
		 (:module conf
		  :components ((:file "stores"))
		  :depends-on ("blog"))
		 (:module src
		  :components ((:file "init-session" :depends-on ("layout"))
			       (:file "layout" :depends-on ("models" "views"))
			       (:file "models")
			       (:file "views" :depends-on ("models")))
		  :depends-on ("blog" conf))))


;;;; Now compile the files or load them from asdf, and restart the
;;;; blog from the REPL. 

(blog:stop-blog)
(blog:start-blog :debug t)

;;;; The two gridedit widgets show "no information available" but we
;;;; can add a user using the "Add" button.
;;;;
;;;; A few remarks:
;;;;
;;;; - For some reason unknown to me the title that I added in front
;;;;   of each widget disappears.
;;;;   
;;;; - The scaffold defines fields from all slots for which there is a
;;;;   slot reader when in data/table view, and all fields for which
;;;;   there is a slot writer when in form view.  So the ID slots are
;;;;   not shown but all other slots are shown.
;;;;
;;;; - The data are validated by using the :TYPE information in the
;;;;   slots.  So you can add a user, but no post as there is no way
;;;;   to enter a user object directly.  Let's change this by
;;;;   modifying POST-FORM-VIEW.


;;;; ChangeLog
blog-v1:	
	
	* src/views.lisp (user-table-view, user-data-view, user-form-view)
	(post-table-view, post-data-view, post-form-view): scaffolded views
	for the gridedit interface

	* src/init-session.lisp (init-user-session): call MAKE-ADMIN-PAGE

	* src/layout.lisp (make-users-gridedit, make-posts-gridedit)
	(make-admin-page): add simple gridedit interface for the two
	models

	* src/models.lisp (user, post): USER and POST models
