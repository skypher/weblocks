(in-package :blog)

;;;; blog-v2: define views

;;;; We'll override some of the fields in the scaffolding.  For each
;;;; field, we can specify some arguments to specify how to present,
;;;; obtain, write or parse the data.  Note also that currently, if I
;;;; override a field then I lose the associated scaffolding
;;;; (e.g. that the field is required).

;;; added in src/views.lisp
(defview post-form-view (:type form :inherit-from '(:scaffold post))
  (time :hidep t)
  ;; POST-AUTHOR-ID and ALL-USERS will be defined below
  (author :reader #'post-author-id
	  :present-as (dropdown :choices #'all-users
				:label-key #'user-name)
	  :parse-as (object-id :class-name 'user)
	  :requiredp t)
  (short-text :present-as textarea
	      :requiredp t)
  (text :present-as (textarea :cols 30)
	:requiredp t))

;;;; Here we don't really need to manually edit the time as it should
;;;; be done automatically by the backend, so we hide the TIME field.
;;;;
;;;; We had better present the SHORT-TEXT and TEXT by using a TEXTAREA
;;;; presentation so that they're easier to edit.  It is actually a
;;;; class named TEXTAREA-PRESENTATION located in the weblocks source
;;;; in the file
;;;; "cl-weblocks/src/views/types/presentations/textarea.lisp".  Now
;;;; is probably a good time to have a look.  You will see that it has
;;;; among others a COLS slot, which the DEFVIEW macro allows us to
;;;; set by using the syntax above.
;;;;
;;;; Now for the AUTHOR.  As is done in the weblocks-demo application,
;;;; we present it as an HTML dropdown list.  The idea is to show the
;;;; user a list of the names of users, and each name has for value
;;;; the ID of the user.  We then map the selected user ID to the
;;;; actual user object.
;;;;
;;;; This is done by using a DROPDOWN presentation (in a file next to
;;;; the TEXTAREA one).  To :CHOICES we assign a function that returns
;;;; a list of objects to choose from, and :LABEL-KEY the function
;;;; used to convert each object to a string.  That's it for what
;;;; we'll see in the browser.
;;;;
;;;; Under the cover we need to pass the USER objects as their ID's,
;;;; and parse an ID into a USER object.  The former is done by
;;;; specifying a function as :READER (takes a POST as argument), and
;;;; the latter by using a parser named OBJECT-ID.  As for
;;;; presentation, this is actually a class named OBJECT-ID-PARSER
;;;; which you'll find in
;;;; cl-weblocks/src/views/types/parsers/common.lisp at the end of the
;;;; file.  As before CLASS-NAME is one slot of OBJECT-ID-PARSER.
;;;;
;;;; Finally, before we make this work we need to define
;;;; POST-AUTHOR-ID and ALL-USERS,

;;; added in src/models.lisp:
(in-package :blog)

(defgeneric post-author-id (post)
  (:method ((post post))
    (when (post-author post)
      (object-id (post-author post)))))

(defun all-users (&rest args)
  "return all objects of class USER.  ARGS is an added argument that
  is ignored (needed for use in dropdown lists in views)."
  (declare (ignore args))
  (find-persistent-objects (class-store 'user) 'user))

;;;; Now we can add and delete users and posts using the gridedit
;;;; widgets.  That should be enough of an introduction to views and
;;;; presentations.


;;;; ChangeLog
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
