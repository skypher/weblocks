;;;; mode: common-lisp; mode: paredit; mode: slime
(in-package :simple-blog)

(defclass user ()
  ((id)
   (name :accessor user-name
	 :initarg :name
	 :initform ""
	 :type string)))

(defun all-users (&rest args)
  (declare (ignore args))
  (find-persistent-objects (class-store 'user) 'user))