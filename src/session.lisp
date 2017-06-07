(eval-when (:compile-toplevel)
  (format t "~3&Loading weblocks.session~3%"))

(defpackage #:weblocks.session
  (:use #:cl)
  (:export
   #:delete-value
   #:get-value))
(in-package weblocks.session)


(defvar *session* nil
  "Stores current requests's session")


;; previously webapp-session-value
(defun get-value (key &optional (webapp weblocks::*current-webapp*))
  "Get a session value from the currently running webapp.
KEY is compared using EQUAL."
  (declare (ignorable webapp))

  (unless *session*
    (error "Session was not created for this request!"))
  ;; TODO: seems, previously keys were separated for different weblocks apps
  ;;       but I've simplified it for now
  (gethash key *session*))


(defun (setf get-value) (value key &optional (webapp weblocks::*current-webapp*))
  "Set a session value for the currently running webapp.
KEY is compared using EQUAL."
  (declare (ignorable webapp))
  
  (setf (gethash key *session*)
        value))


;; Previously delete-webapp-session-value
(defun delete-value (key &optional (webapp weblocks::*current-webapp*))
  "Clear the session value for the currently running webapp.
KEY is compared using EQUAL."
  (declare (ignorable webapp))

  (remhash key *session*))

