(eval-when (:compile-toplevel)
  (format t "~3&Loading weblocks.session~3%"))

(defpackage #:weblocks.session
  (:use #:cl)
  (:export
   #:delete-value
   #:get-value
   #:reset-latest-session
   #:set-value))
(in-package weblocks.session)


(defvar *session* nil
  "Stores current requests's session")


(defvar *latest-session* nil
  "Stores last session, to be able to clear it during development.

To clear, use function \(reset-last-session\).")


;; previously webapp-session-value
(defmacro get-value (key &optional default)
  "Get a session value from the currently running webapp.
KEY is compared using EQUAL.

It was made as a macro to not evaluate 'default' on each call."

  `(progn (unless *session*
            (error "Session was not created for this request!"))
          ;; TODO: seems, previously keys were separated for different weblocks apps
          ;;       but I've simplified it for now
          (alexandria:ensure-gethash ,key *session* ,default)))


(defun set-value (key value)
  "Set a session value for the currently running webapp.
KEY is compared using EQUAL."
  
  (setf (gethash key *session*)
        value))


;; Previously delete-webapp-session-value
(defun delete-value (key &optional (webapp weblocks::*current-webapp*))
  "Clear the session value for the currently running webapp.
KEY is compared using EQUAL."
  (declare (ignorable webapp))

  (remhash key *session*))


(defun reset-latest-session ()
  (when *latest-session*
    (clrhash *latest-session*)))
