(defpackage #:weblocks.session
  (:use #:cl)
  (:export
   #:delete-value
   #:get-value
   #:set-value
   #:gen-id))
(in-package weblocks.session)


(defvar *session* nil
  "Stores current requests's session")



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


(defun weblocks.session:gen-id (&optional (prefix "dom"))
  "Generates an ID unique accross the session. The generated ID can be
used to create IDs for html elements, widgets, etc."
  (let ((new-widget-id (1+ (or (get-value 'last-unique-id) -1))))
    (set-value 'last-unique-id new-widget-id)
    (apply #'concatenate 'string (mapcar #'princ-to-string (list prefix new-widget-id)))))

