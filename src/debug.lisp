(defpackage #:weblocks.debug
  (:use #:cl)
  (:export #:*latest-session*
           #:reset-latest-session
           #:on
           #:off
           #:status))
(in-package weblocks.debug)


;; TODO: move useful staff from debug-mode.lisp, to this package


(defvar *on* nil
  "When true, then Weblocks will be saving addional information useful
for debugging.")


(defvar *config* nil
  "Stores options of last call to (on) or nil if it newer called.

The value of this variable is returned by (status) call as a second value.")


(defvar *latest-session* nil
  "Stores last session, to be able to clear it during development.

To clear, use function \(reset-last-session\).")


(defun on (&key (track-latest-session t))
  (setf *on* t)
  
  (when track-latest-session
    ;; This piece will store latest session in a variable
    (weblocks.hooks:add-application-hook :handle-request
        track-latest-session ()
      
      (setf *latest-session*
            weblocks.session::*session*)))

  (values))


(defun off ()
  (setf *on* t)
  (values))


(defun status ()
  (values *on* *config*))


(defun reset-latest-session ()
  (unless *on*
    (error "Debugging wasn't turned on and I know nothing about latest session."))
  
  (when *latest-session*
    (weblocks.hooks:with-hook (:reset-session *latest-session*)
                              (clrhash *latest-session*))))
