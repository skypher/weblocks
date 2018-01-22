(defpackage #:weblocks/debug
  (:use #:cl)
  (:import-from #:weblocks/hooks
                #:add-application-hook
                #:with-hook)
  (:import-from #:weblocks/app
                #:*current-app*
                #:find-active-app
                #:get-registered-apps)
  (:import-from #:weblocks/session
                #:*session*)
  (:import-from #:weblocks/request
                #:*request*)
  (:import-from #:weblocks/variables
                #:*ignore-missing-actions*)
  (:export #:*latest-session*
           #:*latest-request*
           #:reset-latest-session
           #:on
           #:off
           #:status
           #:in-app))
(in-package weblocks/debug)


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


(defvar *latest-request* nil
  "Stores last request if debug mode was enabled.")


(defun on (&key
             (track-latest-session t)
             (debug-actions t)
             (track-latest-request t))
  ;; TODO: think about pluggable switchers to not hardcode them into this function
  (setf *on* t)
  
  (when track-latest-session
    ;; This piece will store latest session in a variable
    (add-application-hook :handle-request
        track-latest-session ()
      
      (setf *latest-session*
            *session*))
    
    ;; Remember that we turned this on
    (setf (getf *config* :track-latest-session)
          t))

  (when track-latest-request
    (setf *latest-request*
          *request*)
    (setf (getf *config* :track-latest-request)
          t))

  (when debug-actions
    (setf *ignore-missing-actions*
          nil
          (getf *config* :debug-actions)
          t))

  (values))


(defun off ()
  (setf *on* t)

  (when (getf *config* :track-latest-session)
    ;; TODO: implement hook removal
    ;; (weblocks.hooks:remove-application-hook :handle-request
    ;;                                         track-latest-session)
    )
  
  (when (getf *config* :debug-actions)
    (setf *ignore-missing-actions*
          t))
  
  (setf *config* nil)
  (values))


(defun status ()
  (values *on* *config*))


(defun reset-latest-session ()
  (unless *on*
    (error "Debugging wasn't turned on and I know nothing about latest session."))
  
  (when *latest-session*
    (with-hook (:reset-session *latest-session*)
      (clrhash *latest-session*))))


(defun in-app (&optional name)
  "Set the current webapp to NAME, or the last webapp registered if NAME is
not supplied. Returns the selected webapp. Convenience function for the REPL."
  (setf *current-app*
        (find-active-app
         (or name
             (first (get-registered-apps))))))
