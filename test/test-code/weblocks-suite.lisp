
(in-package :weblocks-test)

;;; A suite that forces `*print-case*' to be `:upcase', needed in some
;;; comparison contexts
(deftestsuite print-upcase-suite ()
  ())

(defmethod lift::lift-test :around ((suite print-upcase-suite) name)
  (declare (ignore suite name))
  (let ((*print-case* :upcase))
    (call-next-method)))

;;; A suite that sets up an application environment
(deftestsuite application-suite ()
  ()
  (:dynamic-variables (weblocks::*current-webapp*
		       (make-instance 'weblocks::weblocks-webapp :prefix ""
				      :html-indent-p nil)))
  (:setup 
   (setf (weblocks::weblocks-webapp-init-user-session weblocks::*current-webapp*)
	 (lambda (&rest args)
	   (declare (ignore args))))
   (weblocks::open-stores))
  (:teardown (weblocks::close-stores)))

;;; A suite that sets up a session environment
(deftestsuite session-suite ()
  ()
  (:dynamic-variables (hunchentoot::*session-secret* (hunchentoot::reset-session-secret))
		      *session*))

;;; A suite that sets up a web request environment
(deftestsuite request-suite ()
  (make-action-orig
   generate-widget-id-orig)
  (:dynamic-variables (*acceptor* (make-instance 'unittest-server))
                      (*weblocks-server* *acceptor*)
                      (*request* (make-instance 'unittest-request :acceptor *acceptor*))
		      (hunchentoot::*reply* (make-instance 'hunchentoot::reply))
		      (weblocks::*dirty-widgets* nil)
		      (*weblocks-output-stream* (make-string-output-stream))
		      *uri-tokens* *on-ajax-complete-scripts*
		      *before-ajax-complete-scripts*
		      weblocks::*page-dependencies*)
  (:setup (setf (slot-value *request* 'method) :get)
          (setf generate-widget-id-orig #'gen-id)
          (setf (symbol-function 'gen-id)
			      (lambda (&optional prefix)
                                (declare (ignore prefix))
                                "id-123")))
  (:teardown (setf (symbol-function 'gen-id) generate-widget-id-orig)))

;;; WEBLOCKS-SUITE must set up an environment for all weblocks tests
;;; to run in. This includes setting up an application, a web session,
;;; a web request, etc.
(deftestsuite weblocks-suite (application-suite session-suite request-suite)
  ()
  (:dynamic-variables weblocks::*rendered-actions*))

