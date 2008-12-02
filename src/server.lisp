
(in-package :weblocks)

(export '(*last-session* start-weblocks stop-weblocks
	  compute-public-files-path server-type server-version
          session-name-string-pair))

(defvar *weblocks-server* nil
  "If the server is started, bound to hunchentoot server
  object. Otherwise, nil.")

(defparameter *maintain-last-session* nil
  "Determines whether *last-session* variable will be maintained at
  each request. Note, this variable is automatically set to a
  hunchentoot lock in debug mode and nil in release mode by
  'start-weblocks'.")

(defvar *last-session* nil
  "Bound to a session object associated with the last handled
  request. Note, this variable is only updated in debug mode.")

;;; Tell hunchentoot to output in utf-8 and to try utf-8 on input by
;;; default (if no encoding information is provided by the client)
(setf *hunchentoot-default-external-format*
      (flexi-streams:make-external-format :utf-8))

;;; Set outgoing encoding to utf-8
(setf *default-content-type* "text/html; charset=utf-8")

(defun start-weblocks (&rest keys &key (debug t) (port 8080)
                             (cookie-name (format nil "weblocks-~(~A~)" (gensym)))
		       &allow-other-keys)
  "Starts weblocks framework hooked into Hunchentoot server. Set
DEBUG to true in order for stacktraces to be shown to the
client. Set COOKIE-NAME when you want to have a specific cookie name;
otherwise a random one with prefix 'weblocks' will be generated for
this server instance. Other keys are passed to HUNCHENTOOT:START-SERVER.
Opens all stores declared via DEFSTORE."
  #+sbcl
  (unless (member :sb-thread *features*)
    (cerror "I know what I'm doing and will stubbornly continue."
            "You're trying to start Weblocks on SBCL without threading
            support. Recompile your SBCL with threads enabled."))
  (if debug
      (enable-global-debugging)
      (disable-global-debugging))
  (when (null *weblocks-server*)
    (setf *session-cookie-name* cookie-name)
    (values
      (setf *weblocks-server*
            (apply #'start-server :port port
                   (remove-keyword-parameters keys :port :debug :cookie-name)))
      (mapcar (lambda (class)
                (unless (get-webapps-for-class class)
                  (start-webapp class :debug debug)))
              *autostarting-webapps*))))

(defun stop-weblocks ()
  "Stops weblocks. Closes all stores declared via 'defstore'."
  (when (not (null *weblocks-server*))
    (dolist (app *active-webapps*)
      (stop-webapp (weblocks-webapp-name app)))
    (setf *last-session* nil)
    (reset-sessions)
    (when (not (null *weblocks-server*)) (stop-server *weblocks-server*))
    (setf *weblocks-server* nil)))

(defun compute-public-files-path (asdf-system-name &optional (suffix "pub"))
  "Computes the directory of public files. The function uses the
following protocol: it finds the canonical path of the '.asd' file of
the system specified by 'asdf-system-name', and goes into 'pub'."
  (merge-pathnames
   (make-pathname :directory `(:relative ,suffix))
   (asdf-system-directory asdf-system-name)))

(defun weblocks-dispatcher (request)
  "Dispatcher function suitable for inclusion into hunchentooth dispatch table.
The function serves all started applications"
  (dolist (app *active-webapps*)
    (let* ((script-name (script-name request))
	   (app-prefix (webapp-prefix app))
	   (app-pub-prefix (compute-webapp-public-files-uri-prefix app)))
      (log-message :debug "Application dispatch for ~S/~S" (hunchentoot:host) script-name)
      (cond
	((list-starts-with (tokenize-uri script-name nil)
			   (tokenize-uri "/weblocks-common" nil)
			   :test #'string=)
	 (log-message :debug "Dispatching to common public file")
         (return-from weblocks-dispatcher
                      (funcall (create-folder-dispatcher-and-handler 
                                 "/weblocks-common/pub/"
                                 (aif (ignore-errors (probe-file (compute-public-files-path :weblocks)))
                                      it
                                      #p"./pub")) ; as a last fallback
                               request)))
        ((and (webapp-serves-hostname (hunchentoot:host) app)
              (list-starts-with (tokenize-uri script-name nil)
			   (tokenize-uri app-pub-prefix nil)
			   :test #'string=))
	 (log-message :debug "Dispatching to public file")
         ;; set caching parameters for static files
         ;; of interest: http://www.mnot.net/blog/2007/05/15/expires_max-age
         (if (weblocks-webapp-debug app)
           (no-cache)
           (let ((cache-time (weblocks-webapp-public-files-cache-time app)))
             (check-type cache-time integer)
             (setf (header-out "Expires") (rfc-1123-date (+ (get-universal-time) cache-time)))
             (setf (header-out "Cache-Control") (format nil "max-age=~D" (max 0 cache-time)))))
	 (return-from weblocks-dispatcher
	   (funcall (create-folder-dispatcher-and-handler 
		     (maybe-add-trailing-slash app-pub-prefix)
		     (compute-webapp-public-files-path app))
		    request)))
	((and (webapp-serves-hostname (hunchentoot:host) app)
              (list-starts-with (tokenize-uri script-name nil)
                                (tokenize-uri app-prefix nil)
                                :test #'string=))
	 (log-message :debug "Dispatching to application ~A with prefix ~S"
		      app app-prefix)
	 (return-from weblocks-dispatcher 
	   #'(lambda ()
	       (handle-client-request app)))))))
  (log-message :debug "Application dispatch failed for '~A'" (script-name request)))

;; Redirect to default app if all other handlers fail
(setf hunchentoot:*default-handler*
      (lambda ()
	(if (null (tokenize-uri (script-name) nil))
	    (progn
	      (unless (get-webapp 'weblocks-default nil)
		(start-webapp 'weblocks-default))
	      (redirect "/weblocks-default"))
	    (setf (return-code) +http-not-found+))))

;; install weblocks-dispatcher

(eval-when (:load-toplevel)
  (push 'weblocks-dispatcher *dispatch-table* ))

(defun session-name-string-pair ()
  "Returns a session name and string suitable for URL rewriting. This
pair is passed to JavaScript because web servers don't normally do URL
rewriting in JavaScript code."
  (if (and *rewrite-for-session-urls*
	   (null (cookie-in *session-cookie-name*))
	   (hunchentoot::session-cookie-value))
      (format nil "~A=~A"
	      (url-encode *session-cookie-name*)
	      (string-upcase (url-encode (hunchentoot::session-cookie-value))))
      ""))

(defun server-type ()
  "Hunchentoot")

(defun server-version ()
  hunchentoot::*hunchentoot-version*)

(defun active-sessions ()
  "Returns a list of currently active sessions."
  (let (acc)
    (do-sessions (s acc)
      (push s acc))))


