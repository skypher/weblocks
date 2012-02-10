
(in-package :weblocks)

(export '(*last-session*
          start-weblocks
          stop-weblocks
          *weblocks-server*
	  server-type
          server-version
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
                                       (acceptor-class 'weblocks-acceptor)
		       &allow-other-keys)
  "Starts weblocks framework hooked into Hunchentoot server.

Set DEBUG to true in order for error messages and stack traces to be shown
to the client (note: stack traces are temporarily not available due to changes
in Hunchentoot 1.0.0).

Set ACCEPTOR-CLASS if you want to use a custom acceptor (it must inherit
from WEBLOCKS-ACCEPTOR).

All other keywords will be passed as initargs to the acceptor;
the initargs :PORT and :SESSION-COOKIE-NAME default to
8080 and `weblocks-GENSYM'.

Also opens all stores declared via DEFSTORE and starts webapps
declared AUTOSTART."
  (unless (member :bordeaux-threads *features*)
    (cerror "I know what I'm doing and will stubbornly continue."
            "You're trying to start Weblocks without threading ~
            support. Recompile your Lisp with threads enabled."))
  (if debug
    (enable-global-debugging)
    (disable-global-debugging))
  (when (null *weblocks-server*)
    (values
      (start (setf *weblocks-server*
                   (apply #'make-instance acceptor-class :port port
                          (remove-keyword-parameters keys :port :debug :acceptor-class))))
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
    (when *weblocks-server*
      (stop *weblocks-server*))
    (setf *weblocks-server* nil)))


;;; of interest: http://www.mnot.net/blog/2007/05/15/expires_max-age
(defun send-cache-rules (cache-time)
  (when cache-time
    (check-type cache-time integer)
    (setf (header-out "Expires") (rfc-1123-date (+ (get-universal-time) cache-time)))
    (setf (header-out "Cache-Control") (format nil "max-age=~D" (max 0 cache-time)))))

(defun send-gzip-rules (types script-name request virtual-folder physical-folder)
  (let (content-type)
    (when (and types
	       (search "gzip" (header-in :accept-encoding request))
	       (cl-fad:file-exists-p (format nil "~A~A.gz" physical-folder
					     (relative-path script-name virtual-folder)))
	       (or (and (find :script types)
			(cl-ppcre:scan "(?i)\\.js$" script-name)
			(setf content-type "text/javascript"))
		   (and (find :stylesheet types)
			(cl-ppcre:scan "(?i)\\.css$" script-name)
			(setf content-type "text/css"))))
      (setf (header-out "Content-Encoding") "gzip")
      (setf (slot-value request 'script-name) (format nil "~A.gz" script-name))
      content-type)))
    
(defun weblocks-dispatcher (request)
  "Weblocks' Hunchentoot dispatcher. The function serves all started applications
  and their static files."
  (dolist (app *active-webapps*)
    (let* ((script-name (script-name* request))
           (app-prefix (webapp-prefix app))
           (app-pub-prefix (compute-webapp-public-files-uri-prefix app))
	   content-type)
      (cond
        ((list-starts-with (tokenize-uri script-name nil)
                           (tokenize-uri "/weblocks-common" nil)
                           :test #'string=)
	 (let ((virtual-folder "/weblocks-common/pub/")
	       (physical-folder (aif (ignore-errors (probe-file (compute-public-files-path :weblocks)))
				       it
				       #p"./pub/")))
	   (unless *weblocks-global-debug*
	     (send-cache-rules 100000)
	     (setf content-type
		   (send-gzip-rules '(:stylesheet :script)
				    script-name request virtual-folder physical-folder)))
	   (return-from weblocks-dispatcher
	     (funcall (create-folder-dispatcher-and-handler virtual-folder physical-folder content-type)
		      request))))
        ((and (webapp-serves-hostname (hunchentoot:host) app)
              (list-starts-with (tokenize-uri script-name nil)
                                (tokenize-uri app-pub-prefix nil)
                                :test #'string=))
	 (let ((virtual-folder (maybe-add-trailing-slash app-pub-prefix))
	       (physical-folder (compute-webapp-public-files-path app)))
	   (send-cache-rules (weblocks-webapp-public-files-cache-time app))
	   (setf content-type (send-gzip-rules (gzip-dependency-types* app)
					       script-name request virtual-folder physical-folder))
	   (return-from weblocks-dispatcher
	     (funcall (create-folder-dispatcher-and-handler virtual-folder physical-folder content-type)
		      request))))
        ((and (webapp-serves-hostname (hunchentoot:host) app)
              (list-starts-with (tokenize-uri script-name nil)
                                (tokenize-uri app-prefix nil)
                                :test #'string=))
         (no-cache) ; disable caching for dynamic pages
	 (return-from weblocks-dispatcher 
	   (f0 (handle-client-request app)))))))
  (hunchentoot:log-message* :debug "Application dispatch failed for '~A'" (script-name request)))

;; Redirect to default app if all other handlers fail
;; *** removed from Hunchentoot; find another way to implement this.
#|
(setf hunchentoot:*default-handler*
      (lambda ()
	(if (null (tokenize-uri (script-name*) nil))
	    (progn
	      (unless (get-webapp 'weblocks-default nil)
		(start-webapp 'weblocks-default))
	      (redirect "/weblocks-default"))
	    (setf (return-code*) +http-not-found+))))
|#

;; install weblocks-dispatcher
(eval-when (:load-toplevel)
  (push 'weblocks-dispatcher *dispatch-table*))

(defun session-name-string-pair ()
  "Returns a session name and string suitable for URL rewriting. This
pair is passed to JavaScript because web servers don't normally do URL
rewriting in JavaScript code."
  (if (and *rewrite-for-session-urls*
	   (null (cookie-in (session-cookie-name *weblocks-server*)))
	   (hunchentoot:session-cookie-value *session*))
      (format nil "~A=~A"
	      (url-encode (session-cookie-name *weblocks-server*))
	      (string-upcase (url-encode (hunchentoot:session-cookie-value *session*))))
      ""))

(defun server-type ()
  "Hunchentoot")

(defun server-version ()
  hunchentoot::*hunchentoot-version*)

(defun reset-sessions ()
  (let ((*acceptor* *weblocks-server*))
    (hunchentoot:reset-sessions)))
(export 'reset-sessions)

(defun active-sessions ()
  "Returns a list of currently active sessions."
  (loop for s in (mapcar #'cdr (session-db *weblocks-server*))
        collect s))

