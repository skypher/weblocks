
(in-package :weblocks)

(export '(*last-session* start-weblocks stop-weblocks
	  compute-public-files-path server-type
	  server-version active-sessions 
	  set-weblocks-default-public-files-path weblocks-default-public-files-path))

(defvar *weblocks-server* nil
  "If the server is started, bound to hunchentoot server
  object. Otherwise, nil.")

(defparameter *maintain-last-session* nil
  "Determines whether *last-session* variable will be maintained at
  each request. Note, this variable is automatically set to a
  hunchentoot lock in debug mode and nil in release mode by
  'start-weblocks'.")

(defparameter *last-session* nil
  "Bound to a session object associated with the last handled
  request. Note, this variable is only updated in debug mode.")

;;; Tell hunchentoot to output in utf-8 and to try utf-8 on input by
;;; default (if no encoding information is provided by the client)
(setf *hunchentoot-default-external-format*
      (flexi-streams:make-external-format :utf-8))

;;; Set outgoing encoding to utf-8
(setf *default-content-type* "text/html; charset=utf-8")

(defun start-weblocks (&rest keys &key debug (port 8080)
		       &allow-other-keys)
  "Starts weblocks framework hooked into Hunchentoot server. Set
':debug' keyword to true in order for stacktraces to be shown to the
client. Other keys are passed to 'hunchentoot:start-server'. Opens all
stores declared via 'defstore'."
  (when (null *weblocks-server*)
    (setf *session-cookie-name* "weblocks-session")
    (setf *weblocks-server*
	  (apply #'start-server :port port
		 (remove-keyword-parameters keys :port :debug)))
    (dolist (class *autostarting-webapps*)
      (unless (get-webapps-for-class class)
	(start-webapp class :debug debug)))))

(defun stop-weblocks ()
  "Stops weblocks. Closes all stores declared via 'defstore'."
  (when (not (null *weblocks-server*))
    (dolist (app *active-webapps*)
      (stop-webapp (weblocks-webapp-name app)))
    (setf *last-session* nil)
    (reset-sessions)
    (stop-server *weblocks-server*)
    (setf *weblocks-server* nil)))

(defun compute-public-files-path (asdf-system-name)
  "Computes the directory of public files. The function uses the
following protocol: it finds the canonical path of the '.asd' file of
the system specified by 'asdf-system-name', and goes into 'pub'."
  (merge-pathnames
   (make-pathname :directory '(:relative "pub"))
   (asdf-system-directory asdf-system-name)))

(defvar *public-files-path*
  (compute-public-files-path :weblocks)
  "Must be set to a directory on the filesystem that contains public
files that should be available via the webserver (images, stylesheets,
javascript files, etc.) Modify this directory to set the location of
your files. Points to the weblocks' 'pub' directory by default.")

(defun set-weblocks-default-public-files-path (path)
  (setf *public-files-path* path))

(defun weblocks-default-public-files-path ()
  *public-files-path*)

(defun weblocks-dispatcher (request)
  "Dispatcher function suitable for inclusion into hunchentooth dispatch table.
The function serves all started applications"
  (flet ((is-prefix-of (string prefix)
	   (let ((mismatch (mismatch string prefix)))
	     (or (null mismatch)
		 (>= mismatch (length prefix))))))
    (dolist (app *active-webapps*)
      (let* ((script-name (script-name request))
	     (app-prefix (webapp-prefix app))
	     (app-pub-prefix (concatenate 'string app-prefix "/pub/")))
	(log-message :debug "Application dispatch for '~A'" script-name)
	(cond
	  ((is-prefix-of script-name app-pub-prefix)
	   (log-message :debug "Dispatching to public file")
	   (return-from weblocks-dispatcher
	     (funcall (create-folder-dispatcher-and-handler 
		       app-pub-prefix 
		       (or (weblocks-webapp-public-app-path app) *public-files-path*))
		      request)))
	  ((is-prefix-of script-name app-prefix)
	   (log-message :debug "Dispatching to application ~A with prefix '~A'" 
			app app-prefix)
	   (return-from weblocks-dispatcher 
	     #'(lambda ()
		 (handle-client-request app)))))))
    (log-message :debug "Application dispatch failed for '~A'" (script-name request))))

;; install weblocks-dispatcher

(eval-when (:load-toplevel)
  (progn
    (let ((pos (position 'weblocks-dispatcher *dispatch-table*)))
      (if pos
	  (rplaca (nthcdr (1- pos) *dispatch-table*)
		  #'(lambda (request)
		      (funcall (create-folder-dispatcher-and-handler "/pub/" *public-files-path*)
			       request)))
	  (setf *dispatch-table*
		(append 
		 (list 
		  ;; CSS files that are served from webapp specific directories have dependencies
		  ;; on images in "/pub". I am not sure how to dynamically adjusts references in
		  ;; CSS files.
		  #'(lambda (request)
		    (funcall (create-folder-dispatcher-and-handler "/pub/" *public-files-path*)
			     request))
		  'weblocks-dispatcher)
		 *dispatch-table*))))))

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


