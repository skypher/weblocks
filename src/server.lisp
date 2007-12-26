
(in-package :weblocks)

(export '(start-weblocks stop-weblocks compute-public-files-path
	  *public-files-path* server-type server-version))

(defvar *weblocks-server* nil
  "If the server is started, bound to hunchentoot server
  object. Otherwise, nil.")

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
  (if debug
      (setf *render-debug-toolbar* t)
      (setf *render-debug-toolbar* nil))
  (when debug
    (setf *show-lisp-errors-p* t)
    (setf *show-lisp-backtraces-p* t))
  (open-stores)
  (when (null *weblocks-server*)
    (setf *session-cookie-name* "weblocks-session")
    (setf *weblocks-server*
	  (apply #'start-server :port port
		 (remove-keyword-parameter
		  (remove-keyword-parameter keys :port) :debug)))))

(defun stop-weblocks ()
  "Stops weblocks. Closes all stores declared via 'defstore'."
  (if (not (null *weblocks-server*))
      (progn
	(reset-sessions)
	(stop-server *weblocks-server*)
	(setf *weblocks-server* nil)))
  (close-stores))

(defun compute-public-files-path (asdf-system-name)
  "Computes the directory of public files. The function uses the
following protocol: it finds the canonical path of the '.asd'
file of the system specified by 'asdf-system-name', goes up one
directory, and goes into 'pub'."
  (merge-pathnames
   (make-pathname :directory '(:relative :up "pub"))
   (asdf-system-directory asdf-system-name)))

(defparameter *public-files-path*
  (compute-public-files-path :weblocks)
  "Must be set to a directory on the filesystem that contains public
files that should be available via the webserver (images, stylesheets,
javascript files, etc.) Modify this directory to set the location of
your files. Points to the weblocks' 'pub' directory by default.")

(setf *dispatch-table*
      (append (list (lambda (request)
		      (funcall (create-folder-dispatcher-and-handler "/pub/" *public-files-path*)
			       request))
		    (create-prefix-dispatcher "/" 'handle-client-request))
	      *dispatch-table*))

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

