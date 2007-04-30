
(in-package :weblocks)

(export '(start-weblocks stop-weblocks defwebapp))

(defvar *weblocks-server* nil
  "If the server is started, bound to hunchentoot server
  object. Otherwise, nil.")

(defun start-weblocks (&key debug)
  "Starts weblocks framework hooked into Hunchentoot server. Set
':debug' keyword to true in order for stacktraces to be shown to
the client."
  (when debug
    (setf *show-lisp-errors-p* t)
    (setf *show-lisp-backtraces-p* t))
  (when (null *weblocks-server*)
    (setf *session-cookie-name* "weblocks-session")
    (setf *weblocks-server* (start-server :port 8080))))

(defun stop-weblocks ()
  "Stops weblocks."
  (if (not (null *weblocks-server*))
      (progn
	(stop-server *weblocks-server*)
	(setf *weblocks-server* nil))))

(defparameter *stylesheet-directory*
  (if (equal (machine-type) "PowerPC")
	     "/Users/coffeemug/projects/cl-weblocks/pub/"
	     "/home/coffeemug/projects/weblocks2/pub/"))

(setf *dispatch-table*
      (append (list (create-folder-dispatcher-and-handler "/pub/" *stylesheet-directory*)
		    (create-prefix-dispatcher "/" 'handle-client-request))
	      *dispatch-table*))

(defvar *webapp-name* nil
  "The name of the currently running web application. See
'defwebapp' for more details.")

(defun defwebapp (name)
  "Sets the application name (the *webapp-name* variable). 'name'
must be a symbol. This symbol will later be used to find a
package that defined 'init-user-session' - a function responsible
for the web application setup.

'init-user-session' must be defined by weblocks client in the
same package as 'name'. This function will accept a single
parameter - a composite widget at the root of the
application. 'init-user-session' is responsible for adding
initial widgets to this composite."
  (check-type name symbol)
  (setf *webapp-name* name))
