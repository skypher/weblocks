
(in-package :weblocks)

(export '(defwebapp start-webapp stop-webapp restart-webapp get-webapp
	  get-webapps-for-class initialize-webapp finalize-webapp
	  webapp-application-dependencies webapp-name
	  webapp-description weblocks-webapp-public-files-path
	  webapp-public-files-uri-prefix webapp-prefix
	  running-webapp make-webapp-uri make-webapp-public-file-uri
          reset-webapp-session
	  webapp-session-value define-permanent-action
	  define-permanent-action/cc remove-webapp-permanent-action
	  compute-webapp-public-files-path
	  compute-webapp-public-files-uri-prefix
	  compute-webapp-public-files-uri-prefix-util
          current-webapp))

(defvar *registered-webapps* nil
  "A list of applications that the system knows about")

(defvar *webapp-permanent-actions*
  (make-hash-table))

(defclass weblocks-webapp ()
  ((name :accessor weblocks-webapp-name :initarg :name
	 :type (or symbol string))
   (description :accessor weblocks-webapp-description :initarg :description 
		:initform nil :type (or null string)
		:documentation "The name of the application.  This slot will be used 
                   by 'page-title' to generate the default title for each page.")
   (public-files-path :initarg :public-files-path 
		      :accessor weblocks-webapp-public-files-path
		      :initform nil
		      :documentation "The filesystem directory path
		      for public files. The final value is computed
		      with 'compute-webapp-public-files-path'.")
   (public-files-uri-prefix :reader weblocks-webapp-public-files-uri-prefix
			    :initform "pub"
			    :initarg :public-files-uri-prefix
			    :documentation "The uri prefix for public
			    files. By default, this slot is
			    initialized to 'pub'. The final uri prefix
			    for application public files is computed
			    by 'compute-webapp-public-files-uri-prefix'.")
   (public-files-cache-time :accessor weblocks-webapp-public-files-cache-time
                            :initform 3600
                            :initarg :public-files-cache-time
                            :type integer
                            :documentation "HTTP cache time for public files in seconds.
                            Caching is automatically disabled in debug mode.")
   (hostnames :reader weblocks-webapp-hostnames :type list
         :initarg :hostnames :initform nil
         :documentation "The hostnames (a list of strings) reserved for this webapp.
         See section 14.32 of RFC 2616.
         
         Example: '(\"foo.com\" \"www.foo.com\" \"shoo.bar.org\")
         
         If NIL (the default), don't care about the hostname at all.
         
         TODO: support regex matching or wildcards here.")
   (prefix :reader weblocks-webapp-prefix :initarg :prefix :type string
	   :documentation "The subtree of the URI space at this site that belongs to
           the webapp.")
   (application-dependencies :accessor weblocks-webapp-application-dependencies 
			     :initarg :dependencies :initform nil :type list
			     :documentation "The public dependencies for all pages rendered by this 
                                application.  The automatic dependencies system will handle all of 
                                the context or request specific dependencies.")
   (init-user-session :accessor weblocks-webapp-init-user-session :initarg :init-user-session
		      :type (or symbol function)
		      :documentation "'init-user-session' must be defined by weblocks client in the
                         same package as 'name'. This function will accept a single parameter - a 
                         composite widget at the root of the application. 'init-user-session' is 
                         responsible for adding initial widgets to this composite.")
   (debug :accessor weblocks-webapp-debug :initarg :debug :initform nil)
   (html-indent-p :accessor weblocks-webapp-html-indent-p :initarg :html-indent-p :initform nil
		  :documentation "Turns on indentation of HTML for easier visual inspection."))
  (:metaclass webapp-class)
  (:documentation 
"A class that encapsulates a unique web application and all relevant rnesources.
A webapp is a unique set of dependencies and information that can be enabled or
disabled independently of others.  Multiple webapps can be active concurrently 
and incoming connections are dispatched to the root of the webapp according to a 
prefix parameter that defines the URLs parsed by that webapp.  The webapp does 
not see the prefix parameter in URLs that are provided to it.  You can, for 
instance, have different sites (e.g. mobile vs. desktop) with vastly different 
layout and dependencies running on the same server."))

;; we use a "transform on write" approach for two reasons:
;;
;; 1. sane values in slots all the time (except when someone messes around
;;    with SLOT-VALUE)
;;
;; 2. increased performance
;;
(defmethod (setf weblocks-webapp-prefix) (prefix (app weblocks-webapp))
  "Set the prefix of the webapp. Ensures normalization."
  (unless (string= prefix "/") ;; XXX multiple slashes?
    (setf (slot-value app 'prefix) (strip-trailing-slashes prefix))))
(defmethod (setf weblocks-webapp-public-files-uri-prefix) (prefix (app weblocks-webapp))
  "Set the public files URI prefix of the webapp. Ensures normalization."
  (setf (slot-value app 'public-files-uri-prefix) (strip-trailing-slashes prefix)))

(defun webapp-public-files-uri-prefix (&optional (app (current-webapp)))
  (weblocks-webapp-public-files-uri-prefix app))

(defmacro defwebapp (name &rest initargs &key 
		     subclasses
		     slots
		     (autostart t) &allow-other-keys)
  "This macro defines the key parameters for a stand alone web application.  
It defines both a class with name 'name' and registers an instance of that class.
It also instantiates a defvar with an instance of this class.  This is intended
to be the primary way a web application is defined.

:subclasses - if you want to inherit subclass behvior from other webapps, you
can.  It's not likely to be needed much

:slots - webapps are class so slots are a list of definitions just as in defclass,
but as slots are likely to be rare on webapps, we make this a keyword argument.

All of the following, when present, are passed through as additional
initargs:

:name - instantiates a username (and the default title for) a webapp.  use this
name to get and delete this webapp.  Multiple instances of a webapp class can
co-exist, so long as they have different prefixes

:description - A description of the application for the title page

:ignore-default-dependencies inhibits appending the default dependencies to
the dependencies list.  By default 'defwebapp' adds the following resources:

  Stylesheets: layout.css, main.css
  Scripts: prototype.js, weblocks.js, scriptaculous.js

:dependencies - is a list of dependencies to append to the default dependencies
list of the application.

:public-files-path - a physical path to the directory which contains
public files for the application. The virtual uri for public files
will be mapped to this physical path by a Hunchentoot handler. If the
static files are server by a different web server, this value may be
set to nil.

:public-files-uri-prefix - a prefix for the virtual uri of public
files (default is 'pub'). Final uri prefix for public files is
computed with 'compute-webapp-public-files-uri-prefix'. Note, the uri
computed by 'compute-webapp-public-files-uri-prefix' is used to
generate dependencies and to set up a Hunchentoot handler to map from
the uri to the path specified by public-files-path.

:public-files-cache-time - make the client cache public files for N
seconds (default 3600). Caching is disabled in debug mode.

:init-user-session - A function object that is used to initialize new
user sessions. If it is not passed, a function named
'init-user-session' is looked up in the package where the web
application name symbol is defined.

:autostart - Whether this webapp is started automatically when start-weblocks is
called (primarily for backward compatibility"
  `(progn
     (defclass ,name (,@subclasses weblocks-webapp)
       ,slots
       (:autostart . ,autostart)
       (:default-initargs
	. ,(remove-keyword-parameters
	    initargs :subclasses :slots :autostart))
       (:metaclass webapp-class))))

(defmethod initialize-instance :after
    ((self weblocks-webapp) &key ignore-default-dependencies &allow-other-keys)
  "Add some defaults to my slots.  In particular, unless
IGNORE-DEFAULT-DEPENDENCIES, prepend the default Weblocks dependencies
to my `application-dependencies' slot."
  (macrolet ((slot-default (name initform)
	       `(unless (slot-boundp self ',name)
		  (setf (slot-value self ',name) ,initform))))
    ;; special handling for prefix slots since initargs
    ;; bypass the normalizing axr
    (when (slot-boundp self 'prefix)
      (setf (weblocks-webapp-prefix self) (slot-value self 'prefix)))
    (when (slot-boundp self 'public-files-uri-prefix)
      (setf (weblocks-webapp-public-files-uri-prefix self)
            (slot-value self 'public-files-uri-prefix)))
    (slot-default html-indent-p (weblocks-webapp-debug self))
    (let ((class-name (class-name (class-of self))))
      (slot-default name (attributize-name class-name))
      (slot-default init-user-session
		    (or (find-symbol (symbol-name 'init-user-session)
				     (symbol-package class-name))
			(error "Cannot initialize application ~A because ~
				no init-user-session function is found."
			       (weblocks-webapp-name self))))
      (slot-default prefix
                      (concatenate 'string "/" (attributize-name class-name))))
    (unless ignore-default-dependencies
      (setf (weblocks-webapp-application-dependencies self)
	    (append '((:stylesheet "layout")
		      (:stylesheet "main")
		      (:stylesheet "dialog")
		      (:script "prototype")
		      (:script "scriptaculous")
		      (:script "shortcut")
		      (:script "weblocks")
		      (:script "dialog"))
		    (weblocks-webapp-application-dependencies self)))))
  (let ((pfp (weblocks-webapp-public-files-path self)))
    (when (and pfp (or (pathname-name pfp) (pathname-type pfp)))
      (warn "~S ~S includes a nondirectory component; this can break file probing"
	    'public-files-path pfp))))

(defun get-webapp (name &optional (error-p t))
  "Get a running web application"
  (let ((app (find (if (symbolp name) (attributize-name name) name)
		   *active-webapps*
		   :key #'weblocks-webapp-name :test #'equal)))
    (if app app
	(when error-p
	  (error "Argument ~a is not a running weblocks application." name)))))

(defun get-webapps-for-class (name)
  (let ((class (or (and (symbolp name) (find-class name nil))
		   (find-class (intern (attributize-name name) :keyword) nil))))
    (when class
      (loop for app in *active-webapps* 
	 when (eq (class-of app) class)
	 collect app))))


(defun check-webapp (name)
  "Ensure that the we have a valid webapp class"
  (unless (find-class name nil)
    (error "~a is not a valid weblocks application class." name)))

(defun start-webapp (class &rest initargs
		     &key (name (attributize-name class)) &allow-other-keys)
  "Starts the web application"
  (check-webapp class)
  (let ((app (get-webapp name nil)))
    (when app
      (warn "An instance of ~A with name ~A is already running, ignoring start request"
	    class name)
      (return-from start-webapp))
    ;; only pass name when truly given
    (setq app (apply #'make-instance class initargs))
    (let ((*default-webapp* app))
      (declare (special *default-webapp*))
      (initialize-webapp app)
      (enable-webapp app)
      app)))

(defun enable-webapp (app)
  "Make sure the app with the \"\" prefix is always the last one and that there
   is only one!"
  (setf *active-webapps*
	(sort (pushnew app *active-webapps*)
	      #'string>
	      :key #'weblocks-webapp-prefix))
  (when (> (count "" (mapcar #'weblocks-webapp-prefix *active-webapps*) :test #'equal) 1)
    (error "Cannot have two defaults dispatchers with prefix \"\"")))

(defgeneric initialize-webapp (app)
  (:documentation "A protocol for performing any special initialization on the creation of a webapp object.")
  (:method ((app t)) nil))

(defmethod initialize-webapp :before ((app weblocks-webapp))
  "Ensure that all registered stores are open"
  (unless weblocks::*weblocks-server*
    (start-weblocks))
  (open-stores))

(defun stop-webapp (name)
  "Stops the web application"
  (let ((app (find-app name)))
    (setf *active-webapps* (delete app *active-webapps*))
    (finalize-webapp app)))

(defgeneric finalize-webapp (app)
  (:documentation "Called when the app has been pulled off the running list to perform any 
   webapp specific cleanup")
  (:method ((app t)) nil))

(defmethod finalize-webapp :after ((app weblocks-webapp))
  "When all webapps are shut down, close any open stores and stop the weblocks server"
  (when (null *active-webapps*)
    (close-stores)
    (setf *last-session* nil)
    (reset-sessions)
    (stop-server *weblocks-server*)
    (setf *weblocks-server* nil)))

(defun find-app (name)
  (let ((app (get-webapp name nil))
	(apps (get-webapps-for-class name)))
    (when (not app)
      (if (eq (length apps) 1)
	  (setf app (first apps))
	  (error "App name ~A not found or no instances of class ~A found" name name)))
    app))

(defun restart-webapp (name)
  (let* ((app (find-app name))
	 (class (class-of app))
	 (name (weblocks-webapp-name app)))
    (stop-webapp name)
    (start-webapp class :name name)))

;;
;; These procedures are relative to the current request's selected webapp
;;

(defvar *current-webapp*)
(setf (documentation '*uri-tokens* 'variable)
      "A currently active web application.")

(defun current-webapp ()
  "Returns the currently invoked instance of a web application."
  (declare (special *current-webapp*))
  *current-webapp*)

(defun reset-webapp-session (&optional (app (current-webapp)))
  "Reset sessions on a per-webapp basis"
  (setf (webapp-session-value app) nil))

(defun webapp-application-dependencies (&optional (app (current-webapp)))
  "Returns a list of dependencies on scripts and/or stylesheets that
   will persist throughout the whole application. See documentation for
   'widget-application-dependencies' for more details."
  (build-local-dependencies
   (weblocks-webapp-application-dependencies app)))

(defun webapp-name (&optional (app (current-webapp)))
  "Returns the name of the web application (also see 'defwebapp'). Please
   note, this name will be used for the composition of the page title
   displayed to the user. See 'page-title' for details."
  (weblocks-webapp-name app))

(defun webapp-description (&optional (app (current-webapp)))
  "Returns the description of the web application. Please note, this
   description will be used for the composition of the page title
   displayed to the user. See 'page-title' for details."
  (weblocks-webapp-description app))

(defun webapp-serves-hostname (hostname &optional (app (current-webapp)))
  "Does APP serve requests for HOSTNAME?"
  (or (null (webapp-hostnames app))
      (member (car (cl-ppcre:split ":" hostname))
              (webapp-hostnames app)
              :test #'equalp)))

(defun webapp-hostnames (&optional (app (current-webapp)))
  "Returns the hostnames this application will serve requests for."
  (weblocks-webapp-hostnames app))

(defun webapp-prefix (&optional (app (current-webapp)))
  "Returns the URL prefix of the application."
  (weblocks-webapp-prefix app))

(defun webapp-init-user-session (&optional (app (current-webapp)))
  "Returns the init function for the user session."
  (let ((init (weblocks-webapp-init-user-session app)))
    (etypecase init
      (function init)
      (symbol (symbol-function init)))))

(defun make-webapp-uri (uri &optional (app (current-webapp)))
  "Makes a URI for a weblocks application (by concatenating the app
prefix and the provided uri)."
  (remove-spurious-slashes
    (concatenate 'string "/" (webapp-prefix app) "/" uri)))

(defun make-webapp-public-file-uri (uri &optional (app (current-webapp)))
  "Makes a URI for a public file for a weblocks application (by
concatenating the app prefix, the public folder prefix, and the
provider URI)."
  (make-webapp-uri
    (concatenate 'string (weblocks-webapp-public-files-uri-prefix app) "/" uri)
    app))

(defun webapp-session-value (symbol &optional (session *session*))
  "Get a session value from the currently running webapp"
  (declare (special *current-webapp*))
  (let ((webapp-session (session-value (class-name (class-of *current-webapp*)) session)))
    (cond (webapp-session
	   (gethash symbol webapp-session))
	  (*current-webapp* (values nil nil))
	  (t nil))))

(defun (setf webapp-session-value) (value symbol)
  "Set a session value for the currently runnin webapp"
  (declare (special *current-webapp*))
  (let ((webapp-session (session-value (class-name (class-of *current-webapp*)))))
    (unless webapp-session
      (setf webapp-session (make-hash-table :test 'equal)
	    (session-value (class-name (class-of *current-webapp*))) webapp-session))
    (setf (gethash symbol webapp-session) value)))


;;
;; Permanent actions
;;

;; NOTES: Should lock-protect this table since users may add actions at runtime

(defun webapp-permanent-action (action)
  "Returns the action function associated with this symbol in the current webapp"
  (declare (special *current-webapp*))
  (when *current-webapp*
    (let ((action-table (webapp-permanent-actions *current-webapp*)))
      (when action-table
	(gethash (if (symbolp action) (symbol-name action) action)
		 action-table)))))

(defun webapp-permanent-actions (webapp)
  (gethash (if (symbolp webapp) webapp
	       (type-of webapp))
	   *webapp-permanent-actions*))

(defun add-webapp-permanent-action (webapp-name action-name function-or-name)
  "Remove an action from a webapp.  action-name should be a string, or it
   will be converted to one (to work with the macro).  function-or-name is
   a symbol or a function object (valid object for funcall)"
  (assert (symbolp webapp-name))
  (assert (or (symbolp action-name) (stringp action-name)))
  (macrolet ((action-table (appname)
	       `(gethash ,appname *webapp-permanent-actions*)))
    (unless (action-table webapp-name)
      (setf (action-table webapp-name) (make-hash-table :test 'equal)))
    (setf (gethash (if (symbolp action-name)
		       (string-downcase (symbol-name action-name))
		       (string-downcase action-name))
		   (action-table webapp-name)) 
	  function-or-name)))
      
(defun remove-webapp-permanent-action (webapp-name action-name)
  "Remove a permanent action from a webapp"
  (macrolet ((action-table (appname)
	       `(gethash ,appname *webapp-permanent-actions*)))
    (let ((table (action-table webapp-name)))
      (when table
	(remhash action-name table)))))

(defmacro define-permanent-action (name webapp-class action-params &body body)
  "Adds a permanent action to the class's set of permanent actions"
  (assert (and (find-class webapp-class)
	       (subtypep webapp-class 'weblocks-webapp)))
  `(add-webapp-permanent-action ',webapp-class ',name
				(lambda ,action-params
				  ,@body)))

(defmacro define-permanent-action/cc (name webapp-class action-params &body body)
  "Adds a permanent action to the class's set of permanent actions"
  (assert (and (find-class webapp-class)
	       (subtypep webapp-class 'weblocks-webapp)))
  `(add-webapp-permanent-action ',webapp-class ',name
				(lambda/cc ,action-params
				  ,@body)))

(let (warned)
  (defgeneric compute-webapp-public-files-path (app)
    (:documentation "If 'weblocks-webapp-public-files-path' is
  nil (default), tries a number of strategies to determine a default
  location of public files. First, an asdf system with the name of the
  application ('weblocks-webapp-name') is searched for. If found, a
  directory 'pub' is searched for in the parent directory of the asdf
  file. If found, this directory is returned as the physical
  value. Otherwise, 'weblocks.asd' is found, and the 'pub' folder in
  that directory is returned.")
    (:method (app)
      (if (weblocks-webapp-public-files-path app)
          (weblocks-webapp-public-files-path app)
          (let ((path (ignore-errors
                        (compute-public-files-path
                         (attributize-name
                          (weblocks-webapp-name app))))))
            (if (and path (probe-file path))
                path
                (progn
                  (and (weblocks-webapp-debug app) (not warned)
                    (setf warned t)
                    (warn "Couldn't determine application's public files folder, using standard Weblocks files."))
                  (compute-public-files-path :weblocks))))))))

(defgeneric compute-webapp-public-files-uri-prefix (app)
  (:documentation "Computes a virtual uri for public files of an
  app. Default implementation concatenates webapp-prefix with the
  public-files-uri-prefix of a webapp.")
  (:method (app)
    (make-webapp-uri
      (weblocks-webapp-public-files-uri-prefix app)
      app)))

(defun compute-webapp-public-files-uri-prefix-util (&optional (app (current-webapp)))
  "A wrapper around 'compute-webapp-public-files-uri-prefix' that
  handles current app."
  (compute-webapp-public-files-uri-prefix app))
