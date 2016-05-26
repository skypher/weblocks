
(in-package :weblocks)

;; make SBCL use and verify type information for slots
(declaim (optimize (safety 3)))

(export '(defwebapp
          weblocks-webapp
          start-webapp
          stop-webapp
          restart-webapp
          get-webapp
          get-webapps-for-class
          initialize-webapp
          finalize-webapp
          webapp-application-dependencies
          webapp-name in-webapp
          bundle-dependency-types
          version-dependency-types
          webapp-description
          *default-public-files-path*
          compute-public-files-path
          weblocks-webapp-public-files-path
          webapp-public-files-path
          webapp-public-files-uri-prefix
          webapp-prefix
          webapp-debug
          running-webapp
          make-webapp-uri
          make-webapp-public-file-uri
          reset-webapp-session
          webapp-session-key
          webapp-session-value
          delete-webapp-session-value
          define-permanent-action
          define-permanent-action/cc
          remove-webapp-permanent-action
          compute-webapp-public-files-path
          compute-webapp-public-files-uri-prefix
          compute-webapp-public-files-uri-prefix-util
          current-webapp
          webapp-update-thread-status
          update-thread-status
          *registered-webapps*
          with-webapp
          weblocks-webapp-default-dependencies 
          weblocks-webapp-js-backend))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *registered-webapps* nil
    "A list of applications that the system knows about"))

(defclass weblocks-webapp ()
  ((name :type (or symbol string)
         :accessor weblocks-webapp-name
         :initarg :name)
   (description :type (or null string)
                :accessor weblocks-webapp-description
                :initarg :description 
                :initform nil 
                :documentation "The name of the application.  This slot will be used 
                   by 'application-page-title' to generate the default title for each page.")
   (js-backend :accessor weblocks-webapp-js-backend 
               :initarg :js-backend
               :initform (error "Please load javascript backend for framework (default is :prototype from https://github.com/html/weblocks-prototype-js) and pass :js-backend value (`:js-backend :prototype` for example) into your `defwebapp`"))
   (public-files-path :type (or null string pathname)
                      :accessor weblocks-webapp-public-files-path
                      :initarg :public-files-path 
                      :initform nil
                      :documentation "The filesystem directory path
                      for public files. The final value is computed
                      with 'compute-webapp-public-files-path'.")
   (public-files-uri-prefix :type string
                            :reader weblocks-webapp-public-files-uri-prefix
                            :initarg :public-files-uri-prefix
                            :initform "pub"
                            :documentation "The uri prefix for public
                            files. By default, this slot is
                            initialized to 'pub'. The final uri prefix
                            for application public files is computed
                            by 'compute-webapp-public-files-uri-prefix'.")
   (public-files-cache-time :type integer
                            :accessor weblocks-webapp-public-files-cache-time
                            :initarg :public-files-cache-time
                            :initform 3600
                            :documentation "HTTP cache time for public files in seconds.
                            When debug is on, caching is turned off.")
   (hostnames :type list
              :reader weblocks-webapp-hostnames
              :initarg :hostnames
              :initform nil
              :documentation "The hostnames (a list of strings) reserved for this webapp.
              See section 14.32 of RFC 2616.
         
              Example: '(\"foo.com\" \"www.foo.com\" \"shoo.bar.org\")
         
              Wildcard patterns are also allowed. Example: '(\"foo.*.com\")
         
              If NIL (the default), don't care about the hostname at all.
         
              TODO: support regex matching here.")
   (prefix :type string
           :reader weblocks-webapp-prefix
           :initarg :prefix
           :documentation "The subtree of the URI space at this site that belongs to
           the webapp.")
   (application-dependencies :type list
                             :accessor weblocks-webapp-application-dependencies 
                             :initarg :dependencies
                             :initform nil
                             :documentation "The public dependencies for all pages rendered by this 
                                application.  The automatic dependencies system will handle all of 
                                the context or request specific dependencies.")
   (bundle-dependency-types :type list
                            :accessor bundle-dependency-types
                            :initarg :bundle-dependency-types
                            ;:initform '(:stylesheet :script)
                            :initform nil ; current bundling code is flaky
                            :documentation "This enables bundling of css, js files.
       If you only want js files to get bundled, set this to '(script-dependency).
       Set it to nil disables bundling. When debug is on, bundling is turned off.
       ATTENTION: If your 'whatever.css' file contains import rules, please take them out,
       put them in a separate file, and name it 'whatever-import.css'. This way all import
       rules will get properly placed in the beginning of the bundled css file.
       ATTENTION: Bundling depends on versioning to detect change in a bundle.
       TIPS:You can also prevent files from being bundled, for example,
       '((stylesheet-dependency filepath-1 filepath-2) script-dependency)
       These two files however, will come after the bundled ones in HTML.")
   (version-dependency-types :type list
                             :accessor version-dependency-types
                             :initarg :version-dependency-types
                             ;:initform '(:stylesheet :script)
                             :initform nil ; versioning does not work well for multiple webapps
                             :documentation "This enables versioning of css, js files. The purpose
       of versioning is to serve modified static files that have been cached permanently, and gziping.
       Anytime you modified the original (unversioned) css or js file, new versioned (maybe gziped)
       file will get created. This way, you only need to work on the unversioned file and not keep
       track of the versioned ones. When debug is t, versioning is turned off.")
   (gzip-dependency-types :type list
                          :accessor gzip-dependency-types
                          :initarg :gzip-dependency-types
                          :initform '(:stylesheet :script)
                          :documentation "This enables gziping of css, js files.
                                          When debug is on, gzip is turned off.")
   (init-user-session :type (or symbol function)
                      :accessor weblocks-webapp-init-user-session
                      :initarg :init-user-session
                      :documentation "'init-user-session' must be defined by weblocks client in the
                         same package as 'name'. This function will accept a single parameter - a 
                         widget at the root of the application. 'init-user-session' is
                         responsible for adding initial children to this widget.")
   (default-store-name
    :accessor webapp-default-store-name :initarg :default-store :type symbol
    :documentation "If non-nil, the name of the `*default-store*'
    bound during request handlers.")
   (session-key :type symbol :accessor weblocks-webapp-session-key :initarg :session-key)
   (debug :accessor weblocks-webapp-debug :initarg :debug :initform nil 
          :documentation "Responsible for debug mode, use WEBAPP-DEBUG function for getting slot value")
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


;;; Make slot readers that will return nil if debug is on
;;; Yet still retain the original values when debug is off
(defmacro def-debug-p-slot-readers (&rest reader-symbols)
  `(progn
     ,@(loop for symbol in reader-symbols
          collect `(defun ,(form-symbol symbol '*) (object)
                     "This reader will prompt debug parameters first."
                     (unless (or *weblocks-global-debug*
                                 (weblocks-webapp-debug object))
                       (,symbol object))))))

(def-debug-p-slot-readers
    weblocks-webapp-public-files-cache-time
    version-dependency-types
    bundle-dependency-types
    gzip-dependency-types)

;; Slash-normalizing accessors
;;
;; We use a "transform on write" approach for two reasons:
;;
;; 1. Sane values in slots all the time (except when someone messes around
;;    with SLOT-VALUE)
;;
;; 2. Increased performance
;;
(defmethod (setf weblocks-webapp-prefix) (prefix (app weblocks-webapp))
  "Set the prefix of the webapp. Ensures normalization."
  (unless (string= prefix "/") ;; XXX multiple slashes?
    (setf (slot-value app 'prefix) (strip-trailing-slashes prefix))))

(defmethod (setf weblocks-webapp-public-files-uri-prefix) (prefix (app weblocks-webapp))
  "Set the public files URI prefix of the webapp. Ensures normalization."
  (setf (slot-value app 'public-files-uri-prefix) (strip-trailing-slashes prefix)))

(defmethod (setf weblocks-webapp-public-files-path) (path (app weblocks-webapp))
  "Set the public files URI prefix of the webapp. Ensures normalization."
  (setf (slot-value app 'public-files-path) (maybe-add-trailing-slash path)))

;; abstraction macro
(defmacro defwebapp (name &rest initargs &key 
                     subclasses
                     slots
                     (autostart t) &allow-other-keys)
  "This macro defines the key parameters for a stand alone web application.  
It defines both a class with name 'name' and registers an instance of that class.
It also instantiates a defvar with an instance of this class.  This is intended
to be the primary way a web application is defined.

:subclasses - if you want to inherit subclass behavior from other webapps, you
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
  Scripts (only for prototype js backend): prototype.js, weblocks.js, scriptaculous.js 

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
       (:metaclass webapp-class))
     (when (get-webapp ',name nil)
       (restart-webapp ',name))))

(defmethod weblocks-webapp-default-dependencies ((self weblocks-webapp))
  '((:stylesheet "layout")
    (:stylesheet "main")
    (:stylesheet "dialog")))

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
    (and (slot-boundp self 'public-files-path)
         (slot-value self 'public-files-path)
      (setf (weblocks-webapp-public-files-path self)
            (slot-value self 'public-files-path)))
    (slot-default default-store-name
                  (webapp-default-store-name (class-of self)))
    (slot-default html-indent-p (weblocks-webapp-debug self))
    (let ((class-name (class-name (class-of self))))
      (slot-default session-key class-name)
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
            (append (weblocks-webapp-default-dependencies self)
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
    (or app
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
  "Starts the web application if it is not started, shows warning in other case.
   Returns app.

     :class - is an application name symbol 
     :initargs - is a list of attributes to pass to application 
     :name - is an application name - a key for an app to be stored. Application then can be found by this key."
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
      (unless (webapp-default-store-name app)
        (style-warn 'missing-default-store :webapp app))
      (enable-webapp app)
      app)))

(defun sort-webapps (webapps) 
  (let* ((webapps-sorted-by-prefix (sort webapps #'string> :key #'weblocks-webapp-prefix))
         (webapps-sorted-by-hostname-and-prefix (stable-sort
                                                  webapps-sorted-by-prefix
                                                  (lambda (x y)
                                                    (and x (null y)))
                                                  :key #'weblocks-webapp-hostnames)))
    webapps-sorted-by-hostname-and-prefix))

(defun enable-webapp (app)
  "Make sure the app with the \"\" prefix is always the last one and that there
   is only one!"
   (let ((webapps (sort-webapps (remove-duplicates (cons app *active-webapps*)))))
     (if (> (count "" (mapcar #'weblocks-webapp-prefix *active-webapps*) :test #'equal) 1)
       (error "Cannot have two defaults dispatchers with prefix \"\"")
       (setf *active-webapps* webapps))))

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
  "Shutdown Weblocks when no more apps are running."
  (when (null *active-webapps*)
    (stop-weblocks)))

(defun find-app (name)
  "Returns registered webapp by its NAME
  Use get-webapp when you need to find an application."
  (let ((app (get-webapp name nil))
        (apps (get-webapps-for-class name)))
    (when (not app)
      (if (eq (length apps) 1)
          (setf app (first apps))
          (error "App name ~A not found or no instances of class ~A found" name name)))
    app))

(defun restart-webapp (name)
  (stop-webapp name)
  (start-webapp name))

;;; building webapp uris
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


;;; webapp-scoped session values
(defun webapp-session-key (&optional (webapp (current-webapp)))
  (weblocks-webapp-session-key webapp))

(defun webapp-session-hash (&optional (session *session*) (webapp (current-webapp)))
  (session-value (webapp-session-key webapp) session))

(defun webapp-session-value (key &optional (session *session*) (webapp (current-webapp)))
  "Get a session value from the currently running webapp.
KEY is compared using EQUAL."
  (let ((webapp-session (webapp-session-hash session webapp )))
    (cond (webapp-session
           (gethash key webapp-session))
          (webapp
           (values nil nil))
          (t
           nil))))

(defun (setf webapp-session-value) (value key &optional (session *session*) (webapp (current-webapp)))
  "Set a session value for the currently running webapp.
KEY is compared using EQUAL."
  (let ((webapp-session (session-value (webapp-session-key webapp) session)))
    (unless webapp-session
      (setf webapp-session (make-hash-table :test #'equal)
            (session-value (webapp-session-key webapp)) webapp-session))
    (setf (gethash key webapp-session) value)))

(defun delete-webapp-session-value (key &optional (session *session*) (webapp (current-webapp)))
  "Clear the session value for the currently running webapp.
KEY is compared using EQUAL."
  (let ((webapp-session (session-value (webapp-session-key) session)))
    (when webapp-session
      (remhash key webapp-session))))

;;
;; Permanent actions
;;

;; FIXME: lock-protect this table since users may add actions at runtime
(defvar *webapp-permanent-actions*
  (make-hash-table))

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

;;; finding public files
(defun compute-public-files-path (asdf-system-name &optional (suffix "pub"))
  "Computes the directory of public files. The function uses the
following protocol: it finds the canonical path of the '.asd' file of
the system specified by 'asdf-system-name', and goes into 'pub'."
  (setf suffix (fad:pathname-as-directory suffix))
  (assert (eq :relative (first (pathname-directory suffix))))
  (merge-pathnames
   suffix
   (asdf-system-directory asdf-system-name)))

(defvar *default-public-files-path* (compute-public-files-path :weblocks))

(let (warned)
  (defgeneric compute-webapp-public-files-path (app)
    (:documentation "If 'weblocks-webapp-public-files-path' is
  nil (default), tries a number of strategies to determine a default
  location of public files. First, an asdf system with the name of the
  application ('weblocks-webapp-name') is searched for. If found, a
  directory 'pub' is searched for in the parent directory of the asdf
  file. If found, this directory is returned as the physical
  value. As a last resort the value of *DEFAULT-PUBLIC-FILES-PATH* is
  used which in turn defaults on the 'pub' folder below the directory
  containing weblocks.asd.")
    (:method (app)
      (if (weblocks-webapp-public-files-path app)
          (weblocks-webapp-public-files-path app)
          (let ((path (ignore-errors
                        (compute-public-files-path
                         (attributize-name
                          (weblocks-webapp-name app))))))
            (if (and path (probe-file path))
              (setf (weblocks-webapp-public-files-path app) path)
                (progn
                  (and (weblocks-webapp-debug app) (not warned)
                    (setf warned t)
                    (warn "Couldn't determine application's public files folder, using standard Weblocks files."))
                  (setf (weblocks-webapp-public-files-path app)
                        *default-public-files-path*))))))))

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


(defgeneric update-thread-status (app status)
  (:documentation "Specialize this function to use some other means
                  to communicate a request's status.

                  The default implementation updates the thread's name
                  (currently only on SBCL).")
  (:method (app (status string))
    #+sbcl(setf (sb-thread:thread-name sb-thread:*current-thread*) status)))

;;; Convenience accessors
;;; These procedures are relative to the current request's selected webapp
(defvar *current-webapp*)
(setf (documentation '*current-webapp* 'variable)
      "A currently active web application.")

(defun current-webapp ()
  "Returns the currently invoked instance of a web application."
  (declare (special *current-webapp*))
  *current-webapp*)

(defun package-webapp-classes (&optional (package *package*))
  "Answer a list of webapp classes that were defined (e.g. by
`defwebapp') in PACKAGE, by default the current package."
  (loop for webapp-class-name in *registered-webapps*
        for class = (find-class webapp-class-name)
        when (eql package (webapp-class-home-package class))
          collect class))

(defun call-in-webapp (app proc)
  "Helper for `in-webapp'."
  (let ((*current-webapp* app))
    (aif (aand (webapp-default-store-name app)
               (symbol-value it))
         (let ((*default-store* it))
           (funcall proc))
         (funcall proc))))

(defmacro with-webapp (webapp &body forms)
  "Bind variables that are both webapp-specific, or applicable to just
this app, and webapp-general, or not particular to some request to
this app, with regard to WEBAPP."
  `(call-in-webapp ,webapp (f0 . ,forms)))

(defun in-webapp (&optional name)
  "Set the current webapp to NAME, or the last webapp registered if NAME is
not supplied. Returns the selected webapp. Convenience function for the REPL."
  (setf *current-webapp*
        (get-webapp (or name
                        (first *registered-webapps*)))))

(defun reset-webapp-session (&optional (app (current-webapp)))
  "Reset sessions on a per-webapp basis"
  (setf (session-value (webapp-session-key app)) nil))

(defun webapp-application-dependencies (&optional (app (current-webapp)))
  "Returns a list of dependencies on scripts and/or stylesheets that
   will persist throughout the whole application. See documentation for
   'widget-application-dependencies' for more details."
  (build-dependencies
   (weblocks-webapp-application-dependencies app)
   app))

(defun webapp-name (&optional (app (current-webapp)))
  "Returns the name of the web application (also see 'defwebapp'). Please
   note, this name will be used for the composition of the page title
   displayed to the user. See 'application-page-title' for details."
  (weblocks-webapp-name app))

(defun webapp-description (&optional (app (current-webapp)))
  "Returns the description of the web application. Please note, this
   description will be used for the composition of the page title
   displayed to the user. See 'application-page-title' for details."
  (weblocks-webapp-description app))

(defun webapp-serves-hostname (hostname &optional (app (current-webapp)))
  "Does APP serve requests for HOSTNAME?"
  (let ((hostname (car (cl-ppcre:split ":" hostname)))) ; ignore port
  (or (null (webapp-hostnames app))
        (member hostname (webapp-hostnames app) :test #'equalp)
        (loop for pattern in (webapp-hostnames app)
              thereis (hostname-match-p pattern hostname)))))

(defun hostname-match-p (pattern hostname)
  (values (cl-ppcre:scan-to-strings
           (cl-ppcre:regex-replace-all "\\*" pattern ".*")
           hostname)))

(defun webapp-hostnames (&optional (app (current-webapp)))
  "Returns the hostnames this application will serve requests for."
  (weblocks-webapp-hostnames app))

(defun webapp-prefix (&optional (app (current-webapp)))
  "Returns the URL prefix of the application."
  (weblocks-webapp-prefix app))

(defun webapp-debug (&optional (app (current-webapp)))
  "Whether APP is in debug mode."
  (weblocks-webapp-debug app))

(defun webapp-public-files-uri-prefix (&optional (app (current-webapp)))
  (weblocks-webapp-public-files-uri-prefix app))

(defun webapp-public-files-path (&optional (app (current-webapp)))
  (weblocks-webapp-public-files-path app))

(defun webapp-init-user-session (&optional (app (current-webapp)))
  "Returns the init function for the user session."
  (let ((init (weblocks-webapp-init-user-session app)))
    (etypecase init
      (function init)
      (symbol (symbol-function init)))))

(defun webapp-update-thread-status (status &optional (app (ignore-errors *current-webapp*)))
  (update-thread-status app status))

