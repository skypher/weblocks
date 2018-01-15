(defpackage #:weblocks.app
  (:use #:cl
        #:f-underscore)
  (:import-from #:weblocks.app-mop
                #:get-autostarting-apps
                #:get-registered-apps)
  (:export
   #:defapp
   #:app
   #:get-autostarting-apps
   #:get-registered-apps
   #:app-active-p
   #:start
   #:find-active-app
   #:get-active-apps
   #:get-prefix
   #:app-serves-hostname-p
   #:get-current
   #:with-app
   #:get-action
   #:add-action
   #:remove-action
   #:define-action
   #:define-action/cc))
(in-package weblocks.app)
;; (in-package weblocks)

;; (export '(defwebapp
;;           weblocks-webapp
;;           start-webapp
;;           stop-webapp
;;           restart-webapp
;;           get-webapp
;;           get-webapps-for-class
;;           initialize-webapp
;;           finalize-webapp
;;           webapp-name in-webapp
;;           bundle-dependency-types
;;           version-dependency-types
;;           webapp-description
;;           webapp-prefix
;;           webapp-debug
;;           running-webapp
;;           make-webapp-uri
;;           make-webapp-public-file-uri
;;           reset-webapp-session
;;           webapp-session-key
;;           define-permanent-action
;;           define-permanent-action/cc
;;           remove-webapp-permanent-action
;;           *registered-webapps*
;;           with-webapp
;;           weblocks-webapp-default-dependencies 
;;           ;; weblocks-webapp-js-backend
;;           initialize-js-backend
;;           ;; get-js-backend-dependencies
;;           ))

;; TODO: dont understand why this defvar is surrounded by eval-when
;; MOVED TO weblocks.app-mop
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defvar *registered-webapps* nil
;;     "A list of applications that the system knows about"))


(serapeum:defvar-unbound
    *current-app*
  "A currently active web application.")


(defvar *active-apps* nil
  "A list of running applications.
   Applications are added to this list after they have been started.")


(defclass app ()
  ((name :type (or symbol string)
         :accessor weblocks-webapp-name
         :initarg :name)
   (description :type (or null string)
                :accessor weblocks-webapp-description
                :initarg :description 
                :initform nil 
                :documentation "The name of the application.  This slot will be used 
                   by 'application-page-title' to generate the default title for each page.")
   (js-backend :accessor webapp-js-backend 
               :initarg :js-backend
               :initform :jquery
               :documentation "Specify a javascript backend for
               framework (default is :jquery-js)")
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
           :reader get-prefix
           :initarg :prefix
           :documentation "The subtree of the URI space at this site that belongs to
           the webapp.")
   ;; TODO: may be remove
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
   (session-key :type symbol :accessor weblocks-webapp-session-key :initarg :session-key)
   (debug :accessor weblocks-webapp-debug :initarg :debug :initform nil 
          :documentation "Responsible for debug mode, use WEBAPP-DEBUG function for getting slot value"))
  (:metaclass weblocks.app-mop::app-class)
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
          collect `(defun ,(weblocks::form-symbol symbol '*) (object)
                     "This reader will prompt debug parameters first."
                     (unless (or *weblocks-global-debug*
                                 (weblocks-webapp-debug object))
                       (,symbol object))))))

(def-debug-p-slot-readers
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
(defmethod (setf get-prefix) (prefix (app app))
  "Set the prefix of the webapp. Ensures normalization by removing trailing slash."
  (unless (string= prefix "/") ;; XXX multiple slashes?
    (setf (slot-value app 'prefix)
          (weblocks::strip-trailing-slashes prefix))))


;; abstraction macro
(defmacro defapp (name
                  &rest initargs
                  &key 
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

:autostart - Whether this webapp is started automatically when start-weblocks is
called (primarily for backward compatibility"
  `(progn
     (defclass ,name (,@subclasses app)
       ,slots
       (:autostart . ,autostart)
       (:default-initargs
        . ,(weblocks::remove-keyword-parameters
            initargs :subclasses :slots :autostart))
       (:metaclass weblocks.app-mop:app-class))
     (when (find-active-app ',name :signal-error nil)
       (restart-webapp ',name))))


(defmethod initialize-instance :after
    ((self app) &key ignore-default-dependencies &allow-other-keys)
  "Add some defaults to my slots.  In particular, unless
IGNORE-DEFAULT-DEPENDENCIES, prepend the default Weblocks dependencies
to my `application-dependencies' slot."

  ;; Make an instance of js backend class from
  ;; a keyword name
  (setf (webapp-js-backend self)
        (weblocks.js:make-js-backend
         (webapp-js-backend self)))
  
  
  ;; TODO: refactor this mess
  (macrolet ((slot-default (name initform)
               `(unless (slot-boundp self ',name)
                  (setf (slot-value self ',name) ,initform))))
    ;; special handling for prefix slots since initargs
    ;; bypass the normalizing axr
    (when (slot-boundp self 'prefix)
      (setf (get-prefix self)
            (slot-value self 'prefix)))

    (let ((class-name (class-name (class-of self))))
      (slot-default session-key class-name)
      (slot-default name (weblocks::attributize-name class-name))
      (slot-default prefix
                    (concatenate 'string "/" (weblocks::attributize-name class-name))))
    (unless ignore-default-dependencies
      ;; Replaced with new king of dependencies
      ;;
      ;; (let ((*current-app* self))
      ;;   (setf (weblocks-webapp-application-dependencies self)
      ;;         (build-dependencies
      ;;          (append
      ;;           ;; Backend dependencies should go first, to
      ;;           ;; load jquery or prototype before all other code
      ;;           ;; will be loaded.
      ;;           (get-js-backend-dependencies
      ;;            self
      ;;            (weblocks-webapp-js-backend self))
      ;;           ;; After that, we load dependencies of a class
      ;;           (weblocks-webapp-default-dependencies self)
      ;;           ;; And finally, we add them to existing application
      ;;           ;; dependencies.
      ;;           (weblocks-webapp-application-dependencies self)))))
      ))
  
)

(defun find-active-app (name &key (signal-error t))
  "Get a running web application"
  (let ((app (find (if (symbolp name) (weblocks::attributize-name name) name)
                   *active-apps*
                   :key #'weblocks-webapp-name :test #'equal)))
    (when (and (null app)
               signal-error)
      (error "Argument ~a is not a running weblocks application." name))
    app))


(defun app-active-p (name)
  "Returns t if application of given class is already active."
  (let ((class (or (and (symbolp name) (find-class name nil))
                   (find-class (intern (weblocks::attributize-name name) :keyword) nil))))
    (when class
      (loop for app in *active-apps*
            when (eq (class-of app)
                     class)
              do (return-from app-active-p t)))))


(defun check-if-valid-class-name (name)
  "Ensure that the we have a valid webapp class"
  (unless (find-class name nil)
    (error "~a is not a valid weblocks application class." name)))

(defun sort-webapps (webapps) 
  (let* ((webapps-sorted-by-prefix (sort webapps #'string> :key #'get-prefix))
         (webapps-sorted-by-hostname-and-prefix (stable-sort
                                                  webapps-sorted-by-prefix
                                                  (lambda (x y)
                                                    (and x (null y)))
                                                  :key #'weblocks-webapp-hostnames)))
    webapps-sorted-by-hostname-and-prefix))

(defun enable-webapp (app)
  "Make sure the app with the \"\" prefix is always the last one and that there
   is only one!"
  (log:debug "Enabling webapp" app)
  
  (let ((webapps (sort-webapps (remove-duplicates (cons app *active-apps*)))))
    (if (> (count "" (mapcar #'get-prefix *active-apps*) :test #'equal) 1)
        (error "Cannot have two defaults dispatchers with prefix \"\"")
        (setf *active-apps* webapps))))

(defgeneric initialize-webapp (app)
  (:documentation "A protocol for performing any special initialization on the creation of a webapp object.")
  (:method ((app t)) nil))


(defun start (class &rest initargs
              &key (name (weblocks::attributize-name class)) &allow-other-keys)
  "Starts the web application if it is not started, shows warning in other case.
   Returns app.

     :class - is an application name symbol 
     :initargs - is a list of attributes to pass to application 
     :name - is an application name - a key for an app to be stored. Application then can be found by this key."
  (log:debug "Starting webapp" class initargs name)
  
  (check-if-valid-class-name class)
  
  (let ((app (find-active-app name :signal-error nil)))
    (cond
      (app
       (warn "An instance of ~A with name ~A is already running, ignoring start request"
             class name)
       app)
      (t
       (let ((app (apply #'make-instance class initargs)))
         (initialize-webapp app)
         (enable-webapp app)
         app)))))


(defmethod initialize-webapp :before ((app app))
  "Ensure that all registered stores are open"

  ;; to not introduce circular dependency we have these intern calls
  (unless (intern "*SERVER*" :weblocks.server)
    (funcall (intern "START-WEBLOCKS" :weblocks.server)))

  ;; TODO: Separate stores into a separate system
  ;;       with dependency from weblocks and make a hook
  ;;       to initialize stores (or anything else) without
  ;;       explicit call in weblocks itself.
  ;;       Probably, stores should have a class-mixin
  ;;       which will define it's own initialize-webapp :before method.
  ;; (open-stores)
  )


(defmethod weblocks.dependencies:get-dependencies ((self app))
  (log:debug "Returning new-style dependencies for base application class.")
  
  (weblocks.dependencies:get-dependencies
   (webapp-js-backend self)))


(defgeneric finalize-webapp (app)
  (:documentation "Called when the app has been pulled off the running list to perform any 
   webapp specific cleanup")
  (:method ((app t)) nil))


(defmethod finalize-webapp :after ((app app))
  "Shutdown Weblocks when no more apps are running."
  (unless *active-apps*
    ;; TODO: break this tie
    (funcall (intern "STOP-WEBLOCKS" :weblocks.server))))

(defun stop-webapp (name)
  "Stops the web application"
  (log:debug "Stopping webapp" name)
  
  (let ((app (find-active-app name :signal-error nil)))
    (when app
      (setf *active-apps*
            (remove app *active-apps*))
      (finalize-webapp app))))


(defun restart-webapp (name)
  (stop-webapp name)
  (start-webapp name))

;;; building webapp uris
(defun make-webapp-uri (uri &optional (app *current-app*))
  "Makes a URI for a weblocks application (by concatenating the app
prefix and the provided uri)."
  (weblocks::remove-spurious-slashes
    (concatenate 'string "/" (weblocks::webapp-prefix app) "/" uri)))


;;
;; Permanent actions
;;

;; TODO: lock-protect this table since users may add actions at runtime
(defvar *apps-actions*
  (make-hash-table)
  "This hash maps app classes to hashes which store application's actions.")

(defun get-app-actions (app)
  (gethash (if (symbolp app)
               app
               (type-of app))
           *apps-actions*))

(defun get-action (action)
  "Returns the action function associated with this symbol in the current app"
  (when (boundp '*current-app*)
    (let ((action-table (get-app-actions *current-app*)))
      (when action-table
        (gethash (if (symbolp action)
                     (symbol-name action)
                     action)
                 action-table)))))

(defun add-action (app-class action-name function-or-name)
  "Remove an action from a webapp.  action-name should be a string, or it
   will be converted to one (to work with the macro).  function-or-name is
   a symbol or a function object (valid object for funcall)"
  (check-type app-class symbol)
  (check-type action-name (or symbol string))
  (macrolet ((action-table (appname)
               `(gethash ,appname *apps-actions*)))
    (unless (action-table app-class)
      (setf (action-table app-class)
            (make-hash-table :test 'equal)))
    (setf (gethash (string-downcase
                    (if (symbolp action-name)
                        (symbol-name action-name)
                        action-name))
                   (action-table app-class)) 
          function-or-name)))
      
(defun remove-action (app-class action-name)
  "Removes a permanent action from a webapp"
  (check-type app-class symbol)
  (check-type action-name (or symbol string))
  
  (let ((table (gethash app-class *apps-actions*)))
    (when table
      (remhash action-name table))))


(defmacro define-action (name app-class action-params &body body)
  "Adds a permanent action to the class's set of permanent actions"
  (assert (and (find-class app-class)
               (subtypep app-class 'app)))
  `(add-action ',app-class ',name
               (lambda ,action-params
                 ,@body)))

(defmacro define-action/cc (name app-class action-params &body body)
  "Adds a permanent action to the class's set of permanent actions"
  (assert (and (find-class app-class)
               (subtypep app-class 'app)))
  `(add-action ',app-class ',name
               (lambda/cc ,action-params
                 ,@body)))


;;; Convenience accessors
;;; These procedures are relative to the current request's selected webapp

(defun package-webapp-classes (&optional (package *package*))
  "Answer a list of webapp classes that were defined (e.g. by
`defwebapp') in PACKAGE, by default the current package."
  (loop for app-class-name in (get-registered-apps)
        for class = (find-class app-class-name)
        when (eql package (weblocks::webapp-class-home-package class))
          collect class))


(defun call-in-webapp (app proc)
  "Helper for `in-webapp'."
  (let ((*current-app* app))
    (funcall proc)))


(defmacro with-app (app &body forms)
  "Bind variables that are both webapp-specific, or applicable to just
this app, and webapp-general, or not particular to some request to
this app, with regard to WEBAPP."
  `(call-in-webapp ,app (f0 . ,forms)))


(defun webapp-name (&optional (app *current-app*))
  "Returns the name of the web application (also see 'defwebapp'). Please
   note, this name will be used for the composition of the page title
   displayed to the user. See 'application-page-title' for details."
  (weblocks-webapp-name app))

(defun webapp-description (&optional (app *current-app*))
  "Returns the description of the web application. Please note, this
   description will be used for the composition of the page title
   displayed to the user. See 'application-page-title' for details."
  (weblocks-webapp-description app))

(defun webapp-hostnames (&optional (app *current-app*))
  "Returns the hostnames this application will serve requests for."
  (weblocks-webapp-hostnames app))


(defun hostname-match-p (pattern hostname)
  (values (cl-ppcre:scan-to-strings
           (cl-ppcre:regex-replace-all "\\*" pattern ".*")
           hostname)))

(defun app-serves-hostname-p (app hostname)
  "Does APP serve requests for HOSTNAME?"
  (let ((hostname (car (cl-ppcre:split ":" hostname)))) ; ignore port
  (or (null (webapp-hostnames app))
        (member hostname (webapp-hostnames app) :test #'equalp)
        (loop for pattern in (webapp-hostnames app)
              thereis (hostname-match-p pattern hostname)))))


(defun webapp-debug (&optional (app *current-app*))
  "Whether APP is in debug mode."
  (weblocks-webapp-debug app))


(defun get-current ()
  *current-app*)

(defun get-active-apps ()
  *active-apps*)
