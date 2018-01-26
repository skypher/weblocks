(defpackage #:weblocks/app
  (:use #:cl
        #:f-underscore)
  (:import-from #:metatilities
                #:form-symbol)
  (:import-from #:weblocks/app-mop
                #:get-autostarting-apps
                #:get-registered-apps
                #:app-class
                #:webapp-class-home-package)
  (:import-from #:weblocks/js/base
                #:make-js-backend)
  (:import-from #:weblocks/utils/string
                #:attributize-name
                #:remove-spurious-slashes
                #:strip-trailing-slashes)
  (:import-from #:weblocks/utils/list
                #:remove-keyword-parameters)
  ;; Just dependencies
  ;; Here we need to load jquery js backend, because it is a default
  (:import-from #:weblocks/variables
                #:*current-app*)
  
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
   #:make-uri))
(in-package weblocks/app)
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
   (js-backend :accessor get-js-backend 
               :initarg :js-backend
               :initform :jquery
               :documentation "Specify a javascript backend for
               framework (default is :jquery)")
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
   (session-key :type symbol :accessor weblocks-webapp-session-key :initarg :session-key)
   (debug :accessor weblocks-webapp-debug :initarg :debug :initform nil 
          :documentation "Responsible for debug mode, use WEBAPP-DEBUG function for getting slot value"))
  (:metaclass app-class)
  (:documentation 
   "A class that encapsulates a unique web application and all relevant rnesources.
A webapp is a unique set of dependencies and information that can be enabled or
disabled independently of others.  Multiple webapps can be active concurrently 
and incoming connections are dispatched to the root of the webapp according to a 
prefix parameter that defines the URLs parsed by that webapp.  The webapp does 
not see the prefix parameter in URLs that are provided to it.  You can, for 
instance, have different sites (e.g. mobile vs. desktop) with vastly different 
layout and dependencies running on the same server."))


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
          (strip-trailing-slashes prefix))))


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

:autostart - Whether this webapp is started automatically when start-weblocks is
called (primarily for backward compatibility"
  `(progn
     (defclass ,name (,@subclasses app)
       ,slots
       (:autostart . ,autostart)
       (:default-initargs
        . ,(remove-keyword-parameters
            initargs :subclasses :slots :autostart))
       (:metaclass app-class))
     (when (find-active-app ',name :signal-error nil)
       (restart-webapp ',name))))


(defmethod initialize-instance :after
    ((self app) &rest initargs)
  "Add some defaults to the slots."
  (declare (ignorable initargs))

  ;; Make an instance of js backend class from
  ;; a keyword name
  (setf (get-js-backend self)
        (make-js-backend
         (get-js-backend self)))
  
  
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
      (slot-default name (attributize-name class-name))
      (slot-default prefix
                    (concatenate 'string "/" (attributize-name class-name))))))


(defun find-active-app (name &key (signal-error t))
  "Get a running web application"
  (let ((app (find (if (symbolp name) (attributize-name name) name)
                   *active-apps*
                   :key #'weblocks-webapp-name :test #'equal)))
    (when (and (null app)
               signal-error)
      (error "Argument ~a is not a running weblocks application." name))
    app))


(defun app-active-p (name)
  "Returns t if application of given class is already active."
  (let ((class (or (and (symbolp name) (find-class name nil))
                   (find-class (intern (attributize-name name) :keyword) nil))))
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
              &key (name (attributize-name class)) &allow-other-keys)
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
  (unless (intern "*SERVER*" :weblocks/server)
    (funcall (intern "START-WEBLOCKS" :weblocks/server)))

  ;; TODO: Separate stores into a separate system
  ;;       with dependency from weblocks and make a hook
  ;;       to initialize stores (or anything else) without
  ;;       explicit call in weblocks itself.
  ;;       Probably, stores should have a class-mixin
  ;;       which will define it's own initialize-webapp :before method.
  ;; (open-stores)
  )


(defgeneric finalize-webapp (app)
  (:documentation "Called when the app has been pulled off the running list to perform any 
   webapp specific cleanup")
  (:method ((app t)) nil))


(defmethod finalize-webapp :after ((app app))
  "Shutdown Weblocks when no more apps are running."
  (unless *active-apps*
    ;; TODO: break this tie
    (funcall (intern "STOP-WEBLOCKS" :weblocks/server))))

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


;;; Convenience accessors
;;; These procedures are relative to the current request's selected webapp

(defun package-webapp-classes (&optional (package *package*))
  "Answer a list of webapp classes that were defined (e.g. by
`defwebapp') in PACKAGE, by default the current package."
  (loop for app-class-name in (get-registered-apps)
        for class = (find-class app-class-name)
        when (eql package (webapp-class-home-package class))
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
