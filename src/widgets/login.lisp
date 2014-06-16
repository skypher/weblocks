
(in-package :weblocks)

(export '(do-login authenticatedp on-signout-hooks on-signin-hooks
          logout hash-password *default-login-title*
          *default-login-failure-error* *authentication-key* default-login-view
          email password login login-view login-on-login login-quickform
          authenticated-server-users login-class-store login-data
          anonymous-user-count print-user-stats))

(defparameter *default-login-title* "Please Log In"
  "Default title to be used in 'do-login'.")

(defparameter *authentication-key* 'authentication-object
  "A key used to store the authentication object in the session.")

;;; A wrapper function to quickly present a login dialog
(defun/cc do-login (on-login &key (view 'default-login-view) (title (translate *default-login-title*)))
  (do-dialog title (make-instance 'login :on-login on-login :view view)))

(defun authenticatedp ()
  "Returns nil if the current user is not authenticated. If the user
is authenticated, authentication information stored in the session is
returned."
  (multiple-value-bind (auth-info success)
      (webapp-session-value *authentication-key*)
    (when success auth-info)))

(defmacro on-signout-hooks ()
  "A list of callback functions of no arguments to be called when the user signs out."
  `(webapp-session-value 'on-signout))

(defmacro on-signin-hooks ()
  "A list of callback functions of no arguments to be called when the user signs out."
  `(webapp-session-value 'on-signin))

(defun logout ()
  "Removes any authentication information from the session."
  (setf (webapp-session-value *authentication-key*) nil)
  (dolist (fn (on-signout-hooks))
    (safe-funcall fn))
  (weblocks:reset-webapp-session))

(defun hash-password (password)
  "Returns a one way hash of a plain-text password."
  (md5 (copy-seq password)))

(defparameter *default-login-failure-error* "Invalid credentials."
  "Default message in case of login failure.")

(defview default-login-view (:type form :persistp nil
                             :buttons '((:submit . "Login") :cancel)
                             :caption "Login"
                             :focusp t)
  (email :requiredp t :required-indicator nil)
  (password :requiredp t
            :present-as password
            :required-indicator nil
            :writer (lambda (pwd obj)
                      (setf (slot-value obj 'password)
                            (hash-password pwd)))))

(defwidget login ()
  ((view :accessor login-view
         :initform 'default-login-view
         :initarg :view
         :documentation "A form view containing fields necessary to
         collect credentials information. By default uses
         'default-login-view' which requires an email address and a
         password. Note that default view automatically hashes the
         incoming password.")
   (on-login :accessor login-on-login
             :initform nil
             :initarg :on-login
             :documentation "A function that must accept two
             parameters (a login widget object, and a CLOS object that
             contains information obtained from the user), and return
             an authentication object if login is successful, or nil
             if login failed. If the function returns a non-nil value,
             it will be stored in the session under
             *authentication-key*. If the function returns nil, it may
             return a string as a second value that will be used as an
             error message. Otherwise, *default-login-failure-error*
             will be used. Note, the object passed to the specified
             function is generated from the view provided in the
             'login-view' slot.  By default, it has email and
             password slots.  The password is hashed using md5 by
             the publically accessible hash-password function. This
             should not be considered particularily secure, as it is
             not salted.  If real security is desired, override the
             login-view to provide your own password handling,
             probably by passing it unchanged to the on-login
             function for serious processing there.")
   (quickform :accessor login-quickform
              :initform nil
              :initarg :quickform
              :documentation "A quickform widget used by the login to
              provide UI.")
   (data :reader login-data
         :initform nil
         :initarg :data
         :documentation "A data object to hold the registration
         information. Must match the view if provied. If not
         provided, a CLOS object will be generated holding the values
         of the input fields in the view in the corresponding slots.")
   (class-store :reader login-class-store
                :initform nil
                :initarg :class-store
                :documentation "Store where user data is being kept.")
   (on-success :accessor login-on-success
               :initarg :on-success
               :initform nil
               :documentation
                "A function that accepts two paramets: a registration widget and
                a CLOS object that contains information obtained from the user.
                It is called when the registration was successful. ")

   (on-cancel :accessor login-on-cancel
              :initarg :on-cancel
              :initform nil
              :documentation
                "A function that accepts one parameter a registration widget.
                It is called when the registration was cancelled. "))
  (:documentation "A widget that provides basic login
  functionality. Based on a view to be rendered (or a default of email
  and password), and an authentication function, provides the UI for
  login, and stores login information the session on success."))

(defmethod initialize-instance :after ((obj login) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (login-quickform obj)
        (make-quickform (login-view obj)
                        :on-success (lambda (w o)
                                      (declare (ignore w))
                                      (safe-funcall (login-on-success obj) obj o)
                                      (answer obj (authenticatedp)))
                        :on-cancel (lambda (w)
                                     (declare (ignore w))
                                     (safe-funcall (login-on-cancel obj) obj)
                                     (answer obj))
                        :satisfies (lambda (w o)
                                     (declare (ignore w))
                                     (multiple-value-bind (success error)
                                         (funcall (login-on-login obj) obj o)
                                       (if success
                                           (prog1 ; make sure we return the proper value, or validation will fail
                                             (setf (webapp-session-value *authentication-key*) success)
                                             (dolist (fn (on-signin-hooks))
                                               (safe-funcall fn)))
                                           (values nil
                                                   (list
                                                    (cons nil (or error (translate *default-login-failure-error*))))))))
                        :answerp nil
                        :data (login-data obj)
                        :class-store (login-class-store obj))))

(defmethod render-widget-body ((obj login) &rest args)
  (declare (ignore args))
  (render-widget (login-quickform obj)))

(defun authenticated-server-users ()
  "Returns authentication information found in each session on the
server. Sessions without authentication information are ignored."
  (unique-elements (remove nil
                           (let ((lst '()))
                             (dolist (webapp *active-webapps*)
                               (declare (special *current-webapp*))
                               (setf *current-webapp* webapp)
                               (mapcar (lambda (session)
                                         (push (car (multiple-value-list (webapp-session-value *authentication-key* session))) lst))
                                       (active-sessions)))
                             lst))))

(defun anonymous-user-count ()
  "Returns the number of anonymous users on the server."
  (- (length (active-sessions))
     (length (authenticated-server-users))))

(defun print-user-stats (&optional (stream t))
  "Prints user statistics to an optionally specified stream. If the
stream isn't specified, prints to standard output."
  (format stream "Total Users On Server: ~A (Authenticated: ~A, Anonymous: ~A)~%" 
          (length (active-sessions))
          (length (authenticated-server-users))
          (anonymous-user-count)))
