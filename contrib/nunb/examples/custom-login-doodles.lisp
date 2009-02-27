(in-package :app)

; called like this: (do-custom-login #'force-login-user :view 'a-login-view)

;;; A wrapper function to quickly present a login dialog
(defun/cc do-custom-login (on-login &key (view 'default-login-view) (title *default-login-title*))
  (do-dialog title (make-instance 'custom-login :on-login on-login :view view)))

(defun user-satisfies (u)
  (if (text-input-present-p u)
    t
    (values nil #!"login username blank")))

(defun password-satisfies (u)
  (if (text-input-present-p u)
    t
    (values nil #!"login password blank")))

(defwidget custom-quickform (quickform)
  ((custom-composite :accessor base-composite-of :initform nil))
  (:documentation "Enhance basic login with lagru-form. Add a custom error message and a forgot password link."))

(defwidget custom-login (login)
  ((custom-composite :accessor base-composite-of :initform nil)
   (second-attempt :accessor second-attempt-of  :initform nil))
  (:documentation "Enhance basic login with lagru-form. Add a custom error message and a forgot password link."))

(defun login-view-satisfier (login-obj
			    &rest args
			    &key  email email-field password password-field
			    &allow-other-keys)
  (let ((error-messages nil))
    (cond
      ((or (null email)
	   (string= email "")
	   (string= email "foo"))
       (push
	(cons email-field "Nome utente errato")
	error-messages))
      ((or (null password)
	   (string= password ""))
       (push
	(cons password-field "Password errato")
	error-messages))
      ((and (string= email "nb")
	    (string= password "nb"))
       t)
      (t
       (let ((check-db))
	 (multiple-value-bind (result error)
	     (find-normal-user email (hash-password password))
	   (warn (format nil "Result of f-l-u from l-v-s ~%~% ~A . ~A" result error))
	   (when (member result '(:DEACTIVATED :PASSWORD :NOUSER))
	     (push (cons nil "") error-messages)
	     (cond
	       ((eql result :DEACTIVATED)
		(push
		 (cons email-field error)
		 error-messages))
	       ((eql result :NOUSER)
		(push
		 (cons email-field error)
		 error-messages))
		 ((eql result :PASSWORD)
		  (push
		   (cons password-field error)
		   error-messages))))))))
     (if  error-messages
	 (values nil error-messages)
	 t)))

(defview a-login-view (:type templform 
			     :persistp nil 
			     :file "login.view"
			     :buttons '((:submit . "Entra") (:cancel . "Annulla"))
			     :caption #!"LoginCaption"
			     :focusp t
			     :satisfies #'login-view-satisfier)
  (email ;:requiredp t
         :label #!"Email"
	 ;:satisfies #'user-satisfies
	 )
  (password 
	    :label #!"Password"
	    :present-as password	    
	    ;:satisfies #'password-satisfies
	    :writer (lambda (pwd obj)
		      (setf (slot-value obj 'password)
			    (hash-password pwd)))))
(defmacro qf-maker ()
             `(make-quickform (login-view obj)
			:class 'custom-quickform
			:on-success (lambda (w o)
				      (declare (ignore w))
                                      (safe-funcall (weblocks::login-on-success obj) obj o)
				      (answer obj (authenticatedp)))
			:on-cancel (lambda (w)
				     (declare (ignore w))
                                     (safe-funcall (weblocks::login-on-cancel obj) obj)
				     (answer obj)) ;answers with a nil. hence (the-user) etc. fail -- they bork on nils
			:satisfies (lambda (w o)
				     (declare (ignore w))
				     (multiple-value-bind (success error)
					 (funcall (login-on-login obj) obj o)
				       (if success
					   (setf (webapp-session-value *authentication-key*) success)
					   (values nil
						   (list
						    (cons nil (or error *default-login-failure-error*)))))))
			:answerp nil
                        :data (login-data obj)
                        :class-store (login-class-store obj)))


(defmethod initialize-instance :after ((obj custom-login) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (login-quickform obj)
	(qf-maker))
  (setf (base-composite-of obj)
	(make-instance 'composite :widgets (list
					    ; (make-instance 'simple-widget :render-fn (lambda (me) (with-html (:p "yeehaw" ))))
					     (login-quickform obj))))
  (setf (base-composite-of (login-quickform obj))
	(base-composite-of obj)))


(defmethod render-validation-summary ((view (eql (find-view 'a-login-view))) obj widget errors)
   (declare (ignore view))
   ;(warn "me r")
    (when errors
      (let ((non-field-errors (find-all errors #'null :key #'car))
	    (field-errors (find-all errors (compose #'not #'null) :key #'car))
	    (qf-widget-with-parent widget))
	(with-html
	  (:div :class "login-validation-errors-summary"
		(when non-field-errors
		  (htm
		   (:ul :class "non-field-validation-errors"
			(mapc (lambda (err)
				(with-html
				  (:li
				   (str (format nil "~A" (cdr err))))))
			      non-field-errors))
		   ;simple-password forget.
		   #+old(render-link-proper (closure (redirect (format nil "/forgot/~A" (slot-value obj 'email))))
				       #!"(forgot password?)"  :class "forgot-link")
		   ;ooh too fancy for the simple one, are we?
		   (render-link-proper
		     (closure
		       (let ((tl (make-instance 'tmplogin)))
			 (setf (slot-value qf-widget-with-parent 'weblocks::validation-errors) nil)
			 (with-flow qf-widget-with-parent ;what is the value of this var here? the actual qf or the parent?
			   (when (eq :fresh (yield (make-forgotten-password qf-widget-with-parent tl)))
			     (yield (make-quickform (weblocks::defview-anon (:type templform :file "password.sent" :persistp nil  :caption #!"PasswordSent" :buttons '((:submit . "Va bene")))
							(email :present-as (paragraph)))
					;:on-success (lambda (w o) (answer w))
					:data tl))))))
		     #!"(forgot password?)"))))))))

;; This works pretty well, but fails when the user makes mistakes twice in a row.
;; 		   (render-link-proper
;; 		     (closure
;; 		       (with-flow qf-widget-with-parent
;; 			 (when (eq :fresh (yield (make-forgotten-password qf-widget-with-parent)))
;; 			   (yield (second-attempt-of qf-widget-with-parent)))))
;; 		     #!"(forgot password?)")


(defun make-forgotten-password (qfp tmpemailsave)
  "Make a dataform widget that supports getting your password emailed to you. This makes a QF but disables the auto answering, as we would like to decide which widget to answer ourselves. If success, then we should send the email, possibly show an updating message, and return to a new (unborked) login dialog -- ie we cannot return to the original (that's why we have the qf-maker macro, it makes us a fresh identical QF. If it is cancel, we assume that means the user wants to try again, so we don't mind returning him to the original login form he just botched. Of course, cancel and enter on *that* original/botched form continue to work as expected. Finally, if we give the user a new unborked dialong, and he goes and also borks *that* then he's an idiot and we don't care if we return him to the original borked dialog -- he may have made different bork-causing mistakes on each, but we don't care if we confuse him by sending him back to his original bork."
  (make-quickform
                (weblocks::defview-anon
				(:type templform 
				       :persistp nil 
				       :file "forgot.password.view"
				       :buttons '((:submit . "Invio") (:cancel . "Annulla"))
				       :caption #!"SendPassword"
				       :focusp t)
				(email :satisfies (make-satisfier #'valid-email-p "Email non valida")))
		 :data tmpemailsave
		 :on-success (lambda (w o)
				      ;(declare (ignore w))
                                      (warn "new password shiznit happens")
				      (answer w :fresh))
		 :on-cancel (lambda (w)
				     ;(declare (ignore w))
                                     (warn "cancelling")
				     (answer w :rotten))
		 :answerp nil))


(defmethod render-widget-body ((obj custom-login) &rest args)
  (declare (ignore args))
  (render-widget (base-composite-of obj)))

;; (defun authenticated-server-users ()
;;   "Returns authentication information found in each session on the
;; server. Sessions without authentication information are ignored."
;;   (unique-elements (remove nil
;;                            (let ((lst '()))
;;                              (dolist (webapp *active-webapps*)
;;                                (declare (special *current-webapp*))
;;                                (setf *current-webapp* webapp)
;;                                (mapcar (lambda (session)
;;                                          (push (car (multiple-value-list (webapp-session-value *authentication-key* session))) lst))
;;                                        (active-sessions)))
;;                              lst))))

;; (defun anonymous-user-count ()
;;   "Returns the number of anonymous users on the server."
;;   (- (length (active-sessions))
;;      (length (authenticated-server-users))))

;; (defun print-user-stats (&optional (stream t))
;;   "Prints user statistics to an optionally specified stream. If the
;; stream isn't specified, prints to standard output."
;;   (format stream "Total Users On Server: ~A (Authenticated: ~A, Anonymous: ~A)~%" 
;; 	  (length (active-sessions))
;; 	  (length (authenticated-server-users))
;; 	  (anonymous-user-count)))


;; (view :accessor login-view
;; 	 :initform 'default-login-view
;; 	 :initarg :view
;; 	 :documentation "A form view containing fields necessary to
;; 	 collect credentials information. By default uses
;; 	 'default-login-view' which requires an email address and a
;; 	 password. Note that default view automatically hashes the
;; 	 incoming password.")
;;    (on-login :accessor login-on-login
;; 	     :initform nil
;; 	     :initarg :on-login
;; 	     :documentation "A function that must accept two
;; 	     parameters (a login widget object, and a CLOS object that
;; 	     contains information obtained from the user), and return
;; 	     an authentication object if login is successful, or nil
;; 	     if login failed. If the function returns a non-nil value,
;; 	     it will be stored in the session under
;; 	     *authentication-key*. If the function returns nil, it may
;; 	     return a string as a second value that will be used as an
;; 	     error message. Otherwise, *default-login-failure-error*
;; 	     will be used. Note, the object passed to the specified
;; 	     function is generated from the view provided in the
;; 	     'login-view' slot.  By default, it has email and
;; 	     password slots.  The password is hashed using md5 by
;; 	     the publically accessible hash-password function. This
;; 	     should not be considered particularily secure, as it is
;; 	     not salted.  If real security is desired, override the
;; 	     login-view to provide your own password handling,
;; 	     probably by passing it unchanged to the on-login
;; 	     function for serious processing there.")
;;    (quickform :accessor login-quickform
;; 	      :initform nil
;; 	      :initarg :quickform
;; 	      :documentation "A quickform widget used by the login to
;; 	      provide UI.")
;;    (data :reader login-data
;;          :initform nil
;;          :initarg :data
;;          :documentation "A data object to hold the registration
;;          information. Must match the view if provied. If not
;;          provided, a CLOS object will be generated holding the values
;;          of the input fields in the view in the corresponding slots.")
;;    (class-store :reader login-class-store
;;                 :initform nil
;;                 :initarg :class-store
;;                 :documentation "Store where user data is being kept.")
;;    (on-success :accessor login-on-success
;;                :initarg :on-success
;;                :initform nil
;;                :documentation
;;      		"A function that accepts two paramets: a registration widget and
;; 		a CLOS object that contains information obtained from the user.
;; 		It is called when the registration was successful. ")

;;    (on-cancel :accessor login-on-cancel
;;               :initarg :on-cancel
;;               :initform nil
;;               :documentation
;;      		"A function that accepts one paramets: a registration widget.
;; 		It is called when the registration was cancelled. ")


;; (defun authenticatedp ()
;;   "Returns nil if the current user is not authenticated. If the user
;; is authenticated, authentication information stored in the session is
;; returned."
;;   (multiple-value-bind (auth-info success)
;;       (webapp-session-value *authentication-key*)
;;     (when success auth-info)))

;; (defun logout ()
;;   "Removes any authentication information from the session."
;;   (setf (webapp-session-value *authentication-key*) nil))

;; (defun hash-password (password)
;;   "Returns a one way hash of a plain-text password."
;;   (hunchentoot::md5-hex (copy-seq password)))

;; (defparameter *default-login-title* "Please Log In"
;;   "Default title to be used in 'do-login'.")

;; (defparameter *default-login-failure-error* "Invalid credentials."
;;   "Default message in case of login failure.")

;; (defparameter *authentication-key* 'authentication-object
;;   "A key used to store the authentication object in the session.")

;; (defview default-login-view (:type form :persistp nil
;; 			     :buttons '((:submit . "Login") :cancel)
;; 			     :caption "Login"
;; 			     :focusp t)
;;   (email :requiredp t)
;;   (password :requiredp t
;; 	    :present-as password
;; 	    :writer (lambda (pwd obj)
;; 		      (setf (slot-value obj 'password)
;; 			    (hash-password pwd)))))


;; (defmethod render-validation-summary ((view (eql (find-view 'a-login-view))) obj widget errors)
;;    (declare (ignore view obj))
;;    ;(break "me")
;;     (when errors
;;       (let ((non-field-errors (find-all errors #'null :key #'car))
;; 	    (field-errors (find-all errors (compose #'not #'null) :key #'car)))
;; 	(with-html
;; 	  (:div :class "login-validation-errors-summary"
;; 		(when non-field-errors
;; 		  (htm
;; 		   (:ul :class "non-field-validation-errors"
;; 			(mapc (lambda (err)
;; 				(with-html
;; 				  (:li
;; 				   (str (format nil "~A" (cdr err))))))
;; 			      non-field-errors))
;; 		   (render-link-proper (closure (redirect (format nil "/forgot/~A" (slot-value obj 'email))))
;; 				       #!"(forgot password?)"  :class "forgot-link")
;; 		    #+old(render-link-proper
;; 		     (closure
;; 		       (with-flow widget
;; 			 (yield (make-instance 'simple-widget :render-fn (lambda (me) (with-html (:p "yeehaw" (render-link (answer me) "go on"))))))
;; 			 (yield (make-instance 'simple-widget :render-fn (lambda (me) (with-html (:p "2 yeehaw 2" (render-link (answer me) "go on"))))))
;; 			 (yield (make-instance 'simple-widget :render-fn (lambda (me) (with-html (:p "final yell" (render-link (answer widget) "go on"))))))))
;; 		     #!"(forgot password?)"))))))))

(defwidget login-dataform (dataform)
  ())

(defmethod render-validation-summary (view obj (widget login-dataform) errors)
   (declare (ignore view obj))
   (break "me")
    (when errors
      (let ((non-field-errors (find-all errors #'null :key #'car))
	    (field-errors (find-all errors (compose #'not #'null) :key #'car)))
	(with-html
	  (:div :class "login-validation-errors-summary"
		(when non-field-errors
		  (htm
		   (:ul :class "non-field-validation-errors"
			(mapc (lambda (err)
				(with-html
				  (:li
				   (str (format nil "~A" (cdr err))))))
			      non-field-errors)))
		 ))))))

(defmethod earliest-working-example-render-validation-summary ((view (eql (find-view 'a-login-view))) obj widget errors)
   (declare (ignore view))
   (warn "me r")
    (when errors
      (let ((non-field-errors (find-all errors #'null :key #'car))
	    (field-errors (find-all errors (compose #'not #'null) :key #'car))
	    (base-compo widget))
	(with-html
	  (:div :class "login-validation-errors-summary"
		(when non-field-errors
		  (htm
		   (:ul :class "non-field-validation-errors"
			(mapc (lambda (err)
				(with-html
				  (:li
				   (str (format nil "~A" (cdr err))))))
			      non-field-errors))
		   (render-link-proper (closure (redirect (format nil "/forgot/~A" (slot-value obj 'email))))
				       #!"(forgot password?)"  :class "forgot-link")
		   (render-link-proper
		     (closure
		       (with-flow base-compo
			 (yield (make-instance 'simple-widget :render-fn (lambda (me) (with-html (:p "yeehaw" (render-link (closure (answer me)) "go on"))))))
			 (yield (make-instance 'simple-widget :render-fn (lambda (me) (with-html (:p "2 yeehaw 2" (render-link (closure (answer me)) "go on") )))))
			 (yield (make-instance 'simple-widget :render-fn (lambda (me) (with-html (:p "final yell" (render-link (closure (answer me)) "go on") )))))))
		     #!"(forgot passgooword?)"))))))))