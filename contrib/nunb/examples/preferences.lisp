(in-package :app)
 
;(defgeneric make-user-settings (usertype))

(defwidget flowable-widget (widget state-changer-mixin)
  ((custom-composite :accessor base-composite-of :initform nil)
   (initial-widget :accessor initial-widget-of  :initarg :initial :initform (break "Need initial widget!")))
  (:documentation "A widget to be used in dialogs etc, provides a base composite that can be used for with-flow"))

(defmethod initialize-instance :after ((obj flowable-widget) &rest initargs &key initial &allow-other-keys)
  (declare (ignore initargs))
  (setf (base-composite-of obj)
	(make-instance 'composite :widgets (list (initial-widget-of obj))))) ;no need (list (initial-widget-of obj))

(defmethod render-widget-body ((obj flowable-widget) &rest args)
  (declare (ignore args))
  (render-widget (base-composite-of obj)))


; need this in invitations and in logins and in preferences?
; NAME: 4 char minim char, numbers, letters
(defun  uniq-username-p (acode)
  (warn (format nil "Unique username was called with ~A " acode))
  (if acode
      (let ((is-uniq
	     (not (remove-if-not (lambda (co)
				   (when (string= (get-email co)
						  acode)
				     t))
				 (all-userlogins)))))
	(if is-uniq    ; more testing ..
	    (cond
	      ((> 4 (length acode))
	       (values nil #!"Unique username says: Nome utente troppo breve"))
	      (t
	       t))
	    (values nil #!"Nome utente non disponibile")))
      (values nil #!"Nome utente non valido")))

; 4 chars and 1 number
(defun  decent-password-p (acode)
  (warn (format nil "Password chk was called with ~A " acode))
  (if acode
       (cond
	      ((> 4 (length acode))
	       (values nil #!"Password troppo breve"))
	      ((not (cl-ppcre:scan "[0-9]" acode))
	       (values nil #!"At least 1 number in password"))
	      (t
	       t))
      (values nil #!"Password non valido")))

; --- upto here

(defview example-logins (:type templform)
  (username :reader (closure (get-username nl)) :writer (lambda (v) (setf (get-username nl) v)) :present-as (input) :satisfies #'uniq-username-p)
  (password :reader (closure (get-password nl)) :writer (lambda (v) (setf (get-password nl) v)) :present-as (input)))

(defview login-template-form-view (:type templform)
  (username  :satisfies #'uniq-username-p)
  (password  :present-as (input)))



(defwidget custom-prefs (widget)
  ((custom-composite :accessor base-composite-of :initform nil)
   (second-attempt :accessor second-attempt-of   :initform nil))
  (:documentation "Second fucken attempt"))

(defun choose-settings-for-user ()
  "Depending on what type of user is logged in == return their settings page"
  (cond
    ((authenticatedp)
     (ecase (user-type)
	      (adminsetup  (make-instance 'simple-widget :render-fn (closure (with-html (:p "No preferences for you, bubba!")))))
	      (co-agent    (make-delegate-settings))
	      (co-employee (make-handler-settings 1))))
    (t
     (make-instance 'simple-widget :render-fn (closure (with-html (:p "You are an idiot, and so am I")))))))

(defmethod initialize-instance :after ((obj custom-prefs) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (second-attempt-of obj)
	(make-quickform
	 (weblocks::defview-anon (:type templform
					:persistp nil 
					:file "prefs.password"
					:buttons '((:submit . "Settings") (:cancel . "Annulla"))
					:caption #!"LoginCaption"
					:focusp t)
	     (password :present-as password
		       :satisfies (make-satisfier (lambda (val)
						    	(cond
							  ((string= (hash-password val)
								    (get-hashed-password (authenticatedp)))
							   t)
							  (t
							   nil)))
						  #!"Password non valida")))
	 :on-success (lambda (w o)
		       (with-flow w
			 (yield (choose-settings-for-user))
			 (answer obj)))
	 :answerp nil
	 :on-cancel (closure (answer obj))
	 :satisfies (lambda (w o)
		      (warn (format nil "obj o"))
		      (describe o)
		      (warn (format nil "auth " )) (describe (authenticatedp))
		      (with-slots (password) o
			(cond
			  ((string= (hash-password password)
				    (get-hashed-password (authenticatedp)))
			   t)
			  (t
			   (break "ritorno bill nil")
			   nil))))
	 :data (make-instance 'tmplogin)))
  (setf (base-composite-of obj)
	(make-instance 'composite :widgets (list
					    (second-attempt-of  obj)))))


(defmethod render-widget-body ((obj custom-prefs) &rest args)
  (declare (ignore args))
  (render-widget (base-composite-of obj)))

(defun final-user-settings (user)
  (make-instance 'custom-prefs))

;;  (with-flow qf-widget-with-parent ;what is the value of this var here? the actual qf or the parent?
;;       (when (eq :fresh (yield (make-forgotten-password qf-widget-with-parent tl)))
;; 	(yield  (make-quickform (weblocks::defview-anon (:type templform :file "password.sent" :persistp nil  :caption #!"PasswordSent" :buttons '((:submit . "Va bene")))
;; 							(email :present-as (paragraph)))
;; 					;:on-success (lambda (w o) (answer w))
;; 					:data tl)
;;		)))
;(defmethod make-user-settings ((usr co-agent))  )

(defun handler-prefs-ok-modif
   (newlogin-obj
    handler-obj
    &rest args
    &key
    firstname lastname
    phone1    phone2
    email     fax
    password password-field
    username username-field
    firstname-field
    lastname-field
    phone1-field
    phone2-field
    email-field
    fax-field
    &allow-other-keys)
  (warn "Handler testing fn called")
  (let ((error-messages nil)
	(errs-1 (list phone1-field phone2-field email-field)))
    (unless (or phone1 phone2 email)
      (mapcar (lambda (fld)
		(push
		 (cons fld #!"prefs at least way to contact you")
		 error-messages))
	      errs-1))
    ; just set it, so the form has the updated (perhaps bad) pass
    (when (not (string= password (make-user-fake-pass)))
      (setf (get-password newlogin-obj) password))
    (cond
      ((and (not (string= password (make-user-fake-pass)))
	    (not (decent-password-p password)))
       (warn (format nil "PASSWORD ~A USERNAME ~A" password username))
       (push
	(cons password-field #!"that is not a good password")
	error-messages))
      ((and (not (string= username (get-email (authenticatedp)))))
       (multiple-value-bind (r e)
		 (uniq-username-p username)
	       (unless r
		 (push
		  (cons username-field e)
		  error-messages)))))
    (if  error-messages
	 (values nil error-messages)
	 t)))

;; (string= (slot-value (get-hashed-password (authenticatedp)))
;; 			  (hash-password password))

(defclass booper (person)
  ((nl)))

(defun make-fake-password (len)
  (let ((str (make-array 0
			 :element-type 'character
			 :fill-pointer 0
			 :adjustable t)))
    (loop for i from 1 to (or len 10) do   (vector-push-extend #\A str))
    str))

(defun make-user-fake-pass ()
  (make-fake-password (passlen-of (authenticatedp))))

(defun make-handler-settings (val)
  (when (the-user)
    (let* ((nl           (make-instance 'new-login :username (get-email (authenticatedp)) :password (make-user-fake-pass)))
	   (the-handler  (the-user)))
      (make-quickform
		(weblocks::defview-anon (:type templform
					       :persistp nil
					       :satisfies (curry #'handler-prefs-ok-modif nl)
					       :inherit-from 'handler-template-form-view-for-invite
					       :file "prefs.handler" :language "htm"
					       :caption "Settings")
		    (username :reader (closure (get-username nl)) :writer (lambda (v o) (setf (get-username nl) v)) :present-as (input)) ; :satisfies #'uniq-username-p
		  (password :reader (closure (get-password nl)) :writer (lambda (v o) (setf (get-password nl) v)) :present-as (showpasswd))) ;:satisfies #'decent-password-p password-presentation
		:data the-handler
		:answerp t
		:on-success (closure
			      (persist-object *default-store* the-handler)
			      ;(persist-object *default-store* nl)
			      :ok) ;(answer w :ok)
		:on-cancel (closure :ok)))))

(defun make-delegate-settings ()
  (when (the-user)
    (let* ((nl           (make-instance 'new-login :username (get-email (authenticatedp)) :password (make-fake-password (passlen-of (authenticatedp)))))
	   (the-handler  (the-user)))
      (make-quickform
		(weblocks::defview-anon (:type templform
					       :persistp nil
					       :satisfies (list (curry #'handler-prefs-ok-modif nl) (curry #'delegate-prefs-ok-modif nl) ) 
					       :inherit-from 'handler-template-form-view-for-invite
					       :file "prefs.agent" :language "htm"
					       :caption "Settings")
		    (location-details :type mixin :view 'location-template-form-view)
		    (city-or-location)
		    (username :reader (closure (get-username nl)) :writer (lambda (v o) (setf (get-username nl) v)) :present-as (input)) ; :satisfies #'uniq-username-p
		    (password :reader (closure (get-password nl)) :writer (lambda (v o) (setf (get-password nl) v)) :present-as (showpasswd))) ;:satisfies #'decent-password-p password-presentation
		:data the-handler
		:answerp t
		:on-success (closure
			      (persist-object *default-store* the-handler)
			      ;(persist-object *default-store* nl)
			      :ok) ;(answer w :ok)
		:on-cancel (closure :ok)))))

(defun delegate-prefs-ok-modif ()
  t)

(defwidget custom-quickform (quickform)
  ((custom-composite :accessor base-composite-of :initform nil)
   (second-attempt :accessor second-attempt-of  :initform nil))
  (:documentation "Enhance basic login with lagru-form. Add a custom error message and a forgot password link."))

(defwidget custom-login (login)
  ((custom-composite :accessor base-composite-of :initform nil)
   (second-attempt :accessor second-attempt-of  :initform nil))
  (:documentation "Enhance basic login with lagru-form. Add a custom error message and a forgot password link."))
