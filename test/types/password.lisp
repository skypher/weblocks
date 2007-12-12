(in-package :weblocks-test)

(defclass password-employee ()
  ((password-slot :initform nil :type password :initarg :password-slot)))

;;; Create instances for introspection testing
(defparameter *password-joe* (make-instance 'password-employee :password-slot "Joe"))
(defparameter *password-bob* (make-instance 'password-employee :password-slot "Bob"))

;;; test render-form-value
(deftest-html render-form-value/password1
  (render-form-value *password-joe* 'password-slot 'password "Joe")
  (:input :type "password" :name "password-slot" :value "Joe" :maxlength 10
							      :class "password"))
(deftest-html render-form-value/password2
  (render-form-value *password-bob* 'password-slot 'password "Bob")
  (:input :type "password" :name "password-slot" :value "Bob" :maxlength 10 :class "password"))

;;; test data-print-object
(deftest data-print-object/password1
    (data-print-object *password-joe* 'password-slot 'password "Joe")
   "*******")

(deftest data-print-object/password2
    (data-print-object *password-bob* 'password-slot 'password "Bob")
   "*******")

;;; test form-print-object 
(deftest form-print-object/password1
  (form-print-object *password-joe* 'password-slot 'password "Joe")
  "Joe")

(deftest form-print-object/password2
  (form-print-object *password-bob* 'password-slot 'password "Bob")
  "Bob")

;; test for max-password-length
(deftest max-raw-slot-input-length/password
    (max-raw-slot-input-length t t 'password)
  10)
