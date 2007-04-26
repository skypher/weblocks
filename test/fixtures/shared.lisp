
(in-package :weblocks-test)

;;; Define classes for introspection testing
(defclass address ()
  ((street :reader address-street :initform "100 Broadway")
   (city :reader address-city :initform "New York")))

(defparameter *home-address* (make-instance 'address))

(defclass education-history ()
  ((university :reader university :initform "Bene Gesserit University")
   (graduation-year :reader graduation-year :initform 2000 :type integer)))

(defparameter *some-college* (make-instance 'education-history))

(defclass person ()
  ((name :reader first-name :initform "Joe")
   (age :initform 30 :type integer)
   (address-ref :initform *home-address*)
   (education :initform *some-college*)))

(defclass employee (person)
  ((manager :reader manager :initform "Jim")))

(decl-validate employee
	       name (:required)
	       age (:required))

;;; Create an instance for introspection testing
(defparameter *joe* (make-instance 'employee))

