
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
  ((name :accessor first-name :initform "Joe" :initarg :name)
   (age :initform 30 :type integer :initarg :age)
   (address-ref :initform *home-address*)
   (education :initform *some-college*)))

(defclass employee (person)
  ((manager :reader manager :initform "Jim")))

(decl-validate employee
	       name (:required)
	       age (:required))

;;; Create instances for introspection testing
(defparameter *joe* (make-instance 'employee))
(defparameter *bob* (make-instance 'employee :name "Bob" :age 50))

