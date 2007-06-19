
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
  ((id :initarg :id :initform (gen-object-id))
   (name :accessor first-name :initarg :name)
   (age :type integer :initarg :age)
   (address-ref :initform *home-address*)
   (education :initform *some-college*)))

(defclass employee (person)
  ((manager :reader manager :initform "Jim")))

(decl-validate employee
	       name (:required)
	       age (:required))

;;; Create instances for introspection testing
(defparameter *joe* (make-instance 'employee :name "Joe" :age 30 :id 1))
(defparameter *bob* (make-instance 'employee :name "Bob" :age 50 :id 2))

;;; helper to create complex site layout
(defun create-site-layout ()
  (make-instance 'composite :name 'root :widgets
		 (list
		  (make-instance
		   'composite
		   :name 'root-inner
		   :widgets
		   (list
		    (make-instance 'composite :name 'leaf)
		    (make-navigation "test-nav-1"
				     "test1" (make-instance
					      'composite
					      :widgets
					      (list
					       (make-instance 'composite :name 'test1-leaf)
					       (make-navigation "test-nav-2"
								"test3" (lambda (&rest args) nil)
								"test4" (lambda (&rest args) nil))))
				     "test2" (make-instance
					      'composite
					      :widgets
					      (list
					       (make-instance 'composite :name 'test2-leaf)
					       (make-navigation "test-nav-3"
								"test5" (lambda (&rest args) nil)
								"test6" (lambda (&rest args) nil))))))))))

