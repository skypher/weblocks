
(in-package :weblocks-test)

;;; Define classes for introspection testing
(defclass address ()
  ((id :initarg :id :initform (gen-object-id))
   (street :reader address-street :initform "100 Broadway")
   (city :reader address-city :initform "New York")
   (state :type us-state :initarg :state :initform "NY")))

(defparameter *home-address* (make-instance 'address :id "*home-address*"))

(defclass education-history ()
  ((id :initarg :id :initform (gen-object-id))
   (university :reader university :initform "Bene Gesserit University")
   (graduation-year :reader graduation-year :initform 2000 :type (or null integer))))

(defparameter *some-college* (make-instance 'education-history))

(defclass person ()
  ((id :initarg :id :initform (gen-object-id))
   (name :accessor first-name :initarg :name :type string)
   (age :initarg :age :type integer)
   (address-ref :initform *home-address*)
   (education :initform *some-college* :type (or null education-history))))

(defslotmethod max-raw-slot-input-length ((obj person) (slot-name (eql 'age)) slot-type)
  3)

(defclass employee (person)
  ((manager :reader manager :initform "Jim")
   (veteran :initform nil :type boolean :initarg :veteranp)))

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

;;; Some dummy typespecs
(deftype foo1 () 'integer)
(deftype foo2 () 'foo1)
(deftype dummy-exported-type () 'foo2)

; dummy-exported-type needs to be exported for
; invalid-input-error-message tests to work properly
(export '(dummy-exported-type))

;;; A class to test behavior on unbound slots
(defclass unbound-slots-test ()
    ((slot1 :accessor unbound-slots-test-slot1)
     (slot2 :accessor unbound-slots-test-slot2)))
