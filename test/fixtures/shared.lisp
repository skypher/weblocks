
(in-package :weblocks-test)

;;; Define classes for introspection testing
(defclass address ()
  ((id :initarg :id)
   (street :reader address-street :initarg :street :initform "100 Broadway")
   (city :reader address-city :initarg :city :initform "New York")
   (state :type us-state :initarg :state :initform "NY")))

(defparameter *home-address* (make-instance 'address :id "*home-address*"))
(defparameter *work-address* (make-instance 'address :id "*work-address*"
					    :street "200 Pine St"
					    :city "Chicago"
					    :state "IL"))

(defclass address-parser (parser)
  ())

(defmethod parse-view-field-value ((parser address-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (when (not (text-input-present-p value))
    (return-from parse-view-field-value (values t nil)))
  (let ((object (find value (addresses) :key #'object-id :test #'string-equal)))
    (when object
      (values t t object))))

(defun addresses (&rest args)
  "Returns a list of addresses."
  (declare (ignore args))
  (list *home-address* *work-address*))

(defclass education-history ()
  ((id :initarg :id)
   (university :reader university :initform "Bene Gesserit University")
   (graduation-year :reader graduation-year :initform 2000 :type (or null integer))))

(defparameter *some-college* (make-instance 'education-history :id 1))

(defclass person ()
  ((id :initarg :id)
   (name :accessor first-name :initarg :name :type string)
   (age :initarg :age :type integer)
   (address :initform *home-address*)
   (education :initform *some-college* :type (or null education-history))))

(defclass employee (person)
  ((manager :accessor manager :initform "Jim")
   (veteran :initform nil :type boolean :initarg :veteranp)))

;;; Create instances for introspection testing
(defparameter *joe* (make-instance 'employee :name "Joe" :age 30 :id 1))
(defparameter *bob* (make-instance 'employee :name "Bob" :age 50 :id 2))
(defparameter *employee1* (make-instance 'employee :name "Andy" :age 51 :id 3))
(defparameter *employee2* (make-instance 'employee :name "Tom" :age 52 :id 4))
(defparameter *employee3* (make-instance 'employee :name "Harry" :age 53 :id 5))
(defparameter *employee4* (make-instance 'employee :name "Guy" :age 54 :id 6))

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
					      :dom-id "test2"
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
