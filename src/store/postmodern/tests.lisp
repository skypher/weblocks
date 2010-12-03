(defpackage :weblocks-postmodern-test
  (:use :cl :weblocks-postmodern))

(in-package :weblocks-postmodern-test)

(defparameter *db* "")
(defparameter *user* "")
(defparameter *pass* "")
(defparameter *host* "")

(defvar *postmodern-store*
  (open-store :postmodern :db *db* :user *user* :pass *pass* :host *host*))

;; Let's go with an "Office Space" theme...
(defclass employee
  ((id :col-type serial
       :initarg :id :reader id)
   (company :col-type string
	    :initarg :company :accessor company)
   (first-name :col-type string
	       :initarg :first-name :accessor first-name)
   (last-name :col-type string
	      :initarg :last-name :accessor last-name)
   (quote :col-type string
	  :initarg :quote :accessor quote)
   (tps-reports :col-type boolean
		:initarg :tps-reports :accessor tps-reports)
   (stapler :col-type boolean
	    :initarg :stapler :accessor stapler))
  (:metaclass dao-class)
  (:keys id))

(deftable employee (!dao-def))

(defvar *bill*
  (make-instance 'employee
		 :company "Initech"
		 :first-name "Bill"
		 :last-name "Lumbergh"
		 :quote "Yeeeaaaaah."
		 :tps-reports nil
		 :stapler nil))

(defvar *peter*
  (make-instance 'employee
		 :company "Initech"
		 :first-name "Peter"
		 :last-name "Gibbons"
		 :quote "The thing is, Bob, it's not that I'm lazy, it's that I just don't care."
		 :tps-reports nil
		 :stapler nil))

(defvar *milton*
  (make-instance 'employee
		 :company "Initech"
		 :first-name "Milton"
		 :last-name "Waddams"
		 :quote "I could set the building on fire."
		 :tps-reports t
		 :stapter t))

;; Create
(loop for employee in '(*bill* *peter* *milton*) do
  (persist-object *postmodern-store* employee))

(defun display-employee-count ()
  (format t "Employees count: ~A~%"
	  (count-persistent-objects *postmodern-store* 'employee)))

;; Retrieve
(display-employee-count)
(format t "Employee 1: ~S~%"
	(find-persistent-object-by-id *postmodern-store* 'employee 1))
(format t "Employees: ~S~%"
	(find-persistent-objects *postmodern-store* 'employee))

;; Update
(let ((employee (find-persistent-objects *postmodern-store* 'employee 1)))
  (format t "Old Quote: ~A~%" (quote employee))
  (set (quote employee) "Not right now, Lumbergh, I'm kinda busy. In fact, look, I'm gonna have to ask you to just go ahead and come back another time.")
  (persist-object employee)
  (format t "New Quote: ~A~%" (quote employee)))

;; Delete
(delete-persistent-object-by-id *postmodern-store* 'employee 2)
(format t "Yeah...you're gonna have to talk to payroll...about that.~%")
(let ((employee (find-persistent-object *postmodern-store* 'employee
					:where '(:= 'first-name "Peter"))))
  (delete-persistent-object *postmodern-store* employee))
(format t "I think I might be going away for a little while...to jail.~%")
(display-employee-count)
(clean-store *postmodern-store*)
(format t "Yeah I think the fire pretty much took care of everything.")
(display-employee-count)

(close-store *postmodern-store*)
