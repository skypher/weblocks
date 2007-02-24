
(in-package :weblocks)

(defclass address ()
  ((street :reader street)
   (city :reader city)
   (state :reader state)))

(defclass person ()
  ((first-name :reader first-name)
   (last-name :reader last-name)
   (age :reader age)
   (address-ref :reader address)
   (id :initform 1)))

(defclass employee (person)
  ((department :reader department)))

(defparameter *joe-employee* (make-instance 'employee))

(setf (slot-value *joe-employee* 'first-name) "Slava")
(setf (slot-value *joe-employee* 'last-name) "Akhmechet")
(setf (slot-value *joe-employee* 'age) "23")

(let ((addr (make-instance 'address)))
  (setf (slot-value addr 'street) "1877 Ocean Ave.")
  (setf (slot-value addr 'city) "Brooklyn")
  (setf (slot-value addr 'state) "NY")
  (setf (slot-value *joe-employee* 'address-ref) addr))

(setf (slot-value *joe-employee* 'department) "Technology")
