
(in-package :weblocks-test)

;;; Test object-class-name
(deftest object-class-name-1
    (object-class-name *joe*)
  employee)

;;; Test object-name
(deftest object-name-1
    (object-name *joe*)
  "Employee")

(deftest object-name-2
    (let (employee-name)
      (defun employee-name (empl)
	(first-name empl))
      (setf employee-name (object-name *joe*))
      (fmakunbound 'employee-name)
      employee-name)
  "Joe")

(deftest object-name-3
    (let (employee-name)
      (defun employee-name (empl)
	123)
      (setf employee-name (object-name *joe*))
      (fmakunbound 'employee-name)
      employee-name)
  "Employee")

;; test render-standard-object
(deftest-html render-standard-object-1
    (render-standard-object (lambda (obj body-fn &rest keys)
			      (with-html
				(:div
				 (funcall body-fn))))
			    #'render-slot-simple *joe*)
  (htm
   (:div
    (:p "NAME")
    (:p "STRING")
    (:p "Joe")
    (:p "MANAGER")
    (:p "T")
    (:p "Jim"))))

;;; test slot-type->css-class
(deftest slot-type->css-class-1
    (slot-type->css-class 'foobar)
  "foobar")

(deftest slot-type->css-class-2
    (slot-type->css-class '(or foobar baz))
  nil)

