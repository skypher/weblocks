
(defpackage #:weblocks-store-test
  (:use :cl :weblocks :lift :f-underscore))

(in-package :weblocks-store-test)

(deftestsuite store-suite ()
  ()
  (:setup (weblocks::open-stores))
  (:teardown (mapstores #'clean-store)
	     (weblocks::close-stores))
  (:documentation "Tests for the store API and implementations on
  currently defined stores."))

;; XXX this dupes deftest in weblocks-test (not yet loaded)
(defmacro deftest-store (name test &rest values)
  "A macro that makes defining test cases for stores easier. The macro
prepends the test expression with opening all stores, and appends it
with closing all stores. The result of the test expression is
returned."
  `(addtest ,name
     (ensure-same ,test
       ,(if (typep values '(cons t null))
	    `',(first values)
	    `(values . ,(mapcar (f_ `',_) values))))))

;;; test begin-transaction (no comprehensive test)
(deftest-store begin-transaction-1
    (begin-transaction *default-store*)
  nil)

;;; test commit-transaction (no comprehensive test)
(deftest-store commit-transaction-1
    (commit-transaction *default-store*)
  nil)

;;; test rollback-transaction (no comprehensive test)
(deftest-store rollback-transaction-1
    (rollback-transaction *default-store*)
  nil)

;;; object classes
#-(or weblocks-store-test-clsql weblocks-store-test-ele)
(progn
  (defclass persistent-1 ()
    ((id :initform nil)
     (slot-1 :initarg :slot-1 :accessor p-1-s-1)
     (slot-2 :initarg :slot-2 :accessor p-1-s-2)))

  (defclass persistent-2 ()
    ((id :initform nil)
     (slot-1 :initarg :slot-1 :accessor p-2-s-1)
     (slot-2 :initarg :slot-2 :accessor p-2-s-2))))

#+weblocks-store-test-clsql
(progn
  (clsql:def-view-class persistent-1 ()
    ((id :initform nil
	 :db-kind :key
	 :db-constraints (:not-null :unique)
	 :type integer)
     (slot-1 :initarg :slot-1 :accessor p-1-s-1 :type integer)
     (slot-2 :initarg :slot-2 :accessor p-1-s-2 :type integer)))

  (clsql:def-view-class persistent-2 ()
    ((id :initform nil
	 :db-kind :key
	 :db-constraints (:not-null :unique)
	 :type integer)
     (slot-1 :initarg :slot-1 :accessor p-2-s-1 :type integer)
     (slot-2 :initarg :slot-2 :accessor p-2-s-2 :type integer))))

#+weblocks-store-test-ele
(progn
  (defpclass persistent-1 ()
    ((slot-1 :initarg :slot-1 :accessor p-1-s-1 :type integer)
     (slot-2 :initarg :slot-2 :accessor p-1-s-2 :type integer)))

  (defpclass persistent-2 ()
    ((slot-1 :initarg :slot-1 :accessor p-2-s-1 :type integer)
     (slot-2 :initarg :slot-2 :accessor p-2-s-2 :type integer))))

;;; test persist-object
(deftest-store persist-object-1
    (progn
      (persist-objects *default-store*
		       (list (make-instance 'persistent-1 :slot-1 1 :slot-2 2)
			     (make-instance 'persistent-1 :slot-1 3 :slot-2 4)
			     (make-instance 'persistent-2 :slot-1 4 :slot-2 5)))
      (mapcar (lambda (obj)
		(list (p-1-s-1 obj)
		      (p-1-s-2 obj)))
	      (find-persistent-objects *default-store* 'persistent-1
				       :order-by (cons 'slot-1 :asc))))
  ((1 2) (3 4)))

(deftest-store persist-object-2
    (progn
      (persist-objects *default-store*
		       (list (make-instance 'persistent-1 :slot-1 1 :slot-2 2)
			     (make-instance 'persistent-1 :slot-1 3 :slot-2 4)
			     (make-instance 'persistent-2 :slot-1 4 :slot-2 5)))
      (mapcar (lambda (obj)
		(list (p-2-s-1 obj)
		      (p-2-s-2 obj)))
	      (find-persistent-objects *default-store* 'persistent-2)))
  ((4 5)))

(deftest-store persist-object-3
    (let ((obj (make-instance 'persistent-1 :slot-1 1 :slot-2 2)))
      (persist-object *default-store* obj)
      (setf (p-1-s-2 obj) 10)
      (persist-object *default-store* obj)
      (mapcar (lambda (obj)
		(list (p-1-s-1 obj)
		      (p-1-s-2 obj)))
	      (find-persistent-objects *default-store* 'persistent-1)))
  ((1 10)))

;;; test find-persistent-object-by-id
(deftest-store find-persistent-object-by-id-1
    (let ((obj1 (make-instance 'persistent-1 :slot-1 1 :slot-2 2))
	  (obj2 (make-instance 'persistent-1 :slot-1 3 :slot-2 4)))
      (persist-objects *default-store* (list obj1 obj2))
      (let ((obj-in-store (find-persistent-object-by-id
			   *default-store* 'persistent-1 (object-id obj2))))
	(list (p-1-s-1 obj-in-store)
	      (p-1-s-2 obj-in-store))))
  (3 4))

;;; test delete-persistent-object
(deftest-store delete-persistent-object-1
    (let ((obj1 (make-instance 'persistent-1 :slot-1 1 :slot-2 2))
	  (obj2 (make-instance 'persistent-1 :slot-1 3 :slot-2 4))
	  (obj3 (make-instance 'persistent-2 :slot-1 4 :slot-2 5)))
      (persist-objects *default-store*
		       (list obj1 obj2 obj3))
      (delete-persistent-object *default-store* obj1)
      (mapcar (lambda (obj)
		(list (p-1-s-1 obj)
		      (p-1-s-2 obj)))
	      (find-persistent-objects *default-store* 'persistent-1)))
  ((3 4)))

;;; test delete-persistent-object-by-id
(deftest-store delete-persistent-object-by-id-1
    (let ((obj1 (make-instance 'persistent-1 :slot-1 1 :slot-2 2))
	  (obj2 (make-instance 'persistent-1 :slot-1 3 :slot-2 4))
	  (obj3 (make-instance 'persistent-2 :slot-1 4 :slot-2 5)))
      (persist-objects *default-store*
		       (list obj1 obj2 obj3))
      (delete-persistent-object-by-id *default-store* 'persistent-1 (object-id obj1))
      (mapcar (lambda (obj)
		(list (p-1-s-1 obj)
		      (p-1-s-2 obj)))
	      (find-persistent-objects *default-store* 'persistent-1)))
  ((3 4)))

;;; test find-persistent-objects
(defun make-find-persistent-objects-test-fixtures ()
  "Makes fixtures for find-persistent-objects tests."
  (list (make-instance 'persistent-1 :slot-1 1 :slot-2 10)
	(make-instance 'persistent-1 :slot-1 3 :slot-2 8)
	(make-instance 'persistent-1 :slot-1 5 :slot-2 6)
	(make-instance 'persistent-1 :slot-1 7 :slot-2 4)
	(make-instance 'persistent-1 :slot-1 9 :slot-2 2)
	(make-instance 'persistent-1 :slot-1 11 :slot-2 0)))

(deftest-store find-persistent-objects-1
    (flet ((view-object (obj)
	     (list (p-1-s-1 obj)
		   (p-1-s-2 obj))))
      (persist-objects *default-store* (make-find-persistent-objects-test-fixtures))
      (list
       (mapcar #'view-object
	       (find-persistent-objects *default-store* 'persistent-1
					:range (cons 0 2)
					:order-by (cons 'slot-1 :asc)))
       (mapcar #'view-object
	       (find-persistent-objects *default-store* 'persistent-1
					:range (cons 2 5)
					:order-by (cons 'slot-1 :asc)))))
  (((1 10) (3 8))
   ((5 6) (7 4) (9 2))))

(deftest-store find-persistent-objects-2
    (flet ((view-object (obj)
	     (list (p-1-s-1 obj)
		   (p-1-s-2 obj))))
      (persist-objects *default-store* (make-find-persistent-objects-test-fixtures))
      (list
       (mapcar #'view-object
	       (find-persistent-objects *default-store* 'persistent-1
					:range (cons 0 2)
					:order-by (cons 'slot-2 :asc)))
       (mapcar #'view-object
	       (find-persistent-objects *default-store* 'persistent-1
					:range (cons 2 5)
					:order-by (cons 'slot-2 :asc)))))
  (((11 0) (9 2))
   ((7 4) (5 6) (3 8))))

(deftest-store find-persistent-objects-3
    (flet ((view-object (obj)
	     (list (p-1-s-1 obj)
		   (p-1-s-2 obj))))
      (persist-objects *default-store* (make-find-persistent-objects-test-fixtures))
      (mapcar #'view-object
	      (find-persistent-objects *default-store* 'persistent-1
				       :range (cons 2 5)
				       :order-by (cons 'slot-2 :desc))))
  ((5 6) (7 4) (9 2)))

;;; test count-persistent-objects
(deftest-store count-persistent-objects-1
    (progn
      (persist-objects *default-store*
		       (list (make-instance 'persistent-1 :slot-1 1 :slot-2 2)
			     (make-instance 'persistent-1 :slot-1 3 :slot-2 4)
			     (make-instance 'persistent-2 :slot-1 4 :slot-2 5)))
      (count-persistent-objects *default-store* 'persistent-1))
  2)

(deftest-store count-persistent-objects-2
    (progn
      (persist-objects *default-store*
		       (list (make-instance 'persistent-1 :slot-1 1 :slot-2 2)
			     (make-instance 'persistent-1 :slot-1 3 :slot-2 4)
			     (make-instance 'persistent-2 :slot-1 4 :slot-2 5)))
      (count-persistent-objects *default-store* 'persistent-2))
  1)

