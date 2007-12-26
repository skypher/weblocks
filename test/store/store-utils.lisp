
(in-package :weblocks-test)

;;; test class-id-slot-name
(deftest class-id-slot-name-1
    (class-id-slot-name 'foobar)
  id)

;;; test object-id-slot-name
(deftest object-id-slot-name-1
    (object-id-slot-name *joe*)
  id)

;;; test object-id
(deftest object-id-1
    (object-id *joe*)
  1)

(deftest object-id-2
    (let ((employee (copy-template *joe*)))
      (setf (object-id employee) 11)
      (object-id employee))
  11)

;;; test class-store
(deftest class-store-1
    (with-request :get nil
      (string-downcase (symbol-name (class-name (class-of (class-store 'foobar))))))
  "memory-store")

;;; test object-store
(deftest object-store-1
    (with-request :get nil
      (string-downcase (symbol-name (class-name (class-of (object-store *joe*))))))
  "memory-store")

;;; Note, defstore, open-stores, and close-stores are tested
;;; implicitly as part of weblocks-test

;;; test mapstores
(deftest mapstores-1
    (with-request :get nil
      (let ((i 0))
	(mapstores (lambda (store)
		     (declare (ignore store))
		     (incf i)))
	(> i 0)))
  t)

;;; test persist-objects
(deftest persist-objects-1
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (count-persistent-objects *default-store* 'employee))
  2)

