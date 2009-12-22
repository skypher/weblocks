
(in-package :weblocks-test)

(deftestsuite store/store-utils-suite (weblocks-suite)
  ())

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

(addtest reeval-defstore-doesnt-reset
  (ensure (typep weblocks::*stores* 'hash-table))
  (ensure (typep weblocks::*store-names* 'list))
  (let ((fakestore (gensym))
	(weblocks::*stores* (make-hash-table))
	(weblocks::*store-names* '()))
    (eval `(defstore ,fakestore :memory))
    (weblocks::open-stores)
    (unwind-protect
	 (progn
	   (ensure (symbol-value fakestore))
	   (let ((oldval (symbol-value fakestore)))
	     (eval `(defstore ,fakestore :memory))
	     (ensure-same (symbol-value fakestore) oldval)))
      (weblocks::close-stores))))

;;; test mapstores
(deftest mapstores-1
    (with-request :get nil
      (let ((i 0))
	(mapstores (lambda (store)
		     (declare (ignore store))
		     (incf i)))
	(> i 0)))
  t)

;;; test webapp/store association
(addtest webapp-associates-first-defstore
  (with-test-webapp (:class-name 'app-with-not-searchable-store)
    (with-webapp (current-webapp)
      (ensure-same *default-store* *not-searchable-store*)))
  (ensure-same *default-store* *test-store*))

;;; test persist-objects
(deftest persist-objects-1
    (with-request :get nil
      (persist-objects *default-store* (list *joe* *bob*))
      (count-persistent-objects *default-store* 'employee))
  2)

