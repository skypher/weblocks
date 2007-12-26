
(in-package :weblocks-test)

;;; test request-object-mapping
(deftest request-object-mapping-1
    (with-request :get '(("name" . "Pink") ("age" . "25"))
      (let ((new-joe (copy-template *joe*)))
	(update-object-from-request new-joe :slots '(age))
	(values (slot-value new-joe 'name)
		(slot-value new-joe 'age))))
  "Pink" 25)

(deftest request-object-mapping-2
    (with-request :post '(("name" . "Pink") ("age" . "25"))
      (let ((new-joe (copy-template *joe*)))
	(update-object-from-request new-joe :slots '(age))
	(values (slot-value new-joe 'name)
		(slot-value new-joe 'age))))
  "Pink" 25)

(deftest request-object-mapping-3
    (with-request :post '(("name" . "Pink") ("age" . "2t5"))
      (let ((new-joe (copy-template *joe*)))
	(update-object-from-request new-joe :slots '(age))
	(values (slot-value new-joe 'name)
		(slot-value new-joe 'age))))
  "Joe" 30)

(deftest request-object-mapping-4
    (with-request :post '(("name" . "Pink") ("age" . "30"))
      (let ((new-joe (copy-template *joe*)))
	(multiple-value-bind (success results)
	    (weblocks::object-from-request-valid-p new-joe :slots '(age))
	  (weblocks::update-object-from-request-aux new-joe results :slots '(age)))
	(values (slot-value new-joe 'name)
		(slot-value new-joe 'age))))
  "Pink" 30)

(deftest request-object-mapping-5
    (with-request :post '(("name" . "") ("age" . "2t5"))
      (let ((new-joe (copy-template *joe*)))
	(multiple-value-bind (result errors) (update-object-from-request new-joe :slots '(age))
	  (mapcar (lambda (res)
		    `(,(car res) . ,(format nil "~A" (cdr res))))
		  errors))))
  (("name" . "Name is a required field.")
   ("age" . "Age must be an integer.")))

(deftest request-object-mapping-6
    (with-request :post '(("name" . "Pink") ("age" . "25"))
      (let ((new-joe (copy-template *joe*)))
	(update-object-from-request new-joe :slots '((name . nick) (age . how-old)))
	(values (slot-value new-joe 'name)
		(slot-value new-joe 'age))))
  "Pink" 25)

(deftest request-object-mapping-7
    (with-request :post '(("name" . "") ("age" . "r2d2"))
      (let ((new-joe (copy-template *joe*)))
	(multiple-value-bind (result errors) (update-object-from-request new-joe :slots '(age))
	  (mapcar (lambda (res)
		    `(,(car res) . ,(format nil "~A" (cdr res))))
		  errors))))
  (("name" . "Name is a required field.")
   ("age" . "Age must not exceed 3 characters.")))

;;; We need to make sure validation summary errors are returned in the
;;; right order. For a full test we need new fixtures
(defclass vseo-inner () ; validation-symmary-error-order
  ((b :accessor vseo-inner-b :initform 1 :type integer)
   (c :accessor vseo-inner-c :initform 1 :type integer)))

(defclass vseo ()
  ((a :accessor vseo-a :initform 1 :type integer)
   (inner :accessor vseo-inner :initform (make-instance 'vseo-inner))
   (d :accessor vseo-d :initform 1 :type integer)))

(deftest request-object-mapping-8
    (with-request :post '(("a" . "a") ("b" . "b") ("c" . "c") ("d" . "d"))
      (multiple-value-bind (result errors) (update-object-from-request (make-instance 'vseo))
	(mapcar (lambda (res)
		  `(,(car res) . ,(format nil "~A" (cdr res))))
		errors)))
  (("a" . "A must be an integer.")
   ("b" . "B must be an integer.")
   ("c" . "C must be an integer.")
   ("d" . "D must be an integer.")))

(deftest request-object-mapping-9
    (with-request :get '(("age" . "25"))
      (let ((new-joe (copy-template *joe*)))
	(update-object-from-request new-joe :slots '(age) :mode :strict)
	(slot-value new-joe 'age)))
  25)

(deftest request-object-mapping-10
    (with-request :get '(("name" . "Pink") ("age" . "25"))
      (let ((new-joe (copy-template *joe*)))
	(update-object-from-request new-joe :slots '(age))
	(values (slot-value new-joe 'name)
		(slot-value new-joe 'age)))
      (count-persistent-objects *default-store* 'employee))
  1)

(deftest request-object-mapping-11
    (with-request :get '(("name" . "Pink") ("age" . "25"))
      (let ((new-joe (copy-template *joe*)))
	(update-object-from-request new-joe :slots '(age) :persist-object-p nil)
	(values (slot-value new-joe 'name)
		(slot-value new-joe 'age)))
      (count-persistent-objects *default-store* 'employee))
  0)

;;; test object-from-request-valid-p (also tested through request-object-mapping)
(deftest object-from-request-valid-p-1
    (with-request :get '(("name" . "Pink") ("age" . "25"))
      (let ((new-joe (copy-template *joe*)))
	(weblocks::object-from-request-valid-p new-joe :slots '(age))))
  t
  (("manager") ("age" . 25) ("name" . "Pink")))

(deftest object-from-request-valid-p-2
    (with-request :get '(("university" . "Stony Brook"))
      (let ((new-joe (copy-template *joe*)))
	(weblocks::object-from-request-valid-p new-joe :slots '(education))))
  nil
  (("name" . "Name is a required field.")))

(deftest object-from-request-valid-p-3
    (with-request :get '(("university" . "Stony Brook")
			 ("name" . "Foo"))
      (let ((new-joe (copy-template *joe*)))
	(weblocks::object-from-request-valid-p new-joe :slots '(education))))
  t
  (("manager")
   ("graduation-year")
   ("university" . "Stony Brook")
   ("name" . "Foo")))

(deftest object-from-request-valid-p-4
    (with-request :get '(("name" . "Pink") ("age" . "25"))
      (let ((new-joe (copy-template *joe*)))
	(weblocks::object-from-request-valid-p new-joe :slots '((name . nick) (age . how-old)))))
  t
  (("manager") ("age" . 25) ("name" . "Pink")))

(deftest object-from-request-valid-p-5
    (with-request :get '(("university" . "Stony Brook") ("name" . "Foo"))
      (let ((new-joe (copy-template *joe*)))
	(weblocks::object-from-request-valid-p new-joe :slots '(education (university . college)))))
  t
  (("manager") ("graduation-year") ("university" . "Stony Brook") ("name" . "Foo")))

;;; test slot-in-request-empty-p
(deftest slot-in-request-empty-p-1
    (slot-in-request-empty-p nil "")
  t)

(deftest slot-in-request-empty-p-2
    (slot-in-request-empty-p nil "hello")
  nil)

(deftest slot-in-request-empty-p-3
    (slot-in-request-empty-p 'integer "")
  t)

;;; test request-parameters-for-object
(deftest request-parameters-for-object-1
    (with-request :get '(("name" . "Foo"))
      (request-parameters-for-object (copy-template *joe*)))
  (("name" . "Foo") ("manager")))

