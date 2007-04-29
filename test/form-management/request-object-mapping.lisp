
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
	(multiple-value-bind (success results) (weblocks::object-from-request-valid-p new-joe '(age))
	  (weblocks::update-object-from-request-aux new-joe results '(age)))
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
  (("age" . "\"Age\" must be of type Integer.")
   ("name" . "\"Name\" is a required field.")))

(deftest request-object-mapping-6
    (with-request :post '(("name" . "Pink") ("age" . "25"))
      (let ((new-joe (copy-template *joe*)))
	(update-object-from-request new-joe :slots '((name . nick) (age . how-old)))
	(values (slot-value new-joe 'name)
		(slot-value new-joe 'age))))
  "Pink" 25)

;;; test object-from-request-valid-p (also tested through request-object-mapping)
(deftest object-from-request-valid-p-1
    (with-request :get '(("name" . "Pink") ("age" . "25"))
      (let ((new-joe (copy-template *joe*)))
	(weblocks::object-from-request-valid-p new-joe '(age))))
  t
  (("age" . 25) ("name" . "Pink")))

(deftest object-from-request-valid-p-2
    (with-request :get '(("university" . "Stony Brook"))
      (let ((new-joe (copy-template *joe*)))
	(weblocks::object-from-request-valid-p new-joe '(education))))
  t
  (("university" . "Stony Brook")))

(deftest object-from-request-valid-p-3
    (with-request :get '(("name" . "Pink") ("age" . "25"))
      (let ((new-joe (copy-template *joe*)))
	(weblocks::object-from-request-valid-p new-joe '((name . nick) (age . how-old)))))
  t
  (("age" . 25) ("name" . "Pink")))

(deftest object-from-request-valid-p-4
    (with-request :get '(("university" . "Stony Brook"))
      (let ((new-joe (copy-template *joe*)))
	(weblocks::object-from-request-valid-p new-joe '(education (university . college)))))
  t
  (("university" . "Stony Brook")))

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

