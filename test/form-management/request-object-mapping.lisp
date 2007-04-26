
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
    (with-request :post '(("name" . "") ("age" . "2t5"))
      (let ((new-joe (copy-template *joe*)))
	(multiple-value-bind (result errors) (update-object-from-request new-joe :slots '(age))
	  (mapcar (lambda (res)
		    `(,(car res) . ,(format nil "~A" (cdr res))))
		  errors))))
  (("age" . "\"Age\" must be of type Integer.")
   ("name" . "\"Name\" is a required field.")))

;;; test object-from-request-valid-p (also tested through request-object-mapping)
(deftest object-from-request-valid-p-1
    (with-request :get '(("name" . "Pink") ("age" . "25"))
      (let ((new-joe (copy-template *joe*)))
	(weblocks::object-from-request-valid-p new-joe (object-visible-slots *joe* :slots '(age)))))
  t
  (("age" . 25) ("name" . "Pink")))

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

