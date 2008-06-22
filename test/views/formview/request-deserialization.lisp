
(in-package :weblocks-test)

;;; Test update-object-view-from-request
(deftest update-object-view-from-request-1
    (with-request :get '(("name" . "foo") ("manager" . "bar"))
      (let ((obj (copy-template *joe*)))
	(update-object-view-from-request obj
					 (find-view '(form employee)))
	(values (first-name obj)
		(manager obj)
		(length (find-persistent-objects *default-store* 'employee)))))
  "foo" "bar" 1)

(deftest update-object-view-from-request-2
    (with-request :get '(("name" . "foo") ("manager" . "bar"))
      (let ((obj (copy-template *joe*)))
	(update-object-view-from-request obj
					 (defview () (:type form
						      :inherit-from '(:scaffold employee)
						      :persistp nil)))
	(values (first-name obj)
		(manager obj)
		(length (find-persistent-objects *default-store* 'employee)))))
  "foo" "bar" 0)

(deftest update-object-view-from-request-3
    (with-request :get '(("manager" . "bar"))
      (not (null
	    (update-object-view-from-request (copy-template *joe*)
					     (find-view '(form employee))))))
  nil)

(deftest update-object-view-from-request-4
    (with-request :get '(("manager" . "bar"))
      (not (null
	    (update-object-view-from-request (copy-template *joe*)
					     (find-view '(form employee))))))
  nil)

(deftest update-object-view-from-request-5
    (with-request :get '(("name" . "foo") ("manager" . "bar"))
      (let ((obj (copy-template *joe*)))
	(multiple-value-bind (result errors)
	    (update-object-view-from-request obj
					     (defview () (:type form
								:inherit-from '(:scaffold employee)
								:persistp nil
								:satisfies (lambda (&key name manager)
									     (if (string-equal name manager)
										 t
										 (values nil "abcd"))))))
	  (values result (cdr (first errors))))))
  nil "abcd")

(deftest update-object-view-from-request-6
    (with-request :get '(("name" . "foo") ("manager" . "foo"))
      (let ((obj (copy-template *joe*)))
	(multiple-value-bind (result errors)
	    (update-object-view-from-request obj
					     (defview () (:type form
								:inherit-from '(:scaffold employee)
								:persistp nil
								:satisfies (lambda (&key name manager)
									     (if (string-equal name manager)
										 t
										 (values nil "abcd"))))))
	  (values result (cdr (first errors))))))
  t nil)

;;; Test request-parameters-for-object-view
(deftest request-parameters-for-object-view-1
    (with-request :get '(("name" . "blah"))
      (mapcar
       (lambda (item)
	 (cons (view-field-slot-name (car item)) (cdr item)))
       (request-parameters-for-object-view (find-view '(form employee)))))
  ((name . "blah") (manager)))

