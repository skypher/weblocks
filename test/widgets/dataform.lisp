
(in-package :weblocks-test)

;;; Define a test suite for dataform testing
(deftestsuite dataform-suite (weblocks-suite)
  ((dataform (make-instance 'dataform :data (copy-template *joe*)))))

;;; click modify, change some data, click cancel
(addtest render-dataform-modify/cancel
  ;; initial state
  (render-widget dataform)
  ;; click modify
  (do-action (find-ui-link :name "Modify"))
  ;; change name to Bob and click cancel
  (do-action (find-ui-form :class "employee" :test #'search)
    "name" "Bob"
    "cancel" "Cancel")
  (ensure-same (first-name (dataform-data dataform)) "Joe"))

;;; click modify, change some data, click submit
(addtest render-dataform-modify/submit
  ;; initial state
  (render-widget dataform)
  ;; click modify
  (do-action (find-ui-link :name "Modify"))
  ;; change to bob and click submit
  (do-action (find-ui-form :class "employee" :test #'search)
    "name" "Bob"
    "manager" "Jim"
    "submit" "Submit")
  (ensure-same (first-name (dataform-data dataform)) "Bob")
  (ensure-same (manager (dataform-data dataform)) "Jim"))

;;; click modify, introduce some errors, submit
(addtest render-dataform-modify/errors
  ;; initial state
  (render-widget dataform)
  ;; click modify
  (do-action (find-ui-link :name "Modify"))
  ;; change manager to Bill and name to error, and click submit
  (do-action (find-ui-form :class "employee" :test #'search)
    "manager" "Bill"
    "name" ""
    "submit" "Submit")
  (ensure-same (manager (dataform-data dataform)) "Jim"))

;;; make sure slots without readers that were specified are updated
;;; render data (with custom slot), modify with error, modify, submit.
(addtest render-dataform-custom-slot
  (let ((dataform (make-instance 'dataform :data (copy-template *joe*)
				 :data-view (defview () (:inherit-from '(:scaffold employee))
					      age)
				 :form-view (defview () (:type form :inherit-from '(:scaffold employee))
					      (age :parse-as (integer
							      :error-message "Age must be an integer.")
						   :requiredp t
						   :present-as (input :max-length 3))))))
    ;; initial state
    (render-widget dataform)
    ;; click modify
    (do-action (find-ui-link :name "Modify"))
    ;; change age to an error
    (do-action (find-ui-form :class "employee" :test #'search)
      "age" "bad"
      "name" "Bob"
      "manager" "Jim"
      "submit" "Submit")
    (ensure-same (first-name (dataform-data dataform)) "Joe")
    (ensure-same (slot-value (dataform-data dataform) 'age) 30)
    ;; change age
    (do-action (find-ui-form :class "employee" :test #'search)
      "age" "18"
      "name" "Bob"
      "manager" "Jim"
      "submit" "Submit")
    (ensure-same (first-name (dataform-data dataform)) "Bob")
    (ensure-same (slot-value (dataform-data dataform) 'age) 18)))

;;; make sure on-cancel/on-success/on-close callbacks work as expected
#+(or)
(addtest render-dataform-callbacks
  (let* (on-cancel-called-p
	 on-success-called-p
	 on-close-called-p
	 (dataform (make-instance 'dataform
				  :data (copy-template *joe*)
				  :ui-state :form
				  :on-cancel (lambda (grid)
					       (declare (ignore grid))
					       (setf on-cancel-called-p t))
				  :on-success (lambda (grid)
						(declare (ignore grid))
						(setf on-success-called-p t))
				  :on-close (lambda (grid)
						(declare (ignore grid))
						(setf on-close-called-p t)))))
    ;; initial state
    (render-widget dataform)
    (do-action (find-ui-form :class "employee" :test #'search)
      "cancel" "Cancel")
    (ensure on-cancel-called-p)
    ;; click modify
    (do-action (find-ui-link :name "Modify"))
    ;; click submit
    (do-action (find-ui-form :class "employee" :test #'search)
      "name" "Foo"
      "submit" "Submit")
    (ensure on-success-called-p)
    ;; click close
    (do-action (find-ui-link :name "Close"))
    (ensure on-close-called-p)))

(addtest render-dataform-dropdowns
  (let* ((data (copy-template *joe*))
	 (dataform (make-instance 'dataform :data data
				  :data-view (defview () ()
					       name address)
				  :form-view (defview () (:type form)
					       (name :requiredp t)
					       (address :present-as (dropdown :choices #'addresses)
							:parse-as address
							:reader (lambda (obj)
								  (object-id
								   (slot-value obj 'address))))))))
    ;; initial state
    (render-widget dataform)
    ;; click modify
    (do-action (find-ui-link :name "Modify"))
    ;; change Address but forget a required value
    (do-action (find-ui-form :class "employee" :test #'search)
      "address" "*work-address*"
      "submit" "Submit")
    (ensure-same (slot-value data 'address) *home-address*)
    ;; change Address
    (do-action (find-ui-form :class "employee" :test #'search)
      "address" "*work-address*"
      "name" "Joe"
      "submit" "Submit")
    ;; make sure address is persisted
    (ensure-same (slot-value data 'address) *work-address*)))

