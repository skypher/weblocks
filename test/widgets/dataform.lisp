
(in-package :weblocks-test)

;;; a dataform test flow: render data, modify, cancel, modify again, submit
(deftest-html render-dataform-1
    (with-request :get nil
      (let ((edit-joe (make-instance 'dataform :data (copy-template *joe*))))
	;; initial state
	(render-widget edit-joe)
	;; click modify
	(do-request `((,weblocks::*action-string* . "abc123")))
	(render-widget edit-joe)
	;; change name to Bob and click cancel
	(do-request `(("name" . "Bob")
		      ("cancel" . "Cancel")
		      (,weblocks::*action-string* . "abc124")))
	(render-widget edit-joe)
	;; click modify
	(do-request `((,weblocks::*action-string* . "abc125")))
	(render-widget edit-joe)
	;; change to bob and click submit
	(do-request `(("name" . "Bob")
		      ("manager" . "Jim")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . "abc126")))
	(render-widget edit-joe)))
  (htm
   ;; initial state
   (:div :class "widget dataform" :id "widget-123"
	 #.(data-header-template
	    "abc123"
	    '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	      (:li :class "manager" (:span :class "label" "Manager:&nbsp;")
	       (:span :class "value" "Jim")))))
   ;; click modify
   (:div :class "widget dataform" :id "widget-123"
	 #.(form-header-template
	    "abc124"
	    '((:li :class "name"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Name:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;")))
		(:input :type "text" :name "name" :value "Joe" :maxlength "40")))
	      (:li :class "manager"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Manager:&nbsp;"))
		(:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))
	    :method "post"))
   ;; change name to Bob and click cancel
   (:div :class "widget dataform" :id "widget-123"
	 #.(data-header-template
	    "abc125"
	    '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	      (:li :class "manager" (:span :class "label" "Manager:&nbsp;")
	       (:span :class "value" "Jim")))))
   ;; click modify
   (:div :class "widget dataform" :id "widget-123"
	 #.(form-header-template
	    "abc126"
	    '((:li :class "name"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Name:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;")))
		(:input :type "text" :name "name" :value "Joe" :maxlength "40")))
	      (:li :class "manager"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Manager:&nbsp;"))
		(:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))
	    :method "post"))
   ;; change name to Bob and click submit
   (:div :class "widget dataform" :id "widget-123"
	 #.(data-header-template
	    "abc127"
	    '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Bob"))
	      (:li :class "manager" (:span :class "label" "Manager:&nbsp;")
	       (:span :class "value" "Jim")))))))

;;; a dataform test flow: render data, modify with errors, fix errors, submit
(deftest-html render-dataform-2
    (with-request :get nil
      (let ((edit-joe (make-instance 'dataform :data (copy-template *joe*))))
	;; initial state
	(render-widget edit-joe)
	;; click modify
	(do-request `((,weblocks::*action-string* . "abc123")))
	(render-widget edit-joe)
	;; change manager to Bill and name to error, and click submit
	(do-request `(("manager" . "Bill")
		      ("name" . "")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . "abc124")))
	(render-widget edit-joe)
	;; fix error and click submit
	(do-request `(("manager" . "Bill")
		      ("name" . "Ivan")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . "abc125")))
	(render-widget edit-joe)))
  (htm
   ;; initial state
   (:div :class "widget dataform" :id "widget-123"
	 #.(data-header-template
	    "abc123"
	    '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	      (:li :class "manager" (:span :class "label" "Manager:&nbsp;")
	       (:span :class "value" "Jim")))))
   ;; modify clicked
   (:div :class "widget dataform" :id "widget-123"
	 #.(form-header-template
	    "abc124"
	    '((:li :class "name"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Name:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;")))
		(:input :type "text" :name "name" :value "Joe" :maxlength "40")))
	      (:li :class "manager"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Manager:&nbsp;"))
		(:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))
	    :method "post"))
   ;; manager changed to Bill and name to changed error, submit clicked
   (:div :class "widget dataform" :id "widget-123"
	 #.(form-header-template
	    "abc125"
	    '((:li :class "name item-not-validated"
	       (:label (:span :class "slot-name"
			      (:span :class "extra" "Name:&nbsp;"
				     (:em :class "required-slot" "(required)&nbsp;")))
		(:input :type "text" :name "name" :value "" :maxlength "40")
		(:p :class "validation-error"
		    (:em (:span :class "validation-error-heading" "Error:&nbsp;")
			 "Name is a required field."))))
	      (:li :class "manager"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Manager:&nbsp;"))
		(:input :type "text" :name "manager" :value "Bill" :maxlength "40"))))
	    :method "post"
	    :preslots '((:div :class "validation-errors-summary"
			 (:h2 :class "error-count" "There is 1 validation error:")
			 (:ul (:li "Name is a required field."))))))
   ;; error fixed, submit clicked
   (:div :class "widget dataform" :id "widget-123"
	 #.(data-header-template
	    "abc126"
	    '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Ivan"))
	      (:li :class "manager"
	       (:span :class "label" "Manager:&nbsp;")
	       (:span :class "value" "Bill")))))))

;;; a dataform test flow: make sure slots without readers that were specified are updated
;;; render data (with custom slot), modify with error, modify, submit.
(deftest-html render-dataform-3
    (with-request :get nil
      (let ((edit-joe (make-instance 'dataform :data (copy-template *joe*)
				     :widget-args '(:slots (age)))))
	;; initial state
	(render-widget edit-joe)
	;; click modify
	(do-request `((,weblocks::*action-string* . "abc123")))
	(render-widget edit-joe)
	;; change age to an error
	(do-request `(("age" . "bad")
		      ("name" . "Joe")
		      ("manager" . "Jim")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . "abc124")))
	(render-widget edit-joe)
	;; change age
	(do-request `(("age" . "18")
		      ("name" . "Joe")
		      ("manager" . "Jim")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . "abc125")))
	(render-widget edit-joe)))
  (htm
   ;; initial state
   (:div :class "widget dataform" :id "widget-123"
	 #.(data-header-template
	    "abc123"
	    '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	      (:li :class "age" (:span :class "label" "Age:&nbsp;") (:span :class "value" "30"))
	      (:li :class "manager" (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))))
   ;; modify clicked
   (:div :class "widget dataform" :id "widget-123"
	 #.(form-header-template
	    "abc124"
	    '((:li :class "name"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Name:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;")))
		(:input :type "text" :name "name" :value "Joe" :maxlength "40")))
	      (:li :class "age"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Age:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;")))
		(:input :type "text" :name "age" :value "30" :maxlength "3")))
	      (:li :class "manager"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Manager:&nbsp;"))
		(:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))
	    :method "post"))
   (:div :class "widget dataform" :id "widget-123"
	 #.(form-header-template
	    "abc125"
	    '((:li :class "name"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Name:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;")))
		(:input :type "text" :name "name" :value "Joe" :maxlength "40")))
	      (:li :class "age item-not-validated"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Age:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;")))
		(:input :type "text" :name "age" :value "bad" :maxlength "3")
		(:p :class "validation-error"
		    (:em (:span :class "validation-error-heading" "Error:&nbsp;")
			 "Age must be an integer."))))
	      (:li :class "manager"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Manager:&nbsp;"))
		(:input :type "text" :name "manager" :value "Jim" :maxlength "40"))))
	    :method "post"
	    :preslots '((:div :class "validation-errors-summary"
			 (:h2 :class "error-count" "There is 1 validation error:")
			 (:ul (:li "Age must be an integer."))))))
   ;; age changed
   (:div :class "widget dataform" :id "widget-123"
	 #.(data-header-template
	    "abc126"
	    '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	      (:li :class "age" (:span :class "label" "Age:&nbsp;") (:span :class "value" "18"))
	      (:li :class "manager" (:span :class "label" "Manager:&nbsp;")
	       (:span :class "value" "Jim")))))))

(deftest render-dataform-4
    (with-request :get nil
      (let* ((*weblocks-output-stream* (make-string-output-stream))
	     on-cancel-called-p
	     on-success-called-p
	     (edit-joe (make-instance 'dataform
				      :data (copy-template *joe*)
				      :ui-state :form
				      :on-cancel (lambda (grid)
						   (setf on-cancel-called-p t))
				      :on-success (lambda (grid)
						    (setf on-success-called-p t)))))
	(declare (special *weblocks-output-stream*))
	;; initial state
	(render-widget edit-joe)
	(do-request `(("cancel" . "Cancel")
		      (,weblocks::*action-string* . "abc123")))
	(render-widget edit-joe)
	;; click modify
	(do-request `((,weblocks::*action-string* . "abc124")))
	(render-widget edit-joe)
	;; click submit
	(do-request `(("name" . "Foo")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . "abc125")))
	(render-widget edit-joe)
	(values on-cancel-called-p on-success-called-p)))
  t t)

(deftest-html render-dataform-5
    (with-request :get nil
      (let* ((a 0)
	     (edit-joe (make-instance 'dataform :data (copy-template *joe*)
						:on-close (lambda (&rest args)
							    (setf a 1)))))
	;; initial state
	(render-widget edit-joe)
	;; click close
	(do-request `((,weblocks::*action-string* . "abc124")))
	(with-html (:div (str a)))))
  (htm
   ;; initial state
   (:div :class "widget dataform" :id "widget-123"
	 #.(data-header-template
	    "abc123"
	    '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	      (:li :class "manager" (:span :class "label" "Manager:&nbsp;")
	       (:span :class "value" "Jim")))
	    :postslots
	    `((:div :class "submit"
		    ,(link-action-template "abc123" "Modify") (str "&nbsp;")
		    ,(link-action-template "abc124" "Close")))))
   (:div "1")))

(deftest-html render-dataform-6
    (with-request :get nil
      (let* ((a 0)
	     (edit-joe (make-instance 'dataform :data (copy-template *joe*)
						:on-close (lambda (&rest args)
							    (setf a 1))
						:allow-close-p nil)))
	;; initial state
	(render-widget edit-joe)))
  (htm
   ;; initial state
   (:div :class "widget dataform" :id "widget-123"
	 #.(data-header-template
	    "abc123"
	    '((:li :class "name" (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	      (:li :class "manager" (:span :class "label" "Manager:&nbsp;")
	       (:span :class "value" "Jim")))))))

(deftest-html render-dataform-7
    (with-request :get nil
      (let* ((new-joe (copy-template *joe*))
	     (edit-joe (make-instance 'dataform :data new-joe
						:widget-args '(:slots (name address-ref) :mode :strict))))
	;; initial state
	(render-widget edit-joe)
	;; click modify
	(do-request `((,weblocks::*action-string* . "abc123")))
	(render-widget edit-joe)
	;; change Address but forget a required value
	(do-request `(("address-ref" . "*work-address*")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . "abc124")))
	(render-widget edit-joe)
	;; change Address
	(do-request `(("address-ref" . "*work-address*")
		      ("name" . "Joe")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . "abc125")))
	(render-widget edit-joe)
	;; make sure address is persisted
	(assert (eq (slot-value new-joe 'address-ref) *work-address*))))
  (htm
   ;; initial state
   (:div :class "widget dataform" :id "widget-123"
	 #.(data-header-template
	    "abc123"
	    '((:li :class "name" (:span :class "label" "Name:&nbsp;")
	       (:span :class "value" "Joe"))
	      (:li :class "address-ref" (:span :class "label" "Address:&nbsp;")
	       (:span :class "value" "Address")))))
   ;; click modify
   (:div :class "widget dataform" :id "widget-123"
	 #.(form-header-template
	    "abc124"
	    '((:li :class "name"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Name:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;")))
		(:input :type "text" :name "name" :value "Joe" :maxlength "40")))
	      (:li :class "address-ref"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Address:&nbsp;"))
		(:select :name "address-ref"
			 (:option :value "" "[Select None]")
			 (:option :value "*home-address*" :selected "selected" "Address")
			 (:option :value "*work-address*" "Address")))))
	    :method "post"))
   ;; click submit
   (:div :class "widget dataform" :id "widget-123"
	 #.(form-header-template
	    "abc125"
	    '((:li :class "name item-not-validated"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Name:&nbsp;"
			      (:em :class "required-slot" "(required)&nbsp;")))
		(:input :type "text" :name "name" :maxlength "40")
		(:p :class "validation-error"
		    (:em (:span :class "validation-error-heading" "Error:&nbsp;")
			 "Name is a required field."))))
	      (:li :class "address-ref"
	       (:label
		(:span :class "slot-name"
		       (:span :class "extra" "Address:&nbsp;"))
		(:select :name "address-ref"
			 (:option :value "" "[Select None]")
			 (:option :value "*home-address*" "Address")
			 (:option :value "*work-address*" :selected "selected" "Address")))))
	    :method "post"
	    :preslots '((:div :class "validation-errors-summary"
			 (:h2 :class "error-count" "There is 1 validation error:")
			 (:ul (:li "Name is a required field."))))))
   ;; click submit again
   (:div :class "widget dataform" :id "widget-123"
	 #.(data-header-template
	    "abc126"
	    '((:li :class "name" (:span :class "label" "Name:&nbsp;")
	       (:span :class "value" "Joe"))
	      (:li :class "address-ref" (:span :class "label" "Address:&nbsp;")
	       (:span :class "value" "Address")))))))

