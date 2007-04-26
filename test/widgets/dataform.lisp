
(in-package :weblocks-test)

;;; a dataform test flow: render data, modify, cancel, modify again, submit
(deftest-html render-dataform-1
    (with-request :get nil
      (let ((edit-joe (make-instance 'dataform :data (copy-template *joe*))))
	;; initial state
	(render edit-joe)
	;; click modify
	(do-request `((,weblocks::*action-string* . ,*dummy-action*)))
	(render edit-joe)
	;; change name to Bob and click cancel
	(do-request `(("name" . "Bob")
		      ("cancel" . "Cancel")
		      (,weblocks::*action-string* . ,*dummy-action*)))
	(render edit-joe)
	;; click modify
	(do-request `((,weblocks::*action-string* . ,*dummy-action*)))
	(render edit-joe)
	;; change to bob and click submit
	(do-request `(("name" . "Bob")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . ,*dummy-action*)))
	(render edit-joe)))
  (htm
   ;; initial state
   #.(data-header-template
      '((:li (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	(:li (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))
      :postslots '((:div :class "submit" (:a :href "?action=abc123" "Modify"))))
   ;; click modify
   #.(form-header-template
	 '((:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
	   (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
	 :method "post")
   ;; change name to Bob and click cancel
   #.(data-header-template
      '((:li (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	(:li (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))
      :postslots '((:div :class "submit" (:a :href "?action=abc123" "Modify"))))
   ;; click modify
   #.(form-header-template
	 '((:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
	   (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
	 :method "post")
   ;; change name to Bob and click submit
   #.(data-header-template
      '((:li (:span :class "label" "Name:&nbsp;") (:span :class "value" "Bob"))
	(:li (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))
      :postslots '((:div :class "submit" (:a :href "?action=abc123" "Modify"))))))

;;; a dataform test flow: render data, modify with errors, fix errors, submit
(deftest-html render-dataform-2
    (with-request :get nil
      (let ((edit-joe (make-instance 'dataform :data (copy-template *joe*))))
	;; initial state
	(render edit-joe)
	;; click modify
	(do-request `((,weblocks::*action-string* . ,*dummy-action*)))
	(render edit-joe)
	;; change manager to Bill and name to error, and click submit
	(do-request `(("manager" . "Bill")
		      ("name" . "")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . ,*dummy-action*)))
	(render edit-joe)
	;; fix error and click submit
	(do-request `(("manager" . "Bill")
		      ("name" . "Ivan")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . ,*dummy-action*)))
	(render edit-joe)))
  (htm
   ;; initial state
   #.(data-header-template
      '((:li (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	(:li (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))
      :postslots '((:div :class "submit" (:a :href "?action=abc123" "Modify"))))
   ;; modify clicked
   #.(form-header-template
	 '((:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
	   (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
	 :method "post")
   ;; manager changed to Bill and name to changed error, submit clicked
   #.(form-header-template
	 '((:li :class "item-not-validated"
	    (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "")
	     (:p :class "validation-error"
		 (:em (:span :class "validation-error-heading" "Error:&nbsp;")
		      "\"Name\" is a required field."))))
	   (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Bill"))))
	 :method "post"
	 :preslots '((:div :class "validation-errors-summary"
		  (:h2 :class "error-count" "There is 1 validation error:")
		  (:ul (:li "\"Name\" is a required field.")))))
   ;; error fixed, submit clicked
   #.(data-header-template
      '((:li (:span :class "label" "Name:&nbsp;") (:span :class "value" "Ivan"))
	(:li (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Bill")))
      :postslots '((:div :class "submit" (:a :href "?action=abc123" "Modify"))))))

;;; a dataform test flow: make sure slots without readers that were specified are updated
;;; render data (with custom slot), modify with error, modify, submit.
(deftest-html render-dataform-3
    (with-request :get nil
      (let ((edit-joe (make-instance 'dataform :data (copy-template *joe*))))
	;; initial state
	(render edit-joe :slots '(age))
	;; click modify
	(do-request `((,weblocks::*action-string* . ,*dummy-action*)))
	(render edit-joe :slots '(age))
	;; change age to an error
	(do-request `(("age" . "bad")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . ,*dummy-action*)))
	(render edit-joe :slots '(age))
	;; change age
	(do-request `(("age" . "18")
		      ("submit" . "Submit")
		      (,weblocks::*action-string* . ,*dummy-action*)))
	(render edit-joe :slots '(age))))
  (htm
   ;; initial state
   #.(data-header-template
      '((:li (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	(:li (:span :class "label" "Age:&nbsp;") (:span :class "value" "30"))
	(:li (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))
      :postslots '((:div :class "submit" (:a :href "?action=abc123" "Modify"))))
   ;; modify clicked
   #.(form-header-template
	 '((:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
	   (:li (:label (:span "Age:&nbsp;") (:input :type "text" :name "age" :value "30")))
	   (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
	 :method "post")
   #.(form-header-template
	 '((:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
	   (:li :class "item-not-validated"
	    (:label (:span "Age:&nbsp;") (:input :type "text" :name "age" :value "bad")
	     (:p :class "validation-error"
		 (:em (:span :class "validation-error-heading" "Error:&nbsp;")
		      "\"Age\" must be of type Integer."))))
	   (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
	 :method "post"
	 :preslots '((:div :class "validation-errors-summary"
		  (:h2 :class "error-count" "There is 1 validation error:")
		  (:ul (:li "\"Age\" must be of type Integer.")))))
   ;; age changed
   #.(data-header-template
      '((:li (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	(:li (:span :class "label" "Age:&nbsp;") (:span :class "value" "18"))
	(:li (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))
      :postslots '((:div :class "submit" (:a :href "?action=abc123" "Modify"))))))
