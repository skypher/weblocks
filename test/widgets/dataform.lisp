
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

