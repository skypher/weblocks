
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
   (:div :class "renderer data employee"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:h1 (:span :class "action" "Viewing:&nbsp;")
	      (:span :class "object" "Employee"))
	 (:ul
	  (:li (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	  (:li (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))
	 (:div :class "submit" (:a :href "?action=abc123" "Modify"))
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;"))
   ;; click modify
   (:form :class "renderer form employee" :action "" :method "post"
	  (:div :class "extra-top-1" "&nbsp;")
	  (:div :class "extra-top-2" "&nbsp;")
	  (:div :class "extra-top-3" "&nbsp;")
	  (:fieldset
	   (:h1 (:span :class "action" "Modifying:&nbsp;")
		(:span :class "object" "Employee"))
	   (:ul
	    (:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
	    (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
	   (:div :class "submit"
		 (:input :name "action" :type "hidden" :value "abc123")
		 (:input :name "submit" :type "submit" :value "Submit")
		 (:input :name "cancel" :type "submit" :value "Cancel")))
	  (:div :class "extra-bottom-1" "&nbsp;")
	  (:div :class "extra-bottom-2" "&nbsp;")
	  (:div :class "extra-bottom-3" "&nbsp;"))
   ;; change name to Bob and click cancel
   (:div :class "renderer data employee"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:h1 (:span :class "action" "Viewing:&nbsp;")
	      (:span :class "object" "Employee"))
	 (:ul
	  (:li (:span :class "label" "Name:&nbsp;") (:span :class "value" "Joe"))
	  (:li (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))
	 (:div :class "submit" (:a :href "?action=abc123" "Modify"))
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;"))
   ;; click modify
   (:form :class "renderer form employee" :action "" :method "post"
	  (:div :class "extra-top-1" "&nbsp;")
	  (:div :class "extra-top-2" "&nbsp;")
	  (:div :class "extra-top-3" "&nbsp;")
	  (:fieldset
	   (:h1 (:span :class "action" "Modifying:&nbsp;")
		(:span :class "object" "Employee"))
	   (:ul
	    (:li (:label (:span "Name:&nbsp;") (:input :type "text" :name "name" :value "Joe")))
	    (:li (:label (:span "Manager:&nbsp;") (:input :type "text" :name "manager" :value "Jim"))))
	   (:div :class "submit"
		 (:input :name "action" :type "hidden" :value "abc123")
		 (:input :name "submit" :type "submit" :value "Submit")
		 (:input :name "cancel" :type "submit" :value "Cancel")))
	  (:div :class "extra-bottom-1" "&nbsp;")
	  (:div :class "extra-bottom-2" "&nbsp;")
	  (:div :class "extra-bottom-3" "&nbsp;"))
   ;; change name to Bob and click submit
   (:div :class "renderer data employee"
	 (:div :class "extra-top-1" "&nbsp;")
	 (:div :class "extra-top-2" "&nbsp;")
	 (:div :class "extra-top-3" "&nbsp;")
	 (:h1 (:span :class "action" "Viewing:&nbsp;")
	      (:span :class "object" "Employee"))
	 (:ul
	  (:li (:span :class "label" "Name:&nbsp;") (:span :class "value" "Bob"))
	  (:li (:span :class "label" "Manager:&nbsp;") (:span :class "value" "Jim")))
	 (:div :class "submit" (:a :href "?action=abc123" "Modify"))
	 (:div :class "extra-bottom-1" "&nbsp;")
	 (:div :class "extra-bottom-2" "&nbsp;")
	 (:div :class "extra-bottom-3" "&nbsp;"))))

