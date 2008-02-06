
(in-package :weblocks-test)

;;; test us-state-p
(deftest us-state-p-1
    (weblocks::us-state-p "ZZ")
  nil)

(deftest us-state-p-2
    (weblocks::us-state-p "NJ")
  t)

(deftest us-state-p-3
    (weblocks::us-state-p "New Jersey")
  nil)

;;; Test us-state render-view-field-value
(deftest-html us-state-render-view-field-value-1
    (with-request :get nil
      (render-view-field-value "NY" (make-instance 'us-state-presentation
						   :input-id "G0"
						   :choices-id "G1")
			       (make-instance 'form-view-field
					      :slot-name 'state)
			       (find-view '(form employee))
			       nil *joe*))
  (htm
   (:select :id "G0" :name "state"
	    (:option :value "" "[Select State]")
	    (:option"Alabama") (:option "Alaska") (:option "Arizona") (:option "Arkansas")
	    (:option "California") (:option "Colorado") (:option "Connecticut")
	    (:option "Delaware") (:option "Florida") (:option "Georgia") (:option "Hawaii")
	    (:option "Idaho") (:option "Illinois") (:option "Indiana") (:option "Iowa")
	    (:option "Kansas") (:option "Kentucky") (:option "Louisiana") (:option "Maine")
	    (:option "Maryland") (:option "Massachusetts") (:option "Michigan")
	    (:option "Minnesota") (:option "Mississippi") (:option "Missouri")
	    (:option "Montana") (:option "Nebraska") (:option "Nevada")
	    (:option "New Hampshire") (:option "New Jersey") (:option "New Mexico")
	    (:option :selected "selected" "New York") (:option "North Carolina") (:option "North Dakota")
	    (:option "Ohio") (:option "Oklahoma") (:option "Oregon") (:option "Pennsylvania")
	    (:option "Rhode Island") (:option "South Carolina") (:option "South Dakota")
	    (:option "Tennessee") (:option "Texas") (:option "Utah") (:option "Vermont")
	    (:option "Virginia") (:option "Washington") (:option "West Virginia")
	    (:option "Wisconsin") (:option "Wyoming"))
   (:script :type "text/javascript"
	    (fmt "~%// <![CDATA[~%")
	    (fmt "replaceDropdownWithSuggest(true, 'G0', 'state', 'G1', 'NY');")
	    (fmt "~%// ]]>~%"))))

;;; Test us-state parse-view-field-value
(deftest us-state-parse-view-field-value-1
    (parse-view-field-value (make-instance 'us-state-parser)
			    "NY"
			    *joe*
			    (find-view '(form employee))
			    (make-instance 'form-view-field))
  t t "NY")

(deftest us-state-parse-view-field-value-2
    (parse-view-field-value (make-instance 'us-state-parser)
			    "New York"
			    *joe*
			    (find-view '(form employee))
			    (make-instance 'form-view-field))
  t t "NY")

(deftest us-state-parse-view-field-value-3
    (parse-view-field-value (make-instance 'us-state-parser)
			    ""
			    *joe*
			    (find-view '(form employee))
			    (make-instance 'form-view-field))
  t nil)

(deftest us-state-parse-view-field-value-4
    (parse-view-field-value (make-instance 'us-state-parser)
			    "Ukraine"
			    *joe*
			    (find-view '(form employee))
			    (make-instance 'form-view-field))
  nil)

;;; Test us-state typespec->view-field-presentation
(deftest us-state-typespec->view-field-presentation-1
    (object-class-name
     (cadr
      (multiple-value-list
       (typespec->view-field-presentation (make-instance 'form-scaffold)
					  'us-state nil))))
  us-state-presentation)

;;; Test us-state typespec->form-view-field-parser
(deftest us-state-typespec->form-view-field-parser-1
    (object-class-name
     (cadr
      (multiple-value-list
       (typespec->form-view-field-parser (make-instance 'form-scaffold)
					 'us-state nil))))
  us-state-parser)


