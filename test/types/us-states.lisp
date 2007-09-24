
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

;;; test render-form-aux for us-state
(deftest-html render-form/aux-us-states-1
    (with-request :post nil
      (render-form-value *home-address* 'state 'us-state "NY" :input-id "G0" :choices-id "G1"))
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

;;; test parse-slot-from-request for us-state
(deftest parse-slot-from-request-us-states-1
    (parse-slot-from-request 'us-state nil "NJ")
  t "NJ")

(deftest parse-slot-from-request-us-states-2
    (parse-slot-from-request 'us-state nil "New Jersey")
  t "NJ")

(deftest parse-slot-from-request-us-states-3
    (parse-slot-from-request 'us-state nil "")
  nil)

(deftest parse-slot-from-request-us-states-4
    (parse-slot-from-request 'us-state nil "Ukraine")
  nil)

;;; test invalid-input-error-message
(deftest invalid-input-error-message-us-states-1
    (invalid-input-error-message nil nil nil 'us-state "foo")
  "Please enter a valid US State.")

