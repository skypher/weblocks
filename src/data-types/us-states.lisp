
(in-package :weblocks)

(defparameter *us-states* '("Alabama - AL" "Alaska - AK" "Arizona - AZ" "Arkansas - AR"
			    "California - CA" "Colorado - CO" "Connecticut - CT"
			    "Delaware - DE" "Florida - FL" "Georgia - GA" "Hawaii  - HI"
			    "Idaho - ID" "Illinois - IL" "Indiana - IN" "Iowa - IA"
			    "Kansas - KS" "Kentucky - KY" "Louisiana - LA" "Maine - ME"
			    "Maryland - MD" "Massachusetts - MA" "Michigan - MI"
			    "Minnesota - MN" "Mississippi - MS" "Missouri - MO"
			    "Montana - MT" "Nebraska - NE" "Nevada - NV"
			    "New Hampshire - NH" "New Jersey - NJ" "New Mexico - NM"
			    "New York - NY" "North Carolina - NC" "North Dakota - ND"
			    "Ohio - OH" "Oklahoma - OK" "Oregon - OR" "Pennsylvania - PA"
			    "Rhode Island - RI" "South Carolina - SC" "South Dakota - SD"
			    "Tennessee - TN" "Texas - TX" "Utah - UT" "Vermont - VT"
			    "Virginia - VA" "Washington - WA" "West Virginia - WV"
			    "Wisconsin - WI" "Wyoming - WY"))

(defclass us-state ()
  ((state :accessor us-state-accessor
	  :initarg :state
	  :type string)))

(defmethod us-state ((state string))
  (make-instance 'us-state :state state))

(defmethod us-state ((state (eql nil)))
  (make-instance 'us-state :state nil))

(defmethod us-state ((state us-state))
  (slot-value state 'state))

(defmethod render-form ((obj us-state) &rest keys &key inlinep
			slot-path intermediate-fields &allow-other-keys)
  (let* ((slot-name (attributize-name (last-item slot-path)))
	 (intermediate-value (assoc slot-name intermediate-fields :test #'string-equal)))
    (render-suggest slot-name *us-states* :value (if intermediate-value
						     (cdr intermediate-value)
						     (us-state obj)))))


(defmethod render-form-slot (obj slot-name (slot-value us-state) &rest keys
			     &key (human-name slot-name) validation-errors &allow-other-keys)
  (let* ((attribute-slot-name (attributize-name slot-name))
	 (validation-error (assoc attribute-slot-name validation-errors :test #'string-equal))
	 (field-class (concatenate 'string attribute-slot-name
				   (when validation-error " item-not-validated"))))
    (with-html
      (:li :class field-class
       (:label
	(:span (str (humanize-name human-name)) ":&nbsp;")
	(apply #'render-form slot-value keys)
	(when validation-error
	  (htm (:p :class "validation-error"
		   (:em
		    (:span :class "validation-error-heading" "Error:&nbsp;")
		    (str (format nil "~A" (cdr validation-error))))))))))))

  #|
    (with-html
      (:input :type "text" :name slot-name :value (if intermediate-value
								  (cdr intermediate-value)
								  obj)))))
|#
