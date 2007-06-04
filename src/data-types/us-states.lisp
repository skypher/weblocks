
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
  ((state :initarg :state
	  :type string)))

(defmethod us-state ((state string))
  (make-instance 'us-state :state state))

(defmethod us-state ((state us-state))
  (slot-value state 'state))

(defmethod render-form ((obj us-state) &rest keys &key inlinep
			slot-path intermediate-fields &allow-other-keys)
  (let* ((slot-name (attributize-name (last-item slot-path)))
	 (intermediate-value (assoc slot-name intermediate-fields :test #'string-equal)))
    (render-suggest slot-name *us-states*)))
#|
    (with-html
      (:input :type "text" :name slot-name :value (if intermediate-value
								  (cdr intermediate-value)
								  obj)))))
|#
