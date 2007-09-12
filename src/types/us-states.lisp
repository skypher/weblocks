
(in-package :weblocks)

(export '(*us-states* *use-suggest-for-us-states* us-state))

(defparameter *us-states*
  '(("Alabama" . "AL") ("Alaska" . "AK") ("Arizona" . "AZ") ("Arkansas" . "AR")
    ("California" . "CA") ("Colorado" . "CO") ("Connecticut" . "CT")
    ("Delaware" . "DE") ("Florida" . "FL") ("Georgia" . "GA") ("Hawaii" . "HI")
    ("Idaho" . "ID") ("Illinois" . "IL") ("Indiana" . "IN") ("Iowa" . "IA")
    ("Kansas" . "KS") ("Kentucky" . "KY") ("Louisiana" . "LA") ("Maine" . "ME")
    ("Maryland" . "MD") ("Massachusetts" . "MA") ("Michigan" . "MI")
    ("Minnesota" . "MN") ("Mississippi" . "MS") ("Missouri" . "MO")
    ("Montana" . "MT") ("Nebraska" . "NE") ("Nevada" . "NV")
    ("New Hampshire" . "NH") ("New Jersey" . "NJ") ("New Mexico" . "NM")
    ("New York" . "NY") ("North Carolina" . "NC") ("North Dakota" . "ND")
    ("Ohio" . "OH") ("Oklahoma" . "OK") ("Oregon" . "OR") ("Pennsylvania" . "PA")
    ("Rhode Island" . "RI") ("South Carolina" . "SC") ("South Dakota" . "SD")
    ("Tennessee" . "TN") ("Texas" . "TX") ("Utah" . "UT") ("Vermont" . "VT")
    ("Virginia" . "VA") ("Washington" . "WA") ("West Virginia" . "WV")
    ("Wisconsin" . "WI") ("Wyoming" . "WY"))
  "A alist of us state names and their abbreviations.")

(defparameter *us-states-abreviation->name*
  (mapcar (lambda (state)
	    (cons (cdr state) (car state)))
	  *us-states*)
  "An alist of us state abbreviations mapped to state names.")

(defparameter *use-suggest-for-us-states* t
  "If true, suggest snippet will be used to render us-state types in
forms. A simple dropdown will be used otherwise.")

(defun us-state-p (str)
  "Used by the type specifier 'us-state' to determine if a string is a
state."
  (not (null (member str *us-states* :test #'string= :key #'cdr))))

(deftype us-state ()
  '(satisfies us-state-p))

(defslotmethod render-form-aux (obj slot-name (slot-type (eql 'us-state)) slot-value &rest
				    keys &key inlinep slot-path intermediate-fields &allow-other-keys)
  (let* ((intermediate-value (slot-intermedia-value slot-name intermediate-fields))
	 (selections (mapcar #'car *us-states*))
	 (default-value (if intermediate-value
			    (cdr intermediate-value)
			    (append (ensure-list slot-value)
				    (ensure-list
				     (cdr (assoc slot-value *us-states-abreviation->name*
						 :test #'equalp))))))
	 (welcome-name (cons "State" "")))
    (if *use-suggest-for-us-states*
	(render-suggest slot-name
			selections
			:default-value default-value
			:welcome-name welcome-name
			:max-length (max-raw-slot-input-length obj slot-name slot-type))
	(render-dropdown slot-name selections :selected-value default-value
			 :welcome-name welcome-name))))

(defslotmethod parse-slot-from-request ((slot-type (eql 'us-state)) slot-name request-slot-value)
  (setf request-slot-value (string-trim +whitespace-characters+ request-slot-value))
  (when (not (empty-p request-slot-value))
    (if (eq (length request-slot-value) 2)
	(values t (string-upcase request-slot-value))
	(let ((state (assoc request-slot-value *us-states* :test #'equalp)))
	  (when state
	    (values t (cdr state)))))))

(defslotmethod invalid-input-error-message (obj slot-name humanized-name (slot-type (eql 'us-state))
						parsed-request-slot-value)
  "Please enter a valid US State.")

