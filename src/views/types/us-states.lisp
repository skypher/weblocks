
(in-package :weblocks)

(export '(*us-states* *use-suggest-for-us-states* us-state
	  us-state-presentation us-state-presentation-use-suggest-p
	  us-state-presentation-input-id
	  us-state-presentation-choices-id us-state-parser))

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

(defclass us-state-presentation (input-presentation)
  ((use-suggest-p :initform *use-suggest-for-us-states*
		  :initarg :use-suggest-p
		  :accessor us-state-presentation-use-suggest-p
		  :documentation "If set to true, suggest snippet will
		  be used as a state input control. Otherwise,
		  dropdown will be used.")
   (input-id :initform (gensym)
	     :initarg :input-id
	     :accessor us-state-presentation-input-id
	     :documentation "An input ID passed to suggest or
	     dropdown.")
   (choices-id :initform (gensym)
	       :initarg :choices-id
	       :accessor us-state-presentation-choices-id
	       :documentation "A choices ID passed to suggest.")))

(defmethod render-view-field-value (value (presentation us-state-presentation) 
				    (field form-view-field) (view form-view) widget obj
				    &rest args &key intermediate-values field-info &allow-other-keys)
  (declare (ignore args))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (let ((selections (mapcar #'car *us-states*))
	  (default-value (if intermediate-value-p
			     intermediate-value
			     (append (ensure-list value)
				     (ensure-list
				      (cdr (assoc value *us-states-abreviation->name*
						  :test #'equalp))))))
	  (welcome-name "State"))
      (if (us-state-presentation-use-suggest-p presentation)
	  (render-suggest (if field-info
                            (attributize-view-field-name field-info)
                            (attributize-name(view-field-slot-name field)))
			  selections
			  :default-value default-value
			  :welcome-name welcome-name
			  :max-length (input-presentation-max-length presentation)
			  :input-id (us-state-presentation-input-id presentation)
			  :choices-id (us-state-presentation-choices-id presentation))
	  (render-dropdown (attributize-view-field-name field-info) selections
			   :selected-value default-value
			   :welcome-name welcome-name
			   :id (us-state-presentation-input-id presentation))))))

(defclass us-state-parser (parser)
  ((error-message :initform "a valid US state"))
  (:documentation "A parser designed to parse strings into
  a US state."))

(defmethod parse-view-field-value ((parser us-state-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (let ((value (string-trim +whitespace-characters+ value)))
    (if (empty-p value)
	(values t nil)
	(if (eq (length value) 2)
	    (when (us-state-p (string-upcase value))
	      (values t t (string-upcase value)))
	    (let ((state (assoc value *us-states* :test #'equalp)))
	      (when state
		(values t t (cdr state))))))))

;;; Scaffolding magic
(defmethod typespec->view-field-presentation ((scaffold form-scaffold)
					      (typespec (eql 'us-state)) args)
  (values t (make-instance 'us-state-presentation)))

(defmethod typespec->form-view-field-parser ((scaffold form-scaffold)
					     (typespec (eql 'us-state)) args)
  (values t (make-instance 'us-state-parser)))

(defmethod dependencies append ((self us-state-presentation))
  (when (us-state-presentation-use-suggest-p self)
    (list (make-local-dependency :stylesheet "suggest"))))
