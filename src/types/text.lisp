
(in-package :weblocks)

(export '(text *textarea-rows* textarea-rows *textarea-cols*
	  textarea-cols *text-data-output-mode* text-data-output-mode
	  *text-data-cutoff-threshold* text-data-cutoff-threshold
	  *max-text-input-length*))

(deftype text () 'string)

;;; textarea rows
(defparameter *textarea-rows* 5
  "Default number of rows rendered in textarea")

(defgeneric textarea-rows (obj slot-name slot-type)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "Returns the suggested number of textarea rows. Default
implementation returns the value of *textarea-rows*."))

(defslotmethod textarea-rows (obj slot-name slot-type)  
  *textarea-rows*)

;;; textarea columns
(defparameter *textarea-cols* 20
  "Default number of columns rendered in textarea")

(defgeneric textarea-cols (obj slot-name slot-type)
  (:generic-function-class slot-management-generic-function)
  (:documentation
   "Returns the suggested number of textarea columns. Default
implementation returns the value of *textarea-cols*."))

(defslotmethod textarea-cols (obj slot-name slot-type)  
  *textarea-cols*)

;;; text data output mode
(defparameter *text-data-output-mode* :cutoff
  "When a large amount of text is presented as data, two modes could
be used - :cutoff or :paragraph. In :cutoff mode, the text is cut off
at *text-cutoff-threshold* and ellipsis is inserted. In :paragraph
mode, the text is wrapped in an HTML paragraph tag. This parameter
defines the default mode to be used for text slots.")

(defgeneric text-data-output-mode (obj slot-name)
  (:documentation
   "Returns the data output mode for the text slot. Should return
either :cufoff or :paragraph. Default implementation returns the value
of *text-data-output-mode*."))

(defmethod text-data-output-mode (obj slot-name)  
  *text-data-output-mode*)

;;; text cutoff threshold
(defparameter *text-data-cutoff-threshold* 15
  "In the ellipsis mode, the number of characters to be rendered
before the text is cut off and an ellipsis is inserted.")

(defgeneric text-data-cutoff-threshold (obj slot-name)
  (:documentation
   "Returns the number of characters before the text is cut off and an
ellipsis is inserted in the :cutoff mode. Default implementation
returns the value of *text-data-cutoff-threshold*."))

(defmethod text-data-cutoff-threshold (obj slot-name)  
  *text-data-cutoff-threshold*)

;;; input length server check
(defparameter *max-text-input-length* 200
  "Maximum number of characters that can be entered before the server complains.")

(defslotmethod max-raw-slot-input-length (obj slot-name (slot-type (eql 'text)))
  *max-text-input-length*)

;;; rendering text in a form
(defslotmethod render-form-value (obj slot-name (slot-type (eql 'text)) slot-value &rest
				    keys &key slot-path intermediate-fields &allow-other-keys)
  (let ((attributized-slot-name (attributize-name (if slot-name slot-name (last-item slot-path))))
	(intermediate-value (slot-intermedia-value slot-name intermediate-fields)))
    (render-textarea attributized-slot-name  
		     (if intermediate-value
			 (cdr intermediate-value)			
			 (apply #'form-print-object obj slot-name
				slot-type slot-value keys)) 
		     (textarea-rows obj slot-name slot-type)  
		     (textarea-cols obj slot-name slot-type))))

(defslotmethod form-print-object (obj slot-name (slot-type (eql 'text)) slot-value
				      &rest args)
  "Used to dispatch rendering slot-value to data-print-object
with (slot-type standard-object)"
  (when slot-value
    (apply #'data-print-object obj slot-name t slot-value args)))

;;; rendering text as data
(defslotmethod render-data-value (obj slot-name (slot-type (eql 'text)) slot-value
				      &rest keys &key highlight &allow-other-keys)
  (when (null slot-value)
    (call-next-method)
    (return-from render-data-value))
  (let* ((item (apply #'data-print-object obj slot-name slot-type slot-value keys))
	 (lit-item (if highlight
		       (highlight-regex-matches item highlight)
		       (escape-for-html item))))
    (ecase (text-data-output-mode obj slot-name)
      (:cutoff (with-html
		 (:span :class "value"
			(str lit-item)
			(unless (<= (length slot-value)
				    (text-data-cutoff-threshold obj slot-name))
			  (htm (:span :class "ellipsis" "..."))))))
      (:paragraph (with-html
		    (:p :class "value text"
			(str (apply #'concatenate 'string
				    (intersperse (tokenize-string lit-item
								  :delimiter #\Newline
								  :include-empties? t)
						 "<br />")))))))))

(defslotmethod data-print-object (obj slot-name (slot-type (eql 'text)) slot-value
				      &rest args)
  "Renders maximum amount of characters allowed for the slot plus
... or the full length of the slot if it is smaller than the maximum
length allowed"
  (ecase (text-data-output-mode obj slot-name)
    (:cutoff (let ((threshold (text-data-cutoff-threshold obj slot-name)))
	       (format nil "~A"
		       (if (<= (length slot-value) threshold)
			   slot-value
			   (subseq slot-value 0 threshold)))))
    (:paragraph (call-next-method))))

