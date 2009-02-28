(in-package :cm)

;; (defparameter *template-temporary-validation-errors* nil)
;; (defparameter *template-temporary-intermediate-values* nil)
;; (defparameter *out-of-band-template-vars* nil)

;; A generic function that allows the user to 
(defgeneric munge-validation-summary (view obj widget validation-errors))

(defclass templform-view (form-view)
  ((caption :initform "Template Form View" :initarg :caption :accessor template-caption)
   (language :initform "it" :initarg :language :accessor template-language)
   (file :initform nil :initarg :file 
	 :accessor template-file 
	 :documentation "A file MUST be specified. This file will be looked up in the pub/templates/languages/ directory"))
  (:documentation "A view designed to present a template filled with data to the user."))

(defclass templform-scaffold (scaffold)
  ())

(defclass templform-view-field (form-view-field)
  ()
  (:documentation "A field class of the data view."))

(defmethod view-default-field-type ((view-type (eql 'templform)) (field-type (eql 'mixin)))
  'mixin-form)

(defun trim-value (v)
  (string-trim " " v))

; An around method to trim value -- must not change types of formal params!
; parse-view-field-value (parser value obj view field &rest args)
(defmethod parse-view-field-value :around ((parser text-parser) value obj view field &rest args)
  (call-next-method parser (trim-value value) obj view field args))

(defmethod view-caption ((view templform-view))
  (if (slot-value view 'caption)
      (slot-value view 'caption)))

;-- form rendered fields are more complex than data view fields, because for example
;   they contain intermediate values, error texts etc. so the same logic
;   as was used for simple view fields won't work.
;   (defmethod render-view-field ((field templform-view-field) (view templform-view)
	 
(let ((keyword-package (find-package :keyword)))
  (defun fv-symbol-to-keyword (symbol)
    (intern (symbol-name symbol) keyword-package)))

(defun fv-format-weblocks-render-into-string (view widget field-info
					      &rest args  &key
			     (method (form-view-default-method view))
			     (action (form-view-default-action view))
			     (fields-prefix-fn (view-fields-default-prefix-fn view))
			     (fields-suffix-fn (view-fields-default-suffix-fn view))
			     &allow-other-keys)
  (declare (special *weblocks-output-stream*))
  ;(break (format nil "Args were ~A" args))
  (let ((field (field-info-field field-info))
	(flobj (field-info-object field-info))
	(*weblocks-output-stream* (make-string-output-stream))) 
    #+OLD(safe-apply (view-field-prefix-fn field) view field flobj)
    (apply #'render-view-field
	   field view widget (view-field-presentation field)
	   (obtain-view-field-value field flobj) flobj
	   ;(list :field-info field-info )
	   ;:validation-errors validation-errors
	   nil)
    #+OLD(safe-apply (view-field-suffix-fn field) view field flobj)
    (get-output-stream-string *weblocks-output-stream*)))

(defun old-fv-format-weblocks-render-into-string (view widget field-info)
  (declare (special *weblocks-output-stream*))  
  (let ((field (field-info-field field-info))
	(flobj (field-info-object field-info))
	(*weblocks-output-stream* (make-string-output-stream))) 
    (apply #'render-view-field
	   field view widget (view-field-presentation field)
	   (obtain-view-field-value field flobj) flobj (list :field-info field-info))
    (get-output-stream-string *weblocks-output-stream*)))

(defun fv-gather-field-symbol-render-list (view obj widget &rest args  &key
			     (method (form-view-default-method view))
			     (action (form-view-default-action view))
			     (fields-prefix-fn (view-fields-default-prefix-fn view))
			     (fields-suffix-fn (view-fields-default-suffix-fn view))
			     &allow-other-keys)
  (let ((a-lambda (lambda (field-info)
		    (list (fv-symbol-to-keyword (view-field-slot-name (field-info-field field-info))) 
			  (fv-format-weblocks-render-into-string view widget field-info)))))
    (map-view-fields a-lambda view obj :include-invisible-p nil)))

(defun fv-insert-rendered-template (view o w &rest args &key
			     (method (form-view-default-method view))
			     (action (form-view-default-action view))
			     (fields-prefix-fn (view-fields-default-prefix-fn view))
			     (fields-suffix-fn (view-fields-default-suffix-fn view))
			     &allow-other-keys)
  (declare (special *out-of-band-template-vars*))
  (let ((field-symbol-render-list (flatten (fv-gather-field-symbol-render-list view o w))))
    (warn (format nil "-- This is the point where template gets filled! OOB is ~A" *out-of-band-template-vars*))
    (warn (format nil "-- Rest of tmpl_vars are ~{~A-->~A  ~}" field-symbol-render-list))
    (with-html (htm (:div :class "template"
			  (render-widget-body (fill-template-widget (template-file view)
								    :language (template-language view)
								    :assoc field-symbol-render-list
								    :assoc2 *out-of-band-template-vars*)))))))



;;; Implement rendering protocol
(defmethod with-view-header ((view templform-view) obj widget body-fn &rest args &key
			     (method (form-view-default-method view))
			     (action (form-view-default-action view))
			     (fields-prefix-fn (view-fields-default-prefix-fn view))
			     (fields-suffix-fn (view-fields-default-suffix-fn view))
			     validation-errors
			     intermediate-values
			     &allow-other-keys)
  (declare (special *on-ajax-complete-scripts* *form-submit-dependencies* *out-of-band-template-vars*))
  (warn "My special template rendering takes place (with-view-header)")
  (let ((form-id (gen-id))
	(header-class (format nil "view form ~A"
			      (attributize-name (object-class-name obj)))))
    ; DOES NOT WORK (send-script (format nil "document.onkeypress = function(){if( window.event.keyCode == 13 ){return false;}}"))
    (when (>= (count-view-fields view)
	      (form-view-error-summary-threshold view))
      (setf header-class (concatenate 'string header-class " long-not form")))
    (let ((form-body
	   (let ((*weblocks-output-stream* (make-string-output-stream))
		 (*template-temporary-validation-errors* validation-errors)
		 (*template-temporary-intermediate-values* intermediate-values))
	     (with-html
	       (:h1 (fmt (view-caption view)
			 (humanize-name (object-class-name obj))))
	       ; we don't want this -- well, now we do!
	       (render-validation-summary view obj widget validation-errors)

	       ;the former gives user more control thru munge-validation summary ..
	       (setf *out-of-band-template-vars* (munge-validation-summary view obj widget validation-errors))
	       ;(setf *out-of-band-template-vars* (my-alist->plist validation-errors))

	       (:h2 :class "form-fields-title" "Form fields:")
	       (format t "~%~% --- Form template rendering time (with-view-header) ---- ~% validation errors ~A ~% intermediate-values ~A ~%" validation-errors intermediate-values)
	       (format t "~% template temporary errors ~A ~%" *template-temporary-validation-errors*)
	       (safe-apply fields-prefix-fn view obj args)
	       ;(:ul (apply body-fn view obj args))
	       ; -- replaced by --
	       ;insert-rendered-template
	       (fv-insert-rendered-template view obj widget)
	       ; end changes
	       (safe-apply fields-suffix-fn view obj args)
	       (apply #'render-form-view-buttons view obj widget args)
	       (get-output-stream-string *weblocks-output-stream*)))))
      (with-html-form (method action
			      :id form-id ;we *always* want the id
			      :class header-class
			      :enctype (form-view-default-enctype view)
			      :extra-submit-code (weblocks::render-form-submit-dependencies *form-submit-dependencies*)
			      :use-ajax-p (form-view-use-ajax-p view))
	(write-string form-body *weblocks-output-stream*)))
     (when 1 ;(form-view-focus-p view)
       (send-script (format nil "(function() { $('~A').observe('keypress', function(e){ var el = e.element(); if( e.keyCode == 13 ){return false;} } ); }) ();" form-id))
       (send-script (ps* `(.focus-first-element ($ ,form-id)))))))


;; (apply #'render-view-field
;; 	   field view widget (view-field-presentation field)
;; 	   (obtain-view-field-value field flobj) flobj (list :field-info field-info)
;; 	   args)

(defmethod render-view-field ((field templform-view-field) (view templform-view)
			      widget presentation value obj 
			      &rest args &key validation-errors &allow-other-keys)
  ;(break "I was called?")
  (declare (special *template-temporary-validation-errors*))
  (let* ((attribute-slot-name (attributize-name (view-field-slot-name field)))
	 (validation-error (assoc field *template-temporary-validation-errors*))
	 (intermediate-values *template-temporary-intermediate-values*)
	 (field-class (concatenate 'string attribute-slot-name
				   (when validation-error " item-not-validated"))))
    ;(format t "########## we have ############### args are: ~% ~A ~% " args)
    (with-html
      (:div :class (if validation-error "validation-error-scrollto" "validation-error-scrolltoNOT"))
	    
      (:label :class (attributize-presentation
			   (view-field-presentation field))
		   (:span :class "slot-name"
			  (:span :class "extra"
				 ;(str (view-field-label field)) ":&nbsp;"
				 (when (form-view-field-required-p field)
				   (htm (:em :class "required-slot" ;"(required)&nbsp;"
					     )))))

		   (:span :class
			  (if validation-error
			    "validation-error-site"
			    "regular-field")
				 (if intermediate-values
				     (progn
				       (apply #'render-view-field-value
					      value presentation
					      field view widget obj
					      (list :intermediate-values intermediate-values)))
				     (apply #'render-view-field-value
					    value presentation
					    field view widget obj
			  args)))
			   
		 		   (when validation-error
		     ;(break "yay! Validation error")
		     (htm (:p :class "validation-error"
			      (:em
			       (:span :class "validation-error-heading" "Error:&nbsp;")
			       (str (format nil "~A" (cdr validation-error)))))))))))

(defmethod render-view-field ((field mixin-form-view-field) (view templform-view)
			      widget presentation value obj 
			      &rest args &key validation-errors &allow-other-keys)
  (declare (special *template-temporary-validation-errors*))
  (let* ((attribute-slot-name (attributize-name (view-field-slot-name field)))
	 ;(validation-error (assoc field validation-errors))
	 (validation-error (assoc field *template-temporary-validation-errors*))
	 (intermediate-values *template-temporary-intermediate-values*)
	 (field-class (concatenate 'string attribute-slot-name
				   (when validation-error " item-not-validated"))))
    (warn     (format nil "Field ~A has error ~A ~%" field (assoc field validation-errors)))
    (with-html
      (:div :class (if validation-error "validation-error-scrollto" "validation-error-scrolltoNOT") "&nbsp;" )
      
      (:label :class (attributize-presentation
			   (view-field-presentation field))
		   (:span :class "slot-name"
			  (:span :class "extra"
				 ;(str (view-field-label field)) ":&nbsp;"
				 (when (form-view-field-required-p field)
				   (htm (:em :class "required-slot"
					     ;"(required)&nbsp;"
					     )))))

		   (:span :class
			  (if validation-error
			    "validation-error-site"
			    "regular-field")
				 (if intermediate-values
				     (progn
				       (apply #'render-view-field-value
					      value presentation
					      field view widget obj
					      (list :intermediate-values intermediate-values)))
				     (apply #'render-view-field-value
					    value presentation
					    field view widget obj
			  args)))
		   
		   (when validation-error
		     (htm (:p :class "validation-error"
			      (:em
			       (:span :class "validation-error-heading" "Error:&nbsp;")
			       (str (format nil "~A" (cdr validation-error)))))))))))

(defmethod template-render-view-field-value (value (presentation input-presentation)
				    field (view templform-view) widget obj
				    &rest args)
  (let ((attributized-slot-name (attributize-name (view-field-slot-name field)))
	(intermediate-values *template-temporary-intermediate-values*))
    (multiple-value-bind (intermediate-value intermediate-value-p)
	(form-field-intermediate-value field intermediate-values)
      (with-html
	  (:input :type "text" :name attributized-slot-name
		  :value (if intermediate-value-p
			     intermediate-value
			     (apply #'print-view-field-value value presentation field view widget obj args))
		  :maxlength (input-presentation-max-length presentation))))))

 

;;; -------------------------------------------------------------
;;;   Changes made as a result of new validation requests by Alex Z
;;;   approach followed is this:
;;;        1  if there is a single field error (for any number of fields) then show that err message below that field
;;;        2  once those errors no longer exist, compare fields (eg, at least 1 phone present)
;;;        3  in order to enable the above test, we must allow blanks in phone/email validators!
;;;        4  finally we have the quickform satisfies but we aren't using that.
;;;        5  now the issue is in step 2, the framework allows only a single error message at a global level
;;;        6  but we want the ability to display "enter at least one phone" below the field phone1, phone2
;;;           => iow, the form view validator *should* generate field validation errors! :-) 
;;;        7  these changes below are intended to allow that: we'll see. 
;;; -------------------------------------------------------------


;This should really be specialized on special-validation-templform-view or similar
 (defmethod render-validation-summary ((view templform-view) obj widget errors)
    (declare (ignore view obj))
    ;(break "my validation summary called")
    (when errors
      ;  new Effect.Highlight ( $$('.validation-error-site')[0] ); REMOVED new Effect.Highlight ( $$('.validation-error')[0] );
      (send-script " $$('.validation-error-scrollto')[0].scrollTo(); "))
    (when errors ;
      (and nil errors)
      (let ((non-field-errors (find-all errors #'null :key #'car))
	    (field-errors (find-all errors (compose #'not #'null) :key #'car)))
	(with-html
	  (:div :class "validation-errors-summary"
		#+old(:h2 :class "error-count"
		     (let ((error-count (length errors)))
		       (if (eql error-count 1)
			   (str (format nil "There is 1 validation error:"))
			   (str (format nil "There are ~S validation errors:" error-count)))))
		(when non-field-errors
		  (htm
		   (:ul :class "non-field-validation-errors"
			(mapc (lambda (err)
				(with-html
				  (:li
				   (str (format nil "~A" (cdr err))))))
			      non-field-errors))))
		(when (and nil field-errors)
		  (htm
		   (:ul :class "field-validation-errors"
			(mapc (lambda (err)
				(with-html
				  (:li
				   (str (format nil "~A" (cdr err))))))
			      field-errors)))))))))

(defun nunb-run-view-validators (obj validators fields-values)
"A customized version of weblocks fn. Accepts extra arg, parsed-values, which is a list of (fieldinfo. parsed-value) pairs.. we use this
to allow the form-view validator/satisfies to add/remove error messages on a per field basis, similar to code in dolist of the fn (below)
validate-object-form-view"
  (let ((validates t)
	errors)
    (warn (format nil "~%~% nunb-run-view-validators obj is ~A ~%~% " obj))
    (dolist (validator-function validators)
      (multiple-value-bind (validatesp error-lst)
	  (apply validator-function obj fields-values)
	(unless validatesp
	  (setf validates nil)
	  ; was (push-end (cons nil error) errors)
	  (setf errors (append error-lst errors))
	  )))
    (warn "nunb-run-view-validators errors are ~A ~% .. and valid is ~A" errors validates)
    (if validates
	t
	(values nil errors))))

(defun nunb-make-error-field-symbol (asymbol)
  ;(warn (format nil "nb err fld symb saw ~A " asymbol))
  (string->symbol (format nil "~A-field" asymbol)))

(defmethod  validate-object-form-view
  (object (view templform-view) parsed-values)  ;This should really be specialized on special-validation-templform-view or similar
	    (let ((validates t)
		  errors
		  fields-values)
	      (dolist (info-value-pair parsed-values)
		(destructuring-bind (field-info . parsed-value)
		    info-value-pair
		  (multiple-value-bind (validatesp error)
		      (let ((field (field-info-field field-info))
			    (object (field-info-object field-info)))
			(push parsed-value fields-values)
			(push (weblocks::symbol-to-keyword (view-field-slot-name field)) fields-values)
			; ** see explanation below
			;    list of (field-symbol, field-value) are normally passed by weblocks to the validators
			;    we go one step further, we also add the pair (field-made-up-symbol, field-itelf)
			; ** see bottom of file for how it's used.
			
			; 2 lines added by nunb: allow a symbol like :phone2-field to refer to the field itself in the argslist to form-validator
			(push field fields-values)
			(push (weblocks::symbol-to-keyword (nunb-make-error-field-symbol (view-field-slot-name field))) fields-values)
			; -- upto here nunb
			(validate-form-view-field (view-field-slot-name field)
						  object field view parsed-value))
		    (unless validatesp
		      (setf validates nil)
		      (push-end (cons (field-info-field field-info) error) errors)))))
	      ;; We proceed to view-level validation only if individual fields were
	      ;; successfully validated.
	      (if validates
		  (if (form-view-satisfies view)
		      (nunb-run-view-validators object (form-view-satisfies view) fields-values ) 
		      t)
		  (values nil errors))))


;; How the scheme is used.
;; Normally the validator would get a keyword param list like (:field1 value1 :field2 value2 :field3 value3)
;; on the basis of this the validator can validate and create error messages: ((nil . message1)   (nil . message2)) assuming field3 has no error.

;; In our case, we get access to the field too: (field1 value1 field1symbol actual-field1 .. &rest )

;; Normally, validators can only add error messages of the type (nil . "message") to the list of error messages.
;; This is different from what the field-validators (satisfies functions -- see #'form-view-field-satisfies) are able to do.

;; The form-view-field-satisfies aka satisfies functions have access to the field object, and can push a pair (field-object, error-message) onto the validation errors list.
;; This in turn means the error message can be picked out by render-view-field and shown during field rendering (typically just below the field in error).

;; By giving view level validators access to the field objects, we can have high level validation that nonetheless points the blame at specific fields.

;; --- And now, for a further enhancement .. what if the error occurred elsewhere on the page, say in a template?

;; We then need to populate (say) error messages of type (nil . "message") into some kind of template out of band list, or else, all messages not consumed by a field ..
;; or a message (symbol . "message") is translated into a html-template assoc list (:symbol "message")

;; note: due to html-template behavior, a tmpl_var with no corresponding value in the assoc list is rendered as empty.