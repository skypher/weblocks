
(in-package :weblocks)

(export '(update-object-view-from-request
	  request-parameters-for-object-view
          request-parameter-for-presentation))

(defgeneric update-object-view-from-request (obj view &rest args
						 &key class-store satisfies
						 &allow-other-keys)
  (:documentation "Parses view fields from request into a given
object. The 'form-view-field-parser' slot of each field is used to
parse a string obtained from the request into an appropriate
value. The generic function 'parse-view-field-value' is called to
invoke the parser.

If this function succeeds updating the object with all fields obtained
from the request, it returns true. Otherwise returns nil as the first
value, and an association list of fields and errors as the second
value.

The function uses 'form-view-persist-p' to determine whether the
object is to be persisted. If so, it calls 'persist-object'.

Specialize this function to parse given objects differently.")
  (:method (obj view &rest args
	    &key class-store satisfies
	    &allow-other-keys)
    (labels ((parse-object-view-from-request (obj view)
	       "Parses an object from request. If parsed successfully,
               returns true as the first value and an association list
               of field-info structures and parsed values as the
               second value. Otherwise, returns nil as the first value
               and an association list of fields and error messages as
               the second value."
	       (let (results errors)
		 (apply #'map-view-fields
			(lambda (field-info)
			  (let* ((field (field-info-field field-info))
				 (obj (field-info-object field-info))
				 (field-key (attributize-view-field-name field-info))
				 (field-value (request-parameter-for-presentation field-key
                                                                                  (view-field-presentation field))))
			    (when (typep (view-field-presentation field) 'form-presentation)
			      (multiple-value-bind (parsedp presentp parsed-value)
				  (apply #'parse-view-field-value (form-view-field-parser field)
					 field-value obj view field
					 :field-info field-info args)
				(if parsedp
				    (if (not presentp)
					(if (form-view-field-required-p field)
					    (push (cons field (get-required-error-msg field))
						  errors)
					    (push (cons field-info nil) results))
					(push (cons field-info parsed-value) results))
				    (push (cons field (parser-error-message
						       (form-view-field-parser field)))
					  errors))))))
			view obj args)
		 (if errors
		     (values nil errors)
		     (values t results))))
	     (write-value (field value obj)
	       "Writes a field value into object's slot."
	       (if (slot-boundp field 'writer)
		   (funcall (form-view-field-writer field) value obj)
		   (when (view-field-slot-name field)
		     (let* ((writer-name (car
					  (slot-definition-writers
					   (find-slot-dsd (class-of obj)
							  (view-field-slot-name field)))))
			    (writer (when writer-name (fdefinition writer-name))))
		       (if writer
			   (funcall writer value obj)
			   (setf (slot-value obj (view-field-slot-name field))
				 value))))))
	     (deserialize-object-from-parsed-values (parsed-values &key write-delayed)
	       "Accepts an an association list of field-info
               structures and parsed-values, and records each parsed
               value in the corresponding field's object slot."
	       (mapc (lambda (field-info-parsed-value-pair)
		       (destructuring-bind (field-info . parsed-value)
			   field-info-parsed-value-pair
			 (let ((field (field-info-field field-info))
			       (obj (field-info-object field-info)))
			   (when (typep (view-field-presentation field) 'form-presentation)
			     (if (form-view-field-writer-delayed-p field)
				 (when write-delayed
				   (write-value field parsed-value obj))
				 (unless write-delayed
				   (write-value field parsed-value obj)))))))
		     parsed-values))
	     (persist-object-view (obj view field-info-list)
	       "Persists an object view to the backend store. If the
               object has any fields mixed in, persists them first (if
               necessary)."
	       (apply #'map-mixin-fields
		      (lambda (field-info)
			(let* ((field (field-info-field field-info))
			       (mixin-obj (field-info-object
					   (find-if (lambda (item)
						      (when (field-info-parent-info item)
							(eq (field-info-field
							     (field-info-parent-info item))
							    field)))
						    field-info-list))))
			  (when (mixin-form-view-field-persist-p field)
			    (persist-object-view mixin-obj (mixin-view-field-view field)
						 field-info-list))
			  (write-value field mixin-obj obj)))
		      view obj args)
	       (persist-object (or class-store (object-store obj)) obj)))
      ;; Parse, validate, deserialize, and persist the object view
      (multiple-value-bind (parsep results)
	  (parse-object-view-from-request obj view)
	(if parsep
	    (multiple-value-bind (validatesp errors)
		(validate-object-form-view obj view results)
	      (if validatesp
		  (progn
		    (deserialize-object-from-parsed-values results)
		    (multiple-value-bind (success errors)
			(or (null satisfies) (funcall satisfies obj))
		      (if success
			  (progn
			    (when (form-view-persist-p view)
			      (persist-object-view obj view (mapcar #'car results)))
			    (deserialize-object-from-parsed-values results :write-delayed t)
			    t)
			(values nil errors))))
		(values nil errors)))
	    (values nil results))))))

(defun request-parameters-for-object-view (view &rest args)
  "Returns request parameters taking into account a particular view of
an object. This function is necessary because in certain cases web
browsers don't send some parameters (e.g. unchecked checkboxes). This
function returns an association list of fields and values found in the
request.

'view' - view to take account of.
'obj' - object to take account of."
  (apply #'map-view-fields
	 (lambda (field-info)
	   (let ((field (field-info-field field-info)))
	     (when (typep (view-field-presentation field) 'form-presentation)
	       (let* ((slot-name (view-field-slot-name field))
		      (slot-key (attributize-view-field-name field-info))
		      (request-slot-value (request-parameter-for-presentation
					   slot-key (view-field-presentation field))))
		 (cons field request-slot-value)))))
	 (find-view view) nil args))

(defgeneric request-parameter-for-presentation (name presentation)
  (:documentation "Answer HTTP request parameter NAME, but
  preprocessed for PRESENTATION.")
  (:method (name presentation)
    (declare (ignore presentation))
    (request-parameter name)))
