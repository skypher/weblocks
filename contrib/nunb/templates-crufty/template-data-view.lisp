(in-package :cm)

; In data view, will ALWAYS render the field suffix of the view"
(defwidget template-dataform  (dataform)
  ())

; NEVER renders suffix, omits the normal dataform chrome too -- essentially, just renders the HTML-TEMPLATE"
(defwidget naked-template-form  (dataform)
  ())

(defwidget lagru-details  (template-dataform)
  ())

(defwidget lagru-form  (lagru-details)
  ())

; A dataform that lets us render a suffix function that can be a closure (see notes above)
(defwidget suffix-dataform (lagru-details)
  ((fields-suffix-fn :accessor get-fields-suffix-fn :initarg :fields-suffix-fn :initform (closure ))))

; As above, but has a modify button
(defwidget lagru-details-modifiable (suffix-dataform)
  ())

(defmethod initialize-instance :after ((s lagru-details-modifiable) &rest initargs &key fields-suffix-fn &allow-other-keys)
  (when fields-suffix-fn (break "Cannot make modifiable form if you want to use fields-suffix-fn!"))
  (when (slot-boundp s 'fields-suffix-fn) 
    (setf (get-fields-suffix-fn s)
	  (lambda (&rest args)
	     (declare (ignore args))
	     (with-html
	       (:div :class "submit"
		      (render-link (make-action
				     (lambda (&rest args)
				       (declare (ignore args))
				       (setf (slot-value s 'weblocks::ui-state) :form)))
				    "Modify"
				    :class "modify")))))))  

;-------- lagru-details and lagru-form
(defmethod render-dataform-data ((obj lagru-details) data view &rest args)
  (call-next-method)
  #+old(with-html (:div :class "lagru-details"
		   (call-next-method))))

(defmethod render-dataform-form ((obj lagru-form) data view &rest args)
  (call-next-method)
  ;(with-html (:div :class "lagru-details lagru-form"		   ))
  )
;-------- lagru-details and lagru-form


;  "Template dataform does not render the modify link, ever. However, it will render a field-suffix if defined in teh view (check TODO)"
(defmethod render-dataform-data ((obj template-dataform) data view &rest args)
  (apply #'render-object-view data view
	   :widget obj args))

; Suffix dataform should call view suffix from view obj and itself
(defmethod render-dataform-data ((obj suffix-dataform) data view &rest args)
    (apply #'render-object-view data view
	   :fields-suffix-fn (lambda (&rest args)
			       (safe-funcall (view-fields-default-suffix-fn view) args)
			       (safe-funcall (get-fields-suffix-fn obj)))
	   :widget obj
	   args))


;------------------- Scaffold and rendering machinery

(defclass templ-view (data-view)
  ((caption :initform "Templ View Title" :initarg :caption :accessor template-caption)
   (language :initform "it" :initarg :language :accessor template-language)
   (file :initform nil :initarg :file 
	 :accessor template-file 
	 :documentation "A file MUST be specified. This file will be looked up in the pub/templates/ directory"))
  (:documentation "A view designed to present a template filled with data to the user."))
 
(defclass templ-scaffold (scaffold)
  ())

(defclass templ-view-field (inline-view-field)
  ((presentation :initform (make-instance 'text-presentation)))
  (:documentation "A field class of the template data view."))

(defmethod view-caption ((view templ-view))
  (if (slot-value view 'caption)
      (slot-value view 'caption)
      (with-html-output-to-string (out)
	(:span :class "action" "Viewing:&nbsp;")
	(:span :class "object" "~A"))))

(defmacro string+ (&rest args)
  `(concatenate 'string ,@args))

(defmethod render-view-field ((field templ-view-field) (view templ-view) widget  presentation value obj &rest args)
  (concatenate 'string  "myfield"
	   (apply #'render-view-field-value
		  value presentation
		  field view widget obj
		  args)))


(defmethod render-view-field ((field templ-view-field) (view templ-view) widget (presentation text-presentation)  value obj &rest args) 
  (string+ "<p> myfield </p>"
	   (apply #'render-view-field-value
	 value presentation
	 field view widget obj
	 args)))


(let ((keyword-package (find-package :keyword)))
  (defun x-symbol-to-keyword (symbol)
    (intern (symbol-name symbol) keyword-package)))



(defun format-weblocks-render-into-string (view widget field-info)
  (declare (special *weblocks-output-stream*))  
  (let ((field (field-info-field field-info))
	(flobj (field-info-object field-info))
	(*weblocks-output-stream* (make-string-output-stream)))
    (safe-apply (view-field-prefix-fn field) view field flobj)
    (apply #'render-view-field
	   field view widget (view-field-presentation field)
	   (obtain-view-field-value field flobj) flobj (list :field-info field-info))
    (safe-apply (view-field-suffix-fn field) view field flobj)
    (get-output-stream-string *weblocks-output-stream*)))


;; How it's usually called.
;;  (let ((field (field-info-field field-info))
;; 	  (obj (field-info-object field-info)))
;; 			    (safe-apply (view-field-prefix-fn field) view field obj args)
;; 			    (apply #'render-view-field
;; 				   field view widget (view-field-presentation field)
;; 				   (obtain-view-field-value field obj) obj
;; 				   :field-info field-info
;; 				   args)
;; 			    (safe-apply (view-field-suffix-fn field) view field obj args))

(defun gather-field-symbol-render-list (view obj widget)
  (let ((a-lambda (lambda (field-info)
		    (list (x-symbol-to-keyword (view-field-slot-name (field-info-field field-info))) 
			  (format-weblocks-render-into-string view widget field-info)))))
    (map-view-fields a-lambda view obj)))


(defun v-insert-rendered-template (v o w args)
  (declare (special *out-of-band-template-vars*))
  (warn (format nil "template display oob at use ~A" *out-of-band-template-vars*))
  (let ((field-symbol-render-list (flatten (gather-field-symbol-render-list v o w))))
    (with-html (htm (:div :class "template" 
			  (render-widget-body (fill-template-widget (template-file v)
								    :language (template-language v)
								    :assoc field-symbol-render-list
								    :assoc2 *out-of-band-template-vars*)))))))

;-- used when we want to hide tags, eg in templates
(defmacro with-extra-tags-hidden (&body body)
  `(progn
     ,@body))




(defmethod with-view-header ((view templ-view) obj widget body-fn &rest args &key
			     (fields-prefix-fn (view-fields-default-prefix-fn view))
			     (fields-suffix-fn (view-fields-default-suffix-fn view))
			     &allow-other-keys)
  (with-html
    (:div :class (format nil "view data ~A"
			 (attributize-name (object-class-name obj)))
	  (with-extra-tags-hidden
	    (htm
	     (:h1 (fmt (view-caption view)
		       (humanize-name (object-class-name obj))))
	     (safe-apply fields-prefix-fn view obj args)
	     ;(:ul (apply body-fn view obj args))
	     ;-- replaced by --
	     ;insert-rendered-template
	     (v-insert-rendered-template view obj widget args)
	     ; end changes -- well, we don't want to see modify link either
	     (safe-apply fields-suffix-fn view obj args))))))



(defmethod with-view-header ((view templ-view) obj (widget naked-template-form) body-fn &rest args &key
			     (fields-prefix-fn (view-fields-default-prefix-fn view))
			     (fields-suffix-fn (view-fields-default-suffix-fn view))
			     &allow-other-keys)
  (with-html
    (v-insert-rendered-template view obj widget args)))


