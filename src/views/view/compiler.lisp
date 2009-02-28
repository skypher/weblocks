
(in-package :weblocks)

(export '(*custom-view-field-argument-compilers*
	  view-argument-quoting-strategy defview-anon defview))

;;; Declarative view definition implementation
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Note, this is in a separate eval-when block so that recompiling
  ;; the block below won't reset the parameter.
  (defparameter *custom-view-field-argument-compilers* (make-hash-table)
    "A hash map of keywords, each mapped to a compilation
    function. When a keyword is encountered in a field definition
    during compilation, the corresponding compilation function is
    called and the emitted code is used for field initialization. The
    function must accept slot name symbol and initialization
    declaration."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric view-argument-quoting-strategy (arg-name)
    (:documentation "A quoting strategy for an argument of a given
    name. Possible values are :quote, in which case the argument is
    always quoted; :list, in which case argument is quoted but if it
    is a list, its elements aren't quoted; and :none, in which case
    argument isn't quoted. Default implementation returns :none.")
    (:method (arg-name)
      :none))
  
  (defun quote-property-list-arguments (proplist)
    "Quotes the arguments of a property list."
    (flet ((quote-if-necessary (value)
	     (if (or (listp value)
		     (and (symbolp value)
			  (not (keywordp value))))
		 `(quote ,value)
		 value)))
      (loop for (key . (value)) on proplist by #'cddr
	 collect key
	 collect (ecase (view-argument-quoting-strategy key)
		   (:quote (quote-if-necessary value))
		   (:list (if (listp value)
			      (cons 'list value)
			      (quote-if-necessary value)))
		   (:none value)))))

  (defun compile-view-fields (view-type fields)
    "Compiles declarative representation of view fields into CL code."
    (cons 'list
	  (mapcar (lambda (field)
		    (destructuring-bind (slot-name &rest args &key type
						   &allow-other-keys)
			(ensure-list field)
		      (let ((temp-field (gensym))
			    field-args-init-code)
			`(let ((,temp-field
				(funcall #'make-instance (view-field-class-name
							  (view-default-field-type ',view-type ',type))
					 :slot-name ',slot-name
					 ,@(quote-property-list-arguments
					    (apply #'remove-keyword-parameters
						   args :type :slot-name
						   (hash-keys *custom-view-field-argument-compilers*))))))
			   ,@(progn
			      (maphash (lambda (key value)
					 (when (getf args key)
					   (push-end (funcall value temp-field (getf args key))
						     field-args-init-code)))
				       *custom-view-field-argument-compilers*)
			      field-args-init-code)
			   ,temp-field))))
		  fields))))

;;; Special handling for INITFORM argument (to mixin-view-field)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod view-argument-quoting-strategy ((arg-name (eql :initform)))
    :quote)
    
  (setf (gethash :initform *custom-view-field-argument-compilers*)
	(lambda (slot-name initform)
	  `(setf (mixin-view-field-init-form ,slot-name)
		 (lambda ()
		   ,initform)))))

(defmacro defview-anon ((&rest args &key (type 'data) inherit-from satisfies &allow-other-keys) &rest fields)
  "A macro used to easily define anonymous user interface views in a
declarative manner."
  (let ((view (gensym)))
    `(let (,view)
       (setf ,view (funcall #'make-instance (view-class-name ',type)
			    :fields ,(compile-view-fields type fields)
			    ,@(when satisfies (list :satisfies `(ensure-list ,satisfies)))
			    ,@(quote-property-list-arguments
			       (remove-keyword-parameters args :type :inherit-from :satisfies))))
       ,(when inherit-from
	  (let ((inherit-from (second
			       (quote-property-list-arguments
				(list :inherit-from inherit-from))))
		(inherit-from-sym (gensym)))
	    `(setf (view-inherit-from ,view)
		   (let ((,inherit-from-sym ,inherit-from))
		     (if (listp ,inherit-from-sym)
			 (find-view (if (eq (car ,inherit-from-sym) :scaffold)
					(cons ',type (cdr ,inherit-from-sym))
					,inherit-from-sym))
			 ,inherit-from-sym)))))
       ,view)))

(defmacro defview (name (&rest args &key &allow-other-keys) &rest fields)
  "A macro used to easily define user interface views in a declarative
manner.

 (defview (NAME [:type TYPE] [:inherit-from INHERIT-FROM]
                [:satisfies SATISFIES] VIEW-KWARGS...)
   [FIELD-NAME
    | (FIELD-NAME [:type FIELD-TYPE] [:initform INITFORM]
		  [:present-as PRESENT-AS] [:parse-as PARSE-AS]
		  FIELD-KWARGS...)]...)

In the above form, these metasyntactic variables have the following values:

    NAME
	When non-nil, an unevaluated unique symbol identifying the
	view to other parts of the system that require a view,
	resolving it using `find-view'.
    TYPE
	An unevaluated designator for the class of the resulting
	view (determined by `view-class-name'), and half the
	designator for the class of this view's fields (determined by
	`view-field-class-name' applied to `view-default-field-type').
	Defaults to `data'.
    INHERIT-FROM
	A designator for `view' slot `inherit-from'.  Forms
	like (:scaffold DATA-CLASS) will replace :scaffold with TYPE.
	See `find-view' for further transformation.
    SATISFIES
	A designator for `form-view' slot `satisfies'.
    VIEW-KWARGS
	Other arguments passed directly to `make-instance' for the
	resulting view, quoting subject to
	`view-argument-quoting-strategy'.

INHERIT-FROM and SATISFIES are evaluated.

    FIELD-NAME
	An unevaluated designator for `view-field' slot `slot-name'.
    FIELD-TYPE
	A designator for the class of the field, in combination with
	TYPE, as explained above for TYPE.
    FIELD-KWARGS
	Other arguments passed directly to `make-instance' for the
	resulting view field, quoting subject to
	`view-argument-quoting-strategy'.

However, FIELD-KWARGS keywords present in
`*custom-view-field-argument-compilers*' are not included in the
`make-instance' call; see that variable for how those are transformed.
The built-in custom view fields are as follows:

    INITFORM
	A form for `mixin-view-field' slot `initform' (which is
	really an initfunction).
    PRESENT-AS
	A designator for `view-field' slot `presentation'.  The symbol
	or CAR is mapped to a class name through
	`presentation-class-name', and the CDR, a series of keyword
	arguments, is passed to `make-instance' on said class, subject
	to `view-argument-quoting-strategy'.
    PARSE-AS
	A designator for `form-view-field' slot `parser'.  Otherwise
	similar to PRESENT-AS, but mapping class names using
	`parser-class-name'."
  (if name
      `(setf (gethash ',name *views*)
	     (defview-anon ,args ,@fields))
      `(defview-anon ,args ,@fields)))

