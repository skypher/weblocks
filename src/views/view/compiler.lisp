
(in-package :weblocks)

(export '(*custom-view-field-argument-compilers*
	  view-argument-quoting-strategy defview))

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

(defmacro defview-anon ((&rest args &key (type 'data) inherit-from &allow-other-keys) &rest fields)
  "A macro used to easily define anonymous user interface views in a
declarative manner."
  (let ((view (gensym)))
    `(let (,view)
       (setf ,view (funcall #'make-instance (view-class-name ',type)
			    :fields ,(compile-view-fields type fields)
			    ,@(quote-property-list-arguments
			       (remove-keyword-parameters args :type :inherit-from))))
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
manner."
  (if name
      `(setf (gethash ',name *views*)
	     (defview-anon ,args ,@fields))
      `(defview-anon ,args ,@fields)))

