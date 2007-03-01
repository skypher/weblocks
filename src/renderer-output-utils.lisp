;;;; Utility functions for generic renderers
(in-package :weblocks)

;;; Convert "some-name" or 'SOME-NAME to "Some Name"
(defun humanize-name (name)
  "Takes a string or a symbol and converts it to a human readable format.
   Dashes are replaced by spaces and words are capitalized. If a string
   ends with '-ref', it is removed."
  (let* ((namestr (if (symbolp name)
		      (string-downcase (symbol-name name))
		      name))
	 (namestrpost (if (string-ends-with namestr "-ref")
			  (substring namestr 0 (- (length namestr) 4))
			  namestr)))
    (string-capitalize (substitute #\Space #\- namestrpost))))

;;; Convert 'SOME-NAME to  "some-name"
(defun attributize-name (name)
  "Takes a string or a symbol and converts it to html convential name"
  (let ((namestr (if (symbolp name)
		     (symbol-name name)
		     name)))
    (string-downcase namestr)))

;;; Returns a list of direct slot objects for a class and its subclasses
(defmethod object-visible-slots (obj &key slot-names hidep observe-order-p)
  "Returns a list of direct slot objects for an object and its parents
   iff they have reader accessors."
  (if hidep
      (let ((all-slots (class-visible-slots (class-of obj))))
	(list->assoc (remove-if (curry-after #'member slot-names :test #'string-equal)
				all-slots :key #'slot-definition-name)
		     :map #'slot-definition-name))
      (let* ((slot-assoc (list->assoc slot-names))
	     (all-slots (class-visible-slots (class-of obj) :visible-slots slot-assoc)))
	(if observe-order-p
	    (mapcar (lambda (i)
		      (let ((slot (car (member (car i) all-slots
					       :test #'string-equal
					       :key #'slot-definition-name))))
			(if (not (null slot))
			    (cons slot (cdr i)))))
		    slot-assoc)
	    (mapcar (lambda (i)
		      (cons i (let* ((slot-name (slot-definition-name i))
				     (alt-name (assoc slot-name slot-assoc)))
				(if (null alt-name)
				    slot-name
				    (cdr alt-name)))))
		    all-slots)))))

;;; '((a . b) c (d . e)) -> ((a . b) (c . c) (d . e))
(defun list->assoc (lst &key (map #'identity))
  (mapcar (lambda (i)
	    (if (consp i) i (cons i (funcall map i))))
	  lst))

;;; Returns a list of direct slot objects for a class and its subclasses
(defun class-visible-slots (cls &key visible-slots)
  "Returns a list of direct slot objects for a class and its subclasses
   if they have reader accessors or they're specified in visible-slots."
  (if (eql (class-name cls) 'standard-object)
      nil
      (apply #'append (append (mapcar (curry-after #'class-visible-slots :visible-slots visible-slots)
				      (class-direct-superclasses cls))
			      (list (remove-if (lambda (x)
						 (and (null (slot-definition-readers x))
						      (not (assoc (slot-definition-name x) visible-slots))))
					       (class-direct-slots cls)))))))

;;; Takes some-object and returns its class name
(defmethod object-class-name (obj)
  "Returns an object's class name."
  (class-name (class-of obj)))

;;; Takes some-object and tries to figure out its name
(defmethod object-name (obj)
  "Takes an object and tries to guess its name. Looks for reader class-name (project-name)
   and checks that the value is of type string."
  (let* ((cls-symbol (class-name (class-of obj)))
	 (cls-package (symbol-package cls-symbol))
	 (expected-name (concatenate 'string (symbol-name cls-symbol) "-NAME"))
	 (expected-symbol (find-symbol expected-name cls-package)))
    (if (and (not (null expected-symbol))
	     (fboundp expected-symbol))
	(let ((obj-name (funcall expected-symbol obj)))
	  (if (stringp obj-name)
	      (return-from object-name obj-name)))))
  nil)

;;; A basic implementation of a generic function to figure out if an object should be
;;; rendered inline. Currently an object is always rendered inline unless the slot name
;;; end with "-ref".
(defmethod render-slot-inline-p (obj slot-name)
  (let ((name (if (symbolp slot-name)
		  (symbol-name slot-name)
		  slot-name)))
    (not (string-ends-with name "-ref" :ignore-case-p t))))

;;; If a reader accessor for the slot exists, get its value via the accessor
;;; otherwise, use slot-value to access the slot
(defun get-slot-value (obj slot)
  (let ((slot-reader (car (slot-definition-readers slot))))
    (if (null slot-reader)
	(slot-value obj (slot-definition-name slot))
	(funcall slot-reader obj))))

