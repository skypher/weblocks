
(in-package :weblocks)

(export '(uri-parameters-mixin uri-parameters-slotmap))

(defclass uri-parameters-mixin ()
  ())

(defgeneric uri-parameters-slotmap (w)
  (:documentation "Returns an alist of (slotname . param-name)")
  (:method (w) (declare (ignore w)) nil))

(defgeneric uri-parameter-values (w)
  (:documentation "Returns an alist of (param-name . slot-value)")
  (:method (w) (declare (ignore w)) nil)
  (:method ((w uri-parameters-mixin))
    (loop for (sname . pname) in (uri-parameters-slotmap w) 
       when (slot-boundp w sname)
       collect (cons pname (slot-value w sname)))))

(defun maybe-generate-parameter-slot-map-fn (class slot-defs)
  "When a slot contains "
  (awhen (get-parameter-slot-map slot-defs)
    (with-gensyms (widget)
      `(defmethod uri-parameters-slotmap ((,widget ,class))
	 ',it))))

(defun uri-parameter-def-p (slot-defs)
  (member :uri-parameter (flatten slot-defs)))
    
(defun get-parameter-slot-map (slot-defs)
  (remove-if #'null
    (mapcar (lambda (slot-def)
	      (awhen (member :uri-parameter slot-def)
		(cons (first slot-def) 
		      (as-string (cadr it)))))
	    slot-defs)))

(defun as-string (obj)
  (etypecase obj
    (symbol (string-downcase (symbol-name obj)))
    (string obj)))
