
(in-package :weblocks)

;;; CLOS and MOP utils.

(wexport '(slot-value-by-path 
           find-slot-dsd
           find-slot-esd
           object-class-name
           slot-equal)
         '(t util))


(defun slot-value-by-path (obj path)
  "Retrieves a value of a slot from a hierarchy of objects. A nil on
the path is ignored. 

ex:
\(slot-value-by-path employee '(address street)) => \"17 Sunvalley St.\"
\(slot-value-by-path employee '(address)) => #<ADDRESS {XXX}>
\(slot-value-by-path employee 'address) => #<ADDRESS {XXX}>
\(slot-value-by-path address '(street)) => \"17 Sunvalley St.\"
\(slot-value-by-path address '(nil street)) => \"17 Sunvalley St.\"

obj - a CLOS object
path - a list of slot names"
  (when (symbolp path)
    (return-from slot-value-by-path
      (slot-value-by-path obj (list path))))
  (let* ((clean-path (remove nil path))
	 (value (ignore-errors (slot-value obj (car clean-path))))
	 (path-rest (cdr clean-path)))
    (if path-rest
	(slot-value-by-path value path-rest)
	value)))

(defun find-slot-dsd (class slot-name)
  "Returns a direct-slot-definition object of a slot with 'slot-name'
in 'class'."
  (let ((class (if (symbolp class)
		   (find-class class)
		   class)))
    (or (loop
	   for dsd in (class-direct-slots class)
	   when (eq (slot-definition-name dsd) slot-name)
	   do (return dsd))
	(find-if (compose #'not #'null)
		 (mapcar (curry-after #'find-slot-dsd slot-name)
			 (class-direct-superclasses class))))))

(defun find-slot-esd (class slot-name)
  "Returns an effective-slot-definition object of a slot with
'slot-name' in 'class'."
  (let ((class (if (symbolp class)
		   (find-class class)
		   class)))
    (loop
       for esd in (class-slots class)
       when (eq (slot-definition-name esd) slot-name)
       do (return esd))))

(defgeneric object-class-name (obj)
  (:documentation
   "Returns an object's class name (i.e. \"Employee\"). This method is
be used to present the name of an entity to the user. Override this
method to change the name for particular objects.")
  (:method (obj)
    (class-name (class-of obj))))

(defun slot-equal (o1 o2 &key exclude (test #'equal))
  "Whether O1 and O2 have identical slot contents, excluding
  slot names in EXCLUDE.

  Two slots are considered equal if they are either both unbound
  or if they are both bound and pass TEST.

  Signals an error when the slot names of O1 and O2 don't match."
  (let* ((slotnames-o1 (mapcar #'slot-definition-name (class-slots (class-of o1))))
         (slotnames-o2 (mapcar #'slot-definition-name (class-slots (class-of o2))))
         (diff (set-difference slotnames-o1 slotnames-o2)))
    (when diff
      (error "Objects with differing slot names detected. Difference: ~S~%" diff))
    (dolist (slotname slotnames-o1 t)
      (unless (member slotname (ensure-list exclude) :test #'eq)
        (cond
          ((or (and (slot-boundp o1 slotname) (not (slot-boundp o2 slotname)))
               (and (slot-boundp o2 slotname) (not (slot-boundp o1 slotname))))
           (return-from slot-equal nil))
          ((and (not (slot-boundp o1 slotname)) (not (slot-boundp o2 slotname)))
           t)
          ((funcall test (slot-value o1 slotname) (slot-value o2 slotname))
           t)
          (t
           (return-from slot-equal nil)))))))
