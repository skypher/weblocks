;;; persist-children.lisp: Delayed computation and persistence of relations.

(in-package #:weblocks-s11)

(export '(persistent-children-mixin persistent-children add-child-to-persist
	  persist-children persist-child))

(defgeneric persistent-children (self)
  (:documentation "Answer a list of objects to persist when persisting
SELF, via `persist-child'.  It will be cleared after such persistence."))
(defgeneric (setf persistent-children) (list self)
  (:documentation "Alter the value of `persistent-children'.  You
oughtn't use this.  Use `add-child-to-persist' instead."))

(defclass persistent-children-mixin ()
  ((children :type list :initform '() :accessor persistent-children)
   (children-tail :type list :initform nil))
  (:documentation "Provide the `persistent-children' accessor and
backing."))

(defmethod (setf persistent-children) (list (self persistent-children-mixin))
  "Obviously not too efficient, which is why you ought to take heed of
the GF docstring."
  (with-slots (children children-tail) self
    (setf children list
	  children-tail (last list))))

(defgeneric add-child-to-persist (child self &optional position)
  (:documentation "Arrange for CHILD to be sent `persist-child' when
persisting SELF.  POSITION may be of (member :before :after nil),
determining where CHILD ought to be added in the list.  NIL means you
don't care."))

(defmethod add-child-to-persist (child (self persistent-children-mixin)
				 &optional position)
  "Yes, we alter structure."
  (with-slots (children children-tail) self
    (ecase position
      ((:before nil)
	 (push child children)
	 (unless children-tail
	   (setf children-tail children)))
      ((:after)
	 (if children-tail
	     (setf children-tail
		   (setf (cdr children-tail) (list child)))
	     (setf children (setf children-tail (list child))))))))

(defgeneric persist-children (store self)
  (:documentation "Access and persist `persistent-children' of SELF,
then remove them from the list.  This is always done by calling
`persist-child'."))

(defmethod persist-children (store (self persistent-children-mixin))
  "I provide the unneeded but nice additional feature that if any
persister exits non-locally, it and the remaining children are left in
the queue for a re-call of `persist-children'."
  (with-slots (children children-tail) self
    (let ((remaining children))
    (unwind-protect (loop :while remaining
			  :do (persist-child store (car remaining) self)
			  :do (setf remaining (cdr remaining)))
      (setf children remaining))
    (setf children-tail '()))))

(defgeneric persist-child (store child parent)
  (:documentation "Persist CHILD as a consequence of PARENT.  The
default is to call `persist-object'."))

(defmethod persist-child (store child (parent persistent-children-mixin))
  (weblocks:persist-object store child))

(defmethod weblocks:persist-object :after (store (object persistent-children-mixin))
  "Glue persistent children to the persistence API."
  (persist-children store object))

;;; persist-children.lisp ends here
