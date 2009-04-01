;;; persist-children.lisp: Delayed computation and persistence of relations.

(in-package #:weblocks-s11)

(export '(persistent-children-mixin persistent-children add-child-to-persist
	  persist-children persist-child
	  default-persist-child-order define-default-persist-child-order))

(defgeneric persistent-children (self)
  (:documentation "Answer a list of objects to persist when persisting
SELF, via `persist-child'.  It will be cleared after such
persistence."))

(defclass persistent-children-mixin ()
  ((children :type hash-table :initform (make-hash-table :test 'eq)))
  (:documentation "Provide the `persistent-children' accessor and
backing."))

(defmethod persistent-children ((self persistent-children-mixin))
  (arnesi:hash-table-keys (slot-value self 'children)))

(defgeneric add-child-to-persist (child self &optional order)
  (:documentation "Arrange for CHILD to be sent `persist-child' when
persisting SELF.  ORDER may be of (member :before :after t nil),
determining when CHILD will be persisted relative to SELF.  NIL means
use `default-persist-child-order'.  T means do it both times, on the
off chance this means something."))

(defmethod add-child-to-persist
    (child (self persistent-children-mixin)
     &optional (order (default-persist-child-order child self)))
  (setf (gethash child (slot-value self 'children)) order))

(defgeneric persist-children (store self order)
  (:documentation "Access and persist `persistent-children' of SELF,
then remove them from the list.  This is always done by calling
`persist-child'.  ORDER indicates which should be taken."))

(defmethod persist-children (store (self persistent-children-mixin) order)
  "I provide the unneeded but nice additional feature that if any
persister exits non-locally, only it and the remaining children are
left in the queue for a re-call of `persist-children'."
  (let ((removable '()) (children (slot-value self 'children)))
    (unwind-protect (progn
		      (maphash (lambda (child child-order)
				 (when (member child-order `(,order t))
				   (persist-child store child self order)
				   (push child removable)))
			       children))
      (dolist (rm removable)
	(remhash rm children)))))

(defgeneric persist-child (store child parent order)
  (:documentation "Persist CHILD as a consequence of PARENT.  The
default is to call `persist-object'."))

(defmethod persist-child (store child (parent persistent-children-mixin) order)
  (declare (ignore parent order))
  (weblocks:persist-object store child))

(defgeneric default-persist-child-order (child parent)
  (:documentation "Answer the time to persist CHILD relative to PARENT
when no explicit order is given to `add-child-to-persist'.

I intentionally fail to provide a default, because there isn't a good
one.  Use `define-default-persist-child-order' to make one
yourself."))

(defmacro define-default-persist-child-order (child-specializer parent-specializer expr)
  (arnesi:with-unique-names (child parent)
    `(defmethod default-persist-child-order ((,child ,child-specializer)
					     (,parent ,parent-specializer))
       ,expr)))

(defmethod weblocks:persist-object :before
    (store (object persistent-children-mixin) &key &allow-other-keys)
  "Glue persistent children to the persistence API."
  (persist-children store object :before))

(defmethod weblocks:persist-object :after
    (store (object persistent-children-mixin) &key &allow-other-keys)
  "Glue persistent children to the persistence API."
  (persist-children store object :after))

;;; persist-children.lisp ends here
