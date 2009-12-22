
(defpackage #:weblocks-prevalence
  (:use :cl :metabang.utilities :cl-prevalence :weblocks :weblocks-memory)
  (:shadowing-import-from :metabang.utilities #:size)
  (:shadowing-import-from :cl-prevalence #:id)
  (:export make-persistent-instance)
  (:documentation
   "A driver for weblocks backend store API that connects to CL-Prevalence."))

(in-package :weblocks-prevalence)

(defvar *locks* (make-hash-table :test #'eq)
  "Locks for Prevalence operation guards; one lock per store.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization/finalization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod open-store ((store-type (eql :prevalence)) &rest args)
  (let* ((store (apply #'make-instance 'guarded-prevalence-system :directory (car args) (cdr args)))
         (lock-name (format nil "Prevalence lock for store ~S" store))
         (lock (bordeaux-threads:make-lock lock-name)))
    (setf (gethash store *locks*) lock)
    (setf (get-guard store) (lambda (thunk)
                              (bordeaux-threads:with-lock-held (lock)
                                (funcall thunk))))
    (setf *default-store* store)))

(defmethod close-store ((store prevalence-system))
  (when (eq *default-store* store)
    (setf *default-store* nil))
  (snapshot store)
  (cl-prevalence::close-open-streams store)
  (setf (gethash store *locks*) nil))

(defmethod clean-store ((store prevalence-system))
  (totally-destroy store))

;;;;;;;;;;;;;;;;;;;;
;;; Transactions ;;;
;;;;;;;;;;;;;;;;;;;;
(defmethod begin-transaction ((store prevalence-system))
  ; No support for composable transactions in cl-prevalence
  nil)

(defmethod commit-transaction ((store prevalence-system))
  ; No support for composable transactions in cl-prevalence
  nil)

(defmethod rollback-transaction ((store prevalence-system))
  ; No support for composable transactions in cl-prevalence
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating and deleting persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass persistent-objects-of-class ()
  ((objects-by-id :initform (make-hash-table)
		  :accessor persistent-objects-of-class-by-id
		  :documentation "A hashmap with keys being object
		  IDs, and values being object instances.")
   (next-id :initform -1
	    :accessor persistent-objects-of-class-next-id
	    :documentation "The ID of the last created object. When
	    objects are created, this slot is incremented and its
	    value is used to automatically generate object IDs."))
  (:documentation "This class represents an alternative to RDBMS
  table, holding object instances of a given class."))

(defmethod persist-object ((store prevalence-system) object &key (txn-p t))
  (if txn-p
    (execute store (make-transaction 'tx-persist-object-prevalence
                                     object))
    (funcall 'tx-persist-object-prevalence store object)))

(defun tx-persist-object-prevalence (store object)
  "Persists the object in the store."
  (let* ((class-name (class-name (class-of object)))
	 (objects (or (get-root-object store class-name)
		      (setf (get-root-object store class-name)
			    (make-instance 'persistent-objects-of-class))))
	 (object-id (object-id object)))
    ; assign object id
    (if object-id
	(when (< (persistent-objects-of-class-next-id objects) object-id)
	  (setf (persistent-objects-of-class-next-id objects) (1+ object-id)))
	(setf (object-id object)
	      (incf (persistent-objects-of-class-next-id objects))))
    ; store the object
    (setf (gethash (object-id object) (persistent-objects-of-class-by-id objects))
	  object)))

(defmethod delete-persistent-object ((store prevalence-system) object)
  (execute store (make-transaction 'tx-delete-object-by-id-prevalence
				   (class-name (class-of object))
				   (object-id object))))

(defmethod delete-persistent-object-by-id ((store prevalence-system) class-name object-id)
  (execute store (make-transaction 'tx-delete-object-by-id-prevalence
				   class-name object-id)))

(defun tx-delete-object-by-id-prevalence (store class-name object-id)
  "Delets a persistent object from the store."
  (let ((objects (get-root-object store class-name)))
    ; delete the object
    (remhash object-id (persistent-objects-of-class-by-id objects))
    ; delete table if necessary
    (when (eq (hash-table-count (persistent-objects-of-class-by-id objects)) 0)
      (remove-root-object store class-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Querying persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod find-persistent-object-by-id ((store prevalence-system) class-name object-id)
  (let ((objects (get-root-object store class-name)))
    ; find the object
    (when objects
      (gethash object-id (persistent-objects-of-class-by-id objects)))))

(defmethod find-persistent-objects ((store prevalence-system) class-name 
				    &key (filter nil) order-by range slot
                                         (value nil value-given)
                                         (test #'equal))
  "The slot and value keys must appear together.  If they appear, a
filter will be applied (before the filter passed, if any) that
requires all objects to have the given value in the given slot."
  (range-objects-in-memory
   (order-objects-in-memory
    (let* ((seq (query store 'tx-find-persistent-objects-prevalence class-name))
	   (seq2 (cond
		   ((and seq slot value-given)
		    (remove-if-not
		      (lambda (obj)
                        (cond
                          ((not (slot-exists-p obj slot))
                           (warn "FIND-PERSISTENT-OBJECTS: object ~A doesn't have the slot ~A" obj slot))
                          ((not (slot-boundp obj slot))
                           (warn "FIND-PERSISTENT-OBJECTS: slot ~A of object ~A is not bound" obj slot))
                          (t (funcall test (slot-value obj slot) value))))
		      seq))
		   (t seq)))
	   (seq3 (cond
		   ((and seq2
			 (functionp filter))
		    (remove-if-not filter seq2))
		   (t seq2))))
      seq3)
    order-by)
   range))

(defun tx-find-persistent-objects-prevalence (store class-name)
  "Finds persistent objects of a given class."
  (let ((objects (get-root-object store class-name)))
    (when objects
      (loop for i being the hash-values in (persistent-objects-of-class-by-id objects)
	 collect i))))

(defmethod count-persistent-objects ((store prevalence-system) class-name &rest args)
  (length (apply #'find-persistent-objects store class-name args)))

(defun make-persistent-instance (store class &rest initargs)
  (persist-object store (apply #'make-instance class initargs)))

