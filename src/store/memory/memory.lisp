
(in-package :weblocks-memory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization/finalization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass memory-store ()
  ((root-objects :initform (make-hash-table)
		 :initarg :root-objects
		 :accessor memory-store-root-objects
		 :documentation "Stores objects that represent an
		 alternative to RDBMS tables, that further store
		 object instances."))
  (:documentation "A weblocks backend that stores all data in memory
  without disk backing."))

(defun get-root-object (store object-name)
  "Returns a root object from the store."
  (gethash object-name (memory-store-root-objects store)))

(defun (setf get-root-object) (value store object-name)
  "Sets a root object in the store."
  (setf (gethash object-name (memory-store-root-objects store))
	value))

(defun remove-root-object (store object-name)
  "Deletes a root object from the store."
  (remhash object-name (memory-store-root-objects store)))

(defmethod open-store ((store-type (eql :memory)) &rest args)
  (setf *default-store* (make-instance 'memory-store)))

(defmethod close-store ((store memory-store))
  (when (eq *default-store* store)
    (setf *default-store* nil)))

(defmethod clean-store ((store memory-store))
  nil)

;;;;;;;;;;;;;;;;;;;
;;; Information ;;;
;;;;;;;;;;;;;;;;;;;
(defmethod supports-filter-p ((store memory-store))
  t)

;;;;;;;;;;;;;;;;;;;;
;;; Transactions ;;;
;;;;;;;;;;;;;;;;;;;;
(defmethod begin-transaction ((store memory-store))
  ; No support for composable transactions in memory
  nil)

(defmethod commit-transaction ((store memory-store))
  ; No support for composable transactions in memory
  nil)

(defmethod rollback-transaction ((store memory-store))
  ; No support for composable transactions in memory
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating and deleting persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass persistant-objects-of-class ()
  ((objects-by-id :initform (make-hash-table)
		  :accessor persistent-objects-of-class-by-id
		  :documentation "A hashmap with keys being object
		  IDs, and values being object instances.")
   (next-id :initform -1
	    :accessor persistant-objects-of-class-next-id
	    :documentation "The ID of the last created object. When
	    objects are created, this slot is incremented and its
	    value is used to automatically generate object IDs."))
  (:documentation "This class represents an alternative to RDBMS
  table, holding object instances of a given class."))

(defmethod persist-object ((store memory-store) object)
  (let* ((class-name (class-name (class-of object)))
	 (objects (or (get-root-object store class-name)
		      (setf (get-root-object store class-name)
			    (make-instance 'persistant-objects-of-class))))
	 (object-id (or (object-id object)
			(setf (object-id object)
			      (incf (persistant-objects-of-class-next-id objects))))))
    ; store the object
    (setf (gethash object-id (persistent-objects-of-class-by-id objects))
	  object)))

(defmethod delete-persistent-object ((store memory-store) object)
  (delete-persistent-object-by-id store 
				  (class-name (class-of object))
				  (object-id object)))

(defmethod delete-persistent-object-by-id ((store memory-store) class-name object-id)
  (let ((objects (get-root-object store class-name)))
    ; delete the object
    (remhash object-id (persistent-objects-of-class-by-id objects))
    ; delete table if necessary
    (when (eq (hash-table-count (persistent-objects-of-class-by-id objects)) 0)
      (remove-root-object store class-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Querying persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod find-persistent-objects ((store memory-store) object-class
				    &key filter filter-args order-by range)
  (range-objects-in-memory
   (order-objects-in-memory
    (filter-objects-in-memory
     (find-persistent-objects-aux store object-class)
     filter filter-args)
    order-by)
   range))

(defun find-persistent-objects-aux (store class-name)
  "Finds persistent objects of a given class."
  (let ((objects (get-root-object store class-name)))
    (when objects
      (loop for i being the hash-values in (persistent-objects-of-class-by-id objects)
	 collect i))))

(defmethod count-persistent-objects ((store memory-store) class-name
				     &key filter filter-args order-by range)
  (length (find-persistent-objects store class-name
				   :filter filter
				   :order-by order-by
				   :range range
				   :filter-args filter-args)))

