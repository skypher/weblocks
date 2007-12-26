
(defpackage #:weblocks-prevalence
  (:use :cl :metabang.utilities :cl-prevalence :weblocks :weblocks-memory)
  (:shadowing-import-from :metabang.utilities #:size)
  (:shadowing-import-from :cl-prevalence #:id)
  (:documentation
   "A driver for weblocks backend store API that connects to CL-Prevalence."))

(in-package :weblocks-prevalence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization/finalization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod open-store ((store-type (eql :prevalence)) &rest args)
  (setf *default-store* (apply #'make-prevalence-system args)))

(defmethod close-store ((store prevalence-system))
  (when (eq *default-store* store)
    (setf *default-store* nil))
  (snapshot store)
  (cl-prevalence::close-open-streams store))

(defmethod clean-store ((store prevalence-system))
  (totally-destroy store))

;;;;;;;;;;;;;;;;;;;
;;; Information ;;;
;;;;;;;;;;;;;;;;;;;
(defmethod supports-filter-p ((store prevalence-system))
  t)

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

(defmethod persist-object ((store prevalence-system) object)
  (execute store (make-transaction 'tx-persist-object-prevalence
				   object)))

(defun tx-persist-object-prevalence (store object)
  "Persists the object in the store."
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

(defmethod delete-persistent-object ((store prevalence-system) object)
  (execute store (make-transaction 'tx-delete-object-by-id-prevalence
				   (class-name (class-of object))
				   (object-id object))))

(defmethod delete-persistent-object-by-id ((store prevalence-system) class-name object-id)
  (execute store (make-transaction 'tx-delete-object-by-id-prevalence
				   class-name object-id)))

(defun tx-delete-object-by-id-prevalence (store class-name object-id)
  "Delets a persistant object from the store."
  (let ((objects (get-root-object store class-name)))
    ; delete the object
    (remhash object-id (persistent-objects-of-class-by-id objects))
    ; delete table if necessary
    (when (eq (hash-table-count (persistent-objects-of-class-by-id objects)) 0)
      (remove-root-object store class-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Querying persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod find-persistent-objects ((store prevalence-system) class-name
				    &key filter filter-args order-by range)
  (range-objects-in-memory
   (order-objects-in-memory
    (filter-objects-in-memory
     (query store 'tx-find-persistent-objects-prevalence
			  class-name)
     filter filter-args)
    order-by)
   range))

(defun tx-find-persistent-objects-prevalence (store class-name)
  "Finds persistent objects of a given class."
  (let ((objects (get-root-object store class-name)))
    (when objects
      (loop for i being the hash-values in (persistent-objects-of-class-by-id objects)
	 collect i))))

(defmethod count-persistent-objects ((store prevalence-system) class-name
				     &key filter filter-args order-by range)
  (length (find-persistent-objects store class-name
				   :filter filter
				   :order-by order-by
				   :range range
				   :filter-args filter-args)))

