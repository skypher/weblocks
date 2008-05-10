(in-package :cl-user)

(defpackage #:weblocks-elephant
  (:use :cl :metabang.utilities :weblocks :weblocks-memory)
  (:import-from :elephant #:store-controller #:controller-start-transaction
		#:controller-abort-transaction #:controller-commit-transaction
		#:*store-controller*)
  (:shadowing-import-from :metabang.utilities #:size)
  (:documentation
   "A driver for weblocks backend store API that connects to CL-Prevalence."))

(in-package :weblocks-elephant)

(defvar *current-ele-txn* nil)

(defclass elephant-store ()
  ((elephant-store :accessor elephant-store :initarg :store)))

(defmethod object-id ((obj elephant::persistent))
  (elephant::oid obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization/finalization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod open-store ((store-type (eql :elephant)) &rest args &key spec &allow-other-keys)
  (setf *default-store*
	(make-instance 'elephant-store 
		       :store (setf *store-controller* (elephant:open-store spec :recover t)))))

(defmethod close-store ((store elephant-store))
  (when (eq *default-store* store)
    (setf *default-store* nil))
  (elephant:close-store (elephant-store store)))

(defmethod clean-store ((store elephant-store))
  ;; Drop everything from the root
  (elephant::map-btree (lambda (k v)
			 (declare (ignore k v))
			 (elephant:remove-current-kv))
		       (elephant:controller-root
			(elephant-store store))))


;;;;;;;;;;;;;;;;;;;
;;; Information ;;;
;;;;;;;;;;;;;;;;;;;
(defmethod supports-filter-p ((store elephant-store))
  nil)

;;;;;;;;;;;;;;;;;;;;
;;; Transactions ;;;
;;;;;;;;;;;;;;;;;;;;
(defmethod begin-transaction ((store elephant-store))
  ;; NOTE: No support for composable transactions here or multiple BDB stores
  (if *current-ele-txn* *current-ele-txn*
      (setf *current-ele-txn* 
	    (elephant::controller-start-transaction 
	     (elephant-store store)))))

(defmethod commit-transaction ((store elephant-store))
  (assert *current-ele-txn*)
  (controller-commit-transaction (elephant-store store) *current-ele-txn*)
  (setf *current-ele-txn* nil))

(defmethod rollback-transaction ((store elephant-store))
  (assert *current-ele-txn*)
  (controller-abort-transaction (elephant-store store) *current-ele-txn*)
  (setf *current-ele-txn* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating and deleting persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod persist-object ((store elephant-store) object)
  (declare (ignore object))
  nil)
;  (elephant:add-to-root (elephant::oid object) object
;;			(elephant-store store)))

(defmethod delete-persistent-object ((store elephant-store) object)
  (elephant:drop-pobject object)
  (elephant:remove-from-root (elephant::oid object) 
			     (elephant-store store)))

(defmethod delete-persistent-object-by-id ((store elephant-store) class-name object-id)
  (declare (ignore class-name))
  (elephant:drop-pobject 
   (elephant::controller-recreate-instance (elephant-store store)
					   object-id))
  (elephant:remove-from-root object-id
			     (elephant-store store)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Querying persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod find-persistent-object-by-id ((store elephant-store) class-name object-id)
  (declare (ignore class-name))
  (elephant:get-from-root object-id :sc (elephant-store store)))

(defmethod find-persistent-objects ((store elephant-store) class-name
				    &key filter filter-view order-by range)
  ;; Use query system?
  ;; Short term: walk class or appropriate index; construct a filter function
  ;; - only construct objects when in-range, unless filter and not on index fn
  ;; - 
  (declare (ignore filter filter-view))
  (range-objects-in-memory
;;     (let ((*store-controller* (elephant-store store)))
;;       (declare (special *store-controller*))
;;       (if (and (consp order-by) (has-index class-name (car order-by)))
;;	   (elephant::map-inverted-index class-name (car order-by)
;;					 :from-end (when (eq (cdr order-by) :desc) t)
;;					 :collect t)
	   (order-objects-in-memory
	    (elephant::get-instances-by-class class-name)
	    order-by)
     range))

(defun has-index (class-ref slot)
  (subtypep (type-of (mopu:get-slot-definition class-ref slot)) 
	    'elephant::indexed-effective-slot-definition))

(defmethod count-persistent-objects ((store elephant-store) class-name
				     &key filter filter-view)
  (length (find-persistent-objects store class-name
				   :filter filter
				   :filter-view filter-view)))

