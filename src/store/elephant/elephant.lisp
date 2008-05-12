(in-package :cl-user)

(defpackage #:weblocks-elephant
  (:use :cl :weblocks :weblocks-memory :elephant)
  (:shadowing-import-from :weblocks #:open-store #:close-store)
  (:export #:defpclass #:*current-ele-txn*)
  (:documentation
   "A driver for weblocks backend store API that connects to CL-Prevalence."))

(in-package :weblocks-elephant)

(defvar *current-ele-txn* nil)

(defclass elephant-store ()
  ((controller :accessor elephant-controller :initarg :controller)
   (lock :accessor elephant-lock :initarg :lock :initform (hunchentoot-mp:make-lock (gensym)))
   (transactions :accessor elephant-txns :initform nil)))

(defmethod class-id-slot-name ((class persistent-metaclass))
  'elephant::oid)

(defmethod object-id ((obj persistent))
  (handler-case 
      (elephant::oid obj)
    (error ()
      (error "Object oid unbound for persistent instance: ~A" obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization/finalization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod open-store ((store-type (eql :elephant)) &rest args &key spec &allow-other-keys)
  (setf *default-store*
	(make-instance 'elephant-store
		       :controller (setf *store-controller* (elephant:open-store spec :recover t)))))

(defmethod close-store ((store elephant-store))
  (when (eq *default-store* store)
    (setf *default-store* nil))
  (elephant:close-store (elephant-controller store))
  (when (eq (elephant-controller store) *store-controller*)
    (setf *store-controller* nil)))

(defmethod clean-store ((store elephant-store))
  ;; Drop everything from the root
  (elephant::map-btree (lambda (k v)
			 (declare (ignore k v))
			 (elephant:remove-current-kv))
		       (elephant:controller-root
			(elephant-controller store))))

(defmethod class-store ((class persistent-metaclass))
  "This works if you only have one elephant store open"
  (if (subtypep *default-store* 'elephant-store)
      *default-store*
      (progn
	(mapstores (lambda (store)
		     (when (subtypep store 'elephant-store)
		       (return-from class-store store))))
	(error "No valid elephant store available for instance of persistent class ~A" class))))
		 

;;;;;;;;;;;;;;;;;;;;;
;;; For scaffolds ;;;
;;;;;;;;;;;;;;;;;;;;;

(defmethod class-visible-slots-impl ((class (eql (find-class 'persistent))) &key readablep writablep)
  (declare (ignore readablep writablep))
  (remove-if (lambda (dsd)
	       (or (eq (weblocks::slot-definition-name dsd) 'elephant::oid)
		   (eq (weblocks::slot-definition-name dsd) 'elephant::spec)))
	     (call-next-method)))
	       

;;;;;;;;;;;;;;;;;;;;
;;; Transactions ;;;
;;;;;;;;;;;;;;;;;;;;
(defmethod begin-transaction ((store elephant-store))
  ;; NOTE: No support for nested transactions here or multiple BDB stores
  (let ((current-txn (get-txn store)))
    (if current-txn current-txn
	(set-txn store (controller-start-transaction (elephant-controller store))))))

(defmethod commit-transaction ((store elephant-store))
  (let ((current-txn (get-txn store)))
    (assert current-txn)
    (controller-commit-transaction (elephant-controller store) current-txn)
    (clear-txn store)))

(defmethod rollback-transaction ((store elephant-store))
  (let ((current-txn (get-txn store)))
    (assert current-txn)
    (controller-abort-transaction (elephant-controller store) current-txn)
    (clear-txn store)))

;; per-store / per-thread transactions

(defstruct txn-rec thread txn)

(defun get-txn (store)
  (hunchentoot-mp:with-lock ((elephant-lock store)) 
    (txn-rec-txn (get-txn-rec store))))

(defun get-txn-rec (store)
  (let* ((thread hunchentoot-mp:*current-process*)
	 (rec (find thread (elephant-txns store)
		    :key #'txn-rec-thread)))
    (if rec rec
	(let ((newrec (make-txn-rec :thread thread)))
	  (push newrec (elephant-txns store))
	  newrec))))

(defun set-txn (store txn)
  (hunchentoot-mp:with-lock ((elephant-lock store)) 
    (setf (txn-rec-txn (get-txn-rec store)) txn)))

(defun clear-txn (store)
  (hunchentoot-mp:with-lock ((elephant-lock store)) 
    (set-txn store nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating and deleting persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod persist-object ((store elephant-store) (object elephant:persistent))
  object)

(defmethod persist-object ((store elephant-store) object)
  (error "Cannot persist non-persistent objects to elephant stores: ~A" object))

(defmethod delete-persistent-object ((store elephant-store) object)
  (break)
  (when object
    (elephant:drop-pobject object)))

(defmethod delete-persistent-object-by-id ((store elephant-store) class-name object-id)
  (declare (ignore class-name))
  (when object-id
    (elephant:drop-pobject 
     (elephant::controller-recreate-instance (elephant-controller store)
					     object-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Querying persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod find-persistent-object-by-id ((store elephant-store) class-name object-id)
  (declare (ignore class-name))
  (elephant::controller-recreate-instance (elephant-controller store) object-id))

(defmethod find-persistent-objects ((store elephant-store) class-name
				    &key filter filter-view order-by range)
  ;; Use query system?
  ;; Short term: walk class or appropriate index; construct a filter function
  ;; - only construct objects when in-range, unless filter and not on index fn
  ;; - 
  (declare (ignore filter filter-view))
  (range-objects-in-memory
;;     (let ((*store-controller* (elephant-controller store)))
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

(defmethod supports-filter-p ((store elephant-store))
  nil)

