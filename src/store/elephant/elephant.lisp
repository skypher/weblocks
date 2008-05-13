(in-package :cl-user)

(defpackage #:weblocks-elephant
  (:use :cl :weblocks :weblocks-memory :elephant)
  (:shadowing-import-from :weblocks #:open-store #:close-store)
  (:export #:defpclass)
  (:documentation
   "A driver for weblocks backend store API that connects to CL-Prevalence."))

(in-package :weblocks-elephant)

(defclass elephant-store ()
  ((controller :accessor elephant-controller :initarg :controller)))

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
  (setup-elephant-transaction-hooks)
  (setf *default-store*
	(make-instance 'elephant-store
		       :controller (setf *store-controller* (elephant:open-store spec :recover t)))))

(defmethod close-store ((store elephant-store))
  (when (eq *default-store* store)
    (setf *default-store* nil))
  (elephant:close-store (elephant-controller store))
  (when (eq (elephant-controller store) *store-controller*)
    (setf *store-controller* nil))
  (remove-elephant-transaction-hooks))

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
  t)

(defmethod commit-transaction ((store elephant-store))
  t)

(defmethod rollback-transaction ((store elephant-store))
  t)

(defun elephant-transaction-hook (hooks)
  "This dynamic hook wraps an elephant transaction macro around the body hooks.
   This allows us to gain the benefits of the stable transaction system in elephant"
  (let ((valid-sc (get-valid-sc)))
    (if valid-sc
	(ensure-transaction (:store-controller valid-sc)
	  (eval-dynamic-hooks hooks))
	(eval-dynamic-hooks hooks))))

(defun get-valid-sc ()
  "This function provides some reasonable defaults for elephant.  Namely, that
   the default transaction is either the default store or the current store controller.
   Care must be taken when using multiple elephant stores with weblocks.  The 
   consequences are as yet undefined."
  (cond ((subtypep (type-of *default-store*) 'elephant-store)
	 (elephant-controller *default-store*))
	((not (null *store-controller*))
	 *store-controller*)))

(defun setup-elephant-transaction-hooks ()
  "Ensure that the elephant transaction hook is registered on action and rendering code"
  (pushnew 'elephant-transaction-hook (request-hook :application :dynamic-action))
  (pushnew 'elephant-transaction-hook (request-hook :application :dynamic-render)))

(defun remove-elephant-transaction-hooks ()
  "Remove the elephant-specific transaction hooks"
  (symbol-macrolet ((action-list (request-hook :application :dynamic-action))
		    (render-list (request-hook :application :dynamic-render)))
    (setf action-list (delete 'elephant-transaction-hook action-list))
    (setf render-list (delete 'elephant-transaction-hook render-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating and deleting persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod persist-object ((store elephant-store) (object elephant:persistent))
  object)

(defmethod persist-object ((store elephant-store) object)
  (error "Cannot persist non-persistent objects to elephant stores: ~A" object))

(defmethod delete-persistent-object ((store elephant-store) object)
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
;; Short term: walk class or appropriate index; construct a filter function
;; - optimize order by if using an index
;; - only construct objects when in-range, unless filter and not on index fn
;; - support filtering
;; Long term: use and support query system on objects?
;;     (let ((*store-controller* (elephant-controller store)))
;;       (declare (special *store-controller*))
;;       (if (and (consp order-by) (has-index class-name (car order-by)))
;;	   (elephant::map-inverted-index class-name (car order-by)
;;					 :from-end (when (eq (cdr order-by) :desc) t)
;;					 :collect t)
  (when (or filter filter-view)
    (warn "Elephant does not support filter functions at present"))
  (range-objects-in-memory
   (order-objects-in-memory
    (elephant::get-instances-by-class class-name)
    order-by)
   range))

(defun has-index (class-ref slot)
  "Test whether a given class has an index on its slot.  This helps us
   optimize find-persistent-objects"
  (subtypep (type-of (mopu:get-slot-definition class-ref slot)) 
	    'elephant::indexed-effective-slot-definition))

(defmethod count-persistent-objects ((store elephant-store) class-name
				     &key filter filter-view)
  (length (find-persistent-objects store class-name
				   :filter filter
				   :filter-view filter-view)))

(defmethod supports-filter-p ((store elephant-store))
  nil)
