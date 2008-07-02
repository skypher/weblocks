(in-package :cl-user)

(defpackage #:weblocks-elephant
  (:use :cl :weblocks :weblocks-memory :elephant :metatilities :metabang.moptilities)
  (:shadowing-import-from :weblocks #:open-store #:close-store)
  (:shadowing-import-from :elephant #:find-item #:insert-item #:add-index)
  (:export #:defpclass)
  (:documentation
   "A driver for weblocks backend store API that connects to CL-Prevalence."))

(in-package :weblocks-elephant)

(defclass elephant-store ()
  ((controller :accessor elephant-controller :initarg :controller)
   (stdidx :accessor elephant-stdobj-index)))

(defmethod initialize-instance :after ((store elephant-store) &rest args)
  (declare (ignore args))
  (ensure-standard-object-index store))

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
  (declare (ignore args))
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
  (map-btree (lambda (k v)
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
  (pushnew 'elephant-transaction-hook (request-hook :application :dynamic-action)))
;;  (pushnew 'elephant-transaction-hook (request-hook :application :dynamic-render)))

(defun remove-elephant-transaction-hooks ()
  "Remove the elephant-specific transaction hooks"
  (symbol-macrolet ((action-list (request-hook :application :dynamic-action))
		    (render-list (request-hook :application :dynamic-render)))
    (setf action-list (delete 'elephant-transaction-hook action-list))))
;;    (setf render-list (delete 'elephant-transaction-hook render-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating and deleting persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod persist-object ((store elephant-store) (object elephant:persistent-object))
  object)

(defmethod delete-persistent-object ((store elephant-store) (object elephant:persistent-object))
  (when object
    (elephant:drop-instance object)))

(defmethod delete-persistent-object-by-id ((store elephant-store) class-name object-id)
   (if (subtypep class-name 'persistent-object)
      (when (and object-id (subtypep class-name 'persistent-object))
	(elephant:drop-instance
	 (elephant::controller-recreate-instance (elephant-controller store)
						 object-id)))
      (delete-persistent-standard-object-by-id store class-name object-id)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Querying persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-store-controller (store &body body)
  `(let ((*store-controller* (elephant-controller ,store)))
     (declare (special *store-controller*))
     ,@body))

(defmethod find-persistent-object-by-id ((store elephant-store) class-name object-id)
  (declare (ignore class-name))
  (with-store-controller store
    (elephant::controller-recreate-instance (elephant-controller store) object-id)))

(defmacro with-ranged-collector ((var range) &body body)
  (with-gensyms (results accumulator value)
    `(let ((,results nil))
       (labels ((,accumulator (,value)
		  (push (elephant::controller-recreate-instance 
			 *store-controller* ,value)
			,results)))
	 (let ((,var (make-ranged-collector #',accumulator ,range)))
	   (catch 'finish-map
	     ,@body)))
       ,results)))

(defmethod find-persistent-objects ((store elephant-store) class-name
				    &key order-by range &allow-other-keys)
  "This implements a reasonably efficient form of the core weblocks query
   functionality for paged displays of objects.  More sophisticated stuff
   can be done via the on-query facility."
  (if (subtypep class-name 'persistent-object)
      (with-store-controller store
	(catch 'finish-map
	  (cond ((and (consp order-by) (has-index class-name (car order-by)))
		 (if range
		     (with-ranged-collector (rcollector range)
		       (map-inverted-index 
			(lambda (key oid)
			  (declare (ignore key))
			  (funcall rcollector oid))
			class-name (car order-by)
			:from-end (when (eq (cdr order-by) :desc) t)
			:oids t))
		     (map-inverted-index 
		      #'elephant::identity2
		      class-name (car order-by)
		      :from-end (when (eq (cdr order-by) :desc) t)
		      :collect t)))
		((consp order-by)
		 (range-objects-in-memory
		  (order-objects-in-memory
		   (get-instances-by-class class-name)
		   order-by)
		  range))
		(range
		 (with-ranged-collector (collector range)
		   (map-class collector class-name :oids t)))
		(t
		 (get-instances-by-class class-name)))))
      (find-persistent-standard-objects store class-name :order-by order-by :range range)))

(defmethod count-persistent-objects ((store elephant-store) class-name
				     &key &allow-other-keys)
  "A reasonably fast method for counting class instances"
  (if (subtypep class-name 'persistent-object)
      (with-store-controller store
	(let ((count 0))
	  (flet ((counter (x)
		   (declare (ignore x))
		   (incf count)))
	    (map-class #'counter class-name :oids t))
	  count))
      (count-persistent-objects store class-name)))

(defun make-ranged-collector (collector range)
  (let ((offset 0)
	(start (car range))
	(end (cdr range)))
    (labels ((range-mapper (oid)
	       (cond ((>= offset end)
		      (throw 'finish-map nil))
		     ((>= offset start)
		      (funcall collector oid)
		      (incf offset))
		     (t (incf offset)))))
      #'range-mapper)))

(defun has-index (class-ref slot)
  "Test whether a given class has an index on its slot.  This helps us
   optimize find-persistent-objects"
  (elephant::indexed-p (get-slot-definition class-ref slot)))


;; ========================================================================
;;  Support for persisting standard classes
;; ========================================================================

(defmacro aif (pred con alt)
  `(let ((it ,pred))
     (if it
	 ,con
	 ,alt)))

(defun ensure-standard-object-index (store)
  (setf (elephant-stdobj-index store)
	(aif (get-from-root 'stdobj-index :sc (elephant-controller store))
		       it
		       (make-standard-object-index store))))

(defun make-standard-object-index (store)
  (let ((bt (make-indexed-btree (elephant-controller store))))
    (add-index bt
	       :index-name 'class 
	       :key-form '(lambda (idx pk v)
			   (declare (ignore idx pk))
			   (values t (class-of v))))
    bt))

(defmethod persist-object ((store elephant-store) object)
  (aif (object-id object)
       (setf (get-value it (elephant-stdobj-index store))
	     object)
       (error "No object ID with which to persist ~A" object)))
	  
(defmethod delete-persistent-object ((store elephant-store) object)
  (if (and object (object-id object))
      (remove-kv (object-id object) (elephant-stdobj-index store))
      (error "Object ~A is null or has no object-id" object)))

(defun delete-persistent-standard-object-by-id (store class-name object-id)
  (declare (ignore class-name))
  (aif (and object-id (get-value object-id (elephant-stdobj-index store)))
       (delete-persistent-object store it)
       (error "No standard object with ID ~A found in store ~A" object-id store)))

;; =================================================================
;;  Searching for standard classes (not very efficient)
;; =================================================================

(defun find-persistent-standard-object-by-id (store class-name object-id)
  (declare (ignore class-name))
  (with-store-controller store
    (get-value object-id (elephant-stdobj-index store))))


(defun find-persistent-standard-objects (store class-name &key order-by range)
  "This implements a slow version of lookup"
  (range-objects-in-memory
   (order-objects-in-memory
    (with-store-controller store
      (map-index (lambda (k v pk) 
		   (declare (ignore k pk))
		   v)
		 (get-index (elephant-stdobj-index store) 'class)
		 :value class-name
		 :collect t))
    order-by)
   range))
	

(defun count-persistent-standard-objects (store class-name)
  "Count class instances"
  (let ((count 0))
    (map-index (lambda (k v pk) 
		 (declare (ignore k v pk))
		 (incf count))
	       (get-index (elephant-stdobj-index store) 'class)
	       :value class-name)))
