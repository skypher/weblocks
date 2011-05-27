(in-package :cl-user)

(defpackage #:weblocks-elephant
  (:use :cl :weblocks :weblocks-memory :elephant :metatilities :metabang.moptilities)
  (:shadowing-import-from :weblocks #:open-store #:close-store)
  (:shadowing-import-from :elephant #:find-item #:insert-item #:add-index)
  (:export #:defpclass)
  (:documentation
   "A driver for weblocks backend store API that connects to Elephant."))

(in-package :weblocks-elephant)

(export '(elephant-store))

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
  (setf *default-store*
	(make-instance 'elephant-store
		       :controller (setf *store-controller* (elephant:open-store spec)))))

(defmethod close-store ((store elephant-store))
  (when (eq *default-store* store)
    (setf *default-store* nil))
  (elephant:close-store (elephant-controller store))
  (when (eq (elephant-controller store) *store-controller*)
    (setf *store-controller* nil)))

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

(defmethod class-visible-slots-impl ((class (eql (find-class 'ele::cacheable-persistent-object))) &key readablep writablep)
  (declare (ignore readablep writablep))
  (remove-if (lambda (dsd)
	       (or (eq (weblocks::slot-definition-name dsd) 'elephant::pchecked-out)
		   (eq (weblocks::slot-definition-name dsd) 'elephant::checked-out)))
	     (call-next-method)))
	       
;;;;;;;;;;;;;;;;;;;;
;;; Transactions ;;;
;;;;;;;;;;;;;;;;;;;;

(defmethod use-dynamic-transaction-p ((store elephant-store))
  t)

(defmethod dynamic-transaction ((store elephant-store) proc)
  "This dynamic hook wraps an elephant transaction macro around the body hooks.
   This allows us to gain the benefits of the stable transaction system in elephant"
  (ensure-transaction (:store-controller (elephant-controller store))
    (funcall proc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating and deleting persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod persist-object ((store elephant-store) (object elephant:persistent-object) &key)
  (ele::maybe-persistent-sync object)
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
				    &key order-by range filter-fn &allow-other-keys)
  "This implements a reasonably efficient form of the core weblocks query
   functionality for paged displays of objects.  More sophisticated stuff
   can be done via the on-query facility."
  (if (subtypep class-name 'persistent-object)
      (with-store-controller store
	(catch 'finish-map
	  (cond (filter-fn
		 (range-objects-in-memory
		  (advanced-order-objects-in-memory
		   (filter-objects-in-memory
		    (get-instances-by-class class-name)
		    filter-fn)
		   order-by)
		  range))
		((and (consp order-by) (has-index class-name (car order-by)))
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
		  (advanced-order-objects-in-memory
		   (get-instances-by-class class-name)
		   order-by)
		  range))
		(range
		 (with-ranged-collector (collector range)
		   (map-class collector class-name :oids t)))
		(t
		 (get-instances-by-class class-name)))))
      (find-persistent-standard-objects store class-name
                                        :order-by order-by
                                        :range range
                                        :filter-fn filter-fn)))

(defun filter-objects-in-memory (objects fn &aux results)
  (labels ((filter-if (object)
	     (unless (funcall fn object)
	       (push object results))))
    (mapc #'filter-if objects)
    (nreverse results)))

(defun advanced-order-objects-in-memory (seq order-by)
  "Orders objects in 'seq' according to 'order-by'."
  (cond ((not order-by)
	 seq)
	((not (consp (first order-by)))
	 (weblocks-memory::order-objects-in-memory seq order-by))
	(t 
	 (stable-sort seq (multi-value-sort-predicate-asc order-by)))))

(defun multi-value-sort-predicate-asc (order-by)
  (let ((query-records 
	 (mapcar #'(lambda (rec)
		     (destructuring-bind (slot-fn . dir) rec
		       (cons (curry-after #'slot-value-by-path slot-fn)
			     dir)))
		 order-by)))
    (lambda (a b)
      (loop for (accessor . dir) in query-records do
	   (let ((a-value (funcall accessor a))
		 (b-value (funcall accessor b)))
	     (if (eq dir :asc)
		 (weblocks-memory::strictly-less-p a-value b-value)
		 (and (not (weblocks-memory::strictly-less-p a-value b-value))
		      (not (weblocks-memory::equivalentp a-value b-value)))))))))

(defmethod count-persistent-objects ((store elephant-store) class-name
				     &key filter-fn &allow-other-keys)
  "A reasonably fast method for counting class instances"
  (if (subtypep class-name 'persistent-object)
      (with-store-controller store
	(let ((count 0))
	  (flet ((counter (x)
		   (unless (and filter-fn 
				(funcall filter-fn 
					 (elephant::controller-recreate-instance 
					  *store-controller* x)))
		     (incf count))))
	    (map-class #'counter class-name :oids t))
	  count))
      (count-persistent-standard-objects store class-name)))

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

(defmethod persist-object ((store elephant-store) object &key)
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


(defun find-persistent-standard-objects (store class-name &key order-by range filter-fn)
  "This implements a slow version of lookup"
  (range-objects-in-memory
   (order-objects-in-memory
    (with-store-controller store
      (let ((seq (map-index (lambda (k v pk) 
                              (declare (ignore k pk))
                              v)
                            (get-index (elephant-stdobj-index store) 'class)
                            :value class-name
                            :collect t)))
        (if (and seq
                 (functionp filter-fn))
            (remove-if-not filter-fn seq)
            seq)))
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
