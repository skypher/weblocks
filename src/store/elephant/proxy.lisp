(in-package :weblocks-elephant)

(defvar *proxies* (make-hash-table))
(defvar *view-proxies* (make-hash-table))

(defclass persistent-proxy ()
  ((base-class :accessor base-class :initarg :base :allocation :class)
   (proxy-oid :accessor proxy-oid :initarg :oid :initform nil)))

(defun return-proxy-classname (classname)
  (if (gethash classname *proxies*) 
      (gethash classname *proxies*)
    (let* ((persistent-class (find-class classname))
	   (new-name (intern (format nil "~A-~A" classname (gensym)) *package*))
	   (visible-slot-defs (class-visible-slots-impl persistent-class))
	   (class-def `(defclass ,new-name (persistent-proxy)
			 (,@(mapcar #'def-to-proxy-slot
				    visible-slot-defs))
			 (:default-initargs :base ',classname))))
      (eval class-def)
      (setf (gethash classname *proxies* new-name)
	    new-name))))

(defun def-to-proxy-slot (def)
  `(,(weblocks::slot-definition-name def)
     ,@(mapcan #'(lambda (arg)
		   `(:reader ,arg))
	       (weblocks::slot-definition-readers def))
     ,@(mapcan #'(lambda (arg)
		   `(:initarg ,arg))
	       (weblocks::slot-definition-initargs def))
     :initform ,(weblocks::slot-definition-initform def)
     :type ,(weblocks::slot-definition-type def)))
   

;;
;; Hook into elephant mechanism
;;

(defmethod shared-initialize :after ((class persistent-metaclass) slot-names &rest args)
  "Ensure that the proxy list is always clean when the class is redefined"
  (declare (ignore args slot-names))
  (remhash (class-name class) *proxies*))

;;
;; Hook into weblocks mechanisms
;;

(defmethod weblocks:dataseq-data-form-class :around ((seq dataseq))
  "Catch persistent classes and create/return proxy classes that can
   be used to instantiate views"
  (let* ((classname (call-next-method)))
    (if (subtypep classname 'persistent)
	(return-proxy-classname classname)
	classname)))

(defmethod weblocks::class-from-view :around (view &optional class-name)
  "Part of the quickview / dataform method for creating anonymous instances"
  (if (subtypep class-name 'persistent)
      (find-class (return-proxy-classname class-name))
      (call-next-method)))

(defmethod persist-object ((store elephant-store) (object persistent-proxy))
  "Catch when weblocks tries to persist a proxy object and create an instance
   of the persistent-object"
  (if (proxy-oid object)
      (elephant::controller-recreate-instance *store-controller* (proxy-oid object))
      (let ((instance (make-instance (base-class object))))
	(loop for slot in (weblocks::class-slots (class-of object)) do
	     (let ((slotname (weblocks::slot-definition-name slot)))
	       (unless (or (eq slotname 'base-class)
			   (eq slotname 'proxy-oid))
		 (setf (slot-value instance slotname)
		       (slot-value object slotname)))))
	(setf (proxy-oid object) (elephant::oid instance))
	instance)))

(defmethod object-class-name ((obj persistent-proxy))
  (base-class obj))