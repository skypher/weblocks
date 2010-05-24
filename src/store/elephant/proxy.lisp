(in-package :weblocks-elephant)

(export '(make-proxy-instance))

(defvar *proxies* (make-hash-table))
(defvar *view-proxies* (make-hash-table))

(defclass persistent-proxy ()
  ((proxy-oid :accessor proxy-oid :initarg :oid :initform nil)))

(defmethod object-id ((obj persistent-proxy))
  (proxy-oid obj))

(defun make-proxy-instance (class-name &rest args)
  "Creates an instance of the proxy."
  (apply #'make-instance (return-proxy-classname class-name) args))

(defun return-proxy-classname (classname)
  (or (gethash classname *proxies*)
      (let* ((persistent-class (find-class classname))
             (new-name (intern (format nil "~A-~A" classname (gensym)) *package*))
             (visible-slot-defs (class-visible-slots-impl persistent-class))
             (class-def `(defclass ,new-name (persistent-proxy)
                           ((base-class :accessor base-class :allocation :class
                                        :initform ',classname)
                            ,@(mapcar #'def-to-proxy-slot
                                      visible-slot-defs)))))
        (eval class-def)
        (setf (gethash classname *proxies* new-name)
              new-name))))

(defun def-to-proxy-slot (def)
  `(,(weblocks::slot-definition-name def)
     ,@(mapcan #'(lambda (arg)
		   `(:reader ,arg))
	       (weblocks::slot-definition-readers def))
     ,@(mapcan #'(lambda (arg)
		   `(:writer ,arg))
	       (weblocks::slot-definition-writers def))
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
  (if (and class-name (subtypep class-name 'persistent))
      (find-class (return-proxy-classname class-name))
      (call-next-method)))

(defmethod persist-object ((store elephant-store) (object persistent-proxy) &key)
  "Catch when weblocks tries to persist a proxy object and create an instance
   of the persistent-object"
  (with-store-controller store
    (if (proxy-oid object)
        (let ((instance (elephant::controller-recreate-instance *store-controller* (proxy-oid object))))
	  (ele::maybe-persistent-sync instance)
	  instance)
        (let ((instance (make-instance (base-class object))))
          (loop for slot in (weblocks::class-slots (class-of object)) do
               (let ((slotname (weblocks::slot-definition-name slot)))
                 (unless (or (eq slotname 'base-class)
                             (eq slotname 'proxy-oid))
                   (setf (slot-value instance slotname)
                         (slot-value object slotname)))))
          (setf (proxy-oid object) (elephant::oid instance))
          instance))))

(defmethod object-class-name ((obj persistent-proxy))
  (base-class obj))
