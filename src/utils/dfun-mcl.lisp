
(in-package :weblocks)

;;; This file is only loaded on MCL and OpenMCL. These platforms don't
;;; provide support for compute-discriminating-function, so we have to
;;; fake it.

(defmethod compute-discriminating-function ((gf standard-generic-function))
  (let ((std-dfun (ccl::%gf-dcode gf))
	(dt (ccl::%gf-dispatch-table gf)))
    (lambda (&rest args)
      (case (ccl::function-name std-dfun)
	(ccl::%%one-arg-dcode (apply std-dfun dt args))
	(ccl::%%1st-two-arg-dcode (apply std-dfun dt args))
	(t (funcall std-dfun dt args))))))

(defmethod add-method :after ((gf slot-management-generic-function)
			      (method slot-management-method))
  (set-funcallable-instance-function gf (compute-discriminating-function gf)))

(defmethod remove-method :after ((gf slot-management-generic-function)
				 (method slot-management-method))
  (set-funcallable-instance-function gf (compute-discriminating-function gf)))

(defmethod initialize-instance :after ((gf slot-management-generic-function) &rest args)
  (set-funcallable-instance-function gf (compute-discriminating-function gf)))

(defmethod reinitialize-instance :after ((gf slot-management-generic-function) &rest args)
  (set-funcallable-instance-function gf (compute-discriminating-function gf)))

(defmethod initialize-instance :after ((method slot-management-method) &rest args)
  (let ((gf (method-generic-function method)))
    (when gf
      (set-funcallable-instance-function gf (compute-discriminating-function gf)))))

(defmethod reinitialize-instance :after ((method slot-management-method) &rest args)
  (let ((gf (method-generic-function method)))
    (when gf
      (set-funcallable-instance-function gf (compute-discriminating-function gf)))))
