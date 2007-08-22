
(in-package :weblocks)

(defmethod data-print-object (obj slot-name (slot-type (eql 'boolean)) slot-value &rest args)
  (if slot-value "Yes" "No"))

(defmethod render-data-aux (obj slot-name (slot-type (eql 'boolean)) (slot-value (eql nil))
			    &rest keys &key highlight &allow-other-keys)
  (apply #'call-next-method obj slot-name slot-type slot-value :ignore-missing-values-p t keys))

(defmethod render-form-aux (obj slot-name (slot-type (eql 'boolean)) slot-value &rest
			    keys &key inlinep slot-path intermediate-fields &allow-other-keys)
  (check-type slot-value boolean)
  (let* ((intermediate-value (slot-intermedia-value slot-name intermediate-fields))
	 (checkedp (if intermediate-value
		       (cdr intermediate-value)
		       slot-value)))
    (render-checkbox slot-name checkedp)))

(defmethod parse-slot-from-request ((slot-type (eql 'boolean)) slot-name request-slot-value)
  (cond
    ((string-equal "t" request-slot-value) t)
    ((null request-slot-value) nil)
    (t (error 'parse-error))))

(defmethod slot-in-request-empty-p ((slot-type (eql 'boolean)) request-slot-value)
  nil)

