
(in-package :weblocks)

(defmethod render-data-aux (obj slot-name (slot-type (eql 'boolean))
			    slot-value &rest keys &key highlight &allow-other-keys)
  (let* ((item (if slot-value "Yes" "No")))
    (with-html
      (:span :class "value"
	     (str (if highlight
		      (highlight-regex-matches item highlight)
		      (escape-for-html item)))))))

(defmethod render-form-aux (obj slot-name (slot-type (eql 'boolean)) slot-value &rest
			    keys &key inlinep slot-path intermediate-fields &allow-other-keys)
  (check-type slot-value boolean)
  (let* ((attributized-slot-name (attributize-name slot-name))
	 (intermediate-value (assoc attributized-slot-name intermediate-fields :test #'string-equal))
	 checkedp)
    (if intermediate-value
	(setf checkedp (cdr intermediate-value))
	(when slot-value
	  (setf checkedp t)))
    (with-html
      (if checkedp
	  (htm (:input :type "checkbox" :class "checkbox" :name attributized-slot-name
		       :value "t" :checked "yes"))
	  (htm (:input :type "checkbox" :class "checkbox" :name attributized-slot-name
		       :value "t"))))))

(defmethod parse-slot-from-request ((slot-type (eql 'boolean)) slot-name request-slot-value)
  (cond
    ((string-equal "t" request-slot-value) t)
    ((null request-slot-value) nil)
    (t (error 'parse-error "Improper value for a boolean type."))))

(defmethod slot-in-request-empty-p ((slot-type (eql 'boolean)) request-slot-value)
  nil)

