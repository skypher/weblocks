
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
  (let* ((intermediate-value (slot-intermedia-value slot-name intermediate-fields))
	 (checkedp (if intermediate-value
		       (cdr intermediate-value)
		       slot-value)))
    (render-checkbox slot-name checkedp)))

(defmethod parse-slot-from-request ((slot-type (eql 'boolean)) slot-name request-slot-value)
  (cond
    ((string-equal "t" request-slot-value) t)
    ((null request-slot-value) nil)
    (t (error 'parse-error "Improper value for a boolean type."))))

(defmethod slot-in-request-empty-p ((slot-type (eql 'boolean)) request-slot-value)
  nil)

(defmethod object-satisfies-search-p (search-regex obj slot-name (slot-type (eql 'boolean))
				      slot-value &rest args)
  (not (null (ppcre:scan search-regex (if slot-value "Yes" "No")))))

