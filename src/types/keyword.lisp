
(in-package :weblocks)

(defslotmethod parse-slot-from-request ((slot-type (eql 'keyword)) slot-name request-slot-value)
  (values t (intern
	     (parse-symbol-from-request request-slot-value)
	     (find-package :keyword))))

