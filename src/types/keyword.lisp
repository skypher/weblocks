
(in-package :weblocks)

(defslotmethod parse-slot-from-request (obj slot-name (slot-type (eql 'keyword)) request-slot-value)
  (values t (intern
	     (parse-symbol-from-request request-slot-value)
	     (find-package :keyword))))

