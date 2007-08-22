
(in-package :weblocks)

(defmethod parse-slot-from-request ((slot-type (eql 'keyword)) slot-name request-slot-value)
  (intern
   (parse-symbol-from-request request-slot-value)
   (find-package :keyword)))

