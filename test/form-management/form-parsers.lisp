
(in-package :weblocks-test)

;;; test parse-slot-from-request
(deftest parse-slot-from-request-1
    (parse-slot-from-request 'integer "AGE" "25")
  25 2)

(deftest parse-slot-from-request-2
    (multiple-value-bind (success error)
	(ignore-errors
	  (parse-slot-from-request 'integer "AGE" "2t5"))
      (values success (format nil "~A" error)))
  nil
  "Age must be an integer.")

(deftest parse-slot-from-request-3
    (parse-slot-from-request 'unknown-type "AGE" "25")
  "25")

(deftest parse-slot-from-request-4
    (parse-slot-from-request nil "AGE" "25")
  "25")
