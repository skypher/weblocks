
(in-package :weblocks-test)

;;; test parsers-from-typespec
(deftest parsers-from-typespec-1
    (weblocks::parsers-from-typespec '(or integer string))
  (integer string))

(deftest parsers-from-typespec-2
    (weblocks::parsers-from-typespec 'integer)
  (integer))

(deftest parsers-from-typespec-3
    (weblocks::parsers-from-typespec '(mod 4))
  (integer))

(deftest parsers-from-typespec-4
    (weblocks::parsers-from-typespec '(or integer (mod 4)))
  (integer))

(deftest parsers-from-typespec-5
    (weblocks::parsers-from-typespec '(or (member hi) (and (not bit)) (and string (or string))
				       (and pathname vector)))
  (symbol string pathname vector))

(deftest parsers-from-typespec-6
    (weblocks::parsers-from-typespec '(integer 1 3))
  (integer))

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
