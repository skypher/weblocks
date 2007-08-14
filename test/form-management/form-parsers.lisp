
(in-package :weblocks-test)

;;; test parsers-from-typespec-aux
(deftest parsers-from-typespec-aux-1
    (weblocks::parsers-from-typespec-aux '(or integer string))
  (integer string))

(deftest parsers-from-typespec-aux-2
    (weblocks::parsers-from-typespec-aux 'integer)
  (integer))

(deftest parsers-from-typespec-aux-3
    (weblocks::parsers-from-typespec-aux '(mod 4))
  (integer))

(deftest parsers-from-typespec-aux-4
    (weblocks::parsers-from-typespec-aux '(integer 1 3))
  (integer))

(deftest parsers-from-typespec-aux-5
    (weblocks::parsers-from-typespec-aux '(or null integer))
  (integer))

;;; test parsers-from-typespec
(deftest parsers-from-typespec-1
    (weblocks::parsers-from-typespec '(or integer (mod 4)))
  (integer))

(deftest parsers-from-typespec-2
    (weblocks::parsers-from-typespec '(or (member hi) (and (not bit)) (and string (or string))
				       (and pathname vector)))
  (symbol string pathname vector array base-string))

(deftest parsers-from-typespec-3
    (weblocks::parsers-from-typespec 'foo2)
  (foo2 integer))

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
  "junk in string \"2t5\"")

(deftest parse-slot-from-request-3
    (multiple-value-bind (res error)
	(ignore-errors
	  (parse-slot-from-request 'unknown-type "AGE" "25"))
      res)
  nil)

(deftest parse-slot-from-request-4
    (parse-slot-from-request t "AGE" "25")
  "25")

;;; test invoke-parsers-on-slot
(deftest invoke-parsers-on-slot-1
    (weblocks::invoke-parsers-on-slot '(or null integer) 'age "25")
  t 25)

(deftest invoke-parsers-on-slot-2
    (weblocks::invoke-parsers-on-slot '(or blah integer) 'age "25")
  t 25)

(deftest invoke-parsers-on-slot-3
    (weblocks::invoke-parsers-on-slot '(or blah integer) 'age "2t5")
  nil)

(deftest invoke-parsers-on-slot-4
    (multiple-value-bind (result error)
	(ignore-errors
	  (weblocks::invoke-parsers-on-slot '(or foo bar) 'age "25"))
      (format nil "~A" error))
  "No specialized methods for any of the parsers obtained from (OR FOO BAR)")

(deftest invoke-parsers-on-slot-5
    (multiple-value-bind (result error)
	(ignore-errors
	  (weblocks::invoke-parsers-on-slot '(or) 'age "25"))
      (format nil "~A" error))
  "No parsers could be obtained from type specifier (OR)")

