
(in-package :weblocks-test)

;;; test parse-slot-from-request
(deftest parse-slot-from-request-1
    (parse-slot-from-request 'integer "AGE" "25")
  t 25)

(deftest parse-slot-from-request-2
    (car (multiple-value-list (parse-slot-from-request 'integer "AGE" "2t5")))
  nil)

(deftest parse-slot-from-request-3
    (multiple-value-bind (res error)
	(ignore-errors
	  (parse-slot-from-request 'unknown-type "AGE" "25"))
      res)
  nil)

(deftest parse-slot-from-request-4
    (parse-slot-from-request t "AGE" "blah")
  t "blah")

(deftest parse-slot-from-request-5
    (parse-slot-from-request 'string "AGE" "blah")
  t "blah")

(deftest parse-slot-from-request-6
    (car (multiple-value-list (parse-slot-from-request '(mod 10) "AGE" "blah")))
  nil)

(deftest parse-slot-from-request-7
    (parse-slot-from-request '(mod 10) "AGE" "5")
  t 5)

;;; test complex parsers
(deftest parse-slot-from-request-member-1
    (weblocks::invoke-parsers-on-slot-with-count '(member test 1) 'age "25")
  nil)

(deftest parse-slot-from-request-member-2
    (weblocks::invoke-parsers-on-slot-with-count '(member test 1) 'age "1")
  t 1)

(deftest parse-slot-from-request-member-3
    (weblocks::invoke-parsers-on-slot-with-count '(member test 1) 'age "foo")
  nil)

(deftest parse-slot-from-request-member-4
    (weblocks::invoke-parsers-on-slot-with-count '(member test 1) 'age "test")
  t test)

(deftest parse-slot-from-request-member-5
    (weblocks::invoke-parsers-on-slot-with-count '(member :a :b) 'age "a")
  t :a)

(deftest parse-slot-from-request-eql-1
    (weblocks::invoke-parsers-on-slot-with-count '(eql 5) 'age "6")
  t 6)

(deftest parse-slot-from-request-eql-2
    (car (multiple-value-list (weblocks::invoke-parsers-on-slot-with-count '(eql 5) 'age "test")))
  nil)

(deftest parse-slot-from-request-or-1
    (weblocks::invoke-parsers-on-slot-with-count '(or foo bar integer) 'age "25")
  t 25)

(deftest parse-slot-from-request-or-2
    (weblocks::invoke-parsers-on-slot-with-count '(or foo bar) 'age "25")
  nil)

(deftest parse-slot-from-request-or-3
    (weblocks::invoke-parsers-on-slot-with-count '(or) 'age "25")
  nil)

(deftest parse-slot-from-request-and-1
    (weblocks::invoke-parsers-on-slot-with-count '(and foo bar integer) 'age "25")
  t 25)

(deftest parse-slot-from-request-and-2
    (weblocks::invoke-parsers-on-slot-with-count '(and foo bar) 'age "25")
  nil)

(deftest parse-slot-from-request-satisfies-1
    (weblocks::invoke-parsers-on-slot-with-count '(satisfies integerp) 'age "25")
  t 25)

;;; test parser recursion
(deftype recursive-test-type () '(or recursive-test-type))

(deftest invoke-parsers-on-slot-recursive-1
    (weblocks::invoke-parsers-on-slot-with-count 'recursive-test-type 'age "25")
  nil)

;;; test invoke-parsers-on-slot/invoke-parsers-on-slot-with-count
(deftest invoke-parsers-on-slot-1
    (weblocks::invoke-parsers-on-slot-with-count '(or null integer) 'age "25")
  t 25)

(deftest invoke-parsers-on-slot-2
    (weblocks::invoke-parsers-on-slot-with-count '(or blah integer) 'age "25")
  t 25)

(deftest invoke-parsers-on-slot-3
    (weblocks::invoke-parsers-on-slot-with-count '(or blah integer) 'age "2t5")
  nil)

(deftest invoke-parsers-on-slot-4
    (weblocks::invoke-parsers-on-slot-with-count '(or foo bar) 'age "25")
  nil)

(deftest invoke-parsers-on-slot-5
    (weblocks::invoke-parsers-on-slot-with-count '(or) 'age "25")
  nil)

