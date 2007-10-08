
(in-package :weblocks-test)

;;; test parse-foreign-object
(deftest parse-foreign-object-1
    (multiple-value-bind (res value)
	(weblocks::parse-foreign-object *joe* 'address-ref t "*work-address*")
      (values res (object-id value)))
  t "*work-address*")

(deftest parse-foreign-object-2
    (weblocks::parse-foreign-object *joe* 'address-ref t "foo-bar")
  nil nil)

;;; test parse-slot-from-request
(deftest parse-slot-from-request-1
    (parse-slot-from-request nil "AGE" 'integer "25")
  t 25)

(deftest parse-slot-from-request-2
    (car (multiple-value-list (parse-slot-from-request nil "AGE" 'integer "2t5")))
  nil)

(deftest parse-slot-from-request-3
    (multiple-value-bind (res error)
	(ignore-errors
	  (parse-slot-from-request nil "AGE" 'unknown-type "25"))
      res)
  nil)

(deftest parse-slot-from-request-4
    (parse-slot-from-request nil "AGE" t "blah")
  t "blah")

(deftest parse-slot-from-request-5
    (parse-slot-from-request nil "AGE" 'string "blah")
  t "blah")

(deftest parse-slot-from-request-6
    (car (multiple-value-list (parse-slot-from-request nil "AGE" '(mod 10) "blah")))
  nil)

(deftest parse-slot-from-request-7
    (parse-slot-from-request nil "AGE" '(mod 10) "5")
  t 5)

(deftest parse-slot-from-request-8
    (multiple-value-bind (result value)
	(parse-slot-from-request *joe* 'address-ref t "*home-address*")
      (values result (object-id value)))
  t "*home-address*")

(deftest parse-slot-from-request-9
    (parse-slot-from-request *joe* 'address-ref t "*foo-bar*")
  nil nil)

;;; test complex parsers
(deftest parse-slot-from-request-member-1
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(member test 1) "25")
  nil)

(deftest parse-slot-from-request-member-2
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(member test 1) "1")
  t 1)

(deftest parse-slot-from-request-member-3
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(member test 1) "foo")
  nil)

(deftest parse-slot-from-request-member-4
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(member test 1) "test")
  t test)

(deftest parse-slot-from-request-member-5
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(member :a :b) "a")
  t :a)

(deftest parse-slot-from-request-eql-1
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(eql 5) "6")
  t 6)

(deftest parse-slot-from-request-eql-2
    (car (multiple-value-list (weblocks::invoke-parsers-on-slot-with-count nil 'age '(eql 5) "test")))
  nil)

(deftest parse-slot-from-request-or-1
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(or foo bar integer) "25")
  t 25)

(deftest parse-slot-from-request-or-2
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(or foo bar) "25")
  nil)

(deftest parse-slot-from-request-or-3
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(or) "25")
  nil)

(deftest parse-slot-from-request-and-1
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(and foo bar integer) "25")
  t 25)

(deftest parse-slot-from-request-and-2
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(and foo bar) "25")
  nil)

(deftest parse-slot-from-request-satisfies-1
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(satisfies integerp) "25")
  t 25)

;;; test parser recursion
(deftype recursive-test-type () '(or recursive-test-type))

(deftest invoke-parsers-on-slot-recursive-1
    (weblocks::invoke-parsers-on-slot-with-count nil 'age 'recursive-test-type "25")
  nil)

;;; test invoke-parsers-on-slot/invoke-parsers-on-slot-with-count
(deftest invoke-parsers-on-slot-1
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(or null integer) "25")
  t 25)

(deftest invoke-parsers-on-slot-2
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(or blah integer) "25")
  t 25)

(deftest invoke-parsers-on-slot-3
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(or blah integer) "2t5")
  nil)

(deftest invoke-parsers-on-slot-4
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(or foo bar) "25")
  nil)

(deftest invoke-parsers-on-slot-5
    (weblocks::invoke-parsers-on-slot-with-count nil 'age '(or) "25")
  nil)

