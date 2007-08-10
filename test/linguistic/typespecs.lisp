
(in-package :weblocks-test)

;;; test value-required-p
(deftest value-required-p-1
    (value-required-p '(or string integer))
  t)

(deftest value-required-p-2
    (value-required-p '(or string integer null))
  nil)

;;; test typespec-compound-only-p
(deftest typespec-compound-only-p-1
    (values (typespec-compound-only-p 'and) (typespec-compound-only-p 'satisfies)
	    (typespec-compound-only-p 'eql) (typespec-compound-only-p 'member)
	    (typespec-compound-only-p 'mod) (typespec-compound-only-p 'values)
	    (typespec-compound-only-p 'not) (typespec-compound-only-p 'or)
	    (typespec-compound-only-p 'int))
  t t t t t t t t nil)

;;; test typespec-accepts-adjectives-p
(deftest typespec-accepts-adjectives-p-1
    (values (weblocks::typespec-accepts-adjectives-p 'not)
	    (weblocks::typespec-accepts-adjectives-p 'eql)
	    (weblocks::typespec-accepts-adjectives-p 'and)
	    (weblocks::typespec-accepts-adjectives-p 'or)
	    (weblocks::typespec-accepts-adjectives-p 'satisfies)
	    (weblocks::typespec-accepts-adjectives-p 'member))
  nil nil t t nil nil)

(deftest typespec-accepts-adjectives-p-2
    (values (weblocks::typespec-accepts-adjectives-p '(and a b))
	    (weblocks::typespec-accepts-adjectives-p '(not 1)))
  t nil)

;;; test typespec-to-error

;;; test typespec-to-error-aux

;;; test atomic-typespec-to-error
(deftest atomic-typespec-to-error-1
    (atomic-typespec-to-error 'integer)
  "an integer")

(deftest atomic-typespec-to-error-2
    (multiple-value-bind (res err)
	(ignore-errors (atomic-typespec-to-error '(integer)))
      res)
  nil)

(deftest atomic-typespec-to-error-3
    (atomic-typespec-to-error 'null)
  "empty")

(deftest atomic-typespec-to-error-4
    (atomic-typespec-to-error 'integer :adjectives "even, positive")
  "an even, positive integer")

(deftest atomic-typespec-to-error-5
    (atomic-typespec-to-error 'integer :adjectives "positive, even")
  "a positive, even integer")

(deftest atomic-typespec-to-error-6
    (atomic-typespec-to-error 'null  :adjectives "even, positive")
  "even, positive, and empty")

(deftest atomic-typespec-to-error-7
    (compound-typespec-to-error 'integer '(1 5) :adjectives "positive, even")
  "a positive, even integer between 1 and 5")

;;; test compound-typespec-to-error (integer)
(deftest compound-typespec-to-error-integer-1
    (multiple-value-bind (res err)
	(ignore-errors (compound-typespec-to-error 'integer nil))
      res)
  nil)

(deftest compound-typespec-to-error-integer-2
    (compound-typespec-to-error 'integer '(3))
  "an integer greater than or equal to 3")

(deftest compound-typespec-to-error-integer-3
    (compound-typespec-to-error 'integer '(*))
  "an integer")

(deftest compound-typespec-to-error-integer-4
    (compound-typespec-to-error 'integer '(3 7))
  "an integer between 3 and 7")

(deftest compound-typespec-to-error-integer-5
    (compound-typespec-to-error 'integer '(* 7))
  "an integer less than or equal to 7")

(deftest compound-typespec-to-error-integer-6
    (compound-typespec-to-error 'integer '(3 *))
  "an integer greater than or equal to 3")

;;; test  compound-typespec-to-error (satisfies)
(deftest compound-typespec-to-error-satisfies-1
    (multiple-value-bind (res err)
	(ignore-errors (compound-typespec-to-error 'satisfies nil))
      res)
  nil)

(deftest compound-typespec-to-error-satisfies-2
    (compound-typespec-to-error 'satisfies '(lowercasep))
  "lowercase")

;;; test compound-typespec-to-error (or)
(deftest compound-typespec-to-error-or-1
    (multiple-value-bind (res err)
	(ignore-errors (compound-typespec-to-error 'or nil))
      res)
  nil)

(deftest compound-typespec-to-error-or-2
    (compound-typespec-to-error 'or '(integer))
  "an integer")

(deftest compound-typespec-to-error-or-3
    (compound-typespec-to-error 'or '(integer string))
  "either an integer or a string")

(deftest compound-typespec-to-error-or-4
    (compound-typespec-to-error 'or '(integer string pathname))
  "either an integer, a string, or a pathname")

(deftest compound-typespec-to-error-or-5
    (compound-typespec-to-error 'or '(integer string pathname null))
  "either an integer, a string, or a pathname")

;;; test compound-typespec-to-error (and)
(deftest compound-typespec-to-error-and-1
    (compound-typespec-to-error 'and nil)
  "anything you want")

(deftest compound-typespec-to-error-and-2
    (compound-typespec-to-error 'and '(integer))
  "an integer")

(deftest compound-typespec-to-error-and-3
    (compound-typespec-to-error 'and '(integer string))
  "an integer and a string")

(deftest compound-typespec-to-error-and-4
    (compound-typespec-to-error 'and '(integer string pathname))
  "an integer, a string, and a pathname")

(deftest compound-typespec-to-error-and-5
    (compound-typespec-to-error 'and '(integer (satisfies evenp)))
  "an integer and even")

(deftest compound-typespec-to-error-and-6
    (compound-typespec-to-error 'and '(integer (satisfies evenp) (satisfies positivep)))
  "an integer, even, and positive")

(deftest compound-typespec-to-error-and-7
    (compound-typespec-to-error 'and '((integer 1 100) (satisfies evenp) (satisfies positivep)))
  "an even, positive integer between 1 and 100")

(deftest compound-typespec-to-error-and-8
    (compound-typespec-to-error 'and '((satisfies evenp)))
  "even")

(deftest compound-typespec-to-error-and-7
    (compound-typespec-to-error 'and '((satisfies evenp) (satisfies positivep) integer))
  "an even, positive integer")

(deftest compound-typespec-to-error-and-8
    (compound-typespec-to-error 'and '((satisfies evenp) (satisfies positivep) (integer 1 100)))
  "an even, positive integer between 1 and 100")

(deftest compound-typespec-to-error-and-9
    (compound-typespec-to-error 'and '((satisfies evenp) (satisfies positivep) integer
				       (satisfies nicep) string))
  "an even, positive integer and a nice string")

(deftest compound-typespec-to-error-and-10
    (compound-typespec-to-error 'and '((satisfies evenp) (satisfies positivep) (or integer string)
				       (satisfies nicep) string))
  "even, positive integer or string and a nice string")

(deftest compound-typespec-to-error-and-11
    (compound-typespec-to-error 'and '((satisfies evenp) (satisfies positivep) (or integer string pathname)
				       (satisfies nicep) string))
  "even, positive integer, string, or pathname and a nice string")

(deftest compound-typespec-to-error-and-12
    (compound-typespec-to-error 'and '((satisfies evenp) (satisfies positivep) (and integer string pathname)
				       (satisfies nicep) string))
  "even, positive integer, string, and pathname and a nice string")

(deftest compound-typespec-to-error-and-12
    (compound-typespec-to-error 'and '((satisfies evenp) (satisfies positivep) (and integer string pathname)
				       (satisfies nicep) string))
  "even, positive integer, string, and pathname and a nice string")

;;; test compound-typespec-to-error (eql)
(deftest compound-typespec-to-error-eql-1
    (compound-typespec-to-error 'eql '(5))
  "equal to 5")

(deftest compound-typespec-to-error-eql-2
    (compound-typespec-to-error 'and '((satisfies evenp) (satisfies positivep) (eql 3)))
  "even, positive, and equal to 3")


;;; test compound-typespec-to-error (not)
(deftest compound-typespec-to-error-not-1
    (compound-typespec-to-error 'not '(integer))
  "not an integer")

(deftest compound-typespec-to-error-not-2
    (compound-typespec-to-error 'and '((satisfies evenp) (satisfies positivep) (not integer)))
  "even, positive, and not an integer")

;;; test compound-typespec-to-error (mod)
(deftest compound-typespec-to-error-mod-1
    (compound-typespec-to-error 'mod '(8))
  "a non-negative integer less than 8")

(deftest compound-typespec-to-error-mod-2
    (compound-typespec-to-error 'and '((satisfies evenp) (satisfies positivep) (mod 5)))
  "an even, positive, non-negative integer less than 5")

;;; test compound-typespec-to-error-member-1
(deftest compound-typespec-to-error-member-1
    (multiple-value-bind (res err)
	(ignore-errors (compound-typespec-to-error 'member nil))
      res)
  nil)

(deftest compound-typespec-to-error-member-2
    (compound-typespec-to-error 'member '(1))
  "1")

(deftest compound-typespec-to-error-member-3
    (compound-typespec-to-error 'member '(1 2))
  "either 1 or 2")

(deftest compound-typespec-to-error-member-4
    (compound-typespec-to-error 'member '(1 2 3 4))
  "either 1, 2, 3, or 4")

(deftest compound-typespec-to-error-member-5
    (compound-typespec-to-error 'and '((satisfies oddp) (member 1 2 3)))
  "odd and either 1, 2, or 3")

;;; test compound-typespec-to-error-values-1
(deftest compound-typespec-to-error-member-1
    (multiple-value-bind (res err)
	(ignore-errors (compound-typespec-to-error 'values nil))
      res)
  nil)

;;; default
(deftest compound-typespec-to-error-default-1
    (compound-typespec-to-error 'weird-type '(1 2 3))
  "a weird type")

