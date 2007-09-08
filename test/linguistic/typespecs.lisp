
(in-package :weblocks-test)

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

;;; test humanize-typespec
(deftest humanize-typespec-1
    (weblocks::humanize-typespec '(or integer string))
  "either an integer or a string")

(deftest humanize-typespec-2
    (weblocks::humanize-typespec 'integer)
  "an integer")

;;; test humanize-atomic-typespec-aux
(deftest humanize-atomic-typespec-aux-1
    (humanize-atomic-typespec-aux 'integer)
  "an integer")

(deftest humanize-atomic-typespec-aux-2
    (multiple-value-bind (res err)
	(ignore-errors (humanize-atomic-typespec-aux '(integer)))
      res)
  nil)

(deftest humanize-atomic-typespec-aux-3
    (humanize-atomic-typespec-aux 'null)
  "empty")

(deftest humanize-atomic-typespec-aux-4
    (humanize-atomic-typespec-aux 'integer :adjectives "even, positive")
  "an even, positive integer")

(deftest humanize-atomic-typespec-aux-5
    (humanize-atomic-typespec-aux 'integer :adjectives "positive, even")
  "a positive, even integer")

(deftest humanize-atomic-typespec-aux-6
    (humanize-atomic-typespec-aux 'null  :adjectives "even, positive")
  "even, positive, and empty")

;;; test humanize-compound-typespec-aux (integer)
(deftest humanize-compound-typespec-aux-integer-1
    (multiple-value-bind (res err)
	(ignore-errors (humanize-compound-typespec-aux 'integer nil))
      res)
  nil)

(deftest humanize-compound-typespec-aux-integer-2
    (humanize-compound-typespec-aux 'integer '(3))
  "an integer greater than or equal to 3")

(deftest humanize-compound-typespec-aux-integer-3
    (humanize-compound-typespec-aux 'integer '(*))
  "an integer")

(deftest humanize-compound-typespec-aux-integer-4
    (humanize-compound-typespec-aux 'integer '(3 7))
  "an integer between 3 and 7")

(deftest humanize-compound-typespec-aux-integer-5
    (humanize-compound-typespec-aux 'integer '(* 7))
  "an integer less than or equal to 7")

(deftest humanize-compound-typespec-aux-integer-6
    (humanize-compound-typespec-aux 'integer '(3 *))
  "an integer greater than or equal to 3")

(deftest humanize-compund-typespec-aux-integer-7
    (humanize-compound-typespec-aux 'integer '(1 5) :adjectives "positive, even")
  "a positive, even integer between 1 and 5")

;;; test  humanize-compound-typespec-aux (satisfies)
(deftest humanize-compound-typespec-aux-satisfies-1
    (multiple-value-bind (res err)
	(ignore-errors (humanize-compound-typespec-aux 'satisfies nil))
      res)
  nil)

(deftest humanize-compound-typespec-aux-satisfies-2
    (humanize-compound-typespec-aux 'satisfies '(lowercasep))
  "lowercase")

;;; test humanize-compound-typespec-aux (or)
(deftest humanize-compound-typespec-aux-or-1
    (multiple-value-bind (res err)
	(ignore-errors (humanize-compound-typespec-aux 'or nil))
      res)
  nil)

(deftest humanize-compound-typespec-aux-or-2
    (humanize-compound-typespec-aux 'or '(integer))
  "an integer")

(deftest humanize-compound-typespec-aux-or-3
    (humanize-compound-typespec-aux 'or '(integer string))
  "either an integer or a string")

(deftest humanize-compound-typespec-aux-or-4
    (humanize-compound-typespec-aux 'or '(integer string pathname))
  "either an integer, a string, or a pathname")

(deftest humanize-compound-typespec-aux-or-5
    (humanize-compound-typespec-aux 'or '(integer string pathname null))
  "either an integer, a string, or a pathname")

(deftest humanize-compound-typespec-aux-or-6
    (humanize-compound-typespec-aux 'or '(null integer))
  "an integer")

(deftest humanize-compound-typespec-aux-or-7
    (humanize-compound-typespec-aux 'or '(null integer string))
  "either an integer or a string")

;;; test humanize-compound-typespec-aux (and)
(deftest humanize-compound-typespec-aux-and-1
    (humanize-compound-typespec-aux 'and nil)
  "anything you want")

(deftest humanize-compound-typespec-aux-and-2
    (humanize-compound-typespec-aux 'and '(integer))
  "an integer")

(deftest humanize-compound-typespec-aux-and-3
    (humanize-compound-typespec-aux 'and '(integer string))
  "an integer and a string")

(deftest humanize-compound-typespec-aux-and-4
    (humanize-compound-typespec-aux 'and '(integer string pathname))
  "an integer, a string, and a pathname")

(deftest humanize-compound-typespec-aux-and-5
    (humanize-compound-typespec-aux 'and '(integer (satisfies evenp)))
  "an integer and even")

(deftest humanize-compound-typespec-aux-and-6
    (humanize-compound-typespec-aux 'and '(integer (satisfies evenp) (satisfies positivep)))
  "an integer, even, and positive")

(deftest humanize-compound-typespec-aux-and-7
    (humanize-compound-typespec-aux 'and '((satisfies evenp) (satisfies positivep) (integer 1 100)))
  "an even, positive integer between 1 and 100")

(deftest humanize-compound-typespec-aux-and-8
    (humanize-compound-typespec-aux 'and '((satisfies evenp)))
  "even")

(deftest humanize-compound-typespec-aux-and-9
    (humanize-compound-typespec-aux 'and '((satisfies evenp) (satisfies positivep) integer))
  "an even, positive integer")

(deftest humanize-compound-typespec-aux-and-10
    (humanize-compound-typespec-aux 'and '((satisfies evenp) (satisfies positivep) (integer 1 100)))
  "an even, positive integer between 1 and 100")

(deftest humanize-compound-typespec-aux-and-11
    (humanize-compound-typespec-aux 'and '((satisfies evenp) (satisfies positivep) integer
				       (satisfies nicep) string))
  "an even, positive integer and a nice string")

(deftest humanize-compound-typespec-aux-and-12
    (humanize-compound-typespec-aux 'and '((satisfies evenp) (satisfies positivep) (or integer string)
				       (satisfies nicep) string))
  "even, positive integer or string and a nice string")

(deftest humanize-compound-typespec-aux-and-13
    (humanize-compound-typespec-aux 'and '((satisfies evenp) (satisfies positivep) (or integer string pathname)
				       (satisfies nicep) string))
  "even, positive integer, string, or pathname and a nice string")

(deftest humanize-compound-typespec-aux-and-14
    (humanize-compound-typespec-aux 'and '((satisfies evenp) (satisfies positivep) (and integer string pathname)
				       (satisfies nicep) string))
  "even, positive integer, string, and pathname and a nice string")

(deftest humanize-compound-typespec-aux-and-15
    (humanize-compound-typespec-aux 'and '((satisfies evenp) (satisfies positivep) (and integer string pathname)
				       (satisfies nicep) string))
  "even, positive integer, string, and pathname and a nice string")

;;; test humanize-compound-typespec-aux (eql)
(deftest humanize-compound-typespec-aux-eql-1
    (humanize-compound-typespec-aux 'eql '(5))
  "equal to 5")

(deftest humanize-compound-typespec-aux-eql-2
    (humanize-compound-typespec-aux 'and '((satisfies evenp) (satisfies positivep) (eql 3)))
  "even, positive, and equal to 3")


;;; test humanize-compound-typespec-aux (not)
(deftest humanize-compound-typespec-aux-not-1
    (humanize-compound-typespec-aux 'not '(integer))
  "not an integer")

(deftest humanize-compound-typespec-aux-not-2
    (humanize-compound-typespec-aux 'and '((satisfies evenp) (satisfies positivep) (not integer)))
  "even, positive, and not an integer")

;;; test humanize-compound-typespec-aux (mod)
(deftest humanize-compound-typespec-aux-mod-1
    (humanize-compound-typespec-aux 'mod '(8))
  "a non-negative integer less than 8")

(deftest humanize-compound-typespec-aux-mod-2
    (humanize-compound-typespec-aux 'and '((satisfies evenp) (satisfies positivep) (mod 5)))
  "an even, positive, non-negative integer less than 5")

;;; test humanize-compound-typespec-aux-member-1
(deftest humanize-compound-typespec-aux-member-1
    (multiple-value-bind (res err)
	(ignore-errors (humanize-compound-typespec-aux 'member nil))
      res)
  nil)

(deftest humanize-compound-typespec-aux-member-2
    (humanize-compound-typespec-aux 'member '(1))
  "1")

(deftest humanize-compound-typespec-aux-member-3
    (humanize-compound-typespec-aux 'member '(1 2))
  "either 1 or 2")

(deftest humanize-compound-typespec-aux-member-4
    (humanize-compound-typespec-aux 'member '(1 2 3 4))
  "either 1, 2, 3, or 4")

(deftest humanize-compound-typespec-aux-member-5
    (humanize-compound-typespec-aux 'and '((satisfies oddp) (member 1 2 3)))
  "odd and either 1, 2, or 3")

(deftest humanize-compound-typespec-aux-member-6
    (humanize-compound-typespec-aux 'member '(hi))
  "Hi")

(deftest humanize-compound-typespec-aux-member-7
    (humanize-compound-typespec-aux 'member '(hi bye))
  "either Hi or Bye")

;;; test humanize-compound-typespec-aux-values-1
(deftest humanize-compound-typespec-aux-values-1
    (multiple-value-bind (res err)
	(ignore-errors (humanize-compound-typespec-aux 'values nil))
      res)
  nil)

;;; default
(deftest humanize-compound-typespec-aux-default-1
    (humanize-compound-typespec-aux 'weird-type '(1 2 3))
  "a weird type")

