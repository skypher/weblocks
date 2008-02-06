
(in-package :weblocks-test)

;;; Test presentation-choices-default-label-key
(deftest presentation-choices-default-label-key-1
    (presentation-choices-default-label-key 'foo)
  "Foo")

(deftest presentation-choices-default-label-key-2
    (presentation-choices-default-label-key *joe*)
  "Employee")

;;; Test presentation-choices-default-value-key
(deftest presentation-choices-default-value-key-1
    (presentation-choices-default-value-key 'foo)
  "foo")

(deftest presentation-choices-default-value-key-2
    (presentation-choices-default-value-key *joe*)
  "1")

;;; Test obtain-presentation-choices
(deftest obtain-presentation-choices-1
    (obtain-presentation-choices (make-instance 'choices-presentation-mixin
						:choices (list (cons 1 2)
							       (cons 3 4)))
				 *joe*)
  (("1" . "2") ("3" . "4")))

(deftest obtain-presentation-choices-2
    (obtain-presentation-choices (make-instance 'choices-presentation-mixin
						:choices (list *joe* *bob*))
				 *joe*)
  (("Employee" . "1") ("Employee" . "2")))

