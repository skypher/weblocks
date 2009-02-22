
(in-package :weblocks-test)

;;; Test view-argument-quoting-strategy
(deftest view-argument-quoting-strategy-1
    (view-argument-quoting-strategy 'some-arg)
  :none)

;;; Test quote-property-list-arguments
(deftest quote-property-list-arguments-1
    (weblocks::quote-property-list-arguments '(:a 1 :b 2 :c 3))
  (:a 1 :b 2 :c 3))

(deftest quote-property-list-arguments-2
    (weblocks::quote-property-list-arguments '(:a 1 :initform foo :b 2))
  (:a 1 :initform 'foo :b 2))

