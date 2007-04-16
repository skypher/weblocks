
(in-package :weblocks-test)

;;; test safe-apply
(deftest safe-apply-1
    (safe-apply #'identity '(5))
  5)

(deftest safe-apply-2
    (safe-apply nil '(5))
  nil)

;;; test request-parameter
(deftest request-parameter-1
    (with-request :get '(("a" . 1) ("b" . 2))
      (request-parameter "a"))
  1)

(deftest request-parameter-2
    (with-request :post '(("a" . 1) ("b" . 2))
      (request-parameter "b"))
  2)
