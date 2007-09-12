
(in-package :weblocks-test)

;;; test parse-slot-from-request for keywords
(deftest parse-slot-from-request-keywords-1
    (parse-slot-from-request 'keyword 'test "test")
  t :test)

