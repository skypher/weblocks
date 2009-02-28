
(in-package :weblocks-test)

;;; test session-request-hooks
(deftest session-request-hooks-1
    (with-request :get nil
      (null (weblocks::session-request-hooks)))
  nil)

