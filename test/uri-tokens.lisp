
(in-package :weblocks-test)

(deftestsuite uri-tokens-suite (weblocks-suite) nil)

(addtest init-uri-tokens
  (let ((tokens (make-instance 'uri-tokens :all '("foo")))
        (*lift-equality-test* (curry-after #'tree-equal :test #'equalp)))
    (ensure-same (remaining tokens) '("foo"))))
