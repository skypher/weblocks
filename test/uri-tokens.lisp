
(in-package :weblocks-test)

(deftestsuite uri-tokens-suite (weblocks-suite) nil)

(addtest init-uri-tokens
  (let ((tokens (make-instance 'uri-tokens :tokens '("foo")))
        (*lift-equality-test* (curry-after #'tree-equal :test #'equalp)))
    (ensure-same (consumed-tokens tokens) nil)
    (ensure-same (remaining-tokens tokens) '("foo"))))

