
(in-package :weblocks-test)

;;; utilities for easier testing
(defun link-action-template (action name &key (uri "/foo/bar"))
  `(:a :href ,(format nil "~A?action=~A" uri action)
       :onclick ,(format nil "initiateAction(\"~A\", ~
                              \"weblocks-session=1%3ATEST\"); ~
                              return false;" action)
       ,name))

