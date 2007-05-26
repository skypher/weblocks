
(in-package :weblocks-test)

;;; utilities for easier testing
(defun link-action-template (action name)
  `(:a :href ,(format nil "?action=~A" action)
       :onclick ,(format nil "initiateAction(\"~A\", ~
                              \"weblocks-session=1%3Atest\"); ~
                              return false;" action)
       ,name))

