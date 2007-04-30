
(in-package :weblocks-test)

;;; render function as a widget
(deftest-html render-function-1
    (render (lambda (&rest args)
	      (with-html (:p "blah"))))
  (:p "blah"))
