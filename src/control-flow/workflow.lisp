
(in-package :weblocks)

(export '(with-flow yield))

(defmacro with-flow (widget &body body)
  "Eases the burden of creating flows. Instead of using 'do-widget' in
the body, one can use 'yield', which will expand into appropriate
'do-widget' code."
  (let ((w (gensym)))
    `(let ((,w ,widget))
       (declare (ignorable ,w))
       (macrolet ((yield (target)
		    `(do-widget ,',w ,target)))
	 (with-call/cc
	   ,@body)))))

