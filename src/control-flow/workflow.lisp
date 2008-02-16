
(in-package :weblocks)

(export '(with-flow yield))

(defun yield->do-widget (expr widget)
  "Walks the expression tree and converts calls to 'yield' to
appropriate calls to 'do-widget'."
  (cond
    ((atom expr) expr)
    ((eq (car expr) 'yield) `(do-widget ,widget ,(cadr expr)))
    (t (mapcar (curry-after #'yield->do-widget widget) expr))))

(defmacro with-flow (widget &body body)
  "Eases the burden of creating flows. Instead of using 'do-widget' in
the body, one can use 'yield', which will expand into appropriate
'do-widget' code."
  (let ((w (gensym)))
    `(let ((,w ,widget))
       (declare (ignorable ,w))
       (with-call/cc
	 ,@(yield->do-widget body w)))))

