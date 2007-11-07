
(in-package :weblocks)

(export '(with-flow yield))

(defun yield->do-place (expr place)
  "Walks the expression tree and converts calls to 'yield' to
appropriate calls to 'do-place'."
  (cond
    ((atom expr) expr)
    ((eq (car expr) 'yield) `(do-place ,place ,(cadr expr)))
    (t (mapcar (curry-after #'yield->do-place place) expr))))

(defmacro with-flow (place &body body)
  "Eases the burden of creating flows. Instead of using 'do-place' in
the body, one can use 'yield', which will expand into appropriate
'do-place' code."
  (let ((p (gensym)))
    `(let ((,p ,place))
       (declare (ignorable ,p))
       (with-call/cc
	 ,@(yield->do-place body place)))))

