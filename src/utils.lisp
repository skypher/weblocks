
(in-package :weblocks)

(export '(safe-apply request-parameter))

(defmacro safe-apply (fn &rest args)
  "Apply 'fn' if it isn't nil. Otherwise return nil."
  `(if ,fn
       (apply ,fn ,@args)
       nil))

(defun request-parameter (name)
  "Get parameter 'name' from the request. If the request was
submitted via GET method, the parameter is obtained from the
query string. If the request was submitted via POST, the
parameter is obtained from the body of the request. Otherwise, an
error is signalled."
  (ecase (request-method)
    (:get (get-parameter name))
    (:post (post-parameter name))))

