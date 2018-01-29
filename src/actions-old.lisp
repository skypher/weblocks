(in-package :weblocks)

(export '(page-not-found-handler
          make-action-url make-action function-or-action->action))



;; TODO add to documentation
(defun make-js-action (action)
  "Returns a code which can be inserted into onclick attribute and will
execute given Lisp function on click.

It accepts any function as input and produces a string with JavaScript code.
"
  
  (let* ((action-code (function-or-action->action action)))
    (format nil "initiateAction(\"~A\", \"~A\"); return false;"
            action-code (session-name-string-pair))))
