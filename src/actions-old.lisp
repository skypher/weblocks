(in-package :weblocks)

(export '(page-not-found-handler
          make-action-url make-action function-or-action->action))



(defun make-action-url (action-code &optional (include-question-mark-p t))
  "Accepts action code and returns a URL that can be used to render
the action.

Ex:

\(make-action-url \"test-action\") => \"?action=test-action\""
  (concatenate 'string
               (weblocks.request:get-path) ; we need this for w3m
               (if include-question-mark-p "?" "")
               weblocks.variables:*action-string* "="
               (quri:url-encode (princ-to-string action-code))))



;; TODO add to documentation
(defun make-js-action (action)
  "Returns a code which can be inserted into onclick attribute and will
execute given Lisp function on click.

It accepts any function as input and produces a string with JavaScript code.
"
  
  (let* ((action-code (function-or-action->action action)))
    (format nil "initiateAction(\"~A\", \"~A\"); return false;"
            action-code (session-name-string-pair))))
