(defpackage #:weblocks/utils/warn
  (:use #:cl)
  (:shadow #:style-warning)
  (:export #:style-warn
           #:style-warning
           #:non-idempotent-rendering
           #:misunderstood-action))
(in-package weblocks/utils/warn)


(defun style-warn (condition &rest warn-args)
  "A variant of `warn' that doesn't guarantee evaluation of its
arguments."
  (apply #'warn condition warn-args))


(define-compiler-macro style-warn (condition &rest warn-args)
  `(when (or (not (boundp '*current-webapp*))
             (null *current-webapp*)
             (weblocks-webapp-debug *current-webapp*))
     (warn ,condition . ,warn-args)))


(define-condition style-warning (cl:style-warning)
  ()
  (:documentation "Signalled by Weblocks when detecting unwise
behavior on the part of webapps."))


(define-condition non-idempotent-rendering (style-warning)
  ((change-made :initarg :change-made :reader change-made-during-rendering
                :documentation "A description of the change that
                should be moved to action handling."))
  (:report report-non-idempotent-rendering)
  (:documentation "Signalled in common cases where code that alters
the webapp state appears in a rendering process."))


(defun report-non-idempotent-rendering (c stream)
  "Describe a condition where code appears in rendering that should be
in the action handler."
  (format stream "During the rendering phase, ~A, which should ~
                  typically be done only during action handling"
          (change-made-during-rendering c)))

(define-condition misunderstood-action (style-warning)
  ((action :initarg :action :reader misunderstood-action
           :documentation "What the user did to reveal this problem.")
   (missing :initarg :missing :reader missing-action-handler-part
            :documentation "A description of what's missing for ACTION
            to be handled correctly."))
  (:report report-misunderstood-action)
  (:documentation "Signalled when a user invoked an action, and some
part of the handler for which the app writer is responsible doesn't
seem to be implemented correctly."))

(defun report-misunderstood-action (c stream)
  "Describe a `misunderstood-action'."
  (format stream "A webapp user did: \"~A\"
But it was handled incorrectly; this is probably an issue with ~A"
          (misunderstood-action c) (missing-action-handler-part c)))
