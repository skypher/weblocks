
(in-package :weblocks)

(export '(disable-global-debugging enable-global-debugging *weblocks-global-debug*))

(declaim (special *current-webapp* *maintain-last-session*))

(defvar *weblocks-global-debug* nil)

(defun enable-global-debugging ()
  "Setup hooks for session maintenance and showing backtraces"
  (setf *weblocks-global-debug* t)
  ;; Set hunchentoot defaults (for everyone)
  (setf *show-lisp-errors-p* t)
  (setf *process-html-parts-p* (constantly t))
  ;(setf *show-lisp-backtraces-p* t)
  ;; Set session maintenance (for everyone)
  (unless *maintain-last-session*
    (setf *maintain-last-session*
          (bordeaux-threads:make-lock "*maintain-last-session*"))))

(defun disable-global-debugging ()
  "A manual method for resetting global debugging state"
  (setf *weblocks-global-debug* nil)
  (setf *show-lisp-errors-p* nil)
  (setf *process-html-parts-p* (constantly nil))
  ;(setf *show-lisp-backtraces-p* nil)
  (setf *maintain-last-session* nil))

;;; Further aid in debugging by reporting potential problems

(defun style-warn (condition &rest warn-args)
  "A variant of `warn' that doesn't guarantee evaluation of its
arguments."
  (apply #'warn condition warn-args))

(define-compiler-macro style-warn (condition &rest warn-args)
  `(when (or (not (boundp '*current-webapp*))
             (null *current-webapp*)
             (weblocks-webapp-debug (current-webapp)))
     (warn ,condition . ,warn-args)))

(define-condition webapp-style-warning (style-warning)
  ()
  (:documentation "Signalled by Weblocks when detecting unwise
behavior on the part of webapps."))

(define-condition non-idempotent-rendering (webapp-style-warning)
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

(define-condition misunderstood-action (webapp-style-warning)
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

(define-condition missing-default-store (webapp-style-warning)
  ((webapp :initarg :webapp :reader webapp-missing-default-store
           :documentation "The webapp lacking one at start-time."))
  (:report report-missing-default-store)
  (:documentation "Signalled when a webapp lacking a `*default-store*'
  is started."))

(defun report-missing-default-store (c stream)
  "Describe a `missing-default-store'."
  (format stream "~A has no default store defined ~
                  (try ~S or moving ~S after ~S)"
          (webapp-missing-default-store c)
          :default-store 'defstore 'defwebapp))
