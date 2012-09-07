
(in-package :weblocks)

(export '(weblocks-acceptor))

(defclass weblocks-acceptor (#.(if (find-class 'easy-acceptor nil)
                                 'easy-acceptor
                                 'acceptor))
  ((session-cookie-name :type string :accessor session-cookie-name
                        :initarg :session-cookie-name
                        :initform (format nil "weblocks-~(~A~)" (gensym)))))

(defmethod initialize-instance :after ((inst weblocks-acceptor) &rest initargs)
  "Set the session secret to prevent a Hunchentoot warning emitted upon
  starting the acceptor."
  (unless (boundp 'hunchentoot:*session-secret*)
    (hunchentoot:reset-session-secret)))

(defmethod process-connection ((acceptor weblocks-acceptor) socket)
  ;; CCL uses predictable random states for new threads
  #+ccl(setf *random-state* (make-random-state t))
  (let ((*print-readably* nil))
    (call-next-method)))

(when (function hunchentoot:acceptor-status-message)
  (defmethod acceptor-status-message :around ((acceptor weblocks-acceptor) (http-status-code (eql 500)) &key &allow-other-keys)
    nil))
