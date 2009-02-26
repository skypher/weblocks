
(in-package :weblocks)

(export '(weblocks-acceptor))

(defclass weblocks-acceptor (acceptor)
  ((session-cookie-name :type string :accessor session-cookie-name
                        :initarg :session-cookie-name
                        :initform (format nil "weblocks-~(~A~)" (gensym)))))

(defmethod initialize-instance :after ((inst weblocks-acceptor) &rest initargs)
  "Set the session secret to prevent a Hunchentoot warning emitted upon
  starting the acceptor."
  (unless (boundp 'hunchentoot::*session-secret*)
    (hunchentoot:reset-session-secret)))

(defmethod process-connection ((acceptor weblocks-acceptor) socket)
  (let ((*print-readably* nil))
    (call-next-method)))

