
(in-package :weblocks)

(export '(weblocks-acceptor))

(defclass weblocks-acceptor (acceptor)
  ((session-cookie-name :type string :accessor session-cookie-name
                        :initarg :session-cookie-name
                        :initform (format nil "weblocks-~(~A~)" (gensym)))))

(defmethod process-connection ((acceptor weblocks-acceptor) socket)
  (let ((*print-readably* nil))
    (call-next-method)))

