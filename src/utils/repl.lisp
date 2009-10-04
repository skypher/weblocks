
(in-package :weblocks)

(export '(sessions in-session pt))

(declaim (special *weblocks-server*))

(defun sessions ()
  (hunchentoot:session-db *weblocks-server*))

(defun in-session (&optional sid)
  "Enter a session context. If SID is supplied the session with
this id will be selected. Otherwise the first session (likely
the one started most recently) will be selected"
  (setf hunchentoot:*session*
    (if sid
      (cdr (assoc sid (sessions)))
      (cdar (sessions)))))

(defun pt ()
  "Print the current session's widget tree"
  (walk-widget-tree (root-widget)
                    (lambda (w d)
                      (loop repeat d do (format t " "))
                      (format t "~S~%" w))))
