
(in-package :weblocks)

(defvar *weblocks-server* nil)

(defun start-weblocks ()
  (if (null *weblocks-server*)
      (setf *weblocks-server* (start-server :port 8080))))

(defun stop-weblocks ()
  (if (not (null *weblocks-server*))
      (stop-server *weblocks-server*)))

(defun hala ()
  (get-output-stream-string (render-data *joe-employee* :slot-names '((first-name . fn)))))

(setf *dispatch-table*
      (cons (create-prefix-dispatcher "/test" 'hala) *dispatch-table*))
