(defpackage #:weblocks.session-lock
  (:use #:cl)
  (:import-from #:weblocks.session
                #:*session*)
  (:export
   #:get-lock))
(in-package weblocks.session-lock)


(defvar *session-locks* (make-hash-table :test #'eq
                                         #+sbcl :weakness #+sbcl :key
                                         #+ccl :weak #+ccl :key)
  "Per-session locks to avoid having unrelated threads
  waiting.")
#-(or sbcl ccl) (warn "No GC mechanism for *SESSION-LOCKS* on your Lisp. ~
            Expect a tiny memory leak until fixed.")


(defvar *session-lock-table-lock* (bordeaux-threads:make-lock
                                    "*session-lock-table-lock*"))


(defun get-lock ()
  (bordeaux-threads:with-lock-held (*session-lock-table-lock*)
    (unless (gethash *session* *session-locks*)
      (setf (gethash *session* *session-locks*) 
            (bordeaux-threads:make-lock
             (format nil "session lock for session ~S"
                     *session*)))))
  (gethash *session* *session-locks*))


