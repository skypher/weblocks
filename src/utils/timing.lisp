
(in-package :weblocks)

(wexport '(*enable-timings*
           *timing-report-fn*
           timing)
         '(t util))

(defvar *enable-timings* nil)

(defvar *timing-report-fn* (lambda (name real cpu)
                             (format t "time spent for ~S (real/cpu): ~F/~F~%"
                                     name real cpu)))

(defun report-timing (name real cpu)
  (funcall *timing-report-fn* name real cpu))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *timing-level* 0))
(declaim (fixnum *timing-level*))

(defmacro timing (name &body body)
  (with-gensyms (start/real start/cpu
                 end/real end/cpu
                 spent/real spent/cpu)
    `(let ((thunk (lambda () ,@body)))
       (if *enable-timings*
         (let ((,start/real (get-internal-real-time))
               (,start/cpu (get-internal-run-time)))
           (declare (optimize (speed 3)(safety 3))
                    ((integer 0) ,start/real ,start/cpu))
           (incf *timing-level*)
           (prog1
             (funcall thunk)
             (let* ((,end/real (get-internal-real-time))
                    (,end/cpu (get-internal-run-time))
                    (,spent/real (/ (- ,end/real ,start/real)
                                    internal-time-units-per-second))

                    (,spent/cpu (/ (- ,end/cpu ,start/cpu)
                                   internal-time-units-per-second)))
               (declare ((integer 0) ,end/real ,end/cpu)
                        ((rational 0) ,spent/real ,spent/cpu))
               (report-timing ,name ,spent/real ,spent/cpu)
               (decf *timing-level*))))
         (funcall thunk)))))

