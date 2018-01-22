
(in-package :weblocks)

(export '(disable-global-debugging enable-global-debugging *weblocks-global-debug*))

;; (declaim (special *current-webapp* *maintain-last-session*))

(defvar *weblocks-global-debug* nil)

(defvar *show-lisp-errors-p* nil)

(defun enable-global-debugging ()
  "Setup hooks for session maintenance and showing backtraces"
  (setf *weblocks-global-debug* t)

  ;; Drop into a lisp debugger on errors
  (setf weblocks.variables:*catch-errors-p* nil)
  ;; Previously this option was turned on here.
  ;; It renders a html page with a backtrace on error.
  ;; (setf *show-lisp-errors-p* t)
  
  ;(setf *show-lisp-backtraces-p* t)
  ;; Set session maintenance (for everyone)
  (unless *maintain-last-session*
    (setf *maintain-last-session*
          (bordeaux-threads:make-lock "*maintain-last-session*"))))

(defun disable-global-debugging ()
  "A manual method for resetting global debugging state"
  (setf *weblocks-global-debug* nil)
  (setf weblocks.variables:*catch-errors-p* t)
  (setf *show-lisp-errors-p* nil)
  ;(setf *show-lisp-backtraces-p* nil)
  (setf *maintain-last-session* nil))

;;; Further aid in debugging by reporting potential problems


