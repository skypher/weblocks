
(defpackage #:weblocks-test
  (:use :cl :weblocks :rt :c2mop :cl-who)
  (:export #:test-weblocks))

(in-package :weblocks-test)

(defun test-weblocks ()
  "Call this function to run all unit tests defined in
weblocks-test package."
  (do-tests))

;;; a helper macro to define html tests
(defmacro deftest-html (name form value)
  (let ((expected-result (eval
			  `(with-html-output-to-string (s)
			     ,value))))
    `(deftest ,name
	 (let ((stream-bak *weblocks-output-stream*)
	       result)
	   (setf *weblocks-output-stream* (make-string-output-stream))
	   ,form
	   (setf result (get-output-stream-string *weblocks-output-stream*))
	   (setf *weblocks-output-stream* stream-bak)
	   result)
       ,expected-result)))
