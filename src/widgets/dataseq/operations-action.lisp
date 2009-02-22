
(in-package :weblocks)

;;; This function processes actions that perform operations on dataseq
;;; items. It's in a separate file because there is an issue with
;;; CMUCL and compiling this function transformed through
;;; CL-CONT. CMUCL interpreter, however, works. We'll just add this to
;;; ASDF for all implementations except CMUCL, and for CMUCL this file
;;; will be loaded without compilation.
(defun/cc dataseq-operations-action (obj &rest args)
  (declare (ignore args))
  (dataseq-clear-selection obj)
  (loop for i in (request-parameters)
     when (string-starts-with (car i) "item-")
     do (dataseq-select-item obj (parse-integer (substring (car i) 5))))
  (loop for i in (append
		  (dataseq-item-ops obj)
		  (dataseq-common-ops obj))
        when (member (attributize-name (car i)) (request-parameters)
                     :key (compose #'attributize-name #'car)
                     :test #'string-equal)
          do (funcall (cdr i) obj (dataseq-selection obj)))
  (dataseq-clear-selection obj))

