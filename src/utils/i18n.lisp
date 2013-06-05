(in-package :weblocks-util)

; i18n utils

(wexport '(*translation-function* translate)
         '(t util))

(defvar *translation-function* 
  (lambda (string &rest args)
    (declare (ignore args))
    string))

(defun translate (string &rest args)
  (apply *translation-function* (list* string args)))
