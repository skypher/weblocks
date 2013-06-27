(in-package :weblocks-util)

; i18n utils

(wexport '(*translation-function* translate default-translation-function)
         '(t util))

(defvar *translation-function* 'default-translation-function)

(defun translate (string &rest args)
  (apply *translation-function* (list* string args)))
