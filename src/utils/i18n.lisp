(in-package :weblocks)

; i18n utils

;; (wexport '(*translation-function* translate default-translation-function)
;;          '(t util))

(defvar *translation-function* 'default-translation-function)

(defun translate (string &rest scope)
  "Translates given string by calling *translation-function* 
   with string and 'scope'.
   'scope' is a set of function key arguments which can be 
   :accusative-form-p 
   :genitive-form-p 
   :items-count 
   :preceding-count"
  (apply *translation-function* (list* string scope)))
