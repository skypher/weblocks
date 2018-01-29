(defpackage #:weblocks/utils/i18n
  (:use #:cl)
  (:import-from #:weblocks/linguistic/grammar
                #:default-translation-function)
  
  (:export #:*translation-function*
           #:translate
           #:default-translation-function))
(in-package weblocks/utils/i18n)


;; TODO: rethink this module, unexport variable and make
;;       it more pluggable, probably by making translate a
;;       a generic function. Or may be to remove i18n at all?


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
