(in-package :weblocks)

(export '(widget-translate 
          widget-dynamic-translate 
          widget-translation-table))

(defgeneric widget-translation-table (obj &rest args)
  (:documentation "Widget translation table is an alist with all strings used in widget which should be translated.
                   The idea of this table is to hold every string so we just translate this table to translate all widget strings.
                   Some strings like messages or questions to user can be hard to locate for translation but 
                   when we put all widget translation strings in one place there is no need in location it will be easy to translate them. 
                   This function should return translation strings from widget and all its children.
                   To use string from translation table see 'widget-translate'. 
                   To use some dynamic string which you want to put into translation table use 'widget-dynamic-translate'")
                   (:method-combination append)
                   (:method append (obj &rest args)
                    (loop for (key . value) in (get 'dynamic-translation-table obj)
                          collect (cons key (if (functionp value)
                                              (funcall value)
                                              value)))))

(defmethod widget-translate (obj key &key items-count)
  "Returns string associated with key from widget translation table"
  (when items-count 
    (setf key (concatenate-keywords key :- (number-form-type-with-locale (current-locale) items-count))))
  (let ((translation-record (assoc key (widget-translation-table obj))))
    (unless translation-record 
      (warn "Translation missing for key ~A" key))
    (if (functionp (cdr translation-record))
      (funcall (cdr translation-record))
      (cdr translation-record))))

(defmethod widget-dynamic-translate-impl (obj key value)
  "Updates widget dynamic translation table. When 'key' is already exists in table, replaces it."
  (let* ((table (get 'dynamic-translation-table obj))
         (found-element (assoc key table)))

    (when found-element
      (setf table (remove found-element table)))

    (push (cons key value) table)

    (setf (get 'dynamic-translation-table obj) table)

    (if (functionp value)
      (funcall value)
      value)))

(defmacro widget-dynamic-translate (obj key value)
  `(widget-dynamic-translate-impl ,obj ,key (f0 ,value)))

