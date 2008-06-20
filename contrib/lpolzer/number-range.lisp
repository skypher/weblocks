(in-package :weblocks)

(export '(number-range-presentation number-range-parser parse-float))

(defclass number-range-presentation (text-presentation input-presentation)
  () (:documentation "A presentation for number ranges."))

(defmethod render-view-field-value (value (presentation number-range-presentation)
				    (field form-view-field) (view form-view)
				    widget obj &rest args
                                    &key intermediate-values &allow-other-keys)
  (declare (ignore args))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (let* ((value (if intermediate-value-p intermediate-value value))
           (value (if (consp value) value (cons value value))))
      (render-input-field "text" (view-field-slot-name field)
                          (format nil "~D-~D" (car value) (cdr value))))))

(defclass number-range-parser (text-parser)
  ())

(defmethod parse-view-field-value ((parser number-range-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (flet ((maybe-float (str) (find #\. str)))
    (let* ((value* (cl-ppcre:split "-" value))
           (min (car value*))
           (max (car (metatilities:ensure-list (cdr value*))))
           (min (if (maybe-float min) (parse-float min) (parse-integer min)))
           (max (if (maybe-float max) (parse-float max) (parse-integer max)))
           (present-p (and min max))
           (valid-p (and present-p (cl-ppcre:scan "[0-9\.]+-[0-9\.]+|[0-9\.]+" value)
                         (<= min max))))
        (values valid-p present-p (cons min max)))))

(defun parse-float (string &key (start 0) end (radix 10) junk-allowed)
  "Converts a substring of STRING, as delimited by START and END, to a 
   floating point number, if possible. START and END default to the 
   beginning and end of the string. RADIX must be between 2 and 36. 
   A floating point number will be returned if the string consists of an
   optional string of spaces and an optional sign, followed by a string
   of digits optionally containing a decimal point, and an optional e or
   E followed by an optionally signed integer. The use of e/E to indicate
   an exponent only works for RADIX = 10. Returns the floating point
   number, if any, and the index for the first character after the number."

  ;; END defaults to the end of the string
  ;; We don't accomplish this by sticking (end (length string)) in the 
  ;; lambda list because I've encountered too many implementations that 
  ;; don't handle such properly. Also, this will work ok if somebody calls
  ;; the function with :end nil.
  (setq end (or end (length string))) 

  ;; Skip over whitespace. If there's nothing but whitespace, signal an error.
  (let ((index (or (cl:position-if-not #'whitespacep string :start start :end end)
                   (if junk-allowed
                       (return-from parse-float (values nil end))
                     (error "No non-whitespace characters in number."))))
        (minusp nil) (decimalp nil) (found-digit nil) 
        (before-decimal 0) (after-decimal 0) (decimal-counter 0)
        (exponent 0)
        (result 0))
    (declare (fixnum index))

    ;; Take care of optional sign.
    (let ((char (char string index)))
      (cond ((char= char #\-)
             (setq minusp t)
             (incf index))
            ((char= char #\+)
             (incf index))))

    (loop
     (when (= index end) (return nil))
     (let* ((char (char string index))
            (weight (digit-char-p char radix)))
       (cond ((and weight (not decimalp))
              ;; A digit before the decimal point
              (setq before-decimal (+ weight (* before-decimal radix))
                    found-digit t))
             ((and weight decimalp)
              ;; A digit after the decimal point
              (setq after-decimal (+ weight (* after-decimal radix))
                    found-digit t)
              (incf decimal-counter))
             ((and (char= char #\.) (not decimalp))
              ;; The decimal point
              (setq decimalp t))
             ((and (char-equal char #\e) (= radix 10))
              ;; E is for exponent
              (multiple-value-bind (num idx) 
                  (parse-integer string :start (1+ index) :end end
                                 :radix radix :junk-allowed junk-allowed)
                (setq exponent (or num 0)
                      index idx)
                (when (= index end) (return nil))))
             (junk-allowed (return nil))
             ((whitespacep char)
              (when (position-if-not #'whitespacep string
                                     :start (1+ index) :end end)
                (error "There's junk in this string: ~S." string))
              (return nil))
             (t
              (error "There's junk in this string: ~S." string))))
     (incf index))

    ;; Cobble up the resulting number
    (setq result (float (* (+ before-decimal
                              (* after-decimal 
                                 (expt radix (- decimal-counter))))
                           (expt radix exponent))))

    ;; Return the result
    (values
     (if found-digit
         (if minusp (- result) result)
       (if junk-allowed
           nil
         (error "There's no digits in this string: ~S" string)))
     index)))

