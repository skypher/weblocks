
(in-package :weblocks-test)

(defclass text-employee ()
  ((text-slot :initform nil :type text :initarg :text-slot)))

;;; create instances for introspection testing
(defparameter *text-joe* (make-instance 'text-employee :text-slot "Joe"))
(defparameter *text-bob* (make-instance 'text-employee :text-slot "Bob"))

;;; test property access
(deftest textarea-rows-1
    (textarea-rows t t t)
  5)

(deftest textarea-cols-1
    (textarea-cols t t t)
  20)

(deftest text-data-output-mode-1
    (text-data-output-mode *text-joe* 'text-slot)
  :cutoff)

(deftest text-data-cutoff-threshold-1
    (text-data-cutoff-threshold *text-joe* 'text-slot)
  15)

(deftest max-raw-slot-input-length-text-1
    (max-raw-slot-input-length *text-joe* 'text-slot 'text)
  200)

;; test render-form-value
(deftest-html render-form-value-text-1
    (render-form-value *text-joe* 'text-slot 'text "Joe")
  (:textarea :name "text-slot" :rows 5 :cols 20
	     "Joe"))

(deftest-html render-form-value-text-2
    (render-form-value *text-bob* 'text-slot 'text "Bob")
  (:textarea :name "text-slot" :rows 5 :cols 20
	     "Bob"))

;;; test form-print-object
(deftest form-print-text-1
    (form-print-object t 'text-slot 'text "123456789")
  "123456789")

(deftest form-print-text-2
    (form-print-object t 'text-slot 'text "12345678901234567890123456789")
   "12345678901234567890123456789")

;;; test render-data-value
(deftest-html render-data-value-text-1
    (render-data-value *text-bob* 'text-slot 'text "Bob")
  (:span :class "value" "Bob"))

(deftest-html render-data-value-text-2
    (render-data-value *text-bob* 'text-slot 'text "12345678901234567890123456789")
  (:span :class "value" "123456789012345" (:span :class "ellipsis" "...")))

(deftest-html render-data-value-text-3
    (let ((*text-data-output-mode* :paragraph))
      (render-data-value *text-bob* 'text-slot 'text "12345678901234567890123456789"))
  (:p :class "value text" "12345678901234567890123456789"))

(deftest-html render-data-value-text-4
    (let ((*text-data-output-mode* :paragraph))
      (render-data-value *text-bob* 'text-slot 'text
			 (concatenate 'string
				      "12" (list #\Newline #\Newline) "34")))
  (:p :class "value text" "12" (:br) (:br) "34"))

;;; test data-print-object
(deftest data-print-text-1
    (data-print-object *text-joe* 'text-slot 'text "123456789012345678901234567890")
  "123456789012345")

(deftest data-print-text-2
    (let ((*text-data-output-mode* :paragraph))
      (data-print-object *text-joe* 'text-slot 'text "123456789012345678901234567890"))
  "123456789012345678901234567890")

(deftest data-print-text-3
    (data-print-object *text-joe* 'text-slot 'text "123456789")
  "123456789")

