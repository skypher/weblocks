
(in-package :weblocks-test)

;;; test slot-value-required-p
(deftest slot-value-required-p-1
    (weblocks::slot-value-required-p 'employee (get-slot-definition 'employee 'age))
  t)

(deftest slot-value-required-p-2
    (weblocks::slot-value-required-p 'education-history
				     (get-slot-definition 'education-history 'graduation-year))
  nil)

(deftest slot-value-required-p-3
    (weblocks::slot-value-required-p 'employee (get-slot-definition 'employee 'name))
  t)

(deftest slot-value-required-p-4
    (weblocks::slot-value-required-p 'employee 'age)
  t)

;;; slot-from-request-valid-p
(deftest slot-from-request-valid-p-1
    (weblocks::slot-from-request-valid-p *joe* 'age 'integer nil)
  nil)

(deftest slot-from-request-valid-p-2
    (weblocks::slot-from-request-valid-p *joe* 'age 'integer 20)
  t)

(deftest slot-from-request-valid-p-3
    (weblocks::slot-from-request-valid-p *joe* 'age 'integer "Bob")
  nil)


;;; test invalid-input-error-message
(deftest invalid-input-error-message-1
    (invalid-input-error-message nil "some-slot" "Some Human Slot" 'integer "t")
  "Some Human Slot must be an integer.")

(deftest invalid-input-error-message-2
    (invalid-input-error-message nil "some-slot" "Some Human Slot" 'foo1 "t")
  "Some Human Slot must be an integer.")

(deftest invalid-input-error-message-3
    (invalid-input-error-message nil "some-slot" "Some Human Slot" 'dummy-exported-type "t")
  "Some Human Slot must be a dummy exported type.")

;;; test max-raw-slot-input-length
(deftest max-raw-slot-input-length-1
    (max-raw-slot-input-length *joe* 'name 'string)
  40)

(deftest max-raw-slot-input-length-2
    (max-raw-slot-input-length *joe* 'age 'string)
  3)

