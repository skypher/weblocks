
(in-package :weblocks-test)

;;; test data-print-object for symbols
(deftest data-print-object-symbols-1
    (data-print-object nil 'name 'symbol 'test)
  "Test")

(deftest data-print-object-symbols-2
    (data-print-object nil 'name 'symbol :test)
  "Test")

(deftest data-print-object-symbols-3
    (data-print-object nil 'name 'symbol nil)
  "Nil")

;;; test parse-symbol-from-request
(deftest parse-symbol-from-request-1
    (weblocks::parse-symbol-from-request "test")
  "TEST")

(deftest parse-symbol-from-request-2
    (let ((reader-case (readtable-case *readtable*)))
      (setf (readtable-case *readtable*) :preserve)
      (unwind-protect 
	   (weblocks::parse-symbol-from-request "test")
	(setf (readtable-case *readtable*) reader-case)))
  "test")

;;; test parse-slot-from-request for symbols
(deftest parse-slot-from-request-symbols-1
    (let ((sym (cadr (multiple-value-list (parse-slot-from-request nil 'test 'symbol "t")))))
      (unwind-protect
	   (values (symbol-name sym) (package-name (symbol-package sym)))
	(unintern sym)))
  "T" "COMMON-LISP")

(deftest parse-slot-from-request-symbols-2
    (let ((sym (cadr (multiple-value-list (parse-slot-from-request nil 'test 'symbol
								   "veryunlikelytoexist")))))
      (unwind-protect
	   (values (symbol-name sym) (package-name (symbol-package sym)))
	(unintern sym)))
  "VERYUNLIKELYTOEXIST" "WEBLOCKS-TEST")

(deftest parse-slot-from-request-symbols-3
    (parse-slot-from-request nil 'test 'symbol "test")
  t test)

