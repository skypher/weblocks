
(in-package :weblocks-test)

(deftestsuite widgets/widget/widget-suite (weblocks-suite print-upcase-suite)
  ())

;;; test defwidget
(deftest defwidget-1
    (macroexpand-1
     '(defwidget foo (bar)
       ((slot1 :initarg :slot1) (slot2 :initform nil))))
  (progn
    (defclass foo (bar)
      ((slot1 :initarg :slot1)
      (slot2 :initform nil))
      (:metaclass widget-class))
    (defmethod per-class-dependencies append ((weblocks::obj foo))
       (declare (ignore weblocks::obj))
       (weblocks::dependencies-by-symbol (quote foo))))
  t)

(deftest defwidget-2
    (macroexpand-1
     '(defwidget foo ()
       ((slot1 :initarg :slot1) (slot2 :initform nil))))
  (progn
    (defclass foo (widget)
      ((slot1 :initarg :slot1)
       (slot2 :initform nil))
      (:metaclass widget-class))
    (defmethod per-class-dependencies append ((weblocks::obj foo))
      (declare (ignore weblocks::obj))
      (weblocks::dependencies-by-symbol (quote foo))))
  t)


;;; test widget-dependencies
(addtest widget-dependencies-1
  (ensure-same (values-list (mapcar (lambda (x) (puri:uri-path (dependency-url x)))
				    (dependencies (make-instance 'navigation))))
	       (values-list (mapcar (lambda (x) (apply #'make-versioned-regex x))
				    '(("menu" "css")
				      ("navigation" "css"))))
	       :test (lambda (x y) (cl-ppcre:scan y x))))

(addtest widget-dependencies-2
  (ensure-same
   (values-list (remove-import-urls (mapcar (lambda (x) (puri:uri-path (dependency-url x)))
					    (dependencies (make-instance 'gridedit
									 :data-class 'employee)))))
   ;; note, pagination and dataform are there because for gridedit and
   ;; datagrid widget-dependencies is specialized
   (values-list (mapcar (lambda (x) (apply #'make-versioned-regex x))
			'(("dataseq" "css")
			  ("datagrid" "js")
			  ("datagrid" "css")
			  ("pagination" "css")
			  ("dataform" "css"))))
   :test (lambda (x y) (cl-ppcre:scan y x))))

(deftest widget-dependencies-3
    (with-request :get nil
      (dependencies 'test))
  nil)

;;; test render-widget-body
(deftest-html render-widget-body-1
    (render-widget-body (lambda (&rest args)
			  (with-html (:p "blah"))))
  (:p "blah"))

(deftest-html render-widget-body-2
    (render-widget-body "test")
  (:p "test"))

(deftest-html render-widget-body-3
    (render-widget-body "test" :id "foo" :class "bar")
  (:p :id "foo" :class "bar" "test"))

;; helper function
(defun dummy-symbol-function (&rest args)
  (with-html (:p "test")))

(deftest-html render-widget-body-4
    (render-widget-body 'dummy-symbol-function)
  (:p "test"))

;;; test dom-classes
(deftest dom-classes-1
    (dom-classes #'identity)
  "widget function")

(deftest dom-classes-2
    (dom-classes 'identity)
  "widget function identity")

(deftest dom-classes-3
    (dom-classes "test")
  "widget string")

(deftest dom-classes-4
    (with-request :get nil
      (dom-classes (make-instance 'gridedit
					 :data-class 'employee)))
  "widget dataseq datagrid dataedit-mixin gridedit")

;;; test with-widget-header
(deftest-html with-widget-header-1
    (with-request :get nil
      (with-widget-header (make-instance 'dataform :data *joe*)
	(lambda (obj &rest args)
	  (with-html (:p "test")))
	:widget-prefix-fn (lambda (&rest args) (with-html (:p "hello")))
	:widget-suffix-fn (lambda (&rest args) (with-html (:p "world")))))
  (:div :class "widget data-editor dataform" :id "id-123"
	(:p "hello")
	(:p "test")
	(:p "world")))

;;; test widget-name specialization for widgets
(deftest widget-name-1
    (widget-name #'identity)
  nil)

(deftest widget-name-2
    (widget-name "identity")
  nil)

(deftest widget-name-3
    (widget-name 'identity)
  identity)

;;; test ensure-widget-methods

(defgeneric ewm-g0 ())
(defgeneric ewm-g1 (a &optional b))
(defgeneric ewm-g2 (a b &rest c))
(defgeneric ewm-g5 (a b c d e))

(defun widget-class-count ()
  (length weblocks::*widget-classes*))

(addtest ensure-widget-methods-0
  (remove-all-methods #'ewm-g0)
  (ensure-widget-methods #'ewm-g0 '() (constantly 42))
  (ensure-same (generic-function-methods #'ewm-g0) '()))

(addtest ensure-widget-methods-1
  (remove-all-methods #'ewm-g1)
  (ensure-widget-methods #'ewm-g1 0 (lambda (n &optional m)
				      (declare (ignore n m))
				      42))
  (ensure-same (ewm-g1 (make-instance 'composite)) 42)
  (ensure-same (length (generic-function-methods #'ewm-g1))
	       (widget-class-count)))

(addtest ensure-widget-methods-2
  (remove-all-methods #'ewm-g2)
  (ensure-widget-methods #'ewm-g2 1 (lambda (a b &rest c)
				      (declare (ignore a b c))
				      84))
  (ensure-same (ewm-g2 t (make-instance 'composite)) 84)
  (ensure-same (length (generic-function-methods #'ewm-g2))
	       (widget-class-count)))

(addtest ensure-widget-methods-5
  (remove-all-methods #'ewm-g5)
  (ensure-widget-methods #'ewm-g5 '(3 1)
			 (lambda (a b c d e)
			   (list b a c d e)))
  (ensure-same (ewm-g5 1 'two 3 'four 5)
	       '(two 1 3 four 5))
  (ensure-same (length (generic-function-methods #'ewm-g5))
	       (expt (widget-class-count) 2)))

;;; widget-designator typechecking
(addtest nil-is-not-valid
  (ensure-same (typep nil 'weblocks::widget-designator)
	       (values nil t))
  (ensure-null (weblocks::widget-designator-p nil)))

(addtest widget-designator-export-status-same
  (ensure-same (symbol-status 'weblocks::widget-designator)
	       (symbol-status 'weblocks::widget-designator-p)))

(addtest widget-designator-type-not-extensible
  (ensure-null
   (typep #'weblocks::widget-designator-p 'generic-function)))

;;; test composite-widgets specialization for widgets
(deftest composite-widgets-1
    (composite-widgets #'identity)
  nil)

(deftest composite-widgets-2
    (composite-widgets "identity")
  nil)

(deftest composite-widgets-3
    (composite-widgets 'identity)
  nil)

(deftest composite-widgets-4
    (with-request :get nil
      (let ((w (make-instance 'composite)))
	(setf (composite-widgets w) 1)
	(composite-widgets w)))
  (1))

(deftest composite-widgets-5
    (with-request :get nil
      (let ((w (make-instance 'composite)))
	(setf (composite-widgets w) nil)
	(composite-widgets w)))
  nil)

(deftest composite-widgets-6
    (with-request :get nil
      (let ((w (make-instance 'composite)))
	(setf (composite-widgets w) (list 1))
	(composite-widgets w)))
  (1))

;;; render function as a widget
(deftest-html render-function-1
    (with-request :get nil
      (render-widget (lambda (&rest args)
		       (with-html (:p "blah")))))
  (:div :class "widget function"
	(:p "blah")))

;;; render some other widget without a name
(deftest-html render-widget-1
    (with-request :get nil
      (render-widget (make-instance 'dataform :data *joe*)))
  (:div :class "widget data-editor dataform" :id "id-123"
	#.(data-header-template
	   "abc123"
	   '((:li :class "name" (:span :class "label text" "Name:&nbsp;") (:span :class "value" "Joe"))
	     (:li :class "manager" (:span :class "label text" "Manager:&nbsp;")
	      (:span :class "value" "Jim"))))))

;;; render some widget with a name
(deftest-html render-widget-2
    (with-request :get nil
      (render-widget (make-instance 'dataform :data *joe* :name "Test Widget")))
  (:div :class "widget data-editor dataform" :id "test-widget"
	#.(data-header-template
	   "abc123"
	   '((:li :class "name" (:span :class "label text" "Name:&nbsp;") (:span :class "value" "Joe"))
	     (:li :class "manager" (:span :class "label text" "Manager:&nbsp;")
	      (:span :class "value" "Jim"))))))

(deftest-html render-widget-3
    (with-request :get nil
      (render-widget (make-instance 'dataform :data *joe*) :inlinep t))
  #.(data-header-template "abc123"
     '((:li :class "name" (:span :class "label text" "Name:&nbsp;") (:span :class "value" "Joe"))
       (:li :class "manager" (:span :class "label text" "Manager:&nbsp;") (:span :class "value" "Jim")))))

(deftest render-widget-4
    (ensure-same
     (let ((*weblocks-output-stream* (make-string-output-stream)))
       (declare (special *weblocks-output-stream*))
       (with-request :get nil
	 (render-widget (make-instance 'dataform :data *joe*))
	 (format nil "~A" (car (mapcar #'dependency-url weblocks::*page-dependencies*)))))
     (make-versioned-regex "dataform-import" "css")
     :test (lambda (x y) (cl-ppcre:scan y x))))


;;; mark-dirty
(deftest mark-dirty-1
    (multiple-value-bind (res errors)
	(ignore-errors (mark-dirty (lambda () nil)))
      (values res (null errors)))
  nil nil)

(deftest mark-dirty-2
    (with-request :get nil
      (progv '(*weblocks-output-stream*) (list (make-string-output-stream))
	(let ((weblocks::*dirty-widgets* nil)
	      (w (make-instance 'composite :name "test")))
	  (declare (special weblocks::*dirty-widgets*))
	  (render-widget w)
	  (mark-dirty w)
	  (widget-name (car weblocks::*dirty-widgets*)))))
  "test")

(addtest mark-dirty-both-propagate-and-putp-supplied
  (ensure-error (mark-dirty (make-instance 'widget) :propagate t :putp t)))


;;; widget-dirty-p
(deftest widget-dirty-p-1
    (let ((weblocks::*dirty-widgets* nil)
	  (w (make-instance 'composite :name "test")))
      (declare (special weblocks::*dirty-widgets*))
      (widget-dirty-p w))
  nil)

(deftest widget-dirty-p-2
    (with-request :get nil
      (progv '(*weblocks-output-stream*) (list (make-string-output-stream))
	(let ((weblocks::*dirty-widgets* nil)
	      (w (make-instance 'composite :name "test")))
	  (declare (special weblocks::*dirty-widgets*))
	  (render-widget w)
	  (mark-dirty w)
	  (not (null (widget-dirty-p w))))))
  t)

(addtest setf-slot-value-using-class-marks-dirty
  (with-request :get nil
    (let ((weblocks::*dirty-widgets* nil)
          (w (make-instance 'dataform)))
      (declare (special weblocks::*dirty-widgets*))
      (setf (dataform-ui-state w) :form)
      (ensure (widget-dirty-p w)))))


;;; test get-widgets-by-type

;; TODO


;;; test get-widgets-by-id

;; TODO


;;; test customized widget printing
(deftest widget-printing-1
    (progv '(*package*) (list (find-package :weblocks-test))
      (format nil "~s" (make-instance 'weblocks::navigation :dom-id nil)))
  "#<NAVIGATION NIL>")

(deftest widget-printing-2
    (progv '(*package*) (list (find-package :weblocks-test))
      (format nil "~s" (make-instance 'weblocks::dataform :name 'users)))
  "#<DATAFORM USERS>")

;; navigation is no longer a special case which DOES NOT autogenerate ids
(deftest widget-printing-3
    (with-request :get nil
      (progv '(*package*) (list (find-package :weblocks-test))
	(format nil "~s" (make-instance 'weblocks::navigation))))
  "#<NAVIGATION \"id-123\">")

(deftest widget-printing-4
    (with-request :get nil
      (progv '(*package*) (list (find-package :weblocks-test))
	(format nil "~s" (make-instance 'weblocks::navigation :dom-id "id-234"))))
  "#<NAVIGATION \"id-234\">")


;;; widget-parents
(addtest widget-parents.simple
  (ensure-same (widget-parents (make-instance 'widget)) nil))

(addtest widget-parents-2
  (let* ((w1 (make-instance 'widget))
         (w2 (make-instance 'widget :parent w1))
         (w3 (make-instance 'widget :parent w2)))
  (ensure-same (widget-parents w3) (list w2 w1))))


;;; widgets-roots
(addtest widgets-roots.simple
  (let ((w (make-instance 'widget)))
    (ensure-same (widgets-roots (list w)) (list w))))

(addtest widgets-roots.straight-parent-chain-1
  (let* ((w1 (make-instance 'widget :dom-id "w1"))
         (w2 (make-instance 'widget :parent w1 :dom-id "w2"))
         (w3 (make-instance 'widget :parent w2 :dom-id "w3")))
    (ensure-same (widgets-roots (list w3)) (list w3))))

(addtest widgets-roots.straight-parent-chain-2
  (let* ((w1 (make-instance 'widget :dom-id "w1"))
         (w2 (make-instance 'widget :parent w1 :dom-id "w2"))
         (w3 (make-instance 'widget :parent w2 :dom-id "w3")))
    (ensure-same (widgets-roots (list w3 w2)) (list w2))))

(addtest widgets-roots.straight-parent-chain-3
  (let* ((w1 (make-instance 'widget :dom-id "w1"))
         (w2 (make-instance 'widget :parent w1 :dom-id "w2"))
         (w3 (make-instance 'widget :parent w2 :dom-id "w3")))
    (ensure-same (widgets-roots (list w3 w2 w1)) (list w1))))

(addtest widgets-roots.common-root
  (let* ((w1 (make-instance 'widget :dom-id "w1"))
         (w2 (make-instance 'widget :parent w1 :dom-id "w2"))
         (w3 (make-instance 'widget :parent w1 :dom-id "w3")))
    (ensure-same (widgets-roots (list w3 w2)) (list w2 w3))))

(addtest widgets-roots.common-root-with-root
  (let* ((w1 (make-instance 'widget :dom-id "w1"))
         (w2 (make-instance 'widget :parent w1 :dom-id "w2"))
         (w3 (make-instance 'widget :parent w1 :dom-id "w3")))
    (ensure-same (widgets-roots (list w3 w2 w1)) (list w1))))

(addtest widgets-roots.multiple-roots
  (let* ((w1 (make-instance 'widget :dom-id "w1"))
         (w2 (make-instance 'widget :parent w1 :dom-id "w2"))
         (w3 (make-instance 'widget :parent w1 :dom-id "w3"))
         (w4 (make-instance 'widget :parent w3 :dom-id "w4")))
    (ensure-same (widgets-roots (list w4)) (list w4))
    (ensure-same (widgets-roots (list w3)) (list w3))
    (ensure-same (widgets-roots (list w4 w2)) (list w2 w4))
    (ensure-same (widgets-roots (list w4 w2 w1)) (list w1))))


;;; slot-equal
(defclass slot-equal-test-class nil
  ((s1 :initarg :s1)
   (s2 :initarg :s2)))

(addtest slot-equal-both-unbound
  (ensure (slot-equal (make-instance 'slot-equal-test-class)
                      (make-instance 'slot-equal-test-class))))

(addtest slot-equal-one-unbound
  (ensure-null
    (slot-equal (make-instance 'slot-equal-test-class :s1 t)
                (make-instance 'slot-equal-test-class))))

(addtest slot-equal-unbound-equal-mix
  (ensure (slot-equal (make-instance 'slot-equal-test-class :s1 t)
                      (make-instance 'slot-equal-test-class :s1 t))))

(addtest slot-equal-all-bound
  (ensure (slot-equal (make-instance 'slot-equal-test-class :s1 t :s2 nil)
                      (make-instance 'slot-equal-test-class :s1 t :s2 nil))))

(addtest slot-equal-custom-test
  (let ((test (lambda (x y) (and (integerp x) (= x y)))))
    (ensure-null
      (slot-equal (make-instance 'slot-equal-test-class :s1 t :s2 nil)
                  (make-instance 'slot-equal-test-class :s1 t :s2 nil)
                  :test test))
    (ensure
      (slot-equal (make-instance 'slot-equal-test-class :s1 5 :s2 1)
                  (make-instance 'slot-equal-test-class :s1 5 :s2 1)
                  :test test))
    (ensure
      (slot-equal (make-instance 'slot-equal-test-class :s1 5)
                  (make-instance 'slot-equal-test-class :s1 5)
                  :test test))))

(addtest widget-tree-equal.root-only
  (let ((w1 (make-instance 'widget :dom-id "w"))
        (w2 (make-instance 'widget :dom-id "w")))
  (ensure (widget-tree-equal w1 w2))))

(addtest widget-tree-equal.unrelated
  (let ((w1 (make-instance 'widget :dom-id "w1"))
        (w2 (make-instance 'widget :dom-id "w2")))
  (ensure-null (widget-tree-equal w1 w2))))

(addtest widget-tree-equal.straight
  (let ((t1-w1 (make-instance 'widget :dom-id "w1"))
        (t1-w2 (make-instance 'widget :dom-id "w2"))
        (t2-w1 (make-instance 'widget :dom-id "w1"))
        (t2-w2 (make-instance 'widget :dom-id "w2")))
    (setf (widget-children t1-w1) (list t1-w2))
    (setf (widget-children t2-w1) (list t2-w2))
  (ensure (widget-tree-equal t1-w1 t2-w1))))

(addtest widget-tree-equal.hierarchy
  (let ((t1-w1 (make-instance 'widget :dom-id "w1"))
        (t1-w1-a (make-instance 'widget :dom-id "w1a"))
        (t1-w2 (make-instance 'widget :dom-id "w2"))
        (t1-w2-a (make-instance 'widget :dom-id "w2a"))
        (t1-w2-b (make-instance 'widget :dom-id "w2b"))
        (t2-w1 (make-instance 'widget :dom-id "w1"))
        (t2-w1-a (make-instance 'widget :dom-id "w1a"))
        (t2-w2 (make-instance 'widget :dom-id "w2"))
        (t2-w2-a (make-instance 'widget :dom-id "w2a"))
        (t2-w2-b (make-instance 'widget :dom-id "w2b")))
    (setf (widget-children t1-w1) (list t1-w1-a t1-w2))
    (setf (widget-children t1-w2) (list t1-w2-a t1-w2-b))
    (setf (widget-children t2-w1) (list t2-w1-a t2-w2))
    (setf (widget-children t2-w2) (list t2-w2-a t2-w2-b))
  (ensure (widget-tree-equal t1-w1 t2-w1))))

#+(or)
(addtest copy-widget-tree.simple
  (let ((*lift-equality-test* #'widget-tree-equal))
    (let* ((w1 (make-instance 'widget :dom-id "w1")))
      (ensure-same (copy-widget-tree w4) w4))))

#+(or)
(addtest copy-widget-tree.straight
  (let ((*lift-equality-test* #'widget-tree-equal))
    (let* ((w1 (make-instance 'widget :dom-id "w1"))
           (w2 (make-instance 'widget :parent w1 :dom-id "w2")))
      (ensure-same (copy-widget-tree w1) 5))))

