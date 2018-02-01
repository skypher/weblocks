(defpackage #:weblocks-test/widgets/macroexpansion
  (:use #:cl
        #:rove)
  (:import-from #:weblocks/widget
                #:widget
                #:defwidget)
  (:import-from #:weblocks/widgets/mop
                #:widget-class))
(in-package weblocks-test/widgets/macroexpansion)


(deftest test-defwidget-adds-metaclass
  (ok (equal (macroexpand-1
              '(defwidget foo (bar)
                ((slot1 :initarg :slot1) (slot2 :initform nil))))
             '(progn
               (defclass foo (bar)
                 ((slot1 :initarg :slot1)
                  (slot2 :initform nil))
                 (:metaclass widget-class))))
      "Defwidget should add a metaclass to a class definition, and if parent class was given, class 'widget will not be added as a parent."))


(deftest test-if-no-parent-class-given-widget-is-added
  (ok (equal (macroexpand-1
              '(defwidget foo ()
                ((slot1 :initarg :slot1) (slot2 :initform nil))))
             '(progn
               (defclass foo (widget)
                 ((slot1 :initarg :slot1)
                  (slot2 :initform nil))
                 (:metaclass widget-class))))
      "If no parent class was given, class widget will be added as a parent"))



