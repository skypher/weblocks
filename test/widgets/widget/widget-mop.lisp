
(in-package :weblocks-test)

;;; test direct-slot-definition-class for widget
(deftest direct-slot-definition-class-widget-1
    (class-name (direct-slot-definition-class (find-class 'widget)))
  weblocks::widget-direct-slot-definition)

;;; test effective-slot-definition-class for widget
(deftest effective-slot-definition-class-widget-1
    (class-name (effective-slot-definition-class (find-class 'widget)))
  weblocks::widget-effective-slot-definition)
