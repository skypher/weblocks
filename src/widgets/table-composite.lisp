
(in-package :weblocks)
 
(export '(table-composite render-widget-body))
 
(defwidget table-composite (composite)
  ((cols :type integer :accessor cols :initarg :cols :initform 0))
  (:documentation "Renders a set of widgets in table layout."))
 
(defmethod render-widget-body ((widget table-composite) &rest args)
  (declare (ignore args))
  (let* ((widgets (composite-widgets widget))
         (num-widgets (list-length widgets))
         (cols (cols widget))
         (rows (ceiling (/ num-widgets cols))))
    (with-html
      (:table :rows rows :cols cols
        (loop for r from 0 below rows
              do (htm (:tr :class (when (= r 0)
                                    "first-row")
              (loop for c from 0 below cols
                    do (let ((child (nth (+ c (* r cols)) widgets)))
                         (htm (:td (when child (render-widget child)))))))))))))
 
#|; EXAMPLE CODE:
(make-instance 'table-composite :cols 3
  :widgets (loop for i from 1 to 10
                 collect (write-to-string i)))
|#
