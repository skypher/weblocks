(defpackage #:weblocks-test/widgets/render-methods
  (:use #:cl
        #:rove)
  (:import-from #:weblocks/widget
                #:get-html-tag
                #:render
                #:defwidget)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:weblocks-test/utils
                #:is-html))
(in-package weblocks-test/widgets/render-methods)


(defwidget simple-widget ()
  ())


(defmethod render ((widget simple-widget))
  (with-html
    (:p "Hello world")))


(defwidget widget-with-custom-tag (simple-widget)
  ())


(defmethod get-html-tag ((widget widget-with-custom-tag))
  :h1)


(deftest test-widget-rendering-as-a-div
  (let ((widget (make-instance 'simple-widget)))
    (is-html (render widget)
             "<div class=\"widget simple-widget\"><p>Hello world
</div>")))


(deftest test-widget-with-custom-tag
  (let ((widget (make-instance 'widget-with-custom-tag)))
    (is-html (render widget)
             "<h1 class=\"widget widget-with-custom-tag\"><p>Hello world</h1>")))
