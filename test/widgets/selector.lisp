
(in-package :weblocks-test)

(deftestsuite widgets/static-selector-suite (weblocks-suite) nil)

(addtest static-selector-select-pane-1
  (let* ((widget (make-instance 'widget))
         (selector (make-instance 'static-selector :panes `(("one" . ,widget)))))
    (setf (selector-base-uri selector) "/")
    (let ((tokens (make-instance 'uri-tokens :tokens '("one"))))
      (ensure-same
        (get-widget-for-tokens selector tokens)
        widget))))

(addtest static-selector-select-pane-2
  (let* ((widget (make-instance 'widget))
         (selector (make-instance 'static-selector :panes `(("one" . ,widget)))))
    (setf (selector-base-uri selector) "/")
    (let ((tokens (make-instance 'uri-tokens :tokens '("two"))))
      (ensure-same
        (get-widget-for-tokens selector tokens)
        nil))))

(addtest static-selector-update-children-base-uri
  (let* ((widget (make-instance 'widget))
         (selector (make-instance 'static-selector :panes `(("one" . ,widget)))))
    (let ((*uri-tokens* (make-instance 'uri-tokens :tokens '("one"))))
      (update-children selector)
      (ensure-same (selector-base-uri selector) "/"))))

(addtest static-selector-update-children-404
  (let* ((widget (make-instance 'widget))
         (selector (make-instance 'static-selector :panes `(("one" . ,widget)))))
    (let ((*uri-tokens* (make-instance 'uri-tokens :tokens '("two"))))
      (ensure-condition 'http-not-found
        (update-children selector)))))

(addtest static-selector-update-dependents
  (let* ((widget (make-instance 'widget))
         (selector (make-instance 'static-selector :panes `(("one" . ,widget)))))
    (let ((*uri-tokens* (make-instance 'uri-tokens :tokens '("one"))))
      (update-children selector)
      (ensure-same (widget-children selector) (list widget)))))

(addtest static-selector-current-pane
  (let* ((widget (make-instance 'widget))
         (selector (make-instance 'static-selector :panes `(("one" . ,widget)))))
    (let ((*uri-tokens* (make-instance 'uri-tokens :tokens '("one"))))
      (update-children selector)
      (ensure-same (static-selector-current-pane selector) "one"))))

