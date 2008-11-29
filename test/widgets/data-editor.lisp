
(in-package #:weblocks-test)

(deftestsuite widgets/data-editor-suite (weblocks-suite)
  ())

(addtest reinterpret-dataform-in-diwd
  (let* ((obj (make-instance 'employee))
	 (wij (make-instance 'dataform :data obj)))
    (dolist (d (list (dataform-data wij)
		     (dataedit-item-widget-data wij)
		     (progn (change-class wij 'data-editor)
			    (dataedit-item-widget-data wij))))
      (ensure-same d obj))))
