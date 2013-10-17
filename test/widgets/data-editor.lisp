
(in-package #:weblocks-test)

(deftestsuite widgets/data-editor-suite (weblocks-suite)
  ())

(addtest reinterpret-dataform-in-data-accessor
  (let* ((obj (make-instance 'employee))
	 (wij (make-instance 'dataform :data obj)))
    (dolist (d (list (dataform-data wij)
		     (progn (change-class wij 'data-editor)
			    (dataform-data wij))))
      (ensure-same d obj))))

(addtest dataform-i18n-1 
  (ensure-alist-has-keys 
    (widget-translation-table 
      (make-instance 'dataform :data *joe*))
    (list :close-link-title :modify-link-title)))
