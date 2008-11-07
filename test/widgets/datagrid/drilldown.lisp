
(in-package :weblocks-test)

;;; test datagrid-drilldown-style
(deftest datagrid-drilldown-style-1
    (weblocks::datagrid-drilldown-style 'some-slot)
  "drilldown some-slot")

;;; test datagrid-render-view-field-drilldown
(deftest-html datagrid-render-view-field-drilldown-1
    (with-request :get nil
      (make-action (lambda (&rest args) nil))
      (let* ((view (defview () (:type table :inherit-from '(:scaffold employee))))
	     (grid (make-instance 'datagrid :data-class 'employee
				  :on-drilldown (cons 'details
						      (lambda (&rest args)
							nil))
				  :view view))
	     (field (weblocks::make-drilldown-field grid))
	     (presentation (view-field-presentation field))
	     (value (first-name *joe*)))
	(render-view-field field view grid presentation value *joe*
			   :row-action "abc123")))
  (:td :class "drilldown details"
       (:noscript (:div (:a :href "/foo/bar?action=abc123" "Details")))))

;;; test datagrid-render-view-field-header-drilldown
(deftest-html datagrid-render-view-field-header-drilldown-1
    (with-request :get nil
      (make-action (lambda (&rest args) nil))
      (let* ((view (defview () (:type table :inherit-from '(:scaffold employee))))
	     (grid (make-instance 'datagrid :data-class 'employee
				  :on-drilldown (cons 'details
						      (lambda (&rest args)
							nil))
				  :view view))
	     (field (weblocks::make-drilldown-field grid))
	     (presentation (view-field-presentation field))
	     (value (first-name *joe*)))
	(render-view-field-header field view grid presentation value *joe*
			   :row-action "abc123")))
  (:th :class "drilldown details"))

;;; test datagrid-with-table-view-body-row
(deftest-html datagrid-with-table-view-body-row-1
    (with-request :get nil
      (let* ((view (defview () (:type table :inherit-from '(:scaffold employee))))
	     (grid (make-instance 'datagrid :data-class 'employee
				  :on-drilldown (cons 'details
						      (lambda (&rest args)
							nil))
				  :view view))
	     (field (weblocks::make-drilldown-field grid))
	     (presentation (view-field-presentation field))
	     (value (first-name *joe*)))
	(with-table-view-body-row view *joe* grid)))
  (:tr :onclick "initiateActionOnEmptySelection(\"abc123\", \"weblocks-session=1%3ATEST\");"
       :onmouseover "this.style.cursor = \"pointer\";"
       :style "cursor: expression(\"hand\");"
       (:td :class "name" (:span :class "value" "Joe"))
       (:td :class "manager" (:span :class "value" "Jim"))))

;;; test drilldown
(deftest-html datagrid-drilldown-test-1
    (let ((res))
      (with-request :get nil
	(persist-objects *default-store* (list *joe* *bob*))
	(let ((grid (make-instance 'datagrid
				   :data-class 'employee
				   :allow-drilldown-p t
				   :on-drilldown (cons 'edit (lambda (grid item)
							       (setf res (first-name item))))))
	      (*on-ajax-complete-scripts* nil))
	  (declare (special *on-ajax-complete-scripts*))
	  ;; render datagrid
	  (dataseq-update-sort-column grid)
	  (render-dataseq-body grid)
	  ;; sort by name (should be descending)
	  (do-request `((,weblocks::*action-string* . "abc126")))
	  ;; output result
	  (with-html (str res)))))
  (htm
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "name sort-asc" (:span #.(link-action-template "abc123" "Name")))
	      (:th :class "manager" (:span #.(link-action-template "abc124" "Manager")))
	      (:th :class "drilldown edit" ""))
	    '((:tr :onclick "initiateActionOnEmptySelection(\"abc125\", \"weblocks-session=1%3ATEST\");"
	           :onmouseover "this.style.cursor = \"pointer\";"
	           :style "cursor: expression(\"hand\");"
	       (:td :class "name" (:span :class "value" "Bob"))
	       (:td :class "manager" (:span :class "value" "Jim"))
	       (:td :class "drilldown edit"
		(:noscript
		 (:div
		  (:a :href "/foo/bar?action=abc125" "Edit")))))
	      (:tr :class "altern"
	           :onclick "initiateActionOnEmptySelection(\"abc126\", \"weblocks-session=1%3ATEST\");"
	           :onmouseover "this.style.cursor = \"pointer\";"
	           :style "cursor: expression(\"hand\");"
	       (:td :class "name" (:span :class "value" "Joe"))
	       (:td :class "manager" (:span :class "value" "Jim"))
	       (:td :class "drilldown edit"
		(:noscript
		 (:div
		  (:a :href "/foo/bar?action=abc126" "Edit"))))))
	    :summary "Ordered by name, ascending."))
   "Joe"))

;;; test autoset-drilled-down-item-p
(addtest autoset-drilled-down-item-p-1
  (with-request :get nil
    (persist-objects *default-store* (list *joe* *bob*))
    (let ((grid (make-instance 'datagrid
			       :data-class 'employee
			       :allow-drilldown-p t
			       :on-drilldown
			       (cons 'edit (lambda (grid item)
					     (ensure-null (dataseq-drilled-down-item grid))))))
	  (*on-ajax-complete-scripts* nil))
      (declare (special *on-ajax-complete-scripts*))
      ;; render datagrid
      (dataseq-update-sort-column grid)
      (ensure-html-output
       (progn
	 (render-dataseq-body grid)
	 ;; sort by name (should be descending)
	 (do-request `((,weblocks::*action-string* . "abc126"))))
       (htm
	(:div :class "datagrid-body"
	      #.(table-header-template
		 '((:th :class "name sort-asc" (:span #.(link-action-template "abc123" "Name")))
		   (:th :class "manager" (:span #.(link-action-template "abc124" "Manager")))
		   (:th :class "drilldown edit" ""))
		 '((:tr :onclick "initiateActionOnEmptySelection(\"abc125\", \"weblocks-session=1%3ATEST\");"
		    :onmouseover "this.style.cursor = \"pointer\";"
		    :style "cursor: expression(\"hand\");"
		    (:td :class "name" (:span :class "value" "Bob"))
		    (:td :class "manager" (:span :class "value" "Jim"))
		    (:td :class "drilldown edit"
		     (:noscript
		       (:div
			(:a :href "/foo/bar?action=abc125" "Edit")))))
		   (:tr :class "altern"
		    :onclick "initiateActionOnEmptySelection(\"abc126\", \"weblocks-session=1%3ATEST\");"
		    :onmouseover "this.style.cursor = \"pointer\";"
		    :style "cursor: expression(\"hand\");"
		    (:td :class "name" (:span :class "value" "Joe"))
		    (:td :class "manager" (:span :class "value" "Jim"))
		    (:td :class "drilldown edit"
		     (:noscript
		       (:div
			(:a :href "/foo/bar?action=abc126" "Edit"))))))
		 :summary "Ordered by name, ascending.")))))))

(deftest-html autoset-drilled-down-item-p-2
    (let ((res))
      (with-request :get nil
	(persist-objects *default-store* (list *joe* *bob*))
	(let ((grid (make-instance 'datagrid
				   :data-class 'employee
				   :allow-drilldown-p t
				   :autoset-drilled-down-item-p t
				   :on-drilldown
				   (cons 'edit (lambda (grid item)
						 (setf res
						       (first-name (dataseq-drilled-down-item grid)))))))
	      (*on-ajax-complete-scripts* nil))
	  (declare (special *on-ajax-complete-scripts*))
	  ;; render datagrid
	  (dataseq-update-sort-column grid)
	  (render-dataseq-body grid)
	  ;; sort by name (should be descending)
	  (do-request `((,weblocks::*action-string* . "abc126")))
	  ;; output result
	  (with-html (str res)))))
  (htm
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "name sort-asc" (:span #.(link-action-template "abc123" "Name")))
	      (:th :class "manager" (:span #.(link-action-template "abc124" "Manager")))
	      (:th :class "drilldown edit" ""))
	    '((:tr :onclick "initiateActionOnEmptySelection(\"abc125\", \"weblocks-session=1%3ATEST\");"
	           :onmouseover "this.style.cursor = \"pointer\";"
	           :style "cursor: expression(\"hand\");"
	       (:td :class "name" (:span :class "value" "Bob"))
	       (:td :class "manager" (:span :class "value" "Jim"))
	       (:td :class "drilldown edit"
		(:noscript
		 (:div
		  (:a :href "/foo/bar?action=abc125" "Edit")))))
	      (:tr :class "altern"
	           :onclick "initiateActionOnEmptySelection(\"abc126\", \"weblocks-session=1%3ATEST\");"
	           :onmouseover "this.style.cursor = \"pointer\";"
	           :style "cursor: expression(\"hand\");"
	       (:td :class "name" (:span :class "value" "Joe"))
	       (:td :class "manager" (:span :class "value" "Jim"))
	       (:td :class "drilldown edit"
		(:noscript
		 (:div
		  (:a :href "/foo/bar?action=abc126" "Edit"))))))
	    :summary "Ordered by name, ascending."))
   "Joe"))

