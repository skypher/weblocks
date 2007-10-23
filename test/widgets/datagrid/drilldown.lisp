
(in-package :weblocks-test)

;;; test datagrid-drilldown-style
(deftest datagrid-drilldown-style-1
    (weblocks::datagrid-drilldown-style 'some-slot)
  "drilldown some-slot")

;;; test datagrid-drilldown-slot-p
(deftest datagrid-drilldown-slot-p-1
    (with-request :get nil
      (weblocks::datagrid-drilldown-slot-p (make-instance 'datagrid
							  :data-class 'employee
							  :on-drilldown
							  (cons 'details
								(lambda (&rest args)
								  nil)))
					   'view))
  nil)

(deftest datagrid-drilldown-slot-p-2
    (with-request :get nil
      (weblocks::datagrid-drilldown-slot-p (make-instance 'datagrid :data-class 'employee)
					   'view))
  nil)

(deftest datagrid-drilldown-slot-p-3
    (with-request :get nil
      (weblocks::datagrid-drilldown-slot-p (make-instance 'datagrid
							  :data-class 'employee
							  :on-drilldown
							  (cons 'details
								(lambda (&rest args)
								  nil)))
					   'details))
  t)

;;; test render-datagrid-drilldown-body-cell
(deftest-html render-datagrid-drilldown-body-cell-1
    (with-request :get nil
      (make-action #'identity "abc123")
      (render-datagrid-drilldown-body-cell (make-instance 'datagrid
							  :data-class 'employee
							  :on-drilldown
							  (cons 'details
								(lambda (&rest args)
								  nil)))
					   nil 'view t nil
					   :row-action "abc123"))
  (:td :class "drilldown view"
       (:noscript (:div (:a :href "/foo/bar?action=abc123" "Details")))))


;;; test render-datagrid-drilldown-header-cell
(deftest-html render-datagrid-drilldown-header-cell-1
    (with-request :get nil
      (render-datagrid-drilldown-header-cell (make-instance 'datagrid
							    :data-class 'employee
							    :on-drilldown
							    (cons 'details
								  (lambda (&rest args)
								    nil)))
					     nil 'view t nil
					     :row-action "abc123"))
  (:th :class "drilldown view"))

;;; test with-datagrid-drilldown-table-body-row
(deftest-html with-datagrid-drilldown-table-body-row-1
    (with-request :get nil
      (with-datagrid-drilldown-table-body-row (make-instance 'datagrid
							     :data-class 'employee
							     :on-drilldown
							     (cons 'details
								   (lambda (&rest args)
								     nil)))
	nil (lambda (&rest args)
	      (with-html (:td "foo")))))
  (:tr :onclick "initiateActionOnEmptySelection(\"abc123\", \"weblocks-session=1%3ATEST\");"
       :onmouseover "this.style.cursor = \"pointer\";"
       :style "cursor: expression(\"hand\");"
       (:td "foo")))

;;; test drilldown
(deftest-html datagrid-drilldown-test-1
    (let ((res))
      (with-request :get nil
	(let ((grid (make-instance 'datagrid
				   :data (list *joe* *bob*)
				   :data-class 'employee
				   :allow-drilldown-p t
				   :on-drilldown (cons 'edit (lambda (grid item)
							       (setf res (first-name item))))))
	      (*on-ajax-complete-scripts* nil))
	  (declare (special *on-ajax-complete-scripts*))
	  ;; render datagrid
	  (render-datagrid-table-body grid)
	  ;; sort by name (should be descending)
	  (do-request `((,weblocks::*action-string* . "abc126")))
	  ;; output result
	  (with-html (str res)))))
  (htm
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "name sort-ascending" (:span #.(link-action-template "abc123" "Name")))
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
(deftest-html autoset-drilled-down-item-p-1
    (let ((res))
      (with-request :get nil
	(let ((grid (make-instance 'datagrid
				   :data (list *joe* *bob*)
				   :data-class 'employee
				   :allow-drilldown-p t
				   :on-drilldown
				   (cons 'edit (lambda (grid item)
						 (setf res
						       (null (datagrid-drilled-down-item grid)))))))
	      (*on-ajax-complete-scripts* nil))
	  (declare (special *on-ajax-complete-scripts*))
	  ;; render datagrid
	  (render-datagrid-table-body grid)
	  ;; sort by name (should be descending)
	  (do-request `((,weblocks::*action-string* . "abc126")))
	  ;; output result
	  (with-html (str res)))))
  (htm
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "name sort-ascending" (:span #.(link-action-template "abc123" "Name")))
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
   "T"))

(deftest-html autoset-drilled-down-item-p-2
    (let ((res))
      (with-request :get nil
	(let ((grid (make-instance 'datagrid
				   :data (list *joe* *bob*)
				   :data-class 'employee
				   :allow-drilldown-p t
				   :autoset-drilled-down-item-p t
				   :on-drilldown
				   (cons 'edit (lambda (grid item)
						 (setf res
						       (first-name (datagrid-drilled-down-item grid)))))))
	      (*on-ajax-complete-scripts* nil))
	  (declare (special *on-ajax-complete-scripts*))
	  ;; render datagrid
	  (render-datagrid-table-body grid)
	  ;; sort by name (should be descending)
	  (do-request `((,weblocks::*action-string* . "abc126")))
	  ;; output result
	  (with-html (str res)))))
  (htm
   (:div :class "datagrid-body"
	 #.(table-header-template
	    '((:th :class "name sort-ascending" (:span #.(link-action-template "abc123" "Name")))
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

