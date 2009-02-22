
(in-package :weblocks-test)

;;; test initialize instance for pagination
(deftest pagination-initialize-instance-1
    (with-request :get nil
      (let ((p (make-instance 'pagination)))
	(not (null (request-hook :session :pre-action)))))
  t)

;;; test (setf pagination-total-items)
(deftest setf-pagination-total-items-1
    (with-request :get nil
      (let ((p (make-instance 'pagination :total-items 10
			      :items-per-page 3 :current-page 3)))
	(setf (pagination-total-items p) 5)
	(pagination-current-page p)))
  2)

(deftest setf-pagination-total-items-2
    (with-request :get nil
      (let ((p (make-instance 'pagination :total-items 10
			      :items-per-page 3 :current-page 3)))
	(setf (pagination-total-items p) 0)
	(pagination-current-page p)))
  1)

(deftest setf-pagination-total-items-3
    (with-request :get nil
      (let ((p (make-instance 'pagination :total-items 10
			      :items-per-page 3 :current-page 3)))
	(setf (pagination-total-items p) 20)
	(pagination-current-page p)))
  3)

;;; test (setf pagination-items-per-page)
(deftest setf-pagination-items-per-page-1
    (with-request :get nil
      (let ((p (make-instance 'pagination :total-items 10
			      :items-per-page 3 :current-page 3)))
	(setf (pagination-items-per-page p) 5)
	(pagination-current-page p)))
  2)

(deftest setf-pagination-items-per-page-2
    (with-request :get nil
      (let ((p (make-instance 'pagination :total-items 10
			      :items-per-page 3 :current-page 1)))
	(setf (pagination-items-per-page p) 5)
	(pagination-current-page p)))
  1)

;;; test pagination-page-count
(deftest pagination-page-count-1
    (with-request :get nil
      (let ((p (make-instance 'pagination :total-items 10
			      :items-per-page 3 :current-page 3)))
	(pagination-page-count p)))
  4)

(deftest pagination-page-count-2
    (with-request :get nil
      (let ((p (make-instance 'pagination :total-items 0
			      :items-per-page 3 :current-page 3)))
	(pagination-page-count p)))
  0)

;;; test pagination-render-total-item-count
(deftest-html pagination-render-total-item-count-1
    (with-request :get nil
      (let ((p (make-instance 'pagination :total-items 10
			      :items-per-page 3 :current-page 3)))
	(pagination-render-total-item-count p)))
  (:span :class "total-items" " (Total of 10 Items)"))

;;; test render-widget-body for pagination
(deftest-html pagination-render-widget-body-1
    (with-request :get nil
      (let ((p (make-instance 'pagination :total-items 10
			      :items-per-page 3 :current-page 3)))
	(render-widget-body p)))
  (htm
   #.(link-action-template "abc123" "< Previous" :class "previous-page")
   "&nbsp;"
   #.(pagination-page-info-template 3 4)
   "&nbsp;"
   #.(link-action-template "abc124" "Next >" :class "next-page")
   #.(pagination-goto-form-template "abc125")
   (:span :class "total-items" " (Total of 10 Items)")))

(deftest-html pagination-render-widget-body-2
    (with-request :get nil
      (let ((p (make-instance 'pagination :total-items 10
			      :items-per-page 3 :current-page 1)))
	(render-widget-body p)))
  (htm
   #.(pagination-page-info-template 1 4)
   "&nbsp;"
   #.(link-action-template "abc123" "Next >" :class "next-page")
   #.(pagination-goto-form-template "abc124" :page-one-p t)
   (:span :class "total-items" " (Total of 10 Items)")))

(deftest-html pagination-render-widget-body-3
    (with-request :get nil
      (let ((*request-hook* (make-instance 'weblocks::request-hooks))
	    (p (make-instance 'pagination :total-items 10
			      :items-per-page 3 :current-page 1)))
	; render pagination widget
	(render-widget-body p)
	; go to invalid page
	(do-request `(("page-number" . "10")
		      (,weblocks::*action-string* . "abc124")))
	(render-widget-body p)
	; go to valid page
	(weblocks::eval-hook :pre-action)
	(do-request `(("page-number" . "2")
		      (,weblocks::*action-string* . "abc126")))
	(render-widget-body p)))
  (htm
   ; render pagination widget
   #.(pagination-page-info-template 1 4)
   "&nbsp;"
   #.(link-action-template "abc123" "Next >" :class "next-page")
   #.(pagination-goto-form-template "abc124" :page-one-p t)
   (:span :class "total-items" " (Total of 10 Items)")
   ; go to invalid page
   #.(pagination-page-info-template 1 4)
   "&nbsp;"
   #.(link-action-template "abc125" "Next >" :class "next-page")
   #.(pagination-goto-form-template "abc126" :page-one-p t :validatedp nil)
   (:span :class "total-items" " (Total of 10 Items)")
   ; go to valid page
   #.(link-action-template "abc127" "< Previous" :class "previous-page")
   "&nbsp;" #.(pagination-page-info-template 2 4) "&nbsp;"
   #.(link-action-template "abc128" "Next >" :class "next-page")
   #.(pagination-goto-form-template "abc129" :page-one-p nil)
   (:span :class "total-items" " (Total of 10 Items)")))

(deftest-html pagination-render-widget-body-4
    (with-request :get nil
      (let ((*request-hook* (make-instance 'weblocks::request-hooks))
	    (p (make-instance 'pagination :total-items 10
			      :items-per-page 3 :current-page 1)))
	; render pagination widget
	(render-widget-body p)
	; go to next page
	(do-request `((,weblocks::*action-string* . "abc123")))
	(render-widget-body p)))
  (htm
   ; render pagination widget
   #.(pagination-page-info-template 1 4)
   "&nbsp;"
   #.(link-action-template "abc123" "Next >" :class "next-page")
   #.(pagination-goto-form-template "abc124" :page-one-p t)
   (:span :class "total-items" " (Total of 10 Items)")
   ; go to next page
   #.(link-action-template "abc125" "< Previous" :class "previous-page")
   "&nbsp;" #.(pagination-page-info-template 2 4) "&nbsp;"
   #.(link-action-template "abc126" "Next >" :class "next-page")
   #.(pagination-goto-form-template "abc127")
   (:span :class "total-items" " (Total of 10 Items)")))

(deftest-html pagination-render-widget-body-5
    (with-request :get nil
      (let ((*request-hook* (make-instance 'weblocks::request-hooks))
	    (p (make-instance 'pagination :total-items 10
			      :items-per-page 3 :current-page 4)))
	; render pagination widget
	(render-widget-body p)
	; go to next page
	(do-request `((,weblocks::*action-string* . "abc123")))
	(render-widget-body p)))
  (htm
   ; render pagination widget
   #.(link-action-template "abc123" "< Previous" :class "previous-page") "&nbsp;"
   #.(pagination-page-info-template 4 4)
   #.(pagination-goto-form-template "abc124")
   (:span :class "total-items" " (Total of 10 Items)")
   ; go to next page
   #.(link-action-template "abc125" "< Previous" :class "previous-page")
   "&nbsp;" #.(pagination-page-info-template 3 4) "&nbsp;"
   #.(link-action-template "abc126" "Next >" :class "next-page")
   #.(pagination-goto-form-template "abc127")
   (:span :class "total-items" " (Total of 10 Items)")))

;;; make sure on-change for pagination works
(deftest pagination-on-change-1
    (with-request :get nil
      (let* ((*weblocks-output-stream* (make-string-output-stream))
	     (*request-hook* (make-instance 'weblocks::request-hooks))
	     on-change-called-p
	     (p (make-instance 'pagination :total-items 10
			       :items-per-page 3 :current-page 1
			       :on-change (lambda (&rest args)
					    (setf on-change-called-p t)))))
	; render pagination widget
	(render-widget-body p)
	; go to invalid page
	(do-request `(("page-number" . "10")
		      (,weblocks::*action-string* . "abc124")))
	on-change-called-p))
  nil)

(deftest pagination-on-change-2
    (with-request :get nil
      (let* ((*weblocks-output-stream* (make-string-output-stream))
	     (*request-hook* (make-instance 'weblocks::request-hooks))
	     on-change-called-p
	     (p (make-instance 'pagination :total-items 10
			       :items-per-page 3 :current-page 1
			       :on-change (lambda (&rest args)
					    (setf on-change-called-p t)))))
	; render pagination widget
	(render-widget-body p)
	; go to invalid page
	(do-request `(("page-number" . "2")
		      (,weblocks::*action-string* . "abc124")))
	on-change-called-p))
  t)

(deftest pagination-on-change-3
    (with-request :get nil
      (let* ((*weblocks-output-stream* (make-string-output-stream))
	     (*request-hook* (make-instance 'weblocks::request-hooks))
	     on-change-called-p
	     (p (make-instance 'pagination :total-items 10
			       :items-per-page 3 :current-page 2
			       :on-change (lambda (&rest args)
					    (setf on-change-called-p t)))))
	; render pagination widget
	(render-widget-body p)
	; go to invalid page
	(do-request `((,weblocks::*action-string* . "abc123")))
	on-change-called-p))
  t)

(deftest pagination-on-change-4
    (with-request :get nil
      (let* ((*weblocks-output-stream* (make-string-output-stream))
	     (*request-hook* (make-instance 'weblocks::request-hooks))
	     on-change-called-p
	     (p (make-instance 'pagination :total-items 10
			       :items-per-page 3 :current-page 2
			       :on-change (lambda (&rest args)
					    (setf on-change-called-p t)))))
	; render pagination widget
	(render-widget-body p)
	; go to invalid page
	(do-request `((,weblocks::*action-string* . "abc124")))
	on-change-called-p))
  t)

;;; make sure on-error for pagination works
(deftest pagination-on-error-1
    (with-request :get nil
      (let* ((*weblocks-output-stream* (make-string-output-stream))
	     (*request-hook* (make-instance 'weblocks::request-hooks))
	     on-error-called-p
	     (p (make-instance 'pagination :total-items 10
			       :items-per-page 3 :current-page 1
			       :on-error (lambda (&rest args)
					   (setf on-error-called-p t)))))
	; render pagination widget
	(render-widget-body p)
	; go to invalid page
	(do-request `(("page-number" . "10")
		      (,weblocks::*action-string* . "abc124")))
	on-error-called-p))
  t)

(deftest pagination-on-error-2
    (with-request :get nil
      (let* ((*weblocks-output-stream* (make-string-output-stream))
	     (*request-hook* (make-instance 'weblocks::request-hooks))
	     on-error-called-p
	     (p (make-instance 'pagination :total-items 10
			       :items-per-page 3 :current-page 1
			       :on-error (lambda (&rest args)
					   (setf on-error-called-p t)))))
	; render pagination widget
	(render-widget-body p)
	; go to invalid page
	(do-request `(("page-number" . "2")
		      (,weblocks::*action-string* . "abc124")))
	on-error-called-p))
  nil)

(deftest pagination-on-error-3
    (with-request :get nil
      (let* ((*weblocks-output-stream* (make-string-output-stream))
	     (*request-hook* (make-instance 'weblocks::request-hooks))
	     (f (make-instance 'flash))
	     (p (make-instance 'pagination :total-items 10
			       :items-per-page 3 :current-page 1
			       :on-error f)))
	; render pagination widget
	(render-widget-body p)
	; go to invalid page
	(do-request `(("page-number" . "10")
		      (,weblocks::*action-string* . "abc124")))
	(flash-messages f)))
  ("Page number must be an integer between 1 and 4."))

