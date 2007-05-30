
(in-package :weblocks)

(export '(*items-per-page* paging paging-items-per-page
	  paging-total-items paging-current-page))

(defparameter *items-per-page* 15)
(defparameter *page-around-current-page-count* 1)
(defparameter *triangulation-page-count* 4)

(defclass paging (widget)
  ((items-per-page :accessor paging-items-per-page
		   :initform *items-per-page*
		   :initarg :items-per-page)
   (total-items :accessor paging-total-items
		:initform nil
		:initarg :total-items)
   (current-page :accessor paging-current-page
		 :initform 1
		 :initarg :current-page))
  (:metaclass widget-class))

(defun page-count (p)
  (check-type p paging)
  (ceiling (/ (paging-total-items p)
	      (paging-items-per-page p))))

(defmethod with-widget-header ((obj paging) body-fn &rest args)
  (if (<= (page-count obj) 1)
    nil
    (apply #'call-next-method obj body-fn args)))


(defmethod render-widget-body ((obj paging) &rest args)
  (when (<= (page-count obj) 1)
    (return-from render-widget-body))
  (with-slots (current-page) obj
    (let* ((pages-before-current (- current-page *page-around-current-page-count* 1 1))
	   (pages-after-current (- (page-count obj) (+ current-page *page-around-current-page-count*) 1))
	   (hidden-page-count (+ pages-before-current pages-after-current))
	   (unavailable-ratio (/ pages-before-current hidden-page-count))
	   (before-triang-count (clamp
				 1
				 (- *triangulation-page-count* 1)
				 (round (* *triangulation-page-count* unavailable-ratio))))
	   (after-triang-count (- *triangulation-page-count* before-triang-count)))
      (with-html
	(:p
	 ; first-page
	 (:span :class "paging"
		(make-paging-link obj 1))
	 (:span :class "paging" "..")
	 ; triangulation
	 (loop for i from 1 to before-triang-count
	       for j = (round (* i (/ pages-before-current (+ before-triang-count 1))))
	       do (progn
		    (htm (:span :class "paging" 
				(make-paging-link obj j)))))
	 (:span :class "paging" "..")
	 ; prior to current
	 (loop for i from (- current-page *page-around-current-page-count*) to (- current-page 1)
	      do (htm (:span :class "paging" (make-paging-link obj i))))
	 ; current
	 (:span :class "paging current"  (str current-page))
	 ; post current
	 (loop for i from (+ current-page 1) to (+ current-page *page-around-current-page-count*)
	      do (htm (:span :class "paging" (make-paging-link obj i))))
	 (:span :class "paging" "..")
	 ; triangulation
	 (loop for i from 1 to after-triang-count
	       for j = (round (+ current-page (round (* i (/ pages-after-current (+ after-triang-count 1))))))
	       do (progn
		    (log-message* "i: ~A, pac: ~A, atc: ~A" i pages-after-current after-triang-count)
		    (htm (:span :class "paging"
				(make-paging-link obj j)))))
	 ; last
	 (:span :class "paging" "..")
	 (:span :class "paging"  
		(make-paging-link obj (page-count obj))))))))


(defun make-paging-link (obj page)
  (render-link (make-action
		(lambda ()
		  (setf (paging-current-page obj) page)))
	       (princ page)))

(defun clamp (a b j)
  (cond ((< j a) a)
	((> j b) b)
	(t j)))
