
(in-package :weblocks)

(export '(*items-per-page* pager pager-items-per-page
	  pager-total-items))

(defparameter *items-per-page* 3)

(defwidget pager (widget)
  ((items-per-page :accessor pager-items-per-page
		   :initform *items-per-page*
		   :initarg :items-per-page)
   (total-items :accessor pager-total-items
		:initform 1
		:initarg :total-items)
   (slider-position :accessor pager-slider-position
		    :initform 0
		    :initarg :slider-position
		    :affects-dirty-status-p nil)
   (on-change :accessor pager-on-change
	      :initform nil
	      :initarg :on-change
	      :affects-dirty-status-p nil)))

(defun slider-width (p)
  (floor (* (/ (pager-items-per-page p)
	       (pager-total-items p))
	    100)))

(defun get-item-range (p)
  (with-slots (items-per-page total-items slider-position) p
    (let ((start (floor (* (- total-items items-per-page)
			   slider-position))))
      (cons start (+ start items-per-page)))))

(defmethod render-widget-body ((obj pager) &rest args)
  (with-html
    (:div :id "slider-track" :class "slider-track"
	  (:div :id "slider-handle"
		:class "slider-handle"
		:style (format nil "width: ~A%;" (slider-width obj))
		"")))
  (with-javascript (format nil "new Control.Slider('slider-handle', 'slider-track', { sliderValue: ~A, onChange: function(value) { initiateAction('~A' + '&value=' + value, '~A'); } });"
			   (pager-slider-position obj)
			   (make-action (lambda (&rest args &key value &allow-other-keys)
					  (log-message* "VALUE: ~A" value)
					  (setf (pager-slider-position obj)
						(read (make-string-input-stream value)))
					  (safe-funcall (pager-on-change obj) obj)))
			   (session-name-string-pair))))

