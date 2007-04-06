;;;; Generic table renderer
(in-package :weblocks)

(defparameter *render-empty-sequence-string* "No information available."
  "The default string used by the table renderer to signify that
there is no information available.")

;; The usual div wrapper
(defmethod with-table-header (obj body-fn)
  (let* ((object-name (object-class-name obj))
	 (header-class (format nil "renderer table ~A"
			       (if (eql object-name 'null)
				   "empty-table"
				   (attributize-name object-name)))))
    (with-html
      (:div :class header-class
	    (with-extra-tags
	      (funcall body-fn))))))

;; Auxilary functions
(defun render-table-row-aux (obj slots render-cell-fn keys)
  (mapcar (lambda (slot)
	    (apply render-cell-fn obj (cdr slot) (get-slot-value obj (car slot))
		   keys))
	  slots))

(defun render-table-row (obj render-cell-fn &rest keys &key inlinep alternp &allow-other-keys)
  (let ((slots (apply #'object-visible-slots obj keys)))
    (if (not inlinep)
	(with-html
	  (:tr :class (if alternp "altern" nil)
	       (render-table-row-aux obj slots render-cell-fn keys)))
	(render-table-row-aux obj slots render-cell-fn keys))))

;; Table header
(defmethod render-table-header-cell (obj slot-name slot-value
				     &rest keys &key inlinep &allow-other-keys)
  (with-html
    (:th :class (attributize-name slot-name) (str (humanize-name slot-name)))))

(defmethod render-table-header-cell (obj slot-name (slot-value standard-object)
				     &rest keys &key inlinep &allow-other-keys)
  (render-object-slot #'render-table-header-row #'render-table-header-cell obj slot-name slot-value keys))

(defmethod render-table-header-row (obj &rest keys &key inlinep &allow-other-keys)
  (apply #'render-table-row obj #'render-table-header-cell :alternp nil keys))

;; Table body
(defmethod render-table-body-cell (obj slot-name slot-value
				   &rest keys &key inlinep &allow-other-keys)
  (with-html
    (:td :class (attributize-name slot-name) (str slot-value))))

(defmethod render-table-body-cell (obj slot-name (slot-value standard-object)
				   &rest keys &key inlinep &allow-other-keys)
  (render-object-slot #'render-table-body-row #'render-table-body-cell obj slot-name slot-value keys))

(defmethod render-table-body-row (obj &rest keys &key inlinep alternp &allow-other-keys)
  (apply #'render-table-row obj #'render-table-body-cell keys))

;; The table itself
(defmethod render-table ((objs sequence) &rest keys
			 &key (on-empty-string *render-empty-sequence-string*)
			 caption
			 &allow-other-keys)
  (if (empty-p objs)
      (progn
	(render-empty-table :on-empty-string on-empty-string :caption caption)
	(return-from render-table)))
  (let ((row-num -1))
    (with-table-header (car objs)
      (lambda ()
	(with-html
	  (:table
	   (if caption
	       (htm (:caption (str caption))))
	   (htm
	    (:thead (apply #'render-table-header-row (car objs) keys)))
	    (:tbody
	     (mapcar (lambda (obj)
		       (apply #'render-table-body-row obj
			:alternp (oddp (incf row-num))
			keys))
		     objs))))))))
     
(defun render-empty-table (&key on-empty-string caption)
  (with-table-header nil
    (lambda ()
      (with-html
	(:p (if caption
		(htm (:span :class "caption" (str caption) ":&nbsp;")))
	    (:span :class "message" (str on-empty-string)))))))
