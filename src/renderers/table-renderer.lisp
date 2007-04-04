;;;; Generic table renderer
(in-package :weblocks)

(defparameter *render-empty-sequence-string* "No information available."
  "The default string used by the table renderer to signify that
there is no information available.")

(defmethod with-table-header (obj body-fn)
  (let* ((object-name (object-class-name obj))
	 (header-class (format nil "renderer table ~A"
			       (if (eql object-name 'null)
				   "empty-table"
				   (attributize-name object-name)))))
    (with-html
      (:div :class header-class
	    (render-extra-tags "extra-top-" 3)
	    (funcall body-fn)
	    (render-extra-tags "extra-bottom-" 3)))))

(defmethod render-table-header-cell (slot-name slot-value &rest keys)
  (with-html
    (:th :class (attributize-name slot-name) (str (humanize-name slot-name)))))

(defmethod render-table-header-cell (slot-name (slot-value standard-object) &rest keys)
  (apply #'render-table-header slot-value (apply #'object-visible-slots slot-value keys) :inlinep t keys))

(defmethod render-table-header (obj slots &rest keys &key inlinep &allow-other-keys)
  (let ((render-headers (lambda ()
			  (mapcar (lambda (slot)
				    (if (render-slot-inline-p obj (cdr slot))
					(apply #'render-table-header-cell (cdr slot)
					       (get-slot-value obj (car slot)) keys)
					(apply #'render-table-header-cell (cdr slot)
					       (object-name (get-slot-value obj (car slot)))
					       keys)))
				  slots))))
    (if (not inlinep)
	(with-html
	  (:thead (:tr (funcall render-headers))))
	(funcall render-headers))))

(defmethod render-table-cell (obj slot-name (slot-value standard-object)
			      &rest keys &key inlinep &allow-other-keys)
  (if (render-slot-inline-p obj slot-name)
      (apply #'render-table-row slot-value (apply #'object-visible-slots slot-value keys) :inlinep t keys)
      (with-html
	(:td (render-data (object-name slot-value))))))

(defmethod render-table-cell (obj slot-name slot-value
			      &rest keys &key inlinep &allow-other-keys)
  (with-html
    (:td :class (attributize-name slot-name) (str slot-value))))

(defmethod render-table-row (obj slots &rest keys &key inlinep alternp &allow-other-keys)
  (let ((render-row (lambda ()
		      (mapcar (lambda (slot)
				(apply #'render-table-cell obj (cdr slot) (get-slot-value obj (car slot))
				       keys))
			      slots))))
    (if (not inlinep)
	(with-html
	  (:tr :class
	       (if alternp "altern" nil)
	   (funcall render-row)))
	(funcall render-row))))

(defmethod render-table ((objs sequence) &rest keys
			 &key (on-empty-string *render-empty-sequence-string*)
			 caption
			 &allow-other-keys)
  (if (empty-p objs)
      (progn
	(render-empty-table :on-empty-string on-empty-string :caption caption)
	(return-from render-table)))
  (let* ((row-num -1)
	 (render-body (lambda ()
			(with-html
			  (:table
			   (if caption
			       (htm (:caption (str caption))))
			   (apply #'render-table-header (car objs)
				  (apply #'object-visible-slots (car objs) keys) keys)
			   (htm
			    (:tbody
			     (mapcar (lambda (obj)
				       (apply #'render-table-row obj
					      (apply #'object-visible-slots obj keys)
					      :alternp (oddp (incf row-num))
					      keys))
				     objs))))))))
    (with-table-header (car objs) render-body)))
     
(defmethod render-empty-table (&key on-empty-string caption)
  (let ((render-empty (lambda ()
			(with-html
			  (:p
			   (if caption
			       (htm (:span (str caption) ":&nbsp;")))
			   (:span (str on-empty-string)))))))
      (with-table-header nil render-empty)))
