(in-package :app)

(defwidget replaceable-stack-grid (stackgridedit) 
  ()
  (:documentation "A widget based on the 'gridedit' that replaces the entire grid with the drilldown, and renders an return link."))


(defmethod dataseq-render-operations ((obj replaceable-stack-grid) &rest args)
  (declare (ignore args))
  ;purposely do not render buttons. pagination, datamining fns are separate!
  (when (null (dataedit-item-widget obj))
    (call-next-method)))

(defmethod dataedit-create-new-item-widget ((grid replaceable-stack-grid))
    (make-instance 'stackform
     :data (make-instance (dataseq-data-form-class grid))
     :class-store (dataseq-class-store grid)
     :ui-state :form
     :subheadings (stackgridedit-subheadings grid)
     :on-cancel (lambda (obj)
		  (declare (ignore obj))
		  (dataedit-reset-state grid)
		  (throw 'annihilate-dataform nil))
     :on-success (lambda (obj)
		   (safe-funcall (dataedit-on-add-item grid) grid obj)
		   (flash-message (dataseq-flash grid)
				  (format nil "~A ~A."
					  #!"Added"
					  (humanize-name (dataseq-data-class grid))))
		   (dataedit-reset-state grid))
     :on-close (lambda (obj)
		 (declare (ignore obj))
		 (dataedit-reset-state grid))
     ;we're making a new item, so don't really need the data-view ..?
     ;:data-view (dataedit-item-data-view grid)
     :form-view (dataedit-item-form-view grid)))

(defmethod dataedit-create-drilldown-widget ((grid replaceable-stack-grid) item)

  (make-instance 'stackform
		 :data item
		 :subheadings (stackgridedit-subheadings grid)
		 :class-store (dataseq-class-store grid)
		 :hide-modifiers (stackgridedit-hide-modifiers grid)
		 :ui-state (if (eql (gridedit-drilldown-type grid) :edit)
			       :form
			       :data)
		 :on-success (lambda (obj)
			       (declare (ignore obj))
			       (flash-message (dataseq-flash grid)
					      (format nil "Modified ~A."
						      (humanize-name (dataseq-data-class grid))))
			       (if (eql (gridedit-drilldown-type grid) :edit)
				   (dataedit-reset-state grid)
				   (mark-dirty grid)))
		 :on-cancel (when (eql (gridedit-drilldown-type grid) :edit)
			      (lambda (obj)
				(declare (ignore obj))
				(dataedit-reset-state grid)))
		 :on-close (lambda (obj)
			     (declare (ignore obj))
			     (dataedit-reset-state grid))
		 :data-view (dataedit-item-data-view grid)
		 :form-view (dataedit-item-form-view grid))

)

;;; Renders our custom version of the gridedit. 
;;; Essentially, if we are viewing an item, we render only that item,
;;;  and no others (ie the grid body is not rendered, only the drilldown).
;;; datagrid's render method, except that proper controls (add item,
;;; delete item, etc.) are added or removed depending on the settings
(defmethod render-widget-body ((obj replaceable-stack-grid) &rest args)
  (declare (ignore args))
  (dataedit-update-operations obj)
  ; call next method here followed by return will reset behavior to parent widget.
  (if (dataedit-item-widget obj)
      (progn 
	;Some JS to make a diff loading effect
	#+OLD(send-script (ps* (progn `(*effect.toggle ,(format nil "~A" (dom-id obj)) "blind") 
				 ))
				
		     :before-load)
	#+OLD(send-script (ps* (progn `(*effect.appear ,(format nil "~A" (dom-id obj)) "blind") 
				 ))
					;`(*effect.toggle ,(format nil "~A"(dom-id obj)) "blind" )))
		     :after-load)
	;(break (format nil "Widget parent of item widget is ~A " (widget-parent (dataedit-item-widget obj)) ))
	;;attempting to fix using clear-parents idea
	(when (widget-parent (dataedit-item-widget obj))
	  (setf (widget-parent (dataedit-item-widget obj)) nil))
	(render-widget-body 
	 (wrap-in-composite 
	  (list 
	   (make-instance 'simple-widget
			  :render-fn (lambda (w)
				       (render-link (lambda (&rest args ) (declare (ignore args)) (dataedit-reset-state obj))
						    #!"return to grid" :class "button")))
	   (dataedit-item-widget obj))))) 
      (progn 
	;(break "Calling next method since there is no dataedit object")
	(call-next-method))))


#+OLD(defmethod render-widget-body ((obj dataseq) &rest args &key
			       pre-data-mining-fn post-data-mining-fn)
  ;; Do necessary bookkeeping
  (dataseq-update-sort-column obj)
  (when (dataseq-allow-pagination-p obj)
    (setf (pagination-total-items (dataseq-pagination-widget obj))
	  (dataseq-data-count obj)))
  ;; Render Data mining
  (safe-funcall pre-data-mining-fn obj)
  (when (and (>= (dataseq-data-count obj) 1)
	     (or (dataseq-allow-select-p obj)
		 (dataseq-show-total-items-count-p obj)))
    (apply #'dataseq-render-mining-bar obj args))
  (safe-funcall post-data-mining-fn obj)
  ;; Render flash
  (render-widget (dataseq-flash obj))
  ;; Render Body
  (flet ((render-body ()
	   ;; Render items
	   (apply #'render-dataseq-body obj args)
	   ;; Render item ops
	   (when (and (dataseq-allow-operations-p obj)
		      (or (dataseq-item-ops obj)
			  (dataseq-common-ops obj)))
	     (apply #'dataseq-render-operations obj args))))
    (if (dataseq-wrap-body-in-form-p obj)
	(with-html-form (:get (make-action (curry #'dataseq-operations-action obj))
			      :class "dataseq-form")
	  (render-body))
	(render-body)))
  ;; Render Pagination
  (when (dataseq-allow-pagination-p obj)
    (dataseq-render-pagination-widget obj)))
