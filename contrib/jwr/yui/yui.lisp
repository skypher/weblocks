(in-package #:weblocks)

(defpsmacro ensure-dom-ready (&body body)
  "Take &body and put it in a function that will be called after a
DOM-ready event is triggered."
  `(*YAHOO*.util.*event.on-d-o-m-ready (lambda ()
					 ,@body)))

(defpsmacro with-lazy-loaded-modules ((modules &key (load-optional t) base) &body body)
  "Take &body and put it in a function that will be called back once
required modules have been loaded and the DOM is ready."
  (let ((callback-name (ps-gensym "$yui_callback"))
	(loader-name (ps-gensym "$yui_loader")))
    `(progn
       (defun ,callback-name ()
	 (ensure-dom-ready ,@body))
       (defvar ,loader-name (new (*YAHOO*.util.*y-u-i-loader
				  (create :require (array ,@modules)
					  :load-optional ,load-optional
					  ,@(when base `(:base ,base))
					  :on-success ,callback-name))))
       ((@ ,loader-name insert)))))

(defpsmacro keywords-to-object (args)
  `(create ,@args))


(defclass yui-any-editor-presentation (textarea-presentation)
  ((component-config :accessor yui-component-config
		     :initarg :config
		     :initform nil
		     :documentation "A list of JavaScript widget
		     configuration options. Will be passed straight
		     through to `(create ,@options) in parenscript
		     code. Usually a list of keyword value pairs.."))
  (:documentation "A common superclass for both types of rich text
  editor widgets available in the YUI library. Used to provide certain
  common methods. Not to be instantiated directly."))

(defmethod create-and-configure-editor-script ((presentation yui-any-editor-presentation)
					       widget-variable target-id)
  (error "yui-any-editor-presentation is not to be used standalone. Please use one of
its subclasses."))

(defclass yui-simpleeditor-presentation (yui-any-editor-presentation)
  ()
  (:documentation "YUI SimpleEditor: a rich text editor, in its simple incarnation."))

(defclass yui-editor-presentation (yui-any-editor-presentation)
  ()
  (:documentation "YUI Editor: a rich text editor, in full glory."))

(defmethod create-and-configure-editor-script ((presentation yui-simpleeditor-presentation)
					       editor-widget target-id)
  (ps* `(with-lazy-loaded-modules (("simpleeditor"))
	  (setf ,editor-widget
		(new (*YAHOO*.widget.*simple-editor ,target-id
						    (keywords-to-object
						     ,(yui-component-config presentation)))))
	  (setf (slot-value ,editor-widget '_default-toolbar.titlebar) false)
	  ((@ ,editor-widget render)))))

(defmethod create-and-configure-editor-script ((presentation yui-editor-presentation)
					       editor-widget target-id)
  (ps* `(with-lazy-loaded-modules (("editor"))
	  (setf ,editor-widget
		(new (*YAHOO*.widget.*editor ,target-id
					     (keywords-to-object
					      ,(yui-component-config presentation)))))
	  (setf (slot-value ,editor-widget '_default-toolbar.titlebar) false)
	  ((@ ,editor-widget render)))))

(defmethod render-view-field-value :after (value (presentation yui-any-editor-presentation)
						 (field form-view-field) (view form-view) widget obj
						 &rest args &key &allow-other-keys)
  (declare (ignore args)
	   (special *on-ajax-complete-scripts*
		    *presentation-dom-id*
		    *form-submit-dependencies*))
  (let ((widget-variable (ps-gensym "$yui_widget")))
    (send-script (create-and-configure-editor-script presentation
						     widget-variable
						     *presentation-dom-id*))
    (push (make-instance 'javascript-code-dependency
			 :code (ps* `((@ ,widget-variable save-H-T-M-L))))
	  *form-submit-dependencies*)))



(export '(yui-grid-page yui-grid-page-type yui-grid-page-template yui-grid-page-header
	  yui-grid-page-primary-body yui-grid-page-secondary-body yui-grid-page-footer
	  yui-grid-layout yui-grid-first yui-grid-left yui-grid-right))

(defwidget yui-grid-page ()
  ((header :accessor yui-grid-page-header :initarg :header)
   (primary-body :accessor yui-grid-page-primary-body :initarg :primary-body)
   (secondary-body :accessor yui-grid-page-secondary-body :initarg :secondary-body)
   (footer :accessor yui-grid-page-footer :initarg :footer)))


(defmethod initialize-instance :after ((obj yui-grid-page) &rest initargs
				       &key
				       (type "doc")
				       (template "")
				       &allow-other-keys)
  (declare (ignore initargs))
  ;; we don't actually use this list for rendering, but it is necessary
  ;; to keep the weblocks widget tree consistent --jwr
  (setf (widget-children obj :yui-grid-page)
	(list (yui-grid-page-header obj)
	      (yui-grid-page-primary-body obj)
	      (yui-grid-page-secondary-body obj)
	      (yui-grid-page-footer obj)))
  ;; this will set HTML id for us
  (setf (widget-name obj) type)
  (setf (dom-class obj) template))

(defmethod render-widget-body ((obj yui-grid-page) &rest args)
  (declare (ignore args))
  (with-html
    (:div :id "hd" (render-widget (yui-grid-page-header obj)))
    (:div :id "bd"
	  (:div :id "yui-main"
		(:div :class "yui-b" (render-widget (yui-grid-page-primary-body obj))))
	  (:div :class "yui-b" (render-widget (yui-grid-page-secondary-body obj))))
    (:div :id "ft" (render-widget (yui-grid-page-footer obj)))))

(defmethod render-widget-children ((obj yui-grid-page) &rest args)
  "For yui-grid-page, render-widget-body does all the work"
  (declare (ignore args)))


;; TODO: implement various layout types
(defwidget yui-grid-layout ()
  ((first :accessor yui-grid-first :initarg :first :initform nil)
   (left :accessor yui-grid-left :initarg :left :initform nil)
   (right :accessor yui-grid-right :initarg :right :initform nil)))

(defmethod initialize-instance :after ((obj yui-grid-layout) &rest initargs)
  (declare (ignore initargs))
  (setf (dom-class obj) (format nil "yui-g~:[~; first~]" (yui-grid-first obj))))

(defmethod update-children ((obj yui-grid-layout))
  (setf (widget-children obj :yui-grid-layout) (list (yui-grid-left obj) (yui-grid-right obj))))

(defmethod render-widget-body ((obj yui-grid-layout) &rest args)
  (declare (ignore args))
  (with-accessors ((left yui-grid-left) (right yui-grid-right)) obj
    (with-html
      (if (eq 'yui-grid-layout (class-of left))
	  (render-widget left)
	  (htm (:div :class "yui-u first"
		     (render-widget left))))
      (if (eq 'yui-grid-layout (class-of right))
	  (render-widget right)
	  (htm (:div :class "yui-u"
		     (render-widget right)))))))


(export '(yui-mixin yui-widget-variable))

(defclass yui-mixin ()
  ((widget-variable :accessor yui-widget-variable
                    :documentation "Global JavaScript variable that will
                    hold the YUI widget."))
  (:documentation "A mixin used for YUI widgets that need a
  corresponding javascript variable."))

(defmethod initialize-instance :after ((w yui-mixin) &rest args)
  (declare (ignore args))
  (setf (yui-widget-variable w) (ps-gensym "$yui_widget")))


(export '(yui-tabview yui-tabview-tab-labels yui-tabview-selected-tab yui-tabview-tabs))

(defwidget yui-tabview (yui-mixin widget)
  ((tab-labels :accessor yui-tabview-tab-labels
	       :initarg :tab-labels
	       :initform nil
	       :documentation "A list of strings containing tab labels.")
   (selected-tab :accessor yui-tabview-selected-tab
		 :initarg :selected-tab
		 :initform nil
		 :documentation "A string containing the label of the
		 tab that is to be initially selected. Will be compared
		 with tab-labels using EQUAL.")
   (tabs :accessor yui-tabview-tabs
	 :initarg :tabs
	 :initform nil
	 :documentation "A list of widgets, one for each tab,
	 corresponding to tab labels."))
  (:documentation "Implements the YUI TabView widget."))

(defmethod initialize-instance :after ((obj yui-tabview) &rest initargs)
  (declare (ignore initargs))
  ;; this will set HTML id for us
  (setf (dom-class obj) "yui-navset")
  (setf (widget-children obj :yui-tabview) (yui-tabview-tabs obj)))

(defmethod (setf yui-tabview-tabs) ((obj yui-tabview) tab-list)
  (setf (slot-value obj 'tabs) tab-list)
  (setf (widget-children obj :yui-tabview) (yui-tabview-tabs obj)))

(defun tabview-script (tabview-js-var tabview-id)
  (ps* `(with-lazy-loaded-modules (("tabview"))
	  (setf ,tabview-js-var (new (*YAHOO*.widget.*tab-view ,tabview-id))))))

(defmethod render-widget-body ((obj yui-tabview) &rest args)
  (declare (ignore args))
  (send-script (tabview-script (yui-widget-variable obj) (dom-id obj)))
  (let ((tab-counter 0))
    (with-html
      (:ul :class "yui-nav"
	   (mapc (lambda (label)
		   (incf tab-counter)
		   (htm (:li :class (when (equal (yui-tabview-selected-tab obj) label)
				      "selected")
			 (:a :href (format nil "#tab~D" tab-counter)
			     (:em (str label))))))
		 (yui-tabview-tab-labels obj))))))

(defmethod render-widget-children ((obj yui-tabview) &rest args)
  (with-html (:div :class "yui-content"
		   (mapc (lambda (tab)
			   (htm (:div (apply #'render-widget tab args))))
			 (yui-tabview-tabs obj)))))
