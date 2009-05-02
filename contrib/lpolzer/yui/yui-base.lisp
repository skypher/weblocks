(in-package :weblocks)

(export '(yui-widget yui-widget-variable yui-target-id yui-component-config
	  yui-settings-mixin add-options
	  *yui-loader-base* 
          add-component-config js-bool))

(defparameter *yui-loader-base* "http://yui.yahooapis.com/2.6.0/build/"
  "Default base for yui-loader, change if necessary (eg. for 2.7.0, or a local cache)")

(defclass yui-settings-mixin ()
  ((yui-settings :accessor yui-settings :initarg :settings :initform ""))
  (:documentation "Some YUI widgets require additional settings to be
  made after the widget is created, via a series of assignments. This
  mixin provides the functionality to do that. It is to be used in
  conjunction with yui-mixin."))

(defmethod add-options ((obj yui-settings-mixin) &rest options)
  (setf (yui-settings obj)
	(concatenate 'string (yui-settings obj)
		     (args-to-component-settings (yui-widget-variable obj) options))))

(defun args-to-component-settings (component-name &rest args)
  (iterate:iter (iterate:for (option value) on args by #'cddr)
                (iterate:reducing (ps* `(setf (slot-value ,component-name (quote ,option)) ,value))
                  by (lambda (v e)
                       (concatenate 'string v e " "))
                  initial-value "")))


(defwidget yui-widget (widget yui-settings-mixin)
  ((widget-variable :reader yui-widget-variable
                    :initarg :widget-variable
		    :initform (intern (gen-id "yuiWidget")) ; ps-gensym is broken on some lisps
		    :documentation "Global JavaScript variable that will
		    hold the YUI widget.")
   (target-id :accessor yui-target-id
	      :initarg :target-id
	      :initform nil
	      :documentation "Target HTML element that will be replaced
	      or acted upon by the YUI widget.")
   (component-config :accessor yui-component-config
		     :initarg :config
		     :initform nil
		     :documentation "A list of JavaScript widget
		     configuration options. Will be passed straight
		     through to `(create ,@options) in parenscript
		     code. Usually a list of keyword value pairs..")
   (modules :type list
            :reader yui-modules
            :initarg :modules
            :documentation "The YUI modules to be loaded for the widget.")
   (loader-args :type list
            :reader yui-loader-args
            :initarg :loader-args
            :initform nil
            :documentation "Additional arguments to pass to the YUI loader.")
   (class-name :type symbol
               :reader yui-class-name
               :initarg :class-name
               :documentation "The JS class name used to instantiate the widget."))
  (:documentation "YUI widget base class. Most widgets using the YUI library will
                  want to use this class as one of their superclasses."))

(defmethod add-component-config ((obj yui-widget) &rest config)
  (setf (yui-component-config obj)
        (append (yui-component-config obj) config)))

(defmethod render-widget-body ((widget yui-widget) &rest args)
  (declare (ignore args))
  (send-script
    (ps* `(with-lazy-loaded-modules (,(yui-modules widget) ,@(yui-loader-args widget))
            (setf (global-variable ,(yui-widget-variable widget))
                  (new (,(yui-class-name widget) ,(yui-target-id widget)
                                                 (keywords-to-object ,(yui-component-config widget)))))
            ;(console.log ,(format nil "rendered yui widget ~A." (yui-widget-variable widget)))
            ))))

(defun js-bool (lisp-bool)
  (if lisp-bool
    t
    'false))

(defpsmacro ensure-dom-ready (&body body)
  "Take &body and put it in a function that will be called after a
DOM-ready event is triggered."
  `(|:YAHOO.util.:Event.:onDOMReady| (lambda () ,@body)))

(defpsmacro with-lazy-loaded-modules ((modules &key (load-optional t) (base *yui-loader-base*) (include-css-p t)) &body body)
  "Take &body and put it in a function that will be called back once
required modules have been loaded and the DOM is ready."
  (let ((callback-name (ps-gensym "$yui_callback"))
	(loader-name (ps-gensym "$yui_loader")))
    `(progn
       (defun ,callback-name ()
	 (ensure-dom-ready ,@body))
       (defvar ,loader-name (new (|:YAHOO.util.:YUILoader|
				  (create :require (array ,@modules)
					  :load-optional ,load-optional
					  ,@(when base `(:base ,base))
					  :on-success ,callback-name))))
       ((@ ,loader-name insert) ,@(unless include-css-p '({} "js"))))))

(defpsmacro keywords-to-object (args)
  `(create ,@args))

(defpsmacro global-variable (var)
  `(slot-value window (quote ,var)))

(defpsmacro with-global-variable ((variable reference) &body body)
  (let ((global `(global-variable ,reference)))
    `(let ((,variable ,global))
       `(progn ,@body))))

