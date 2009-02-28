
(in-package :cm)

(export '(html-template with-widget-header render-widget-body))


(defwidget html-template (widget)
  ((tp :accessor tp :initform nil)
   (src :type string :accessor src :initarg :src :initform nil)
   (file :type pathname :accessor file :initarg :file :initform nil)
   (vars :type list :accessor vars :initarg :vars :initform nil))
  (:documentation "Models a HTML-TEMPLATE from a file."))

(defmethod initialize-instance :after ((obj html-template) &rest args)
  (unless (or (file obj) (src obj)
    (error "You need to specify either a template file (initarg :FILE) or a template
           string (initarg :SRC) when creating a HTML-TEMPLATE widget.")))
  (setf (tp obj) (html-template:create-template-printer (or (src obj)
                                                            (pathname (file obj))))))

(defmethod with-widget-header ((widget html-template) body-fn &rest args)
    (apply body-fn widget args))

(defmethod render-widget-body ((widget html-template) &rest args)
  (html-template:fill-and-print-template (tp widget) (vars widget) 
                                         :stream *weblocks-output-stream*))

