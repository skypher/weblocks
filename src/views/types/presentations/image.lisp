
(in-package :weblocks)

(export '(image image-presentation image-presentation-alt
	  image-presentation-title image-presentation-width
	  image-presentation-height image-presentation-url-default))

(defclass image-presentation (text-presentation)
  ((alt :initform nil
	:initarg :alt
	:accessor image-presentation-alt
	:documentation "Alternative text to be specified with the
	image.")
   (title :initform nil
	  :initarg :title
	  :accessor image-presentation-title
	  :documentation "A title to be specified with the image.")
   (width :initform nil
	  :initarg :width
	  :accessor image-presentation-width
	  :documentation "A width attribute of the image.")
   (height :initform nil
	   :initarg :height
	   :accessor image-presentation-height
	   :documentation "A height attribute of the image.")
   (url-default :initform nil
                :initarg :url-default
                :accessor image-presentation-url-default
                :documentation "If specified, uses this URL to render
                a default image, if the value being rendered is
                null."))
  (:documentation "Presents a url as an image."))

(defmethod render-view-field-value ((value null) (presentation image-presentation)
				    field view widget obj &rest args
				    &key highlight &allow-other-keys)
  (declare (ignore highlight))
  (if (image-presentation-url-default presentation)
      (apply #'render-view-field-value (image-presentation-url-default presentation)
             presentation field view widget obj args)
      (call-next-method)))

(defmethod render-view-field-value (value (presentation image-presentation)
				    field view widget obj &rest args
				    &key highlight &allow-other-keys)
  (declare (ignore args highlight))
  (if (null value)
      (call-next-method)
      (with-html
        (:div
         (:img :src value
               :width (image-presentation-width presentation) 
               :height (image-presentation-height presentation) 
               :alt (image-presentation-alt presentation)
               :title (image-presentation-title presentation))))))

