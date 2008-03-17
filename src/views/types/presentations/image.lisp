
(in-package :weblocks)

(export '(image image-presentation image-presentation-alt
	  image-presentation-title))

(defclass image-presentation (text-presentation)
  ((alt :initform nil
	:initarg :alt
	:accessor image-presentation-alt
	:documentation "Alternative text to be specified with the
	image.")
   (title :initform nil
	  :initarg :title
	  :accessor image-presentation-title
	  :documentation "A title to be specified with the image."))
  (:documentation "Presents a url as an image."))

(defmethod render-view-field-value (value (presentation image-presentation)
				    field view widget obj &rest args
				    &key highlight &allow-other-keys)
  (declare (ignore args highlight))
  (if (null value)
      (call-next-method)
      (with-html
	(:img :src value
	      :alt (image-presentation-alt presentation)
	      :title (image-presentation-title presentation)))))

