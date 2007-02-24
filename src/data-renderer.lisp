;;;; Generic data renderer
(in-package :weblocks)

(defmethod render-data-header (obj)
  (format *weblocks-output-stream* "<div class=\"~A\">~%"
	  (attributize-name (object-class-name obj)))
  (format *weblocks-output-stream* "<p><h1>Viewing ~A</h1></p>~%"
	  (humanize-name (object-class-name obj))))

(defmethod render-data-pre-slots (obj)
  (format *weblocks-output-stream* "<ul>~%"))

(defmethod render-data-slot (obj slot-name slot-value)
  (format *weblocks-output-stream* "<li><h2>~A:</h2>"
	  (humanize-name slot-name))
  (render-data slot-value)
  (format *weblocks-output-stream* "</li>~%"))

(defmethod render-data-post-slots (obj)
  (format *weblocks-output-stream* "</ul>~%"))

(defmethod render-data-footer (obj)
  (format *weblocks-output-stream* "</div>"))

(defmethod render-data ((obj standard-object))
  (render-data-header obj)
  (render-data-pre-slots obj)
  (mapc (lambda (slot)
	  (render-data-slot obj (slot-definition-name slot) (get-slot-value obj slot)))
	(object-visible-slots obj))
  (render-data-post-slots obj)
  (render-data-footer obj)
  *weblocks-output-stream*)

(defmethod render-data (obj)
  (format *weblocks-output-stream* "<span>~A</span>" obj)
  *weblocks-output-stream*)

