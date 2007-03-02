;;;; Generic data renderer
(in-package :weblocks)

(defmethod render-data-header (obj)
  (format *weblocks-output-stream* "<div class=\"data ~A\">~%"
	  (attributize-name (object-class-name obj)))
  (format *weblocks-output-stream*
	  "<div class=\"extra-top-1\">&nbsp;</div>
<div class=\"extra-top-2\">&nbsp;</div>
<div class=\"extra-top-3\">&nbsp;</div>~%")
  (format *weblocks-output-stream* "<h1><span class=\"action\">Viewing:&nbsp;</span>")
  (format *weblocks-output-stream* "<span class=\"object\">~A</span></h1>~%"
	  (humanize-name (object-class-name obj))))

(defmethod render-data-slot-object-inline (obj slot-name (slot-value standard-object) &rest args)
  (format *weblocks-output-stream* "~%<!-- Rendering ~A -->~%" (attributize-name slot-name))
  (apply #'render-data slot-value :inlinep t args)
  (format *weblocks-output-stream* "~%"))

(defmethod render-data-slot-object-reference (obj slot-name (slot-value standard-object) &rest args)
  (let ((obj-name (object-name slot-value)))
    (if (not (null obj-name))
	(apply #'render-data-slot obj slot-name obj-name args))))

(defmethod render-data-slot (obj slot-name (slot-value standard-object) &rest args)
  (if (render-slot-inline-p obj slot-name)
      (apply #'render-data-slot-object-inline obj slot-name slot-value args)
      (apply #'render-data-slot-object-reference obj slot-name slot-value args)))

(defmethod render-data-slot (obj slot-name slot-value &rest args)
  (format *weblocks-output-stream* "<li><h2>~A:</h2>"
	  (humanize-name slot-name))
  (apply #'render-data slot-value args)
  (format *weblocks-output-stream* "</li>~%"))

(defmethod render-data-pre-slots (obj)
  (format *weblocks-output-stream* "<ul>~%"))

(defmethod render-data-post-slots (obj)
  (format *weblocks-output-stream* "</ul>~%"))

(defmethod render-data-footer (obj)
  (format *weblocks-output-stream*
	  "<div class=\"extra-bottom-1\">&nbsp;</div>
<div class=\"extra-bottom-2\">&nbsp;</div>
<div class=\"extra-bottom-3\">&nbsp;</div>~%")
  (format *weblocks-output-stream* "</div>"))

; slot-names is a list of slots. If hidep is t, only the slots in
; slot-names will be displayed, otherwise the slots in slot-names will
; be hidden and all other slots will be displayed. If observe-order-p
; is true, slots will be displayed in the order they appear in the
; list. This option has effect only with hidep is true. If
; slot-names are an association list, slots will be renamed according
; to the values in the list. This option takes effect only if hidep
; is true.
(defmethod render-data ((obj standard-object) &key
			inlinep slot-names hidep observe-order-p)
  (if (not inlinep) (render-data-header obj))
  (if (not inlinep) (render-data-pre-slots obj))
  (mapc (lambda (slot)
	  (apply #'render-data-slot obj (cdr slot) (get-slot-value obj (car slot))
		 `(:slot-names ,slot-names :hidep ,hidep :observe-order-p ,observe-order-p)))
	(object-visible-slots obj
			      :slot-names slot-names
			      :hidep hidep
			      :observe-order-p observe-order-p))
  (if (not inlinep) (render-data-post-slots obj))
  (if (not inlinep) (render-data-footer obj))
  *weblocks-output-stream*)

(defmethod render-data (obj &key
			inlinep slot-names hidep observe-order-p)
  (format *weblocks-output-stream* "<span>~A</span>" obj)
  *weblocks-output-stream*)

