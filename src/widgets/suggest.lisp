
(in-package :weblocks)

(export '(suggest suggest-local-p fetch-fn))

(defwidget suggest (widget)
  ((localp :accessor suggest-local-p
	   :initform nil
	   :initarg :localp
	   :documentation "If nil, the completion will be done via an
	   asynchronious call to the server. Otherwise, must be set to
	   a list of items which will be sent to the client for local
	   autocompletion. In this case, the value of 'fetch-fn' slot
	   is ignored. Note, if JavaScript is turned off local
	   autocompletion will degrade to a simple drowdown.")
   (fetch-fn :accessor suggest-fetch-fn
	     :initform nil
	     :initarg fetch-fn
	     :documentation "A function that accepts a single
	     argument (a string entered by the user) and returns a
	     list of items that will be sent to the client in an
	     appropriate format."))
  (:documentation "Provides functionality similar to google-suggest."))

; render input box
; render suggest div
; backend call
(defmethod render-widget-body ((obj suggest) &rest args)
  (with-html
    (:input :type "text" :id "autocomplete" :name "autocomplete_parameter" :class "autocomplete-input")
    (:div :id "autocomplete_choices" :class "autocomplete" "")
    (with-javascript
	"new Autocompleter.Local('autocomplete', 'autocomplete_choices', ~A, {});"
      (encode-json-to-string (suggest-local-p obj)))))

;"new Ajax.Autocompleter(\"autocomplete\", \"autocomplete_choices\", \"/url/on/server\", {});"
