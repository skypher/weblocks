
(in-package :weblocks)

(export '(render-suggest))

(declaim (special *max-raw-input-length*)) ;early

(defun render-suggest (input-name resultset &key
		       default-value
		       (input-id (gensym))
		       (choices-id (gensym))
		       (format-fn #'format-suggest-list)
		       (max-length *max-raw-input-length*)
		       welcome-name)
  "Renders a block that provides functionality similar to google-suggest.

'input-name' - the 'name' attribute of the input box.

If 'resultset' is a list of items, suggestions will be done via local
autocompletions cashed on the client. Otherwise, completion will be
done via an asynchronious call to the server ('resultset' must be set
to a function that accepts a search parameter and returns a list of
itmes that match). Note, if JavaScript is turned off local
autocompletion will degrade to a simple drowdown.

'default-value' - can be used to set the value of the input box and/or
dropdown. If 'default-value' is a list, car of the list will be used
to set the default value, and all subsequent elements will be used to
test dropdown elements (in case suggest degrades to dropdown). This is
useful because occassionally dropdown options have different names
than intended default value.

'input-id', 'choices-id' - optional IDs of input control and choice div,
respectively. If an ID isn't provided, it will be generated.

'format-fn' - a function used to format the results into html sent to
the client. Accepts a list of results.

'welcome-name' - a welcome option in case dropdown is used (see
'render-dropdown')."
  (setf default-value (ensure-list default-value))
  (let ((a-input-name (attributize-name input-name)))
    (if (and (listp resultset)
	     (not (ajax-request-p)))
	(progn
	  (render-dropdown a-input-name resultset :id input-id :selected-value default-value
			   :welcome-name welcome-name)
	  (with-javascript (if default-value
			       "replaceDropdownWithSuggest(~A, '~A', '~A', '~A', '~A');"
			       "replaceDropdownWithSuggest(~A, '~A', '~A', '~A');")
	    (if welcome-name "true" "false") input-id a-input-name choices-id (car default-value)))
	(with-html
	  (:input :type "text" :id input-id :name a-input-name :class "suggest" :value (car default-value)
		  :maxlength max-length)
	  (:div :id choices-id :class "suggest" :style "display: none" "")
	  (with-javascript "declareSuggest('~A', '~A', ~A, '~A');"
	    input-id choices-id
	    (if (listp resultset)
		(encode-json-to-string resultset)
		(format nil "'~A'"
			(make-action
			 (lambda (&rest keys)
			   (declare (ignore keys))
			   (funcall format-fn (funcall resultset (request-parameter a-input-name)))))))
	    (session-name-string-pair))))))

(defun format-suggest-list (results)
  "Formats a list of results into HTML that can later be sent to a
suggest control on the client."
  (let ((*weblocks-output-stream* (make-string-output-stream)))
    (declare (special *weblocks-output-stream*))
    (with-html
      (:ul
       (mapc (lambda (res)
	       (htm (:li (str res))))
	     results)))
    (get-output-stream-string *weblocks-output-stream*)))

