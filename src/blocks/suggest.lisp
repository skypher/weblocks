
(in-package :weblocks)

(export '(render-suggest))

(defun render-suggest (input-name localp &key fetch-fn
		       (input-id (attributize-name (gensym)))
		       (choices-id (attributize-name (gensym))))
  "Renders a block that provides functionality similar to google-suggest.

input-name - the 'name' attribute of the input box.

localp - If nil, the completion will be done via an asynchronious call
to the server. Otherwise, must be set to a list of items which will be
sent to the client for local autocompletion. In this case, the value
of 'fetch-action' is ignored. Note, if JavaScript is turned off local
autocompletion will degrade to a simple drowdown.

fetch-fn - when 'localp' is false, a function that accepts a single
argument (the string entered by the user) and returns a list of items
that will be sent to the client in an appropriate format.

input-id, choices-id - optional IDs of input control and choice div,
respectively. If an ID isn't provided, it will be generated."
  (if localp
      (with-html
	(:select :id input-id :name input-name
		 (mapc (lambda (i)
			 (htm
			  (:option (str i))))
		       localp))
	(with-javascript "replaceDropdownWithSuggest('~A', '~A', '~A');"
	  input-id input-name choices-id))
      (with-html
	(:input :type "text" :id input-id :name input-name :class :class "suggest")
	(:div :id choices-id :class "suggest" "")
	(with-javascript "new Ajax.Autocompleter('~A', '~A', ~A, {});"
	  input-id choices-id url))))

