
(in-package :weblocks)

(export '(render-isearch))

(defun render-isearch (input-name isearch-action &key
		       (input-id (gensym))
		       (method :get))
  (let* ((a-input-id (attributize-name input-id))
	 (a-input-name (attributize-name input-name))
	 (a-search-id (concatenate 'string a-input-id "-isearch-submit"))
	 (a-form-id (concatenate 'string a-input-id "-isearch-form")))
    (with-html
      (:form :id a-form-id :class "isearch" :action "" :method (attributize-name method)
	     (:input :type "text" :id a-input-id :name a-input-name :class "isearch")
	     (:input :id a-search-id :name *submit-control-name* :type "submit" :value "Search")
	     (:input :name "action" :type "hidden" :value isearch-action))
      (with-javascript "~
new Form.Element.DelayedObserver('~A', 0.4, function(elem, value) {~
initiateFormAction('~A', $('~A'), '~A');
});~
$('~A').hide();"
	a-input-id
	isearch-action a-form-id (session-name-string-pair)
	a-search-id))))
