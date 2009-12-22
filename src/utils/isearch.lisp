
(in-package :weblocks)

(export '(*isearch-input-delay* 
          *isearch-max-input-length*
          render-isearch
          make-isearch-regex))

(defparameter *isearch-input-delay* 0.4
  "Delay in seconds after keystrokes a client should wait before it
sends an isearch request.")

(defparameter *isearch-max-input-length* 80
  "Maximum input length allowed in an isearch.")

(defun render-isearch (input-name isearch-fn &key value
		       (form-id (gensym))
		       (input-id (gensym))
		       (search-id (gensym))
		       (method :get)
		       (delay *isearch-input-delay*)
		       (max-length *isearch-max-input-length*) &allow-other-keys)
  "Renders an input bar with 'input-name' that calls back 'isearch-fn'
in delayed manner. This is useful for realtime searching
capabilities. Note, 'isearch-fn' is expected to be a function, not an
action. Render-isearch automatically creates an action that performs
certain operations (trims input to 'max-length', etc.) and then passes
action's arguments to 'isearch-fn'.

When JavaScript is turned off a button is drawn next to the input
box. The user may invoke 'isearch-action' by clicking the button.

value - an initial value.
method - form request method, defaults to GET."
  (let* ((a-input-name (attributize-name input-name))
	 (isearch-action
	  (make-action (lambda (&rest args)
			 (let ((param (request-parameter a-input-name))
			       (symbol (intern (if (stringp input-name)
						   (string-upcase input-name)
						   (symbol-name input-name))
					       (find-package :keyword))))
			   (apply isearch-fn
				  symbol
				  (subseq param 0 (min max-length (length param)))
				  (remove-keyword-parameter args symbol)))))))
    (with-html-form (method isearch-action :id form-id :class "isearch")
      (:input :type "text" :id input-id :name a-input-name :class "search-bar" :value value
	      :maxlength max-length)
      (unless (ajax-request-p)
	(htm (:input :id search-id :name *submit-control-name* :type "submit" :class "submit"
		     :value "Search"))))
    (with-javascript "~
new Form.Element.DelayedObserver('~A', ~A, function(elem, value) {~
initiateFormAction('~A', $('~A'), '~A');
});"
      input-id
      delay
      isearch-action form-id (session-name-string-pair))
    (unless (ajax-request-p)
      (with-javascript "$('~A').remove();"
	search-id))))

(defun make-isearch-regex (search)
  "Create a regular expression from the user's input that tries to be
faithful to Emacs' isearch."
  (if (some #'upper-case-p search)
      (ppcre:create-scanner (ppcre:quote-meta-chars search) :case-insensitive-mode nil)
      (ppcre:create-scanner (ppcre:quote-meta-chars search) :case-insensitive-mode t)))

