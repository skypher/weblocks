
(in-package :weblocks)

(export '(*submit-control-name* *cancel-control-name* with-html-form
	  render-link render-button render-form-and-button
	  render-checkbox render-dropdown render-autodropdown
	  *dropdown-welcome-message* render-radio-buttons
	  render-close-button render-password render-textarea
	  render-list scriptonly noscript render-message))

(defparameter *submit-control-name* "submit"
  "The name of the control responsible for form submission.")

(defparameter *cancel-control-name* "cancel"
  "The name of the control responsible for cancellation of form
  submission.")

(defmacro with-html-form ((method-type action &key id class enctype (use-ajax-p t) extra-submit-code
                          (submit-fn "initiateFormAction(\"~A\", $(this), \"~A\")")) &body body)
  "Transforms to cl-who (:form) with standard form code (AJAX support, actions, etc.)"
  (let ((action-code (gensym)))
    `(let ((,action-code (function-or-action->action ,action)))
       (with-html
         (:form :id ,id :class ,class :action (string-right-trim "/" *current-navigation-url*)
                :method (attributize-name ,method-type) :enctype ,enctype
                :onsubmit (when ,use-ajax-p
                            (format nil "~@[~A~]~A; return false;"
				    ,extra-submit-code
                                    (format nil ,submit-fn
                                            (url-encode (or ,action-code ""))
                                            (session-name-string-pair))))
                (with-extra-tags
                  (htm (:fieldset
                        ,@body
                        (:input :name *action-string* :type "hidden" :value ,action-code)))))))))

(defun render-link (action name &key (ajaxp t) id class)
  "Renders an action into an href link. If 'ajaxp' is true (the
default), the link will be rendered in such a way that the action will
be invoked via AJAX, or will fall back to regular request if
JavaScript is not available. When the user clicks on the link, the
action will be called on the server.

'action' - may be a function (in which case 'render-link' will
automatically call 'make-action'), or a result of a call
to 'make-action'.
'name' - A string that will be presented to the user in the
link.
'ajaxp' - whether link is submitted via AJAX if JS is available (true
by default).
'id' - An id passed into HTML.
'class' - A class placed into HTML."
  (let* ((action-code (function-or-action->action action))
	 (url (make-action-url action-code)))
    (with-html
      (:a :id id :class class
	  :href url :onclick (when ajaxp
			       (format nil "initiateAction(\"~A\", \"~A\"); return false;"
				       action-code (session-name-string-pair)))
	  (str name)))))

(defun render-button (name  &key (value (humanize-name name)) id (class "submit"))
  "Renders a button in a form.

'name' - name of the html control. The name is attributized before
being rendered.
'value' - a value on html control. Humanized name is default.
'id' - id of the html control. Default is nil.
'class' - a class used for styling. By default, \"submit\"."
  (with-html
    (:input :name (attributize-name name) :type "submit" :id id :class class
	    :value value :onclick "disableIrrelevantButtons(this);")))

(defun render-form-and-button (name action &key (value (humanize-name name))
			       (method :get)
			       button-id (button-class "submit")
			       (use-ajax-p t)
			       form-id form-class)
  "Renders a button within a form. This function can be used a short
cut to quickly render a sumbit button."
  (with-html-form (method action
			  :use-ajax-p use-ajax-p
			  :id form-id :class form-class)
    (render-button name :value value :id button-id :class button-class)))

(defun render-checkbox (name checkedp &key id (class "checkbox"))
  "Renders a checkbox in a form.

'name' - name of the html control. The name is attributized before
being rendered.
'checkedp' - if true, renders the box checked.
'id' - id of the html control. Default is nil.
'class' - a class used for styling. By default, \"submit\"."
  (with-html
    (if checkedp
	(htm (:input :name (attributize-name name) :type "checkbox" :id id :class class
		     :value "t" :checked "checked"))
	(htm (:input :name (attributize-name name) :type "checkbox" :id id :class class
		     :value "t")))))

(defparameter *dropdown-welcome-message* "[Select ~A]"
  "A welcome message used by dropdowns as the first entry.")

(defun render-dropdown (name selections &key id class selected-value
			welcome-name autosubmitp)
  "Renders a dropdown HTML element (select).

'name' - the name of html control. The name is attributized before
being rendered.

'selections' - a list of strings to render as selections. Each element
may be a string or a cons cell. If it is a cons cell, the car of each
cell will be used as the text for each option and the cdr will be used
for the value.

'id' - an id of the element.

'class' - css class of the element.

'selected-value' - a list of strings. Each option will be tested
against this list and if an option is a member it will be marked as
selected. A single string can also be provided.

'welcome-name' - a string used to specify dropdown welcome option (see
*dropdown-welcome-message*). If nil, no welcome message is used. If
'welcome-name' is a cons cell, car will be treated as the welcome name
and cdr will be returned as value in case it's selected.

'autosubmitp' - determines if the dropdown automatically submits
itself on selection. Note, if the parent form uses ajax, the dropdown
will submit an ajax request. Otherwise, a regular request will be
submitted."
  (when welcome-name
    (setf welcome-name (car (list->assoc (list welcome-name)
					 :map (constantly "")))))
  (with-html
    (:select :id id
	     :class class
	     :name (attributize-name name)
	     :onchange (when autosubmitp
			 "if(this.form.onsubmit) { this.form.onsubmit(); } else { this.form.submit(); }")
	     (mapc (lambda (i)
		     (if (member (format nil "~A" (or (cdr i) (car i)))
				 (ensure-list selected-value)
				 :test #'equalp :key (curry #'format nil "~A"))
			 (htm (:option :value (cdr i) :selected "selected" (str (car i))))
			 (htm (:option :value (cdr i) (str (car i))))))
		   (list->assoc (append (when welcome-name
					  (list
					   (cons (format nil *dropdown-welcome-message* (car welcome-name))
						 (cdr welcome-name))))
					selections)
				:map (constantly nil))))))

(defun render-autodropdown (name selections action
			    &key selected-value welcome-name
			    id (class "autodropdown")
			    dropdown-id dropdown-class
			    submit-id submit-class
			    (submit-button-name (humanize-name *submit-control-name*))
			    (method-type :get)
			    (use-ajax-p t))
  "Renders a dropdown along with a form. The dropdown automatically
submits on selection, or if JS is off, a button is automatically
presented to the user."
  (with-html-form (method-type action :use-ajax-p use-ajax-p :id id :class class)
    (render-dropdown name selections
		     :id dropdown-id
		     :class dropdown-class
		     :selected-value selected-value
		     :welcome-name welcome-name
		     :autosubmitp t)
    (:noscript
     (render-button *submit-control-name* :value (humanize-name submit-button-name)
		    :id submit-id :class submit-class))))

(defun render-radio-buttons (name selections &key id (class "radio") selected-value)
  "Renders a group of radio buttons.

'name' - name of radio buttons.
'selections' - a list of selections. May be an association list, in
which case its car is used to dispaly selection text, and cdr is used
for the value.
'id' - id of a label that holds the radio buttons.
'class' - class of the label and of radio buttons.
'selected-value' - selected radio button value."
  (loop for i in (list->assoc selections)
     for j from 1
     with count = (length selections)
     for label-class = (cond
			 ((eq j 1) (concatenate 'string class " first"))
			 ((eq j count) (concatenate 'string class " last"))
			 (t class))
     do (progn
	  (when (null selected-value)
	    (setf selected-value (cdr i)))
	  (with-html
	    (:label :id id :class label-class
		    (if (equalp (cdr i) selected-value)
			(htm (:input :name (attributize-name name) :type "radio" :class "radio"
				     :value (cdr i) :checked "checked"))
			(htm (:input :name (attributize-name name) :type "radio" :class "radio"
				     :value (cdr i))))
		    (:span (str (format nil "~A&nbsp;" (car i)))))))))

(defun render-close-button (close-action &optional (button-string "(Close)"))
  "Renders a close button. If the user clicks on the close button,
'close-action' is called back. If 'button-string' is provided, it used
used instead of the default 'Close'."
  (with-html
    (:span :class "close-button"
	   (render-link close-action (humanize-name button-string)))))

;;; render password implementation
(defun render-password (name value &key id (class "password") maxlength)
    "Renders a password in a form.
'name' - name of the html control. The name is attributized before being rendered.
'value' - a value on html control.
'id' - id of the html control. Default is nil.
 maxlength - maximum lentgh of the field
'class' - a class used for styling. By default, \"password\"."
  (with-html
    (:input :type "password" :name (attributize-name name) :id id
	    :value value :maxlength maxlength :class class)))


(defun render-textarea (name value rows cols &key id class)
  "Renders a textarea in a form.
'name' - name of the html control. The name is attributized before being rendered.
'value' - a value on html control. Humanized name is default.
'id' - id of the html control. Default is nil.
'maxlength' - maximum lentgh of the field  
'rows' - number of rows in textarea
'cols' - number of columns in textarea
'class' - a class used for styling. By default, \"textarea\"."
  (with-html
      (:textarea :name (attributize-name name) :id id
		 :rows rows :cols cols :class class
		 (str (or value "")))))

(defun render-list (seq &key render-fn (orderedp nil) id class
		    (empty-message "There are no items in the list.")
		    empty-caption item-prefix-fn item-suffix-fn)
  "Renders a sequence of items 'seq' in an HTML list. If 'render-fn'
is provided, calls it with one argument (the item being rendered) in
order to render the actual item. Otherwise, renders each item as a
widget. By default the list is unordered. To render an ordered list,
set orderedp to true. If item-prefix-fn or item-suffix-fn are
provided, they're called before and after each item (respectively)
with the item as a single argument."
  (if seq
      (flet ((render-items ()
	       "Renders the items of the list."
	       (loop for i in seq
		  do (with-html
		       (safe-funcall item-prefix-fn i)
		       (:li
			(if render-fn
			    (funcall render-fn i)
			    (render-widget i)))
		       (safe-funcall item-suffix-fn i)))))
	(if orderedp
	    (with-html
	      (:ol :class class :id id
		   (render-items)))
	    (with-html
	      (:ul :class class :id id
		   (render-items)))))
      (with-html
	(:div :class "view"
	      (with-extra-tags 
		(htm
		 (:div :class "empty"
		       (render-message empty-message empty-caption))))))))

(defmacro scriptonly (&body body)
  "Outputs HTML defined in the body in such a way that it takes effect
on the client only if client-side scripting is enabled."
  (let ((output (gensym)))
    `(let (,output)
       (let ((*weblocks-output-stream* (make-string-output-stream)))
	 (with-html
	   ,@body)
	 (setf ,output (get-output-stream-string *weblocks-output-stream*)))
       (if (ajax-request-p)
	   (write-string ,output *weblocks-output-stream*)
	   (with-javascript
	       "document.write(~A);"
	     (encode-json-to-string ,output))))))

(defmacro noscript (&body body)
  "Outputs HTML in a way that it takes effect on the client only if
client-side scripting is disabled. This macro behaves identically
to :noscript html element, but avoids rendering HTML on AJAX requests
in addition."
  `(when (not (ajax-request-p))
     (with-html
       (:noscript
	 ,@body))))

(defun render-message (message &optional caption)
  "Renders a message to the user with standardized markup."
  (with-html
    (:p (if caption
	    (htm (:span :class "caption" (str caption) ":&nbsp;")))
	(:span :class "message" (str message)))))

