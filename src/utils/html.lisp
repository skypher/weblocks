
(in-package :weblocks)

(export '(*submit-control-name*
          *cancel-control-name*
          with-html-form
          render-extra-tags
	  with-extra-tags
	  render-link
          render-button
          render-form-and-button
	  render-checkbox
          render-dropdown
          render-autodropdown
	  *dropdown-welcome-message*
          render-radio-buttons
	  render-close-button
          render-input-field
          render-password
          render-textarea
          render-list
          scriptonly
          noscript
          render-message
          send-script
          with-table
          render-definition-list))

(declaim (special *action-string*))	;early

(defmethod render-extra-tags (tag-class count)
  "Renders extra tags to get around CSS limitations. 'tag-class'
is a string that specifies the class name and 'count' is the
number of extra tags to render.
Ex:
\(render-extra-tags \"extra-\" 2) =>
\"<div class=\"extra-1\"></div><div class=\"extra-1\"></div>\""
  (with-html-output (*weblocks-output-stream*)
    (loop for i from 1 to count
          for attr = (format nil "~A~A" tag-class i)
       do (htm (:div :class attr "<!-- empty -->")))))

(defmacro with-extra-tags (&body body)
  "A macro used to wrap html into extra tags necessary for
hacking CSS formatting. The macro wraps the body with three
headers on top and three on the bottom. It uses
'render-extra-tags' function along with 'extra-top-' and
'extra-bottom-' arguments."
  `(progn
     (render-extra-tags "extra-top-" 3)
     ,@body
     (render-extra-tags "extra-bottom-" 3)))

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
         (:form :id ,id :class ,class :action (request-uri-path)
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
                        (:input :name *action-string* :type "hidden" :value ,action-code))))))
       (log-form ,action-code :id ,id :class ,class))))

(defun render-link (action label &key (ajaxp t) id class title render-fn)
  "Renders an action into a href link. If AJAXP is true (the
default), the link will be rendered in such a way that the action will
be invoked via AJAX or will fall back to a regular request if
JavaScript is not available. When the user clicks on the link the
action will be called on the server.

ACTION may be a function or a result of a call to MAKE-ACTION.
ID, CLASS and TITLE represent their HTML counterparts.
RENDER-FN is an optional function of one argument that is reponsible
for rendering the link's content (i.e. its label). The default rendering
function just calls PRINC-TO-STRING on the label and renders it
without escaping."
  (declare (optimize (speed 3) (space 2)))
  (let* ((*print-pretty* nil)
         (action-code (function-or-action->action action))
	 (url (make-action-url action-code)))
    (with-html
      (:a :id id :class class
	  :href url :onclick (when ajaxp
			       (format nil "initiateAction(\"~A\", \"~A\"); return false;"
				       action-code (session-name-string-pair)))
	  :title title
          (funcall (or render-fn (lambda (label)
                                   (htm (str (princ-to-string label)))))
                   label)))
    (log-link label action-code :id id :class class)))

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

(defun render-checkbox (name checkedp &key id (class "checkbox") onclick)
  "Renders a checkbox in a form.

'name' - name of the html control. The name is attributized before
being rendered.
'checkedp' - if true, renders the box checked.
'id' - id of the html control. Default is nil.
'class' - a class used for styling. By default, \"submit\"."
  (with-html
    (if checkedp
	(htm (:input :name (attributize-name name) :type "checkbox" :id id :class class
		     :value "t" :checked "checked" :onclick onclick))
	(htm (:input :name (attributize-name name) :type "checkbox" :id id :class class
		     :value "f" :onclick onclick)))))

(defparameter *dropdown-welcome-message* "[Select ~A]"
  "A welcome message used by dropdowns as the first entry.")

(defun render-dropdown (name selections &key id class selected-value
                        disabled onchange welcome-name multiple autosubmitp
                        tabindex (frob-welcome-name t))
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

'multiple' - determines whether multiple selections are allowed

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
             :tabindex tabindex
             :disabled disabled
	     :name (attributize-name name)
	     :onchange (or onchange
                           (when autosubmitp
                             "if(this.form.onsubmit) { this.form.onsubmit(); } else { this.form.submit(); }"))
	     :multiple (when multiple "on" "off")
	     (mapc (lambda (i)
		     (if (member (princ-to-string (or (cdr i) (car i))) (ensure-list selected-value)
                                 :test #'string-equal :key #'princ-to-string)
			 (htm (:option :value (cdr i) :selected "selected" (str (car i))))
			 (htm (:option :value (cdr i) (str (car i))))))
		   (list->assoc (append (when welcome-name
					  (list
					   (cons (if frob-welcome-name ; backwards compat
                                                   (format nil *dropdown-welcome-message* (car welcome-name))
                                                   (car welcome-name))
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

(defun render-radio-buttons (name selections &key id (class "radio")
                                                  (selected-value nil selected-value-supplied))
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
     for label-class = (concatenate 'string class (cond
                                                    ((eql j 1) " first")
                                                    ((eql j count) " last")
                                                    (t "")))
     do (with-html
          (:label :id id :class label-class
                  (htm (:input :name (attributize-name name) :type "radio" :class "radio"
                               :value (cdr i) :checked (when (and selected-value-supplied
                                                                  (equalp (cdr i) selected-value))
                                                         "checked")))
                  (:span (str (format nil "~A&nbsp;" (car i))))))))

(defun render-close-button (close-action &optional (button-string "(Close)"))
  "Renders a close button. If the user clicks on the close button,
'close-action' is called back. If 'button-string' is provided, it used
used instead of the default 'Close'."
  (with-html
    (:span :class "close-button"
	   (render-link close-action (humanize-name button-string)))))

(defun render-input-field (type name value &key id class maxlength style size onfocus onblur tabindex)
  (with-html
    (:input :type type :name (attributize-name name) :id id
            :size size
	    :value value :maxlength maxlength :class class
            :style style
            :tabindex tabindex
            :onfocus onfocus
            :onblur onblur)))

(defun render-password (name value &key (id (gen-id)) (class "password") maxlength style
                        default-value size visibility-option-p tabindex)
    "Renders a password in a form.
'name' - name of the html control. The name is attributized before being rendered.
'value' - a value on html control.
'id' - id of the html control. Default is nil.
 maxlength - maximum lentgh of the field
'class' - a class used for styling. By default, \"password\"."
  (render-input-field "password" name value
                      :size size :id id :class class
                      :maxlength maxlength :style style
                      :tabindex tabindex
                      :onfocus (when default-value
                                 (format nil "if (this.value==\"~A\") this.value=\"\";" default-value))
                      :onblur (when default-value
                                (format nil "if (this.value==\"\") this.value=\"~A\";" default-value)))
  (when visibility-option-p
    (send-script (ps:ps*
                   `(defun toggle-password-visibility (field)
                      (let ((it ($ field)))
                        (if (== (slot-value it 'type) "password")
                          (setf (slot-value it 'type) "text")
                          (setf (slot-value it 'type) "password"))))))
    (with-html
      (:label
        (render-checkbox (gen-id) nil
                         :onclick (format nil "togglePasswordVisibility(\"~A\")" id))
        (esc "Show password")))))

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
             (ps:ps* `(funcall (slot-value document 'write) ,,output)))))))

(defmacro noscript (&body body)
  "Outputs HTML in a way that it takes effect on the client only if
client-side scripting is disabled. This macro behaves identically
to :noscript html element, but avoids rendering HTML on AJAX requests
in addition."
  `(when (not (ajax-request-p))
     (with-html
       (:noscript
	 ,@body))))

(defun send-script (script &optional (place :after-load))
  "Send JavaScript to the browser. The way of sending depends
  on whether the current request is via AJAX or not.

  Script may be either a string or a list; if it is a list
  it will be compiled through Parenscript first.
  
  FIXME: is using PUSH or PUSHLAST correct?"
  (let ((script (etypecase script
                  (string script)
                  (list (ps* script)))))
    (if (ajax-request-p)
      (let ((code (if (equalp (hunchentoot:header-in* "X-Weblocks-Client") "JQuery")
                    script
                    (with-javascript-to-string script))))
        (declare (special *before-ajax-complete-scripts* *on-ajax-complete-scripts*))
        (ecase place
          (:before-load (push code *before-ajax-complete-scripts*))
          (:after-load (push code *on-ajax-complete-scripts*))))
      (with-javascript
        script))))

(defun render-message (message &optional caption)
  "Renders a message to the user with standardized markup."
  (with-html
    (:p :class "user-message"
        (when caption
          (htm (:span :class "caption" (str caption) ":&nbsp;")))
	(:span :class "message" (str message)))))

(defmacro with-table (colnames &body body)
  `(with-html
     (:table :cellpadding 0 :cellspacing 0
       (:thead
         (:tr
           ,@(loop for colname in colnames
                   collect `(let ((evaled-colname ,colname))
                              (htm
                                (:th :class (awhen evaled-colname
                                              (attributize-name it))
                                   (awhen evaled-colname
                                     (esc it))))))))
       (:tbody
         ,@body))))

(defun render-definition-list (data &key class id style)
  "Render the definition list DATA (a flat key/value list).
The caller is responsible for escaping all data."
  (assert (evenp (length data)) (data))
  (with-html
    (:dl :class class :id id :style style
      (loop for dt = (pop data)
            for dd = (pop data)
            while (and dt dd)
            do (htm
                 (:dt :class (attributize-name dt) (str dt))
                 (:dd :class (attributize-name dt) (str dd)))))))

