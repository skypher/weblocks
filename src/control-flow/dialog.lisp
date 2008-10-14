
(in-package :weblocks)

(export '(do-dialog do-choice do-confirmation do-information))

(defstruct dialog
  "A structure that stores information about a currently displayed
dialog."
  title widget css-class)

(defmacro current-dialog ()
  "Expands to code that signifies a place that contains information
about the currently active dialog, if any. The place holds a structure
of type 'dialog'."
  `(webapp-session-value 'dialog-contents))

(defun show-dialog-js (title widget css-class &optional escape-script-tags-p)
  "Returns a string with JS code that shows a modal pop-up dialog with
the widget inside."
  (flet ((escape-script-tags (source)
	   (ppcre:regex-replace-all (ppcre:quote-meta-chars "</script>")
				    (ppcre:regex-replace-all (ppcre:quote-meta-chars "]]>")
							     source
							     "]]\" + \">")
				    "</scr\" + \"ipt>"))
	 (widget-html (widget)
	   (let ((*weblocks-output-stream* (make-string-output-stream)))
	     (declare (special *weblocks-output-stream*))
	     (render-widget widget)
	     (get-output-stream-string *weblocks-output-stream*))))
    (format nil "showDialog(~A, ~A, ~A);"
	    (encode-json-to-string title)
	    (funcall (if escape-script-tags-p
			 #'escape-script-tags
			 #'identity)
		     (encode-json-to-string
		      (widget-html widget)))
	    (encode-json-to-string (or css-class "")))))

(defun update-dialog-on-request ()
  "This callback function is called by 'handle-client-request'. If a
request is a refresh and a dialog was shown, appropriate JS is
inserted into the page to redraw the dialog."
  (let ((current-dialog (current-dialog)))
    (when (and current-dialog
	       (refresh-request-p))
      (with-javascript
	  "Event.observe(window, 'load', function() {~%~
             ~A~%~
        });"
	(show-dialog-js (dialog-title current-dialog)
			(dialog-widget current-dialog)
			(dialog-css-class current-dialog)
			t)))))

;;; Presents 'callee' to the user in a modal dialog, saves the
;;; continuation, and returns from the delimited computation. When
;;; 'callee' answers, removes the modal interface and reactives the
;;; computation. If the modal interface isn't available, automatically
;;; scales down to 'do-modal' instead.
(defun/cc do-dialog (title callee &key css-class)
  (declare (special *on-ajax-complete-scripts*))
  (if (ajax-request-p)
      (prog2
	  (when (current-dialog)
	    (error "Multiple dialogs not allowed."))
	  (call callee (lambda (new-callee)
			 (setf (current-dialog) (make-dialog :title title
							     :widget new-callee
							     :css-class css-class))
                         (send-script (show-dialog-js title new-callee css-class))))
	(setf (current-dialog) nil)
	(send-script (ps (remove-dialog))))
      (do-modal title callee :css-class css-class)))

(defun render-choices-get (msg choices k)
  "Renders the contents of a choice dialog with choices displayed as
links."
  (with-html
    (:p (str msg))
    (mapc (lambda (choice)
	    (render-link (lambda (&rest args)
			   (declare (ignore args))
			   (answer k choice))
			 (humanize-name choice))
	    (htm "&nbsp;"))
	  choices)))

(defun render-choices-post (msg choices k)
  "Renders the contents of a choice dialog with choices displayed as
form buttons in a POST form."
  (with-html-form (:post (lambda (&rest args)
			   (loop for choice in choices
			      when (member choice args)
			      do (progn (answer k choice)
					(return)))))
    (:p (str msg))
    (mapc #'render-button choices)))

;;; Presents a user with a message and a choice of elements
(defun/cc do-choice (msg choices &key (method :post) (css-class "") (title "Select Option"))
  (do-dialog title
             (curry (ecase method
		      (:get #'render-choices-get)
		      (:post #'render-choices-post))
		    msg choices)
             :css-class (format nil "choice ~A" css-class)))

;;; Presents a user with a confirmation dialog
(defun/cc do-confirmation (msg &key (type :ok/cancel) (css-class ""))
  (do-choice msg (ecase type
		   (:ok/cancel (list :ok :cancel))
		   (:yes/no (list :yes :no)))
	     :css-class (format nil "confirmation ~A" css-class)
	     :title "Confirmation"))

;;; Presents a user with an information dialog
(defun/cc do-information (msg &key (css-class ""))
  (do-choice msg (list :ok)
	     :css-class (format nil "information ~A" css-class)
	     :title "Information"))

