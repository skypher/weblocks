
(in-package :weblocks)

(export '(*pagination-items-per-page* pagination
	  pagination-items-per-page pagination-total-items
	  pagination-show-total-items-p pagination-current-page
	  pagination-on-change pagination-on-error pagination-page-count
	  pagination-render-total-item-count pagination-page-item-range
	  pagination-on-go-to-page pagination-call-on-change))

(defparameter *pagination-items-per-page* 15
  "Default number of items visible on a page.")

(defwidget pagination (widget)
  ((items-per-page :accessor pagination-items-per-page
		   :initform *pagination-items-per-page*
		   :initarg :items-per-page
		   :documentation "Number of data items that are
		   displayed on each page. This value defaults to
		   *pagination-items-per-page*.")
   (total-items :accessor pagination-total-items
		:initform 0
		:initarg :total-items
		:documentation "Total number of data items managed by
		this pagination widget. Note, if the number of items
		is zero (the default), the pagination widget will not
		render pagination UI.")
   (show-total-items-p :accessor pagination-show-total-items-p
		       :initarg :show-total-items-p
		       :initform t
		       :documentation "If true (default) allows
		       showing total number of items to the user.")
   (current-page :accessor pagination-current-page
		 :initform 1
		 :initarg :current-page
		 :documentation "The number of the currently viewed
		 page.")
   (on-change :accessor pagination-on-change
	      :initform nil
	      :initarg :on-change
	      :documentation "An optional callback function that will
	      be called by the pagination widget when the currently
	      viewed page is changed by the user. The function must
	      accept three arguments: the pagination widget, starting
	      item index (inclusive), and ending item
	      index (exclusive).")
   (on-error :accessor pagination-on-error
	     :initform nil
	     :initarg :on-error
	     :documentation "Because the pagination widget allows
	     inputting the page number to go to, there are potential
	     errors that need to be handled (invalid page, etc.)
	     Pagination widget expects 'on-error' to be bound to a
	     flash widget (in which case it will be used to notify the
	     user of an error), or a function of one argument (error
	     message) that can handle the error in a custom manner. If
	     'on-error' is bound to anything else the client of the
	     widget will not be notified of an error. In all cases,
	     pagination widget will apply 'item-not-validated' class
	     to the input field.")
   (last-request-error-p :initform nil
			 :documentation "Used an an intermediary flag
		         to determine if the last request resulted in
		         an error."))
  (:documentation "The pagination widget can be used to provide UI to
  break up large amount of data into pages."))

(defmethod initialize-instance :after ((obj pagination) &rest initargs)
  (declare (ignore initargs))
  ; Mark error flag for reset on action
  (push (lambda ()
	  (when (and (slot-value obj 'last-request-error-p)
		     (not (refresh-request-p)))
	    (setf (slot-value obj 'last-request-error-p) nil)))
	(request-hook :session :pre-action)))

(defmethod (setf pagination-total-items) :after (value (obj pagination))
  (when (> (pagination-current-page obj)
	   (pagination-page-count obj))
    (setf (pagination-current-page obj)
	  (max 1 (pagination-page-count obj)))))

(defmethod (setf pagination-items-per-page) :around (value (obj pagination))
  (multiple-value-bind (begin end)
      (pagination-page-item-range obj)
    (declare (ignore end))
    (call-next-method)
    (setf (pagination-current-page obj)
	  (max 1 (ceiling begin value)))))

(defun pagination-page-count (pagination)
  "Returns the total number of pages necessary to present the data
managed by a given pagination widget."
  (multiple-value-bind (page-count remainder)
      (ceiling (pagination-total-items pagination)
	       (pagination-items-per-page pagination))
    (declare (ignore remainder))
    page-count))

(defgeneric pagination-render-total-item-count (obj &rest args)
  (:documentation
   "This function is responsible for rendering the total number of
items if 'show-total-items' is set to true."))

(defmethod pagination-render-total-item-count ((obj pagination) &rest args)
  (declare (ignore args))
  (when (pagination-show-total-items-p obj)
    (with-html (:span :class "total-items"
		      " (Total of " 
		      (str (pagination-total-items obj)) " "
		      (str (proper-number-form (pagination-total-items obj) "Item"))
		      ")"))))

(defmethod render-widget-body ((obj pagination) &rest args) 
  (declare (ignore args)
           (special *request-hook*))
  (when (> (pagination-page-count obj) 0)
    (with-html
      ; 'Previous' link
      (when (> (pagination-current-page obj) 1)
	(render-link (lambda (&rest args)
                       (declare (ignore args))
		       (when (> (pagination-current-page obj) 1)
			 (decf (pagination-current-page obj))
			 (pagination-call-on-change obj)))
		     (humanize-name "< Previous")
		     :class "previous-page")
	(str "&nbsp;"))
      ; 'Viewing Page X of Y'
      (:span :class "page-info"
	     (:span :class "viewing-label" "Viewing ")
	     (:span :class "page-label" "Page ")
	     (:span :class "current-page" (:strong (str (pagination-current-page obj))))
	     (:span :class "of-label" " of ")
	     (:span :class "total-pages" (str (pagination-page-count obj))))
      ; 'Next' link
      (when (< (pagination-current-page obj)
	       (pagination-page-count obj))
	(str "&nbsp;")
	(render-link (lambda (&rest args)
                       (declare (ignore args))
		       (when (< (pagination-current-page obj)
				(pagination-page-count obj))
			 (incf (pagination-current-page obj))
			 (pagination-call-on-change obj)))
		     (humanize-name "Next >")
		     :class "next-page"))
      ; Go to page
      (when (> (pagination-page-count obj) 1)
	(with-html-form (:get (curry #'pagination-on-go-to-page obj))
	  (:label (:span "Go to page:&nbsp;")
		  (:input :name "page-number"
			  :class (concatenate 'string
					      "page-number"
					      (when (slot-value obj 'last-request-error-p)
						" item-not-validated"))
			  :onfocus (concatenate 'string
						"$(this).removeClassName(\"item-not-validated\");"
						(when (/= (pagination-current-page obj) 1)
						  "if(this.value == \"1\") { this.value = \"\"; }"))
			  :onblur (when (/= (pagination-current-page obj) 1)
				    "if(this.value == \"\") { this.value = \"1\"; }")
			  :value (when (/= (pagination-current-page obj) 1)
				   "1")))
	  (render-button "go-to-page" :value "Go")))
      ; Total items
      (pagination-render-total-item-count obj))))

(defun pagination-page-item-range (obj)
  "Returns a range of items that belong to a currently selected
page. This function returns two values, the starting item
index (inclusive) and the ending item index (exclusive)."
  (values (let ((lower-bound (* (pagination-items-per-page obj)
				(1- (pagination-current-page obj)))))
	    (if (< lower-bound 0)
		0 lower-bound))
	  (min (* (pagination-items-per-page obj)
		  (pagination-current-page obj))
	       (pagination-total-items obj))))

(defun pagination-call-on-change (obj)
  "Calls on-change callback for the pagination widget."
  (multiple-value-bind (begin end)
      (pagination-page-item-range obj)
    (safe-funcall (pagination-on-change obj)
		  obj begin end)))

(defun pagination-on-go-to-page (obj &key page-number &allow-other-keys)
  "Handles client requests to go to a given page."
  (multiple-value-bind (res err)
      (ignore-errors
	(let ((page (parse-integer page-number)))
	  (assert (and (>= page 1)
		       (<= page (pagination-page-count obj))))
	  (setf (pagination-current-page obj) page)
	  (pagination-call-on-change obj)))
    (declare (ignore res))
    (when err
      (setf (slot-value obj 'last-request-error-p) t)
      (let ((on-error (pagination-on-error obj))
	    (msg (format nil "Page number must be an integer between 1 and ~A."
			 (pagination-page-count obj))))
	(typecase on-error
	  (flash (flash-message on-error msg))
	  (function (funcall on-error msg)))))))

