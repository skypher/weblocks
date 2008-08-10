
(in-package :weblocks)

(export '(dispatcher dispatcher-on-dispatch dispatcher-cache
	  dispatcher-widgets-ephemeral-p uri-tokens-start-with))

(defwidget dispatcher (widget)
  ((on-dispatch :accessor dispatcher-on-dispatch
		:initform nil
		:initarg :on-dispatch
		:documentation "Must be bound to a function of two
		arguments - (the dispatcher object, and a list of url
		tokens). The function is expected to consume zero or
		more tokens, and return three values - a widget that
		represents a subset of the uri tokens, a list of
		consumed uri tokens, and a list of remaining uri
		tokens. If the widget cannot be instantiated, the
		function should return nil. Note, it is necessary to
		return both consumed and remaining tokens because the
		function may want to return 'implicitly' consumed
		tokens not present in the URL, (for example a url '/'
		may really indicate default url '/employees'). This is
		necessary to properly maintain
		*current-navigation-url* in nested dispatcher
		environments. Additionally, the function may return
		different remaining tokens to modify the URL on the
		fly. If the function returns a widget, the dispatcher
		uses the widget to present the URL. Otherwise,
		assummes the URL is invalid. This can be used for
		wiki-style URLs, or any kind of custom
		dispatching. Note, the returned widget will be cached
		in 'dynamic-pane-cache'. To turn off caching the
		widget, the function should return a fourth
		value :no-cache.")
   (cache :accessor dispatcher-cache
	  :initform nil
	  :documentation "If a widget is generated dynamically via
	  'on-dispatch', and :no-cache isn't returned as the fourth
	  value, the widget is cached in this slot. This is done to
	  maintain the same instance of the generated widget accross
	  non-ajax requests. This slot contains nil (if no widget is
	  cached), or a cons pair with car containing a list of tokens
	  used to generate the widget, and cdr contains the widget
	  object. ")
   (widgets-ephemeral-p :accessor dispatcher-widgets-ephemeral-p
			:initform t
			:documentation "Whether widgets should be
			considered to exist for certain only if in the
			`dispatcher-cache'.  When non-nil (the
			default), `find-widget-by-path*' will only
			follow paths through the cached widget."))
  (:documentation "The dispatcher widget can be used to map a sequence
  of URL tokens to a widget corresponding to those tokens. It can be
  used to implement any dispatch strategy of choice. Note, this
  dispatcher class is not treated in a special way by the
  framework. Similar dispatcher classes can be written without any
  framework modifications. The dispatcher is only required to modify
  *uri-tokens* as they're consumed, maintain *current-navigation-url*,
  and set *uri-tokens-fully-consumed* to true if the tokens are fully
  consumed."))

(defun uri-tokens-start-with (uri-tokens match-tokens)
  "Returns true if 'uri-tokens' start with 'match-tokens'. Returns
false otherwise."
  (or (and match-tokens
	   (list-starts-with uri-tokens match-tokens :test #'string=))
      (and (null match-tokens)
	   (null uri-tokens))))

(defun dispatcher-get-widget (obj tokens &optional (make-if-missing-p t))
  "Looks up and returns the widget in the cache based on the
tokens. If the widget is not in the cache and make-if-missing is
t (the default), calls on-dispatch to make a new one. Returns three
values - a widget, a list of consumed tokens, and a list of remaining
tokens."
  (if (and (dispatcher-cache obj)
	   (uri-tokens-start-with tokens (car (dispatcher-cache obj))))
      ;; we have the widget cached
      (values (cdr (dispatcher-cache obj))
	      (car (dispatcher-cache obj))
	      (safe-subseq tokens (length (car (dispatcher-cache obj)))))
      ;; widget not cached;
      (when make-if-missing-p
	(multiple-value-bind (widget consumed-tokens remaining-tokens caching)
	    (funcall (dispatcher-on-dispatch obj) obj tokens)
	  (when widget
	    ;; reset the parent of the old cached widget
	    (when (cdr (dispatcher-cache obj))
	      (setf (widget-parent (cdr (dispatcher-cache obj))) nil))
	    ;; clear the cache
	    (setf (dispatcher-cache obj) nil)
	    ;; if cache isn't turned off, cache the new widget
	    (when (not (eq caching :no-cache))
	      (setf (dispatcher-cache obj)
		    (cons consumed-tokens widget)))
	    ;; set the dispatcher as parent of the new widget
	    (setf (widget-parent widget) obj)
	    ;; return new widget
	    (values widget consumed-tokens remaining-tokens))))))

(defmethod render-widget-body ((obj dispatcher) &rest args)
  (declare (special *uri-tokens* *current-navigation-url*
		    *uri-tokens-fully-consumed*)
	   (ignore args))
  (multiple-value-bind (widget consumed-tokens *uri-tokens*)
      (dispatcher-get-widget obj *uri-tokens*)
    (declare (special *uri-tokens*))
    (when (null *uri-tokens*)
      (setf *uri-tokens-fully-consumed* t))
    (if widget
	(let ((*current-navigation-url* (concatenate 'string
						     (string-right-trim "/" *current-navigation-url*)
						     "/" (compose-uri-tokens-to-url consumed-tokens))))
	  (declare (special *current-navigation-url*))
	  (log-message* "CNURL: ~A" *current-navigation-url*)
	  (render-widget widget))
	(setf (return-code) +http-not-found+))))

(defmethod find-widget-by-path* (path (obj dispatcher))
  (declare (special *current-navigation-url*))
  (multiple-value-bind (widget consumed-tokens path)
      (dispatcher-get-widget obj path
			     (not (dispatcher-widgets-ephemeral-p obj)))
    (declare (ignore consumed-tokens))
    (find-widget-by-path* path widget)))

