
(in-package :weblocks)

(export '(dispatcher dispatcher-on-dispatch dispatcher-cache
	  dispatcher-widgets-ephemeral-p uri-tokens-start-with))

(defwidget dispatcher (container)
  ((on-dispatch :accessor dispatcher-on-dispatch :initarg :on-dispatch :initform nil)
   (cache :accessor dispatcher-cache :initform nil)
   (widgets-ephemeral-p :accessor dispatcher-widgets-ephemeral-p
			:initform t
			:initarg :widgets-ephemeral-p
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

(defmethod (setf dispatcher-cache) :after (value (d dispatcher))
  (setf (container-children d) (list (cdr value))))

(defun uri-tokens-start-with (uri-tokens match-tokens)
  "Returns true if 'uri-tokens' start with 'match-tokens'. Returns
false otherwise."
  (or (and match-tokens
	   (list-starts-with uri-tokens match-tokens :test #'string=))
      (and (null match-tokens)
	   (null uri-tokens))))

(defun dispatcher-get-widget (obj tokens &optional (make-if-missing-p t) (mutate-cache t))
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
      ;; widget not cached
      (when make-if-missing-p
	(multiple-value-bind (widget consumed-tokens remaining-tokens caching)
	    (funcall (dispatcher-on-dispatch obj) obj tokens)
	  (unless widget
	    ;; clear the cache
	    (setf (dispatcher-cache obj) nil))	 
	  (when widget
	    (when (and mutate-cache (not (eq caching :no-cache)))
	      ;; reset the parent of the old cached widget
	      (when (cdr (dispatcher-cache obj))
		(setf (widget-parent (cdr (dispatcher-cache obj))) nil))
	      ;; replace cache with new widget
	      (setf (dispatcher-cache obj)
		    (cons consumed-tokens widget)))
	    ;; set the dispatcher as parent of the new widget
	    (setf (widget-parent widget) obj)
	    ;; return new widget
	    (values widget consumed-tokens remaining-tokens))))))

(defmethod render-widget :around ((widget widget) &rest args)
  "Backwards compatibility function."
  (let ((*current-navigation-url* (widget-current-navigation-url widget)))
    (declare (special *current-navigation-url*))
    (call-next-method)))

#|
(defmethod find-widget-by-path* (path (obj dispatcher))
  (declare (special *current-navigation-url*))
  (multiple-value-bind (widget consumed-tokens path)
      (dispatcher-get-widget obj path
			     (not (dispatcher-widgets-ephemeral-p obj)) nil)
    (declare (ignore consumed-tokens))
    (find-widget-by-path* path widget)))
|#

(define-condition http-not-found (condition) ())

(defmethod container-update-direct-children ((cont dispatcher))
   "Looks up and returns the widget in the cache based on the
 tokens. If the widget is not in the cache and make-if-missing is
 t (the default), calls on-dispatch to make a new one. Returns three
 values - a widget, a list of consumed tokens, and a list of remaining
 tokens."
  (declare (special *uri-tokens* *current-navigation-url*
		    *uri-tokens-fully-consumed*)
	   (ignore args))
  (log-message :debug "update-direct-children: tokens = ~A~%" *uri-tokens*)
  ;;(setf (widget-current-navigation-url cont) *current-navigation-url*)
  (multiple-value-bind (widget consumed-tokens remaining-tokens)
      (dispatcher-get-widget cont *uri-tokens*)
    (setf *uri-tokens* remaining-tokens) 
    (when (null *uri-tokens*)
      (setf *uri-tokens-fully-consumed* t))
    (if widget
	(let ((*current-navigation-url* (remove-spurious-slashes
                                          (maybe-add-trailing-slash
                                            (concatenate 'string
                                                         "/" (widget-current-navigation-url cont)
                                                         "/" (compose-uri-tokens-to-url consumed-tokens))))))
	  (log-message* "CNURL: ~A" *current-navigation-url*)
          (setf (widget-current-navigation-url widget) *current-navigation-url*)
          widget)
        (assert (signal 'http-not-found)))))

