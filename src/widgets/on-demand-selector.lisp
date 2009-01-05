(in-package :weblocks)

(export '(on-demand-selector on-demand-lookup-function on-demand-make-if-missing
	  on-demand-mutate-cache uri-tokens-start-with))

(defwidget on-demand-selector (selector)
  ((lookup-function :accessor on-demand-lookup-function
		    :initarg :lookup-function
		    :initform nil
		    :documentation "Lookup function should return
		    multiple values: the created widget, consumed
		    tokens, remaining tokens and an optional
		    fourth :no-cache value, indicating that the result
		    is not to be cached.")
   (make-if-missing :accessor on-demand-make-if-missing
		    :initarg :make-if-missing-p
		    :initform t)
   (mutate-cache :accessor on-demand-mutate-cache
		 :initarg :mutate-cache-p
		 :initform t)
   (cache :accessor on-demand-selector-cache :initform nil))
  (:documentation "This selector implements the dynamic wiki-style
  content creation. When provided with a lookup-function, creates and
  caches content based on url-tokens. Lookup function should return
  multiple values: the created widget, consumed tokens, remaining tokens
  and an optional fourth :no-cache value, indicating that the result is
  not to be cached."))

(defun uri-tokens-start-with (uri-tokens match-tokens)
  "Returns true if 'uri-tokens' start with 'match-tokens'. Returns
false otherwise."
  (or (and match-tokens
	   (list-starts-with uri-tokens match-tokens :test #'string=))
      (and (null match-tokens)
	   (null uri-tokens))))

(defmethod get-widget-for-tokens ((obj on-demand-selector) tokens)
  "Looks up and returns the widget in the cache based on the
tokens. If the widget is not in the cache and make-if-missing is
t (the default), calls the lookup-function to make a new one. Returns three
values -- a widget, a list of consumed tokens, and a list of remaining
tokens."
  (if (and (on-demand-selector-cache obj)
	   (uri-tokens-start-with (remaining tokens) (car (on-demand-selector-cache obj))))
      (progn
	;; we have the widget cached, consume the right amount of tokens
	(get-tokens tokens (length (car (on-demand-selector-cache obj))))
	;; ...and return the cached widget
	(cdr (on-demand-selector-cache obj)))
      ;; widget not cached
      (when (on-demand-make-if-missing obj)
	(multiple-value-bind (widget consumed-tokens remaining-tokens caching)
	    (funcall (on-demand-lookup-function obj) obj (remaining tokens))
	  ;; discard the tokens we have decided to consume
	  (get-tokens tokens (length consumed-tokens))
	  (unless widget
	    ;; clear the cache
	    (setf (on-demand-selector-cache obj) nil))
	  (when widget
	    (when (and (on-demand-mutate-cache obj) (not (eq caching :no-cache)))
	      ;; reset the parent of the old cached widget
	      ;; FIXME: I think we do this elsewhere, so this isn't needed --jwr
	      (when (cdr (on-demand-selector-cache obj))
		(setf (widget-parent (cdr (on-demand-selector-cache obj))) nil))
	      ;; replace cache with new widget
	      (setf (on-demand-selector-cache obj)
		    (cons consumed-tokens widget)))
	    ;; return the new widget
	    widget)))))


;; Note there is no render-widget-body nor render-widget-children method
;; for on-demand-selector. That's because the default from widget suit
;; us just fine. --jwr

