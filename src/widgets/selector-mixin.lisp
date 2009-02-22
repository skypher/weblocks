
(in-package :weblocks)

(export '(pane-info pane-info-p make-pane-info pane-info-name
	  pane-info-uri-tokens pane-info-label selector-mixin
	  selector-mixin-panes selector-mixin-current-pane-name
	  selector-mixin-current-pane
	  selector-mixin-current-pane-widget
	  selector-mixin-default-pane
	  selector-mixin-canonicalize-pane-info
	  selector-mixin-canonicalize-pane
	  selector-mixin-find-pane-by-name
	  selector-mixin-find-pane-by-tokens))

(defstruct pane-info
  "A structure that holds information about a pane. Contains the name
  of the pane, URI tokens representing it, and a label that may be
  used to present the pane to the user. Note that if 'uri-tokens' or
  'label' are missing, they're computed by calling 'attributize-name'
  or 'humanize-name' on 'name', respectively."
  name uri-tokens label)

(defclass selector-mixin ()
  ((panes :accessor selector-mixin-panes
	  :initform nil
	  :initarg :panes
	  :documentation "An association list of pane names (or
	  pane-info structures) and widgets. When a particular name is
	  selected, the corresponding widget should be rendered by the
	  selector widget.")
   (current-pane-name :accessor selector-mixin-current-pane-name
		      :initform nil
		      :initarg :current-pane-name
		      :documentation "A name that identifies currently
                      selected entry. The selector widget should set
                      this slot every time a new widget is selected by
                      the user."))
  (:metaclass widget-class)
  (:documentation "A mixin for selectors that allows specifying a list
  of panes which comprise of pane names and corresponding widgets."))

(defgeneric selector-mixin-default-pane (obj)
  (:documentation "Returns a default pane to select in case no uri
tokens are specified. Default implementation returns the first pane.")
  (:method ((obj selector-mixin))
    (car (selector-mixin-panes obj))))

(defgeneric selector-mixin-current-pane (obj)
  (:documentation "Returns a default pane to select in case no uri
tokens are specified. Default implementation returns the first pane.")
  (:method ((obj selector-mixin))
    (find (selector-mixin-current-pane-name obj)
          (selector-mixin-panes obj)
          :key (compose #'selector-mixin-canonicalize-pane-info #'car)
          :test #'equalp)))

(defgeneric selector-mixin-current-pane-widget (obj)
  (:documentation "Returns a default pane to select in case no uri
tokens are specified. Default implementation returns the first pane.")
  (:method ((obj selector-mixin))
    (cdr (selector-mixin-current-pane obj))))

(defun selector-mixin-canonicalize-pane-info (pane-info)
  "Returns a 'canonical' representation of a pane info - a pane-info
structure (this is because car of panes may be a symbol)."
  (flet ((canonicalize (val)
	   (make-pane-info :name val
			   :uri-tokens (list (attributize-name val))
			   :label (humanize-name val))))
    (etypecase pane-info
      (pane-info pane-info)
      (symbol (canonicalize pane-info))
      (string (canonicalize pane-info)))))

(defun selector-mixin-canonicalize-pane (pane)
  "Same as selector-mixin-canonicalize-pane-info, but for convinience
operates on the dotted pair."
  (let ((pane-info (car pane))
	(widget (cdr pane)))
    (cons (selector-mixin-canonicalize-pane-info pane-info)
	  widget)))

(defun selector-mixin-find-pane-by-name (obj name)
  "Returns a canonicalized pane (cons pair as defined by 'panes' slot
of selector-mixin with car being of type pane-info) that has specified
'name'. Otherwise, returns nil."
  (let ((pane (assoc name (selector-mixin-panes obj)
		     :key (lambda (pane-info)
			    (pane-info-name 
			     (selector-mixin-canonicalize-pane-info pane-info)))
		     :test #'equalp)))
    (and pane
	 (cons (selector-mixin-canonicalize-pane-info (car pane))
	       (cdr pane)))))

(defun selector-mixin-find-pane-by-tokens (obj tokens)
  "Returns a canonicalized pane (cons pair as defined by 'panes' slot
of selector-mixin, with car being of type pane-info) that matches
specified uri tokens if such a pane exists. Otherwise, returns nil."
  (loop
     for (pane-info . widget) in (selector-mixin-panes obj)
     for c-pane-info = (selector-mixin-canonicalize-pane-info pane-info)
     for match = (ensure-list
		  (pane-info-uri-tokens c-pane-info))
     when (uri-tokens-start-with tokens match)
     do (return (cons c-pane-info widget))))

(defmethod find-widget-by-path* (path (obj selector-mixin))
  (let ((pane (cdr (selector-mixin-find-pane-by-name obj (car path)))))
    (if (and pane (cdr path))
	(find-widget-by-path* (cdr path) pane)
	pane)))

(defmethod make-widget-place-writer ((selector selector-mixin) child)
  (let ((place (find child (selector-mixin-panes selector) :key #'cdr)))
    (unless place
      (error "Widget ~S cannot be found in parent selector-mixin ~S."
	     child selector))
    (lambda (&optional (callee nil callee-supplied-p))
      (assert (find place (selector-mixin-panes selector)))
      (cond (callee-supplied-p
	     (check-type callee widget-designator
			 "a potential pane of a selector-mixin")
	     (rplacd place callee)
	     (setf (widget-parent callee) selector)
	     (mark-dirty selector))
	    (t (cdr place))))))

