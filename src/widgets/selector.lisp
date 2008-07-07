
(in-package :weblocks)

(export '(selector selector-on-dispatch))

(defwidget selector (dispatcher selector-mixin)
  ((on-dispatch :initform 'selector-on-dispatch))
  (:documentation "A selector is a convinience widget based on the
  dispatcher widget and selector-mixin interface. It allows setting a
  list of widgets declaratively. Every time the selector is rendered,
  it dispatches to one of these widgets based on the URL. When the
  list of widgets is known, selector makes implementing 'on-dispatch'
  unnecessary. Note that the panes are searched in the order in which
  they were declared. If there are panes with multiple tokens and
  panes with single tokens that share the first token name, be sure to
  place panes with multiple tokens first."))

(defgeneric selector-on-dispatch (obj tokens)
  (:documentation "This function implements dispatcher's 'on-dispatch'
  for the selector. It selects from one of the widgets exposed by
  selector-mixin.")
  (:method ((obj selector) tokens)
    (let* ((pane (if tokens
		     (selector-mixin-find-pane-by-tokens obj tokens)
		     (selector-mixin-canonicalize-pane (selector-mixin-default-pane obj))))
	   (pane-tokens (ensure-list (pane-info-uri-tokens (car pane)))))
      (if pane
	  (progn
	    (setf (selector-mixin-current-pane-name obj)
		  (pane-info-name (car pane)))
	    (values (cdr pane)
		    pane-tokens
		    (safe-subseq tokens (length pane-tokens))))
	  (progn
	    (setf (selector-mixin-current-pane-name obj) nil)
	    nil)))))

