
(in-package :weblocks)

(export '(selector static-selector get-widget-for-tokens select-pane http-not-found))

(defgeneric get-widget-for-tokens (selector uri-tokens)
  (:documentation "Given a list of uri-tokens, map them to a widget. All
  selectors implement this method. There can be multiple strategies for
  mapping uri-tokens to widgets: static maps, dynamically-generated
  widgets, dynamically-generated widgets with caching. Returns three
  values -- a widget, a list of consumed tokens, and a list of remaining
  tokens."))

(defwidget selector (container)
  ())

(define-condition http-not-found (condition) ())

(defgeneric update-dependents (selector children)
  (:documentation "Update the dependents for a given selector with
  children. A selector will usually contain the children, but there
  might be selectors that have other widgets dependent on them. Children
  is either a list of widgets or a widget. Note that we do not update
  the widget-parent relations: those are updated by the containers for
  all their children.")
  (:method ((obj selector) children)
    (setf (widget-children obj) children)
    (mapc (lambda (child) (setf (widget-parent child) obj)) (ensure-list children))))

;; Functionality common to all selectors: all selectors process
;; *uri-tokens* by calling (get-widget-for-tokens), update *uri-tokens*
;; and *uri-tokens-fully-consumed*, set the widget-uri-path of the
;; selected widget and update widget-children to point to the selected
;; widget.
(defmethod container-update-direct-children ((selector selector))
  (declare (special *uri-tokens* *uri-tokens-fully-consumed*))
  (multiple-value-bind (widget consumed-tokens remaining-tokens)
      (get-widget-for-tokens selector *uri-tokens*)
    (setf *uri-tokens* remaining-tokens) 
    (when (null *uri-tokens*)
      (setf *uri-tokens-fully-consumed* t))
    (if widget
	(let ((*current-navigation-url* (remove-spurious-slashes
                                          (maybe-add-trailing-slash
                                            (concatenate 'string
                                                         "/" (widget-uri-path selector)
                                                         "/" (compose-uri-tokens-to-url consumed-tokens))))))
	  (declare (special *current-navigation-url*))
          (setf (widget-uri-path widget) *current-navigation-url*)
	  (update-dependents selector widget))
        (assert (signal 'http-not-found)))))


(defwidget static-selector (selector)
  ((panes :accessor static-selector-panes :initarg :panes :initform nil
	  :documentation "An alist mapping uri-tokens (strings) to
	  widgets. The default item (widget) should have nil as the
	  key.")
   (current-pane :accessor static-selector-current-pane :initform nil
		 :documentation "The uri-tokens corresponding to the
		 currently selected pane, or an empty string if the
		 default pane is selected."))
  (:documentation "A static-selector implements a static mapping from a
  single uri-token to a list of widgets, where only one widget can be
  selected at any given time. This forms the base for most static
  navigation systems."))

(defmethod get-widget-for-tokens ((selector static-selector) uri-tokens)
  (let* ((token (first uri-tokens))
	 (pane (assoc token (static-selector-panes selector) :test #'equalp)))
    (if pane
	(progn
	  (select-pane selector token)
	  (values (cdr pane) token (rest uri-tokens)))
	(values nil nil uri-tokens))))


(defgeneric select-pane (selector token)
  (:documentation "Called by get-widget-for-tokens when a pane is found
   and selected. Subclasses may use this method to maintain information
   about what is currently selected.")
  (:method ((obj static-selector) token)
    (setf (static-selector-current-pane obj) token)))


(defmethod make-widget-place-writer ((selector static-selector) child)
  (let ((place (find child (static-selector-panes selector) :key #'cdr)))
    (unless place
      (error "Widget ~S cannot be found in parent static-selector ~S."
	     child selector))
    (lambda (&optional (callee nil callee-supplied-p))
      (assert (find place (static-selector-panes selector)))
      (cond (callee-supplied-p
	     (check-type callee widget-designator
			 "a potential pane of a static-selector")
	     (rplacd place callee)
	     (setf (widget-parent callee) selector)
	     (mark-dirty selector))
	    (t (cdr place))))))




