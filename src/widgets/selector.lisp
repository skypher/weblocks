
(in-package :weblocks)

(export '(selector get-widget-for-tokens selector-base-uri
	  static-selector select-pane static-selector-panes static-selector-current-pane
	  http-not-found))

(define-condition http-not-found (condition) ())

(defwidget selector (container)
  ((base-uri :accessor selector-base-uri
	     :documentation "The base URI for this selector, set during
	     the tree shakedown phase, before rendering. Used during
	     rendering to compute URL paths."))
  (:documentation "A selector is a widget within the tree that has a
  relation with URIs."))

(defgeneric get-widget-for-tokens (selector uri-tokens)
  (:documentation "Given a list of uri-tokens, map them to a widget. All
  selectors implement this method. There can be multiple strategies for
  mapping uri-tokens to widgets: static maps, dynamically-generated
  widgets, dynamically-generated widgets with caching. Returns a widget
  or NIL if not found, modifies uri-tokens."))

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
;; *uri-tokens* by calling (get-widget-for-tokens) and update
;; widget-children to point to the selected widget.
(defmethod container-update-direct-children ((selector selector))
  (declare (special *uri-tokens*))
  (setf (selector-base-uri selector)
	(make-webapp-uri
	 (string-left-trim
	  "/" (string-right-trim
	       "/" (compose-uri-tokens-to-url (consumed *uri-tokens*))))))
  (let ((widget (get-widget-for-tokens selector *uri-tokens*)))
    (if widget
	(update-dependents selector widget)
        (assert (signal 'http-not-found)))))


;; Functionality common to all selectors: all selectors process
;; *uri-tokens* by calling (get-widget-for-tokens) and update
;; widget-children to point to the selected widget.
(defmethod update-children ((selector selector))
  (declare (special *uri-tokens*))
  (setf (selector-base-uri selector)
	(make-webapp-uri
	 (string-left-trim
	  "/" (string-right-trim
	       "/" (compose-uri-tokens-to-url (consumed *uri-tokens*))))))
  (let ((widget (get-widget-for-tokens selector *uri-tokens*)))
    (if widget
	(update-dependents selector widget)
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
  ;; we peek at the token first, because if it isn't found we won't
  ;; consume it, to give others a chance to process it
  (let* ((token (peek-at-token uri-tokens))
	 (pane (assoc token (static-selector-panes selector) :test #'equalp)))
    (when pane
      (select-pane selector (first (get-tokens uri-tokens)))
      (cdr pane))))


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




