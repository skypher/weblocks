
(in-package :weblocks)

(export '(selector get-widget-for-tokens selector-base-uri
	  static-selector select-pane static-selector-panes static-selector-current-pane
	  http-not-found))

(define-condition http-not-found (condition) ())

(defwidget selector ()
  ((base-uri :accessor selector-base-uri
	     :documentation "The base URI for this selector, set during
	     the tree shakedown phase before rendering. Used during
	     rendering to compute URL paths."))
  (:documentation "A selector is a widget within the tree that has a
  relation with URIs."))

(defgeneric get-widget-for-tokens (selector uri-tokens)
  (:documentation "Given a list of uri-tokens, map them to a widget. All
  selectors implement this method. There can be multiple strategies for
  mapping uri-tokens to widgets: static maps, dynamically-generated
  widgets, dynamically-generated widgets with caching. Returns a widget
  or NIL if not found, modifies uri-tokens.

  The whole tree update protocol goes like this:

  1) HANDLE-NORMAL-REQUEST calls UPDATE-WIDGET-TREE, which walks the
  tree using WALK-WIDGET-TREE starting at ROOT-WIDGET and calling
  update-children at every node.

  2) The selector's UPDATE-CHILDREN method (valid for all selectors,
  i.e. widgets that process URI tokens) calls GET-WIDGET-FOR-TOKENS.

  3) if a widget corresponding to particular URI tokens is found,
  UPDATE-CHILDREN calls UPDATE-DEPENDENTS, so that the selector (or its
  subclass) may update its dependents list and do other housekeeping.
  The default implementation of UPDATE-DEPENDENTS just calls
  (SETF WIDGET-CHILDREN) to store the children under the :SELECTOR
  type.

  Usually the only thing you'll want to do if you are implementing your
  own kind of selector is to subclass selector and provide a
  GET-WIDGET-FOR-TOKENS method for it. See class ON-DEMAND-SELECTOR for
  an example."))

(defgeneric update-dependents (selector children)
  (:documentation "Update the dependents for a given selector with
  children. A selector will usually contain the children, but there
  might be selectors that have other widgets dependent on them and need
  to do additional housekeeping. Children is either a list of widgets or
  a widget. Note that we do not update the widget-parent relations:
  those are handled by (SETF WIDGET-CHILDREN).")
  (:method ((obj selector) children)
    (setf (widget-children obj :selector) children)))

;; Functionality common to all selectors: all selectors process
;; *uri-tokens* by calling (get-widget-for-tokens) and update
;; WIDGET-CHILDREN to point to the selected widget.
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
	  key. Not providing a default item will cause a redirect to
          the first item's URI.")
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
    (cond
      ;; found pane
      (pane
        (select-pane selector (first (get-tokens uri-tokens)))
        (cdr pane))
      ;; no default pane -- redirect to the first pane's URI
      ;; FIXME: doesn't cope well with the Flash widget
      ((static-selector-panes selector)
        (redirect (concatenate 'string
                               (string-right-trim "/" (selector-base-uri selector))
                               "/"
                               (car (first (static-selector-panes selector)))))))))

(defgeneric select-pane (selector token)
  (:documentation "Called by get-widget-for-tokens when a pane is found
   and selected. Subclasses may use this method to maintain information
   about what is currently selected.")
  (:method ((obj static-selector) token)
    (setf (static-selector-current-pane obj) token)))

