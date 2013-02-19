
(in-package :weblocks)

(export '(selector
          get-widget-for-tokens
          selector-base-uri
          static-selector
          static-selector-select-pane
          static-selector-panes
          static-selector-get-pane
          static-selector-cached-panes
          static-selector-current-pane
	  http-not-found))

(define-condition http-not-found (condition) ())

(defwidget selector ()
  ((base-uri :accessor selector-base-uri
             :initform nil
	     :documentation "The base URI for this selector, set during
	     the tree shakedown phase before rendering. Used during
	     rendering to compute URL paths."))
  (:documentation "A selector is a widget within the tree that has a
  relation with URIs."))

(defgeneric get-widget-for-tokens (selector uri-tokens)
  (:documentation "Given a list of URI tokens, map them to a widget. All
  selectors implement this method. There can be multiple strategies for
  mapping URI tokens to widgets: static maps, dynamically-generated
  widgets, dynamically-generated widgets with caching. Returns a widget
  or NIL if not found. Modifies URI-TOKENS.

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
	       "/" (uri-tokens-to-string (consumed-tokens *uri-tokens*))))))
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
   (cached-panes :accessor static-selector-cached-panes
                 :initform nil
                 :documentation "Remember the state of the statically
                 selected pane here.")
   (current-pane :accessor static-selector-current-pane :initform nil
		 :documentation "The uri-tokens corresponding to the
		 currently selected pane, or an empty string if the
		 default pane is selected."))
  (:documentation "A static-selector implements a static mapping from a
  single uri-token to a list of widgets, where only one widget can be
  selected at any given time. This forms the base for most static
  navigation systems."))

(defmethod make-widget-place-writer :around ((selector static-selector) widget)
  "Make sure the pane cache is up to date."
  (let ((place-writer (call-next-method)))
    (lambda (&optional (callee nil callee-supplied-p))
      (let ((result (apply place-writer (when callee-supplied-p (list callee)))))
        (when callee
          ;; replace all occurrences of WIDGET in the cache;
          ;; this is a stab at sensible semantics that might
          ;; be changed or amended later.
          (setf (static-selector-cached-panes selector)
                (mapcar (lambda (pane)
                          (cons (car pane)
                                (if (eq (cdr pane) widget)
                                  callee
                                  (cdr pane))))
                        (static-selector-cached-panes selector))))
        result))))

(defgeneric static-selector-get-pane (selector token)
  (:documentation "Get the dotted pair (pane-name . pane-content)
  associated with TOKEN from the list of panes. This function is
  called by static-selector's get-widget-for-tokens. The default
  implementation just returns the pair found by ASSOC."))

(defmethod static-selector-get-pane ((selector static-selector) token)
  (assoc token (static-selector-panes selector) :test #'equalp))

(defmethod get-widget-for-tokens ((selector static-selector) uri-tokens)
  ;;
  ;; Note that here we are carefully managing the uri-tokens instance.
  ;; The uri-tokens keep track of what we have traversed upto now and
  ;; this method will remove (modify) tokens that are used when making the selection.
  ;;
  ;; The behaviour is slightly tricky because we handle a few cases.
  ;; 
  ;; 1 - the token to be consumed (token) is indeed selecting a widget.
  ;;     This is the normal case, and we pop the token from the uri-tokens
  ;;     and return the selected pane.
  ;;     
  ;; 2 - There are no tokens left anymore.
  ;;     In that case we select and return the default pane.
  ;;     now the default pane comes into 2 flavours:
  ;;       2a. explicitly associated with the nil token
  ;;       2b. The first pane available if there is pane associated with the
  ;;           default token.
  ;;     In both case we return the default pane, but in case 2a.
  ;;     we add the token to the consumed list.
  ;;     The reason that this is needed is for nested selectors.
  ;;     Because the child selectors need to have the selector token
  ;;     in their base path, otherwise selecting a sub navigationwidget
  ;;     will create the wrong path (it is missing the selection token
  ;;     for this SELECTOR.
  ;;     
  ;;
  ;; 3 - There are tokens left, but the token does not select a pane AND
  ;;     there is a default pane associated with NIL.
  ;;
  ;;     In that case it should return the child pane.
  ;;
  ;;
  ;; Upshot of this all is that this code will, if possible,
  ;;
  ;;  1 - Return the selected pane.
  ;;  2 - Remove the token used to select this pane, if a token was used.
  ;;  3 - Make sure that if the selected pane has a token associated to it, it
  ;;      is added to the consumed token list.
  ;;
  (let* ((token (peek-at-token uri-tokens))
         (panes (static-selector-panes selector))
         (cached-panes (static-selector-cached-panes selector))
	 (pane (static-selector-get-pane selector token))
         (cached-pane (assoc token cached-panes :test #'equalp))
         (effective-pane (or cached-pane pane))
         (selected-pane (cond
                          ((cdr effective-pane)
			   (when (car effective-pane)
			     (pop-tokens uri-tokens))
                           effective-pane)
                          (panes
			   (let ((default-pane (or (static-selector-get-pane selector nil) (first panes))))
			     (when (car default-pane)
			       (consume-tokens uri-tokens (list (car default-pane))))
			     default-pane)))))
    (unless cached-pane ; already in cache? add if not
      (push (cons (car selected-pane) (cdr selected-pane)) (static-selector-cached-panes selector)))
    (static-selector-select-pane selector (car selected-pane))
    (cdr selected-pane)))

(defgeneric static-selector-select-pane (selector token)
  (:documentation "Called by GET-WIDGET-FOR-TOKENS when a pane is found
   and selected. Subclasses may use this method to maintain information
   about what is currently selected.")
  (:method ((obj static-selector) token)
    (setf (static-selector-current-pane obj) token)))

