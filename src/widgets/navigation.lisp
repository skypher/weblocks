
(in-package :weblocks)

(export '(navigation render-navigation-menu init-navigation make-navigation
          navigation-disabled-pane-names))

(defwidget navigation (static-selector)
  ((pane-names :accessor navigation-pane-names
	       :initarg :pane-names
	       :initform nil
	       :documentation "An alist mapping url-tokens to
	       human-readable pane names (rendered as a menu). Use nil
	       as the key for the default item.")
   (disabled-pane-names :initform nil
                        :initarg :disabled-pane-names
                        :accessor navigation-disabled-pane-names
                        :documentation "Allows presenting panes to the
                        user as a visual queue, but disabling access
                        to them. If not null, this slot should be
                        bound to a list of pane names to be
                        disabled."))
  (:documentation "The navigation widget can act as a menu controls, a
  tabbed control, etc. It is a convenience combination of the
  static-selector widget and a menu snippet."))

(defun navigation-pane-name-for-token (navigation token)
  "Return the pane name for a given uri-token or NIL if not found. Token
may be NIL in which case the default pane name is provided."
  (cdr (assoc token (navigation-pane-names navigation) :test #'equalp)))

(defgeneric render-navigation-menu (obj &rest args)
  (:documentation "Renders the HTML menu for the navigation widget.")
  (:method ((obj navigation) &rest args &key menu-args &allow-other-keys)
    (declare (ignore args))
    (apply #'render-menu (mapcar (lambda (pane)
                                   (cons (navigation-pane-name-for-token obj (car pane))
                                         (compose-uri-tokens-to-url (car pane))))
                                 (static-selector-panes obj))
           :selected-pane (static-selector-current-pane obj)
           :header (if (widget-name obj)
                     (humanize-name (widget-name obj))
                     "Navigation")
           :container-id (ensure-dom-id obj)
           :empty-message "No navigation entries"
           :disabled-pane-names (navigation-disabled-pane-names obj)
           menu-args)))

(defmethod render-widget-body ((obj navigation) &rest args)
  (let ((saved-panes (selector-mixin-panes obj)))
    ;; Remove disabled panes
    (when (navigation-disabled-pane-names obj)
      (setf (selector-mixin-panes obj) 
            (remove-if (lambda (item)
                         (member (car item) (navigation-disabled-pane-names obj)
                                 :test #'string-equal))
                       saved-panes)))
    ;; Render navigation
    (let ((navigation-body (with-html-to-string
                             (:div :class "navigation-body"
                                   (mapc #'render-widget (widget-children obj))))))
      ;; Restore disabled panes before menu is rendered
      (setf (selector-mixin-panes obj) saved-panes)
      ;; Render menu
      (apply #'render-navigation-menu obj args)
      (format *weblocks-output-stream* "~A" navigation-body))))

(defmethod per-class-dependencies append ((obj navigation))
  (list (make-local-dependency :stylesheet "menu")))

(defun init-navigation (obj &rest args)
  "A helper function to create a navigation widget."
  (mapc (lambda (pane-info)
          (let ((token (or (third pane-info) (attributize-name (first pane-info))))
                (name (first pane-info))
                (widget (second pane-info)))
            (when (string-equal token "")
              (setf token nil))
            (push-end (cons token name) (navigation-pane-names obj))
            (push-end (cons token widget) (static-selector-panes obj))))
        args)
  obj)

(defun make-navigation (name &rest args)
  "Instantiates the default navigation widget via 'make-instance'
and forwards it along with 'args' to 'init-navigation'.

The navigation widgets bears the title NAME."
  (let ((nav (make-instance 'navigation :name name)))
    (apply #'init-navigation nav args)
    nav))

