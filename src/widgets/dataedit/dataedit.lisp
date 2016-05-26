
(in-package :weblocks)

(export '(*default-cascade-delete-mixins-p* dataedit-mixin
          dataedit-on-add-item dataedit-allow-add-p
          show-add-form-when-empty-p
          dataedit-mixin-flash-message-on-first-add-p
          dataedit-on-delete-items dataedit-cascade-delete-mixins-p
          dataedit-on-add-item-completed
          dataedit-on-delete-items-completed dataedit-allow-delete-p
          dataedit-item-data-view dataedit-item-form-view
          dataedit-ui-state dataedit-item-widget
          dataedit-create-drilldown-widget dataedit-drilldown-action
          dataedit-create-new-item-widget dataedit-reset-state
          dataedit-add-items-flow dataedit-update-operations))

(defparameter *default-cascade-delete-mixins-p* t
  "Default value for 'dataedit-cascade-delete-mixins-p'. See its
documentation for more details.")

(defclass dataedit-mixin ()
  (;; Adding items
   (on-add-item :accessor dataedit-on-add-item
                :initform nil
                :initarg :on-add-item
                :documentation "A function called by the widget when
                an item is added. The function should accept two
                arguments (the widget object and a new item), and
                should take appropriate action.")
   (on-add-item-completed :accessor dataedit-on-add-item-completed
                          :initform nil
                          :initarg :on-add-item-completed
                          :documentation "A function called by the widget when
                an the add action is complete. The function should
                accept two arguments (the widget object and a new item).")
   (allow-add-p :accessor dataedit-allow-add-p
                :initform t
                :initarg :allow-add-p
                :documentation "If true, the widget should provide the
                UI to add entries to the collection.")
   (show-add-form-when-empty-p :accessor dataedit-show-add-form-when-empty-p
                               :initform nil
                               :initarg :show-add-form-when-empty-p
                               :documentation "If set to true, when
                               the dataseq is empty (has no data), the
                               add form is automatically presented to
                               the user (only if dataedit-allow-add-p
                               is true as well).")
   (flash-message-on-first-add-p :accessor dataedit-mixin-flash-message-on-first-add-p
                                 :initform nil
                                 :initarg :flash-message-on-first-add-p
                                 :documentation "If this field is set
                                 to nil (the default), a message about
                                 adding an item will not be flashed
                                 when the first item is added.")
   ;; Deleting items
   (on-delete-items :accessor dataedit-on-delete-items
                    :initform nil
                    :initarg :on-delete-items
                    :documentation "A function called by the widget
                    when one or more items are deleted. The function
                    should accept two arguments (the widget object and
                    a value that has semantics similar to dataseq's
                    'selection' slot). If the function is missing, the
                    should be deleted via store API.")
   (on-delete-items-completed :accessor dataedit-on-delete-items-completed
                              :initform nil
                              :initarg :on-delete-items-completed
                              :documentation "A function called by the widget
                    when the deletion of items has complted. The function
                    should accept two arguments (the widget object and
                    a value that has semantics similar to dataseq's
                    'selection' slot). ")
   (cascade-delete-mixins-p :accessor dataedit-cascade-delete-mixins-p
                            :initform *default-cascade-delete-mixins-p*
                            :initarg :cascade-delete-mixins-p
                            :documentation "If set to true, mixin
                            objects will be deleted from the store
                            when the parent object is deleted. By
                            default set to
                            '*default-cascade-delete-mixins-p*'.")
   (allow-delete-p :accessor dataedit-allow-delete-p
                   :initform t
                   :initarg :allow-delete-p
                   :documentation "If true, the widget should provide
                   the UI to delete entries in the collection.")
   ;; Views
   (item-data-view :accessor dataedit-item-data-view
                   :initform nil
                   :initarg :item-data-view
                   :documentation "An optional custom data view that,
                   if provided, is used to instantiate the
                   'item-widget'.")
   (item-form-view :accessor dataedit-item-form-view
                   :initform nil
                   :initarg :item-form-view
                   :documentation "An optional custom form view that,
                   if provided, is used to instantiate the
                   'item-widget'.")
   ;; Implementation
   (autoset-drilled-down-item-p :initform t)
   (ui-state :accessor dataedit-ui-state
             :initform nil
             :initarg :ui-state
             :documentation "A state of the dataedit control. The
             values of this slot may differ depending on concrete
             implementation of dataedit. Generally NIL means normal
             state, :add means render a widget to add a new item,
             and :drilldown means drill down on an existing item.")
   (item-widget :accessor dataedit-item-widget
                :initform nil
                :documentation "A widget used by dataedit to display a
                form for adding and editing items. This widget will be
                created and destroyed as necessary."))
  (:documentation "A mixin class for dataseq widgets that impelement
  editing functionality."))

;;; We need to set up the value of dataseq-on-drilldown
(defmethod initialize-instance :after
    ((obj dataedit-mixin) &key (on-drilldown nil on-drilldown?) &allow-other-keys)
  (declare (ignore on-drilldown))
  (unless on-drilldown?
    (setf (dataseq-on-drilldown obj)
          (cons 'modify #'dataedit-drilldown-action))))

;;; Drilldown
(defgeneric dataedit-create-drilldown-widget (obj item)
  (:documentation
   "Must instantiate and return a widget to be used to present a
drilldown of a dataedit item. Specialize this method to provide ways
to edit items in the dataedit widget."))

(defgeneric dataedit-drilldown-action (obj item)
  (:documentation
   "This callback function will be called by the dataedit when the user
attempts to drill down on a given item.")
  (:method ((obj dataedit-mixin) item)
    (setf (dataedit-item-widget obj)
          (dataedit-create-drilldown-widget obj item))
    (setf (dataedit-ui-state obj) :drilldown)))

;;; New Item
(defgeneric dataedit-create-new-item-widget (obj)
  (:documentation
   "Must instantiate and return a dataform that allows adding a new
entry. Note, in order for this functionality to work properly, the
object in question needs to have a slot named 'id' (see 'object-id'),
with an initform that assigns an ID to the object. Specialize this
method to provide different ways to add items to the dataedit."))

;;; Don't render pagination/operations if we're in the middle of
;;; adding/editing an item
(defmethod dataseq-render-operations ((obj dataedit-mixin) &rest args)
  (declare (ignore args))
  (when (null (dataedit-item-widget obj))
    (call-next-method)))

(defmethod dataseq-render-pagination-widget ((obj dataedit-mixin) &rest args)
  (declare (ignore args))
  (when (null (dataedit-item-widget obj))
    (call-next-method)))

;;; This file needs to be loaded with CMU because CMUCL doesn't
;;; compile it properly. Bytecompiler, however, works.
#+cmu (load (merge-pathnames
             (make-pathname :directory '(:relative "src" "widgets" "dataedit")
                            :name "delete-action" :type "lisp")
             (asdf-system-directory :weblocks)))

(defmethod dataedit-reset-state (obj)
  "Resets the state of the dataedit. This function should be used by
in order to reset the state after the item widget has done its job."
  (setf (dataedit-ui-state obj) nil
        (dataedit-item-widget obj) nil
        (dataseq-drilled-down-item obj) nil))

(defmethod dataedit-add-items-flow (obj sel)
  "Initializes the flow for adding items to the dataedit."
  (declare (ignore sel))
  (setf (dataedit-item-widget obj)
        (dataedit-create-new-item-widget obj))
  (setf (dataedit-ui-state obj) :add)
  (mark-dirty obj))

(defmethod dataedit-update-operations (obj &key
                                   (delete-fn #'dataedit-delete-items-flow)
                                   (add-fn #'dataedit-add-items-flow))
  "Should be used to update operations the widget supports.                                                                                                                                                                                                                     
                                                                                                                                                                                                                                                                                
'delete-fn' - a function to be called when delete action is invoked.                                                                                                                                                                                                            
'add-fn' - a function to be called when add action is invoked."
  (setf (dataseq-item-ops obj)
        (remove 'delete (dataseq-item-ops obj)
                :key #'car :test #'string-equal))
  (setf (dataseq-common-ops obj)
        (remove 'add (dataseq-common-ops obj)
                :key #'car :test #'string-equal))
  (when (and (dataedit-allow-delete-p obj)
             (> (dataseq-data-count obj) 0))                                                                                                                                                                                                                                      
    (pushnew (cons (widget-translate obj :delete-items) delete-fn)
             (dataseq-item-ops obj)
             :key #'car))
  (when (dataedit-allow-add-p obj)                                                                                                                                                                                                                                               
    (pushnew (cons (widget-translate obj :add-new-item) add-fn)
             (dataseq-common-ops obj)
             :key #'car)))

(defmethod map-subwidgets progn (function (widget dataedit-mixin))
  "Walk into the item widget if in `:add' or `:drilldown' state."
  (case (dataedit-ui-state widget)
    ((:add :drilldown)
       (funcall function (dataedit-item-widget widget)))))

;;; If the settings are right and there is no data yet, set our state
;;; to add new item immediately
(defmethod render-widget-body :before ((obj dataedit-mixin) &rest args)
  (declare (ignore args))
  (when (and (dataedit-show-add-form-when-empty-p obj)
             (dataedit-allow-add-p obj)
             (not (eq (dataedit-ui-state obj) :add))
             (not (dataedit-item-widget obj))
             (= 0 (dataseq-data-count obj)))
    (dataedit-add-items-flow obj nil)
    (setf (data-editor-form-buttons (dataedit-item-widget obj))
          '(:submit))))

(defmethod widget-translation-table append ((obj dataedit-mixin) &rest args)
  (list* 
    (cons :add-new-item "Add")
    (cons :delete-items "Delete")  
    (cons :choose-items-for-deletion-message 
          (format nil 
                  (translate "Please select ~A to delete.")
                  (translate 
                    (humanize-name (dataseq-data-class obj))
                    :plural-p t
                    :accusative-form-p t)))
    (append 
      (loop for count-keyword in (locale-number-forms (current-locale))
            collect 
            (cons 
              (concatenate-keywords :items-deleted-message- count-keyword)
              (format nil 
                      (translate 
                        "Deleted ~~A ~A."
                        :preceding-gender (determine-gender (humanize-name (dataseq-data-class obj)))
                        :preceding-count count-keyword)
                      (translate  
                        (humanize-name (dataseq-data-class obj))
                        :items-count count-keyword 
                        :accusative-form-p t))))
      (loop for count-keyword in (locale-number-forms (current-locale))
            collect 
            (cons 
              (concatenate-keywords :items-delete-question-for- count-keyword) 
              (format nil 
                      (translate "Delete ~~A ~A?")
                      (translate 
                        (humanize-name (dataseq-data-class obj))
                        :items-count count-keyword
                        :accusative-form-p t))))
      (loop for (key . val) in (widget-translation-table 'do-confirmation :confirmation-type :yes/no)
            collect (cons (concatenate-keywords :do-confirmation- key) val)))))
