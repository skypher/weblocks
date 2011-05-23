;;
;; TODO
;;   dataform
;;     mixins
;;   presentation adapters
;;   presentations as part of field widgets
;;

(in-package :weblocks)

(load (merge-pathnames "define-class.lisp" *load-truename*))
(import 'gbbopen-tools-define-class:define-class)

(export '(define-widget
          humanize
          serialize-for-html

          form-widget

          setup-fields
          caption-of
          instructions-of
          form-id-of
          focusp-of
          fields-of
          state-of
          validators-of
          on-success-of
          reset-form-widget
          render-confirmation
          render-form-controls
          find-field-widget-by-name
          form-value
	  with-form-values
          handle-form-submission

          field-widget
          parser-of
          hidep-of
          requiredp-of
          name-of
          label-of
          form-of
          intermediate-value-of
          pretty-intermediate-value-of
          parsed-value-of
          error-message-of
          error-messages-of
          render-field
          render-field-contents
          update-form-field-value-from-request
          update-field-widgets-parent

          string-field-widget

          dropdown-field-widget
          choices-of

          funcall-field-widget

          field-presentation->field-widget-class
          field-parser->field-widget-parser
          field-widget-from-field-info
          form-widget-initialize-from-view

          dataform2))

(defmacro define-widget (name direct-superclasses &body body)
  "Synthesis of `weblocks:defwidget' and `gbbopen-tools:define-class'."
  `(progn
     (define-class ,name ,(remove-duplicates
                            (append (or direct-superclasses '(widget))
                                    (when (uri-parameter-def-p (car body))
                                      (list 'uri-parameters-mixin))))
       ,@body
       (:metaclass widget-class))
     (defmethod per-class-dependencies append ((obj ,name))
       (declare (ignore obj))
       (dependencies-by-symbol (quote ,name)))
     ,@(awhen (maybe-generate-parameter-slot-map-fn name (car body))
         (list it))))

(define-widget form-widget ()
  ((caption :type (or string function null) :initform nil)
   (instructions :type (or string function null) :initform nil)
   (form-id :initform (gen-id))
   (focusp :type boolean :initform t)
   (validators :initform nil)
   (error-messages :type list :initform nil)
   (state :type t :initform :form
          :documentation "State of this form. Out of the box the supported values
          are :form and :confirmation. You may define others on your own as needed.")
   (on-success :initform '(:confirm :reset)
               :documentation "A function or symbol (or list of those) to specify
               what happens after a successful form submit.")
   (use-ajax-p :initform t)))

(defmethod fields-of ((widget form-widget))
  (widget-children widget))

(defmethod (setf fields-of) (fields (widget form-widget))
  ;; TODO some additional sanity checks would be nice.
  (setf (widget-children widget) fields))

(defmethod (setf widget-children) :after (fields (widget form-widget) &optional type)
  (declare (ignore fields type))
  (update-field-widgets-parent widget))

(defmethod setup-fields ((widget form-widget))
  nil)

(defmethod initialize-instance :after ((widget form-widget) &rest initargs)
  (declare (ignore initargs))
  (setup-fields widget)
  (update-field-widgets-parent widget))

(defmethod find-field-widget-by-name ((widget form-widget) name &key (errorp t))
  (or (find name (widget-children widget) :key #'name-of :test #'string-equal)
      (when errorp (error "Couldn't find field named ~S in form ~S" name widget))))

(defmethod form-value ((widget form-widget) field-name)
  (parsed-value-of (find-field-widget-by-name widget field-name)))

(defmacro with-form-values ((&rest names) form &body body)
  `(let ,(loop for name in names
	       collect `(,name (form-value ,form ',name)))
     ,@body))

(defmethod render-confirmation ((widget form-widget))
  (with-html
    (:p "Your data has been submitted.")
    (:dl
      (mapcar (lambda (field)
                (htm
                  (:dt (esc (label-of field)))
                  (:dd (esc (pretty-intermediate-value-of field)))))
              (widget-children widget)))))

(defmethod reset-form-widget ((widget form-widget))
  ;; This can get complicated when fields are modified
  ;; dynamically. In that case specialize this method.
  (setf (state-of widget) :form)
  (setup-fields widget))

(defmethod render-widget-body ((widget form-widget) &rest args)
  (declare (ignore args))
  (when (eq (state-of widget) :confirmation)
    (render-confirmation widget)))

(defmethod dom-classes ((widget form-widget))
  (concatenate 'string (call-next-method)
               " "
               (string-downcase (symbol-name (state-of widget)))))

(defmethod form-widget-act-on-success-item ((widget form-widget)
                                            (success-item (eql :confirm)))
  (setf (state-of widget) :confirmation))

(defmethod form-widget-act-on-success-item ((widget form-widget)
                                            (success-item (eql :reset)))
  (push (lambda ()
          (reset-form-widget widget))
        (request-hook :request :post-render)))

(defmethod update-field-widgets-parent ((widget form-widget))
  (mapc (lambda (field) (setf (form-of field) widget)) (fields-of widget)))

(defmethod render-form-controls ((widget form-widget))
  (with-html
    (:input :type "submit" :value "Submit")))

(defmethod handle-form-submission ((widget form-widget) &rest args)
  (declare (ignorable args))
  #+leslie(format t "submit with args: ~S~%" args)
  (let* ((fields (fields-of widget))
         (field-results (mapcar (lambda (field)
                                  (multiple-value-list
                                    (update-form-field-value-from-request widget field)))
                                fields)))
    #+leslie(format t "field results: ~S~%" field-results)
    (when (every #'car field-results) ; every form field successfully updated?
      ;; now call form validators, if any
      (let ((form-results (mapcar (lambda (validator)
                                    (multiple-value-list
                                      (funcall validator widget)))
                                  (ensure-list
                                    (validators-of widget)))))
        #+leslie(format t "form results: ~S~%" form-results)
        (if (or (null form-results)
                (every #'car form-results))
          (mapcar (lambda (success-item)
                    (etypecase success-item
                      (keyword
                        (form-widget-act-on-success-item widget success-item))
                      ((or symbol function)
                       (funcall success-item widget))))
                  (ensure-list
                    (on-success-of widget)))
          (setf (error-messages-of widget) (mapcar #'cadr form-results)))))))

(defmethod render-widget-children ((widget form-widget) &rest args)
  (declare (ignore args))
  (when (eq (state-of widget) :form)
    ;; caption and instructions
    (dolist (item (list (caption-of widget) (instructions-of widget)))
      (etypecase item
          (string (with-html
                    (:div :class "item" (esc item))))
          ((or function symbol)
           (safe-funcall item widget))))
    ;; error messages
    (with-html
      (:ul :class "errors"
        (dolist (error-message (error-messages-of widget))
          (htm
            (:li (esc error-message))))))
    ;; fields
    (let ((fields (widget-children widget)))
      (with-html-form (:POST (lambda (&rest args)
                               (apply #'handle-form-submission widget args))
                       :id (form-id-of widget)
                       :use-ajax-p (use-ajax-p-of widget))
        (:div :class "fields"
          (mapc #'render-widget fields))
        (:div :class "controls"
          (render-form-controls widget))))))

(defmethod render-widget-children :after ((widget form-widget) &rest args)
  (declare (ignore args))
  (when (focusp-of widget)
    (send-script
      `((@ ($ ,(form-id-of widget)) focus-first-element)))))


(define-class field-widget-onchange-mixin () ; XXX not used yet
  ((onchange :type function)))

(define-widget field-widget ()
  ((parser :type function)
   (validators :type (or atom list) :initform nil)
   (hidep :type boolean :initform nil)
   (requiredp :type boolean :initform nil)

   (name :type (or string null) :initform nil)
   (label :type (or string null) :initform nil)

   (form :type form-widget)
   (intermediate-value :initform nil)
   (parsed-value :initform nil)
   (error-message :type (or string null) :initform nil)))

(defmethod initialize-instance :after ((widget field-widget) &rest initargs)
  (unless (getf initargs :name)
    (setf (name-of widget) (aif (label-of widget)
                             (attributize-name it)
                             (gen-id)))))

(defmethod pretty-intermediate-value-of ((widget field-widget))
  (or (intermediate-value-of widget) "-"))

(defmethod render-widget-body ((field field-widget) &rest args)
  (declare (ignore args))
  (render-field (form-of field) field))

(defmethod render-field ((form form-widget) (field field-widget))
  (with-html
    (:div :class "label" ; TODO (:label :for id)
      (esc (label-of field))
      (when (requiredp-of field)
        (htm (esc "*")))
      (awhen (error-message-of field)
        (htm
          " "
          (:span :class "error-message"
            (esc it)))))
    (:div :class "contents"
      (render-field-contents form field))))

(defmethod update-form-field-value-from-request ((form form-widget) (field field-widget))
  ;(format t "POST parameters: ~S~%" (tbnl::post-parameters*))
  (let* ((raw-value (hunchentoot:post-parameter (name-of field)))
         (requiredp (requiredp-of field))
         (empty (equal (string-trim " " raw-value) ""))) ; FIXME other whitespace
    (setf (intermediate-value-of field) (unless empty raw-value))
    (cond
      ((and raw-value (not empty)) ; present, parse it
       (multiple-value-bind (parsed-successfully-p parsed-value-or-error-message)
           (funcall (parser-of field) raw-value)
         #+leslie(format t "parser ~S returned ~S~%" (parser-of field) parsed-value-or-error-message)
         (if parsed-successfully-p
           (let ((validation-errors (mapcar #'cadr
                                            (remove-if #'identity ; remove passed validator results
                                                       (mapcar (lambda (v)
                                                                 (multiple-value-list
                                                                   (funcall v parsed-value-or-error-message)))
                                                               (ensure-list (validators-of field)))
                                                       :key #'car))))
             #+leslie(format t "validation errors: ~S~%" validation-errors)
             (if validation-errors
               (values nil (setf (error-message-of field) (first validation-errors)))
               (values t (setf (parsed-value-of field) parsed-value-or-error-message
                               (error-message-of field) nil))))
           (values nil (setf (error-message-of field) parsed-value-or-error-message)))))
      (requiredp
       (values nil (setf (error-message-of field) "Field is required")))
      (t
       (values t (setf (parsed-value-of field) nil
                       (error-message-of field) nil))))))

;; text field
(define-widget string-field-widget (field-widget)
  ((style :type (member :input :password :textarea) :initform :input))
  (:default-initargs :parser (lambda (raw-value)
                               (values t raw-value))))

(defmethod render-field-contents ((form form-widget) (field string-field-widget) &key id &allow-other-keys)
  (with-html
    (let ((style (style-of field))
          (intermediate-value (intermediate-value-of field)))
      (if (eq style :textarea)
        (htm
          (:textarea :id id :name (name-of field)
                     :class (if (error-message-of field) "invalid" "valid")
            (esc intermediate-value)))
        (htm
          (:input :id id
                  :class (concatenate 'string "text "
                                      (when (eq style :password) "password ")
                                      (if (error-message-of field) "invalid" "valid"))
                  :type (if (eq style :password) "password" "text")
                  :name (name-of field)
                  :value (esc (intermediate-value-of field))))))))

;; dropdown/radio
(define-widget dropdown-field-widget (field-widget)
  ((style :type (member :dropdown :radio) :initform :dropdown)
   (choices :type (or function (and symbol (not keyword)) list)
            :initform nil)
   (welcome-name :type (or string null) :initform nil))
  (:default-initargs :parser (lambda (raw-value)
                               (values t raw-value))))

(defmethod humanize ((obj string))
  (humanize-name obj))

(defmethod serialize-for-html ((obj string))
  (attributize-name obj))

(defmethod choices-of :around ((widget dropdown-field-widget))
  (let ((choices (call-next-method)))
    (mapcar (lambda (choice)
              (etypecase choice
                (atom (cons choice (serialize-for-html choice)))
                (cons choice)))
            (etypecase choices
              (list choices)
              (function (funcall choices))))))

(defmethod pretty-intermediate-value-of ((widget dropdown-field-widget))
  (aif (intermediate-value-of widget)
    (let* ((choices (choices-of widget))
           (choice (rassoc it choices :test #'equalp)))
      (car choice))
    "-"))

(defmethod render-field-contents ((form form-widget) (widget dropdown-field-widget) &key id &allow-other-keys)
  (let ((choices (choices-of widget)))
    (ecase (style-of widget)
      (:dropdown
        (render-dropdown (name-of widget) 
                         (etypecase choices
                           (list choices)
                           ((or function symbol) (funcall choices)))
                         :welcome-name (unless (requiredp-of widget)
                                         (welcome-name-of widget))
                         :frob-welcome-name nil
                         :selected-value (intermediate-value-of widget)
                         :class (if (error-message-of widget) "invalid" "valid")
                         :id id))
      (:radio
        ;; TODO
      ))))


;;; view interface
(defclass test-class ()
  ((slot1 :accessor slot1)
   (slot2)))

(defmethod field-presentation->field-widget-class ((presentation input-presentation))
  'string-field-widget)

(defmethod field-presentation->field-widget-class ((presentation password-presentation))
  '(string-field-widget :style :password))

(defmethod field-presentation->field-widget-class ((presentation textarea-presentation))
  '(string-field-widget :style :textarea))

(defmethod field-presentation->field-widget-class ((presentation dropdown-presentation))
  `(dropdown-field-widget :style :dropdown
                          :welcome-name ,(when (slot-boundp presentation 'welcome-name)
                                           (dropdown-presentation-welcome-name presentation))
                          :choices ,(presentation-choices presentation)))

(defmethod field-presentation->field-widget-class ((presentation radio-presentation))
  '(dropdown-field-widget :style :radio))

(defun field-parser->field-widget-parser (parser field)
  (check-type parser parser)
  (lambda (raw-value)
    (multiple-value-bind (parsedp presentp parsed-value)
        (parse-view-field-value parser raw-value nil (make-instance 'form-view) field)
      (declare (ignore presentp))
      (if parsedp
        (values t parsed-value)
        (values nil (parser-error-message parser))))))

(defun field-widget-from-field-info (field-info)
  (let* ((field (field-info-field field-info))
         (field-widget-class (ensure-list
                               (field-presentation->field-widget-class (view-field-presentation field))))
         (field-widget (apply #'make-instance (car field-widget-class)
                              :name (string-downcase (view-field-slot-name field))
                              :hidep (view-field-hide-p field)
                              :label (view-field-label field)
                              :requiredp (form-view-field-required-p field)
                              :validators (ensure-list (form-view-field-satisfies field))
                              :parser (field-parser->field-widget-parser (form-view-field-parser field) field)
                              (cdr field-widget-class))))
    (awhen (field-info-parent-info field-info)
      (setf (name-of field-widget)
            (concatenate 'string (string-downcase
                                   (princ-to-string
                                     (view-field-slot-name (field-info-field it))))
                                 "-"
                                 (name-of field-widget))))
    field-widget))

(defmethod form-widget-initialize-from-view (form view)
  ;; TODO mixins
  (let ((view (find-view view)))
    ;; form-level data
    (setf (caption-of form) (view-caption view))
    (setf (instructions-of form) (form-view-instructions view))
    ;; fields
    (let ((field-widgets
            (mapcar #'field-widget-from-field-info
                    (get-object-view-fields nil view
                                            :include-invisible-p t :expand-mixins nil))))
      (setf (widget-children form) field-widgets))))

;;; data form
(define-widget dataform2 (form-widget)
  ((data :type t :initform nil)
   (readers :type list :initform nil
            :documentation "Alist of (field-widget-name . data-reader-fn[obj]).")
   (writers :type list :initform nil
            :documentation "Alist of (field-widget-name . data-writer-fn[value obj]).")))

(defmethod update-intermediate-values-from-data ((widget dataform2))
  (mapcar (lambda (reader)
            (destructuring-bind (field-widget-name . reader-fn) reader
              (let* ((field-widget (find-field-widget-by-name widget field-widget-name))
                     (data (data-of widget))
                     (serialized-data-value (serialize-for-html (funcall reader-fn data))))
                (assert field-widget)
                (setf (intermediate-value-of field-widget) serialized-data-value)
                (cons field-widget serialized-data-value))))
          (readers-of widget)))

(defmethod initialize-instance :after ((widget dataform2) &rest initargs)
  "Fill in intermediate values from underlying data objects
as reported by reader functions."
  (declare (ignore initargs))
  (push
    (lambda () (update-intermediate-values-from-data widget))
    (request-hook :request :pre-render)))

(defmethod update-data-from-parsed-values ((widget dataform2))
  (mapcar (lambda (writer)
            (destructuring-bind (field-widget-name . writer-fn) writer
              (let* ((field-widget (find-field-widget-by-name widget field-widget-name)))
                (assert field-widget)
                (funcall writer-fn (parsed-value-of field-widget) (data-of widget))
                field-widget)))
          (writers-of widget)))

(defmethod form-widget-act-on-success-item ((widget form-widget)
                                            (success-item (eql :persist)))
  (let ((data (data-of widget)))
    (persist-object (or (class-store data) (object-store data)) data)))

;;; test app
#|
(defwebapp form-test :prefix "/form-test")

(defun init-user-session (root)
  (let* ((string-field (make-instance 'string-field-widget
                                     :label "My Label"
                                     :requiredp t
                                     :validators (list 
                                                   (lambda (parsed-value)
                                                     (or (< (length parsed-value) 5)
                                                         (values nil "Must not exceed 4 chars")))
                                                   (lambda (parsed-value)
                                                     (or (> (length parsed-value) 2)
                                                         (values nil "Must have more than 2 chars"))))))
         (form-widget-1 (make-instance 'form-widget :children (list string-field
                                                                    #+(or)(make-instance 'form-submit-field :label "Send this!"))))
         (form-widget-2 (make-instance 'form-widget))
         #+(or)(dataform2-1 (make-instance 'dataform2)))
    (form-widget-initialize-from-view form-widget-2 'test-form-view)
    (setf (widget-children root) (list form-widget-1
                                       form-widget-2))))

(start-weblocks :port 9110 :debug t)

;;; test views
(defview parent-form-view (:type form
                           :inherit-from '(:scaffold test-class))
  (base-field))

(defview test-form-view (:type form
                         :inherit-from 'parent-form-view
                         :caption "Hello, world"
                         :buttons '((:submit . "Go, go, go!")))
  (field1 :label "Cat")
  (field2 :label "Dog"
          :requiredp nil
          :present-as (dropdown :welcome-name "[Choose a type of dog]"
                                :choices (list "Terrier" "Dobermann" "Chihuahua")))
  (field3 :label "Season"
          :requiredp t
          :present-as (dropdown :choices (constantly
                                           (list "Spring" "Summer" "Autumn" "Winter"))))
  (field4 :label "Car"
          :requiredp nil
          :present-as (dropdown :choices (list "BMW" "Ford" "Mercedes Benz" "Mazda" "Volvo")))
  (field5 :requiredp nil
          :present-as dropdown)
  #+(or)
  (field6 :requiredp t
          :present-as dropdown)
  (field7 :label "Year"
          :requiredp t
          :parse-as integer
          :satisfies (lambda (x)
                       (or (= x 2010) (values nil "Please enter the current year."))))
  (field8 :label "Write something about your life"
          :present-as textarea
          :requiredp nil))
|#

#|
WEBLOCKS(1): (class-direct-subclasses (find-class 'presentation))

(#<STANDARD-CLASS FORM-PRESENTATION> #<STANDARD-CLASS TEXT-PRESENTATION>)
WEBLOCKS(1): (class-direct-subclasses (find-class 'form-presentation))

(#<STANDARD-CLASS CHECKBOXES-PRESENTATION>
 #<STANDARD-CLASS DROPDOWN-PRESENTATION>
 #<STANDARD-CLASS RADIO-PRESENTATION>
 #<STANDARD-CLASS CHECKBOX-PRESENTATION>
 #<STANDARD-CLASS FILE-UPLOAD-PRESENTATION>
 #<STANDARD-CLASS INPUT-PRESENTATION>)
|#

;; funcall field
(define-widget funcall-field-widget (field-widget)
  ((function :type (or symbol function))))

(defmethod render-field-contents ((form form-widget) (field funcall-field-widget) &key id &allow-other-keys)
  (declare (ignore id))
  (funcall (function-of field) form field))

