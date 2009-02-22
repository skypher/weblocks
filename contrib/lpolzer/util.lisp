;; assorted stuff

(eval-when (compile load eval)
  (defclass table*-view (table-view)
    ())

  (defclass table*-view-field (table-view-field)
    () (:default-initargs :hidep t))

  (defclass table*-scaffold (table-scaffold)
    ())

  (defclass form*-view (form-view)
    ())

  (defclass form*-view-field (form-view-field)
    () (:default-initargs :hidep t))

  (defclass form*-scaffold (form-scaffold)
    ())
)

(defmethod humanize-name :around (name)
  (translate (call-next-method)))

(defun wrap-in-composite (w)
  (make-instance 'composite :widgets (if (atom w) (list w) w)))

(defmacro render-image (src &rest attrs)
  ;`(demand-image-attrs ,src (list :alt :title) ,attrs)
  `(with-html
     (:img :src ,src ,@attrs)))

(defmacro render-layout-image (src &rest attrs)
  `(with-html
     (:img :src (string+ *layout-image-root* ,src) ,@attrs :class "layout-element")))

(defun render-image-button (name &optional (type 'normal) (root *button-image-root*))
  (let ((humanized-name name)
        (attributized-name (attributize-name name)))
    (render-image (button-image root type humanized-name)
                  :title humanized-name
                  :alt humanized-name
                  :id attributized-name)))

(defun render-custom-link (action draw-fn &key (ajaxp t) id class url (before-fire ""))
  (declare (special weblocks::*uri-tokens*))
  (let* ((action-code (weblocks::function-or-action->action action))
	 (url (if url
                (let ((weblocks::*uri-tokens* (list url)))
                  (declare (special weblocks::*uri-tokens*))
                  (weblocks::make-action-url action-code))
                (weblocks::make-action-url action-code))))
    (with-html
      (:a :id id :class class
	  :href url :onclick (when ajaxp
			       (format nil "~A initiateAction(\"~A\", \"~A\"); return false;"
                                       before-fire action-code
                                       (weblocks::session-name-string-pair)))
	  (funcall draw-fn)))))

(defun render-button-link (action button &rest args)
  (apply #'render-custom-link action
         (curry #'render-image-button button)
         :class "button" args))

(defun render-icon-link (action icon title &rest args)
  (apply #'render-custom-link action
         (lambda () (render-image (string+ *icon-image-root* icon ".png")
                                  :alt title :title title))
         :class "icon" args))

(defun make-action* (callback &optional (action-code (weblocks::generate-action-code)))
  "Return JS code, an action URL and the action code for a callback function"
  (let* ((action-code (make-action callback action-code))
         (url (make-action-url action-code))
         (js (format nil "initiateAction(\"~A\", \"~A\"); return false;"
                     action-code (weblocks::session-name-string-pair))))
    (values url js action-code)))

(defun make-html-template-from-file (id &optional vars)
  (handler-bind ((warning #'muffle-warning))
    (make-instance 'html-template
                   :file (get-template id)
                   :vars vars)))

(defun make-html-template-from-string (src &optional vars)
  (make-instance 'html-template
                 :src src 
                 :vars vars))

(defwidget simple-widget ()
  ((render-fn :accessor render-fn :initarg :render-fn :initform (constantly nil))))

(defmethod render-widget-body ((widget simple-widget) &rest args)
  (funcall (render-fn widget) widget))

(defmacro with-simple-html-form ((method-type target &key id class enctype) &body body)
  "Transforms to cl-who (:form) with standard form code (AJAX support, actions, etc.)"
  `(with-html
     (:form :id ,id :class ,class :action (string-right-trim "/" ,target)
            :method (attributize-name ,method-type) :enctype ,enctype
            (with-extra-tags
              (htm (:fieldset
                     ,@body))))))

(defmacro render-simple-button-html-form (method-type target label &key id class enctype params)
  `(with-simple-html-form (,method-type ,target)
     ,@(loop for param in params
             collect `(:input :type "hidden" :name ,(car param) :value ,(cdr param)))
     (:input :type "image" :src (button-image *button-image-root* 'normal ,label)
             :alt ,label :title ,label)))

(ps:defpsmacro play-sound (file)
  (let ((filename (concatenate 'string "/pub/sounds/" file))
        (id (princ-to-string (ps:ps-gensym ""))))
    `(sound-manager.play ,id ,filename)))

;; hunchentoot
(defun list-sessions ()
  (hunchentoot:do-sessions (S)
               (unless (hunchentoot:session-too-old-p S)
                 (unless (null (hunchentoot:session-value 'player S))
                 (format t "[id ~A | ~A | nreq ~A | time ~A | last ~A]~%"
                         (hunchentoot:session-cookie-value S)
                         (hunchentoot:session-value 'player S)
                         ;(hunchentoot:session-user-agent S)
                         (hunchentoot:session-counter S)
                         (format nil "~$hrs" (/ (- (get-universal-time)
                                                   (hunchentoot::session-start S)) 3600))
                         (format nil "~$hrs ago" (/ (- (get-universal-time)
                                                       (hunchentoot::session-last-click S)) 3600))
                         )))))

(defun in-session (id)
  (setf *session* (hunchentoot::get-stored-session id)))

;; strings
(defun cap-first (s)
  (string-capitalize s :end 1))

(defun has-trailing-fullstop (s)
  (unless (equal s "")
    (search "." s :from-end t :start2 (1- (length s)))))

(defun add-fullstop (s)
  (concatenate 'string s (if (has-trailing-fullstop s) "" ".")))

(defun plain-text->html (text)
  (format nil "<p>~A</p>"
          (cl-ppcre:regex-replace-all "\\n"
            (cl-ppcre:regex-replace-all "\\n\\n" text "</p><p>") "<br/>")))
  
(defun list->string (list)
    (format nil "~{~A~^, ~}" list))

(defun strip-html (text)
  (cl-ppcre:regex-replace-all "<(\"[^\"]+\"|[^>\"])*>" text ""))

(define-symbol-macro +blindtext+
  (apply #'string+
         (loop for i from 1 below 100
               collect "blind text ")))

(defun string+ (&rest parts)
  (with-output-to-string (out)
    (dolist (part parts)
      (write part :stream out :escape nil))))

(defparameter +default-chars+ "abcdefghijklmnopqrstuvwxyx123456789")

(defun generate-password (length &key (chars +default-chars+))
  (coerce (loop for i from 1 to length
                and random-char = (elt chars (random (length chars)))
                collect random-char)
          'string))

