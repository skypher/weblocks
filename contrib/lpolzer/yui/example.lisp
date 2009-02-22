
(defwidget buddy-manager-panel (yui-panel) ()
  (:default-initargs :target-id "buddy-manager"
                     :config '(:drag-only t :context (array "header-banner" "tr" "tr"))
                     :loader-args '(:include-css-p nil)))

(defwidget buddy-manager-resize (yui-resize) ()
  (:default-initargs :target-id "buddy-manager"
                     :config '(:handles (array "br") :auto-ratio false
                               :status false :proxy false)))

(defwidget buddy-manager (yui-tabview)
  ((proxy-resize-p :type boolean :accessor proxy-resize-p :initarg :proxy-resize-p :initform nil)
   (closable :type boolean :accessor closable :initarg :closable :initform nil)
   (draggable :type boolean :accessor draggable :initarg :draggable :initform nil)
   (resizable :type boolean :accessor resizable :initarg :resizable :initform nil))
  (:default-initargs :name "buddy-manager"
                     :tabs (list
                             (cons #!"Friends" (make-instance 'buddy-list :buddies #'friends))
                             (cons #!"Foes" (make-instance 'buddy-list :buddies #'foes))
                             (cons #!"Clan" (make-instance 'buddy-list :buddies #'clan-buddies)))

                     ))

(defmethod render-widget-body ((widget buddy-manager) &rest args)
  (call-next-method) ; render tabview
  (let ((panel (intern (gen-id "yuiWidget")))
        (resize (intern (gen-id "yuiWidget"))))
    (let ((panel (make-instance 'buddy-manager-panel :widget-variable panel)))
      (add-component-config panel :draggable (js-bool (draggable widget))
                                  :closeable (js-bool (closable widget)))
      (render-widget panel))
    (when (resizable widget)
      (render-widget (make-instance 'buddy-manager-resize
                                    :panel panel
                                    :widget-variable resize
                                    :target-id (attributize-name (widget-name widget))
                                    #|:proxy-resize-p (proxy-resize-p widget)|#)))))

(defun make-buddy-manager ()
  (premium (:GOLD)
    (return-from make-buddy-manager (make-instance 'buddy-manager :closable t :draggable t
                                                                  :resizable t
                                                                  :proxy-resize (eq (+pref+ resize-mode) :proxy))))
  (premium (:SILVER)
    (return-from make-buddy-manager (make-instance 'buddy-manager :closable t :draggable t)))
  (premium (:BRONZE)
    (return-from make-buddy-manager (make-instance 'buddy-manager)))
  nil)

