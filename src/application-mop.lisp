
(in-package :weblocks)

(export '(webapp-class))

(defclass webapp-class (standard-class)
  ((home-package
    :accessor webapp-class-home-package :initform *package*
    :documentation "The current package when I was defined.")
   (default-store-name
    :accessor webapp-default-store-name :initform nil
    :documentation "If non-nil, the name of the `*default-store*'
    bound during request handlers."))
  (:documentation "The class of all webapp classes."))

(defmethod validate-superclass ((self webapp-class) (super standard-class))
  (typep (class-name (class-of super))
	 '(member standard-class webapp-class)))

(defmethod shared-initialize :after
    ((self webapp-class) slots &key autostart &allow-other-keys)
  (declare (ignore slots))
  (let ((name (class-name self)))
    (pushnew name (symbol-value '*registered-webapps*))
    (when autostart
      (pushnew name (symbol-value '*autostarting-webapps*)))))

