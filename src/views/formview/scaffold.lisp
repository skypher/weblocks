
(in-package :weblocks)

(export '(form-scaffold typespec->form-view-field-parser))

(defclass form-scaffold (scaffold)
  ()
  (:documentation "Form scaffold."))

(defmethod generate-scaffold-view ((scaffold form-scaffold) object-class)
  (make-instance (scaffold-view-type scaffold)
                 :inherit-from nil
                 :fields (mapcar
                          (curry #'generate-scaffold-view-field scaffold object-class)
                          (weblocks-stores:class-visible-slots object-class :readablep t :writablep t))))

(defmethod generate-scaffold-view-field ((scaffold form-scaffold)
                                         object-class dsd)
  (let ((slot-type (slot-definition-type dsd)))
    (apply #'make-instance (scaffold-view-field-type scaffold)
           :slot-name (slot-definition-name dsd)
           :label (humanize-name (slot-definition-name dsd))
           :requiredp (and slot-type (not (typep nil slot-type)))
           (append
            (extract-view-property-from-type :present-as #'typespec->view-field-presentation
                                             scaffold dsd)
            (extract-view-property-from-type :parse-as #'typespec->form-view-field-parser
                                             scaffold dsd)))))

;;; Type introspection protocol
(defgeneric typespec->form-view-field-parser (scaffold typespec args)
  (:documentation "Converts a typespec to a parser argument. See
'typespec->view-field-presentation' for more information.")
  (:method ((scaffold form-scaffold) typespec args)
    (declare (ignore typespec args))
    nil))

