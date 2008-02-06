
(in-package :weblocks)

(export '(data-scaffold))

(defclass data-scaffold (scaffold)
  ()
  (:documentation "Data scaffold."))

(defmethod generate-scaffold-view ((scaffold data-scaffold) object-class)
  (make-instance (scaffold-view-type scaffold)
		 :inherit-from nil
		 :fields (mapcar
			  (curry #'generate-scaffold-view-field scaffold object-class)
			  (class-visible-slots object-class :readablep t))))

(defmethod generate-scaffold-view-field ((scaffold data-scaffold)
					 object-class dsd)
  (apply #'make-instance (scaffold-view-field-type scaffold)
	 :slot-name (slot-definition-name dsd)
	 :label (humanize-name (slot-definition-name dsd))
	 (extract-view-property-from-type :present-as #'typespec->view-field-presentation
					  scaffold dsd)))

