
(in-package :weblocks)

(export '(scaffold scaffold-class-name scaffold-view-type
          scaffold-view-field-type generate-scaffold-view
          generate-scaffold-view-field class-visible-slots
          class-visible-slots-impl inspect-typespec
          extract-view-property-from-type
          typespec->view-field-presentation))

;;; Scaffold class
(defclass scaffold ()
  ()
  (:documentation "Base class to be used for scaffolds."))

(defgeneric scaffold-class-name (scaffold)
  (:documentation "Given a scaffold 'type', converts it to the class
name of the scaffold. Default implementation adds '-scaffold' to the
type and returns the symbol.")
  (:method (scaffold)
    (entity-class-name scaffold '#:-scaffold)))

(defgeneric scaffold-view-type (scaffold)
  (:documentation "Given a scaffold object, returns its view
type. Default implementation removes '-scaffold' from the end of the
scaffold's name, adds '-view', and returns the resulting symbol.")
  (:method (scaffold)
    (find-symbol
     (concatenate 'string
                  (string-remove-right (symbol-name (class-name (class-of scaffold)))
                                       (symbol-name '#:-scaffold)
                                       :ignore-case-p t)
                  (symbol-name '#:-view))
     (symbol-package (class-name (class-of scaffold))))))

(defgeneric scaffold-view-field-type (scaffold)
  (:documentation "Given a scaffold object, returns its view field
type. Default implementation removes '-scaffold' from the end of the
scaffold's name, adds '-view-field', and returns the resulting
symbol.")
  (:method (scaffold)
    (find-symbol
     (concatenate 'string
                  (string-remove-right (symbol-name (class-name (class-of scaffold)))
                                       (symbol-name '#:-scaffold)
                                       :ignore-case-p t)
                  (symbol-name '#:-view-field))
     (symbol-package (class-name (class-of scaffold))))))

(defmacro extract-view-property-from-type (property-name property-extraction-fn
                                           scaffold dsd)
  "Helper macro to obtain view properties from typespecs."
  (let ((extractedp (gensym))
        (extraction (gensym)))
    `(multiple-value-bind (,extractedp ,extraction)
         (multiple-value-call ,property-extraction-fn
           ,scaffold (inspect-typespec (slot-definition-type ,dsd)))
       (when ,extractedp
         (list ,property-name ,extraction)))))

;;; Scaffold protocol
(defgeneric generate-scaffold-view (scaffold-type object-class)
  (:documentation "Generates and returns a scaffold view of a given
scaffold type for a given object class. Scaffold views should examine
the object class and provide a sensible default view for an object.")
  (:method ((scaffold scaffold) object-class)
    (make-instance (scaffold-view-type scaffold)
                   :inherit-from nil
                   :fields (mapcar
                            (curry #'generate-scaffold-view-field scaffold object-class)
                            (class-visible-slots object-class :readablep t)))))

(defgeneric generate-scaffold-view-field (scaffold object-class direct-slot-definition)
  (:documentation "Generates a sensible view field from a direct slot
definition. Specialize this function to generate different fields
depending on a scaffold view type, object class, or slot
definition.")
  (:method ((scaffold scaffold) object-class dsd)
    (apply #'make-instance (scaffold-view-field-type scaffold)
           :slot-name (slot-definition-name dsd)
           :label (humanize-name (slot-definition-name dsd))
           (extract-view-property-from-type :present-as #'typespec->view-field-presentation
                                            scaffold dsd))))

(defun inspect-typespec (typespec)
  "Converts 'typespec' into a representation suitable for further
inspection. Returns two values. The first one is a symbol that can be
specialized on via eql specializer, and the second one is a list of
arguments to the typespec.  Ex: \(inspect-typespec '(integer 1 5)) =>
integer => (1 5)

This function also implements certain useful logic. For example:
\(inspect-typespec '(or null (integer 1 5)))
=> integer
=> (1 5)"
  (if (listp typespec)
      (case (car typespec)
        (mod (values (car typespec) (cdr typespec)))
        (eql (values (car typespec) (cdr typespec)))
        (member (values (car typespec) (cdr typespec)))
        (not (values nil nil))
        (satisfies (values (car typespec) (cdr typespec)))
        (values (values nil nil))
        (and (match typespec
                    ((list 'and y) (inspect-typespec y))
                    (y (values (car typespec) (cdr typespec)))))
        (or (match typespec
                   ((list 'or 'null y) (inspect-typespec y))
                   ((list 'or y 'null) (inspect-typespec y))
                   ((list 'or y) (inspect-typespec y))
                   (y (values (car typespec) (cdr typespec)))))
        (otherwise (values (car typespec) (cdr typespec))))
      (values typespec nil)))

;;; Type introspection protocol
(defgeneric typespec->view-field-presentation (scaffold typespec args)
  (:documentation "Expects a scaffold type, a typespec and the
typespec's arguments arguments (obtained via
'inspect-typespec'). Converts the given typespec to a view-field
presentation argument. This function may return multiple values. The
first value should be true if the typespec was converted to a
presentation, nil otherwise. If the first value is true, second value
should be the presentation argument.")
  (:method (scaffold typespec args)
    (declare (ignore typespec args))
    nil))

