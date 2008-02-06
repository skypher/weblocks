
(in-package :weblocks)

(export '(grid grid-view grid-view-field grid-view-allow-sorting-p
	  grid-view-field-order-by-mixin grid-view-field-order-by
	  grid-view-field-allow-sorting-p grid-scaffold))

;;; Grid view
(defclass grid-view (table-view)
  ((allow-sorting-p :initform t
		    :initarg :allow-sorting-p
		    :accessor grid-view-allow-sorting-p
		    :documentation "If set to true, fields will be
		    allowed to be sorted by default (and otherwise if
		    set to nil). Note, whether sorting is allowed for
		    a given field can be overriden with the field's
		    setting (see
		    'grid-view-field-allow-sorting-p')."))
  (:documentation "A view designed to present sequences of object in a
  grid to the user. This view inherits from the table view and
  contains additional options used by the 'datagrid' widget."))

;;; Order by mixin
(defclass grid-view-field-order-by-mixin ()
  ((order-by :initarg :order-by
	     :accessor grid-view-field-order-by
	     :documentation "If set to a symbol or a list of symbols,
	     this slot will be used to build an order-by path that is
	     later passed to the backend store for ordering the
	     entires of the grid. If this slot is unbound (the
	     default), the 'slot-name' slot of the field will be used
	     instead."))
  (:documentation "This class is used in inline and mixin grid
  field for ordering options."))

;;; Grid view field
(defclass grid-view-field (table-view-field grid-view-field-order-by-mixin)
  ((allow-sorting-p :initarg :allow-sorting-p
		    :accessor grid-view-field-allow-sorting-p
		    :documentation "If set to true, the field will be
		    allowed to be sorted (and otherwise if set to
		    nil). If this slot is unbound (the default), the
		    value will be obtained from
		    'grid-view-allow-sorting-p'."))
  (:documentation "A field class representing a column in the grid
  view."))

;;; Grid view mixin field
(defclass mixin-grid-view-field (mixin-view-field grid-view-field-order-by-mixin)
  ()
  (:documentation "A mixin field class of the grid view."))

(defmethod view-default-field-type ((view-type (eql 'grid)) (field-type (eql 'mixin)))
  'mixin-grid)

;;; Inherit scaffolding from table
(defclass grid-scaffold (table-scaffold)
  ()
  (:documentation "Grid scaffold."))

