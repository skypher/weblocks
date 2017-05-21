=======
 Views
=======

Syntax of declarative view declaration
======================================

The macro ``DEFVIEW`` is a declarative DSL for defining views.

Syntax::
  
  (defview (NAME [:type TYPE] [:inherit-from INHERIT-FROM]
                 [:satisfies SATISFIES] VIEW-KWARGS...)
    [FIELD-NAME
     | (FIELD-NAME [:type FIELD-TYPE] [:initform INITFORM]
                   [:present-as PRESENT-AS] [:parse-as PARSE-AS]
                   FIELD-KWARGS...)]...)


In the above form, these metasyntactic variables have the following values:

NAME
  When non-nil, an unevaluated unique symbol identifying the
  view to other parts of the system that require a view,
  resolving it using ``find-view``.

TYPE
  An unevaluated designator for the class of the resulting
  view (determined by ``view-class-name``), and half the
  designator for the class of this view's fields (determined by
  ``view-field-class-name`` applied to ``view-default-field-type``).
  Defaults to ``data``.

INHERIT-FROM
  A designator for ``view`` slot ``inherit-from``.  Forms
  like ``(:scaffold DATA-CLASS)`` will replace ``:scaffold`` with ``TYPE``.
  See ``find-view`` for further transformation.

  Evaluated.

SATISFIES
  A designator for ``form-view`` slot ``satisfies``.

  Evaluated.

VIEW-KWARGS
  Other arguments passed directly to ``make-instance`` for the
  resulting view, quoting subject to
  ``view-argument-quoting-strategy``.

Argument semantics:

FIELD-NAME
  An unevaluated designator for ``view-field`` slot ``slot-name``.

FIELD-TYPE
  A designator for the class of the field, in combination with
  TYPE, as explained above for TYPE.

FIELD-KWARGS
  Other arguments passed directly to `make-instance' for the
  resulting view field, quoting subject to
  ``view-argument-quoting-strategy``.

  However, ``FIELD-KWARGS`` keywords present in
  ```*'CUSTOM-VIEW-FIELD-ARGUMENT-COMPILERS*`` are not included in the
  call to ``MAKE-INSTANCE``; see that variable for information on
  how those are transformed.

The built-in custom view fields are as follows:

INITFORM
  A form for ``mixin-view-field`` slot ``initform`` (which is
  really an initfunction).

PRESENT-AS
  A designator for ``view-field`` slot ``presentation``.  The symbol
  or CAR is mapped to a class name through
  ``presentation-class-name``, and the CDR, a series of keyword
  arguments, is passed to `make-instance' on said class, subject
  to `view-argument-quoting-strategy'.

PARSE-AS
  A designator for ``form-view-field`` slot ``parser``.  Otherwise
  similar to PRESENT-AS, but mapping class names using
  ``parser-class-name``."


Configuring view fields at run-time
===================================


Customizing scaffolding
=======================


Limitations
===========


Available presentations
=======================


input
  Renders form text input

textarea
  Renders textarea

checkbox
  Renders form single checkbox, used for boolean values

checkboxes
  Renders several checkboxes - several choices from several variants

dropdown
  Renders a dropdown - single choice from few variants

radio
  Renders radio buttons - single choice from few variants

date-entry
  Renders custom control for date input. 

file-upload
  Renders input with type file

password
  Renders input with type password

widget
  Renders widget as form field. You should provide
  ``get-widget-form-value-from-request`` method for widget to parse value.


Available parsers
=================

text
  Used with input presentation

number
  Used with input presentation for parsing numbers

integer
  Used with input presentation for parsing integer numbers

float
  Used with input presentation for parsing float numbers

symbol
  Transforms string value into symbol

keyword
  Transforms string value into keyword

checkboxes
  Used with checkboxes-presentation

predicate
  Used with checkbox-presentation

object-id
  ??? TODO: found what it is.

