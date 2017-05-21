========================
 Components of Weblocks
========================


Widgets
=========

Widgets are the main building blocks of Weblocks (hence the name).

At the heart of Weblocks is a tree [#tree]_ of widgets that is manipulated by the clients requests. When a
client sends its first request to a Weblocks application then a new session is
started for it and a widget tree is associated with this session.

This initial widget tree is computed as defined by the application developer.
A special function (usually called ``init-user-session``) is called by Weblocks
to initialize a new session. This function then proceeds to set up a
widget tree by adding children to the root of the widget tree
(accessible with the macro ``root-widget``).

The initial content and structure of the widget tree may depend on arbitrary
factors; it is conceivable (although probably not very sensible) to generate
different widget trees depending on the time of day.

A client's request for a specific URI modifies the widget tree: widgets
called /dispatchers/ choose their one child based on the current request
URI.

There is a convenience function ``MAKE-WIDGET`` that creates widgets
from strings (anything that can be printed, in fact) and functions.


Actions
=======

Apart from session initialization the widget tree may be modified by /actions/.

Actions are plain functions that are stored in a session hash table the keys
of which are unique identifiers. This way they can be called by the browser.

When Javascript is available actions will be called via AJAX, otherwise
a normal request will be initiated.

It is often useful to set up closures as actions.

Example of typical action usage:

.. code:: common-lisp

   (defwidget counter ()
     ((count :accessor count-of :initform 0)))

   (defmethod render-widget-body ((widget counter) &rest args)
     (with-html
       (:p (esc (format nil "The counter is at ~D." (count-of widget))))
       (:p (render-link
            (lambda (&rest args)
             ;; closes over WIDGET.
             (incf (count-of widget)))
            "Count up!"))))

Navigations and dispatchers
===========================

Dispatchers are widgets that configure themselves and their children
(i.e. broadly speaking the widget tree) based on the URI of the request.

Navigations are dispatchers that maintain a simple one-to-one association between
an URI token [#uri-tokens]_ and a widget.

Note that each widget can also in turn be a navigation that consumes an
URI token, thereby building a many-to-many relationship between tokens
and widgets.

The function ``MAKE-NAVIGATION`` is a convenience frontend for building
navigations.

The first argument is a title for the navigation. The rest of the arguments
map tokens to widgets:

.. code:: common-lisp
          
   (defun init-user-session (root)
     (setf (widget-children root) (make-navigation "Food Shop"
                                   "Fruits" (make-fruit-navigation)
                                   "Vegetables" (make-vegetables-navigation)
                                   "Billing" (make-instance 'billing-widget :shop-type 'food))))

``MAKE-WIDGET`` is applied to the widget argument so you can use strings
and function designators [#function-designators]_ instead of widgets in this context.


Views
=====

Views provide convenient ways to define how data object are to
be rendered.

Weblocks currently comes with three types of views: data views, table views
and form views.

Form views are especially useful because they let you build forms
in a declarative manner, error checking and reporting included. 

Views mainly consist of a rendering framework unique to the
view and a series of view fields. The table view for example knows how to render a HTML table.

View fields usually map to the slots of your data class but they are
flexible enough to be used in any other way. Associated with each view field
regardless of the view type are things like a human-readable label and
information on how to render the field (presentation).

Form views include additional parameters like how to translate user
input to a proper model value (parser) and constraints on the data
(satisfies).

Templates
=========

Template is something different from view.  It is another layer which
allows to customize views look.  Most of weblocks code support templates
and this gives us ability to change widget html without changing widget
logic.  We get rid of method overriding for widgets and override only
template.

Templates bring theming functionality into Weblocks, can be used to
simplify some debugging work and to improve widgets reusing.

You can build your own widgets without templates. 

Templates are necessary for widgets which you want to be
reused. Weblocks contains much of such widgets.

Also if you want to make your own Weblocks theme you should use templates.

Template is ...

* template function
* template definition 

And templates should be rendered inside of widgets.

Template function
-----------------

Template function should receive some key parameters and return html string.
You can use any html generation library or template library for templates. 

We encourage you to 

* use naming convention and call template ``*-wt`` (with "-wt" suffix
  ). "wt" means either "web template" or "Weblocks template". Since
  template should be overriden often, name convention will made easier
  to find what we need.
* use ``&allow-other-keys`` in every template.
* use no complex logic and no computations in templates. Just simple
  iteration over lists and if statements when you use cl-who. The best
  thing would be to use no more logic then in mustache templates
  w_url(`http://mustache.github.io/mustache.5.html') For cl-who
  ``format``, ``concatenate`` and other string functions could be used
  (concatenation is allowed), but avoid to use number operations.

Here is template example.

.. code:: common-lisp
          
   (defun my-wt (&key (content "I'm template") &allow-other-keys)
       (with-html-to-string 
           (:p (str content))))

Template definition
-------------------

Template definition is a ``deftemplate`` call. 

.. code:: common-lisp
          
   (deftemplate :my-wt 'my-wt)


Here we just connected ``:my-wt`` template name with template function ``'my-wt``.

And here comes most important templates part.
We can connect many template functions to template name and only one of them - effective template - will be called.
Effectiveness determined by priority which is received from context matchers. 
Context matcher just receives context and returns priority number.

.. code:: common-lisp
   
   (defun my-other-wt(&key (content "I'm template") &allow-other-keys)
     (with-html-to-string 
       (:div :class "other-template" (str content))))

   (deftemplate :my-wt 'my-other-wt 
     :context-matches (lambda(&rest context)
       100))

``my-other-wt`` has more priority than ``my-wt`` so it will be called. 
And this is how template overriding is done.

There is also ``:application-class`` parameter which gives template 10
more priority points.

.. code:: common-lisp
          
   (deftemplate :page-wt 'my-customized-page-wt 
                :application-class 'my-webapp)

Here ``'my-customized-page-wt`` function will have more priority than
one defined in Weblocks.

The source of Weblocks Twitter Bootstrap theme
https://github.com/html/weblocks-twitter-bootstrap-application
is mostly templates and their definitions. Look there for more examples.

Template rendering
------------------

Template rendering is done via two functions - ``RENDER-WT`` and
``RENDER-WT-TO-STRING``. First one renders template to
``*weblocks-output-stream*``, other one returns a string with rendered
template.

Rendering function takes template name, template context parameters and
other arguments which are passed to template function.

.. code:: common-lisp

   (render-wt 
       :checkbox-wt 
       (list :name name :id id :class class)
       :name (attributize-name name)
       :id id 
       :class class
       :value (if checkedp "t" "f")
       :checkedp checkedp 
       :onclick onclick
       :disabledp disabledp)

First, render function will get all templates associated to
``:checkbox-wt``, then will get every template priority based on
context, will choose effective template and call it with parameters

.. code:: common-lisp

   :name (attributize-name name)
   :id id 
   :class class
   :value (if checkedp "t" "f")
   :checkedp checkedp 
   :onclick onclick
   :disabledp disabledp


.. rubric:: Footnotes

.. [#tree] An acyclic graph with exactly one parent per node.

.. [#uri-tokens] Еach path component of an URI is a token; for example
                 in ``"/foo/bar/quux"`` there are three tokens ``foo``
                 ``bar`` and ``quux``.

.. [#function-designators] I.e. symbols naming a global function and
                           function objects themselves.
