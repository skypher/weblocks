====================
 Weblocks Internals
====================

Not only useful for those wishing to hack the internals but also to gain
an understanding of Weblocks that aids in debugging hard problems.

Request handler
===============

Dispatching mechanism
=====================

Widget tree update mechanism
============================

The whole tree update protocol goes like this:

1. HANDLE-NORMAL-REQUEST calls UPDATE-WIDGET-TREE, which walks the tree
   using WALK-WIDGET-TREE starting at ROOT-WIDGET and calling
   update-children at every node.

2. The selector's UPDATE-CHILDREN method (valid for all selectors,
   i.e. widgets that process URI tokens) calls GET-WIDGET-FOR-TOKENS.

3. if a widget corresponding to particular URI tokens is found,
   UPDATE-CHILDREN calls UPDATE-DEPENDENTS, so that the selector (or its
   subclass) may update its dependents list and do other housekeeping.
   The default implementation of UPDATE-DEPENDENTS just calls (SETF
   WIDGET-CHILDREN) to store the children under the :SELECTOR type.

   Usually the only thing you'll want to do if you are implementing your
   own kind of selector is to subclass selector and provide a
   GET-WIDGET-FOR-TOKENS method for it. See class ON-DEMAND-SELECTOR for
   an example."))


Selectors
=========

.. code:: common-lisp
          
   (defgeneric get-widget-for-tokens (selector uri-tokens)
     (:documentation "Given a list of URI tokens, map them to a widget. All
     selectors implement this method. There can be multiple strategies for
     mapping URI tokens to widgets: static maps, dynamically-generated
     widgets, dynamically-generated widgets with caching. Returns a widget
     or NIL if not found. Modifies URI-TOKENS.


Views
=====

Views are the second highest level mechanism (after forms) in the weblocks
system of forms, views, presentations & parsers that form the rendering
framework for structured data. views are defined using defview.

defview is a macro that in turn calls defview-anon. defview-anon is not visible
outside weblocks -- if you have to use it, call it with ``weblocks::defview-anon``.
defview-anon actually makes the view, defview attaches a gensym'd name to it
and stores it in the global views hash.

so all views are global, and any changes made to one show up in all the others
(at least after a refresh of the browser).

Form pipeline
=============

TODO

