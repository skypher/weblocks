=========
 Widgets
=========

Building a widget
=================

Most widgets consist of two main parts:

* a class definition using the ``DEFWIDGET`` macro.
* a method for ``RENDER-WIDGET-BODY`` specialized on the widget's class.


Rendering protocol
==================

Map over the currently active (by URI) widget tree::

  render-widget -> render-widget-body (widget-update-children?)

