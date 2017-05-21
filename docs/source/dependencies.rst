=====================
 Dependency handling
=====================

Automatically installing external dependencies
==============================================

Currently Weblocks external dependencies are handled with "Weblocks
assets" mechanism, see
https://github.com/html/weblocks-utils#weblocks-assets-packages.

It is not in Weblocks repository until it will be well tested. 

At this moment mechanism works well and solves problem of automatically
installing external dependencies.

There is no similar solution found for other frameworks and it gives
much comfort of reusing Weblocks widgets.


Require dependencies for widgets
================================

Weblocks has its dependency mechanism, it is responsible for rendering
links to javascript or css files inside of pages.

There is other dependencies mechanism which has it's own advantages but
it is not considered stable yet.

Weblocks standard dependencies
------------------------------

Each widget and application itself can have dependencies. 
Application dependencies are always present on the page.
Widget dependencies get into page during particular widgets rendering.

Currently there are following dependency classes - ``script-dependency``
for javascript files, ``stylesheet-dependency`` for css files and
``javascript-code-dependency`` for javascript code.

See documentation for more usage information
(http://quickdocs.org/weblocks/api).


jQuery-seq dependencies (not stable)
------------------------------------

If you know javascript well you can use jQuery-seq
(https://github.com/html/jquery-seq) for dependencies loading.

Just evaluate your javascript code inside withScripts and withStyles
calls (see jQuery-seq documentation).

For jQuery-seq dependencies you need to include jQuery-seq itself in
application dependencies.

Before including library we should get it served from http.

The easiest way to do this is to install Weblocks assets package
jquery-seq.  Other way is to download jQuery-seq, to put it into
pub/scripts/ directory.
