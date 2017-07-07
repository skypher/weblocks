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


Notes
=====

Зависимости билдятся через вызов (dependencies root) и последующие
(render-dependency-in-page-head). Между ними дополнительно выполняются
шаги::

  <DEBUG> [16:47:19] weblocks dependencies.lisp (build-dependencies) -
    Building dependencies for WEBLOCKS::APP: #<HACRM::HACRM {1003D02BB3}>
    from WEBLOCKS::DEP-LIST: (#<WEBLOCKS:SCRIPT-DEPENDENCY /pub/scripts/jquery-1.8.2.js>
                              #<WEBLOCKS:SCRIPT-DEPENDENCY /pub/scripts/jquery-seq.js>
                              #<WEBLOCKS:SCRIPT-DEPENDENCY /pub/scripts/weblocks-jquery.js>
                              #<WEBLOCKS:STYLESHEET-DEPENDENCY /bootstrap/css/bootstrap.css>
                              #<WEBLOCKS:STYLESHEET-DEPENDENCY /pub/stylesheets/twitter-bootstrap.css>
                              #<WEBLOCKS:STYLESHEET-DEPENDENCY /pub/stylesheets/layout.css>
                              #<WEBLOCKS:STYLESHEET-DEPENDENCY /pub/stylesheets/main.css>
                              #<WEBLOCKS:STYLESHEET-DEPENDENCY /pub/stylesheets/dialog.css>
                              NIL
                              #<WEBLOCKS:SCRIPT-DEPENDENCY /bootstrap/js/bootstrap.js>)

  <DEBUG> [16:47:19] weblocks dependencies.lisp (compact-dependencies) -
    Compacting dependencies
  <DEBUG> [16:47:19] weblocks dependencies.lisp (prune-dependencies) -
    Pruning dependencies
  <DEBUG> [16:47:19] weblocks dependencies.lisp (bundle-dependencies) -


Метод (dependencies) вызывает (render-widget), который, в свою очередь,
вызывается из handle-normal-request.


Метод (render-dependency-in-page-header) вызывается из render-page, а
тот из handle-normal-request.


Что нужно решить
================

* static dependency должно понимать пути относительно заданной системы.
* не должны создаваться новые одинаковые dependency и роуты (может быть
  сделать around для get-dependencies и в нём кэшировать?)
* роут зависимости должен автоматически включать приложение или виджет,
  чтобы не было конфликтов.
* подумать, что делать с assets, можно ли их как-то переиспользовать.
  Быть может сделать более автоматическими, типа формул, или просто
  сделать тип зависимости asset-dependency, который будет скачивать
  asset и отдавать рендерить на странице несколько зависимостей.


New dependencies
================


Hierarchy
---------

dependency
~~~~~~~~~~

Базовый класс.

У такой зависимости должен быть определён get-url и content-type. Они
используются для рендеринга html.

Влияет на то, рендерится ли зависимость в коде страницы или просто
отдаётся по route. image имеет смысл только в объединении со static-depdendency.

local-dependency
~~~~~~~~~~~~~~~~

Может отдаваться локально, для них заводятся route в веб-сервере.

Должен быть определён метод serve, отдающий путь до файла или контент.

Так же должен быть определён get-url, отдающий локальный путь типа "/dsadsad/dsdsad"

remote-dependency
~~~~~~~~~~~~~~~~~

Удалённый урл, который просто линкуется в страницу.

cached-remote-dependency (remote-dependency local-dependency)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


New kind of dependencies
========================

To define dependencies on whole app, declare a method specialized on
your app:
