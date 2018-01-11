===========
 ChangeLog
===========

0.23.0 (2018-01-11)
===================

* Symbol ``weblocks.routes:*routes*`` is not external anymore.
  Use ``weblocks.routes:add-route`` and ``weblocks.routes:get-route``
  to add new routes and to search a route matched on a path.
* Fixed getting the rendered widgets in ``weblocks.widget:update``
  method when making ``:update-widget`` or ``:insert-widget (before)``
  commands.
* Temporary added method ``weblocks::child-of-p`` for new type of
  widget. This should fix some issues, with widgets updating.

0.22.2 (2018-01-07)
===================

* Class ``weblocks.widget:widget`` was exported, to make possible to
  define widgets based on it and some mixins.

0.22.1 (2018-01-07)
===================

* Code broken in previos release was fixed.


0.22.0 (2018-01-06)
===================

Most functions from ``weblocks.request`` were refactored and renamed:

* ``request-parameters`` -> ``get-parameters``;
* ``request-parameter`` -> ``get-parameter``;
* ``request-header`` -> ``get-header``;
* ``remove-request-header`` -> ``remove-header``;
* ``request-server-name`` -> ``get-host``;
* ``request-server-port`` -> ``get-port``;
* ``request-uri`` -> ``get-uri`` (and now it returns full URI with
  scheme, host and port;
* ``request-path-info`` -> ``get-path`` (and now it has keyword argument
  ``with-params`` to copy behaviour of old ``request-uri`` and return
  strings like ``/some/path?with=parameters``;
* ``request-method`` -> ``get-method``.

All these function now accept keyword argument ``:request``. Previously
it was ``&optional``.

Another change is a new function ``weblocks.response:make-uri``. It can
be used to build new uri, based on the uri of the current request. This
can be useful when embedding links into emails, for example.

.. warning:: These changes require a newer version of Lack.

   I've made a pull request https://github.com/fukamachi/lack/pull/31
   it is not merged yet, so, alternative version of Lack can be used, by
   installing it using Qlot, from here:

   https://github.com/40ants/lack

0.21.0 (2018-01-01)
===================

* Macro ``weblocks.session:get-value`` was replaced with a regular
  function.
* Function ``weblocks.session:set-value`` was removed and replaced with
  a setter ``(setf get-value)``.

0.20.1 (2017-12-20)
===================

* Removed debug these debug messages from client-side JS:

  * LOADED;
  * Starting AJAX;
  * Stopping AJAX progress;
  * Some AJAX error;
  * Action success.

0.20.0 (2017-12-15)
===================

* Package ``weblocks.debug`` now does not export ``*on`` variable,
  but provides three functions ``on``, ``off`` and ``status``.
* New method ``weblocks.server:serve-static-file`` was introduced.
  It can be used to create static routes which will respond with
  file's content. For example, you could add this to your app's
  ``initialize-instance`` method:

  .. code:: common-lisp

     (weblocks.server:serve-static-file
        "/favicon.png"
        (asdf:system-relative-pathname :app "favicon.png"))

0.19.2 (2017-11-29)
===================

* Now weblocks rebinds ``*random-state*`` to itself for each request to
  allow it to use ``setf`` and change ``*random-state*`` until the end
  of request processing.

0.19.1 (2017-11-23)
===================

* Dirty widgets rendering was fixed.

0.19.0 (2017-11-13)
===================

* Variable ``*expired-action-handler*``, method
  ``expired-action-handler`` and function
  ``default-expired-action-handler`` were replaced with method
  ``weblocks.actions:on-missing-action``.
* Now we are trying to call action only if action's name was given.
* Old method ``weblocks:handle-client-request ((app weblocks-webapp))``
  was removed. Look at it's newer version in ``weblocks.request-handler``.
  

0.18.0 (2017-11-12)
===================

* Commented out call to ``update-widget-tree`` inside of ``(setf
  widget-children)``, because it breaks on
  ``(get-widgets-by-type 'selector :root obj)`` sometimes. Seems this is
  because I've removed selector's code previously.

  .. warning:: Probably parent/children handling code will be removed soon.
* Backtrace printing code was replaced with direct usage of
  ``trivial-backtrace:print-backtrace``.

* Call to ``prepare-hooks`` was moved from ``weblocks.request-handler:handle-client-request``
  to the the weblocks.server:handler-request, to fix session hooks processing when
  ``:process-request`` hook is called.
  
0.17.2 (2017-11-11)
===================

* Error handling code was fixed. It was broken in 0.17.1 and prevented
  system loading.

0.17.1 (2017-11-11)
===================

* Fixed error handling when debug mode is "off". Now weblocks returns
  result of ``(weblocks.error-handler:on-error app)`` call.

0.17.0 (2017-11-11)
===================

* Added a ``weblocks.actions`` package.
* Also, a ``commands`` were introduced. Commands describe remote calls
  which have to be applied on a client as a result of action's
  execution. Previously, weblocks used similar technic to replace dirty
  widgets and to execute some javascript code before or after
  replacement. The new mechanism of "commands" is more generic and uses
  the JSON-RPC to pass function name and parameters from backend to
  client-side.
* Added ``weblocks.session:in-session-p`` function which returns ``t``
  if session data can be retrived or stored without error.
* Now function ``initiateActionWithArgsAndCallback`` send arguments as
  JSON payload. This allows to send any datastructures as action's params.
* Function ``weblocks.response:send-script`` was changed to use new
  mechanizm with commands. When you send script from the action, it will
  add a command ``:execute-code`` to the stack. All commands are
  executed in same order as they were added. If you want some code to be
  executed before widget update, just execute ``send-code`` before
  ``weblocks.widget:update``.
  
0.16.0 (2017-11-04)
===================

* New package was introduced - ``weblocks.widget`` it contains a new
  ``widget`` class with simplified rendering based on ``spinneret``
  templating library.
* Now class ``route`` is exported from ``weblocks.routes`` and should be
  used instead of ``routes:route``.
* New package ``weblocks.error-handler`` was introduced. It contains a
  ``on-error`` generic method which is called when some unhandled error
  raise by application.
* Fixed issue of adding multuple routes mapped to the same path. Now if
  url mapper already have a route all subsequent attempts to add a route
  with same path are ignored.
* Fixed error::

    Undefined function WEBLOCKS:WEBAPP-SESSION-KEY called with arguments
    (#<APP::APP #x3020052F01DD>)
* Fixed ``Content-Type`` of the default 500 page. Previously it was
  ``plain/text`` and browser didn't undestand that and downloaded the
  file.

  Now ``Content-Type`` is ``text/plain``.

0.15.0 (2017-11-03)
===================

* Now weblocks does not checks if all tokens from URL were consumed by
  app during root widget rendering. Previously it returned 404 if some
  token weren't consumed. Implement this logic in your app if needed.
* Macro ``assert-hooks-called`` was changed to return hooks in the order
  they were called. Also, now it waits hooks description as a DSL,
  like:

  .. code:: common-lisp

     (assert-hooks-called
       (:fact-created contact "vasya@pupkin.com")
       (:fact-removed contact "vasya@pupkin.com"))

* New system ``weblocks-testutils`` was introduced. It
  compiles ``weblocks.t.utils`` package which macroses useful for
  unittesting.

  Also, a new macro ``catch-hooks`` was added to check if some
  hooks were called during a unittest.
  
* Now weblocks does not open a new tab or window on 500 error
  during an action execution.
  
0.14.4 (2017-10-07)
===================

* No more ``declaim optimize`` in different places. These
  declarations changed compiler's settings at unexpected moments.
* Fixed error happened when "File not found", and now
  ``with-hook`` macro returns the value of the last form's evaluation.

0.14.3 (2017-09-23)
===================

* Default method of ``render-page`` was fixed to really wrap
  page with ``<html>...`` block.
  
* Fixed a way how weblocks.debug:*latest-session* is
  processed.
  
* Function ``weblocks.request:remove-request-header`` now
  returns a new instance of request object and does not modify the
  original request. This fixes issue in ``weblocks-websocket``.

0.14.2 (2017-09-22)
===================

* Added package ``weblocks.debug`` and keeping latest
  session was rewritten using ``:process-request`` hook.

0.14.1 (2017-09-22)
===================

* Added function ``weblocks.request:remove-request-header``.
* Added a hook ``(:reset-session session)``, which is
  called around a code for clearing given session. Right now it is
  called only from ``weblocks.sessions:reset-latest-session``.

0.14.0 (2017-09-20)
===================

* ``html``, ``menu``, ``suggest`` and ``repl`` utilities
  were excluded.
* Code which was in ``request-handler.lisp``, was excluded
  from build and partly moved to ``request-handler2.lisp``.
* Added ``:stop-weblocks`` hook.
* Misc helper for repl were removed: ``sessions``,
  ``in-session`` and ``pt``. May be the will be restored in separate
  package.
* Page boilerplate rendering method ``render-page`` now
  does not use complex templating with contextes.
* Symbols refactoring:
  * ``*style-warn-on-circular-dirtying*`` variable ->
    ``weblocks.variables``;
  * ``*style-warn-on-late-propagation*`` variable ->
    ``weblocks.variables``;
  * ``gen-id`` function -> ``weblocks.session``;
  * ``send-script`` function -> ``weblocks.response``;
  * ``with-html-form`` macro -> ``weblocks-ui``;
  * ``*approved-return-codes*`` variable -> ``weblocks.variables``;
  * ``handle-ajax-request`` method -> ``weblocks.request-handler``;
  * ``update-location-hash-dependents`` function ->
    ``weblocks.request-handler``.
  * ``render-link`` function was moved to ``weblocks-ui.form`` in
    separate system.

0.13.11 (2017-09-12)
====================

* Added ``weblocks.hooks:call-hook`` helper.
* Now ``call-next-hook`` is called automatically if it
  wasn't called explicitly.

0.13.10 (2017-09-06)
====================
  
Changes in weblocks.request-hooks:
----------------------------------

* Package ``weblocks.request-hooks`` was renamed to ``weblocks.hooks``.
* Macro ``with-dynamic-hooks`` was renamed to ``with-hook``.
* Functions add-application-hook, add-session-hook, add-request-hook
  became a macroses and their argument lists were changed. Now the
  should be used like:

  .. code-block:: lisp
       
     (weblocks.hooks:add-session-hook
              :some-hook
              my-beautiful-callback (param)
            (do-something-useful-with param))

  ``weblocks.request-hooks:eval-hooks`` was renamed to
  ``weblocks.hooks:call`` and now can be called with params:

  .. code-block:: lisp

     (weblocks.hooks:call :some-hook
           first-param
           second-param)
           
0.13.10 (2017-09-06)
====================

* Added ``:handle-request`` dynamic hook called around request handling code.

  Called when ``weblocks.request:*request*`` and ``weblocks.session:*session*`` are already bound.

0.13.9 (2017-09-02)
===================

* Added integrity field for remove javascript dependencies.
  Also, ``get-cross-origin`` and ``:cross-origin`` were removed to
  ``get-crossorigin`` and ``:crossorigin``, to conform the html
  attibute's spelling.
  
0.13.8 (2017-09-02)
===================

* Fixed error on ``(weblocks:redirect...)`` call.
* Fixed dependency handling in ajax requests.
* Now if unhandled exception occure, Woo's handler remains
  working. Previously, handler's thread shut down on any unhandled exception.
* Ajax progress now does not inserted into the document,
  but if element with id ``ajax-progress`` is present, it will be shown
  and hidden by jQuery's ``.show`` and ``.hide`` methods. Also, they
  take optional speed parameters from ``data-*`` attributes
  ``data-show-speed`` and ``data-hide-speed``.

* Reformatted documentation. Started to `keep a changelog
  <http://keepachangelog.com/>`_.
  
0.13.7 (2017-04-15)
===================

* Previous history wasn't tracked.
