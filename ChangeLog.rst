===========
 ChangeLog
===========

* :release:`0.14.0 <2017-09-20>`
* :support:`-` ``html``, ``menu``, ``suggest`` and ``repl`` utilities
  were excluded.
* :support:`-` Code which was in ``request-handler.lisp``, was excluded
  from build and partly moved to ``request-handler2.lisp``.
* :feature:`-` Added ``:stop-weblocks`` hook.
* :support:`-` Misc helper for repl were removed: ``sessions``,
  ``in-session`` and ``pt``. May be the will be restored in separate
  package.
* :support:`-` Page boilerplate rendering method ``render-page`` now
  does not use complex templating with contextes.
* :support:`-` Symbols refactoring:
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

* :release:`0.13.11 <2017-09-12>`

* :feature:`-` Added ``weblocks.hooks:call-hook`` helper.
* :feature:`-` Now ``call-next-hook`` is called automatically if it
  wasn't called explicitly.

* :release:`0.13.10 <2017-09-06>`
  
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
           
* :release:`0.13.10 <2017-09-06>`
* Added ``:handle-request`` dynamic hook called around request handling code.

  Called when ``weblocks.request:*request*`` and ``weblocks.session:*session*`` are already bound.

* :release:`0.13.9 <2017-09-02>`
* :feature:`-` Added integrity field for remove javascript dependencies.
  Also, ``get-cross-origin`` and ``:cross-origin`` were removed to
  ``get-crossorigin`` and ``:crossorigin``, to conform the html
  attibute's spelling.
* :release:`0.13.8 <2017-09-02>`
* :bug:`-` Fixed error on ``(weblocks:redirect...)`` call.
* :bug:`-` Fixed dependency handling in ajax requests.
* :feature:`-` Now if unhandled exception occure, Woo's handler remains
  working. Previously, handler's thread shut down on any unhandled exception.
* :feature:`-` Ajax progress now does not inserted into the document,
  but if element with id ``ajax-progress`` is present, it will be shown
  and hidden by jQuery's ``.show`` and ``.hide`` methods. Also, they
  take optional speed parameters from ``data-*`` attributes
  ``data-show-speed`` and ``data-hide-speed``.

* :feature:`-` Reformatted documentation. Started to `keep a changelog
  <http://keepachangelog.com/>`_.
* :release:`0.13.7 <2017-04-15>`
* :bug:`-` Previous history wasn't tracked.
