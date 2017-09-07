===============
 Request Hooks
===============

Hook is a named callbacks list. Depending on name, callbacks are
called in different places though request processing pipeline.

Callback can be bound to ``application``, ``session`` or
``request``. They are added via these functions
``weblocks.hooks:add-application-hook``,
``weblocks.hooks:add-session-hook`` and
``weblocks.hooks:add-request-hook``.

Session and request hooks can be added only during request processing
stage and are called only for current session or a request respectively.

Application hooks can be added at any time and are called for each user
session and request.

You can define hooks with custom names. Hook's name is any symbol.

Here are hook names, defined by Weblocks:

* ``:dynamic-action`` – A set of functions that establish dynamic state around a body function in the action context.
* ``:pre-action`` – A list of callback functions of no arguments called before user action is evaluated.
* ``:post-action`` – A list of callback functions of no arguments called after user action is evaluated.
* ``:dynamic-render`` – A set of functions that establish dynamic state around a body function in the render context.
* ``:pre-render`` – A list of callback functions of no arguments called before widgets are rendered.
* ``:post-render`` – A list of callback functions of no arguments called
  after widgets are rendered.
* ``:handle-request`` – Dynamic hook called around request handling
  code. Called when ``weblocks.request:*request*`` and ``weblocks.session:*session*``
  are already bound.

TODO: Add example of a custom hook.

TODO: Add description how dynamic hooks are working.

.. cl:in-package:: weblocks
                   
.. cl:class:: request-hooks
