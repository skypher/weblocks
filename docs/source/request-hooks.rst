===============
 Request Hooks
===============

Code is located at ``src/request-hooks.lisp``.

TODO: Understand how they work and how to use them. Or get rid of them
:)


Hooks are grouped by scopes:

application
  These are stored in ``*application-request-hooks*``.

session
  Returned in ``(session-request-hooks)`` which takes them from session hash.

request
  Stored in ``*request-hook*``.


All hooks of one scope are collected in class ``request-hooks`` which
stores hooks in separate slots.

.. cl:in-package:: weblocks
                   
.. cl:class:: request-hooks
