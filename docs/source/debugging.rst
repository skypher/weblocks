===========
 Debugging
===========

To make interactive debugger popup in Slime, set
``weblocks.variables:*catch-errors-p*`` to ``nil``. Otherwise, weblocks will catch
an error and render ``HTTP 500 Unhandled error`` page. This option is
``t`` by default.

You can also control verbosity level of 500 page by setting
``*show-lisp-errors-p*`` variable. If it is ``nil``, then no error and
traceback will be shown.

.. note:: TODO: Check if *show-lisp-errors-p* value is not inverted.
          Probably there is a mistake in the code.
