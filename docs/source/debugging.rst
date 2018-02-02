===========
 Debugging
===========

To make interactive debugger popup in Slime, set
``weblocks.variables:*catch-errors-p*`` to ``nil``. Otherwise, weblocks will catch
an error and render ``HTTP 500 Unhandled error`` page. This option is
``t`` by default.
