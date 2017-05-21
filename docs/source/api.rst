=====
 API
=====

Here you can describe an API.

Use `cldomain's <http://cldomain.russellsim.org>`_ directives, to insert
functions or macroses descriptions, like that:

.. cl:package:: weblocks
                
.. cl:function:: foo

.. cl:macro:: bar

   Also you can specify additional examples for some blocks:

   .. code-block:: common-lisp-repl

      TEST> (bar 1 2 3)

      (1 2 3)
      NIL

And because this is Sphinx, you can use it's great cross-referencing
features to link to functions. Here is the link to
:cl:function:`weblocks:foo`.

Read the documentation on `cldomain <http://cldomain.russellsim.org>`_ and
write your own beautiful docs!
