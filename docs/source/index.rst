=========================================
 Welcome to weblocks's documentation!
=========================================

This is an implementation of Hamcrest for Common Lisp.

Here are some examples
----------------------


.. code-block:: common-lisp-repl

   GIT> (assert-that
          log-item
          (has-plist-entries :|@message| "Some"
                             :|@timestamp| _)
          (hasnt-plist-keys :|@fields|))

.. include:: ../../README.rst
   :start-after: include-from
   :end-before: include-to

Contents
========

.. toctree::
   :maxdepth: 2

   api
   changelog


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`


