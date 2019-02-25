==========
 Weblocks
==========

.. insert-your badges like that:

.. image:: https://travis-ci.org/40ants/weblocks.svg?branch=reblocks
    :target: https://travis-ci.org/40ants/weblocks
    
.. image:: https://badges.gitter.im/40ants/weblocks.svg
    :target: https://gitter.im/40ants/weblocks

.. Everything starting from this commit will be inserted into the
   index page of the HTML documentation.
.. include-from

Contribution
============

Join our `Gitter Chat Room`_ to participate in the project or ask your questions about Weblocks.

This branch contains following branches
=======================================

* travis-config
* more-logging-and-js-backend-fix
* documentation


Roadmap
=======

* Move from raw Hunchentoot to Clack.
* Refactor dependencies processing.
* Write easy to understand, working tutorials.
* Make jquery backend default.
* Extract all widgets into a separate system
  and make them use bootstrap by default.

.. Everything after this comment will be omitted from HTML docs.
.. include-to

Moving to Clack and Ningle
==========================

Benefits
--------

Will be able to use different http servers, not only Hunchentoot.

Useful snippets
---------------

Use this::

    (lack.util:generate-random-id)
    
instead of::

    (md5 (hunchentoot::create-random-string 10 36))
    
in `(generate-action-code)`


What to replace
---------------

An File Upload
~~~~~~~~~~~~~~

How to do this with clack and ningle???


How to run tests
================

All tests:

.. code:: common-lisp

          (rove:run :weblocks-test)

or you can run test for some package:

.. code:: common-lisp

          (rove:run :weblocks-test/dependencies)

Single test:

.. code:: common-lisp

          (rove:run-test 'weblocks-test/dependencies::render-js-dependency)

From command line:

.. code:: bash

          rove weblocks.asd


Changes from master branch
==========================

There were very many refactorings. Check ``ChangeLog.rst`` file.
  

Testsuite refactoring
=====================

After some issues were resolved, I've got this result
running stefil tests::

  Test Report for WEBLOCKS-SUITE: 366 tests run, 732 Errors.

Started to port tests to Prove.

License
=======

Licensed under the LLGPL License.

.. _`Gitter Chat Room`: https://gitter.im/40ants/weblocks
