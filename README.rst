==========
 Weblocks
==========

.. insert-your badges like that:

.. image:: https://travis-ci.org/40ants/weblocks.svg?branch=master
    :target: https://travis-ci.org/40ants/weblocks

.. Everything starting from this commit will be inserted into the
   index page of the HTML documentation.
.. include-from

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

          (weblocks-test:test-weblocks)

or

.. code:: common-lisp

          (lift:run-tests :suite 'WEBLOCKS-SUITE)

Single test:

.. code:: common-lisp
          
          (lift:run-test :name 'weblocks-test::dataform-i18n-1
                         :suite :widgets/data-editor-suite)


Changes from master branch
==========================

Added
-----

* make-js-action function

Removed
-------

* with-extra-tags macro
* render-extra-tags generic

License
=======

Licensed under the LLGPL License.
