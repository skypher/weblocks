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
* make-root-widget generic

Removed
-------

* with-extra-tags macro
* render-extra-tags generic
* add-get-param-to-url and remove-parameter-from-uri functions

Renamed
-------

* get-request-action-name was renamed to get-action-name-from-request
* page-not-found-handler generic was moved to weblocks.request-handler
* variable weblocks:*catch-errors-p* moved to
  weblocks.variables:*catch-errors-p*
* slot-value-by-path, find-slot-dsd, find-slot-esd, object-class-name
  and slot-equal were moved from weblocks-util to weblocks.utils.clos.

Replacements
------------

* Function webapp-session-hash was replaced with just
  weblocks.session::*session*
* Function webapp-session-key replaced with weblocks.session:get-value
* Macro hook-by-scope now is a function.
* Macro request-hook now is a function, use add-request-hook instead of
  (push 'some-func (request-hook :session :post-action))
  

Testsuite refactoring
=====================

After some issues were resolved, I've got this result
running stefil tests::

  Test Report for WEBLOCKS-SUITE: 366 tests run, 732 Errors.

Started to port tests to Prove.

License
=======

Licensed under the LLGPL License.
