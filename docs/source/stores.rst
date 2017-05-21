========
 Stores
========

Weblocks offers a storage framework with several backends. At the
moment the supported backends are Elephant, cl-prevalence, CLSQL
and in-memory storage.
There is also weblocks-montezuma with supporting montezuma index as store 
and weblocks-custom which offers wrapping custom data structures 
into store models and their instances.

Weblocks standard widgets are tied to weblocks storage framework.

You are free to use your own storage mechanisms, however.

:ref:`Store comparison` chapter offers a comparison of the different stores.


Applications
============

One Lisp image may serve multiple web applications at once.

The target web application of a request can be determined based
on the hostname, the port or the URI for example.

You don't have to worry much about this if you only intend to
run a single application.


HTML and Javascript generation facilities and utilies
=====================================================

Weblocks comes with CL-WHO and Parenscript, two powerful packages
that provide Lisp compilers for HTML and Javascript.

This means no more munging around with strings, which in turn
means

* no more annoying mistakes like forgetting a semicolon
  or forgetting to close a tag

* no need to remember infix precedence rules 
  
* make full use of Lisps selective quoting facilities

Javascript may still be written as simple strings however.
This comes in handy sometimes when working with a larger example
copied from a Javascript library manual.

TODO: And you're free to use other facilities as you prefer.  For
example YACLML [#yaclml]_ is a function-based HTML generation facility
that you might want to take a look at.

Continuation-based tools
========================

Some neat features of Weblocks are built on continuations. But don't worry,
you will only need some high-level understanding of continuations to use
these features along with some rules of thumb.

.. rubric:: Footnotes

.. [#yaclml] Maciej Pasternacki wrote an
             http://www.3ofcoins.net/2009/02/07/yaclml-in-pictures-part-i-html-generation/,
             excellent introductory article) about YACLML where he also
             compares it with CL-WHO.


