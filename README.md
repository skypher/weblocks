Weblocks
========

[![Build Status](https://travis-ci.org/skypher/weblocks.svg?branch=master)](https://travis-ci.org/skypher/weblocks)

Weblocks is an advanced web framework written in Common Lisp.

[Offical Weblocks site](http://weblocks-framework.info)

Currently your web application should depend on :weblocks and :weblocks-prototype-js packages to work.
[Weblocks PrototypeJs backend](http://github.com/html/weblocks-prototype-js)

Move to Clack and Ningle
========================

Benefits
--------

Will be able to use different http servers, not only Hunchentoot.

Useful snippets
---------------

Use this:

    (lack.util:generate-random-id)
    
instead of:

    (md5 (hunchentoot::create-random-string 10 36))
    
in `(generate-action-code)`


What to replace
---------------

#### An URL routing

Seems easy with ningle:route.

#### An File Upload

How to do this with clack and ningle???

#### Session management

Need to replace

(hunchentoot:session-value ...)
hunchentoot:*session*
etc
