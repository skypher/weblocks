=======================
 html-parts.lisp logic
=======================

This file builds a cache of html pieces and their relations from each
other.

It stores pieces in ``*parts-md5-hash*`` hash where keys are md5 sums
from a piece of HTML and values are list. Head of these lists is a HTML
piece itself and rest – are md5 hashes of pieces which include this
HTML piece as a substring.


Some notes
==========

:cl:macro:`nested-html-part` is used in templates and html rendering code
to ???
