===========
 ChangeLog
===========

* :bug:`-` Fixed error on ``(weblocks:redirect...)`` call.
* :bug:`-` Fixed dependency handling in ajax requests.
* :feature:`-` Now if unhandled exception occure, Woo's handler remains
  working. Previously, handler's thread shut down on any unhandled exception.
* :feature:`-` Ajax progress now does not inserted into the document,
  but if element with id ``ajax-progress`` is present, it will be shown
  and hidden by jQuery's ``.show`` and ``.hide`` methods. Also, they
  take optional speed parameters from ``data-*`` attributes
  ``data-show-speed`` and ``data-hide-speed``.

* :feature:`-` Reformatted documentation. Started to `keep a changelog
  <http://keepachangelog.com/>`_.
* :release:`0.13.7 <2017-04-15>`
* :bug:`-` Previous history wasn't tracked.
