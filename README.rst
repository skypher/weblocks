=================
 weblocks
=================

.. insert-your badges like that:

.. image:: https://travis-ci.org/40ants/cl-hamcrest.svg?branch=master
    :target: https://travis-ci.org/40ants/cl-hamcrest

.. Everything starting from this commit will be inserted into the
   index page of the HTML documentation.
.. include-from

Give some introduction.

Reasoning
=========

Explain why this project so outstanding and why it
was created.

You can give some examples. This is how common lisp
code should be formatted:

.. code-block:: common-lisp

   (defvar log-item '(:|@message| "Some"
                      :|@timestamp| 122434342
                      ;; this field is wrong and
                      ;; shouldn't be here
                      :|@fields| nil))

And this is how you can provide REPL examples:

.. code-block:: common-lisp-repl

   TEST> (format nil "Blah minor: ~a"
                     100500)
   "Blah minor: 100500"

Roadmap
=======

Provide a Roadmap.

.. Everything after this comment will be omitted from HTML docs.
.. include-to

Building Documentation
======================

Provide instruction how to build or use your library.

How to build documentation
--------------------------

To build documentation, you need a Sphinx. It is
documentaion building tool written in Python.

To install it, you need a virtualenv. Read
this instructions
`how to install it
<https://virtualenv.pypa.io/en/stable/installation/#installation>`_.

Also, you'll need a `cl-launch <http://www.cliki.net/CL-Launch>`_.
It is used by documentation tool to run a script which extracts
documentation strings from lisp systems.

Run these commands to build documentation::

  virtualenv env
  source env/bin/activate
  pip install -r docs/requirements.txt
  invoke build_docs

These commands will create a virtual environment and
install some python libraries there. Command ``invoke build_docs``
will build documentation and upload it to the GitHub, by replacing
the content of the ``gh-pages`` branch.


License
=======

Licensed under the LLGPL License.
