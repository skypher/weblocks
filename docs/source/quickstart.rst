============
 Quickstart
============

Load weblocks and create a package for a sandbox:

.. code:: common-lisp-repl
          
   CL-USER> (ql:quickload :weblocks)
   CL-USER> (defpackage todo (:use :cl :weblocks))
   #<PACKAGE "TODO">
   CL-USER> (in-package todo)
   #<PACKAGE "TODO">

Now, create an application:

.. code:: common-lisp-repl

   TODO> (defwebapp todo)
   NIL
   TODO> (start-webapp 'todo)

