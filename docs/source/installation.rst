==============
 Installation
==============

Setup your Common Lisp environment
==================================

You can skip this section if you already have a comfortable Common Lisp environment.


Common Lisp implementation
--------------------------

First you must choose a Common Lisp implementation.

Weblocks is designed and implemented to be portable and should run on the most popular
Common Lisp implementations. It is currently tested best on SBCL and Clozure CL, though.


Development environment setup
-----------------------------

There are at least two fundamentally different development approaches for using
Common Lisp:

* Editor-centric development: you access all Lisp functions from within your editor.

  Example: Emacs/Slime, Vim/Slimv, Lispworks/IDE.
    
  Incremental development happens mainly on the S-Expression level. This means that you
  edit a SEXP and send it with the help of editor directly to your Lisp image, which
  evaluates it, thus affecting the current Lisp environment.
  
* UNIX-style development: one tool for each job. The editor is not all that important
  here (as long as you're comfortable with it and it supports at least a basic level
  of paren highlighting.
    
  Example: Vim and your favorite terminal emulator. You start Vim in one window and
  your Lisp in another. Interaction happens by reloading your applications ASDF system
  and simple copy/paste of snippets.

We will try to be largely agnostic of the development approach in this manual which actually
means that we tend towards the second approach: Lisp calls are referred to by what you'd
type in your REPL, not by Emacs shortcut as it is often the case.

For a basic comfortable SBCL setup, see :ref:`Basic SBCL setup`.
 

Installation
------------

There is a comprehensive installation guide on the main web site:

http://weblocks-framework.info/installation

