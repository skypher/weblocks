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

   TODO> (defwebapp tasks)
   TODO> (start-webapp 'tasks)
   TODO> (weblocks.server:start-weblocks)

Open `http://localhost:8080/tasks/`_ in your browser and you'll see a
text like that:

  No init-user-session

  Please create a function to initialize a session and pass it to the
  defwebapp as :init-user-session argument.

  It could be something simple, like this one:

  (defun init-user-session (root)
    (setf (widget-children root)
          (lambda ()
            (with-html
               (:h1 "Hello world!")))))
               
  Read more in documentaion.

It means that you didn't write any code for your application. Let's do
it now and make an application which outputs a list of tasks.

.. code:: common-lisp-repl

   TODO> (defun init-user-session (root)
           (let ((tasks '("Make my first app in Weblocks"
                          "Deploy it somewhere"
                          "Have a profit")))
             (setf (widget-children root)
                   (lambda ()
                     (with-html
                       (:h1 "Tasks")
                       (:ul :class "tasks"
                            (loop for task in tasks
                                  do (cl-who:htm (:li (str task))))))))))

This code defines a simple list to store our tasks in memory. And also
defines a handler to render this list into a HTML.

Now let's make it alive by providing a form to add new tasks into the list.
