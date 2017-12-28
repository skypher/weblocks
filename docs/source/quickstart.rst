============
 Quickstart
============

Load weblocks and create a package for a sandbox:

.. code-block:: common-lisp-repl

   CL-USER> (ql:quickload :weblocks)
   CL-USER> (defpackage todo (:use :cl :weblocks))
   #<PACKAGE "TODO">
   CL-USER> (in-package todo)
   #<PACKAGE "TODO">

Now, create an application:

.. code-block:: common-lisp-repl

   TODO> (defwebapp tasks)
   TODO> (start-webapp 'tasks)
   TODO> (weblocks.server:start-weblocks)

Open `<http://localhost:8080/tasks/>`_ in your browser and you'll see a
text like that::

  No init-user-session

  Please create a function to initialize a session and pass it to the
  defwebapp as :init-user-session argument.

  It could be something simple, like this one:

  (defun init-user-session (root)
    (setf (widget-children root)
          (lambda ()
            (with-html
               (:h1 "Hello world!")))))

  (defwebapp your-app
     ;; some-options
     :init-user-session 'init-user-session)

  Read more in documentaion.

It means that you didn't write any code for your application. Let's do
it now and make an application which outputs a list of tasks.

In the end, we'll build the mandatory TODO-list app:

.. image:: _static/quickstart-check-task.gif
   :align: center
   :alt: the TODO-list app in Weblocks.

.. code-block:: common-lisp-repl

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
   TODO> (defwebapp tasks
           :init-user-session #'init-user-session)

This code defined a list of tasks. For simplicity, they are defined as a
list in a memory. Then it renders these tasks as HTML ``ul`` block.

Restart the application:

.. code-block:: common-lisp-repl

   TODO> (progn (weblocks:restart-webapp 'tasks)
                (weblocks.debug:reset-latest-session))

Right now it should look like this:

.. image:: _static/quickstart-list.png
   :align: center
   :alt: Our first list of tasks.
   :width: 400px


Now, we'll add some ability to interact with a list – to add some tasks
into it, like so:

.. image:: _static/quickstart-add-task.gif
   :align: center
   :alt: Adding tasks in our TODO-list interactively.

Import a new module, ``weblocks-ui`` to help in creating forms and other UI elements:

.. code-block:: common-lisp-repl

   TODO> (ql:quickload "weblocks-ui")
   TODO> (use-package :weblocks.ui.form)

Write a new ``init-user-session`` in the repl:

.. code-block:: common-lisp-repl

   TODO> (defun init-user-session (root)
           (let ((tasks (reverse '("Make my first app in Weblocks"
                                   "Deploy it somewhere"
                                   "Have a profit"))))
             (flet ((add-task (&rest rest &key task &allow-other-keys)
                      (push task tasks)
                      (mark-dirty root)))
               (setf (widget-children root)
                     (lambda ()
                       (with-html
                         (:h1 "Tasks")
                         (:ul :class "tasks"
                              (loop for task in (reverse tasks)
                                    do (cl-who:htm (:li (str task)))))

                         (with-html-form (:POST #'add-task)
                           (:input :type "text"
                                   :name "task"
                                   :placeholder "Task's title")
                           (:input :type "submit"
                                   :value "Add"))))))))

Pay attention to two new blocks in this code. Now it has the inner function
``add-task``:

.. code-block:: common-lisp

   (add-task (&rest rest &key task &allow-other-keys)
     (push task tasks)
     (mark-dirty root))

It does only two simple things:

- it adds a task into a list;
- it tells Weblocks that the page is dirty and should be redrawn.

This second point is really important because it allows Weblocks to render
necessary parts of the page on the server and to inject it into the HTML DOM
in the browser. Here it rerenders the whole page, but later you'll see that
the same technic can be used to update smaller pieces, called :ref:`widgets`.

Another block in our new version of init-user-session is the form:

.. code-block:: common-lisp

   (with-html-form (:POST #'add-task)
      (:input :type "text"
       :name "task"
       :placeholder "Task's title")
      (:input :type "submit"
       :value "Add"))

It defines a text field, a submit button and an action to perform on
form submit.

.. note:: This is really amazing!

          With Weblocks, you can handle all the business logic
          server-side, because an action can be any lisp function, even an
          anonymous lambda, closuring all necessary variables.

Restart the application and reload the page. Test your form now and see in a
`Webinspector`_ how Weblocks sends requests to the server and receives
HTML code with rendered HTML block.

Now we'll make our application really useful – wekll add code to toggle tasks:

.. code-block:: common-lisp-repl

   TODO> (defstruct task
           (title)
           (done))

   TODO> (defun init-user-session (root)
           (let ((tasks (reverse (list (make-task :title "Make my first app in Weblocks" :done t)
                                       (make-task :title "Deploy it somewhere" :done nil)
                                       (make-task :title "Have a profit" :done nil)))))
             (labels ((add-task (&rest rest &key task &allow-other-keys)
                        (log:info "Pushing" task "to" tasks rest)
                        (push (make-task :title task :done nil) tasks)
                        (mark-dirty root))
                      (toggle-task (task)
                        (setf (task-done task)
                              (if (task-done task)
                                  nil
                                  t))
                        (mark-dirty root))
                      (render-task (task)
                        (let ((title (task-title task))
                              (done (task-done task)))
                          (with-html
                            (:p (:input :type "checkbox"
                                        :checked done
                                        :onclick (weblocks::make-js-action
                                                  (lambda (&rest rest)
                                                    (declare (ignore rest))
                                                    (toggle-task task))))
                                (:span (if done
                                           (cl-who:htm (:strike (str title)))
                                           (str title))))))))
               (setf (widget-children root)
                     (lambda ()
                       (with-html
                         (:h1 "Tasks")
                         (:div :class "tasks"
                               (loop for task in (reverse tasks)
                                     do (cl-who:htm (render-task task))))
                         (with-html-form (:POST #'add-task)
                           (:input :type "text"
                                   :name "task"
                                   :placeholder "Task's title")
                           (:input :type "submit"
                                   :value "Add"))))))))

This code has the following significant changes:

* Now we store our tasks as structures to be able to change their state
  easily:

  .. code-block:: common-lisp

     (defstruct task
        (title)
        (done))

  And now they have the additional attribute ``done`` for indication if we're
  done with a task or not.

* The next change is a small helper to toggle the ``done`` attribute:

  .. code-block:: common-lisp

     (toggle-task (task)
        (setf (task-done task)
        (if (task-done task)
          nil
          t))
        (mark-dirty root))

* And finally, we've modified our task rendering function by adding a
  code to render a checkbox with an anonymous lisp function, attached to
  its ``onclick`` attribute:

  .. code-block:: common-lisp

     (with-html
        (:p (:input :type "checkbox"
                    :checked done
                    :onclick (weblocks::make-js-action
                              (lambda (&rest rest)
                                (declare (ignore rest))
                                (toggle-task task))))
            (:span (if done
                       (cl-who:htm (:strike (str title)))
                       (str title)))))

  The function ``make-js-action`` returns a Javascript code, which
  calls back a lisp lambda function when evaluated in the browser.
  And because ``toggle-task`` marks the root widget as "dirty", Weblocks
  returns on this callback a new prerendered HTML with all tasks.
  Next I'll show how to rerender only a single task on such changes.

What is next?
=============

As a homework:

1. Play with lambdas and add a "Delete" button next after
   each task.
2. Add the ability to sort tasks by name or by completion flag.
3. Read the rest of the documentation and make a real application, using the full
   power of Common Lisp.

.. _Webinspector: https://developers.google.com/web/tools/chrome-devtools/inspect-styles/
