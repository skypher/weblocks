;; Part 1
(declaim (optimize (debug 3)))

(ql:quickload '(:weblocks :weblocks-ui :find-port))
(defpackage todo
  (:use #:cl
        #:weblocks-ui/form
        #:weblocks/html)
  (:import-from #:weblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:weblocks/actions
                #:make-js-action)
  (:import-from #:weblocks/app
                #:defapp))
(in-package todo)


(defapp tasks)

(weblocks/debug:on)

(defvar *port* (find-port:find-port))

(weblocks/app:start 'tasks)
(weblocks/server:start :port *port*)

;; Part 2: defining tasks


(defwidget task ()
    ((title
      :initarg :title
      :reader title)
     (done
      :initarg :done
      :initform nil
      :accessor done)))

(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type t)
    (format stream "~a, done ? ~a" (title task) (done task))))

(defvar *task-1* (make-instance 'task :title "Make my first Weblocks app"))

(defmethod render ((task task))
  (with-html
    (:span (if (done task)
               (with-html
                 (:s (title task)))
               (title task)))))


(defun make-task (&key title done)
  "Create a task."
  (make-instance 'task :title title :done done))

(defwidget task-list ()
  ((tasks
    :initarg :tasks
    :accessor tasks)))

(defmethod render ((widget task-list))
  (with-html
    (:h1 "Tasks")
    (:ul
     (loop for task in (tasks widget) do
          (:li (render task))))))

(defvar *root* nil
  "Our root widget.")

(defmethod weblocks/session:init ((app tasks))
  (declare (ignorable app))
  (let ((tasks (list (make-task :title "Make my first Weblocks app")
                     (make-task :title "Deploy it somewhere")
                     (make-task :title "Have a profit"))))
    (setf *root* (make-instance 'task-list :tasks tasks))))

(weblocks/debug:reset-latest-session)


;; Part 3: add-task

(defun add-task (&rest rest &key title &allow-other-keys)
  (push (make-task :title title) (tasks *root*))
  (update *root*))

(defmethod render ((widget task-list))
  (with-html
    (:h1 "Tasks")
    (loop for task in (tasks widget) do
         (render task))
    (with-html-form (:POST #'add-task)
      (:input :type "text"
              :name "title"
              :placeholder "Task's title")
      (:input :type "submit"
              :value "Add"))))

(weblocks/debug:reset-latest-session)


;; Part 4: toggle

(defmethod toggle ((task task))
  (setf (done task)
        (if (done task)
            nil
            t))
  (update task))

(defmethod render ((task task))
  (with-html
    (:p (:input :type "checkbox"
                :checked (done task)
                :onclick (make-js-action
                          (lambda (&rest rest)
                            (declare (ignore rest))
                            (toggle task))))
        (:span (if (done task)
                   (with-html
                     (:s (title task)))
                   (title task))))))


(weblocks/debug:reset-latest-session)
