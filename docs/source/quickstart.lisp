;; Part 1
(declaim (optimize (debug 3)))

(ql:quickload :weblocks)

(defpackage todo (:use :cl :weblocks))
(in-package todo)


(defwebapp tasks)
(start-webapp 'tasks)
(weblocks.server:start-weblocks)

(setf weblocks.variables:*catch-errors-p* nil)


;; Part 2

(defun init-user-session (root)
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

(defwebapp tasks
     :init-user-session #'init-user-session)

(progn (weblocks:restart-webapp 'tasks)
       (weblocks.session:reset-latest-session))


;; Part 3

(defun init-user-session (root)
  (let ((tasks '("Make my first app in Weblocks"
                 "Deploy it somewhere"
                 "Have a profit")))
    (flet ((add-task (&rest rest &key task &allow-other-keys)
             (push task tasks)
             (mark-dirty root)))
      (setf (widget-children root)
            (lambda ()
              (with-html
                (:h1 "Tasks")
                (:ul :class "tasks"
                     (loop for task in tasks
                           do (cl-who:htm (:li (str task)))))
                (with-html-form (:POST #'add-task)
                  (:input :type "text"
                          :name "task"
                          :placeholder "Task's title")
                  (:input :type "submit"
                          :value "Add"))))))))

(progn (weblocks:restart-webapp 'tasks)
       (weblocks.session:reset-latest-session))


;; Part 4

(defstruct task
  (title)
  (done))


(defun init-user-session (root)
  (let ((tasks (list (make-task :title "Make my first app in Weblocks" :done t)
                     (make-task :title "Deploy it somewhere" :done nil)
                     (make-task :title "Have a profit" :done nil))))
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
                      (loop for task in tasks
                            do (cl-who:htm (render-task task))))
                (with-html-form (:POST #'add-task)
                  (:input :type "text"
                          :name "task"
                          :placeholder "Task's title")
                  (:input :type "submit"
                          :value "Add"))))))))

(progn (weblocks:restart-webapp 'tasks)
       (weblocks.session:reset-latest-session))

