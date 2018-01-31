;; Part 1
(declaim (optimize (debug 3)))

(ql:quickload '(:weblocks :weblocks-ui :find-port))
(defpackage todo
  (:use #:cl
        #:weblocks-ui/form
        #:weblocks/html)
  (:import-from #:weblocks/app
                #:defapp))
(in-package todo)


(defapp tasks)

(weblocks/debug:on)

(defvar *port* (find-port:find-port))
(weblocks/server:start :port *port*)

;; Part 2

(defmethod weblocks/session:init ((app tasks))
  (let ((tasks '("Make my first app in Weblocks"
                 "Deploy it somewhere"
                 "Have a profit")))
    (lambda ()
      (with-html
        (:h1 "Tasks")
        (:ul :class "tasks"
             (loop for task in tasks
                   do (with-html
                        (:li task))))))))

(weblocks/debug:reset-latest-session)


;; Part 3

(defmethod weblocks/session:init ((app tasks))
  (let ((tasks '("Make my first app in Weblocks"
                 "Deploy it somewhere"
                 "Have a profit")))
    (flet ((add-task (&key task &allow-other-keys)
             (push task tasks)
             (weblocks/widget:update
                 (weblocks/widgets/root:get))))
      (lambda ()
        (with-html
          (:h1 "Tasks")
          (:ul :class "tasks"
               (loop for task in tasks
                     do (with-html
                          (:li task))))
          (with-html-form (:POST #'add-task)
            (:input :type "text"
                    :name "task"
                    :placeholder "Task's title")
            (:input :type "submit"
                    :value "Add")))))))

(weblocks/debug:reset-latest-session)


;; Part 4

(defstruct task
  (title)
  (done))


(defmethod weblocks/session:init ((app tasks))
  (let ((tasks (list (make-task :title "Make my first app in Weblocks" :done t)
                     (make-task :title "Deploy it somewhere" :done nil)
                     (make-task :title "Have a profit" :done nil))))
    (labels ((redraw ()
               (weblocks/widget:update
                   (weblocks/widgets/root:get)))
             (add-task (&rest rest &key task &allow-other-keys)
               (log:info "Pushing" task "to" tasks rest)
               (push (make-task :title task :done nil) tasks)
               (redraw))
             (toggle-task (task)
               (setf (task-done task)
                     (if (task-done task)
                         nil
                         t))
               (redraw))
             (render-task (task)
               (let ((title (task-title task))
                     (done (task-done task)))
                 (with-html
                   (:p (:input :type "checkbox"
                               :checked done
                               :onclick (weblocks/actions:make-js-action
                                         (lambda (&rest rest)
                                           (declare (ignore rest))
                                           (toggle-task task))))
                       (:span (if done
                                  (with-html (:s title))
                                  title)))))))
      (lambda ()
        (with-html
          (:h1 "Tasks")
          (:div :class "tasks"
                (loop for task in tasks
                      do (with-html (render-task task))))
          (with-html-form (:POST #'add-task)
            (:input :type "text"
                    :name "task"
                    :placeholder "Task's title")
            (:input :type "submit"
                    :value "Add")))))))

(weblocks/debug:reset-latest-session)
