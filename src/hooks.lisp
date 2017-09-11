(defpackage #:weblocks.hooks
  (:use #:cl
        #:f-underscore)
  (:export
   #:add-application-hook
   #:add-request-hook
   #:add-session-hook
   #:prepare-hooks
   #:with-hook
   #:call-next-hook))
(in-package weblocks.hooks)


(defclass hooks ()
  ((hooks :initform (make-hash-table)))
  
  (:documentation "A data structure that maintains appropriate
  callback functions used to hook into request evaluation."))


(defparameter *application-hooks* (make-instance 'hooks)
  "A request hook object used in the application scope.")


(defun reset-session-hooks ()
  (let ((hooks (make-instance 'hooks)))
    (weblocks.session:set-value 'hooks hooks)
    hooks))


(defun get-or-create-session-hooks ()
  "A request hook object used in the session scope."
  (if (weblocks.session:get-value 'hooks)
      (weblocks.session:get-value 'hooks)
      (reset-session-hooks)))


;; Variables *session-hooks* or *request-hooks*
;; should be bound by prepare-hooks macro, during request processing.
(defvar *session-hooks* nil
        "A session hooks object is stored in user's session and should be
bound to this variable by `prepare-hooks' macro.")


(defvar *request-hooks* nil
  "A request hooks object used in the request scope.")


;; internal function
(defun add-hook (hooks hook-name callback-name callback)
  "Adds a callback to the callbacks list with name `hook-name'."
  (check-type hooks hooks)
  (check-type hook-name symbol)
  (check-type callback (or symbol function))
  (check-type callback-name symbol)

  (let ((hooks-hash (slot-value hooks 'hooks)))
    (if (assoc callback-name
               (gethash hook-name hooks-hash)
               :test #'eql)
        ;; replacing callback with given name
        (setf (cdr (assoc callback-name
                          (gethash hook-name hooks-hash)
                          :test #'eql))
              callback)
        ;; adding new callback
        (push (cons callback-name callback)
              (gethash hook-name hooks-hash)))))



(eval-when  (:compile-toplevel :load-toplevel :execute)
  (defun add-hook-helper (hook-storage hook-name callback-name args body)
    `(flet ((,callback-name (next-hooks ,@args)
              (declare (ignorable next-hooks))
              (flet ((call-next-hook ()
                       (eval-next-hooks next-hooks)))
                ,@body)))
       (add-hook ,hook-storage ,hook-name ',callback-name
                 #',callback-name))))


(defmacro add-session-hook (hook-name callback-name (&rest args) &body body)
  (add-hook-helper '*session-hooks*
                   hook-name
                   callback-name
                   args
                   body))


(defmacro add-application-hook (hook-name callback-name (&rest args) &body body)
  (add-hook-helper '*application-hooks*
                   hook-name
                   callback-name
                   args
                   body))


(defmacro add-request-hook (hook-name callback-name (&rest args) &body body)
  (add-hook-helper '*request-hooks*
                   hook-name
                   callback-name
                   args
                   body))


(defmacro prepare-hooks (&body body)
  "Prepares internal special variables for request processing.

It takes hooks from user session and creates an empty hooks
list bound to a current request."
  `(let ((*request-hooks* (make-instance 'hooks))
         (*session-hooks* (get-or-create-session-hooks)))
     ,@body))


(defun get-callback-name (callback)
  (car callback))


(defun get-callback-value (callback)
  (cdr callback))


(defun get-callbacks (hooks name)
  "Internal function to get callbacks list from a hooks storage."
  (check-type hooks (or hooks null))
  (check-type name symbol)
  (when hooks
    (let* ((hash (slot-value hooks 'hooks))
           (callbacks (gethash name hash)))
      (mapcar #'get-callback-value callbacks))))


(defun get-callbacks-names (hooks name)
  "Internal function to get a list of callbacks names from a hooks storage."
  (check-type hooks (or hooks null))
  (check-type name symbol)
  (when hooks
    (let* ((hash (slot-value hooks 'hooks))
           (callbacks (gethash name hash)))
      (mapcar #'get-callback-name callbacks))))


(defun eval-hook-callbacks (hooks name args)
  "Internal function to eval hooks from all three hook storages."
  (check-type hooks hooks)
  (check-type name symbol)

  (let* ((hash (slot-value hooks 'hooks))
         (callbacks (gethash name hash)))
    (dolist (callback callbacks)
      (log:debug "Calling" callback)
      (apply (get-callback-value callback) args))))


;; external

;; TODO: remove
;; (defun call (name &rest args)
;;   (eval-hook-callbacks *application-hooks*
;;                        name
;;                        args)
;;   (eval-hook-callbacks *session-hooks*
;;                        name
;;                        args)
;;   (eval-hook-callbacks *request-hooks*
;;                        name
;;                        args))


(defun log-hooks (name)
  "A helper function to log all known hooks with given name."
  (mapc (f_ (log:debug "Application hook" _))
        (get-callbacks *application-hooks* name))
  (mapc (f_ (log:debug "Session hook" _))
        (get-callbacks *session-hooks* name))
  (mapc (f_ (log:debug "Request hook" _))
        (get-callbacks *request-hooks* name)))


(defmacro with-hook ((name &rest args) &rest body)
  "Performs nested calls of all the hooks of name, the innermost call is
   a closure over the body expression.  Dynamic action hooks take one
   argument, which is a list of dynamic hooks.  In the inner context, they
   apply the first element of the list to the rest

   TODO: Add another example, using add-session-hook and
         call-next-hook.

   An example of a dynamic hook:
  
   (defun transaction-hook (inner-fns)
     (with-transaction ()
       (unless (null inner-fns)
         (funcall (first inner-fns) (rest inner-fns)))))"
  (metatilities:with-gensyms (null-list ignored-args)
    `(eval-next-hooks 
      (append (get-callbacks *application-hooks* ,name)
              (get-callbacks *session-hooks* ,name)
              (get-callbacks *request-hooks* ,name)
              (list (lambda (,null-list &rest ,ignored-args)
                      (declare (ignorable ,ignored-args))
                      (check-type ,null-list null)
                      ,@body)))
      ,@args)))


;; TODO: remove, was replaced with with-hook
;; (defmacro with-dynamic-hooks ((name) &rest body)
;;   "Performs nested calls of all the hooks of name, the innermost call is
;;    a closure over the body expression.  Dynamic action hooks take one
;;    argument, which is a list of dynamic hooks.  In the inner context, they
;;    apply the first element of the list to the rest

;;    An example of a dynamic hook:
  
;;    (defun transaction-hook (inner-fns)
;;      (with-transaction ()
;;        (unless (null inner-fns)
;;          (funcall (first inner-fns) (rest inner-fns)))))"
;;   (metatilities:with-gensyms (null-list)
;;     `(eval-dynamic-hooks 
;;       (append (get-callbacks *application-hooks* ,name)
;;               (get-callbacks *session-hooks* ,name)
;;               (get-callbacks *request-hooks* ,name)
;;               (list (lambda (,null-list) 
;;                       (assert (null ,null-list))
;;                       ,@body))))))

(defun eval-next-hooks (next-hooks &rest args)
  "Helper function that makes it easier to write dynamic hooks.

Call it at the end of your dynamic hook, to evaluate inner
hooks:

   (defun my-hook (hooks)
     (with-my-context ()
        (eval-dynamic-hooks hooks)))
  "
  (let ((list (etypecase next-hooks 
                (symbol (symbol-value var))
                (list next-hooks))))
    (unless (null list)
      (let ((current-hook (first list))
            (next-hooks (rest list)))
        (apply current-hook
               next-hooks
               args)))))

;; Hooks for using html parts, reset parts set before render and save it to session after render 
;; Allow to modify html parts set when is in debug mode

;; Раньше все pushnew были завёрнуты в этот eval-when,
;; не знаю зачем
;; (eval-when (:load-toplevel))
;; кроме того, pushnew не работает и всё равно при повторном евале добавляет в словарь
;; *application-request-hooks* дубликаты функций, а вот если 

;; TODO: move this hook to place where html parts are processed
(defun reset-html-parts ()
  (when (or weblocks::*weblocks-global-debug*
            (weblocks::webapp-debug))
    
    (log:debug "Resetting html parts cache")
    (weblocks.utils.html-parts:reset)))


(defun update-html-parts ()
  (when (or weblocks::*weblocks-global-debug*
            (weblocks::webapp-debug))
    (weblocks::timing "html parts processing"
      (progn 
        (weblocks.utils.html-parts:update-html-parts-connections)
        ;; Don't know why to do this,
        ;; because this is only place where weblocks.session:get-value
        ;; is called with this argument. Probably, these values
        ;; are never restored from the session.
        ;;
        ;; (setf (weblocks.session:get-value 'parts-md5-hash)
        ;;       weblocks-util:*parts-md5-hash*)
        ;; (setf (weblocks.session:get-value 'parts-md5-context-hash)
        ;;       weblocks-util:*parts-md5-context-hash*)
        ))))

(add-application-hook :render
    process-html-parts ()
  (reset-html-parts)
;;  (call-next-hook)
  (update-html-parts))

