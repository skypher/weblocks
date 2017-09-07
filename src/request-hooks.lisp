(defpackage #:weblocks.hooks
  (:use #:cl
        #:f-underscore)
  (:export
   #:add-application-hook
   #:add-request-hook
   #:add-session-hook
   #:with-hooks
   #:eval-hooks
   #:with-dynamic-hooks
   #:log-hooks
   #:eval-dynamic-hooks))
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
;; should be bound by with-hooks macro, during request processing.
(defvar *session-hooks* nil
        "A session hooks object is stored in user's session and should be
bound to this variable by `with-hooks' macro.")


(defvar *request-hooks* nil
  "A request hooks object used in the request scope.")


;; internal function
(defun add-hook (hooks hook-name callback)
  "Adds a callback to the callbacks list with name `hook-name'."
  (check-type hooks hooks)
  (check-type hook-name symbol)
  (check-type callback (or symbol function))

  (let ((hooks-hash (slot-value hooks 'hooks)))
    (pushnew callback
             (gethash hook-name hooks-hash))))


(defun add-session-hook (name callback)
  (add-hook *session-hooks* name callback))


(defun add-application-hook (name callback)
  (add-hook *application-hooks* name callback))


(defun add-request-hook (name callback)
  (add-hook *request-hooks* name callback))


(defmacro with-hooks (&body body)
  "Prepares internal special variables for request processing.

It takes hooks from user session and creates an empty hooks
list bound to a current request."
  `(let ((*request-hooks* (make-instance 'hooks))
         (*session-hooks* (get-or-create-session-hooks)))
     ,@body))


(defun get-callbacks (hooks name)
  "Internal function to get callbacks list from a hooks storage."
  (check-type hooks (or hooks null))
  (check-type name symbol)
  (when hooks
    (let* ((hash (slot-value hooks 'hooks))
           (callbacks (gethash name hash)))
      callbacks)))


(defun eval-hook-callbacks (hooks name args)
  "Internal function to eval hooks from all three hook storages."
  (check-type hooks hooks)
  (check-type name symbol)

  (let* ((hash (slot-value hooks 'hooks))
         (callbacks (gethash name hash)))
    (loop for callback in callbacks
          do (progn
               (log:debug "Calling" callback)
               (apply callback args)))))


;; external
(defun eval-hooks (name &rest args)
  (eval-hook-callbacks *application-hooks*
                       name
                       args)
  (eval-hook-callbacks *session-hooks*
                       name
                       args)
  (eval-hook-callbacks *request-hooks*
                       name
                       args))


(defun log-hooks (name)
  "A helper function to log all known hooks with given name."
  (mapc (f_ (log:debug "Application hook" _))
        (get-callbacks *application-hooks* name))
  (mapc (f_ (log:debug "Session hook" _))
        (get-callbacks *session-hooks* name))
  (mapc (f_ (log:debug "Request hook" _))
        (get-callbacks *request-hooks* name)))


(defmacro with-dynamic-hooks ((name) &rest body)
  "Performs nested calls of all the hooks of name, the innermost call is
   a closure over the body expression.  Dynamic action hooks take one
   argument, which is a list of dynamic hooks.  In the inner context, they
   apply the first element of the list to the rest

   An example of a dynamic hook:
  
   (defun transaction-hook (inner-fns)
     (with-transaction ()
       (unless (null inner-fns)
         (funcall (first inner-fns) (rest inner-fns)))))"
  (metatilities:with-gensyms (null-list)
    `(eval-dynamic-hooks 
      (append (get-callbacks *application-hooks* ,name)
              (get-callbacks *session-hooks* ,name)
              (get-callbacks *request-hooks* ,name)
              (list (lambda (,null-list) 
                      (assert (null ,null-list))
                      ,@body))))))

(defun eval-dynamic-hooks (var)
  "Helper function that makes it easier to write dynamic hooks.

Call it at the end of your dynamic hook, to evaluate inner
hooks:

   (defun my-hook (hooks)
     (with-my-context ()
        (eval-dynamic-hooks hooks)))
  "
  (let ((list (etypecase var 
                (symbol (symbol-value var))
                (list var))))
    (unless (null list)
      (funcall (first list) (rest list)))))

;; Hooks for using html parts, reset parts set before render and save it to session after render 
;; Allow to modify html parts set when is in debug mode

;; Раньше все pushnew были завёрнуты в этот eval-when,
;; не знаю зачем
;; (eval-when (:load-toplevel))
;; кроме того, pushnew не работает и всё равно при повторном евале добавляет в словарь
;; *application-request-hooks* дубликаты функций, а вот если 

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

(add-application-hook :pre-render
                      'reset-html-parts)

(add-application-hook :post-render
                      'update-html-parts)
