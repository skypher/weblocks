(in-package :weblocks-util)

(wexport '(*templates* deftemplate render-template-to-string)
         '(t util))

(defvar *templates* (make-hash-table))

(defun deftemplate (name function &rest context)
  (push (cons function context)
        (gethash name *templates*)))

(defun calculate-template-priority (template actual-context)
  "Calculates priority for finding effective template."
  (let ((name (car template))
        (context (copy-tree (cdr template))))

    (or 
      (loop for (key value) on context :by 'cddr
            sum (cond 
                  ((equal key :application-class)
                   (if (subtypep 
                         (type-of (funcall (intern "CURRENT-WEBAPP" "WEBLOCKS")))
                         value)
                     10 
                     0))
                  ((equal key :context-matches)
                   (apply value actual-context))
                  ((functionp value) 
                   (funcall value (getf actual-context key)))
                  (t 0)))
      0)))

(defun render-template-to-string (template context &rest args)
  "Calculates effective template and renders it to string.  
   Effective template is template with max priority."
  (let* ((templates (reverse (gethash template *templates*)))
         (context (append 
                    context 
                    (list :application (intern "CURRENT-WEBAPP" "WEBLOCKS"))))
         (effective-template 
           (car 
             (sort 
               (loop for i in templates collect 
                     (cons (car i) (calculate-template-priority i context)))
               #'>
               :key #'cdr))))

    (unless (car effective-template)
      (error "Cannot find template ~A" template))

    (apply 
      (car effective-template)
      args)))
