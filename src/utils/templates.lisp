(in-package :weblocks-util)

(wexport '(*templates* remove-template deftemplate render-wt-to-string render-wt)
         '(t util))

(defvar *templates* (make-hash-table))
(setf (documentation '*templates* 'variable)
      "Hash containing all templates. Hash key is template name, hash value is a cons with template and context arguments")

(defun remove-template (name function)
  "Removes template associated with name and function."
  (let ((templates (gethash name *templates*)))
    (setf templates 
          (remove function  templates
                  :key #'car
                  :test #'equal))
    (setf (gethash name *templates*) templates)))

(defun deftemplate (name function &rest context)
  "Associates function with template name. Template name is a keyword.
   Context arguments used to calculate effective template.
   :application-class, a symbol, this argument gives 10 points for template if its value equal to type of application used.
   :context-matches argument is a callback which will be called with template context during template calculation and should return points"
  (let ((templates (gethash name *templates*)))

    ; Handling redefining template
    (remove-template name function)

    (push (cons function context) templates)
    (setf (gethash name *templates*) templates)))

(defun calculate-template-priority (template actual-context)
  "Calculates priority for finding effective template."
  (let ((name (car template))
        (context (copy-tree (cdr template))))

    (or 
      (loop for (key value) on context :by #'cddr
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

(defun render-wt-to-string (template context &rest args)
  "Similar to render-wt but renders template to string instead of *weblocks-output-stream*"
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

    (nested-html-part 
      (list :type :template 
            :template-name template 
            :effective-template effective-template
            :template-context context)

      (update-current-html-part-children 
        (loop for (key value) on args by #'cddr 
              collect value))
      

      (apply 
        (car effective-template)
        args))))
