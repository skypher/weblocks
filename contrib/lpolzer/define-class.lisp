;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/define-class.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr  7 09:55:52 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                        Define-Class Functions
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2010, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; Define-class provides extended class options to defclass:
;;;
;;;   :export-class-name  -- exports the class-name symbol from the current 
;;;                          package
;;;   :export-accessors   -- exports the generated accessor symbols from the
;;;                          current package (does not export other accessors)
;;;   :export-slot-names  -- exports from the current package the slot-name
;;;                          symbols for specified direct slots
;;;                          (t => all direct slots |
;;;                           <direct-slot-names> |
;;;                           t :exclude ... => all direct slots except ...)
;;;   :generate-accessors -- generates accessors of the form 
;;;                          class-name.slot-name for specified direct slots 
;;;                          (t => all direct slots |
;;;                           <direct-slot-names> |
;;;                           t :exclude ... => all direct slots except ...)
;;;   :generate-accessors-format 
;;;                       -- the format of generated accessors:
;;;                            :prefix => <generate-accessors-prefix>slot-name
;;;                            :suffix => slot-name<generate-accessors-suffix>
;;;                          (the default value is :prefix)
;;;   :generate-accessors-prefix (ignored for :suffix generation)
;;;                       -- provide the prefix string used to generate
;;;                          direct-slot accessors 
;;;                          (default is nil, which is equivalent to:
;;;                            (concatenate 'simple-string
;;;                                         (symbol-name class-name) "."))
;;;   :generate-accessors-suffix (ignored for :prefix generation)
;;;                       -- provide the suffix string used to generate
;;;                          direct-slot accessors 
;;;                          (default is nil, which is equivalent to "-of")
;;;   :generate-initargs  -- generates initargs for specified direct slots:
;;;                            t => all direct slots |
;;;                            <direct-slot-names> |
;;;                            t :exclude ... => all direct slots except ...
;;;   :generate-initargs-symbol-function -- specifies a function of two
;;;                          argyments, class-name and slot-name, that is
;;;                          called to generate the initarg symbol for direct
;;;                          slots:
;;;                            (default is nil, which generates standard
;;;                             keyword initargs)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  09-15-02 Separated from ../gbbopen/units.lisp.  (Corkill)
;;;  05-21-05 Added :generate-accessors-prefix (reluctantly).  (Corkill)
;;;  05-27-06 Added "-of" accessor support, via :generate-accessors-format and 
;;;           :generate-accessors-suffix. (Corkill)
;;;  09-23-06 Added WITH-GENERATE-ACCESSORS-FORMAT macro in preparation for
;;;           change from :prefix to :suffix-style accessors. (Corkill)
;;;  09-28-05 Added (undocumented) :generate-initargs-symbol-function
;;;           class option to DEFINE-CLASS.  (Corkill)
;;;  02-23-09 Added :export-slot-names class option to DEFINE-CLASS. (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(defpackage :gbbopen-tools-define-class
  (:use :cl))

(in-package :gbbopen-tools-define-class)

;;; stuff from gbbopen-tools
(defmacro while (test &body body)
  `(loop (unless ,test (return))
         ,@body))

(defun make-keyword (string-symbol-or-character)
  (intern (string string-symbol-or-character)  
          (load-time-value (find-package 'keyword))))

(defun sole-element-violation (list)
  (cerror "Ignore the remaining elements."
          "The list ~s contains more than 1 element."
          list))

(defun sole-element (list)
  (prog1 (first list)
    (when (rest list)
      (sole-element-violation list))))

(defun memq (item list)
  (member item list :test #'eq))

(defun assq (item alist)
  (assoc item alist :test #'eq))

(defmacro with-once-only-bindings ((&rest symbols) &body body)
  (let ((gensyms (flet ((fn (symbol)
                             (declare (ignore symbol))
                             (gensym)))
                   (declare (dynamic-extent #'fn))
                   (mapcar #'fn symbols))))
    `(let (,.(flet ((fn (gensym) `(,gensym (gensym))))
               (declare (dynamic-extent #'fn))
               (mapcar #'fn gensyms)))
       `(let (,,.(flet ((fn (symbol gensym) ``(,,gensym ,,symbol)))
                   (declare (dynamic-extent #'fn))
                   (mapcar #'fn symbols gensyms)))
          ,(let (,.(flet ((fn (symbol gensym) `(,symbol ,gensym)))
                     (declare (dynamic-extent #'fn))
                     (mapcar #'fn symbols gensyms)))
             ,@body)))))

(defmacro push-acons (key datum place &environment env)
  ;;; Pushes an acons of key and datum onto the place alist (whether or not a
  ;;; matching key exists in the place alist.  Returns the updated alist.
  (if (symbolp place)
      `(setf ,place (acons ,key ,datum ,place))
      (with-once-only-bindings (key datum)
        (multiple-value-bind (vars vals store-vars writer-form reader-form)
            (get-setf-expansion place env)
          `(let* (,.(mapcar #'list vars vals)
                  (,(first store-vars)
                   (acons ,key ,datum ,reader-form)))
             ,writer-form)))))


;;; define-class
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(define-class
            standard-generate-accessors-prefix-function ; not yet documented
            with-generate-accessors-format)))

;;; ---------------------------------------------------------------------------
;;;
;;;  There is insufficient flexibility in the MOP to support defclass option
;;;  extensions at defclass parsing time.  Instead, we must resort to
;;;  separating the extensions from the basic defclass form.  This approach,
;;;  however, introduces its own difficulties in supporting further option
;;;  extensions to define-class.  The best we can do is provide an interface
;;;  into the separation mechanism (parse-define-class) for others to use.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*standard-define-class-options* 
            parse-define-class)))

;;; ---------------------------------------------------------------------------
;;;  These are the extended class-options supported in define-class.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *standard-define-class-options*
      '(:export-accessors 
        :export-class-name 
        :export-slot-names
        :generate-accessors
        :generate-accessors-format
        :generate-accessors-prefix
        :generate-accessors-suffix
        :generate-initargs
        ;; undocumented
        :generate-initargs-symbol-function)))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *default-generate-accessors-format* ':suffix)

  (defun standard-generate-accessors-prefix-function (class-name slot-name)
    (concatenate 'simple-string
      (symbol-name class-name)
      "."
      (symbol-name slot-name)))
  
  (defvar *default-generate-accessors-prefix* 
      (symbol-function 'standard-generate-accessors-prefix-function))
  
  (defvar *default-generate-accessors-suffix* '#:-of)
  
  (defmacro generate-accessors-format-values ()
    (list *default-generate-accessors-format*
          *default-generate-accessors-prefix*
          *default-generate-accessors-suffix*)))

;;; ---------------------------------------------------------------------------

(defmacro with-generate-accessors-format ((format
                                           &optional prefix/suffix-name)
                                          &body body)
  ;; We use macrolet trickery to provide compiler-let-style communication:
  `(macrolet ((generate-accessors-format-values () 
                (list ,format
                      ,(if (and prefix/suffix-name
                                (eq format ':prefix))
                           prefix/suffix-name
                           '*default-generate-accessors-prefix*)
                      ,(if (and prefix/suffix-name
                                (eq format ':suffix))
                           prefix/suffix-name
                           '*default-generate-accessors-suffix*))))
     ,@body))

;;; ---------------------------------------------------------------------------

(defmacro define-class (class-name direct-superclass-names
                        direct-slots &rest options &environment env)
  (multiple-value-bind (clos-direct-slots clos-class-options exports)
      (parse-define-class class-name direct-slots options
                          *standard-define-class-options* env)
    `(progn
       ,@(when exports
           `((eval-when (:compile-toplevel :load-toplevel :execute)
               (export ',exports))))
       (defclass ,class-name ,direct-superclass-names
         ,clos-direct-slots
         ,@clos-class-options))))

;;; ---------------------------------------------------------------------------

(defun parse-define-class (class-name direct-slots options 
                           extended-class-options env)
  (destructuring-bind (*default-generate-accessors-format* 
                       *default-generate-accessors-prefix*
                       *default-generate-accessors-suffix*)
      ;; get the macrolet-bound communication value:
      (macroexpand '(generate-accessors-format-values) env)
    (multiple-value-bind (clos-options extended-options)
        (parse-alist-options options extended-class-options)
      (multiple-value-bind (clos-direct-slots exports)
        (parse-direct-slots class-name direct-slots extended-options)
        (when (cdr (assq :export-class-name extended-options))
          (push class-name exports))
        (values clos-direct-slots clos-options exports)))))

;;; ---------------------------------------------------------------------------

(defun parse-direct-slots (class-name direct-slots extended-options)
  (let ((clos-direct-slots nil)
        (exported-symbols nil)
        (generate-initargs 
         (or (rest (assq :generate-initargs extended-options))
             ;; Default is t
             '(t)))
        (generate-accessors 
         (or (rest (assq :generate-accessors extended-options))
             ;; Default is t
             '(t)))
        (generate-accessors-format
         (or (sole-element
              (rest (assq :generate-accessors-format extended-options)))
             *default-generate-accessors-format*))
        (generate-accessors-prefix 
         (sole-element
          (rest (assq :generate-accessors-prefix extended-options))))
        (generate-accessors-suffix
         (sole-element
          (rest (assq :generate-accessors-suffix extended-options))))
        (export-accessors 
         (rest (assq :export-accessors extended-options)))
        (export-slot-names
         (rest (assq :export-slot-names extended-options)))
        ;; Currently undocumented class option (requested by Zack for CMU),
        ;; must be a symbol naming a function of two arguments, class-name
        ;; slot-name, (or an equivalent lambda expression) that returns the
        ;; symbol to be used as the generated :initarg value.  Function
        ;; objects (and closures) are not supported, as the additional
        ;; complexity to support them wasn't justified.
        (generate-initargs-symbol-function
         (sole-element
          (rest (assq :generate-initargs-symbol-function extended-options)))))
    ;; Check that slot options refer to direct slots only:
    (check-option-slots class-name :generate-initargs 
                        generate-initargs direct-slots)
    (check-option-slots class-name :generate-accessors
                        generate-accessors direct-slots)
    (check-option-slots class-name :export-accessors 
                        export-accessors direct-slots)
    (check-option-slots class-name :export-slot-names
                        export-slot-names direct-slots)
    ;; Warn about unused prefix/suffix specifications:
    (flet ((do-warn (generate-accessors-xxfix-indicator value)
             (warn "A ~s ~s was specified, but ignored, because ~s ~s ~
                    was also specified."
                   generate-accessors-xxfix-indicator
                   value
                   ':generate-accessors-format
                   generate-accessors-format)))
      (ecase generate-accessors-format
        (:prefix (when generate-accessors-suffix
                   (do-warn ':generate-accessors-suffix
                     generate-accessors-suffix)))
        (:suffix (when generate-accessors-prefix
                   (do-warn ':generate-accessors-prefix
                     generate-accessors-prefix)))))
    (dolist (slot direct-slots)
      ;; Convert atomic slot specification to a degenerate list form:
      (unless (consp slot)
        (setf slot (list slot)))
      ;; Process the slot specification:
      (let ((slot-name (first slot))) 
        (when (apply-to-slot-p export-slot-names slot-name)
          (push slot-name exported-symbols))
        (multiple-value-bind (accessor-spec export-p)
            (do-generate-accessors generate-accessors
              export-accessors
              class-name
              slot-name 
              generate-accessors-format
              (or generate-accessors-prefix
                  *default-generate-accessors-prefix*)
              (or generate-accessors-suffix
                  *default-generate-accessors-suffix*))
          (let ((new-clos-slot 
                 `(,slot-name
                   ,@(do-generate-initargs generate-initargs
                       class-name slot-name generate-initargs-symbol-function)
                   ,@accessor-spec
                   ,@(rest slot))))
            (push new-clos-slot clos-direct-slots)
            (when export-p 
              (push export-p exported-symbols))))))
    (values (nreverse clos-direct-slots)
            exported-symbols)))

;;; ---------------------------------------------------------------------------

(defun check-option-slots (class-name option-indicator option direct-slots)
  (let ((option-slots
         (cond ((eq (first option) t)
                (cddr option))
               ((and (eq (first option) nil)
                     (null (rest option)))
                nil)
               (t option))))
    (dolist (option-slot option-slots)
      (unless (flet ((fn (slot)
                       (if (consp slot)
                           (eq option-slot (car slot))
                           (eq option-slot slot))))
                (declare (dynamic-extent #'fn))
                (member-if #'fn direct-slots))
        (warn "The slot name ~s specified in ~s is not a direct slot in 
               class ~s.~@
               It will be ignored."
              option-slot option-indicator class-name)))))

;;; ---------------------------------------------------------------------------

(defun apply-to-slot-p (option slot-name)
  ;;; Returns true if `option' applies to slot `slot-name'
  ;;;
  ;;; Option can be:
  ;;;     (nil)
  ;;;     (t)
  ;;;     (t :exclude <excluded-slot-names>)
  ;;;     (<included-slot-names>)
  (cond 
   ;; t (possibly with :exclude suboption):
   ((eq (first option) t)
    (when (or (null (rest option))
              (and (eq (second option) :exclude)
                   (not (memq slot-name (cddr option)))))
      t))
   ;; list of accessors
   ((and (not (eq (first option) nil))
         (memq slot-name option))
    t)))

;;; ---------------------------------------------------------------------------

(defun do-generate-initargs (generate-initargs-spec class-name slot-name 
                             symbol-function)
  (when (apply-to-slot-p generate-initargs-spec slot-name)
    `(:initarg ,(if symbol-function
                    (funcall symbol-function class-name slot-name)
                    (make-keyword slot-name)))))

;;; ---------------------------------------------------------------------------

(defun do-generate-accessors (generate-accessors-spec export-accessors-spec
                              class-name slot-name format prefix suffix)
  (when (apply-to-slot-p generate-accessors-spec slot-name)
    (let ((accessor-name
           (case format                 ; we've ecased format already...
             (:prefix
              (typecase prefix
                (string
                 (concatenate 'simple-string
                   prefix (symbol-name slot-name)))
                (symbol
                 (concatenate 'simple-string 
                   (symbol-name prefix) (symbol-name slot-name)))
                (otherwise
                 (funcall prefix class-name slot-name))))
             (:suffix
              (typecase suffix
                (string
                 (concatenate 'simple-string
                   (symbol-name slot-name) suffix))
                (symbol
                 (concatenate 'simple-string 
                   (symbol-name slot-name) (symbol-name suffix)))
                (otherwise
                 (funcall suffix class-name slot-name)))))))
      (unless (stringp accessor-name)
        (error "Accessor name ~s is not a string" accessor-name))
      (let ((accessor (intern accessor-name)))
        (values 
         `(:accessor ,accessor)
         (when (apply-to-slot-p export-accessors-spec slot-name)
           accessor))))))

;;; ---------------------------------------------------------------------------

(defun parse-alist-options (alist extended-indicators)
  ;;; Separates extended indicators from ALIST
  (let ((clos-options nil)
        (extended-options nil))
    (dolist (acons alist)
      (destructuring-bind (key . datum) acons
        (cond ((memq key extended-indicators)
               (push-acons key datum extended-options))
              (t (push-acons key datum clos-options)))))
    (values (nreverse clos-options)
            (nreverse extended-options))))

;;; ---------------------------------------------------------------------------
;;;  parse-plist-options is not used in define-class.lisp, but it is useful and
;;;  so closely related to parse-alist-options that we include it here.

(defun parse-plist-options (plist extended-indicators)
  ;;; Separates extended indicators from PLIST
  (let ((clos-options nil)
        (extended-options nil))
    (while plist
       (destructuring-bind (indicator indicator-value &rest rest) plist
         (cond ((memq indicator extended-indicators)
                (setf extended-options 
                  (list* indicator-value indicator extended-options)))
               (t (setf clos-options 
                    (list* indicator-value indicator clos-options))))
         (setf plist rest)))
    (values (nreverse clos-options)
            (nreverse extended-options))))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
