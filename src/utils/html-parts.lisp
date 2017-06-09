(defpackage #:weblocks.utils.html-parts
  (:use #:cl)
  (:export
   #:reset
   #:update-html-parts-connections
   #:update-current-html-part-children
   #:nested-html-part
   #:get-html-part
   #:get-html-part-context
   #:get-html-parts-root-hash
   #:get-html-part-children)
  (:import-from #:weblocks
                #:md5))
(in-package weblocks.utils.html-parts)


(defvar *parts-md5-hash* (make-hash-table :test 'equal)
  "A hash with key of html part md5 hash 
and the value of list with html part as car
and list of children as cdr")


(defvar *parts-md5-context-hash* (make-hash-table :test 'equal)
  "A hash with key of html part md5 hash 
and the value of html part context")


(defvar *current-html-part-children* nil
  "A place where we can put md5 hashes of children parts of processed html part.
Should be used from body of NESTED-HTML-PART macro")


(defvar *process-html-parts-p* (lambda () nil)
  "Contains callback which should return boolean value 
indicating whether html parts should be collected and processed")


(defun process-html-parts-p ()
  "Whether or not to capture html parts information"
  (and (boundp '*parts-md5-context-hash*) (funcall *process-html-parts-p*)))

(defmacro nested-html-part (context &body body)
  "Adds part of html (a string received as a result of &body evaluation)
and context associated with it to html parts set.

Returns value, returned by evaluation of the body.
"
  `(let ((value))
     (setf *current-html-part-children* nil)
     (setf value (progn ,@body))
     (when (process-html-parts-p)
       (setf (gethash (md5 value) *parts-md5-context-hash*) ,context)
       (setf (gethash (md5 value) *parts-md5-hash*)
             (cons value (mapcar #'md5 *current-html-part-children*))))
     value))

(defun reset ()
  "Resets html parts set"

  (setf *parts-md5-hash* (make-hash-table :test 'equal))
  (setf *parts-md5-context-hash* (make-hash-table :test 'equal)))

(defun is-substring-of-p (string-child string-parent)
  "Checks whether one string is substring of another and has less length."
  (and 
    (> (length string-parent) (length string-child))
    (search string-child string-parent)
    t))

(defun get-html-part (key)
  "Returns html part string by its md5"
  (car (gethash key *parts-md5-hash*)))

(defun get-html-part-children (key)
  "Returns html part child parts by its md5"
  (cdr (gethash key *parts-md5-hash*)))

(defun set-html-part-children (key children)
  "Sets html part child parts by its md5"
  (let ((html-part (get-html-part key)))
    (setf (gethash key *parts-md5-hash*)
          (cons html-part children))))

(defun push-new-html-part-child (key child-key)
  "Adds child-key to the list of part's children."
  (set-html-part-children key
                          (adjoin child-key
                                  (get-html-part-children key)
                                  :test #'string=)))

(defun remove-html-part (key)
  "Removes html part by its md5"
  (remhash key *parts-md5-hash*))

(defun get-html-part-context (key)
  "Returns html part context by its md5"
  (gethash key *parts-md5-context-hash*))

(defun update-html-parts-connections ()
  "Builds html parts tree from html parts list. 
   Each html part child is a substring of its parent.
   Html parts which are empty strings removed from html parts set."
  (unless 
    (process-html-parts-p)
    (return-from update-html-parts-connections))

  ; Removing messy items
  (let ((empty-string-md5 (md5 "")))
    (loop for key being the hash-keys of *parts-md5-hash* do 
          ; Remove empty strings from html part children
          (set-html-part-children 
            key
            (remove 
              empty-string-md5 
              (get-html-part-children key)
              :test #'string=))
          ; Remove empty string html parts
          (when (string= key empty-string-md5)
            (remove-html-part key))))

  (loop for key being the hash-keys of *parts-md5-hash* do 
        (let ((value (get-html-part key))
              (child-value))

          ;; Step first, adding child-parent connections
          (loop for child-key being the hash-keys of *parts-md5-hash* do 
                (setf child-value (get-html-part child-key))
                (when (and 
                        (not (zerop (length child-value)))
                        (is-substring-of-p child-value value))
                  (push-new-html-part-child key child-key)))

          ; Step second, removing messy connections
          (loop for child-key being the hash-keys of *parts-md5-hash* do 
            (let ((children (get-html-part-children child-key)))

              (loop for child in children do 
                (let ((child-children (get-html-part-children child)))
                  (loop for i in child-children do 
                    (when (find i children :test #'string=)
                      (setf children (remove i children :test #'string=))))))

              (set-html-part-children child-key children))))))

(defun update-current-html-part-children (args)
  "Takes strings list as an argument and adds strings 
   which are html parts (which are in html parts set)
   to *current-html-part-children* list "

  (loop for value on args
        if (and (stringp value) (gethash (md5 value) *parts-md5-hash*))
        do
        (push value *current-html-part-children*)))

(defun get-html-parts-root-hash ()
  "Returns md5 hash of root html part in parts tree"
  (let ((max-length (loop for key being the hash-keys of *parts-md5-hash* 
                          maximize (length (get-html-part key)))))
    (loop for key being the hash-keys of *parts-md5-hash* 
          if (= max-length (length (get-html-part key)))
          do 
          (return-from get-html-parts-root-hash key))))

