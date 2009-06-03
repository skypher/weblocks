
(defun graph-tmp-path ()
  (merge-pathnames "tmp/" (compute-public-files-path :weblocks)))

(defvar *graph-tmp-html-path* "/pub/tmp/")

(defun clean-graph-tmp-path ()
  (ensure-directories-exist (graph-tmp-path))
  (loop for file in (directory (merge-pathnames "*" (graph-tmp-path)))
        do (when (> (- (get-universal-time) (file-write-date file)) 300)
             (delete-file file))))

(defun router-graph-show (k router)
  (let ((counter 0))
    (flet ((gen-filename ()
             (format nil "tmp-router-~D-~D.png" (random 100000) (incf counter))))
      (clean-graph-tmp-path)
      (labels ((graph-router (router)
                 (let ((graphs (netstatus.manager::sourcegroup-graphs router)))
                   (when graphs
                     (let ((files
                            (loop for g in graphs collect (gen-filename))))
                       (loop for g in graphs
                             for f in files
                             do  (netstatus.manager::generate-graph g router (merge-pathnames f (graph-tmp-path))))
                       (with-html
                           (:h2 (str (netstatus.manager::title router))))

                       (loop for f in files
                             for g in graphs do
                             (with-html
                                 (:div
                                  (:h3 (str (netstatus.manager::graph-def-title g)))
                                  (:img :src (concatenate 'string (string *graph-tmp-html-path*) f)))))
                       (typecase router
                         (netstatus.manager::router
                          (loop for if in (netstatus.manager::router-interfaces router) do
                                (graph-router if)))))))))
        (graph-router router))))

  (render-link
   (lambda(&rest args)
     (declare (ignore args))
     (answer k)) "Back"))

(defun router-graph (obj router)  
  (declare (ignore obj))
  (do-page
    (lambda(k)(router-graph-show k router))))

(defun make-routers-page ()
  (make-instance 'composite :widgets
                 (list
                  (make-instance 'datagrid
                                 :on-drilldown '(graph . router-graph)
                                 :name 'routers-grid
                                 :data-class 'router
                                 :view 'router-table-view
                                 :item-data-view 'router-data-view
                                 :item-form-view 'router-form-view)))) 

