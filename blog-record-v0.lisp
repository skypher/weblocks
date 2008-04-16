(in-package :cl-user)

;;;; blog-v0: Start up

;;;; Follow
;;;; http://trac.common-lisp.net/cl-weblocks/wiki/UserManual#BreakingtheIce
;;;; and start a "blog" project
;;;;
;;;; In the REPL

(asdf:operate 'asdf:load-op :weblocks)

(asdf:operate
 'wop:make-app-op :weblocks
 :name 'blog
 :target "/home/evan/Documents/projects/info/lisp/webapps/")

(push #p"/home/evan/Documents/projects/info/lisp/webapps/blog/"
      asdf:*central-registry*)

(asdf:operate 'asdf:load-op :blog)

(blog:start-blog :debug t)
