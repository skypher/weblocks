;(push #p"/usr/lib/sbcl/site/cl-weblocks/" asdf:*central-registry*)
(push #p"/path/to/the/demo/root/dir/employer-employee/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :employer-employee)
(employer-employee:start-employer-employee :debug t)
