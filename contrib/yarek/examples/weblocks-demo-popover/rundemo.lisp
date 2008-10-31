;(push #p"/usr/lib/sbcl/site/cl-weblocks/" asdf:*central-registry*)
(push #p"/path/to/the/demo/root/dir/weblocks-demo-popover/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :weblocks-demo-popover)
(weblocks-demo-popover:start-weblocks-demo-popover :debug t)
