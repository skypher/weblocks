(push #p"/usr/lib/sbcl/site/cl-weblocks/" asdf:*central-registry*)
(push #p"/usr/lib/sbcl/site/cl-weblocks/examples/weblocks-demo/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :weblocks-demo)
(weblocks-demo:start-weblocks-demo)
