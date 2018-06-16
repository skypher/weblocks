#!/bin/bash

set -e

# we only want to build documentation from the "reblocks" branch
# and want to make it only once.
if [ "$TRAVIS_BRANCH" = "reblocks" -a "$LISP" = "ccl" ]; then
    sudo pip install -r docs/requirements.txt
    ./build-docs.ros
fi
