#!/bin/sh

#sbcl --load avltree.lisp
#sbcl --load test-avltree.lisp
sbcl --script test/test-avltree.lisp

#sbcl --load rbtree.lisp
#sbcl --load test-rbtree.lisp
sbcl --script test/test-rbtree.lisp

#sbcl --load bintree.lisp
#sbcl --load test-bintree.lisp
sbcl --script test/test-bintree.lisp

#sbcl --load random-test.lisp
sbcl --script test/random-test.lisp

