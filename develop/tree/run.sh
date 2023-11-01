#!/bin/sh

#sbcl --load avltree.lisp
#sbcl --load test/test-avltree.lisp
sbcl --script test/test-avltree.lisp

#sbcl --load avltree-parent.lisp
#sbcl --load test/test-avlptree.lisp
sbcl --script test/test-avlptree.lisp

#sbcl --load rbtree.lisp
#sbcl --load test/test-rbtree.lisp
sbcl --script test/test-rbtree.lisp

#sbcl --load bintree.lisp
#sbcl --load test/test-bintree.lisp
sbcl --script test/test-bintree.lisp

#sbcl --load test/random-test.lisp
sbcl --script test/random-test.lisp

