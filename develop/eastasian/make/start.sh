#!/bin/sh
sbcl --quit --load hypd.eastasian.lisp
#ccl -l hypd.eastasian.lisp -e '(quit)'
#clisp -m 1024mb hypd.eastasian.lisp
#npt --standalone --script hypd.eastasian.lisp

