#!/usr/bin/env -S sbcl --script

(defun load! (file)
  (if (probe-file file)
    (load file)
    (load (merge-pathnames file #p"../"))))

(load! #p"bintree.lisp")
(load! #p"avltree.lisp")
(load! #p"avltree-parent.lisp")
(load! #p"rbtree.lisp")

(defpackage work
  (:use common-lisp avltree avlptree rbtree bintree))
(in-package work)

;;
;;  array
;;
(defun random-swap (array size)
  (dotimes (x size)
    (let ((y (random size)))
      (unless (= x y)
        (rotatef (aref array x) (aref array y))))))

(defun make-random-left (array size)
  (loop for i from 0 below size
        do (setf (aref array i) i)))

(defun make-random-right (array size)
  (loop for i downfrom (1- size) downto 0
        do (setf (aref array i) i)))

(defun make-random-randomly (array size)
  (make-random-left array size)
  (dotimes (i 10)
    (random-swap array size)))

(defun make-random-array (size type)
  (let ((array (make-array size)))
    (ecase type
      (left (make-random-left array size))
      (right (make-random-right array size))
      (random (make-random-randomly array size)))
    array))


;;
;;  main
;;
(defun main-insert (inst array call)
  (let ((size (length array)) v)
    (dotimes (i size)
      (setq v (aref array i))
      (funcall call inst v v))))

(defun main-delete (inst array call)
  (let ((size (length array)) v)
    (dotimes (i size)
      (setq v (aref array i))
      (funcall call inst v))))

(defun main ()
  (let* ((inst1 (make-avltree))
         (inst2 (make-avlptree))
         (inst3 (make-rbtree))
         (size 1000000)
         (type 'random)
         (array (make-random-array size type)))
    (format t "AVL~%")
    (time (main-insert inst1 array #'insert-avltree))
    (time (main-delete inst1 array #'delete-avltree))
    (format t "AVL (parent)~%")
    (time (main-insert inst2 array #'insert-avlptree))
    (time (main-delete inst2 array #'delete-avlptree))
    (format t "Red-Black~%")
    (time (main-insert inst3 array #'insert-rbtree))
    (time (main-delete inst3 array #'delete-rbtree))
    ))

(let ((*random-state* (make-random-state t)))
  (main))

