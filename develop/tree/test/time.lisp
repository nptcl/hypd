#!/usr/bin/env -S sbcl --script

(load #p"bintree.lisp")
(load #p"avltree.lisp")
(load #p"rbtree.lisp")

(defpackage work
  (:use common-lisp avltree rbtree bintree))
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
(defun main-insert (inst array)
  (let ((size (length array)) v)
    (dotimes (i size)
      (setq v (aref array i))
      (insert-avltree inst v v))))

(defun main-delete (inst array)
  (let ((size (length array)) v)
    (dotimes (i size)
      (setq v (aref array i))
      (delete-avltree inst v))))

(defun main ()
  (let* ((inst (make-avltree))
         (size 1000000)
         (type 'random)
         (array (make-random-array size type)))
    (time (main-insert inst array))
    (time (main-delete inst array))))

(let ((*random-state* (make-random-state t)))
  (main))

