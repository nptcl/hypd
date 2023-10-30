(cl:in-package common-lisp-user)
(load #p"avltree.lisp")
(load #p"avlptree.lisp")
(load #p"bintree.lisp")
(load #p"rbtree.lisp")
(load #p"test/avltree-check.lisp")
(load #p"test/avlptree-check.lisp")
(load #p"test/bintree-check.lisp")
(load #p"test/rbtree-check.lisp")

(defpackage work (:use common-lisp bintree avltree avlptree rbtree))
(in-package work)

;;
;;  interface
;;
(defclass calltree () ())
(defgeneric clear-calltree (inst))
(defgeneric insert-calltree (inst key value))
(defgeneric delete-calltree (inst key))
(defgeneric search-calltree (inst key))
(defgeneric map-calltree (call inst))
(defgeneric check-calltree-error (inst))

;;  clear
(defmethod clear-calltree ((inst bintree))
  (clear-bintree inst))
(defmethod clear-calltree ((inst avltree))
  (clear-avltree inst))
(defmethod clear-calltree ((inst avlptree))
  (clear-avlptree inst))
(defmethod clear-calltree ((inst rbtree))
  (clear-rbtree inst))

;;  insert
(defmethod insert-calltree ((inst bintree) key value)
  (insert-bintree inst key value))
(defmethod insert-calltree ((inst avltree) key value)
  (insert-avltree inst key value))
(defmethod insert-calltree ((inst avlptree) key value)
  (insert-avlptree inst key value))
(defmethod insert-calltree ((inst rbtree) key value)
  (insert-rbtree inst key value))

;;  delete
(defmethod delete-calltree ((inst bintree) key)
  (delete-bintree inst key))
(defmethod delete-calltree ((inst avltree) key)
  (delete-avltree inst key))
(defmethod delete-calltree ((inst avlptree) key)
  (delete-avlptree inst key))
(defmethod delete-calltree ((inst rbtree) key)
  (delete-rbtree inst key))

;;  search
(defmethod search-calltree ((inst bintree) key)
  (search-bintree inst key))
(defmethod search-calltree ((inst avltree) key)
  (search-avltree inst key))
(defmethod search-calltree ((inst avlptree) key)
  (search-avlptree inst key))
(defmethod search-calltree ((inst rbtree) key)
  (search-rbtree inst key))

;;  map
(defmethod map-calltree (call (inst bintree))
  (map-bintree call inst))
(defmethod map-calltree (call (inst avltree))
  (map-avltree call inst))
(defmethod map-calltree (call (inst avlptree))
  (map-avlptree call inst))
(defmethod map-calltree (call (inst rbtree))
  (map-rbtree call inst))

;;  check
(defmethod check-calltree-error ((inst bintree))
  (check-bintree-error inst))
(defmethod check-calltree-error ((inst avltree))
  (check-avltree-error inst))
(defmethod check-calltree-error ((inst avlptree))
  (check-avlptree-error inst))
(defmethod check-calltree-error ((inst rbtree))
  (check-rbtree-error inst))


;;
;;  main
;;
(defun equal-key-value (inst)
  (let (list)
    (map-calltree
      (lambda (key value)
        (push key list)
        (push value list))
      inst)
    (nreverse list)))

(defun equal-tree (x y)
  (equal (equal-key-value x)
         (equal-key-value y)))

(defun equal-tree-error (x y)
  (unless (equal-tree x y)
    (error "equal-tree error, ~S, ~S." x y)))

(defun insert-testcall (size x y insert)
  (dotimes (i size)
    (destructuring-bind (key . value) (aref insert i)
      (insert-calltree x key value)
      (insert-calltree y key value)
      (check-calltree-error x)
      (check-calltree-error y)
      (equal-tree-error x y))))

(defun delete-testcall (size x y delete)
  (dotimes (i size)
    (destructuring-bind (key . value) (aref delete i)
      (insert-calltree x key value)
      (insert-calltree y key value)
      (check-calltree-error x)
      (check-calltree-error y)
      (equal-tree-error x y))))

(defun insert-delete-testcall (size x y insert delete)
  (insert-testcall size x y insert)
  (delete-testcall size x y delete))

(defun random-swap (array size)
  (dotimes (x size)
    (let ((y (random size)))
      (unless (= x y)
        (rotatef (aref array x) (aref array y))))))

(defun make-random-left (array size)
  (loop for i from 0 below size
        do (setf (aref array i) (cons i (* i 100)))))

(defun make-random-right (array size)
  (loop for i downfrom (1- size) downto 0
        do (setf (aref array i) (cons i (* i 100)))))

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

(defun insert-delete-random (size left right)
  (let ((x (make-bintree))
        (y1 (make-avltree))
        (y2 (make-avlptree))
        (y3 (make-rbtree))
        (array1 (make-random-array size left))
        (array2 (make-random-array size right)))
    (insert-delete-testcall size x y1 array1 array2)
    (clear-calltree x)
    (insert-delete-testcall size x y2 array1 array2)
    (clear-calltree x)
    (insert-delete-testcall size x y3 array1 array2)
    (clear-calltree x)))

(defun cross-product (list1 list2)
  (let (list)
    (dolist (x list1)
      (dolist (y list2)
        (push (cons x y) list)))
    (nreverse list)))

(defun main-loop (index)
  (let ((size 100)
        (list '(left right random)))
    (dolist (x (cross-product list list))
      (destructuring-bind (left . right) x
        (format t "Test.~A: ~A, ~A, ~A~%" index size left right)
        (insert-delete-random size left right)))))

(defun main-call ()
  (dotimes (i 20)
    (main-loop i)))

(defun main ()
  (let ((*random-state* (make-random-state t)))
    (main-call)))
(main)

