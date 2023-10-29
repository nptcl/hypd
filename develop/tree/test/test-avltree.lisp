(load #p"avltree.lisp")
(load #p"test/avltree-check.lisp")

#+sbcl (require 'sb-rt)
(defpackage work (:use common-lisp avltree #+npt npt-rt #+sbcl sb-rt))
(in-package work)

(deftest avltree.1
  (let ((inst (make-avltree)))
    (check-avltree-error inst)
    (values
      (empty-avltree inst)
      (size-avltree inst)))
  t 0)

(deftest avltree.2
  (let ((inst (make-avltree)))
    (insert-avltree inst 10 20)
    (check-avltree-error inst)
    (search-avltree inst 10))
  20 t)

(deftest avltree.3
  (let ((inst (make-avltree)))
    (insert-avltree inst 10 20)
    (check-avltree-error inst)
    (search-avltree inst 20))
  nil nil)

(deftest avltree.4
  (let ((inst (make-avltree)))
    (insert-avltree inst 10 20)
    (check-avltree-error inst)
    (values
      (empty-avltree inst)
      (size-avltree inst)))
  nil 1)

(deftest avltree.5
  (let ((inst (make-avltree)))
    (insert-avltree inst 10 :aaa)
    (insert-avltree inst 20 :bbb)
    (check-avltree-error inst)
    (values
      (empty-avltree inst)
      (size-avltree inst)))
  nil 2)

(deftest avltree.6
  (let ((inst (make-avltree)))
    (insert-avltree inst 10 :aaa)
    (insert-avltree inst 20 :bbb)
    (check-avltree-error inst)
    (values
      (search-avltree inst 10)
      (search-avltree inst 20)
      (search-avltree inst 30)))
  :aaa :bbb nil)

(deftest avltree.7
  (let ((inst (make-avltree)))
    (insert-avltree inst 20 :bbb)
    (insert-avltree inst 10 :aaa)
    (check-avltree-error inst)
    (values
      (search-avltree inst 10)
      (search-avltree inst 20)
      (search-avltree inst 30)))
  :aaa :bbb nil)

(deftest avltree.8
  (let ((inst (make-avltree)))
    (dotimes (i 20)
      (insert-avltree inst i (* 100 i))
      (check-avltree-error inst))
    (values
      (search-avltree inst 5)
      (search-avltree inst 6)
      (search-avltree inst 100)
      (size-avltree inst)))
  500 600 nil 20)

(deftest avltree.9
  (let ((inst (make-avltree)))
    (dotimes (i 20)
      (let ((i (- 20 i)))
        (insert-avltree inst i (* 100 i))))
    (check-avltree-error inst)
    (values
      (search-avltree inst 5)
      (search-avltree inst 6)
      (search-avltree inst 100)
      (size-avltree inst)))
  500 600 nil 20)


;;
;;  init
;;
(deftest init.1
  (let ((inst (init-avltree (constantly 10) 0)))
    (check-avltree-error inst)
    (avltree-p inst))
  t)

(defun init-avltree-left (size)
  (let ((index 0))
    (init-avltree
      (lambda ()
        (multiple-value-prog1
          (values index (* index 10))
          (incf index 1)))
      size)))

(defun init-avltree-right (size)
  (let ((index size))
    (init-avltree
      (lambda ()
        (decf index 1)
        (values index (* index 10)))
      size nil t)))

(defun init-avltree-insert (size)
  (let ((inst (make-avltree)))
    (dotimes (index size inst)
      (insert-avltree inst index (* index 10)))))

(deftest init.2
  (let ((inst (init-avltree-left 1)))
    (check-avltree-error inst)
    (avltree-p inst))
  t)

(deftest init.3
  (let ((inst (init-avltree-right 1)))
    (check-avltree-error inst)
    (avltree-p inst))
  t)

(deftest init.4
  (let ((inst (init-avltree-left 1)))
    (check-avltree-error inst)
    (values
      (size-avltree inst)
      (search-avltree inst 0)))
  1 0)

(deftest init.5
  (let ((inst (init-avltree-right 1)))
    (check-avltree-error inst)
    (values
      (size-avltree inst)
      (search-avltree inst 0)))
  1 0)

(deftest init.6
  (let ((inst (init-avltree-left 2)))
    (check-avltree-error inst)
    (values
      (size-avltree inst)
      (search-avltree inst 0)
      (search-avltree inst 1)))
  2 0 10)

(deftest init.7
  (let ((inst (init-avltree-right 2)))
    (check-avltree-error inst)
    (values
      (size-avltree inst)
      (search-avltree inst 0)
      (search-avltree inst 1)))
  2 0 10)

(deftest init.8
  (let* ((inst (init-avltree-left 2))
         (root (avltree::avltree-root inst)))
    (check-avltree-error inst)
    (values
      (avltree::avlnode-key
        (avltree::avlnode-left root))
      (avltree::avlnode-key root)))
  0 1)

(deftest init.9
  (let* ((inst (init-avltree-right 2))
         (root (avltree::avltree-root inst)))
    (check-avltree-error inst)
    (values
      (avltree::avlnode-key root)
      (avltree::avlnode-key
        (avltree::avlnode-right root))))
  0 1)


;;
;;  delete
;;

;;  delete1
(deftest delete0.1
  (let ((inst (make-avltree)))
    (prog1
      (delete-avltree inst 10)
      (check-avltree-error inst)))
  nil)

(deftest delete1.1
  (let ((inst (make-avltree)) ret)
    (insert-avltree inst 10 20)
    (setq ret (delete-avltree inst 10))
    (check-avltree-error inst)
    (values ret (size-avltree inst)))
  t 0)

(deftest delete1.2
  (let ((inst (make-avltree)) ret)
    (insert-avltree inst 10 20)
    (setq ret (delete-avltree inst 20))
    (check-avltree-error inst)
    (values ret (size-avltree inst)))
  nil 1)


;;  delete2
(deftest delete2.1
  (let ((inst (init-avltree-left 2)) ret size root key value)
    (setq ret (delete-avltree inst 1))
    (check-avltree-error inst)
    (setq size (size-avltree inst))
    (setq root (avltree::avltree-root inst))
    (setq key (avltree::avlnode-key root))
    (setq value (avltree::avlnode-value root))
    (values ret size key value))
  t 1 0 0)

(deftest delete2.2
  (let ((inst (init-avltree-right 2)) ret size root key value)
    (setq ret (delete-avltree inst 0))
    (check-avltree-error inst)
    (setq size (size-avltree inst))
    (setq root (avltree::avltree-root inst))
    (setq key (avltree::avlnode-key root))
    (setq value (avltree::avlnode-value root))
    (values ret size key value))
  t 1 1 10)

(deftest delete2.3
  (let ((inst (init-avltree-left 2)) ret size root key value)
    (setq ret (delete-avltree inst 0))
    (setq size (size-avltree inst))
    (setq root (avltree::avltree-root inst))
    (setq key (avltree::avlnode-key root))
    (setq value (avltree::avlnode-value root))
    (check-avltree-error inst)
    (values ret size key value))
  t 1 1 10)

(deftest delete2.4
  (let ((inst (init-avltree-right 2)) ret size root key value)
    (setq ret (delete-avltree inst 1))
    (setq size (size-avltree inst))
    (setq root (avltree::avltree-root inst))
    (setq key (avltree::avlnode-key root))
    (setq value (avltree::avlnode-value root))
    (check-avltree-error inst)
    (values ret size key value))
  t 1 0 0)

(deftest delete2.5
  (let ((inst (init-avltree-left 2)))
    (delete-avltree inst 1)
    (check-avltree-error inst)
    (delete-avltree inst 0)
    (check-avltree-error inst)
    (size-avltree inst))
  0)

(deftest delete2.6
  (let ((inst (init-avltree-left 2)))
    (delete-avltree inst 0)
    (check-avltree-error inst)
    (delete-avltree inst 1)
    (check-avltree-error inst)
    (size-avltree inst))
  0)

(deftest delete2.7
  (let ((inst (init-avltree-right 2)))
    (delete-avltree inst 1)
    (check-avltree-error inst)
    (delete-avltree inst 0)
    (check-avltree-error inst)
    (size-avltree inst))
  0)

(deftest delete2.8
  (let ((inst (init-avltree-right 2)))
    (delete-avltree inst 0)
    (check-avltree-error inst)
    (delete-avltree inst 1)
    (check-avltree-error inst)
    (size-avltree inst))
  0)


;;  delete3
(deftest delete3.1
  (let ((inst (init-avltree-left 3)))
    (check-avltree-error inst)
    (delete-avltree inst 0)
    (check-avltree-error inst)
    (size-avltree inst))
  2)

(deftest delete3.2
  (let ((inst (init-avltree-left 3)))
    (check-avltree-error inst)
    (delete-avltree inst 2)
    (check-avltree-error inst)
    (size-avltree inst))
  2)

(deftest delete3.3
  (let ((inst (init-avltree-left 3)))
    (check-avltree-error inst)
    (delete-avltree inst 1)
    (check-avltree-error inst)
    (size-avltree inst))
  2)

(deftest delete3.4
  (let ((inst (init-avltree-left 3)))
    (check-avltree-error inst)
    (delete-avltree inst 1)
    (check-avltree-error inst)
    (size-avltree inst))
  2)


;;  delete4
;;    2
;;   1 3
;;  0

(defun search-check-avltree (inst list)
  (dolist (x list t)
    (multiple-value-bind (ignore check) (search-avltree inst x)
      (declare (ignore ignore))
      (unless check
        (return nil)))))

(deftest delete4.1
  (let ((inst (init-avltree-left 4)))
    (check-avltree-error inst)
    (delete-avltree inst 0)
    (check-avltree-error inst)
    (values
      (size-avltree inst)
      (search-avltree inst 0)
      (search-check-avltree inst '(1 2 3))))
  3 nil t)

(deftest delete4.2
  (let ((inst (init-avltree-left 4)))
    (check-avltree-error inst)
    (delete-avltree inst 1)
    (check-avltree-error inst)
    (values
      (size-avltree inst)
      (search-avltree inst 1)
      (search-check-avltree inst '(0 2 3))))
  3 nil t)

(deftest delete4.3
  (let ((inst (init-avltree-left 4)))
    (check-avltree-error inst)
    (delete-avltree inst 2)
    (check-avltree-error inst)
    (values
      (size-avltree inst)
      (search-avltree inst 2)
      (search-check-avltree inst '(0 1 3))))
  3 nil t)

(deftest delete4.4
  (let ((inst (init-avltree-left 4)))
    (check-avltree-error inst)
    (delete-avltree inst 3)
    (check-avltree-error inst)
    (values
      (size-avltree inst)
      (search-avltree inst 3)
      (search-check-avltree inst '(0 1 2))))
  3 nil t)


;;  delete all
(defun delete-all-index (direct size index)
  (let ((inst (ecase direct
                (left (init-avltree-left size))
                (right (init-avltree-right size))
                (insert (init-avltree-insert size)))))
    (check-avltree-error inst)
    (delete-avltree inst index)
    (check-avltree-error inst)
    (unless (= (1- size) (size-avltree inst))
      (error "size error."))
    (when (search-avltree inst index)
      (error "search error."))
    (let* ((list (loop for i from 0 below size collect i))
           (list (delete index list)))
      (unless (search-check-avltree inst list)
        (error "search-check error.")))))

(defun delete-all-type (direct size)
  (dotimes (i size)
    (delete-all-index direct size i)))

(defun delete-all (size)
  (delete-all-type 'left size)
  (delete-all-type 'right size)
  (delete-all-type 'insert size)
  (values))

(deftest delete4.all
  (delete-all 4))

(deftest delete5.all
  (delete-all 5))

(deftest delete6.all
  (delete-all 6))

(deftest delete7.all
  (delete-all 7))

(deftest delete8.all
  (delete-all 8))

(deftest delete.many.all
  (dotimes (i 65)
    (delete-all i))
  nil)


;;  delete
(deftest avltree-delete.1
  (let ((inst (make-avltree)))
    (prog1
      (delete-avltree inst 10)
      (check-avltree-error inst)))
  nil)

(deftest avltree-delete.2
  (let ((inst (make-avltree)))
    (insert-avltree inst 10 20)
    (prog1
      (delete-avltree inst 10)
      (check-avltree-error inst)))
  t)

(deftest avltree-delete.3
  (let ((inst (make-avltree)))
    (insert-avltree inst 10 20)
    (delete-avltree inst 10)
    (check-avltree-error inst)
    (values
      (empty-avltree inst)
      (size-avltree inst)))
  t 0)

(deftest avltree-delete.4
  (let ((inst (make-avltree)))
    (insert-avltree inst 10 20)
    (delete-avltree inst 30)
    (check-avltree-error inst)
    (values
      (empty-avltree inst)
      (size-avltree inst)))
  nil 1)

(deftest avltree-delete.5
  (let ((inst (make-avltree)))
    (dotimes (i 20)
      (insert-avltree inst i (* 100 i)))
    (delete-avltree inst 5)
    (delete-avltree inst 6)
    (delete-avltree inst 7)
    (check-avltree-error inst)
    (values
      (search-avltree inst 4)
      (search-avltree inst 6)
      (search-avltree inst 100)
      (size-avltree inst)))
  400 nil nil 17)

(deftest avltree-delete.6
  (let ((inst (make-avltree)))
    (dotimes (i 20)
      (insert-avltree inst i (* 100 i)))
    (dotimes (i 20)
      (delete-avltree inst i))
    (values
      (empty-avltree inst)
      (size-avltree inst)))
  t 0)

(deftest avltree-min.1
  (let ((inst (make-avltree)))
    (insert-avltree inst 10 100)
    (insert-avltree inst 20 200)
    (insert-avltree inst 30 300)
    (min-avltree inst))
  10 t)


;;
;;  do-tests
;;
(do-tests)
(fresh-line)


;;
;;  init
;;
(defconstant +size+ 1000000)

(defun main-init ()
  (let ((inst (make-avltree))
        (vector (make-array +size+ :initial-element nil)))
    (dotimes (i +size+)
      (setf (aref vector i) (cons i (* 100 i))))
    (init-associate-avltree inst vector)
    (check-avltree-error inst)
    (format t "~S~%" inst)))

(defun main-insert ()
  (let ((inst (make-avltree)))
    (dotimes (i +size+)
      (insert-avltree inst i (* 100 i)))
    (check-avltree-error inst)
    (format t "~S~%" inst)))

;(main-init)
;(main-insert)

