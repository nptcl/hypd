(load #p"rbtree.lisp")
(load #p"test/rbtree-check.lisp")

#+sbcl (require 'sb-rt)
(defpackage work (:use common-lisp rbtree #+npt npt-rt #+sbcl sb-rt))
(in-package work)

(deftest rbtree.1
  (let ((inst (make-rbtree)))
    (check-rbtree-error inst)
    (values
      (empty-rbtree inst)
      (size-rbtree inst)))
  t 0)

(deftest rbtree.2
  (let ((inst (make-rbtree)))
    (insert-rbtree inst 10 20)
    (check-rbtree-error inst)
    (search-rbtree inst 10))
  20 t)

(deftest rbtree.3
  (let ((inst (make-rbtree)))
    (insert-rbtree inst 10 20)
    (check-rbtree-error inst)
    (search-rbtree inst 20))
  nil nil)

(deftest rbtree.4
  (let ((inst (make-rbtree)))
    (insert-rbtree inst 10 20)
    (check-rbtree-error inst)
    (values
      (empty-rbtree inst)
      (size-rbtree inst)))
  nil 1)

(deftest rbtree.5
  (let ((inst (make-rbtree)))
    (insert-rbtree inst 10 :aaa)
    (insert-rbtree inst 20 :bbb)
    (check-rbtree-error inst)
    (values
      (empty-rbtree inst)
      (size-rbtree inst)))
  nil 2)

(deftest rbtree.6
  (let ((inst (make-rbtree)))
    (insert-rbtree inst 10 :aaa)
    (insert-rbtree inst 20 :bbb)
    (check-rbtree-error inst)
    (values
      (search-rbtree inst 10)
      (search-rbtree inst 20)
      (search-rbtree inst 30)))
  :aaa :bbb nil)

(deftest rbtree.7
  (let ((inst (make-rbtree)))
    (insert-rbtree inst 20 :bbb)
    (insert-rbtree inst 10 :aaa)
    (check-rbtree-error inst)
    (values
      (search-rbtree inst 10)
      (search-rbtree inst 20)
      (search-rbtree inst 30)))
  :aaa :bbb nil)

(deftest rbtree.8
  (let ((inst (make-rbtree)))
    (dotimes (i 20)
      (insert-rbtree inst i (* 100 i))
      (check-rbtree-error inst))
    (values
      (search-rbtree inst 5)
      (search-rbtree inst 6)
      (search-rbtree inst 100)
      (size-rbtree inst)))
  500 600 nil 20)

(deftest rbtree.9
  (let ((inst (make-rbtree)))
    (dotimes (i 20)
      (let ((i (- 20 i)))
        (insert-rbtree inst i (* 100 i))))
    (check-rbtree-error inst)
    (values
      (search-rbtree inst 5)
      (search-rbtree inst 6)
      (search-rbtree inst 100)
      (size-rbtree inst)))
  500 600 nil 20)


;;
;;  init
;;
(deftest init.1
  (let ((inst (init-rbtree (constantly 10) 0)))
    (check-rbtree-error inst)
    (rbtree-p inst))
  t)

(defun init-rbtree-left (size)
  (let ((index 0))
    (init-rbtree
      (lambda ()
        (multiple-value-prog1
          (values index (* index 10))
          (incf index 1)))
      size)))

(defun init-rbtree-right (size)
  (let ((index size))
    (init-rbtree
      (lambda ()
        (decf index 1)
        (values index (* index 10)))
      size nil t)))

(defun init-rbtree-insert (size)
  (let ((inst (make-rbtree)))
    (dotimes (index size inst)
      (insert-rbtree inst index (* index 10)))))

(deftest init.2
  (let ((inst (init-rbtree-left 1)))
    (check-rbtree-error inst)
    (rbtree-p inst))
  t)

(deftest init.3
  (let ((inst (init-rbtree-right 1)))
    (check-rbtree-error inst)
    (rbtree-p inst))
  t)

(deftest init.4
  (let ((inst (init-rbtree-left 1)))
    (check-rbtree-error inst)
    (values
      (size-rbtree inst)
      (search-rbtree inst 0)))
  1 0)

(deftest init.5
  (let ((inst (init-rbtree-right 1)))
    (check-rbtree-error inst)
    (values
      (size-rbtree inst)
      (search-rbtree inst 0)))
  1 0)

(deftest init.6
  (let ((inst (init-rbtree-left 2)))
    (check-rbtree-error inst)
    (values
      (size-rbtree inst)
      (search-rbtree inst 0)
      (search-rbtree inst 1)))
  2 0 10)

(deftest init.7
  (let ((inst (init-rbtree-right 2)))
    (check-rbtree-error inst)
    (values
      (size-rbtree inst)
      (search-rbtree inst 0)
      (search-rbtree inst 1)))
  2 0 10)

(deftest init.8
  (let* ((inst (init-rbtree-left 2))
         (root (rbtree::rbtree-root inst)))
    (check-rbtree-error inst)
    (values
      (rbtree::rbnode-key
        (rbtree::rbnode-left root))
      (rbtree::rbnode-key root)))
  0 1)

(deftest init.9
  (let* ((inst (init-rbtree-right 2))
         (root (rbtree::rbtree-root inst)))
    (check-rbtree-error inst)
    (values
      (rbtree::rbnode-key root)
      (rbtree::rbnode-key
        (rbtree::rbnode-right root))))
  0 1)


;;
;;  delete
;;

;;  delete1
(deftest delete0.1
  (let ((inst (make-rbtree)))
    (prog1
      (delete-rbtree inst 10)
      (check-rbtree-error inst)))
  nil)

(deftest delete1.1
  (let ((inst (make-rbtree)) ret)
    (insert-rbtree inst 10 20)
    (setq ret (delete-rbtree inst 10))
    (check-rbtree-error inst)
    (values ret (size-rbtree inst)))
  t 0)

(deftest delete1.2
  (let ((inst (make-rbtree)) ret)
    (insert-rbtree inst 10 20)
    (setq ret (delete-rbtree inst 20))
    (check-rbtree-error inst)
    (values ret (size-rbtree inst)))
  nil 1)


;;  delete2
(deftest delete2.1
  (let ((inst (init-rbtree-left 2)) ret size root key value)
    (setq ret (delete-rbtree inst 1))
    (check-rbtree-error inst)
    (setq size (size-rbtree inst))
    (setq root (rbtree::rbtree-root inst))
    (setq key (rbtree::rbnode-key root))
    (setq value (rbtree::rbnode-value root))
    (values ret size key value))
  t 1 0 0)

(deftest delete2.2
  (let ((inst (init-rbtree-right 2)) ret size root key value)
    (setq ret (delete-rbtree inst 0))
    (check-rbtree-error inst)
    (setq size (size-rbtree inst))
    (setq root (rbtree::rbtree-root inst))
    (setq key (rbtree::rbnode-key root))
    (setq value (rbtree::rbnode-value root))
    (values ret size key value))
  t 1 1 10)

(deftest delete2.3
  (let ((inst (init-rbtree-left 2)) ret size root key value)
    (setq ret (delete-rbtree inst 0))
    (setq size (size-rbtree inst))
    (setq root (rbtree::rbtree-root inst))
    (setq key (rbtree::rbnode-key root))
    (setq value (rbtree::rbnode-value root))
    (check-rbtree-error inst)
    (values ret size key value))
  t 1 1 10)

(deftest delete2.4
  (let ((inst (init-rbtree-right 2)) ret size root key value)
    (setq ret (delete-rbtree inst 1))
    (setq size (size-rbtree inst))
    (setq root (rbtree::rbtree-root inst))
    (setq key (rbtree::rbnode-key root))
    (setq value (rbtree::rbnode-value root))
    (check-rbtree-error inst)
    (values ret size key value))
  t 1 0 0)

(deftest delete2.5
  (let ((inst (init-rbtree-left 2)))
    (delete-rbtree inst 1)
    (check-rbtree-error inst)
    (delete-rbtree inst 0)
    (check-rbtree-error inst)
    (size-rbtree inst))
  0)

(deftest delete2.6
  (let ((inst (init-rbtree-left 2)))
    (delete-rbtree inst 0)
    (check-rbtree-error inst)
    (delete-rbtree inst 1)
    (check-rbtree-error inst)
    (size-rbtree inst))
  0)

(deftest delete2.7
  (let ((inst (init-rbtree-right 2)))
    (delete-rbtree inst 1)
    (check-rbtree-error inst)
    (delete-rbtree inst 0)
    (check-rbtree-error inst)
    (size-rbtree inst))
  0)

(deftest delete2.8
  (let ((inst (init-rbtree-right 2)))
    (delete-rbtree inst 0)
    (check-rbtree-error inst)
    (delete-rbtree inst 1)
    (check-rbtree-error inst)
    (size-rbtree inst))
  0)


;;  delete3
(deftest delete3.1
  (let ((inst (init-rbtree-left 3)))
    (check-rbtree-error inst)
    (delete-rbtree inst 0)
    (check-rbtree-error inst)
    (size-rbtree inst))
  2)

(deftest delete3.2
  (let ((inst (init-rbtree-left 3)))
    (check-rbtree-error inst)
    (delete-rbtree inst 2)
    (check-rbtree-error inst)
    (size-rbtree inst))
  2)

(deftest delete3.3
  (let ((inst (init-rbtree-left 3)))
    (check-rbtree-error inst)
    (delete-rbtree inst 1)
    (check-rbtree-error inst)
    (size-rbtree inst))
  2)

(deftest delete3.4
  (let ((inst (init-rbtree-left 3)))
    (check-rbtree-error inst)
    (delete-rbtree inst 1)
    (check-rbtree-error inst)
    (size-rbtree inst))
  2)


;;  delete4
;;    2
;;   1 3
;;  0

(defun search-check-rbtree (inst list)
  (dolist (x list t)
    (multiple-value-bind (ignore check) (search-rbtree inst x)
      (declare (ignore ignore))
      (unless check
        (return nil)))))

(deftest delete4.1
  (let ((inst (init-rbtree-left 4)))
    (check-rbtree-error inst)
    (delete-rbtree inst 0)
    (check-rbtree-error inst)
    (values
      (size-rbtree inst)
      (search-rbtree inst 0)
      (search-check-rbtree inst '(1 2 3))))
  3 nil t)

(deftest delete4.2
  (let ((inst (init-rbtree-left 4)))
    (check-rbtree-error inst)
    (delete-rbtree inst 1)
    (check-rbtree-error inst)
    (values
      (size-rbtree inst)
      (search-rbtree inst 1)
      (search-check-rbtree inst '(0 2 3))))
  3 nil t)

(deftest delete4.3
  (let ((inst (init-rbtree-left 4)))
    (check-rbtree-error inst)
    (delete-rbtree inst 2)
    (check-rbtree-error inst)
    (values
      (size-rbtree inst)
      (search-rbtree inst 2)
      (search-check-rbtree inst '(0 1 3))))
  3 nil t)

(deftest delete4.4
  (let ((inst (init-rbtree-left 4)))
    (check-rbtree-error inst)
    (delete-rbtree inst 3)
    (check-rbtree-error inst)
    (values
      (size-rbtree inst)
      (search-rbtree inst 3)
      (search-check-rbtree inst '(0 1 2))))
  3 nil t)


;;  delete all
(defun delete-all-index (direct size index)
  (let ((inst (ecase direct
                (left (init-rbtree-left size))
                (right (init-rbtree-right size))
                (insert (init-rbtree-insert size)))))
    (check-rbtree-error inst)
    (delete-rbtree inst index)
    (check-rbtree-error inst)
    (unless (= (1- size) (size-rbtree inst))
      (error "size error."))
    (when (search-rbtree inst index)
      (error "search error."))
    (let* ((list (loop for i from 0 below size collect i))
           (list (delete index list)))
      (unless (search-check-rbtree inst list)
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
(deftest rbtree-delete.1
  (let ((inst (make-rbtree)))
    (prog1
      (delete-rbtree inst 10)
      (check-rbtree-error inst)))
  nil)

(deftest rbtree-delete.2
  (let ((inst (make-rbtree)))
    (insert-rbtree inst 10 20)
    (prog1
      (delete-rbtree inst 10)
      (check-rbtree-error inst)))
  t)

(deftest rbtree-delete.3
  (let ((inst (make-rbtree)))
    (insert-rbtree inst 10 20)
    (delete-rbtree inst 10)
    (check-rbtree-error inst)
    (values
      (empty-rbtree inst)
      (size-rbtree inst)))
  t 0)

(deftest rbtree-delete.4
  (let ((inst (make-rbtree)))
    (insert-rbtree inst 10 20)
    (delete-rbtree inst 30)
    (check-rbtree-error inst)
    (values
      (empty-rbtree inst)
      (size-rbtree inst)))
  nil 1)

(deftest rbtree-delete.5
  (let ((inst (make-rbtree)))
    (dotimes (i 20)
      (insert-rbtree inst i (* 100 i)))
    (delete-rbtree inst 5)
    (delete-rbtree inst 6)
    (delete-rbtree inst 7)
    (check-rbtree-error inst)
    (values
      (search-rbtree inst 4)
      (search-rbtree inst 6)
      (search-rbtree inst 100)
      (size-rbtree inst)))
  400 nil nil 17)

(deftest rbtree-delete.6
  (let ((inst (make-rbtree)))
    (dotimes (i 20)
      (insert-rbtree inst i (* 100 i)))
    (dotimes (i 20)
      (delete-rbtree inst i))
    (values
      (empty-rbtree inst)
      (size-rbtree inst)))
  t 0)

(deftest rbtree-min.1
  (let ((inst (make-rbtree)))
    (insert-rbtree inst 10 100)
    (insert-rbtree inst 20 200)
    (insert-rbtree inst 30 300)
    (min-rbtree inst))
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
  (let ((inst (make-rbtree))
        (vector (make-array +size+ :initial-element nil)))
    (dotimes (i +size+)
      (setf (aref vector i) (cons i (* 100 i))))
    (init-associate-rbtree inst vector)
    (check-rbtree-error inst)
    (format t "~S~%" inst)))

(defun main-insert ()
  (let ((inst (make-rbtree)))
    (dotimes (i +size+)
      (insert-rbtree inst i (* 100 i)))
    (check-rbtree-error inst)
    (format t "~S~%" inst)))

;(main-init)
;(main-insert)

