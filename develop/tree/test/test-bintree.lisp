(load #p"bintree.lisp")
(load #p"test/bintree-check.lisp")

#+sbcl (require 'sb-rt)
(defpackage work (:use common-lisp bintree #+npt npt-rt #+sbcl sb-rt))
(in-package work)

(deftest bintree.1
  (let ((inst (make-bintree)))
    (check-bintree-error inst)
    (values
      (empty-bintree inst)
      (size-bintree inst)))
  t 0)

(deftest bintree.2
  (let ((inst (make-bintree)))
    (insert-bintree inst 10 20)
    (check-bintree-error inst)
    (search-bintree inst 10))
  20 t)

(deftest bintree.3
  (let ((inst (make-bintree)))
    (insert-bintree inst 10 20)
    (check-bintree-error inst)
    (search-bintree inst 20))
  nil nil)

(deftest bintree.4
  (let ((inst (make-bintree)))
    (insert-bintree inst 10 20)
    (check-bintree-error inst)
    (values
      (empty-bintree inst)
      (size-bintree inst)))
  nil 1)

(deftest bintree.5
  (let ((inst (make-bintree)))
    (insert-bintree inst 10 :aaa)
    (insert-bintree inst 20 :bbb)
    (check-bintree-error inst)
    (values
      (empty-bintree inst)
      (size-bintree inst)))
  nil 2)

(deftest bintree.6
  (let ((inst (make-bintree)))
    (insert-bintree inst 10 :aaa)
    (insert-bintree inst 20 :bbb)
    (check-bintree-error inst)
    (values
      (search-bintree inst 10)
      (search-bintree inst 20)
      (search-bintree inst 30)))
  :aaa :bbb nil)

(deftest bintree.7
  (let ((inst (make-bintree)))
    (insert-bintree inst 20 :bbb)
    (insert-bintree inst 10 :aaa)
    (check-bintree-error inst)
    (values
      (search-bintree inst 10)
      (search-bintree inst 20)
      (search-bintree inst 30)))
  :aaa :bbb nil)

(deftest bintree.8
  (let ((inst (make-bintree)))
    (dotimes (i 20)
      (insert-bintree inst i (* 100 i))
      (check-bintree-error inst))
    (values
      (search-bintree inst 5)
      (search-bintree inst 6)
      (search-bintree inst 100)
      (size-bintree inst)))
  500 600 nil 20)

(deftest bintree.9
  (let ((inst (make-bintree)))
    (dotimes (i 20)
      (let ((i (- 20 i)))
        (insert-bintree inst i (* 100 i))))
    (check-bintree-error inst)
    (values
      (search-bintree inst 5)
      (search-bintree inst 6)
      (search-bintree inst 100)
      (size-bintree inst)))
  500 600 nil 20)


;;
;;  init
;;
(deftest init.1
  (let ((inst (init-bintree (constantly 10) 0)))
    (check-bintree-error inst)
    (bintree-p inst))
  t)

(defun init-bintree-left (size)
  (let ((index 0))
    (init-bintree
      (lambda ()
        (multiple-value-prog1
          (values index (* index 10))
          (incf index 1)))
      size)))

(defun init-bintree-right (size)
  (let ((index size))
    (init-bintree
      (lambda ()
        (decf index 1)
        (values index (* index 10)))
      size nil t)))

(defun init-bintree-insert (size)
  (let ((inst (make-bintree)))
    (dotimes (index size inst)
      (insert-bintree inst index (* index 10)))))

(deftest init.2
  (let ((inst (init-bintree-left 1)))
    (check-bintree-error inst)
    (bintree-p inst))
  t)

(deftest init.3
  (let ((inst (init-bintree-right 1)))
    (check-bintree-error inst)
    (bintree-p inst))
  t)

(deftest init.4
  (let ((inst (init-bintree-left 1)))
    (check-bintree-error inst)
    (values
      (size-bintree inst)
      (search-bintree inst 0)))
  1 0)

(deftest init.5
  (let ((inst (init-bintree-right 1)))
    (check-bintree-error inst)
    (values
      (size-bintree inst)
      (search-bintree inst 0)))
  1 0)

(deftest init.6
  (let ((inst (init-bintree-left 2)))
    (check-bintree-error inst)
    (values
      (size-bintree inst)
      (search-bintree inst 0)
      (search-bintree inst 1)))
  2 0 10)

(deftest init.7
  (let ((inst (init-bintree-right 2)))
    (check-bintree-error inst)
    (values
      (size-bintree inst)
      (search-bintree inst 0)
      (search-bintree inst 1)))
  2 0 10)

(deftest init.8
  (let* ((inst (init-bintree-left 2))
         (root (bintree::bintree-root inst)))
    (check-bintree-error inst)
    (values
      (bintree::binnode-key
        (bintree::binnode-left root))
      (bintree::binnode-key root)))
  0 1)

(deftest init.9
  (let* ((inst (init-bintree-right 2))
         (root (bintree::bintree-root inst)))
    (check-bintree-error inst)
    (values
      (bintree::binnode-key root)
      (bintree::binnode-key
        (bintree::binnode-right root))))
  0 1)


;;
;;  delete
;;

;;  delete1
(deftest delete0.1
  (let ((inst (make-bintree)))
    (prog1
      (delete-bintree inst 10)
      (check-bintree-error inst)))
  nil)

(deftest delete1.1
  (let ((inst (make-bintree)) ret)
    (insert-bintree inst 10 20)
    (setq ret (delete-bintree inst 10))
    (check-bintree-error inst)
    (values ret (size-bintree inst)))
  t 0)

(deftest delete1.2
  (let ((inst (make-bintree)) ret)
    (insert-bintree inst 10 20)
    (setq ret (delete-bintree inst 20))
    (check-bintree-error inst)
    (values ret (size-bintree inst)))
  nil 1)


;;  delete2
(deftest delete2.1
  (let ((inst (init-bintree-left 2)) ret size root key value)
    (setq ret (delete-bintree inst 1))
    (check-bintree-error inst)
    (setq size (size-bintree inst))
    (setq root (bintree::bintree-root inst))
    (setq key (bintree::binnode-key root))
    (setq value (bintree::binnode-value root))
    (values ret size key value))
  t 1 0 0)

(deftest delete2.2
  (let ((inst (init-bintree-right 2)) ret size root key value)
    (setq ret (delete-bintree inst 0))
    (check-bintree-error inst)
    (setq size (size-bintree inst))
    (setq root (bintree::bintree-root inst))
    (setq key (bintree::binnode-key root))
    (setq value (bintree::binnode-value root))
    (values ret size key value))
  t 1 1 10)

(deftest delete2.3
  (let ((inst (init-bintree-left 2)) ret size root key value)
    (setq ret (delete-bintree inst 0))
    (setq size (size-bintree inst))
    (setq root (bintree::bintree-root inst))
    (setq key (bintree::binnode-key root))
    (setq value (bintree::binnode-value root))
    (check-bintree-error inst)
    (values ret size key value))
  t 1 1 10)

(deftest delete2.4
  (let ((inst (init-bintree-right 2)) ret size root key value)
    (setq ret (delete-bintree inst 1))
    (setq size (size-bintree inst))
    (setq root (bintree::bintree-root inst))
    (setq key (bintree::binnode-key root))
    (setq value (bintree::binnode-value root))
    (check-bintree-error inst)
    (values ret size key value))
  t 1 0 0)

(deftest delete2.5
  (let ((inst (init-bintree-left 2)))
    (delete-bintree inst 1)
    (check-bintree-error inst)
    (delete-bintree inst 0)
    (check-bintree-error inst)
    (size-bintree inst))
  0)

(deftest delete2.6
  (let ((inst (init-bintree-left 2)))
    (delete-bintree inst 0)
    (check-bintree-error inst)
    (delete-bintree inst 1)
    (check-bintree-error inst)
    (size-bintree inst))
  0)

(deftest delete2.7
  (let ((inst (init-bintree-right 2)))
    (delete-bintree inst 1)
    (check-bintree-error inst)
    (delete-bintree inst 0)
    (check-bintree-error inst)
    (size-bintree inst))
  0)

(deftest delete2.8
  (let ((inst (init-bintree-right 2)))
    (delete-bintree inst 0)
    (check-bintree-error inst)
    (delete-bintree inst 1)
    (check-bintree-error inst)
    (size-bintree inst))
  0)


;;  delete3
(deftest delete3.1
  (let ((inst (init-bintree-left 3)))
    (check-bintree-error inst)
    (delete-bintree inst 0)
    (check-bintree-error inst)
    (size-bintree inst))
  2)

(deftest delete3.2
  (let ((inst (init-bintree-left 3)))
    (check-bintree-error inst)
    (delete-bintree inst 2)
    (check-bintree-error inst)
    (size-bintree inst))
  2)

(deftest delete3.3
  (let ((inst (init-bintree-left 3)))
    (check-bintree-error inst)
    (delete-bintree inst 1)
    (check-bintree-error inst)
    (size-bintree inst))
  2)

(deftest delete3.4
  (let ((inst (init-bintree-left 3)))
    (check-bintree-error inst)
    (delete-bintree inst 1)
    (check-bintree-error inst)
    (size-bintree inst))
  2)


;;  delete4
;;    2
;;   1 3
;;  0

(defun search-check-bintree (inst list)
  (dolist (x list t)
    (multiple-value-bind (ignore check) (search-bintree inst x)
      (declare (ignore ignore))
      (unless check
        (return nil)))))

(deftest delete4.1
  (let ((inst (init-bintree-left 4)))
    (check-bintree-error inst)
    (delete-bintree inst 0)
    (check-bintree-error inst)
    (values
      (size-bintree inst)
      (search-bintree inst 0)
      (search-check-bintree inst '(1 2 3))))
  3 nil t)

(deftest delete4.2
  (let ((inst (init-bintree-left 4)))
    (check-bintree-error inst)
    (delete-bintree inst 1)
    (check-bintree-error inst)
    (values
      (size-bintree inst)
      (search-bintree inst 1)
      (search-check-bintree inst '(0 2 3))))
  3 nil t)

(deftest delete4.3
  (let ((inst (init-bintree-left 4)))
    (check-bintree-error inst)
    (delete-bintree inst 2)
    (check-bintree-error inst)
    (values
      (size-bintree inst)
      (search-bintree inst 2)
      (search-check-bintree inst '(0 1 3))))
  3 nil t)

(deftest delete4.4
  (let ((inst (init-bintree-left 4)))
    (check-bintree-error inst)
    (delete-bintree inst 3)
    (check-bintree-error inst)
    (values
      (size-bintree inst)
      (search-bintree inst 3)
      (search-check-bintree inst '(0 1 2))))
  3 nil t)


;;  delete all
(defun delete-all-index (direct size index)
  (let ((inst (ecase direct
                (left (init-bintree-left size))
                (right (init-bintree-right size))
                (insert (init-bintree-insert size)))))
    (check-bintree-error inst)
    (delete-bintree inst index)
    (check-bintree-error inst)
    (unless (= (1- size) (size-bintree inst))
      (error "size error."))
    (when (search-bintree inst index)
      (error "search error."))
    (let* ((list (loop for i from 0 below size collect i))
           (list (delete index list)))
      (unless (search-check-bintree inst list)
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
(deftest bintree-delete.1
  (let ((inst (make-bintree)))
    (prog1
      (delete-bintree inst 10)
      (check-bintree-error inst)))
  nil)

(deftest bintree-delete.2
  (let ((inst (make-bintree)))
    (insert-bintree inst 10 20)
    (prog1
      (delete-bintree inst 10)
      (check-bintree-error inst)))
  t)

(deftest bintree-delete.3
  (let ((inst (make-bintree)))
    (insert-bintree inst 10 20)
    (delete-bintree inst 10)
    (check-bintree-error inst)
    (values
      (empty-bintree inst)
      (size-bintree inst)))
  t 0)

(deftest bintree-delete.4
  (let ((inst (make-bintree)))
    (insert-bintree inst 10 20)
    (delete-bintree inst 30)
    (check-bintree-error inst)
    (values
      (empty-bintree inst)
      (size-bintree inst)))
  nil 1)

(deftest bintree-delete.5
  (let ((inst (make-bintree)))
    (dotimes (i 20)
      (insert-bintree inst i (* 100 i)))
    (delete-bintree inst 5)
    (delete-bintree inst 6)
    (delete-bintree inst 7)
    (check-bintree-error inst)
    (values
      (search-bintree inst 4)
      (search-bintree inst 6)
      (search-bintree inst 100)
      (size-bintree inst)))
  400 nil nil 17)

(deftest bintree-delete.6
  (let ((inst (make-bintree)))
    (dotimes (i 20)
      (insert-bintree inst i (* 100 i)))
    (dotimes (i 20)
      (delete-bintree inst i))
    (values
      (empty-bintree inst)
      (size-bintree inst)))
  t 0)

(deftest bintree-min.1
  (let ((inst (make-bintree)))
    (insert-bintree inst 10 100)
    (insert-bintree inst 20 200)
    (insert-bintree inst 30 300)
    (min-bintree inst))
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
  (let ((inst (make-bintree))
        (vector (make-array +size+ :initial-element nil)))
    (dotimes (i +size+)
      (setf (aref vector i) (cons i (* 100 i))))
    (init-associate-bintree inst vector)
    (check-bintree-error inst)
    (format t "~S~%" inst)))

(defun main-insert ()
  (let ((inst (make-bintree)))
    (dotimes (i +size+)
      (insert-bintree inst i (* 100 i)))
    (check-bintree-error inst)
    (format t "~S~%" inst)))

;(main-init)
;(main-insert)

