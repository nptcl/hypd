(load #p"avlptree.lisp")
(load #p"test/avlptree-check.lisp")

#+sbcl (require 'sb-rt)
(defpackage work (:use common-lisp avlptree #+npt npt-rt #+sbcl sb-rt))
(in-package work)

(deftest avlptree.1
  (let ((inst (make-avlptree)))
    (check-avlptree-error inst)
    (values
      (empty-avlptree inst)
      (size-avlptree inst)))
  t 0)

(deftest avlptree.2
  (let ((inst (make-avlptree)))
    (insert-avlptree inst 10 20)
    (check-avlptree-error inst)
    (search-avlptree inst 10))
  20 t)

(deftest avlptree.3
  (let ((inst (make-avlptree)))
    (insert-avlptree inst 10 20)
    (check-avlptree-error inst)
    (search-avlptree inst 20))
  nil nil)

(deftest avlptree.4
  (let ((inst (make-avlptree)))
    (insert-avlptree inst 10 20)
    (check-avlptree-error inst)
    (values
      (empty-avlptree inst)
      (size-avlptree inst)))
  nil 1)

(deftest avlptree.5
  (let ((inst (make-avlptree)))
    (insert-avlptree inst 10 :aaa)
    (insert-avlptree inst 20 :bbb)
    (check-avlptree-error inst)
    (values
      (empty-avlptree inst)
      (size-avlptree inst)))
  nil 2)

(deftest avlptree.6
  (let ((inst (make-avlptree)))
    (insert-avlptree inst 10 :aaa)
    (insert-avlptree inst 20 :bbb)
    (check-avlptree-error inst)
    (values
      (search-avlptree inst 10)
      (search-avlptree inst 20)
      (search-avlptree inst 30)))
  :aaa :bbb nil)

(deftest avlptree.7
  (let ((inst (make-avlptree)))
    (insert-avlptree inst 20 :bbb)
    (insert-avlptree inst 10 :aaa)
    (check-avlptree-error inst)
    (values
      (search-avlptree inst 10)
      (search-avlptree inst 20)
      (search-avlptree inst 30)))
  :aaa :bbb nil)

(deftest avlptree.8
  (let ((inst (make-avlptree)))
    (dotimes (i 20)
      (insert-avlptree inst i (* 100 i))
      (check-avlptree-error inst))
    (values
      (search-avlptree inst 5)
      (search-avlptree inst 6)
      (search-avlptree inst 100)
      (size-avlptree inst)))
  500 600 nil 20)

(deftest avlptree.9
  (let ((inst (make-avlptree)))
    (dotimes (i 20)
      (let ((i (- 20 i)))
        (insert-avlptree inst i (* 100 i))))
    (check-avlptree-error inst)
    (values
      (search-avlptree inst 5)
      (search-avlptree inst 6)
      (search-avlptree inst 100)
      (size-avlptree inst)))
  500 600 nil 20)


;;
;;  init
;;
(deftest init.1
  (let ((inst (init-avlptree (constantly 10) 0)))
    (check-avlptree-error inst)
    (avlptree-p inst))
  t)

(defun init-avlptree-left (size)
  (let ((index 0))
    (init-avlptree
      (lambda ()
        (multiple-value-prog1
          (values index (* index 10))
          (incf index 1)))
      size)))

(defun init-avlptree-right (size)
  (let ((index size))
    (init-avlptree
      (lambda ()
        (decf index 1)
        (values index (* index 10)))
      size nil t)))

(defun init-avlptree-insert (size)
  (let ((inst (make-avlptree)))
    (dotimes (index size inst)
      (insert-avlptree inst index (* index 10)))))

(deftest init.2
  (let ((inst (init-avlptree-left 1)))
    (check-avlptree-error inst)
    (avlptree-p inst))
  t)

(deftest init.3
  (let ((inst (init-avlptree-right 1)))
    (check-avlptree-error inst)
    (avlptree-p inst))
  t)

(deftest init.4
  (let ((inst (init-avlptree-left 1)))
    (check-avlptree-error inst)
    (values
      (size-avlptree inst)
      (search-avlptree inst 0)))
  1 0)

(deftest init.5
  (let ((inst (init-avlptree-right 1)))
    (check-avlptree-error inst)
    (values
      (size-avlptree inst)
      (search-avlptree inst 0)))
  1 0)

(deftest init.6
  (let ((inst (init-avlptree-left 2)))
    (check-avlptree-error inst)
    (values
      (size-avlptree inst)
      (search-avlptree inst 0)
      (search-avlptree inst 1)))
  2 0 10)

(deftest init.7
  (let ((inst (init-avlptree-right 2)))
    (check-avlptree-error inst)
    (values
      (size-avlptree inst)
      (search-avlptree inst 0)
      (search-avlptree inst 1)))
  2 0 10)

(deftest init.8
  (let* ((inst (init-avlptree-left 2))
         (root (avlptree::avlptree-root inst)))
    (check-avlptree-error inst)
    (values
      (avlptree::avlpnode-key
        (avlptree::avlpnode-left root))
      (avlptree::avlpnode-key root)))
  0 1)

(deftest init.9
  (let* ((inst (init-avlptree-right 2))
         (root (avlptree::avlptree-root inst)))
    (check-avlptree-error inst)
    (values
      (avlptree::avlpnode-key root)
      (avlptree::avlpnode-key
        (avlptree::avlpnode-right root))))
  0 1)


;;
;;  delete
;;

;;  delete1
(deftest delete0.1
  (let ((inst (make-avlptree)))
    (prog1
      (delete-avlptree inst 10)
      (check-avlptree-error inst)))
  nil)

(deftest delete1.1
  (let ((inst (make-avlptree)) ret)
    (insert-avlptree inst 10 20)
    (setq ret (delete-avlptree inst 10))
    (check-avlptree-error inst)
    (values ret (size-avlptree inst)))
  t 0)

(deftest delete1.2
  (let ((inst (make-avlptree)) ret)
    (insert-avlptree inst 10 20)
    (setq ret (delete-avlptree inst 20))
    (check-avlptree-error inst)
    (values ret (size-avlptree inst)))
  nil 1)


;;  delete2
(deftest delete2.1
  (let ((inst (init-avlptree-left 2)) ret size root key value)
    (setq ret (delete-avlptree inst 1))
    (check-avlptree-error inst)
    (setq size (size-avlptree inst))
    (setq root (avlptree::avlptree-root inst))
    (setq key (avlptree::avlpnode-key root))
    (setq value (avlptree::avlpnode-value root))
    (values ret size key value))
  t 1 0 0)

(deftest delete2.2
  (let ((inst (init-avlptree-right 2)) ret size root key value)
    (setq ret (delete-avlptree inst 0))
    (check-avlptree-error inst)
    (setq size (size-avlptree inst))
    (setq root (avlptree::avlptree-root inst))
    (setq key (avlptree::avlpnode-key root))
    (setq value (avlptree::avlpnode-value root))
    (values ret size key value))
  t 1 1 10)

(deftest delete2.3
  (let ((inst (init-avlptree-left 2)) ret size root key value)
    (setq ret (delete-avlptree inst 0))
    (setq size (size-avlptree inst))
    (setq root (avlptree::avlptree-root inst))
    (setq key (avlptree::avlpnode-key root))
    (setq value (avlptree::avlpnode-value root))
    (check-avlptree-error inst)
    (values ret size key value))
  t 1 1 10)

(deftest delete2.4
  (let ((inst (init-avlptree-right 2)) ret size root key value)
    (setq ret (delete-avlptree inst 1))
    (setq size (size-avlptree inst))
    (setq root (avlptree::avlptree-root inst))
    (setq key (avlptree::avlpnode-key root))
    (setq value (avlptree::avlpnode-value root))
    (check-avlptree-error inst)
    (values ret size key value))
  t 1 0 0)

(deftest delete2.5
  (let ((inst (init-avlptree-left 2)))
    (delete-avlptree inst 1)
    (check-avlptree-error inst)
    (delete-avlptree inst 0)
    (check-avlptree-error inst)
    (size-avlptree inst))
  0)

(deftest delete2.6
  (let ((inst (init-avlptree-left 2)))
    (delete-avlptree inst 0)
    (check-avlptree-error inst)
    (delete-avlptree inst 1)
    (check-avlptree-error inst)
    (size-avlptree inst))
  0)

(deftest delete2.7
  (let ((inst (init-avlptree-right 2)))
    (delete-avlptree inst 1)
    (check-avlptree-error inst)
    (delete-avlptree inst 0)
    (check-avlptree-error inst)
    (size-avlptree inst))
  0)

(deftest delete2.8
  (let ((inst (init-avlptree-right 2)))
    (delete-avlptree inst 0)
    (check-avlptree-error inst)
    (delete-avlptree inst 1)
    (check-avlptree-error inst)
    (size-avlptree inst))
  0)


;;  delete3
(deftest delete3.1
  (let ((inst (init-avlptree-left 3)))
    (check-avlptree-error inst)
    (delete-avlptree inst 0)
    (check-avlptree-error inst)
    (size-avlptree inst))
  2)

(deftest delete3.2
  (let ((inst (init-avlptree-left 3)))
    (check-avlptree-error inst)
    (delete-avlptree inst 2)
    (check-avlptree-error inst)
    (size-avlptree inst))
  2)

(deftest delete3.3
  (let ((inst (init-avlptree-left 3)))
    (check-avlptree-error inst)
    (delete-avlptree inst 1)
    (check-avlptree-error inst)
    (size-avlptree inst))
  2)

(deftest delete3.4
  (let ((inst (init-avlptree-left 3)))
    (check-avlptree-error inst)
    (delete-avlptree inst 1)
    (check-avlptree-error inst)
    (size-avlptree inst))
  2)


;;  delete4
;;    2
;;   1 3
;;  0

(defun search-check-avlptree (inst list)
  (dolist (x list t)
    (multiple-value-bind (ignore check) (search-avlptree inst x)
      (declare (ignore ignore))
      (unless check
        (return nil)))))

(deftest delete4.1
  (let ((inst (init-avlptree-left 4)))
    (check-avlptree-error inst)
    (delete-avlptree inst 0)
    (check-avlptree-error inst)
    (values
      (size-avlptree inst)
      (search-avlptree inst 0)
      (search-check-avlptree inst '(1 2 3))))
  3 nil t)

(deftest delete4.2
  (let ((inst (init-avlptree-left 4)))
    (check-avlptree-error inst)
    (delete-avlptree inst 1)
    (check-avlptree-error inst)
    (values
      (size-avlptree inst)
      (search-avlptree inst 1)
      (search-check-avlptree inst '(0 2 3))))
  3 nil t)

(deftest delete4.3
  (let ((inst (init-avlptree-left 4)))
    (check-avlptree-error inst)
    (delete-avlptree inst 2)
    (check-avlptree-error inst)
    (values
      (size-avlptree inst)
      (search-avlptree inst 2)
      (search-check-avlptree inst '(0 1 3))))
  3 nil t)

(deftest delete4.4
  (let ((inst (init-avlptree-left 4)))
    (check-avlptree-error inst)
    (delete-avlptree inst 3)
    (check-avlptree-error inst)
    (values
      (size-avlptree inst)
      (search-avlptree inst 3)
      (search-check-avlptree inst '(0 1 2))))
  3 nil t)


;;  delete all
(defun delete-all-index (direct size index)
  (let ((inst (ecase direct
                (left (init-avlptree-left size))
                (right (init-avlptree-right size))
                (insert (init-avlptree-insert size)))))
    (check-avlptree-error inst)
    (delete-avlptree inst index)
    (check-avlptree-error inst)
    (unless (= (1- size) (size-avlptree inst))
      (error "size error."))
    (when (search-avlptree inst index)
      (error "search error."))
    (let* ((list (loop for i from 0 below size collect i))
           (list (delete index list)))
      (unless (search-check-avlptree inst list)
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
(deftest avlptree-delete.1
  (let ((inst (make-avlptree)))
    (prog1
      (delete-avlptree inst 10)
      (check-avlptree-error inst)))
  nil)

(deftest avlptree-delete.2
  (let ((inst (make-avlptree)))
    (insert-avlptree inst 10 20)
    (prog1
      (delete-avlptree inst 10)
      (check-avlptree-error inst)))
  t)

(deftest avlptree-delete.3
  (let ((inst (make-avlptree)))
    (insert-avlptree inst 10 20)
    (delete-avlptree inst 10)
    (check-avlptree-error inst)
    (values
      (empty-avlptree inst)
      (size-avlptree inst)))
  t 0)

(deftest avlptree-delete.4
  (let ((inst (make-avlptree)))
    (insert-avlptree inst 10 20)
    (delete-avlptree inst 30)
    (check-avlptree-error inst)
    (values
      (empty-avlptree inst)
      (size-avlptree inst)))
  nil 1)

(deftest avlptree-delete.5
  (let ((inst (make-avlptree)))
    (dotimes (i 20)
      (insert-avlptree inst i (* 100 i)))
    (delete-avlptree inst 5)
    (delete-avlptree inst 6)
    (delete-avlptree inst 7)
    (check-avlptree-error inst)
    (values
      (search-avlptree inst 4)
      (search-avlptree inst 6)
      (search-avlptree inst 100)
      (size-avlptree inst)))
  400 nil nil 17)

(deftest avlptree-delete.6
  (let ((inst (make-avlptree)))
    (dotimes (i 20)
      (insert-avlptree inst i (* 100 i)))
    (dotimes (i 20)
      (delete-avlptree inst i))
    (values
      (empty-avlptree inst)
      (size-avlptree inst)))
  t 0)

(deftest avlptree-min.1
  (let ((inst (make-avlptree)))
    (insert-avlptree inst 10 100)
    (insert-avlptree inst 20 200)
    (insert-avlptree inst 30 300)
    (min-avlptree inst))
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
  (let ((inst (make-avlptree))
        (vector (make-array +size+ :initial-element nil)))
    (dotimes (i +size+)
      (setf (aref vector i) (cons i (* 100 i))))
    (init-associate-avlptree inst vector)
    (check-avlptree-error inst)
    (format t "~S~%" inst)))

(defun main-insert ()
  (let ((inst (make-avlptree)))
    (dotimes (i +size+)
      (insert-avlptree inst i (* 100 i)))
    (check-avlptree-error inst)
    (format t "~S~%" inst)))

;(main-init)
;(main-insert)

