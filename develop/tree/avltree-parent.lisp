;;
;;  AVL tree (parent)
;;
(defpackage #:avlptree
  (:use common-lisp)
  (:export
    #:avlptree
    #:avlptree-p
    #:make-avlptree
    #:clear-avlptree
    #:size-avlptree
    #:empty-avlptree
    #:min-avlptree
    #:max-avlptree
    #:map-avlptree
    #:map-avlpnode
    #:keys-avlptree
    #:values-avlptree
    #:hash-table-avlptree

    #:*replace-mode-avlptree*
    #:*replace-key-avlptree*
    #:insert-avlptree
    #:intern-avlptree
    #:search-avlptree
    #:replace-avlptree
    #:delete-avlptree
    #:init-avlptree
    #:init-property-avlptree
    #:init-associate-avlptree))

(in-package #:avlptree)

;;
;;  avlptree
;;
(defstruct (avlptree
             (:constructor avlptree-heap)
             (:copier nil))
  root
  (compare #'- :type function)
  (size 0 :type unsigned-byte))

(defmethod print-object ((inst avlptree) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (avlptree-size inst))))

(defun make-avlptree (&key (compare #'-))
  (avlptree-heap :compare compare))

(defun clear-avlptree (inst)
  (declare (type avlptree inst))
  (setf (avlptree-root inst) nil)
  (setf (avlptree-size inst) 0))

(defun size-avlptree (inst)
  (declare (type avlptree inst))
  (avlptree-size inst))


;;
;;  avlpnode
;;
(defstruct (avlpnode (:copier nil))
  parent left right key value
  (balance 0 :type (integer -2 2)))  ;; left - right

(defmethod print-object ((inst avlpnode) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (avlpnode-balance inst))))


;;
;;  operator
;;
(defun empty-avlptree (inst)
  (declare (type avlptree inst))
  (if (avlptree-root inst) nil t))

(defun min-avlpnode (node)
  (let ((left (avlpnode-left node)))
    (if left
      (min-avlpnode left)
      (values (avlpnode-key node) t))))

(defun min-avlptree (inst)
  (declare (type avlptree inst))
  (let ((root (avlptree-root inst)))
    (multiple-value-bind (x y)
      (when root
        (min-avlpnode root))
      (if y
        (values x t)
        (values nil nil)))))

(defun max-avlpnode (node)
  (let ((right (avlpnode-right node)))
    (if right
      (max-avlpnode right)
      (values (avlpnode-key node) t))))

(defun max-avlptree (inst)
  (declare (type avlptree inst))
  (let ((root (avlptree-root inst)))
    (multiple-value-bind (x y)
      (when root
        (max-avlpnode root))
      (if y
        (values x t)
        (values nil nil)))))

(defun map-avlpnode (call inst)
  (declare (type avlptree inst))
  (labels ((rec (x) (when x
                      (rec (avlpnode-right x))
                      (funcall call x)
                      (rec (avlpnode-left x)))))
    (rec (avlptree-root inst)))
  (values))

(defun map-avlptree (call inst)
  (declare (type avlptree inst))
  (map-avlpnode
    (lambda (x)
      (funcall call (avlpnode-key x) (avlpnode-value x)))
    inst))

(defun keys-avlptree (inst)
  (declare (type avlptree inst))
  (let (list)
    (map-avlpnode
      (lambda (x)
        (push (avlpnode-key x) list))
      inst)
    list))

(defun values-avlptree (inst)
  (declare (type avlptree inst))
  (let (list)
    (map-avlpnode
      (lambda (x)
        (push (avlpnode-value x) list))
      inst)
    list))

(defun hash-table-avlptree (inst &optional (test 'eql))
  (declare (type avlptree inst))
  (let ((table (make-hash-table :test test)))
    (map-avlptree
      (lambda (key value)
        (setf (gethash key table) value))
      inst)
    table))


;;
;;  rotate
;;
(defun rotate-left-avlpnode (node)
  (let* ((p (avlpnode-parent node))
         (leftp (and p (eq (avlpnode-left p) node)))
         (x (avlpnode-right node))
         (y (avlpnode-left x)))
    (setf (avlpnode-right node) y)
    (when y
      (setf (avlpnode-parent y) node))
    (setf (avlpnode-left x) node)
    (setf (avlpnode-parent node) x)
    (if (null p)
      (setf (avlpnode-parent x) nil)
      (if leftp
        (setf (avlpnode-left p) x)
        (setf (avlpnode-right p) x)))
    x))

(defun rotate-right-avlpnode (node)
  (let* ((p (avlpnode-parent node))
         (leftp (and p (eq (avlpnode-left p) node)))
         (x (avlpnode-left node))
         (y (avlpnode-right x)))
    (setf (avlpnode-left node) y)
    (when y
      (setf (avlpnode-parent y) node))
    (setf (avlpnode-right x) node)
    (setf (avlpnode-parent node) x)
    (if (null p)
      (setf (avlpnode-parent x) nil)
      (if leftp
        (setf (avlpnode-left p) x)
        (setf (avlpnode-right p) x)))
    x))


;;
;;  insert
;;
(defvar *replace-mode-avlptree* nil)
(defvar *replace-key-avlptree* t)

(defun replace-avlpnode (node key value)
  (when *replace-key-avlptree*
    (setf (avlpnode-key node) key))
  (setf (avlpnode-value node) value))

(defun update-balance-avlpnode (node)
  (let ((left (avlpnode-left node))
        (right (avlpnode-right node))
        (balance (avlpnode-balance node)))
    (cond ((= balance 1)
           (setf (avlpnode-balance left) 0)
           (setf (avlpnode-balance right) -1))
          ((= balance -1)
           (setf (avlpnode-balance left) 1)
           (setf (avlpnode-balance right) 0))
          (t (setf (avlpnode-balance left) 0)
             (setf (avlpnode-balance right) 0)))
    (setf (avlpnode-balance node) 0)
    node))

(defun insert-lr-avlpnode (node left)
  (let ((left (rotate-left-avlpnode left)))
    (setf (avlpnode-left node) left)
    (setf (avlpnode-parent left) node))
  (update-balance-avlpnode
    (rotate-right-avlpnode node)))

(defun insert-ll-avlpnode (node)
  (let ((next (rotate-right-avlpnode node)))
    (setf (avlpnode-balance next) 0)
    (setf (avlpnode-balance node) 0)
    next))

(defun insert-left-avlpnode (node)
  (let* ((left (avlpnode-left node))
         (balance (avlpnode-balance left)))
    (if (< balance 0)
      (insert-lr-avlpnode node left)
      (insert-ll-avlpnode node))))

(defun insert-rl-avlpnode (node right)
  (let ((right (rotate-right-avlpnode right)))
    (setf (avlpnode-right node) right)
    (setf (avlpnode-parent right) node))
  (update-balance-avlpnode
    (rotate-left-avlpnode node)))

(defun insert-rr-avlpnode (node)
  (let ((next (rotate-left-avlpnode node)))
    (setf (avlpnode-balance next) 0)
    (setf (avlpnode-balance node) 0)
    next))

(defun insert-right-avlpnode (node)
  (let* ((right (avlpnode-right node))
         (balance (avlpnode-balance right)))
    (if (< 0 balance)
      (insert-rl-avlpnode node right)
      (insert-rr-avlpnode node))))

(defun insert-avlpnode (compare node key value)
  (declare (type (or avlpnode null) node))
  (prog (key1 diff next check)
    ;;  nil
    (unless node
      (setq next (make-avlpnode :key key :value value))
      (return (values next 'make)))

    ;;  move
    (setq key1 (avlpnode-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (go move-left))
    (when (< 0 diff)
      (go move-right))

    ;;  equal
    (when *replace-mode-avlptree*
      (replace-avlnode node key value))
    (return (values nil nil))

    ;;  left
    move-left
    (setq next (avlpnode-left node))
    (multiple-value-setq (next check) (insert-avlpnode compare next key value))
    (unless check
      (return (values nil nil)))
    (when (eq check t)
      (return (values node t)))
    (when (eq check 'finish)
      (setf (avlpnode-left node) next)
      (setf (avlpnode-parent next) node)
      (return (values node t)))
    (when (eq check 'make)
      (setf (avlpnode-left node) next)
      (setf (avlpnode-parent next) node))
    (incf (avlpnode-balance node) 1)
    (go balance)

    ;;  right
    move-right
    (setq next (avlpnode-right node))
    (multiple-value-setq (next check) (insert-avlpnode compare next key value))
    (unless check
      (return (values nil nil)))
    (when (eq check t)
      (return (values node t)))
    (when (eq check 'finish)
      (setf (avlpnode-right node) next)
      (setf (avlpnode-parent next) node)
      (return (values node t)))
    (when (eq check 'make)
      (setf (avlpnode-right node) next)
      (setf (avlpnode-parent next) node))
    (decf (avlpnode-balance node) 1)
    (go balance)

    ;;  balance
    balance
    (setq check (avlpnode-balance node))
    (when (zerop check)
      (return (values node t)))  ;;  skip
    (when (< 1 check)  ;;  2
      (setq next (insert-left-avlpnode node))
      (return (values next 'finish)))
    (when (< check -1)  ;;  -2
      (setq next (insert-right-avlpnode node))
      (return (values next 'finish)))
    ;;  1 or -1
    (return (values node 'loop))))

(defun insert-avlptree (inst key value &optional (replace nil opt))
  (declare (type avlptree inst))
  (let ((compare (avlptree-compare inst))
        (root (avlptree-root inst))
        (*replace-mode-avlptree* (if opt replace *replace-mode-avlptree*)))
    (multiple-value-bind (node check) (insert-avlpnode compare root key value)
      (when check
        (setf (avlptree-root inst) node)
        (incf (avlptree-size inst) 1)
        t))))

(defun intern-avlptree (inst key value)
  (declare (type avlptree inst))
  (insert-avlptree inst key value t))


;;
;;  search
;;
(defun search-node-avlptree (inst key)
  (declare (type avlptree inst))
  (prog ((node (avlptree-root inst))
         (compare (avlptree-compare inst))
         key1 diff)
    loop
    (unless node
      (return nil))
    (setq key1 (avlpnode-key node))
    (setq diff (funcall compare key key1))
    (cond ((< diff 0) (setq node (avlpnode-left node)))
          ((< 0 diff) (setq node (avlpnode-right node)))
          (t (return node)))
    (go loop)))

(defun search-avlptree (inst key)
  (declare (type avlptree inst))
  (let ((node (search-node-avlptree inst key)))
    (if node
      (values (avlpnode-value node) t)
      (values nil nil))))

(defun replace-avlptree (inst key value &optional (replace nil opt))
  (declare (type avlptree inst))
  (let ((node (search-node-avlptree inst key))
        (*replace-key-avlptree* (if opt replace *replace-key-avlptree*)))
    (when node
      (replace-avlpnode node key value)
      t)))


;;
;;  delete
;;
(defun free-avlpnode (delete)
  (setf (avlpnode-parent delete) :error)
  (setf (avlpnode-left delete) :error)
  (setf (avlpnode-right delete) :error)
  (setf (avlpnode-key delete) :error)
  (setf (avlpnode-value delete) :error)
  (setf (avlpnode-balance delete) 0)
  nil)

(defun free-copy-avlpnode (replace delete)
  (setf (avlpnode-key replace) (avlpnode-key delete))
  (setf (avlpnode-value replace) (avlpnode-value delete))
  (free-avlpnode delete))

(defun delete-single-avlpnode (node delete diff)
  (let* ((left (avlpnode-left delete))
         (right (avlpnode-right delete))
         (next (or left right)))
    (if (< diff 0)
      (setf (avlpnode-left node) next)
      (setf (avlpnode-right node) next))
    (when next
      (setf (avlpnode-parent next) node))
    (free-avlpnode delete)))

(defun delete-lr-avlpnode (node left)
  (let ((left (rotate-left-avlpnode left)))
    (setf (avlpnode-left node) left)
    (setf (avlpnode-parent left) node))
  (update-balance-avlpnode
    (rotate-right-avlpnode node)))

(defun delete-ll-avlpnode (node)
  (let ((next (rotate-right-avlpnode node)))
    (if (zerop (avlpnode-balance next))
      (setf (avlpnode-balance next) -1
            (avlpnode-balance node) 1)
      (setf (avlpnode-balance next) 0
            (avlpnode-balance node) 0))
    next))

(defun delete-left-avlpnode (node)
  (let* ((left (avlpnode-left node))
         (balance (avlpnode-balance left)))
    (if (< balance 0)
      (delete-lr-avlpnode node left)
      (delete-ll-avlpnode node))))

(defun delete-rl-avlpnode (node right)
  (let ((right (rotate-right-avlpnode right)))
    (setf (avlpnode-right node) right)
    (setf (avlpnode-parent right) node))
  (update-balance-avlpnode
    (rotate-left-avlpnode node)))

(defun delete-rr-avlpnode (node)
  (let ((next (rotate-left-avlpnode node)))
    (if (zerop (avlpnode-balance next))
      (setf (avlpnode-balance next) 1
            (avlpnode-balance node) -1)
      (setf (avlpnode-balance next) 0
            (avlpnode-balance node) 0))
    next))

(defun delete-right-avlpnode (node)
  (let* ((right (avlpnode-right node))
         (balance (avlpnode-balance right)))
    (if (< 0 balance)
      (delete-rl-avlpnode node right)
      (delete-rr-avlpnode node))))

(defun delete-balance-avlpnode (node)
  (prog (next check)
    (setq check (avlpnode-balance node))
    (when (zerop check)  ;;  0
      (return (values nil 'loop)))
    (when (or (= check 1) (= check -1))  ;;  -1, 1
      (return (values nil t)))
    (when (< 1 check)  ;;  2
      (setq next (delete-left-avlpnode node)))
    (when (< check -1)  ;;  -2
      (setq next (delete-right-avlpnode node)))
    ;;  -2, 2
    (return (values next 'update))))

(defun delete-swap-avlpnode (replace node)
  (prog (next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  loop
    (setq next (avlpnode-left node))
    (multiple-value-setq (next check) (delete-swap-avlpnode replace next))
    (unless check
      (return (values node 'replace)))
    (when (eq check t)
      (return (values nil t)))
    (when (eq check 'replace)
      (let ((right (avlpnode-right next)))
        (setf (avlpnode-left node) right)
        (when right
          (setf (avlpnode-parent right) node)))
      (free-copy-avlpnode replace next))
    (when (eq check 'update)
      (setf (avlpnode-left node) next)
      (setf (avlpnode-parent next) node)
      (unless (zerop (avlpnode-balance next))
        (return (values nil t))))

    ;;  loop, update
    (decf (avlpnode-balance node) 1)  ;; left
    (return (delete-balance-avlpnode node))))

(defun delete-replace-avlpnode (node next)
  (let ((right (avlpnode-right next)))
    (setf (avlpnode-right node) right)
    (when right
      (setf (avlpnode-parent right) node)))
  (free-copy-avlpnode node next)
  (incf (avlpnode-balance node) 1)  ;; right
  (delete-balance-avlpnode node))

(defun delete-avlpnode (compare node key)
  (prog (key1 diff left next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  move
    (setq key1 (avlpnode-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (setq next (avlpnode-left node))
      (go loop-move))
    (when (< 0 diff)
      (setq next (avlpnode-right node))
      (go loop-move))

    ;;  delete
    (setq left (avlpnode-left node))
    (setq next (avlpnode-right node))
    (unless (and left next)
      (return (values node 'delete)))
    (multiple-value-setq (next check) (delete-swap-avlpnode node next))
    (when (eq check 'replace)
      (return (delete-replace-avlpnode node next)))
    (go loop-result)

    ;;  loop
    loop-move
    (multiple-value-setq (next check) (delete-avlpnode compare next key))
    (unless check
      (return (values nil nil)))

    loop-result
    (when (eq check t)
      (return (values nil t)))
    (when (eq check 'delete)
      (delete-single-avlpnode node next diff))
    (when (eq check 'update)
      (if (< diff 0)
        (setf (avlpnode-left node) next
              (avlpnode-parent next) node)
        (setf (avlpnode-right node) next
              (avlpnode-parent next) node))
      (unless (zerop (avlpnode-balance next))
        (return (values nil t))))

    ;;  loop, delete, update
    (if (< diff 0)
      (decf (avlpnode-balance node) 1)
      (incf (avlpnode-balance node) 1))
    (return (delete-balance-avlpnode node))))

(defun delete-setf-avlptree (inst node)
  (setf (avlptree-root inst) node)
  (setf (avlpnode-parent node) nil))

(defun delete-delete-avlptree (inst delete)
  (let* ((left (avlpnode-left delete))
         (right (avlpnode-right delete))
         (next (or left right)))
    (setf (avlptree-root inst) next)
    (when next
      (setf (avlpnode-parent next) nil))
    (free-avlpnode delete)))

(defun delete-avlptree (inst key)
  (declare (type avlptree inst))
  (let ((compare (avlptree-compare inst))
        (root (avlptree-root inst)))
    (multiple-value-bind (node check) (delete-avlpnode compare root key)
      (when check
        (ecase check
          ((t loop) nil)
          (delete (delete-delete-avlptree inst node))
          (update (delete-setf-avlptree inst node)))
        (decf (avlptree-size inst) 1)
        t))))


;;
;;  init
;;
(defstruct init-avlptree-struct
  call depth limit lower direct)

(defun init-loop-avlptree (str now)
  (prog* ((call (init-avlptree-struct-call str))
          (depth (init-avlptree-struct-depth str))
          (limit (init-avlptree-struct-limit str))
          (lower (init-avlptree-struct-lower str))
          (direct (init-avlptree-struct-direct str))
          (depth1 (1- depth))
          node left right x y key value)
    (unless (< now depth)
      (return (values nil now)))
    (when (= now depth1)
      (unless (< lower limit)
        (return (values nil now)))
      (incf (init-avlptree-struct-lower str) 1))
    (incf now 1)
    (multiple-value-setq (left x) (init-loop-avlptree str now))
    (multiple-value-setq (key value) (funcall call))
    (multiple-value-setq (right y) (init-loop-avlptree str now))
    (when direct
      (rotatef left right)
      (rotatef x y))
    (setq node (make-avlpnode
                 :key key :value value :balance (- x y)
                 :left left :right right))
    (when left
      (setf (avlpnode-parent left) node))
    (when right
      (setf (avlpnode-parent right) node))
    (return (values node (max x y)))))

(defun init-call-avlptree (call size direct)
  (let* ((depth (integer-length size))
         (limit (- size (1- (ash 1 (1- depth))))))
    (init-loop-avlptree
      (make-init-avlptree-struct
        :call call :depth depth :limit limit :lower 0 :direct direct)
      0)))

(defun init-avlptree (call size &optional inst direct)
  (unless inst
    (setq inst (make-avlptree)))
  (setf (avlptree-root inst) (init-call-avlptree call size direct))
  (setf (avlptree-size inst) size)
  inst)

(defun init-property1-avlptree (seq)
  (lambda (&aux key value)
    (unless seq
      (error "sequence error, key."))
    (setq key (pop seq))
    (unless seq
      (error "sequence error, value, ~S." key))
    (setq value (pop seq))
    (values key value)))

(defun init-property2-avlptree (seq size)
  (let ((index 0))
    (lambda (&aux key value)
      (unless (< index size)
        (error "sequence error, key."))
      (setq key (aref seq index))
      (incf index 1)
      (unless (< index size)
        (error "sequence error, value, ~S." key))
      (setq value (aref seq index))
      (incf index 1)
      (values key value))))

(defun init-property-avlptree (inst seq)
  (let ((size (length seq)))
    (multiple-value-bind (div rem) (truncate size 2)
      (unless (zerop rem)
        (error "size error, ~A." size))
      (init-avlptree
        (if (listp seq)
          (init-property1-avlptree seq)
          (init-property2-avlptree seq div))
        div inst))))

(defun init-associate1-avlptree (seq)
  (lambda ()
    (unless seq
      (error "sequence error, cons."))
    (destructuring-bind (key . value) (pop seq)
      (values key value))))

(defun init-associate2-avlptree (seq size)
  (let ((index 0))
    (lambda ()
      (unless (< index size)
        (error "sequence error, cons."))
      (destructuring-bind (key . value) (aref seq index)
        (incf index 1)
        (values key value)))))

(defun init-associate-avlptree (inst seq)
  (let ((size (length seq)))
    (init-avlptree
      (if (listp seq)
        (init-associate1-avlptree seq)
        (init-associate2-avlptree seq size))
      size inst)))

