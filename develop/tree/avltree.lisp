;;
;;  AVL tree
;;
(defpackage #:avltree
  (:use common-lisp)
  (:export
    #:avltree
    #:avltree-p
    #:make-avltree
    #:clear-avltree
    #:size-avltree
    #:empty-avltree
    #:min-avltree
    #:max-avltree
    #:map-avltree
    #:map-avlnode
    #:keys-avltree
    #:values-avltree
    #:hash-table-avltree

    #:*replace-mode-avltree*
    #:*replace-key-avltree*
    #:insert-avltree
    #:intern-avltree
    #:search-avltree
    #:replace-avltree
    #:delete-avltree
    #:init-avltree
    #:init-property-avltree
    #:init-associate-avltree))

(in-package #:avltree)

;;
;;  avltree
;;
(defstruct (avltree
             (:constructor avltree-heap)
             (:copier nil))
  root
  (compare #'- :type function)
  (size 0 :type unsigned-byte))

(defmethod print-object ((inst avltree) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (avltree-size inst))))

(defun make-avltree (&key (compare #'-))
  (avltree-heap :compare compare))

(defun clear-avltree (inst)
  (declare (type avltree inst))
  (setf (avltree-root inst) nil)
  (setf (avltree-size inst) 0))

(defun size-avltree (inst)
  (declare (type avltree inst))
  (avltree-size inst))


;;
;;  avlnode
;;
(defstruct (avlnode (:copier nil))
  left right key value
  (balance 0 :type (integer -2 2)))  ;; left - right

(defmethod print-object ((inst avlnode) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (avlnode-balance inst))))


;;
;;  operator
;;
(defun empty-avltree (inst)
  (declare (type avltree inst))
  (if (avltree-root inst) nil t))

(defun min-avlnode (node)
  (let ((left (avlnode-left node)))
    (if left
      (min-avlnode left)
      (values (avlnode-key node) t))))

(defun min-avltree (inst)
  (declare (type avltree inst))
  (let ((root (avltree-root inst)))
    (multiple-value-bind (x y)
      (when root
        (min-avlnode root))
      (if y
        (values x t)
        (values nil nil)))))

(defun max-avlnode (node)
  (let ((right (avlnode-right node)))
    (if right
      (max-avlnode right)
      (values (avlnode-key node) t))))

(defun max-avltree (inst)
  (declare (type avltree inst))
  (let ((root (avltree-root inst)))
    (multiple-value-bind (x y)
      (when root
        (max-avlnode root))
      (if y
        (values x t)
        (values nil nil)))))

(defun map-avlnode (call inst)
  (declare (type avltree inst))
  (labels ((rec (x) (when x
                      (rec (avlnode-right x))
                      (funcall call x)
                      (rec (avlnode-left x)))))
    (rec (avltree-root inst)))
  (values))

(defun map-avltree (call inst)
  (declare (type avltree inst))
  (map-avlnode
    (lambda (x)
      (funcall call (avlnode-key x) (avlnode-value x)))
    inst))

(defun keys-avltree (inst)
  (declare (type avltree inst))
  (let (list)
    (map-avlnode
      (lambda (x)
        (push (avlnode-key x) list))
      inst)
    list))

(defun values-avltree (inst)
  (declare (type avltree inst))
  (let (list)
    (map-avlnode
      (lambda (x)
        (push (avlnode-value x) list))
      inst)
    list))

(defun hash-table-avltree (inst &optional (test 'eql))
  (declare (type avltree inst))
  (let ((table (make-hash-table :test test)))
    (map-avltree
      (lambda (key value)
        (setf (gethash key table) value))
      inst)
    table))


;;
;;  rotate
;;
(defun rotate-left-avlnode (node)
  (let ((x (avlnode-right node)))
    (setf (avlnode-right node) (avlnode-left x))
    (setf (avlnode-left x) node)
    x))

(defun rotate-right-avlnode (node)
  (let ((x (avlnode-left node)))
    (setf (avlnode-left node) (avlnode-right x))
    (setf (avlnode-right x) node)
    x))


;;
;;  insert
;;
(defvar *replace-mode-avltree* nil)
(defvar *replace-key-avltree* t)

(defun replace-avlnode (node key value)
  (when *replace-key-avltree*
    (setf (avlnode-key node) key))
  (setf (avlnode-value node) value))

(defun update-balance-avlnode (node)
  (let ((left (avlnode-left node))
        (right (avlnode-right node))
        (balance (avlnode-balance node)))
    (cond ((= balance 1)
           (setf (avlnode-balance left) 0)
           (setf (avlnode-balance right) -1))
          ((= balance -1)
           (setf (avlnode-balance left) 1)
           (setf (avlnode-balance right) 0))
          (t (setf (avlnode-balance left) 0)
             (setf (avlnode-balance right) 0)))
    (setf (avlnode-balance node) 0)
    node))

(defun insert-lr-avlnode (node left)
  (setf (avlnode-left node) (rotate-left-avlnode left))
  (update-balance-avlnode
    (rotate-right-avlnode node)))

(defun insert-ll-avlnode (node)
  (let ((next (rotate-right-avlnode node)))
    (setf (avlnode-balance next) 0)
    (setf (avlnode-balance node) 0)
    next))

(defun insert-left-avlnode (node)
  (let* ((left (avlnode-left node))
         (balance (avlnode-balance left)))
    (if (< balance 0)
      (insert-lr-avlnode node left)
      (insert-ll-avlnode node))))

(defun insert-rl-avlnode (node right)
  (setf (avlnode-right node) (rotate-right-avlnode right))
  (update-balance-avlnode
    (rotate-left-avlnode node)))

(defun insert-rr-avlnode (node)
  (let ((next (rotate-left-avlnode node)))
    (setf (avlnode-balance next) 0)
    (setf (avlnode-balance node) 0)
    next))

(defun insert-right-avlnode (node)
  (let* ((right (avlnode-right node))
         (balance (avlnode-balance right)))
    (if (< 0 balance)
      (insert-rl-avlnode node right)
      (insert-rr-avlnode node))))

(defun insert-avlnode (compare node key value)
  (declare (type (or avlnode null) node))
  (prog (key1 diff next check)
    ;;  nil
    (unless node
      (setq next (make-avlnode :key key :value value))
      (return (values next 'make)))

    ;;  move
    (setq key1 (avlnode-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (go move-left))
    (when (< 0 diff)
      (go move-right))

    ;;  equal
    (when *replace-mode-avltree*
      (replace-avlnode node key value))
    (return (values nil nil))

    ;;  left
    move-left
    (setq next (avlnode-left node))
    (multiple-value-setq (next check) (insert-avlnode compare next key value))
    (unless check
      (return (values nil nil)))
    (when (eq check t)
      (return (values node t)))
    (when (eq check 'finish)
      (setf (avlnode-left node) next)
      (return (values node t)))
    (when (eq check 'make)
      (setf (avlnode-left node) next))
    (incf (avlnode-balance node) 1)
    (go balance)

    ;;  right
    move-right
    (setq next (avlnode-right node))
    (multiple-value-setq (next check) (insert-avlnode compare next key value))
    (unless check
      (return (values nil nil)))
    (when (eq check t)
      (return (values node t)))
    (when (eq check 'finish)
      (setf (avlnode-right node) next)
      (return (values node t)))
    (when (eq check 'make)
      (setf (avlnode-right node) next))
    (decf (avlnode-balance node) 1)
    (go balance)

    ;;  balance
    balance
    (setq check (avlnode-balance node))
    (when (zerop check)
      (return (values node t)))  ;;  skip
    (when (< 1 check)  ;;  2
      (setq next (insert-left-avlnode node))
      (return (values next 'finish)))
    (when (< check -1)  ;;  -2
      (setq next (insert-right-avlnode node))
      (return (values next 'finish)))
    ;;  1 or -1
    (return (values node 'loop))))

(defun insert-avltree (inst key value &optional (replace nil opt))
  (declare (type avltree inst))
  (let ((compare (avltree-compare inst))
        (root (avltree-root inst))
        (*replace-mode-avltree* (if opt replace *replace-mode-avltree*)))
    (multiple-value-bind (node check) (insert-avlnode compare root key value)
      (when check
        (setf (avltree-root inst) node)
        (incf (avltree-size inst) 1)
        t))))

(defun intern-avltree (inst key value)
  (declare (type avltree inst))
  (insert-avltree inst key value t))


;;
;;  search
;;
(defun search-node-avltree (inst key)
  (declare (type avltree inst))
  (prog ((node (avltree-root inst))
         (compare (avltree-compare inst))
         key1 diff)
    loop
    (unless node
      (return nil))
    (setq key1 (avlnode-key node))
    (setq diff (funcall compare key key1))
    (cond ((< diff 0) (setq node (avlnode-left node)))
          ((< 0 diff) (setq node (avlnode-right node)))
          (t (return node)))
    (go loop)))

(defun search-avltree (inst key)
  (declare (type avltree inst))
  (let ((node (search-node-avltree inst key)))
    (if node
      (values (avlnode-value node) t)
      (values nil nil))))

(defun replace-avltree (inst key value &optional (replace nil opt))
  (declare (type avltree inst))
  (let ((node (search-node-avltree inst key))
        (*replace-key-avltree* (if opt replace *replace-key-avltree*)))
    (when node
      (replace-avlnode node key value)
      t)))


;;
;;  delete
;;
(defun free-avlnode (delete)
  (setf (avlnode-left delete) :error)
  (setf (avlnode-right delete) :error)
  (setf (avlnode-key delete) :error)
  (setf (avlnode-value delete) :error)
  (setf (avlnode-balance delete) 0)
  nil)

(defun free-copy-avlnode (replace delete)
  (setf (avlnode-key replace) (avlnode-key delete))
  (setf (avlnode-value replace) (avlnode-value delete))
  (free-avlnode delete))

(defun delete-single-avlnode (node delete diff)
  (let* ((left (avlnode-left delete))
         (right (avlnode-right delete))
         (next (or left right)))
    (if (< diff 0)
      (setf (avlnode-left node) next)
      (setf (avlnode-right node) next))
    (free-avlnode delete)))

(defun delete-lr-avlnode (node left)
  (setf (avlnode-left node) (rotate-left-avlnode left))
  (update-balance-avlnode
    (rotate-right-avlnode node)))

(defun delete-ll-avlnode (node)
  (let ((next (rotate-right-avlnode node)))
    (if (zerop (avlnode-balance next))
      (setf (avlnode-balance next) -1
            (avlnode-balance node) 1)
      (setf (avlnode-balance next) 0
            (avlnode-balance node) 0))
    next))

(defun delete-left-avlnode (node)
  (let* ((left (avlnode-left node))
         (balance (avlnode-balance left)))
    (if (< balance 0)
      (delete-lr-avlnode node left)
      (delete-ll-avlnode node))))

(defun delete-rl-avlnode (node right)
  (setf (avlnode-right node) (rotate-right-avlnode right))
  (update-balance-avlnode
    (rotate-left-avlnode node)))

(defun delete-rr-avlnode (node)
  (let ((next (rotate-left-avlnode node)))
    (if (zerop (avlnode-balance next))
      (setf (avlnode-balance next) 1
            (avlnode-balance node) -1)
      (setf (avlnode-balance next) 0
            (avlnode-balance node) 0))
    next))

(defun delete-right-avlnode (node)
  (let* ((right (avlnode-right node))
         (balance (avlnode-balance right)))
    (if (< 0 balance)
      (delete-rl-avlnode node right)
      (delete-rr-avlnode node))))

(defun delete-balance-avlnode (node)
  (prog (next check)
    (setq check (avlnode-balance node))
    (when (zerop check)  ;;  0
      (return (values nil 'loop)))
    (when (or (= check 1) (= check -1))  ;;  -1, 1
      (return (values nil t)))
    (when (< 1 check)  ;;  2
      (setq next (delete-left-avlnode node)))
    (when (< check -1)  ;;  -2
      (setq next (delete-right-avlnode node)))
    ;;  -2, 2
    (return (values next 'update))))

(defun delete-swap-avlnode (replace node)
  (prog (next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  loop
    (setq next (avlnode-left node))
    (multiple-value-setq (next check) (delete-swap-avlnode replace next))
    (unless check
      (return (values node 'replace)))
    (when (eq check t)
      (return (values nil t)))
    (when (eq check 'replace)
      (setf (avlnode-left node) (avlnode-right next))
      (free-copy-avlnode replace next))
    (when (eq check 'update)
      (setf (avlnode-left node) next)
      (unless (zerop (avlnode-balance next))
        (return (values nil t))))

    ;;  loop, update
    (decf (avlnode-balance node) 1)  ;; left
    (return (delete-balance-avlnode node))))

(defun delete-replace-avlnode (node next)
  (setf (avlnode-right node) (avlnode-right next))
  (free-copy-avlnode node next)
  (incf (avlnode-balance node) 1)  ;; right
  (delete-balance-avlnode node))

(defun delete-avlnode (compare node key)
  (prog (key1 diff left next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  move
    (setq key1 (avlnode-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (setq next (avlnode-left node))
      (go loop-move))
    (when (< 0 diff)
      (setq next (avlnode-right node))
      (go loop-move))

    ;;  delete
    (setq left (avlnode-left node))
    (setq next (avlnode-right node))
    (unless (and left next)
      (return (values node 'delete)))
    (multiple-value-setq (next check) (delete-swap-avlnode node next))
    (when (eq check 'replace)
      (return (delete-replace-avlnode node next)))
    (go loop-result)

    ;;  loop
    loop-move
    (multiple-value-setq (next check) (delete-avlnode compare next key))
    (unless check
      (return (values nil nil)))

    loop-result
    (when (eq check t)
      (return (values nil t)))
    (when (eq check 'delete)
      (delete-single-avlnode node next diff))
    (when (eq check 'update)
      (if (< diff 0)
        (setf (avlnode-left node) next)
        (setf (avlnode-right node) next))
      (unless (zerop (avlnode-balance next))
        (return (values nil t))))

    ;;  loop, delete, update
    (if (< diff 0)
      (decf (avlnode-balance node) 1)
      (incf (avlnode-balance node) 1))
    (return (delete-balance-avlnode node))))

(defun delete-setf-avltree (inst node)
  (setf (avltree-root inst) node))

(defun delete-delete-avltree (inst delete)
  (let ((left (avlnode-left delete))
        (right (avlnode-right delete)))
    (setf (avltree-root inst) (or left right))
    (free-avlnode delete)))

(defun delete-avltree (inst key)
  (declare (type avltree inst))
  (let ((compare (avltree-compare inst))
        (root (avltree-root inst)))
    (multiple-value-bind (node check) (delete-avlnode compare root key)
      (when check
        (ecase check
          ((t loop) nil)
          (delete (delete-delete-avltree inst node))
          (update (delete-setf-avltree inst node)))
        (decf (avltree-size inst) 1)
        t))))


;;
;;  init
;;
(defstruct init-avltree-struct
  call depth limit lower direct)

(defun init-loop-avltree (str now)
  (prog* ((call (init-avltree-struct-call str))
          (depth (init-avltree-struct-depth str))
          (limit (init-avltree-struct-limit str))
          (lower (init-avltree-struct-lower str))
          (direct (init-avltree-struct-direct str))
          (depth1 (1- depth))
          node left right x y key value)
    (unless (< now depth)
      (return (values nil now)))
    (when (= now depth1)
      (unless (< lower limit)
        (return (values nil now)))
      (incf (init-avltree-struct-lower str) 1))
    (incf now 1)
    (multiple-value-setq (left x) (init-loop-avltree str now))
    (multiple-value-setq (key value) (funcall call))
    (multiple-value-setq (right y) (init-loop-avltree str now))
    (when direct
      (rotatef left right)
      (rotatef x y))
    (setq node (make-avlnode
                 :key key :value value :balance (- x y)
                 :left left :right right))
    (return (values node (max x y)))))

(defun init-call-avltree (call size direct)
  (let* ((depth (integer-length size))
         (limit (- size (1- (ash 1 (1- depth))))))
    (init-loop-avltree
      (make-init-avltree-struct
        :call call :depth depth :limit limit :lower 0 :direct direct)
      0)))

(defun init-avltree (call size &optional inst direct)
  (unless inst
    (setq inst (make-avltree)))
  (setf (avltree-root inst) (init-call-avltree call size direct))
  (setf (avltree-size inst) size)
  inst)

(defun init-property1-avltree (seq)
  (lambda (&aux key value)
    (unless seq
      (error "sequence error, key."))
    (setq key (pop seq))
    (unless seq
      (error "sequence error, value, ~S." key))
    (setq value (pop seq))
    (values key value)))

(defun init-property2-avltree (seq size)
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

(defun init-property-avltree (inst seq)
  (let ((size (length seq)))
    (multiple-value-bind (div rem) (truncate size 2)
      (unless (zerop rem)
        (error "size error, ~A." size))
      (init-avltree
        (if (listp seq)
          (init-property1-avltree seq)
          (init-property2-avltree seq div))
        div inst))))

(defun init-associate1-avltree (seq)
  (lambda ()
    (unless seq
      (error "sequence error, cons."))
    (destructuring-bind (key . value) (pop seq)
      (values key value))))

(defun init-associate2-avltree (seq size)
  (let ((index 0))
    (lambda ()
      (unless (< index size)
        (error "sequence error, cons."))
      (destructuring-bind (key . value) (aref seq index)
        (incf index 1)
        (values key value)))))

(defun init-associate-avltree (inst seq)
  (let ((size (length seq)))
    (init-avltree
      (if (listp seq)
        (init-associate1-avltree seq)
        (init-associate2-avltree seq size))
      size inst)))

