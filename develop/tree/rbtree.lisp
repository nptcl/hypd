;;
;;  Red-Black tree
;;
(defpackage #:rbtree
  (:use common-lisp)
  (:export
    #:rbtree
    #:rbtree-p
    #:make-rbtree
    #:clear-rbtree
    #:size-rbtree
    #:empty-rbtree
    #:min-rbtree
    #:max-rbtree
    #:map-rbtree
    #:map-rbnode
    #:keys-rbtree
    #:values-rbtree
    #:hash-table-rbtree

    #:*replace-mode-rbtree*
    #:*replace-key-rbtree*
    #:insert-rbtree
    #:intern-rbtree
    #:search-rbtree
    #:replace-rbtree
    #:delete-rbtree
    #:init-rbtree
    #:init-property-rbtree
    #:init-associate-rbtree))

(in-package #:rbtree)

;;
;;  rbtree
;;
(defstruct (rbtree
             (:constructor rbtree-heap)
             (:copier nil))
  root
  (compare #'- :type function)
  (size 0 :type unsigned-byte))

(defmethod print-object ((inst rbtree) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (rbtree-size inst))))

(defun make-rbtree (&key (compare #'-))
  (rbtree-heap :compare compare))

(defun clear-rbtree (inst)
  (declare (type rbtree inst))
  (setf (rbtree-root inst) nil)
  (setf (rbtree-size inst) 0))

(defun size-rbtree (inst)
  (declare (type rbtree inst))
  (rbtree-size inst))


;;
;;  rbnode
;;
(defstruct (rbnode (:copier nil))
  left right key value
  (color 'red :type (member red black :error)))

(defmethod print-object ((inst rbnode) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (rbnode-color inst))))

(defun rbnode-error-check (node)
  (declare (type (or rbnode null) node))
  (when (and node (eq (rbnode-color node) :error))
    (error "rbnode-color error, ~S." node))
  (values))

(defun rbnode-red-p (node)
  (declare (type rbnode node))
  (rbnode-error-check node)
  (eq (rbnode-color node) 'red))

(defun rbnode-black-p (node)
  (declare (type rbnode node))
  (rbnode-error-check node)
  (eq (rbnode-color node) 'black))

(defun rbnode-red-p! (node)
  (rbnode-error-check node)
  (and node
       (eq (rbnode-color node) 'red)))

(defun rbnode-black-p! (node)
  (rbnode-error-check node)
  (or (null node)
      (eq (rbnode-color node) 'black)))

(defun setf-rbnode-red (node)
  (declare (type rbnode node))
  (rbnode-error-check node)
  (setf (rbnode-color node) 'red))

(defun setf-rbnode-black (node)
  (declare (type rbnode node))
  (rbnode-error-check node)
  (setf (rbnode-color node) 'black))


;;
;;  operator
;;
(defun empty-rbtree (inst)
  (declare (type rbtree inst))
  (if (rbtree-root inst) nil t))

(defun min-rbnode (node)
  (let ((left (rbnode-left node)))
    (if left
      (min-rbnode left)
      (values (rbnode-key node) t))))

(defun min-rbtree (inst)
  (declare (type rbtree inst))
  (let ((root (rbtree-root inst)))
    (multiple-value-bind (x y)
      (when root
        (min-rbnode root))
      (if y
        (values x t)
        (values nil nil)))))

(defun max-rbnode (node)
  (let ((right (rbnode-right node)))
    (if right
      (max-rbnode right)
      (values (rbnode-key node) t))))

(defun max-rbtree (inst)
  (declare (type rbtree inst))
  (let ((root (rbtree-root inst)))
    (multiple-value-bind (x y)
      (when root
        (max-rbnode root))
      (if y
        (values x t)
        (values nil nil)))))

(defun map-rbnode (call inst)
  (declare (type rbtree inst))
  (labels ((rec (x) (when x
                      (rec (rbnode-right x))
                      (funcall call x)
                      (rec (rbnode-left x)))))
    (rec (rbtree-root inst)))
  (values))

(defun map-rbtree (call inst)
  (declare (type rbtree inst))
  (map-rbnode
    (lambda (x)
      (funcall call (rbnode-key x) (rbnode-value x)))
    inst))

(defun keys-rbtree (inst)
  (declare (type rbtree inst))
  (let (list)
    (map-rbnode
      (lambda (x)
        (push (rbnode-key x) list))
      inst)
    list))

(defun values-rbtree (inst)
  (declare (type rbtree inst))
  (let (list)
    (map-rbnode
      (lambda (x)
        (push (rbnode-value x) list))
      inst)
    list))

(defun hash-table-rbtree (inst &optional (test 'eql))
  (declare (type rbtree inst))
  (let ((table (make-hash-table :test test)))
    (map-rbtree
      (lambda (key value)
        (setf (gethash key table) value))
      inst)
    table))


;;
;;  rotate
;;
(defun rotate-left-rbnode (node)
  (let ((x (rbnode-right node)))
    (setf (rbnode-right node) (rbnode-left x))
    (setf (rbnode-left x) node)
    (setf (rbnode-color x) (rbnode-color node))
    (setf-rbnode-red node)
    x))

(defun rotate-right-rbnode (node)
  (let ((x (rbnode-left node)))
    (setf (rbnode-left node) (rbnode-right x))
    (setf (rbnode-right x) node)
    (setf (rbnode-color x) (rbnode-color node))
    (setf-rbnode-red node)
    x))


;;
;;  insert
;;
(defvar *replace-mode-rbtree* nil)
(defvar *replace-key-rbtree* t)

(defun replace-rbnode (node key value)
  (when *replace-key-rbtree*
    (setf (rbnode-key node) key))
  (setf (rbnode-value node) value))

(defun insert-left-rbnode (node check)
  (prog (left right next)
    (unless (eq check 'update)
      (return (values node check)))
    (when (rbnode-red-p node)
      (return (values node 'update)))

    ;;  black
    (setq left (rbnode-left node))
    (setq next (rbnode-right left))
    (unless next
      (go next-check))
    (when (rbnode-black-p next)
      (go next-check))
    (setq left (rotate-left-rbnode left))
    (setf (rbnode-left node) left)

    next-check
    (setq next (rbnode-left left))
    (unless next
      (go return-balanced))
    (when (rbnode-black-p next)
      (go return-balanced))

    ;;  red-red
    (setq right (rbnode-right node))
    (unless right
      (go rotate-right))
    (when (rbnode-black-p right)
      (go rotate-right))
    (setf-rbnode-red node)
    (setf-rbnode-black left)
    (setf-rbnode-black right)
    (return (values node 'update))

    rotate-right
    (setq node (rotate-right-rbnode node))
    return-balanced
    (return (values node 'balanced))))

(defun insert-right-rbnode (node check)
  (prog (left right next)
    (unless (eq check 'update)
      (return (values node check)))
    (when (rbnode-red-p node)
      (return (values node 'update)))

    ;;  black
    (setq right (rbnode-right node))
    (setq next (rbnode-left right))
    (unless next
      (go next-check))
    (when (rbnode-black-p next)
      (go next-check))
    (setq right (rotate-right-rbnode right))
    (setf (rbnode-right node) right)

    next-check
    (setq next (rbnode-right right))
    (unless next
      (go return-balanced))
    (when (rbnode-black-p next)
      (go return-balanced))

    ;;  red-red
    (setq left (rbnode-left node))
    (unless left
      (go rotate-left))
    (when (rbnode-black-p left)
      (go rotate-left))
    (setf-rbnode-red node)
    (setf-rbnode-black left)
    (setf-rbnode-black right)
    (return (values node 'update))

    rotate-left
    (setq node (rotate-left-rbnode node))
    return-balanced
    (return (values node 'balanced))))

(defun insert-rbnode (compare node key value)
  (prog (key1 diff next check)
    ;;  nil
    (unless node
      (setq next (make-rbnode :key key :value value))
      (return (values next 'update)))

    ;;  move
    (setq key1 (rbnode-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (go insert-left))
    (when (< 0 diff)
      (go insert-right))

    ;;  equal
    (when *replace-mode-rbtree*
      (replace-rbnode node key value))
    (return (values nil nil))

    ;;  left
    insert-left
    (setq next (rbnode-left node))
    (multiple-value-setq (next check)
      (insert-rbnode compare next key value))
    (unless check
      (return (values nil nil)))
    (setf (rbnode-left node) next)
    (return (insert-left-rbnode node check))

    ;;  right
    insert-right
    (setq next (rbnode-right node))
    (multiple-value-setq (next check)
      (insert-rbnode compare next key value))
    (unless check
      (return (values nil nil)))
    (setf (rbnode-right node) next)
    (return (insert-right-rbnode node check))))

(defun insert-rbtree (inst key value &optional (replace nil opt))
  (declare (type rbtree inst))
  (let ((compare (rbtree-compare inst))
        (root (rbtree-root inst))
        (*replace-mode-rbtree* (if opt replace *replace-mode-rbtree*)))
    (multiple-value-bind (node check) (insert-rbnode compare root key value)
      (when check
        (setf-rbnode-black node)
        (setf (rbtree-root inst) node)
        (incf (rbtree-size inst) 1)
        t))))

(defun intern-rbtree (inst key value)
  (declare (type rbtree inst))
  (insert-rbtree inst key value t))


;;
;;  search
;;
(defun search-node-rbtree (inst key)
  (declare (type rbtree inst))
  (prog ((node (rbtree-root inst))
         (compare (rbtree-compare inst))
         key1 diff)
    loop
    (unless node
      (return nil))
    (setq key1 (rbnode-key node))
    (setq diff (funcall compare key key1))
    (cond ((< diff 0) (setq node (rbnode-left node)))
          ((< 0 diff) (setq node (rbnode-right node)))
          (t (return node)))
    (go loop)))

(defun search-rbtree (inst key)
  (declare (type rbtree inst))
  (let ((node (search-node-rbtree inst key)))
    (if node
      (values (rbnode-value node) t)
      (values nil nil))))

(defun replace-rbtree (inst key value &optional (replace nil opt))
  (declare (type rbtree inst))
  (let ((node (search-node-rbtree inst key))
        (*replace-key-rbtree* (if opt replace *replace-key-rbtree*)))
    (when node
      (replace-rbnode node key value)
      t)))


;;
;;  delete
;;
(defun free-rbnode (delete)
  (setf (rbnode-left delete) :error)
  (setf (rbnode-right delete) :error)
  (setf (rbnode-key delete) :error)
  (setf (rbnode-value delete) :error)
  (setf (rbnode-color delete) :error)
  nil)

(defun free-copy-rbnode (replace delete)
  (setf (rbnode-key replace) (rbnode-key delete))
  (setf (rbnode-value replace) (rbnode-value delete))
  (free-rbnode delete))

(defun delete-left-rbnode (node check)
  (prog (left right right-left right-right)
    (when (eq check 'balanced)
      (return (values node 'balanced)))
    (setq right (rbnode-right node))
    (setq right-left (rbnode-left right))
    (when (rbnode-red-p! right-left)
      (go red-label))
    (setq right-right (rbnode-right right))
    (when (rbnode-red-p! right-right)
      (go red-label))

    ;;  black - black
    (if (rbnode-black-p right)
      (progn
        (setf-rbnode-red right)
        (when (rbnode-black-p node)
          (return (values node 'update)))
        (setf-rbnode-black node)
        (return (values node 'balanced)))
      (progn
        (setq node (rotate-left-rbnode node))
        (setq left (rbnode-left node))
        (setf (rbnode-left node) (delete-left-rbnode left 'update))
        (return (values node 'balanced))))

    ;;  red
    red-label
    (when (rbnode-red-p! right-left)
      (setf (rbnode-right node) (rotate-right-rbnode right)))
    (setq node (rotate-left-rbnode node))
    (setf-rbnode-black (rbnode-left node))
    (setf-rbnode-black (rbnode-right node))
    (return (values node 'balanced))))

(defun delete-right-rbnode (node check)
  (prog (left right left-left left-right)
    (when (eq check 'balanced)
      (return (values node 'balanced)))
    (setq left (rbnode-left node))
    (setq left-right (rbnode-right left))
    (when (rbnode-red-p! left-right)
      (go red-label))
    (setq left-left (rbnode-left left))
    (when (rbnode-red-p! left-left)
      (go red-label))


    ;;  black - black
    (if (rbnode-black-p left)
      (progn
        (setf-rbnode-red left)
        (when (rbnode-black-p node)
          (return (values node 'update)))
        (setf-rbnode-black node)
        (return (values node 'balanced)))
      (progn
        (setq node (rotate-right-rbnode node))
        (setq right (rbnode-right node))
        (setf (rbnode-right node) (delete-right-rbnode right 'update))
        (return (values node 'balanced))))

    ;;  red
    red-label
    (when (rbnode-red-p! left-right)
      (setf (rbnode-left node) (rotate-left-rbnode left)))
    (setq node (rotate-right-rbnode node))
    (setf-rbnode-black (rbnode-left node))
    (setf-rbnode-black (rbnode-right node))
    (return (values node 'balanced))))

(defun delete-swap-rbnode (replace node)
  (prog (next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  left
    (setq next (rbnode-left node))
    (multiple-value-setq (next check) (delete-swap-rbnode replace next))
    (when check
      (setf (rbnode-left node) next)
      (return (delete-left-rbnode node check)))

    ;;  right
    (setq next (rbnode-right node))
    (when next
      (setf-rbnode-black next)
      (free-copy-rbnode replace node)
      (return (values next 'balanced)))

    ;;  single
    (if (rbnode-red-p node)
      (setq check 'balanced)
      (setq check 'update))
    (free-copy-rbnode replace node)
    (return (values nil check))))

(defun delete-rbnode (compare node key)
  (prog (key1 diff left right next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  move
    (setq key1 (rbnode-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (go move-left))
    (when (< 0 diff)
      (go move-right))
    (go move-delete)

    ;;  left
    move-left
    (setq next (rbnode-left node))
    (multiple-value-setq (next check) (delete-rbnode compare next key))
    (unless check
      (return (values nil nil)))
    (setf (rbnode-left node) next)
    (return (delete-left-rbnode node check))

    ;;  right
    move-right
    (setq next (rbnode-right node))
    (multiple-value-setq (next check) (delete-rbnode compare next key))
    (unless check
      (return (values nil nil)))
    (setf (rbnode-right node) next)
    (return (delete-right-rbnode node check))

    ;;  delete
    move-delete
    (setq left (rbnode-left node))
    (setq right (rbnode-right node))
    (when (and left right)
      (go delete-swap))
    (when left
      (go delete-left))
    (when right
      (go delete-right))
    (go delete-single)

    ;;  left, right
    delete-swap
    (multiple-value-setq (next check) (delete-swap-rbnode node right))
    (setf (rbnode-right node) next)
    (return (delete-right-rbnode node check))

    ;;  left only
    delete-left
    (setf-rbnode-black left)
    (free-rbnode node)
    (return (values left 'balanced))

    ;;  right only
    delete-right
    (setf-rbnode-black right)
    (free-rbnode node)
    (return (values right 'balanced))

    ;;  null, null
    delete-single
    (if (rbnode-red-p node)
      (setq check 'balanced)
      (setq check 'update))
    (free-rbnode node)
    (return (values nil check))))

(defun delete-rbtree (inst key)
  (declare (type rbtree inst))
  (let ((compare (rbtree-compare inst))
        (root (rbtree-root inst)))
    (multiple-value-bind (node check) (delete-rbnode compare root key)
      (when check
        (setf (rbtree-root inst) node)
        (decf (rbtree-size inst) 1)
        t))))


;;
;;  init
;;
(defstruct init-rbtree-struct
  call depth limit lower direct)

(defun init-loop-rbtree (str now)
  (prog* ((call (init-rbtree-struct-call str))
          (depth (init-rbtree-struct-depth str))
          (limit (init-rbtree-struct-limit str))
          (lower (init-rbtree-struct-lower str))
          (direct (init-rbtree-struct-direct str))
          (depth1 (1- depth))
          node left right x y key value color)
    (setq color 'black)
    (unless (< now depth)
      (return (values nil now)))
    (when (= now depth1)
      (setq color 'red)
      (unless (< lower limit)
        (return (values nil now)))
      (incf (init-rbtree-struct-lower str) 1))
    (incf now 1)
    (multiple-value-setq (left x) (init-loop-rbtree str now))
    (multiple-value-setq (key value) (funcall call))
    (multiple-value-setq (right y) (init-loop-rbtree str now))
    (when direct
      (rotatef left right)
      (rotatef x y))
    (setq node (make-rbnode
                 :key key :value value :color color
                 :left left :right right))
    (return (values node (max x y)))))

(defun init-call-rbtree (call size direct)
  (let* ((depth (integer-length size))
         (limit (- size (1- (ash 1 (1- depth))))))
    (init-loop-rbtree
      (make-init-rbtree-struct
        :call call :depth depth :limit limit :lower 0 :direct direct)
      0)))

(defun init-rbtree (call size &optional inst direct)
  (unless inst
    (setq inst (make-rbtree)))
  (let ((root (init-call-rbtree call size direct)))
    (when root
      (setf-rbnode-black root))
    (setf (rbtree-root inst) root)
    (setf (rbtree-size inst) size)
    inst))

(defun init-property1-rbtree (seq)
  (lambda (&aux key value)
    (unless seq
      (error "sequence error, key."))
    (setq key (pop seq))
    (unless seq
      (error "sequence error, value, ~S." key))
    (setq value (pop seq))
    (values key value)))

(defun init-property2-rbtree (seq size)
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

(defun init-property-rbtree (inst seq)
  (let ((size (length seq)))
    (multiple-value-bind (div rem) (truncate size 2)
      (unless (zerop rem)
        (error "size error, ~A." size))
      (init-rbtree
        (if (listp seq)
          (init-property1-rbtree seq)
          (init-property2-rbtree seq div))
        div inst))))

(defun init-associate1-rbtree (seq)
  (lambda ()
    (unless seq
      (error "sequence error, cons."))
    (destructuring-bind (key . value) (pop seq)
      (values key value))))

(defun init-associate2-rbtree (seq size)
  (let ((index 0))
    (lambda ()
      (unless (< index size)
        (error "sequence error, cons."))
      (destructuring-bind (key . value) (aref seq index)
        (incf index 1)
        (values key value)))))

(defun init-associate-rbtree (inst seq)
  (let ((size (length seq)))
    (init-rbtree
      (if (listp seq)
        (init-associate1-rbtree seq)
        (init-associate2-rbtree seq size))
      size inst)))

