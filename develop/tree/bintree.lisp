;;
;;  Binary tree
;;
(defpackage #:bintree
  (:use common-lisp)
  (:export
    #:bintree
    #:bintree-p
    #:make-bintree
    #:clear-bintree
    #:size-bintree
    #:empty-bintree
    #:min-bintree
    #:max-bintree
    #:map-bintree
    #:map-binnode
    #:keys-bintree
    #:values-bintree
    #:hash-table-bintree

    #:*replace-mode-bintree*
    #:*replace-key-bintree*
    #:insert-bintree
    #:intern-bintree
    #:search-bintree
    #:replace-bintree
    #:delete-bintree
    #:init-bintree
    #:init-property-bintree
    #:init-associate-bintree))

(in-package #:bintree)

;;
;;  bintree
;;
(defstruct (bintree
             (:constructor bintree-heap)
             (:copier nil))
  root
  (compare #'- :type function)
  (size 0 :type unsigned-byte))

(defmethod print-object ((inst bintree) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (bintree-size inst))))

(defun make-bintree (&key (compare #'-))
  (bintree-heap :compare compare))

(defun clear-bintree (inst)
  (declare (type bintree inst))
  (setf (bintree-root inst) nil)
  (setf (bintree-size inst) 0))

(defun size-bintree (inst)
  (declare (type bintree inst))
  (bintree-size inst))


;;
;;  binnode
;;
(defstruct (binnode (:copier nil))
  left right key value)

(defmethod print-object ((inst binnode) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (binnode-key inst))))


;;
;;  operator
;;
(defun empty-bintree (inst)
  (declare (type bintree inst))
  (if (bintree-root inst) nil t))

(defun min-binnode (node)
  (let ((left (binnode-left node)))
    (if left
      (min-binnode left)
      (values (binnode-key node) t))))

(defun min-bintree (inst)
  (declare (type bintree inst))
  (let ((root (bintree-root inst)))
    (multiple-value-bind (x y)
      (when root
        (min-binnode root))
      (if y
        (values x t)
        (values nil nil)))))

(defun max-binnode (node)
  (let ((right (binnode-right node)))
    (if right
      (max-binnode right)
      (values (binnode-key node) t))))

(defun max-bintree (inst)
  (declare (type bintree inst))
  (let ((root (bintree-root inst)))
    (multiple-value-bind (x y)
      (when root
        (max-binnode root))
      (if y
        (values x t)
        (values nil nil)))))

(defun map-binnode (call inst)
  (declare (type bintree inst))
  (labels ((rec (x) (when x
                      (rec (binnode-right x))
                      (funcall call x)
                      (rec (binnode-left x)))))
    (rec (bintree-root inst)))
  (values))

(defun map-bintree (call inst)
  (declare (type bintree inst))
  (map-binnode
    (lambda (x)
      (funcall call (binnode-key x) (binnode-value x)))
    inst))

(defun keys-bintree (inst)
  (declare (type bintree inst))
  (let (list)
    (map-binnode
      (lambda (x)
        (push (binnode-key x) list))
      inst)
    list))

(defun values-bintree (inst)
  (declare (type bintree inst))
  (let (list)
    (map-binnode
      (lambda (x)
        (push (binnode-value x) list))
      inst)
    list))

(defun hash-table-bintree (inst &optional (test 'eql))
  (declare (type bintree inst))
  (let ((table (make-hash-table :test test)))
    (map-bintree
      (lambda (key value)
        (setf (gethash key table) value))
      inst)
    table))


;;
;;  insert
;;
(defvar *replace-mode-bintree* nil)
(defvar *replace-key-bintree* t)

(defun replace-binnode (node key value)
  (when *replace-key-bintree*
    (setf (binnode-key node) key))
  (setf (binnode-value node) value))

(defun insert-binnode (compare node key value)
  (prog (key1 diff next)
    loop
    (setq key1 (binnode-key node))
    (setq diff (funcall compare key key1))

    ;; left
    (when (< diff 0)
      (setq next (binnode-left node))
      (unless next
        (setf (binnode-left node) (make-binnode :key key :value value))
        (return t))
      (setq node next)
      (go loop))

    ;; right
    (when (< 0 diff)
      (setq next (binnode-right node))
      (unless next
        (setf (binnode-right node) (make-binnode :key key :value value))
        (return t))
      (setq node next)
      (go loop))

    ;;  equal
    (when *replace-mode-bintree*
      (replace-binnode node key value))
    (return nil)))

(defun insert-bintree (inst key value &optional (replace nil opt))
  (let ((root (bintree-root inst)))
    (if root
      (let ((compare (bintree-compare inst))
            (*replace-mode-bintree* (if opt replace *replace-mode-bintree*)))
        (when (insert-binnode compare root key value)
          (incf (bintree-size inst) 1)
          t))
      (let ((node (make-binnode :key key :value value)))
        (setf (bintree-root inst) node)
        (setf (bintree-size inst) 1)
        t))))

(defun intern-bintree (inst key value)
  (declare (type bintree inst))
  (insert-bintree inst key value t))


;;
;;  search
;;
(defun search-node-bintree (inst key)
  (declare (type bintree inst))
  (prog ((node (bintree-root inst))
         (compare (bintree-compare inst))
         key1 diff)
    loop
    (unless node
      (return nil))
    (setq key1 (binnode-key node))
    (setq diff (funcall compare key key1))
    (cond ((< diff 0) (setq node (binnode-left node)))
          ((< 0 diff) (setq node (binnode-right node)))
          (t (return node)))
    (go loop)))

(defun search-bintree (inst key)
  (declare (type bintree inst))
  (let ((node (search-node-bintree inst key)))
    (if node
      (values (binnode-value node) t)
      (values nil nil))))

(defun replace-bintree (inst key value &optional (replace nil opt))
  (declare (type bintree inst))
  (let ((node (search-node-bintree inst key))
        (*replace-key-bintree* (if opt replace *replace-key-bintree*)))
    (when node
      (replace-binnode node key value)
      t)))


;;
;;  delete
;;
(defun free-binnode (delete)
  (setf (binnode-left delete) :error)
  (setf (binnode-right delete) :error)
  (setf (binnode-key delete) :error)
  (setf (binnode-value delete) :error)
  nil)

(defun free-copy-binnode (replace delete)
  (setf (binnode-key replace) (binnode-key delete))
  (setf (binnode-value replace) (binnode-value delete))
  (free-binnode delete))

(defun delete-swap-binnode (replace node)
  (prog (next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  left
    (setq next (binnode-left node))
    (multiple-value-setq (next check) (delete-swap-binnode replace next))
    (when check
      (setf (binnode-left node) next)
      (return (values node t)))

    ;;  right
    (setq next (binnode-right node))
    (free-copy-binnode replace node)
    (return (values next t))))

(defun delete-binnode (compare node key)
  (prog (key1 diff left right next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  move
    (setq key1 (binnode-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (go move-left))
    (when (< 0 diff)
      (go move-right))
    (go move-delete)

    ;;  left
    move-left
    (setq next (binnode-left node))
    (multiple-value-setq (next check) (delete-binnode compare next key))
    (unless check
      (return (values nil nil)))
    (setf (binnode-left node) next)
    (return (values node t))

    ;;  right
    move-right
    (setq next (binnode-right node))
    (multiple-value-setq (next check) (delete-binnode compare next key))
    (unless check
      (return (values nil nil)))
    (setf (binnode-right node) next)
    (return (values node t))

    ;;  delete
    move-delete
    (setq left (binnode-left node))
    (setq right (binnode-right node))
    (when (and left right)
      (go delete-swap))
    (when left
      (go delete-left))
    (when right
      (go delete-right))
    (go delete-single)

    ;;  left, right
    delete-swap
    (multiple-value-setq (next check) (delete-swap-binnode node right))
    (setf (binnode-right node) next)
    (return (values node t))

    ;;  left only
    delete-left
    (free-binnode node)
    (return (values left t))

    ;;  right only
    delete-right
    (free-binnode node)
    (return (values right t))

    ;;  null, null
    delete-single
    (free-binnode node)
    (return (values nil t))))

(defun delete-bintree (inst key)
  (declare (type bintree inst))
  (let ((compare (bintree-compare inst))
        (root (bintree-root inst)))
    (multiple-value-bind (node check) (delete-binnode compare root key)
      (when check
        (setf (bintree-root inst) node)
        (decf (bintree-size inst) 1)
        t))))


;;
;;  init
;;
(defstruct init-bintree-struct
  call depth limit lower direct)

(defun init-loop-bintree (str now)
  (prog* ((call (init-bintree-struct-call str))
          (depth (init-bintree-struct-depth str))
          (limit (init-bintree-struct-limit str))
          (lower (init-bintree-struct-lower str))
          (direct (init-bintree-struct-direct str))
          (depth1 (1- depth))
          node left right key value)
    (unless (< now depth)
      (return nil))
    (when (= now depth1)
      (unless (< lower limit)
        (return nil))
      (incf (init-bintree-struct-lower str) 1))
    (incf now 1)
    (setq left (init-loop-bintree str now))
    (multiple-value-setq (key value) (funcall call))
    (setq right (init-loop-bintree str now))
    (when direct
      (rotatef left right))
    (setq node (make-binnode :key key :value value :left left :right right))
    (return node)))

(defun init-call-bintree (call size direct)
  (let* ((depth (integer-length size))
         (limit (- size (1- (ash 1 (1- depth))))))
    (init-loop-bintree
      (make-init-bintree-struct
        :call call :depth depth :limit limit :lower 0 :direct direct)
      0)))

(defun init-bintree (call size &optional inst direct)
  (unless inst
    (setq inst (make-bintree)))
  (setf (bintree-root inst) (init-call-bintree call size direct))
  (setf (bintree-size inst) size)
  inst)

(defun init-property1-bintree (seq)
  (lambda (&aux key value)
    (unless seq
      (error "sequence error, key."))
    (setq key (pop seq))
    (unless seq
      (error "sequence error, value, ~S." key))
    (setq value (pop seq))
    (values key value)))

(defun init-property2-bintree (seq size)
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

(defun init-property-bintree (inst seq)
  (let ((size (length seq)))
    (multiple-value-bind (div rem) (truncate size 2)
      (unless (zerop rem)
        (error "size error, ~A." size))
      (init-bintree
        (if (listp seq)
          (init-property1-bintree seq)
          (init-property2-bintree seq div))
        div inst))))

(defun init-associate1-bintree (seq)
  (lambda ()
    (unless seq
      (error "sequence error, cons."))
    (destructuring-bind (key . value) (pop seq)
      (values key value))))

(defun init-associate2-bintree (seq size)
  (let ((index 0))
    (lambda ()
      (unless (< index size)
        (error "sequence error, cons."))
      (destructuring-bind (key . value) (aref seq index)
        (incf index 1)
        (values key value)))))

(defun init-associate-bintree (inst seq)
  (let ((size (length seq)))
    (init-bintree
      (if (listp seq)
        (init-associate1-bintree seq)
        (init-associate2-bintree seq size))
      size inst)))

