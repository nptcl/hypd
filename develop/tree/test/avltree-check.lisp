(in-package #:avltree)

(export 'check-avltree)
(export 'check-avltree-error)

;;  balanced
(defun check-balanced-avlnode (node)
  (if (null node)
    0
    (let* ((left (avlnode-left node))
           (right (avlnode-right node))
           (balance (avlnode-balance node))
           (x (check-balanced-avlnode left))
           (y (check-balanced-avlnode right))
           (z (- x y)))
      (unless (= balance z)
        (error "balance error, ~S, ~S." balance z))
      (1+ (max x y)))))

(defun check-balanced-avltree (inst &key error)
  (declare (type avltree inst))
  (let ((root (avltree-root inst)))
    (if error
      (check-balanced-avlnode root)
      (handler-case
        (progn (check-balanced-avlnode root) t)
        (error () nil)))))

(defun check-balanced-avltree-error (inst)
  (declare (type avltree inst))
  (check-balanced-avltree inst :error t))

;;  tree
(defun check-small-avlnode (key1 node compare)
  (or (null node)
      (let ((key (avlnode-key node))
            (left (avlnode-left node))
            (right (avlnode-right node)))
        (and (< 0 (funcall compare key1 key))
             (check-small-avlnode key1 left compare)
             (check-small-avlnode key1 right compare)))))

(defun check-large-avlnode (key1 node compare)
  (or (null node)
      (let ((key (avlnode-key node))
            (left (avlnode-left node))
            (right (avlnode-right node)))
        (and (< (funcall compare key1 key) 0)
             (check-large-avlnode key1 left compare)
             (check-large-avlnode key1 right compare)))))

(defun check-tree-avlnode (node compare)
  (or (null node)
      (let ((key (avlnode-key node))
            (left (avlnode-left node))
            (right (avlnode-right node)))
        (and (check-small-avlnode key left compare)
             (check-large-avlnode key right compare)
             (check-tree-avlnode left compare)
             (check-tree-avlnode right compare)))))

(defun check-tree-avltree-call (inst)
  (declare (type avltree inst))
  (check-tree-avlnode
    (avltree-root inst)
    (avltree-compare inst)))

(defun check-tree-avltree (inst &key error)
  (let ((check (check-tree-avltree-call inst)))
    (when (and error (null check))
      (error "check-tree-avltree error, ~S." inst))
    check))

(defun check-tree-avltree-error (inst)
  (declare (type avltree inst))
  (check-tree-avltree inst :error t))

;;  size
(defun check-size-avltree-map (inst)
  (declare (type avltree inst))
  (let ((size 0))
    (map-avlnode
      (lambda (x)
        (declare (ignore x))
        (incf size 1))
      inst)
    size))

(defun check-size-avltree-call (inst)
  (declare (type avltree inst))
  (let ((size (avltree-size inst))
        (check (check-size-avltree-map inst)))
    (values (= check size) check size)))

(defun check-size-avltree (inst &key error)
  (declare (type avltree inst))
  (multiple-value-bind (check count size) (check-size-avltree-call inst)
    (when (and error (null check))
      (error "check-size-avltree error, ~S, ~A /= ~A." inst count size))
    (values check count size)))

(defun check-size-avltree-error (inst)
  (declare (type avltree inst))
  (check-size-avltree inst :error t))

;;  check
(defun check-avltree (inst &key error)
  (declare (type avltree inst))
  (and (check-balanced-avltree inst :error error)
       (check-tree-avltree inst :error error)
       (check-size-avltree inst :error error)))

(defun check-avltree-error (inst)
  (declare (type avltree inst))
  (check-avltree inst :error t))

