(in-package #:avlptree)

(export 'check-avlptree)
(export 'check-avlptree-error)

;;  balanced
(defun check-balanced-avlpnode (node)
  (if (null node)
    0
    (let* ((left (avlpnode-left node))
           (right (avlpnode-right node))
           (balance (avlpnode-balance node))
           (x (check-balanced-avlpnode left))
           (y (check-balanced-avlpnode right))
           (z (- x y)))
      (unless (= balance z)
        (error "balance error, ~S, ~S." balance z))
      (1+ (max x y)))))

(defun check-balanced-avlptree (inst &key error)
  (declare (type avlptree inst))
  (let ((root (avlptree-root inst)))
    (if error
      (check-balanced-avlpnode root)
      (handler-case
        (progn (check-balanced-avlpnode root) t)
        (error () nil)))))

(defun check-balanced-avlptree-error (inst)
  (declare (type avlptree inst))
  (check-balanced-avlptree inst :error t))

;;  parent
(defun check-parent-node-avlptree (node)
  (or (null node)
      (let ((left (avlpnode-left node))
            (right (avlpnode-right node)))
        (and (or (null left) (eq (avlpnode-parent left) node))
             (or (null right) (eq (avlpnode-parent right) node))
             (check-parent-node-avlptree left)
             (check-parent-node-avlptree right)
             t))))

(defun check-parent-root-avlptree (inst)
  (let ((root (avlptree-root inst))
        (size (avlptree-size inst)))
    (if (zerop size)
      (null root)
      (and root t))))

(defun check-parent-avlptree-call (inst)
  (and (check-parent-root-avlptree inst)
       (check-parent-node-avlptree (avlptree-root inst))
       t))

(defun check-parent-avlptree (inst &key error)
  (let ((check (check-parent-avlptree-call inst)))
    (when (and error (null check))
      (error "check-parent-avlptree error, ~S." inst))
    check))

(defun check-parent-avlptree-error (inst)
  (declare (type avlptree inst))
  (check-parent-avlptree inst :error t))

;;  tree
(defun check-small-avlpnode (key1 node compare)
  (or (null node)
      (let ((key (avlpnode-key node))
            (left (avlpnode-left node))
            (right (avlpnode-right node)))
        (and (< 0 (funcall compare key1 key))
             (check-small-avlpnode key1 left compare)
             (check-small-avlpnode key1 right compare)))))

(defun check-large-avlpnode (key1 node compare)
  (or (null node)
      (let ((key (avlpnode-key node))
            (left (avlpnode-left node))
            (right (avlpnode-right node)))
        (and (< (funcall compare key1 key) 0)
             (check-large-avlpnode key1 left compare)
             (check-large-avlpnode key1 right compare)))))

(defun check-tree-avlpnode (node compare)
  (or (null node)
      (let ((key (avlpnode-key node))
            (left (avlpnode-left node))
            (right (avlpnode-right node)))
        (and (check-small-avlpnode key left compare)
             (check-large-avlpnode key right compare)
             (check-tree-avlpnode left compare)
             (check-tree-avlpnode right compare)))))

(defun check-tree-avlptree-call (inst)
  (declare (type avlptree inst))
  (check-tree-avlpnode
    (avlptree-root inst)
    (avlptree-compare inst)))

(defun check-tree-avlptree (inst &key error)
  (let ((check (check-tree-avlptree-call inst)))
    (when (and error (null check))
      (error "check-tree-avlptree error, ~S." inst))
    check))

(defun check-tree-avlptree-error (inst)
  (declare (type avlptree inst))
  (check-tree-avlptree inst :error t))

;;  size
(defun check-size-avlptree-map (inst)
  (declare (type avlptree inst))
  (let ((size 0))
    (map-avlpnode
      (lambda (x)
        (declare (ignore x))
        (incf size 1))
      inst)
    size))

(defun check-size-avlptree-call (inst)
  (declare (type avlptree inst))
  (let ((size (avlptree-size inst))
        (check (check-size-avlptree-map inst)))
    (values (= check size) check size)))

(defun check-size-avlptree (inst &key error)
  (declare (type avlptree inst))
  (multiple-value-bind (check count size) (check-size-avlptree-call inst)
    (when (and error (null check))
      (error "check-size-avlptree error, ~S, ~A /= ~A." inst count size))
    (values check count size)))

(defun check-size-avlptree-error (inst)
  (declare (type avlptree inst))
  (check-size-avlptree inst :error t))

;;  check
(defun check-avlptree (inst &key error)
  (declare (type avlptree inst))
  (and (check-balanced-avlptree inst :error error)
       (check-parent-avlptree inst :error error)
       (check-tree-avlptree inst :error error)
       (check-size-avlptree inst :error error)))

(defun check-avlptree-error (inst)
  (declare (type avlptree inst))
  (check-avlptree inst :error t))

