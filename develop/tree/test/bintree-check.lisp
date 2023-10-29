(in-package #:bintree)

(export 'check-bintree)
(export 'check-bintree-error)

;;  tree
(defun check-small-binnode (key1 node compare)
  (or (null node)
      (let ((key (binnode-key node))
            (left (binnode-left node))
            (right (binnode-right node)))
        (and (< 0 (funcall compare key1 key))
             (check-small-binnode key1 left compare)
             (check-small-binnode key1 right compare)))))

(defun check-large-binnode (key1 node compare)
  (or (null node)
      (let ((key (binnode-key node))
            (left (binnode-left node))
            (right (binnode-right node)))
        (and (< (funcall compare key1 key) 0)
             (check-large-binnode key1 left compare)
             (check-large-binnode key1 right compare)))))

(defun check-tree-binnode (node compare)
  (or (null node)
      (let ((key (binnode-key node))
            (left (binnode-left node))
            (right (binnode-right node)))
        (and (check-small-binnode key left compare)
             (check-large-binnode key right compare)
             (check-tree-binnode left compare)
             (check-tree-binnode right compare)))))

(defun check-tree-bintree-call (inst)
  (declare (type bintree inst))
  (check-tree-binnode
    (bintree-root inst)
    (bintree-compare inst)))

(defun check-tree-bintree (inst &key error)
  (let ((check (check-tree-bintree-call inst)))
    (when (and error (null check))
      (error "check-tree-bintree error, ~S." inst))
    check))

(defun check-tree-bintree-error (inst)
  (declare (type bintree inst))
  (check-tree-bintree inst :error t))

;;  size
(defun check-size-bintree-map (inst)
  (declare (type bintree inst))
  (let ((size 0))
    (map-binnode
      (lambda (x)
        (declare (ignore x))
        (incf size 1))
      inst)
    size))

(defun check-size-bintree-call (inst)
  (declare (type bintree inst))
  (let ((size (bintree-size inst))
        (check (check-size-bintree-map inst)))
    (values (= check size) check size)))

(defun check-size-bintree (inst &key error)
  (declare (type bintree inst))
  (multiple-value-bind (check count size) (check-size-bintree-call inst)
    (when (and error (null check))
      (error "check-size-bintree error, ~S, ~A /= ~A." inst count size))
    (values check count size)))

(defun check-size-bintree-error (inst)
  (declare (type bintree inst))
  (check-size-bintree inst :error t))

;;  check
(defun check-bintree (inst &key error)
  (declare (type bintree inst))
  (and (check-tree-bintree inst :error error)
       (check-size-bintree inst :error error)))

(defun check-bintree-error (inst)
  (declare (type bintree inst))
  (check-bintree inst :error t))

