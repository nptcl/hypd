(in-package #:rbtree)

(export 'check-rbtree)
(export 'check-rbtree-error)

;;  root
(defun check-root-rbtree-call (inst)
  (declare (type rbtree inst))
  (let ((root (rbtree-root inst)))
    (or (and (null root)
             (eql (rbtree-size inst) 0))
        (eq (rbnode-color root) 'black))))

(defun check-root-rbtree (inst &key error)
  (declare (type rbtree inst))
  (let ((check (check-root-rbtree-call inst)))
    (when (and error (null check))
      (error "check-root-rbtree error, ~S, ~A." inst check))
    check))

(defun check-root-rbtree-error (inst)
  (declare (type rbtree inst))
  (check-root-rbtree inst :error t))


;;  red node
(defun check-red-rbtree-node (node)
  (let ((left (rbnode-left node))
        (right (rbnode-right node))
        (color (rbnode-color node)))
    (or (eq color 'black)
        (and (eq color 'red)
             (or (null left) (eq (rbnode-color left) 'black))
             (or (null right) (eq (rbnode-color right) 'black))))))

(defun check-red-rbtree-call (inst)
  (declare (type rbtree inst))
  (block nil
    (map-rbnode
      (lambda (node)
        (unless (check-red-rbtree-node node)
          (return nil)))
      inst)
    t))

(defun check-red-rbtree (inst &key error)
  (declare (type rbtree inst))
  (let ((check (check-red-rbtree-call inst)))
    (when (and error (null check))
      (error "check-red-rbtree error, ~S, ~A." inst check))
    check))

(defun check-red-rbtree-error (inst)
  (declare (type rbtree inst))
  (check-red-rbtree inst :error t))


;;  compare
(defun check-compare-rbtree-compare (node call)
  (let ((left (rbnode-left node))
        (right (rbnode-right node)))
    (when left
      (check-compare-rbtree-compare left call))
    (funcall call node)
    (when right
      (check-compare-rbtree-compare right call))))

(defun check-compare-rbtree-call (inst)
  (block nil
    (let ((compare (rbtree-compare inst))
          (root (rbtree-root inst))
          x y)
      (when root
        (check-compare-rbtree-compare
          root
          (lambda (node)
            (setq x y)
            (setq y (rbnode-key node))
            (when x
              (unless (funcall compare x y)
                (return nil)))))))
    t))

(defun check-compare-rbtree (inst &key error)
  (declare (type rbtree inst))
  (let ((check (check-compare-rbtree-call inst)))
    (when (and error (null check))
      (error "check-compare-rbtree error, ~S, ~A." inst check))
    check))

(defun check-compare-rbtree-error (inst)
  (declare (type rbtree inst))
  (check-compare-rbtree inst :error t))


;;  length
(defun check-length-rbtree-apply (node call &optional (count 0))
  (let* ((left (rbnode-left node))
         (right (rbnode-right node))
         (check1 (null left))
         (check2 (null right))
         (count1 (1+ count)))
    (when (and check1 check2)
      (funcall call count))
    (unless check1
      (check-length-rbtree-apply left call count1))
    (unless check2
      (check-length-rbtree-apply right call count1))))

(defun check-length-rbtree-maxmin (inst)
  (let ((root (rbtree-root inst))
        (max 0)
        (min 0))
    (check-length-rbtree-apply
      root
      (lambda (count)
        (setq max (max max count))
        (setq min (min min count))))
    (values max min)))

(defun check-length-rbtree-size (inst)
  (let ((size (rbtree-size inst)))
    (if (eql size 0)
      (values size size)
      (check-length-rbtree-maxmin inst))))

(defun check-length-rbtree-call (inst)
  (multiple-value-bind (x y) (check-length-rbtree-size inst)
    (values
      (or (and (zerop x) (zerop y))
          (and (eql x 1) (eql y 1))
          (<= y (* 2 x)))
      x y)))

(defun check-length-rbtree (inst &key error)
  (declare (type rbtree inst))
  (multiple-value-bind (check x y) (check-length-rbtree-call inst)
    (when (and error (null check))
      (error "check-length-rbtree error, ~S, ~A, ~A, ~A." inst check x y))
    check))

(defun check-length-rbtree-error (inst)
  (declare (type rbtree inst))
  (check-length-rbtree inst :error t))


;;  size
(defun check-size-rbtree-map (inst)
  (declare (type rbtree inst))
  (let ((size 0))
    (map-rbnode
      (lambda (x)
        (declare (ignore x))
        (incf size 1))
      inst)
    size))

(defun check-size-rbtree-call (inst)
  (declare (type rbtree inst))
  (let ((size (rbtree-size inst))
        (check (check-size-rbtree-map inst)))
    (values (= check size) check size)))

(defun check-size-rbtree (inst &key error)
  (declare (type rbtree inst))
  (multiple-value-bind (check count size) (check-size-rbtree-call inst)
    (when (and error (null check))
      (error "check-size-rbtree error, ~S, ~A /= ~A." inst count size))
    (values check count size)))

(defun check-size-rbtree-error (inst)
  (declare (type rbtree inst))
  (check-size-rbtree inst :error t))


;;  check
(defun check-rbtree (inst &key error)
  (declare (type rbtree inst))
  (and (check-root-rbtree inst :error error)
       (check-red-rbtree inst :error error)
       (check-compare-rbtree inst :error error)
       (check-length-rbtree inst :error error)
       (check-size-rbtree inst :error error)
       t))

(defun check-rbtree-error (inst)
  (declare (type rbtree inst))
  (check-rbtree inst :error t))

