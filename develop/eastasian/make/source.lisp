(defun eastasian<= (a x b)
  (if b
    (<= a x b)
    (= a x)))

(defun eastasian-vector
  (x &optional
     (table *eastasian-vector*)
     (ai 0)
     (bi (1- *eastasian-size*))
     &aux
     (ci (floor (+ ai bi) 2))
     (a (elt table ai))
     (b (elt table bi))
     (c1 (car (elt table ci))))
  (declare (type vector table)
           (type unsigned-byte ai bi))
  (destructuring-bind (a1 a2 a3) a
    (destructuring-bind (b1 b2 b3) b
      (cond ((eastasian<= a1 x a2) a3)
            ((eastasian<= b1 x b2) b3)
            ((<= bi ai) nil)
            (t (if (< x c1)
                 (eastasian-vector x table (1+ ai) ci)
                 (eastasian-vector x table ci (1- bi))))))))

(defun eastasian-symbol (x)
  (declare (type (or character unsigned-byte) x))
  (when (characterp x)
    (setq x (char-code x)))
  (if (< x #x80)
    (elt *eastasian-ascii* x)
    (eastasian-vector x)))

(defun eastasian-width (x)
  (cdr (assoc (eastasian-symbol x) *eastasian-symbol*)))

