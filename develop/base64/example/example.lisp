(in-package common-lisp-user)
(load #p"../base64.lisp")

(defpackage work
  (:use common-lisp hypd-base64))
(in-package work)

(defun base64-encode-binary (input &optional (inst (base64-encode-init)))
  (with-output-to-string (s)
    (dotimes (i (length input))
      (let ((v (elt input i)))
        (multiple-value-bind (x y) (base64-encode-pipe inst v)
          (when x (write-char x s))
          (when y (write-char y s)))))
    (base64-encode-close
      inst
      (lambda (v) (write-char v s)))))

(defun base64-decode-string (input &optional (inst (base64-decode-init)))
  (let ((r (make-array 16 :adjustable t
                       :fill-pointer 0
                       :element-type '(mod 256))))
    (flet ((push-value (x) (vector-push-extend x r (array-total-size r))))
      (dotimes (i (length input))
        (let* ((v (elt input i))
               (x (base64-decode-pipe inst v)))
          (when x (push-value x))))
      (base64-decode-close inst)
      r)))

(defun coerce-binary (str)
  (map 'vector (lambda (c)
                 (if (characterp c)
                   (char-code c)
                   c))
       str))

(defun coerce-string (str)
  (map 'string (lambda (c)
                 (if (characterp c)
                   c
                   (code-char c)))
       str))

(format t "~S~%" (coerce-string
                   (base64-decode-string "QUJD")))

(format t "~S~%" (base64-encode-binary
                   (coerce-binary "ABC")))

