(load "md5encode.lisp")
(defpackage work (:use cl md5encode))
(in-package work)

(let ((array (make-array 2000
                         :element-type '(unsigned-byte 8)
                         :initial-element 0)))
  (dotimes (k 2000)
    (setf (aref array k) (logand #xFF k))
    (let ((x (sequence-md5encode array 2000)))
      (format t "~A: " k)
      (dotimes (i +md5encode-size+)
        (format t "~(~2,'0X~)" (aref x i)))
      (format t "~%"))))

