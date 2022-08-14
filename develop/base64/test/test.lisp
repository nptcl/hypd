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

(format t "~S~%" (coerce-binary "Hello"))
(format t "~S~%" (coerce-string #(72 101 108 108 111)))

(format t "Result: ~S~%"
        (coerce-string
          (base64-decode-string "QUJD")))

(format t "~S~%"
        (coerce-string
          (base64-decode-string
            (base64-encode-binary
              (coerce-binary "ABC")))))


;;
;;  encode
;;
(let (encode v x y value)
  (setq encode (base64-encode-init))

  (setf (base64-encode-char-62 encode) #\+)
  (setf (base64-encode-char-63 encode) #\/)
  (setf (base64-encode-char-padding encode) #\=)
  (setf (base64-encode-padding encode) t)

  (setq v (char-code #\A))
  (multiple-value-setq (x y) (base64-encode-pipe encode v))

  (setq value nil)

  (when x (push x value))
  (when y (push y value))

  (setq v (char-code #\B))
  (multiple-value-setq (x y) (base64-encode-pipe encode v))
  (when x (push x value))
  (when y (push y value))

  '(do (v) (nil)
     (setq v (base64-encode-closing encode))
     (if v
       (push v value)
       (return nil)))

  (base64-encode-close
    encode
    (lambda (v) (push v value)))

  (setq value (nreverse value))
  (format t "~S~%" (coerce value 'string)))


;;
;;  decode
;;
(let (decode value)
  (setq decode (base64-decode-init))

  (setf (base64-decode-char-62 decode) #\+)
  (setf (base64-decode-char-63 decode) #\/)
  (setf (base64-decode-char-padding decode) #\=)
  (setf (base64-decode-ignore-eol decode) t)
  (setf (base64-decode-ignore-others decode) nil)
  (setf (base64-decode-ignore-padding decode) nil)

  (setq value nil)
  (let ((x (base64-decode-pipe decode #\Q)))
    (when x (push x value)))

  (let ((x (base64-decode-pipe decode #\U)))
    (when x (push x value)))
  (let ((x (base64-decode-pipe decode #\I)))
    (when x (push x value)))
  (let ((x (base64-decode-pipe decode #\=)))
    (when x (push x value)))

  (base64-decode-close decode)

  (setq value (nreverse value))
  (format t "~S~%" (coerce-string value)))


;;
;;  random
;;
(defvar *input*)
(defvar *output*)
(defvar *check*)
(defvar *encode*)
(defvar *decode*)

(defun push-extend (value array)
  (vector-push-extend value array (array-total-size array)))

(defun base64-encode-vector (inst input output)
  (base64-encode-clear inst)
  (dotimes (i (length input))
    (let ((v (elt input i)))
      (multiple-value-bind (x y) (base64-encode-pipe inst v)
        (when x (push-extend x output))
        (when y (push-extend y output)))))
  (base64-encode-close
    inst (lambda (v) (push-extend v output))))

(defun base64-decode-vector (inst input output)
  (base64-decode-clear inst)
  (dotimes (i (length input))
    (let* ((v (elt input i))
           (x (base64-decode-pipe inst v)))
      (when x (push-extend x output))))
  (base64-decode-close inst))

(defun make-input-binary (n)
  (dotimes (i n)
    (push-extend (random #x0100) *input*)))

(defun vector-equal (x y)
  (and (= (length x) (length y))
       (every #'eql x y)))

(defun clear-fill-pointer-binary (x)
  (dotimes (i (length x))
    (setf (aref x i) (random #x0100)))
  (setf (fill-pointer x) 0))

(defun clear-fill-pointer-string (x)
  (dotimes (i (length x))
    (setf (aref x i) (code-char (random #x0100))))
  (setf (fill-pointer x) 0))

(defun random-test (n)
  (clear-fill-pointer-binary *input*)
  (clear-fill-pointer-string *output*)
  (clear-fill-pointer-binary *check*)
  (base64-encode-clear *encode*)
  (base64-decode-clear *decode*)
  (make-input-binary n)
  ;;  padding
  (clear-fill-pointer-string *output*)
  (setf (base64-encode-padding *encode*) t)
  (base64-encode-vector *encode* *input* *output*)
  (format t "~S~%" *output*)

  (clear-fill-pointer-binary *check*)
  (setf (base64-decode-ignore-padding *decode*) nil)
  (base64-decode-vector *decode* *output* *check*)
  (unless (vector-equal *input* *check*)
    (error "vector-equal error, ~S, ~S." *input* *check*))

  (clear-fill-pointer-binary *check*)
  (setf (base64-decode-ignore-padding *decode*) t)
  (base64-decode-vector *decode* *output* *check*)
  (unless (vector-equal *input* *check*)
    (error "vector-equal error, ~S, ~S." *input* *check*))

  ;;  no padding
  (clear-fill-pointer-string *output*)
  (setf (base64-encode-padding *encode*) nil)
  (base64-encode-vector *encode* *input* *output*)
  (format t "~S~%" *output*)

  (clear-fill-pointer-binary *check*)
  (setf (base64-decode-ignore-padding *decode*) t)
  (base64-decode-vector *decode* *output* *check*)
  (unless (vector-equal *input* *check*)
    (error "vector-equal error, ~S, ~S." *input* *check*))
  )

(defun run-random (n)
  (dotimes (i n)
    (unless (zerop i)
      (random-test i))))

(defun make-array-fill-pointer (type)
  (make-array 16 :adjustable t
              :fill-pointer 0
              :element-type type))

(let ((*random-state* (make-random-state t))
      (*input* (make-array-fill-pointer '(mod 256)))
      (*output* (make-array-fill-pointer 'character))
      (*check* (make-array-fill-pointer '(mod 256)))
      (*encode* (base64-encode-init))
      (*decode* (base64-decode-init)))
  (dotimes (i 5)
    (run-random 200)))

