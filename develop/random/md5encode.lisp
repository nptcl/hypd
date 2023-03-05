;;
;;  md5encode.lisp
;;
;;  [RFC1321] The MD5 Message-Digest Algorithm
;;  https://www.ietf.org/rfc/rfc1321.txt
;;
;; 1. Sequence
;;   * (sequence-md5encode (map 'vector #'char-code "Hello md5encode"))
;;     -> #(18 2A CD 2B 34 CE 89 38 1 87 5F 2C 20 1D 75 47)
;;
;; 2. Buffers
;;   * (setq md5 (make-md5encode))
;;   * (read-md5encode md5 (map 'vector #'char-code "Hello "))
;;   * (read-md5encode md5 (map 'vector #'char-code "md5encode"))
;;   * (calc-md5encode md5)
;;     -> #(18 2A CD 2B 34 CE 89 38 1 87 5F 2C 20 1D 75 47)
;;
(defpackage #:md5encode
  (:use #:common-lisp)
  (:export
    #:md5encode-error
    #:+md5encode-size+
    #:md5encode
    #:make-md5encode
    #:copy-md5encode
    #:clear-md5encode
    #:read-md5encode
    #:calc-md5encode
    #:sequence-md5encode))

(in-package #:md5encode)


;;
;;  define
;;
(defconstant +initial-word-a+ #x67452301)
(defconstant +initial-word-b+ #xEFCDAB89)
(defconstant +initial-word-c+ #x98BADCFE)
(defconstant +initial-word-d+ #x10325476)
(defconstant +calc-table+
  #(#x00000000
    #xD76AA478 #xE8C7B756 #x242070DB #xC1BDCEEE
    #xF57C0FAF #x4787C62A #xA8304613 #xFD469501
    #x698098D8 #x8B44F7AF #xFFFF5BB1 #x895CD7BE
    #x6B901122 #xFD987193 #xA679438E #x49B40821
    #xF61E2562 #xC040B340 #x265E5A51 #xE9B6C7AA
    #xD62F105D #x02441453 #xD8A1E681 #xE7D3FBC8
    #x21E1CDE6 #xC33707D6 #xF4D50D87 #x455A14ED
    #xA9E3E905 #xFCEFA3F8 #x676F02D9 #x8D2A4C8A
    #xFFFA3942 #x8771F681 #x6D9D6122 #xFDE5380C
    #xA4BEEA44 #x4BDECFA9 #xF6BB4B60 #xBEBFBC70
    #x289B7EC6 #xEAA127FA #xD4EF3085 #x04881D05
    #xD9D4D039 #xE6DB99E5 #x1FA27CF8 #xC4AC5665
    #xF4292244 #x432AFF97 #xAB9423A7 #xFC93A039
    #x655B59C3 #x8F0CCC92 #xFFEFF47D #x85845DD1
    #x6FA87E4F #xFE2CE6E0 #xA3014314 #x4E0811A1
    #xF7537E82 #xBD3AF235 #x2AD7D2BB #xEB86D391))

(defconstant +md5encode-size+ 16)

(define-condition md5encode-error (simple-error) ())


;;
;;  structure
;;
(deftype md5encode-result ()
  `(array (unsigned-byte 32) (,+md5encode-size+)))

(defstruct md5encode
  (a +initial-word-a+ :type (unsigned-byte 32))
  (b +initial-word-b+ :type (unsigned-byte 32))
  (c +initial-word-c+ :type (unsigned-byte 32))
  (d +initial-word-d+ :type (unsigned-byte 32))
  (x (make-array +md5encode-size+
                 :element-type '(unsigned-byte 32)
                 :initial-element 0)
     :type md5encode-result)
  (size 0 :type unsigned-byte)
  (pos 0 :type unsigned-byte)
  (finish nil :type boolean))

(defun clear-md5encode (md5)
  (declare (type md5encode md5))
  (setf (md5encode-a md5) +initial-word-a+)
  (setf (md5encode-b md5) +initial-word-b+)
  (setf (md5encode-c md5) +initial-word-c+)
  (setf (md5encode-d md5) +initial-word-d+)
  (setf (md5encode-size md5) 0)
  (setf (md5encode-pos md5) 0)
  (setf (md5encode-finish md5) nil)
  (values))


;;
;;  encode
;;
(defmacro log32 (x)
  `(logand #xFFFFFFFF ,x))

(defmacro plus32 (x y)
  `(log32 (+ ,x ,y)))

(defmacro calc-f (x y z)
  `(logior (logand ,x ,y)
           (logand (lognot ,x) ,z)))

(defmacro calc-g (x y z)
  `(logior (logand ,x ,z)
           (logand ,y (lognot ,z))))

(defmacro calc-h (x y z)
  `(logxor (logxor ,x ,y) ,z))

(defmacro calc-i (x y z)
  `(logxor ,y (logior ,x (lognot ,z))))

(defmacro calc-r32 (v s)
  `(logior (log32 (ash ,v ,s)) (ash ,v (- ,s 32))))

(defmacro calc (op x a b c d k s i)
  (let ((e (intern (format nil "CALC-~A" op))))
    `(progn
       (setq ,a (log32 (+ ,a (,e ,b ,c ,d)
                          (aref ,x ,k)
                          (aref +calc-table+ ,i))))
       (setq ,a (calc-r32 ,a ,s))
       (setq ,a (plus32 ,a ,b)))))

(defun calcblock (md5)
  (declare (type md5encode md5))
  (let* ((a0 (md5encode-a md5))
         (b0 (md5encode-b md5))
         (c0 (md5encode-c md5))
         (d0 (md5encode-d md5))
         (x (md5encode-x md5))
         (a a0)
         (b b0)
         (c c0)
         (d d0))

    ;;  Round 1.
    (calc F x  a b c d   0   7   1)
    (calc F x  d a b c   1  12   2)
    (calc F x  c d a b   2  17   3)
    (calc F x  b c d a   3  22   4)
    (calc F x  a b c d   4   7   5)
    (calc F x  d a b c   5  12   6)
    (calc F x  c d a b   6  17   7)
    (calc F x  b c d a   7  22   8)
    (calc F x  a b c d   8   7   9)
    (calc F x  d a b c   9  12  10)
    (calc F x  c d a b  10  17  11)
    (calc F x  b c d a  11  22  12)
    (calc F x  a b c d  12   7  13)
    (calc F x  d a b c  13  12  14)
    (calc F x  c d a b  14  17  15)
    (calc F x  b c d a  15  22  16)

    ;;  Round 2.
    (calc G x  a b c d   1   5  17)
    (calc G x  d a b c   6   9  18)
    (calc G x  c d a b  11  14  19)
    (calc G x  b c d a   0  20  20)
    (calc G x  a b c d   5   5  21)
    (calc G x  d a b c  10   9  22)
    (calc G x  c d a b  15  14  23)
    (calc G x  b c d a   4  20  24)
    (calc G x  a b c d   9   5  25)
    (calc G x  d a b c  14   9  26)
    (calc G x  c d a b   3  14  27)
    (calc G x  b c d a   8  20  28)
    (calc G x  a b c d  13   5  29)
    (calc G x  d a b c   2   9  30)
    (calc G x  c d a b   7  14  31)
    (calc G x  b c d a  12  20  32)

    ;;  Round 3.
    (calc H x  a b c d   5   4  33)
    (calc H x  d a b c   8  11  34)
    (calc H x  c d a b  11  16  35)
    (calc H x  b c d a  14  23  36)
    (calc H x  a b c d   1   4  37)
    (calc H x  d a b c   4  11  38)
    (calc H x  c d a b   7  16  39)
    (calc H x  b c d a  10  23  40)
    (calc H x  a b c d  13   4  41)
    (calc H x  d a b c   0  11  42)
    (calc H x  c d a b   3  16  43)
    (calc H x  b c d a   6  23  44)
    (calc H x  a b c d   9   4  45)
    (calc H x  d a b c  12  11  46)
    (calc H x  c d a b  15  16  47)
    (calc H x  b c d a   2  23  48)

    ;;  Round 4.
    (calc I x  a b c d   0   6  49)
    (calc I x  d a b c   7  10  50)
    (calc I x  c d a b  14  15  51)
    (calc I x  b c d a   5  21  52)
    (calc I x  a b c d  12   6  53)
    (calc I x  d a b c   3  10  54)
    (calc I x  c d a b  10  15  55)
    (calc I x  b c d a   1  21  56)
    (calc I x  a b c d   8   6  57)
    (calc I x  d a b c  15  10  58)
    (calc I x  c d a b   6  15  59)
    (calc I x  b c d a  13  21  60)
    (calc I x  a b c d   4   6  61)
    (calc I x  d a b c  11  10  62)
    (calc I x  c d a b   2  15  63)
    (calc I x  b c d a   9  21  64)

    (setf (md5encode-a md5) (plus32 a a0))
    (setf (md5encode-b md5) (plus32 b b0))
    (setf (md5encode-c md5) (plus32 c c0))
    (setf (md5encode-d md5) (plus32 d d0)))
  (values))

(defun read-md5encode (md5 from &optional len)
  (declare (type md5encode md5))
  ;;  finish
  (when (md5encode-finish md5)
    (error (make-condition
             'md5encode-error
             :format-control "md5encode is already finished.")))
  ;;  read
  (unless len
    (setq len (length from)))
  (let* ((pos (md5encode-pos md5))
         (x (md5encode-x md5)))
    (multiple-value-bind (j k) (truncate pos 4)
      (dotimes (i len)
        (when (<= 64 pos)
          (calcblock md5)
          (setq pos 0 j 0 k 0))
        (when (zerop k)
          (setf (aref x j) 0))
        (let* ((or1 (ash (elt from i) (* 8 k)))
               (or2 (logior (aref x j) or1)))
          (setf (aref x j) (log32 or2)))
        (incf k 1)
        (when (<= 4 k)
          (incf j 1)
          (setq k 0))
        (incf pos 1)))
    (incf (md5encode-size md5) len)
    (setf (md5encode-pos md5) pos))
  (values))

(defun wordtobyte (value array shift)
  (dotimes (i 4)
    (setf (aref array (+ i shift)) (logand #xFF value))
    (setq value (ash value -8))))

(defun calcfinal (md5)
  (let* ((padding (make-array (+ 64 8) :element-type '(unsigned-byte 8)))
         (size (md5encode-size md5))
         (len (- 64 (mod (+ size 8) 64)))
         (pos 1))
    (setf (aref padding 0) #x80)
    (do ()
      ((not (< pos len)))
      (setf (aref padding pos) 0)
      (incf pos 1))
    ;;  bit size -> byte size
    (setf (aref padding pos) (logand #xFF (ash size 3)))
    (incf pos 1)
    (setq size (ash size -5)) ;; (- (- 8 3))
    ;;  byte size
    (do ((k 1))
      ((not (< k 8)))
      (setf (aref padding pos) (logand #xFF size))
      (incf k 1)
      (incf pos 1)
      (setq size (ash size -8)))
    ;;  read padding
    (read-md5encode md5 padding pos)
    (calcblock md5)))

(defun calc-md5encode (md5 &optional array)
  (declare (type md5encode md5))
  (unless (md5encode-finish md5)
    (calcfinal md5)
    (setf (md5encode-finish md5) t))
  (unless array
    (setq array (make-array +md5encode-size+ :element-type '(unsigned-byte 8))))
  (wordtobyte (md5encode-a md5) array 0)
  (wordtobyte (md5encode-b md5) array 4)
  (wordtobyte (md5encode-c md5) array 8)
  (wordtobyte (md5encode-d md5) array 12)
  array)

(defun sequence-md5encode (from &optional len)
  (let ((md5 (make-md5encode)))
    (read-md5encode md5 from len)
    (calc-md5encode md5)))

