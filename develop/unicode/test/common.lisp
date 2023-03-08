(load #p"../unicode.lisp")
(defpackage work (:use cl unicode npt-rt))
(in-package work)

;;
;;  UTF-8
;;
(deftest make-state-utf8.1
  (typep (make-state-utf8) 'state-utf8)
  t)

(deftest state-utf8-p.1
  (state-utf8-p (make-state-utf8))
  t)

(deftest state-utf8-p.2
  (state-utf8-p 100)
  nil)

(deftest decode-utf8.1
  (let ((x (make-state-utf8)))
    (decode-utf8 x 0))
  0 t)

(deftest decode-utf8.2
  (let ((x (make-state-utf8)))
    (decode-utf8 x #x20))
  #x20 t)

(deftest decode-utf8.3
  (let ((x (make-state-utf8)))
    (decode-utf8 x #xF0))
  nil nil)

(deftest decode-utf8.4
  (let ((x (make-state-utf8)))
    (decode-utf8 x #xF0)
    (decode-utf8 x #x9F)
    (decode-utf8 x #x80)
    (decode-utf8 x #x81))
  #x01F001 t)

(deftest encode-utf8.1
  (encode-utf8 0)
  (0) 1)

(deftest encode-utf8.2
  (encode-utf8 #x01F001)
  (#xF0 #x9F #x80 #x81) 4)

(deftest encode-utf8.3
  (let ((x (vector nil nil nil nil nil)))
    (encode-utf8 0 x))
  #(0 nil nil nil nil) 1)

(deftest encode-utf8.4
  (let ((x (vector nil nil nil nil nil)))
    (encode-utf8 #x01F001 x))
  #(#xF0 #x9F #x80 #x81 nil) 4)

(deftest encode-utf8.5
  (let ((x (vector nil nil nil nil nil)))
    (encode-utf8 #xD800 x))
  nil 0)

(deftest length-utf8.1
  (length-utf8 -1)
  nil)

(deftest length-utf8.2
  (length-utf8 0)
  1)

(deftest length-utf8.3
  (length-utf8 #x80)
  2)


;;
;;  UTF-16
;;
(deftest make-state-utf16.1
  (typep (make-state-utf16) 'state-utf16)
  t)

(deftest state-utf16-p.1
  (state-utf16-p (make-state-utf16))
  t)

(deftest state-utf16-p.2
  (state-utf16-p 100)
  nil)

(deftest decode-utf16.1
  (let ((x (make-state-utf16)))
    (decode-utf16 x 0))
  0 t)

(deftest decode-utf16.2
  (let ((x (make-state-utf16)))
    (decode-utf16 x 20))
  20 t)

(deftest decode-utf16.3
  (let ((x (make-state-utf16)))
    (decode-utf16 x #xD800))
  nil nil)

(deftest decode-utf16.4
  (let ((x (make-state-utf16)))
    (decode-utf16 x #xD800)
    (decode-utf16 x #xDC00))
  #x010000 t)

(deftest decode-utf16.5
  (let ((x (make-state-utf16)))
    (decode-utf16 x #xD800)
    (decode-utf16 x #x0000))
  nil t)

(deftest encode-utf16.1
  (encode-utf16 0)
  (0) 1)

(deftest encode-utf16.2
  (encode-utf16 20)
  (20) 1)

(deftest encode-utf16.3
  (encode-utf16 #xD800)
  nil 0)

(deftest encode-utf16.4
  (encode-utf16 #x010000)
  (#xD800 #xDC00) 2)

(deftest length-utf16.1
  (length-utf16 -1)
  nil)

(deftest length-utf16.2
  (length-utf16 0)
  1)

(deftest length-utf16.3
  (length-utf16 #xD800)
  nil)

(deftest length-utf16.4
  (length-utf16 #x010000)
  2)


;;
;;  UTF-16LE
;;
(deftest make-state-utf16le.1
  (typep (make-state-utf16le) 'state-utf16le)
  t)

(deftest state-utf16le-p.1
  (state-utf16le-p (make-state-utf16le))
  t)

(deftest state-utf16le-p.2
  (state-utf16le-p 100)
  nil)

(deftest decode-utf16le.1
  (let ((x (make-state-utf16le)))
    (decode-utf16le x 0))
  nil nil)

(deftest decode-utf16le.2
  (let ((x (make-state-utf16le)))
    (decode-utf16le x 0)
    (decode-utf16le x 0))
  0 t)

(deftest decode-utf16le.3
  (let ((x (make-state-utf16le)))
    (decode-utf16le x 10)
    (decode-utf16le x 0))
  10 t)

(deftest decode-utf16le.4
  (let ((x (make-state-utf16le)))
    (decode-utf16le x 0)
    (decode-utf16le x #xAA))
  #xAA00 t)

(deftest decode-utf16le.5
  (let ((x (make-state-utf16le)))
    (decode-utf16le x #x12)
    (decode-utf16le x #x34))
  #x3412 t)

(deftest decode-utf16le.6
  (let ((x (make-state-utf16le)))
    (decode-utf16le x #x00)
    (decode-utf16le x #xD8)
    (decode-utf16le x #x0A)
    (decode-utf16le x #xDC))
  #x01000A t)

(deftest encode-utf16le.1
  (encode-utf16le 0)
  (#x00 #x00) 2)

(deftest encode-utf16le.2
  (encode-utf16le #xAA)
  (#xAA #x00) 2)

(deftest encode-utf16le.3
  (encode-utf16le #xAABB)
  (#xBB #xAA) 2)

(deftest encode-utf16le.4
  (encode-utf16le #xD800)
  nil 0)

(deftest encode-utf16le.5
  (encode-utf16le #x010000)
  (#x00 #xD8 #x00 #xDC) 4)

(deftest length-utf16le.1
  (length-utf16le -1)
  nil)

(deftest length-utf16le.2
  (length-utf16le 0)
  2)

(deftest length-utf16le.3
  (length-utf16le #xD800)
  nil)

(deftest length-utf16le.4
  (length-utf16le #x010000)
  4)


;;
;;  UTF-16BE
;;
(deftest make-state-utf16be.1
  (typep (make-state-utf16be) 'state-utf16be)
  t)

(deftest state-utf16be-p.1
  (state-utf16be-p (make-state-utf16be))
  t)

(deftest state-utf16be-p.2
  (state-utf16be-p 100)
  nil)

(deftest decode-utf16be.1
  (let ((x (make-state-utf16be)))
    (decode-utf16be x 0))
  nil nil)

(deftest decode-utf16be.2
  (let ((x (make-state-utf16be)))
    (decode-utf16be x 0)
    (decode-utf16be x 0))
  0 t)

(deftest decode-utf16be.3
  (let ((x (make-state-utf16be)))
    (decode-utf16be x 0)
    (decode-utf16be x 10))
  10 t)

(deftest decode-utf16be.4
  (let ((x (make-state-utf16be)))
    (decode-utf16be x #xAA)
    (decode-utf16be x 0))
  #xAA00 t)

(deftest decode-utf16be.5
  (let ((x (make-state-utf16be)))
    (decode-utf16be x #x12)
    (decode-utf16be x #x34))
  #x1234 t)

(deftest decode-utf16be.6
  (let ((x (make-state-utf16be)))
    (decode-utf16be x #xD8)
    (decode-utf16be x #x00)
    (decode-utf16be x #xDC)
    (decode-utf16be x #x0A))
  #x01000A t)

(deftest encode-utf16be.1
  (encode-utf16be 0)
  (#x00 #x00) 2)

(deftest encode-utf16be.2
  (encode-utf16be #xAA)
  (#x00 #xAA) 2)

(deftest encode-utf16be.3
  (encode-utf16be #xAABB)
  (#xAA #xBB) 2)

(deftest encode-utf16be.4
  (encode-utf16be #xD800)
  nil 0)

(deftest encode-utf16be.5
  (encode-utf16be #x010000)
  (#xD8 #x00 #xDC #x00) 4)

(deftest length-utf16be.1
  (length-utf16be -1)
  nil)

(deftest length-utf16be.2
  (length-utf16be 0)
  2)

(deftest length-utf16be.3
  (length-utf16be #xD800)
  nil)

(deftest length-utf16be.4
  (length-utf16be #x010000)
  4)


;;
;;  UTF-32
;;
(deftest length-utf32.1
  (length-utf32 -1)
  nil)

(deftest length-utf32.2
  (length-utf32 0)
  1)

(deftest length-utf32.3
  (length-utf32 #xD800)
  nil)

(deftest length-utf32.4
  (length-utf32 #x010000)
  1)


;;
;;  UTF-32LE
;;
(deftest make-state-utf32le.1
  (typep (make-state-utf32le) 'state-utf32le)
  t)

(deftest state-utf32le-p.1
  (state-utf32le-p (make-state-utf32le))
  t)

(deftest state-utf32le-p.2
  (state-utf32le-p 100)
  nil)

(deftest decode-utf32le.1
  (let ((x (make-state-utf32le)))
    (decode-utf32le x 0))
  nil nil)

(deftest decode-utf32le.2
  (let ((x (make-state-utf32le)))
    (decode-utf32le x 0)
    (decode-utf32le x 0)
    (decode-utf32le x 0)
    (decode-utf32le x 0))
  0 t)

(deftest decode-utf32le.3
  (let ((x (make-state-utf32le)))
    (decode-utf32le x 10)
    (decode-utf32le x 0)
    (decode-utf32le x 0)
    (decode-utf32le x 0))
  10 t)

(deftest decode-utf32le.4
  (let ((x (make-state-utf32le)))
    (decode-utf32le x 0)
    (decode-utf32le x #xAA)
    (decode-utf32le x 0)
    (decode-utf32le x 0))
  #xAA00 t)

(deftest decode-utf32le.5
  (let ((x (make-state-utf32le)))
    (decode-utf32le x #x12)
    (decode-utf32le x #x34)
    (decode-utf32le x 0)
    (decode-utf32le x 0))
  #x3412 t)

(deftest decode-utf32le.6
  (let ((x (make-state-utf32le)))
    (decode-utf32le x #x45)
    (decode-utf32le x #x23)
    (decode-utf32le x #x01)
    (decode-utf32le x #x00))
  #x00012345 t)

(deftest encode-utf32le.1
  (encode-utf32le 0)
  (#x00 #x00 #x00 #x00) 4)

(deftest encode-utf32le.2
  (encode-utf32le #xAA)
  (#xAA #x00 #x00 #x00) 4)

(deftest encode-utf32le.3
  (encode-utf32le #x0001AABB)
  (#xBB #xAA #x01 #x00) 4)

(deftest encode-utf32le.4
  (encode-utf32le #xD800)
  nil 0)

(deftest length-utf32le.1
  (length-utf32le -1)
  nil)

(deftest length-utf32le.2
  (length-utf32le 0)
  4)

(deftest length-utf32le.3
  (length-utf32le #xD800)
  nil)

(deftest length-utf32le.4
  (length-utf32le #x010000)
  4)


;;
;;  UTF-32BE
;;
(deftest make-state-utf32be.1
  (typep (make-state-utf32be) 'state-utf32be)
  t)

(deftest state-utf32be-p.1
  (state-utf32be-p (make-state-utf32be))
  t)

(deftest state-utf32be-p.2
  (state-utf32be-p 100)
  nil)

(deftest decode-utf32be.1
  (let ((x (make-state-utf32be)))
    (decode-utf32be x 0))
  nil nil)

(deftest decode-utf32be.2
  (let ((x (make-state-utf32be)))
    (decode-utf32be x 0)
    (decode-utf32be x 0)
    (decode-utf32be x 0)
    (decode-utf32be x 0))
  0 t)

(deftest decode-utf32be.3
  (let ((x (make-state-utf32be)))
    (decode-utf32be x 0)
    (decode-utf32be x 0)
    (decode-utf32be x 0)
    (decode-utf32be x 10))
  10 t)

(deftest decode-utf32be.4
  (let ((x (make-state-utf32be)))
    (decode-utf32be x 0)
    (decode-utf32be x 0)
    (decode-utf32be x #xAA)
    (decode-utf32be x 0))
  #xAA00 t)

(deftest decode-utf32be.5
  (let ((x (make-state-utf32be)))
    (decode-utf32be x 0)
    (decode-utf32be x 0)
    (decode-utf32be x #x34)
    (decode-utf32be x #x12))
  #x3412 t)

(deftest decode-utf32be.6
  (let ((x (make-state-utf32be)))
    (decode-utf32be x #x00)
    (decode-utf32be x #x01)
    (decode-utf32be x #x23)
    (decode-utf32be x #x45))
  #x00012345 t)

(deftest encode-utf32be.1
  (encode-utf32be 0)
  (#x00 #x00 #x00 #x00) 4)

(deftest encode-utf32be.2
  (encode-utf32be #xAA)
  (#x00 #x00 #x00 #xAA) 4)

(deftest encode-utf32be.3
  (encode-utf32be #x0001AABB)
  (#x00 #x01 #xAA #xBB) 4)

(deftest encode-utf32be.4
  (encode-utf32be #xD800)
  nil 0)

(deftest length-utf32be.1
  (length-utf32be -1)
  nil)

(deftest length-utf32be.2
  (length-utf32be 0)
  4)

(deftest length-utf32be.3
  (length-utf32be #xD800)
  nil)

(deftest length-utf32be.4
  (length-utf32be #x010000)
  4)


;;
;;  do-tests
;;
(do-tests)

