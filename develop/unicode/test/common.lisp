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
;;  do-tests
;;
(do-tests)

