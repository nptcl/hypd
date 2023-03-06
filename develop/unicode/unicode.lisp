;;
;;  unicode.lisp
;;
(defpackage #:unicode
  (:use #:common-lisp)
  (:export
    #:*unicode-count*
    ;;  UTF-8
    #:state-utf8
    #:state-utf8-p
    #:make-state-utf8
    #:init-utf8
    #:decode-utf8
    #:encode-utf8
    #:length-utf8
    ;;  UTF-16
    #:state-utf16
    #:state-utf16-p
    #:make-state-utf16
    #:init-utf16
    #:decode-utf16
    #:encode-utf16
    #:length-utf16
    ))

(in-package #:unicode)

(defparameter *unicode-count* #x110000)


;;
;;  UTF-8
;;
(defstruct state-utf8
  state error (value 0 :type integer))

(defun init-utf8 (x)
  (declare (type state-utf8 x))
  (setf (state-utf8-state x) nil)
  (setf (state-utf8-error x) nil)
  (setf (state-utf8-value x) 0)
  (values))

(defun decode-utf8 (x c)
  (declare (type state-utf8 x)
           (type (unsigned-byte 8) c))
  (prog (v)
    (case (state-utf8-state x)
      ((nil) (go sequence0))
      (sequence2-1 (go sequence2-1))
      (sequence3-1 (go sequence3-1))
      (sequence3-2 (go sequence3-2))
      (sequence4-1 (go sequence4-1))
      (sequence4-2 (go sequence4-2))
      (sequence4-3 (go sequence4-3))
      (error (go error-result))
      (t (go error-switch)))

    sequence0
    (when (<= c #x7F)
      (go sequence1))
    (when (<= #xC2 c #xDF)
      (go sequence2))
    (when (<= #xE0 c #xEF)
      (go sequence3))
    (when (<= #xF0 c #xF4)
      (go sequence4))
    (go error-unicode)

    sequence1
    (return (values c t))

    sequence2
    (setf (state-utf8-value x) (ash (logand #x1F c) 6))
    (setf (state-utf8-state x) 'sequence2-1)
    (return (values nil nil))

    sequence2-1
    (unless (<= #x80 c #xBF)
      (go error-unicode))
    (setf (state-utf8-state x) nil)
    (setq v (logior (state-utf8-value x) (logand #x3F c)))
    (return (values v t))

    sequence3
    (setf (state-utf8-value x) (ash (logand #x0F c) 12))
    (setf (state-utf8-state x) 'sequence3-1)
    (return (values nil nil))

    sequence3-1
    (unless (<= #x80 c #xBF)
      (go error-unicode))
    (setq v (ash (logand #x3F c) 6))
    (setf (state-utf8-value x) (logior (state-utf8-value x) v))
    (setf (state-utf8-state x) 'sequence3-2)
    (return (values nil nil))

    sequence3-2
    (unless (<= #x80 c #xBF)
      (go error-unicode))
    (setq v (logior (state-utf8-value x) (logand #x3F c)))
    (when (< v #x0800)
      (go error-range))
    (when (<= #xD800 v #xDFFF)
      (go error-surrogate))
    (setf (state-utf8-state x) nil)
    (return (values v t))

    sequence4
    (setf (state-utf8-value x) (ash (logand #x07 c) 18))
    (setf (state-utf8-state x) 'sequence4-1)
    (return (values nil nil))

    sequence4-1
    (unless (<= #x80 c #xBF)
      (go error-unicode))
    (setq v (ash (logand #x3F c) 12))
    (setf (state-utf8-value x) (logior (state-utf8-value x) v))
    (setf (state-utf8-state x) 'sequence4-2)
    (return (values nil nil))

    sequence4-2
    (unless (<= #x80 c #xBF)
      (go error-unicode))
    (setq v (ash (logand #x3F c) 6))
    (setf (state-utf8-value x) (logior (state-utf8-value x) v))
    (setf (state-utf8-state x) 'sequence4-3)
    (return (values nil nil))

    sequence4-3
    (unless (<= #x80 c #xBF)
      (go error-unicode))
    (setq v (logior (state-utf8-value x) (logand #x3F c)))
    (when (< v #x010000)
      (go error-range))
    (when (<= #x110000 v)
      (go error-range))
    (setf (state-utf8-state x) nil)
    (return (values v t))

    error-switch
    (setf (state-utf8-error x) 'switch)
    (go error-result)

    error-unicode
    (setf (state-utf8-error x) 'unicode)
    (go error-result)

    error-range
    (setf (state-utf8-error x) 'range)
    (go error-result)

    error-surrogate
    (setf (state-utf8-error x) 'surrogate)
    (go error-result)

    error-result
    (setf (state-utf8-state x) 'error)
    (return (values nil t))))

(defun encode-utf8-values (c)
  (prog nil
    (when (< c 0)
      (go error-encode))
    (when (< c #x80)
      (return (values 1 c)))
    (when (< c #x0800)
      (return (values 2
                      (logior #xC2 (ash c -6))
                      (logior #x80 (logand #x3F c)))))
    (when (< c #xD800)
      (go sequence3))
    (when (< c #xE000)
      (go error-encode))
    (when (< c #x010000)
      (go sequence3))
    (when (< c #x110000)
      (go sequence4))

    error-encode
    (return nil)

    sequence3
    (return (values 3
                    (logior #xE0 (ash c -12))
                    (logior #x80 (logand #x3F (ash c -6)))
                    (logior #x80 (logand #x3F c))))

    sequence4
    (return (values 4
                    (logior #xF0 (ash c -18))
                    (logior #x80 (logand #x3F (ash c -12)))
                    (logior #x80 (logand #x3F (ash c -6)))
                    (logior #x80 (logand #x3F c))))))

(defun encode-utf8 (c &optional array)
  (multiple-value-bind (n x y z w) (encode-utf8-values c)
    (cond ((null n) (values nil 0))
          (array
            (when w (setf (elt array 3) w))
            (when z (setf (elt array 2) z))
            (when y (setf (elt array 1) y))
            (setf (elt array 0) x)
            (values array n))
          (t
            (when w (setq array (cons w nil)))
            (when z (setq array (cons z array)))
            (when y (setq array (cons y array)))
            (setq array (cons x array))
            (values array n)))))

(defun length-utf8 (c)
  (cond ((< c 0) nil)
        ((< c #x80) 1)
        ((< c #x0800) 2)
        ((< c #xD800) 3)
        ((< c #xE000) nil)
        ((< c #x010000) 3)
        ((< c #x110000) 4)
        (t nil)))


;;
;;  UTF-16
;;
(defstruct state-utf16
  state error (value 0 :type integer))

(defun init-utf16 (x)
  (declare (type state-utf16 x))
  (setf (state-utf16-state x) nil)
  (setf (state-utf16-error x) nil)
  (setf (state-utf16-value x) 0)
  (values))

(defun decode-utf16 (x c)
  (declare (type state-utf16 x)
           (type (unsigned-byte 16) c))
  (prog (v)
    (case (state-utf16-state x)
      ((nil) (go sequence0))
      (sequence1 (go sequence1))
      (error (go error-result))
      (t (go error-switch)))

    sequence0
    (when (<= #xD800 c #xDBFF)
      (setf (state-utf16-value x) c)
      (setf (state-utf16-state x) 'sequence1)
      (return (values nil nil)))
    (when (<= #xDC00 c #xDFFF)
      (go error-first))
    (setf (state-utf16-state x) nil)
    (return (values c t))

    sequence1
    (unless (<= #xDC00 c #xDFFF)
      (go error-second))
    (let* ((a1 (state-utf16-value x))
           (a2 (ash (1+ (logand (ash a1 -6) #x0F)) 16))
           (a3 (ash (logand a1 #x3F) 10))
           (a4 (logand c #x03FF))
           (or1 (logior a2 a3))
           (or2 (logior or1 a4)))
      (setf (state-utf16-state x) nil)
      (return (values or2 t)))

    error-switch
    (setf (state-utf16-error x) 'switch)
    (go error-result)

    error-first
    (setf (state-utf16-error x) 'first)
    (go error-result)

    error-second
    (setf (state-utf16-error x) 'second)
    (go error-result)

    error-result
    (setf (state-utf16-state x) 'error)
    (return (values nil t))))

(defun encode-utf16-values (c)
  (prog nil
    (when (< c 0)
      (go error-encode))
    (when (< c #xD800)
      (return (values 1 c)))
    (when (< c #xE000)
      (go error-encode))
    (when (< c #x010000)
      (return (values 1 c)))
    (when (< c #x110000)
      (let* ((v1 (logand (ash c -16) #x1F))
             (v2 (logand (ash c -10) #x3F))
             (v3 (ash (1- v1) 6))
             (v4 (logior v2 v3))
             (x (logior #xD800 v4))
             (y (logior #xDC00 (logand #x03FF c))))
        (return (values 2 x y))))

    error-encode
    (return nil)))

(defun encode-utf16 (c &optional array)
  (multiple-value-bind (n x y) (encode-utf16-values c)
    (cond ((null n) (values nil 0))
          (array
            (when y (setf (elt array 1) y))
            (setf (elt array 0) x)
            (values array n))
          (t
            (when y (setq array (cons y array)))
            (setq array (cons x array))
            (values array n)))))

(defun length-utf16 (c)
  (cond ((< c 0) nil)
        ((< c #xD800) 1)
        ((< c #xE000) nil)
        ((< c #x010000) 1)
        ((< c #x110000) 2)
        (t nil)))

