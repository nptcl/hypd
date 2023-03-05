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
    (return (values c nil))

    sequence2
    (setf (state-utf8-value x) (ash (logand #x1F c) 6))
    (setf (state-utf8-state x) 'sequence2-1)
    (return (values nil t))

    sequence2-1
    (unless (<= #x80 c #xBF)
      (go error-unicode))
    (setf (state-utf8-state x) nil)
    (setq v (logior (state-utf8-value x) (logand #x3F c)))
    (return (values v nil))

    sequence3
    (setf (state-utf8-value x) (ash (logand #x0F c) 12))
    (setf (state-utf8-state x) 'sequence3-1)
    (return (values nil t))

    sequence3-1
    (unless (<= #x80 c #xBF)
      (go error-unicode))
    (setq v (ash (logand #x3F c) 6))
    (setf (state-utf8-value x) (logior (state-utf8-value x) v))
    (setf (state-utf8-state x) 'sequence3-2)
    (return (values nil t))

    sequence3-2
    (unless (<= #x80 c #xBF)
      (go error-unicode))
    (setq v (logior (state-utf8-value x) (logand #x3F c)))
    (when (< v #x0800)
      (go error-range))
    (when (<= #xD800 v #xDFFF)
      (go error-surrogate))
    (setf (state-utf8-state x) nil)
    (return (values v nil))

    sequence4
    (setf (state-utf8-value x) (ash (logand #x07 c) 18))
    (setf (state-utf8-state x) 'sequence4-1)
    (return (values nil t))

    sequence4-1
    (unless (<= #x80 c #xBF)
      (go error-unicode))
    (setq v (ash (logand #x3F c) 12))
    (setf (state-utf8-value x) (logior (state-utf8-value x) v))
    (setf (state-utf8-state x) 'sequence4-2)
    (return (values nil t))

    sequence4-2
    (unless (<= #x80 c #xBF)
      (go error-unicode))
    (setq v (ash (logand #x3F c) 6))
    (setf (state-utf8-value x) (logior (state-utf8-value x) v))
    (setf (state-utf8-state x) 'sequence4-3)
    (return (values nil t))

    sequence4-3
    (unless (<= #x80 c #xBF)
      (go error-unicode))
    (setq v (logior (state-utf8-value x) (logand #x3F c)))
    (when (< v #x010000)
      (go error-range))
    (when (<= #x110000 v)
      (go error-range))
    (setf (state-utf8-state x) nil)
    (return (values v nil))

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
    (return (values nil nil))))

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
  (multiple-value-bind (n a b c d) (encode-utf8-values c)
    (cond ((null n) (values nil 0))
          (array
            (when d (setf (elt array 3) d))
            (when c (setf (elt array 2) c))
            (when b (setf (elt array 1) b))
            (setf (elt array 0) a)
            (values array n))
          (t
            (when d (setq array (cons d nil)))
            (when c (setq array (cons c array)))
            (when b (setq array (cons b array)))
            (setq array (cons a array))
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

