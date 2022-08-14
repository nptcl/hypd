(in-package #:common-lisp-user)

(defpackage hypd-base64
  (:use common-lisp)
  (:export
    #:base64-encode-padding
    #:base64-encode-char-62
    #:base64-encode-char-63
    #:base64-encode-char-padding
    #:base64-encode-p
    #:base64-encode-init
    #:base64-encode-clear
    #:base64-encode-pipe
    #:base64-encode-closing
    #:base64-encode-close

    #:base64-decode-ignore-eol
    #:base64-decode-ignore-others
    #:base64-decode-ignore-padding
    #:base64-decode-char-62
    #:base64-decode-char-63
    #:base64-decode-char-padding
    #:base64-decode-p
    #:base64-decode-init
    #:base64-decode-clear
    #:base64-decode-pipe
    #:base64-decode-close))

(in-package hypd-base64)

;;
;;  base64-encode
;;
(defstruct (base64-encode (:constructor base64-encode-init))
  (state 'read-1)
  (padding t)
  (data 0 :type (integer 0 (64)))
  (char-62 #\+ :type character)
  (char-63 #\/ :type character)
  (char-padding #\= :type character))

(defvar *base64-encode-table*
  (load-time-value
    (vector #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
            #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
            #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
            #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
            #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))

(defun base64-encode-clear (inst)
  (declare (type base64-encode inst))
  (setf (base64-encode-state inst) 'read-1)
  (setf (base64-encode-data inst) 0)
  inst)

(defun base64-encode-value (inst c)
  (declare (type base64-encode inst)
           (type (integer 0 (64)) c))
  (case c
    (62 (base64-encode-char-62 inst))
    (63 (base64-encode-char-63 inst))
    (otherwise (the character (aref *base64-encode-table* c)))))

(defun base64-encode-pipe (inst u)
  (declare (type base64-encode inst)
           (type (integer 0 (256)) u))
  (case (base64-encode-state inst)
    (read-1
      (let* ((v (ash u -2))
             (x (base64-encode-value inst v)))
        (setf (base64-encode-data inst) (logand u #x03))
        (setf (base64-encode-state inst) 'read-2)
        (values x nil)))
    (read-2
      (let* ((a (ash (base64-encode-data inst) 4))
             (b (ash u -4))
             (v (logior a b))
             (x (base64-encode-value inst v)))
        (setf (base64-encode-data inst) (logand u #x0F))
        (setf (base64-encode-state inst) 'read-3)
        (values x nil)))
    (read-3
      (let* ((a (ash (base64-encode-data inst) 2))
             (b (ash u -6))
             (v (logior a b))
             (x (base64-encode-value inst v))
             (y (base64-encode-value inst (logand u #x3F))))
        (setf (base64-encode-data inst) 0)
        (setf (base64-encode-state inst) 'read-1)
        (values x y)))
    (otherwise
      (error "Invalid state, ~S." inst))))

(defun base64-encode-closing (inst)
  (declare (type base64-encode inst))
  (case (base64-encode-state inst)
    ((read-1 padding-3)
     (setf (base64-encode-data inst) 0)
     (setf (base64-encode-state inst) nil)
     nil)
    (read-2
      (let* ((v (ash (base64-encode-data inst) 4))
             (x (base64-encode-value inst v)))
        (setf (base64-encode-data inst) 0)
        (if (base64-encode-padding inst)
          (setf (base64-encode-state inst) 'padding-1)
          (setf (base64-encode-state inst) 'padding-3))
        x))
    (read-3
      (let* ((v (ash (base64-encode-data inst) 2))
             (x (base64-encode-value inst v)))
        (setf (base64-encode-data inst) 0)
        (if (base64-encode-padding inst)
          (setf (base64-encode-state inst) 'padding-2)
          (setf (base64-encode-state inst) 'padding-3))
        x))
    (padding-1
      (setf (base64-encode-state inst) 'padding-2)
      (base64-encode-char-padding inst))
    (padding-2
      (setf (base64-encode-state inst) 'padding-3)
      (base64-encode-char-padding inst))
    (otherwise
      (error "Invalid state, ~S." inst))))

(defun base64-encode-close (inst call)
  (do (v) (nil)
    (setq v (base64-encode-closing inst))
    (if v
      (funcall call v)
      (return nil))))


;;
;;  base64-decode
;;
(defstruct (base64-decode (:constructor base64-decode-init))
  (state 'read-1)
  (ignore-eol t)
  (ignore-others nil)
  (ignore-padding nil)
  (ignore-check t)
  (data 0 :type (integer 0 (64)))
  (char-62 #\+ :type character)
  (char-63 #\/ :type character)
  (char-padding #\= :type character))

(defvar *base64-decode-table*
  (load-time-value
    (vector () () () () () () () ()    () () () () () () () ()
            () () () () () () () ()    () () () () () () () ()
            () () () () () () () ()    () () () () () () () ()
            52 53 54 55 56 57 58 59    60 61 () () () () () ()
            ()  0  1  2  3  4  5  6     7  8  9 10 11 12 13 14
            15 16 17 18 19 20 21 22    23 24 25 () () () () ()
            () 26 27 28 29 30 31 32    33 34 35 36 37 38 39 40
            41 42 43 44 45 46 47 48    49 50 51 () () () () ())))

(defun base64-decode-clear (inst)
  (declare (type base64-decode inst))
  (setf (base64-decode-state inst) 'read-1)
  (setf (base64-decode-data inst) 0)
  inst)

(defun base64-decode-error (inst c)
  (declare (type base64-decode inst))
  (setf (base64-decode-state inst) nil)
  (error "Invalid code, ~S." c))

(defun base64-decode-value (inst u)
  (declare (type base64-decode inst)
           (type (integer 0 (64)) u))
  (case (base64-decode-state inst)
    (read-1
      (setf (base64-decode-data inst) u)
      (setf (base64-decode-state inst) 'read-2)
      nil)
    (read-2
      (let* ((a (ash (base64-decode-data inst) 2))
             (b (ash u -4))
             (r (logior a b))
             (v (logand u #x0F)))
        (setf (base64-decode-data inst) v)
        (setf (base64-decode-state inst) 'read-3)
        r))
    (read-3
      (let* ((a (ash (base64-decode-data inst) 4))
             (b (ash u -2))
             (r (logior a b))
             (v (logand u #x03)))
        (setf (base64-decode-data inst) v)
        (setf (base64-decode-state inst) 'read-4)
        r))
    (read-4
      (let* ((a (ash (base64-decode-data inst) 6))
             (r (logior a u)))
        (setf (base64-decode-data inst) 0)
        (setf (base64-decode-state inst) 'read-1)
        r))
    (otherwise
      (base64-decode-error inst u))))

(defun base64-decode-check-last (inst)
  (declare (type base64-decode inst))
  (and (not (base64-decode-ignore-check inst))
       (not (zerop (base64-decode-data inst)))
       (base64-decode-error inst nil)))

(defun base64-decode-padding (inst)
  (declare (type base64-decode inst))
  (base64-decode-check-last inst)
  (setf (base64-decode-data inst) 0)
  (case (base64-decode-state inst)
    ((read-1 padding-4)
     (setf (base64-decode-state inst) 'padding-1))
    ((read-2 padding-1)
     (setf (base64-decode-state inst) 'padding-2))
    ((read-3 padding-2)
     (setf (base64-decode-state inst) 'padding-3))
    ((read-4 padding-3)
     (setf (base64-decode-state inst) 'padding-4))
    (otherwise
      (base64-decode-error inst 'padding)))
  nil)

(defun base64-decode-check (inst check c)
  (declare (type base64-decode inst))
  (unless check
    (base64-decode-error inst c)))

(defun base64-decode-get (c)
  (declare (type character c))
  (let ((v (char-code c)))
    (when (<= #x00 v #x7F)
      (aref *base64-decode-table* v))))

(defun base64-decode-pipe (inst c)
  (declare (type base64-decode inst)
           (type character c))
  (cond ((eql c (base64-decode-char-62 inst))
         (base64-decode-value inst 62))
        ((eql c (base64-decode-char-63 inst))
         (base64-decode-value inst 63))
        ((eql c (base64-decode-char-padding inst))
         (base64-decode-padding inst))
        ((or (eql c #\newline) (eql c #\return))
         (base64-decode-check inst (base64-decode-ignore-eol inst) c))
        (t (let ((v (base64-decode-get c)))
             (if (minusp v)
               (base64-decode-check inst (base64-decode-ignore-others inst) c)
               (base64-decode-value inst v))))))

(defun base64-decode-close-1 (inst)
  (declare (type base64-decode inst))
  (setf (base64-decode-data inst) 0)
  (case (base64-decode-state inst)
    (read-2
      (setf (base64-decode-state inst) 'padding-2))
    (read-3
      (setf (base64-decode-state inst) 'padding-3))
    (read-4
      (setf (base64-decode-state inst) 'padding-4))
    ((read-1 padding-1 padding-2 padding-3 padding-4)
     (setf (base64-decode-state inst) nil))
    (otherwise
      (base64-decode-error inst 'close))))

(defun base64-decode-close-2 (inst)
  (declare (type base64-decode inst))
  (case (base64-decode-state inst)
    ((read-1 padding-4)
     (setf (base64-decode-data inst) 0)
     (setf (base64-decode-state inst) nil))
    (otherwise
      (base64-decode-error inst 'close))))

(defun base64-decode-close (inst)
  (declare (type base64-decode inst))
  (base64-decode-check-last inst)
  (if (base64-decode-ignore-padding inst)
    (base64-decode-close-1 inst)
    (base64-decode-close-2 inst))
  nil)

