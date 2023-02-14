(defpackage make-eastasian-lisp (:use cl))
(in-package make-eastasian-lisp)

(declaim (optimize safety))
(defconstant +width+ #p"EastAsianWidth.txt")
(defconstant +header+ #p"eastasian.h")
(defconstant +source+ #p"source.c")
(defconstant +output+ #p"eastasian.c")

;;
;;  make
;;
(defmacro whenbind (bind expr &body body)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (when ,g
         (destructuring-bind ,bind ,g
           ,@body)))))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (expr &body body)
  `(aif ,expr (progn ,@body)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                  (if (consp rest)
                    (rec rest (cons (subseq source 0 n) acc))
                    (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defmacro mapfn ((var) expr &body body)
  `(mapcar
     (lambda (,var) ,@body)
     ,expr))

(defmacro with-overwrite-file
  ((stream file &rest rest
           &key (direction :output)
           (if-exists :supersede)
           (if-does-not-exist :create)
           &allow-other-keys) &body body)
  `(with-open-file (,stream ,file :direction ,direction
                            :if-exists ,if-exists
                            :if-does-not-exist ,if-does-not-exist
                            ,@rest)
     ,@body))

(defun read-list-stream (stream)
  (do ((str (read-line stream nil nil)
            (read-line stream nil nil))
       list)
    ((null str)
     (nreverse list))
    (push str list)))

(defun read-list-pathname (name)
  (with-open-file (input name)
    (read-list-stream input)))

(defun read-list (stream)
  (etypecase stream
    (stream (read-list-stream stream))
    (pathname (read-list-pathname stream))
    (string (read-list-pathname (pathname stream)))))

(defmacro dobind (bind expr &body body)
  (let ((g (gensym)))
    `(dolist (,g ,expr)
       (destructuring-bind ,bind ,g
         ,@body))))

(defmacro mapbind (bind expr &body body)
  (let ((g (gensym)))
    `(mapcar
       (lambda (,g)
         (destructuring-bind ,bind ,g
           ,@body))
       ,expr)))

(defvar *parse-radix* 16)
(defun parseint (x &key null)
  (when (or x (null null))
    (parse-integer x :radix *parse-radix*)))


;;
;;  optimize
;;
(defun extract-loop (x call)
  (multiple-value-bind (ret check) (funcall call x)
    (if (null check)
      (values x nil)
      (values (extract-loop ret call) t))))

(defun extract-calls (x calls &optional update)
  (if (null calls)
    (values x update)
    (destructuring-bind (call . next) calls
      (multiple-value-bind (ret check) (extract-loop x call)
        (if (null check)
          (extract-calls x next update)
          (extract-calls ret next t))))))

(defun extract (x &rest calls)
  (extract-loop
    x (lambda (y) (extract-calls y calls))))

;;  extract-list
(defun extract-call2 (x calls)
  (multiple-value-bind (ret check) (apply #'extract x calls)
    (if check
      (values ret t)
      (when (consp x)
        (destructuring-bind (a . b) x
          (multiple-value-bind (ret check) (extract-call2 b calls)
            (when check
              (values (cons a ret) t))))))))

(defun extract-list (x &rest calls)
  (extract-loop
    x (lambda (y) (extract-call2 y calls))))

(defun optimize-merge (list)
  (when (consp list)
    (destructuring-bind (a . x) list
      (when (consp x)
        (destructuring-bind (b . c) x
          (destructuring-bind (a1 b1 c1) a
            (destructuring-bind (a2 b2 c2) b
              (when (and (eq c1 c2)
                         (eql (1+ b1) a2))
                (values (cons (list a1 b2 c1) c) t)))))))))

(defun optimize-table (list)
  (extract-list list #'optimize-merge))


;;
;;  EastAsianWidth
;;
(defun trim-space (x)
  (string-trim #(#\Space #\Tab) x))

(defun trim-comment (x)
  (aif (position #\# x)
    (subseq x 0 it)
    x))

(defun trim-line (x)
  (trim-space
    (trim-comment x)))

(defun split-search (x y)
  (awhen (search x y)
    (list (trim-space (subseq y 0 it))
          (trim-space (subseq y (+ (length x) it))))))

(defun split-semicolon (x)
  (split-search ";" x))

(defun hexdecimal-char-p (x)
  (digit-char-p x 16))

(defun hexdecimal-string-p (x)
  (every #'hexdecimal-char-p x))

(defun scan-semicolon (x)
  (let ((x (trim-line x)))
    (whenbind (a b) (split-semicolon x)
      (list a b))))

(defun scan-strings-single (x)
  ;; "^([0-9A-F]+);(\\S+)\\s.*$"
  (whenbind (a b) (scan-semicolon x)
    (when (hexdecimal-string-p a)
      (list a b))))

(defun split-dotdot (x)
  (split-search ".." x))

(defun scan-strings-range (x)
  ;; "^([0-9A-F]+)\\.\\.([0-9A-F]+);(\\S+)\\s.*$"
  (whenbind (a b) (scan-semicolon x)
    (whenbind (c d) (split-dotdot a)
      (and (hexdecimal-string-p c)
           (hexdecimal-string-p d)
           (list c d b)))))

(defvar *width-table*)
(defvar *width-symbol*)

(defun read-width-table (&aux list)
  (labels ((p (x y) (parseint (elt x y)))
           (k (x y) (intern (string-upcase (elt x y)))))
    (dolist (x (read-list +width+))
      (aif (scan-strings-single x)
        (push (list (p it 0) (p it 0) (k it 1)) list))
      (aif (scan-strings-range x)
        (push (list (p it 0) (p it 1) (k it 2)) list)))
    (sort list #'< :key #'car)))

;; ASCII
(defvar *width-vector*)
(defvar *width-ascii*)

(defun east-asian<= (a x b)
  (if b
    (<= a x b)
    (= a x)))

(defun east-asian-width-symbol (x table &optional ai bi)
  (let* ((ai (or ai 0))
         (bi (or bi (1- (length table))))
         (ci (floor (+ ai bi) 2))
         (a (elt table ai))
         (b (elt table bi))
         (c1 (car (elt table ci))))
    (destructuring-bind (a1 a2 a3) a
      (destructuring-bind (b1 b2 b3) b
        (cond ((east-asian<= a1 x a2) a3)
              ((east-asian<= b1 x b2) b3)
              ((<= bi ai) nil)
              (t (if (< x c1)
                   (east-asian-width-symbol x table (1+ ai) ci)
                   (east-asian-width-symbol x table ci (1- bi)))))))))

(defun read-width-ascii ()
  (let (list)
    (dotimes (i #x80)
      (push (east-asian-width-symbol i *width-vector*) list))
    (nreverse list)))

(defun read-width ()
  (setq *width-table* (optimize-table (read-width-table)))
  (setq *width-symbol* '((n . 1) (a . 2) (h . 1) (w . 2) (f . 2) (na . 1)))
  (setq *width-vector* (coerce *width-table* 'vector))
  (setq *width-ascii* (read-width-ascii)))


;;
;;  output
;;
(defun write-structure ()
  (format t "enum EastAsianType {~%")
  (format t "~4TEastAsian_error,~%")
  (dobind (a . b) *width-symbol*
    (declare (ignore b))
    (format t "~4TEastAsian_~A,~%" a))
  (format t "~4TEastAsian_Size~%};~2%")
  (format t "struct eastasian_struct {~%")
  (format t "~4Tunicode a, b;~%")
  (format t "~4Tenum EastAsianType c;~%")
  (format t "};~%"))

(defun write-header-table ()
  (format t "extern unsigned EastAsianSymbol[EastAsian_Size];~%")
  (format t "extern const enum EastAsianType EastAsianAscii[0x80];~%")
  (format t "extern const struct eastasian_struct ")
  (format t "EastAsianTable[EastAsianTable_Size];~%")
  (format t "enum EastAsianType eastasian_symbol(unicode c);~%")
  (format t "unsigned eastasian_width(unicode c);~%")
  (format t "void init_eastasian(void);~2%"))

(defun write-output-list ()
  (mapbind (x y z) *width-table*
    (format nil "~4T{  0x~6,'0X,  0x~6,'0X,  EastAsian_~A~43T}" x y z)))

(defun write-output-table ()
  (format t "unsigned EastAsianSymbol[EastAsian_Size] = {~%")
  (format t "~4T0, 1, 2, 1, 2, 2, 1~%")
  (format t "};~2%")
  (format t "const struct eastasian_struct EastAsianTable[EastAsianTable_Size] = ")
  (format t "{~%~{~A~^,~%~}~%};~%" (write-output-list)))

(defun write-output-group ()
  (mapfn (x) (group *width-ascii* 4)
    (format nil "~{EastAsian_~A~^, ~}" x)))

(defun write-output-ascii ()
  (format t "const enum EastAsianType EastAsianAscii[0x80] = ")
  (format t "{~%~{~4T~A~^,~%~}~%};~%" (write-output-group)))


;;
;;  main
;;
(defparameter +header-comment+
  '("/*  Auto generated by hypd.eastasian.lisp  */"))
(defun write-header-comment ()
  (dolist (x +header-comment+)
    (format t "~A~%" x)))

(defparameter +source-comment+
  '("/*  Auto generated by hypd.eastasian.lisp"
    " *"
    " *  Unicode Consortium"
    " *    http://www.unicode.org/"
    " *"
    " *  Unicode Copyright and Terms of Use"
    " *    http://www.unicode.org/copyright.html"
    " *    http://www.unicode.org/license.html"
    " *"
    " *  East Asian Width"
    " *    http://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt"
    " */"))
(defun write-output-comment ()
  (dolist (x +source-comment+)
    (format t "~A~%" x)))

(defun write-output-include ()
  (format t "#include \"eastasian.h\"~%")
  (format t "#include <stddef.h>~2%"))

(defun write-header ()
  (with-overwrite-file (*standard-output* +header+)
    (write-header-comment)
    (format t "#ifndef __EASTASIAN_UNICODE_HEADER__~%")
    (format t "#define __EASTASIAN_UNICODE_HEADER__~2%")
    (format t "#include \"unicode.h\"~2%")
    (format t "#define EastAsianTable_Size ~A~2%" (length *width-table*))
    (write-structure)
    (terpri)
    (write-header-table)
    (format t "#endif~2%")))

(defun write-output-source ()
  (terpri)
  (format t "/*~%")
  (format t " *  Source~%")
  (format t " */~%")
  (dolist (x (read-list +source+))
    (write-line x)))

(defun write-output ()
  (with-overwrite-file (*standard-output* +output+)
    (write-output-comment)
    (write-output-include)
    (write-output-table)
    (terpri)
    (write-output-ascii)
    (terpri)
    (write-output-source)
    (fresh-line)))

(defun main ()
  (read-width)
  (write-header)
  (write-output))
(main)

