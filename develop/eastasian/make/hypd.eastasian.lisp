(defpackage make-eastasian-lisp (:use cl))
(in-package make-eastasian-lisp)

(declaim (optimize safety))
(defconstant +width+ #p"EastAsianWidth.txt")
(defconstant +header+ #p"eastasian.h")
(defconstant +source-c+ #p"source.c")
(defconstant +output-c+ #p"eastasian.c")
(defconstant +source-lisp+ #p"source.lisp")
(defconstant +output-lisp+ #p"eastasian.lisp")

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
;;  C language
;;
(defun clang-structure ()
  (format t "enum EastAsianType {~%")
  (format t "~4TEastAsian_error,~%")
  (dobind (a . b) *width-symbol*
    (declare (ignore b))
    (format t "~4TEastAsian_~:(~A~),~%" a))
  (format t "~4TEastAsian_Size~%};~2%")
  (format t "struct eastasian_struct {~%")
  (format t "~4Tunicode a, b;~%")
  (format t "~4Tenum EastAsianType c;~%")
  (format t "};~%"))

(defun clang-header-table ()
  (format t "extern unsigned EastAsianSymbol[EastAsian_Size];~%")
  (format t "extern const enum EastAsianType EastAsianAscii[0x80];~%")
  (format t "extern const struct eastasian_struct ")
  (format t "EastAsianTable[EastAsianTable_Size];~%")
  (format t "enum EastAsianType eastasian_symbol(unicode c);~%")
  (format t "unsigned eastasian_width(unicode c);~%")
  (format t "void init_eastasian(void);~2%"))

(defun clang-output-list ()
  (mapbind (x y z) *width-table*
    (format nil "~4T{  0x~6,'0X,  0x~6,'0X,  EastAsian_~:(~A~)~43T}" x y z)))

(defun clang-output-table ()
  (format t "unsigned EastAsianSymbol[EastAsian_Size] = {~%")
  (format t "~4T0, 1, 2, 1, 2, 2, 1~%")
  (format t "};~2%")
  (format t "const struct eastasian_struct EastAsianTable[EastAsianTable_Size] = ")
  (format t "{~%~{~A~^,~%~}~%};~%" (clang-output-list)))

(defun clang-output-group ()
  (mapfn (x) (group *width-ascii* 4)
    (format nil "~{EastAsian_~:(~A~)~^, ~}" x)))

(defun clang-output-ascii ()
  (format t "const enum EastAsianType EastAsianAscii[0x80] = ")
  (format t "{~%~{~4T~A~^,~%~}~%};~%" (clang-output-group)))

(defparameter +clang-header-comment+
  '("/*  Auto generated by hypd.eastasian.lisp  */"))
(defun clang-header-comment ()
  (dolist (x +clang-header-comment+)
    (format t "~A~%" x)))

(defparameter +clang-source-comment+
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
(defun clang-output-comment ()
  (dolist (x +clang-source-comment+)
    (format t "~A~%" x)))

(defun clang-output-include ()
  (format t "#include \"eastasian.h\"~%")
  (format t "#include <stddef.h>~2%"))

(defun clang-header ()
  (with-overwrite-file (*standard-output* +header+)
    (clang-header-comment)
    (format t "#ifndef __EASTASIAN_UNICODE_HEADER__~%")
    (format t "#define __EASTASIAN_UNICODE_HEADER__~2%")
    (format t "#include \"unicode.h\"~2%")
    (format t "#define EastAsianTable_Size ~A~2%" (length *width-table*))
    (clang-structure)
    (terpri)
    (clang-header-table)
    (format t "#endif~2%")))

(defun clang-output-source ()
  (terpri)
  (format t "/*~%")
  (format t " *  Source~%")
  (format t " */~%")
  (dolist (x (read-list +source-c+))
    (write-line x)))

(defun clang-output ()
  (with-overwrite-file (*standard-output* +output-c+)
    (clang-output-comment)
    (clang-output-include)
    (clang-output-table)
    (terpri)
    (clang-output-ascii)
    (terpri)
    (clang-output-source)
    (fresh-line)))

(defun clang-write ()
  (clang-header)
  (clang-output))


;;
;;  Common Lisp
;;
(defparameter +common-source-comment+
  '(";;  Auto generated by hypd.eastasian.lisp"
    ";;"
    ";;  Unicode Consortium"
    ";;    http://www.unicode.org/"
    ";;"
    ";;  Unicode Copyright and Terms of Use"
    ";;    http://www.unicode.org/copyright.html"
    ";;    http://www.unicode.org/license.html"
    ";;"
    ";;  East Asian Width"
    ";;    http://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt"
    ";;"))
(defun common-output-comment ()
  (dolist (x +common-source-comment+)
    (format t "~A~%" x)))

(defun common-output-package ()
  (format t "(defpackage #:eastasian~%")
  (format t "  (:use #:common-lisp)~%")
  (format t "  (:export")
  (dolist (x (list* '*eastasian-symbol*
                    'eastasian-symbol
                    'eastasian-width
                    (mapcar
                      (lambda (y)
                        (format nil "eastasian-~A" (car y)))
                      *width-symbol*)))
    (format t "~%    #:~(~A~)" x))
  (format t "))~%")
  (format t "(in-package #:eastasian)~2%"))

(defun common-output-length ()
  (format t "(defparameter *eastasian-size* ~A)~2%" (length *width-table*)))

(defun common-output-list ()
  (mapbind (x y z) *width-table*
    (format nil "(  #x~6,'0X   #x~6,'0X   eastasian-~(~A~)~43T)" x y z)))

(defun common-output-table ()
  ;;  symbol
  (format t "(defparameter *eastasian-symbol*~%")
  (format t "  '(")
  (let ((first t))
    (dobind (x . y) *width-symbol*
      (if first
        (format t "(eastasian-~(~A~) . ~A)" x y)
        (format t "~%~4T(eastasian-~(~A~) . ~A)" x y))
      (setq first nil)))
  (format t "))~2%")
  ;;  vector
  (format t "(defparameter *eastasian-vector*~%")
  (format t "  #(")
  (let ((first t))
    (dolist (x (common-output-list))
      (if first
        (format t "~A" x)
        (format t "~%~4T~A" x))
      (setq first nil)))
  (format t "))~%"))

(defun common-output-group ()
  (mapfn (x) (group *width-ascii* 4)
    (format nil "~{eastasian-~(~A~)~^  ~}" x)))

(defun common-output-ascii ()
  (format t "(defparameter *eastasian-ascii*~%")
  (format t "  #(")
  (let ((first t))
    (dolist (x (common-output-group))
      (if first
        (format t "~A" x)
        (format t "~%~4T~A" x))
      (setq first nil)))
  (format t "))~%"))

(defun common-output-source ()
  (terpri)
  (format t ";;~%")
  (format t ";;  Source~%")
  (format t ";;~%")
  (dolist (x (read-list +source-lisp+))
    (write-line x)))

(defun common-write ()
  (with-overwrite-file (*standard-output* +output-lisp+)
    (common-output-comment)
    (common-output-package)
    (common-output-length)
    (common-output-table)
    (terpri)
    (common-output-ascii)
    (terpri)
    (common-output-source)
    (fresh-line)))


;;
;;  main
;;
(defun main ()
  (read-width)
  (clang-write)
  (common-write))
(main)

