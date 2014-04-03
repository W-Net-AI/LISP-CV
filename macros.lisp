;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; macros.lisp
;;;; OpenCV bindings for SBCL
;;;; Macros for CFFI functions
(in-package :lisp-cv)


;; DEFPARAMETER macro (to make REPL testing easier)

(defmacro d (var val)
  `(defparameter ,var ,val))


;;sizeof macro

(defmacro size-of (val)
  (if
   (or (equal val :char)               ; = 1 - same as C
       (equal val :unsigned-char)      ; = 1 - same as C
       (equal val :short)              ; = 2 - same as C 
       (equal val :unsigned-short)     ; = 2 - same as C
       (equal val :int)                ; = 4 - same as C 
       (equal val :unsigned-int)       ; = 4 - same as C
       (equal val :long)               ; = 8 - same as C 
       (equal val :unsigned-long)      ; = 8 - same as C
       (equal val :long-long)          ; = 8 - same as C
       (equal val :unsigned-long-long) ; = 8 - same as C
       ; For convenience, the below types are provided as shortcuts 
       ; for unsigned-char, unsigned-short, unsigned-int, unsigned-long, 
       ; long-long and unsigned-long-long, respectively. 
       (equal val :uchar)              ; = 1 - same as C 
       (equal val :ushort)             ; = 2 - same as C
       (equal val :uint)               ; = 4 - same as C 
       (equal val :ulong)              ; = 8 - same as C
       (equal val :llong)              ; = 8 - same as C
       (equal val :ullong)             ; = 8 - same as C
       ; Foreign integer types of specific sizes, corresponding 
       ; to the C types defined in stdint.h.
       (equal val :int8)               ; = 1 - same as C 
       (equal val :uint8)              ; = 1 - same as C
       (equal val :int16)              ; = 2 - same as C 
       (equal val :uint16)             ; = 2 - same as C 
       (equal val :int32)              ; = 4 - same as C 
       (equal val :uint32)             ; = 4 - same as C
       (equal val :int64)              ; = 8 - same as C 
       (equal val :uint64)             ; = 8 - same as C
       ; On all systems, the :float and :double types represent a C float 
       ; and double, respectively. On most but not all systems, :float and 
       ; :double represent a Lisp single-float and double-float, respectively. 
       ; It is not so useful to consider the relationship between Lisp types 
       ; and C types as isomorphic, as simply to recognize the relationship, 
       ; and relative precision, among each respective category. 
       (equal val :float)              ; = 4 - same as C 
       (equal val :double)             ; = 8 - same as C
       (equal val :long-double)        ; = 16 - same as C
       (equal val :pointer))           ; = 8 - Gets error in C - okay in Lisp

   `(foreign-type-size ,val) 

   `(foreign-type-size '(:struct ,val))))



;; Time Macro used to time your functions - see examples.lisp for usage

  (defmacro $ (form &optional (count-form 1000000)) `(time (dotimes (_ ,count-form) ((lambda () ,form)))))



;; Macro for FOREIGN-ALLOC 

(defmacro alloc (&optional type value)
       (cond ((listp value)
	      `(foreign-alloc ,type ,:initial-contents ,value))
	     (t `(foreign-alloc ,type ,:initial-element ,value))))
