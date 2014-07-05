;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; macros.lisp
;;;; OpenCV bindings for SBCL
;;;; Macros for CFFI functions
(in-package :lisp-cv)



;; DEFPARAMETER macro (to make REPL testing easier)

(defmacro d (var val)
  `(defparameter ,var ,val))


;; FORMAT macro (to make debugging easier)
(defmacro f (&optional (val "test"))
  `(if ,val
       (format t "~%~a~%~%" ,val) nil))


;;SIZEOF macro

;;All non CFFI type sizes are the same for now.

(defmacro size-of (val)
  (if
   (or (equal val :char)               ; = 1
       (equal val :unsigned-char)      ; = 1
       (equal val :short)              ; = 2 
       (equal val :unsigned-short)     ; = 2
       (equal val :int)                ; = 4 
       (equal val :unsigned-int)       ; = 4
       (equal val :long)               ; = 8 
       (equal val :unsigned-long)      ; = 8
       (equal val :long-long)          ; = 8
       (equal val :unsigned-long-long) ; = 8
       ; For convenience, the below types are provided as shortcuts 
       ; for unsigned-char, unsigned-short, unsigned-int, unsigned-long, 
       ; long-long and unsigned-long-long, respectively. 
       (equal val :uchar)              ; = 1 
       (equal val :ushort)             ; = 2
       (equal val :uint)               ; = 4 
       (equal val :ulong)              ; = 8
       (equal val :llong)              ; = 8
       (equal val :ullong)             ; = 8
       ; Foreign integer types of specific sizes, corresponding 
       ; to the C types defined in stdint.h.
       (equal val :int8)               ; = 1 
       (equal val :uint8)              ; = 1
       (equal val :int16)              ; = 2 
       (equal val :uint16)             ; = 2 
       (equal val :int32)              ; = 4 
       (equal val :uint32)             ; = 4
       (equal val :int64)              ; = 8 
       (equal val :uint64)             ; = 8
       ; On all systems, the :float and :double types represent a C float 
       ; and double, respectively. On most but not all systems, :float and 
       ; :double represent a Lisp single-float and double-float, respectively. 
       ; It is not so useful to consider the relationship between Lisp types 
       ; and C types as isomorphic, as simply to recognize the relationship, 
       ; and relative precision, among each respective category. 
       (equal val :float)              ; = 4 
       (equal val :double)             ; = 8
       (equal val :long-double)        ; = 16
       (equal val :pointer)            ; = 8 
       (equal val '*string)            ; = 8 
       (equal val 'rect)               ; = 8 
       (equal val 'mat)                ; = 8 
       (equal val 'rect)               ; = 8 
       (equal val 'size)               ; = 8 
       (equal val 'mat-expr)           ; = 8 
       (equal val 'scalar)             ; = 8 
       (equal val 'dmatch)             ; = 8 
       (equal val 'key-point)          ; = 8 
       (equal val 'point)              ; = 8 
       (equal val 'point-2d)           ; = 8 
       (equal val 'point-2f)           ; = 8 
       (equal val 'point-3d)           ; = 8 
       (equal val 'point-3f)           ; = 8 
       (equal val 'point-3i)           ; = 8 
       (equal val 'feature-2d)         ; = 8 
       (equal val 'vector-char)        ; = 8 
       (equal val 'vector-double)      ; = 8 
       (equal val 'vector-dmatch)      ; = 8 
       (equal val 'vector-int)         ; = 8 
       (equal val 'vector-key-point)   ; = 8 
       (equal val 'vector-mat)         ; = 8 
       (equal val 'vector-point)       ; = 8 
       (equal val 'vector-point-2f)    ; = 8 
       (equal val 'vector-rect)        ; = 8 
       (equal val 'vector-uchar)       ; = 8 
       (equal val 'video-capture)      ; = 8 
       (equal val 'video-writer)       ; = 8 
       (equal val 'mouse-callback)     ; = 8 
       (equal val 'trackbar-callback)  ; = 8 
       (equal val 'svm)                ; = 8 
       (equal val 'svm-params)         ; = 8 
       (equal val 'rng)                ; = 8 
       (equal val 'rotated-rect)       ; = 8 
       (equal val 'term-criteria)      ; = 8 
       (equal val 'size-2f)            ; = 8 
       (equal val 'cascade-classifier)); = 8 

   `(foreign-type-size ,val) 

   `(foreign-type-size ,val)))



;; TIME Macro used to time your functions - see examples.lisp for usage

  (defmacro $ (form &optional (count-form 1000000)) `(time (dotimes (_ ,count-form) ((lambda () ,form)))))



;; FOREIGN-ALLOC macro

(defmacro alloc (&optional type value)
       (cond ((listp value)
	      `(foreign-alloc ,type ,:initial-contents ,value))
	     (t `(foreign-alloc ,type ,:initial-element ,value))))


;; FOREIGN-FREE macro


(defmacro free (ptr)
  `(foreign-free ,ptr))


;; MEM-AREF macro


(defun resolve-pointer (ptr)
  (if (pointerp ptr) ptr (c-pointer ptr)))

(defmacro ? (ptr type &optional (index 0))
  `(mem-aref (resolve-pointer ,ptr) ,type ,index))


;; RUN

;; Macro to build and run executable(not implemented all the way yet)

;; To use place the ~/quicklisp/dists/quicklisp/software/lisp-cv-master/extras/lisp-executable-example.asd 
;; file in the ~/quicklisp/dists/quicklisp/software/lisp-executable-20140113-git/example directory.  
;; This directory was installed when you built LISP-CV. Then take open the main.lisp file in that directory 
;; and edit it with your LISP-CV code. The main.lisp file must stay in that directory. An example main.lisp
;; file that shows the camera feed in a window is in the <lisp-cv-source-dir>/extras folder. Edit it to your 
;; liking and run: (RUN) at the REPL. The macro (RUN) will build the executable. It will be placed it in the
;; ~/quicklisp/dists/quicklisp/software/lisp-executable-20140113-git/example directory. Then it will run it.
;; Building an executable from your LISP-CV code will will knock about 33% off the run time.  

(defmacro run ()
(asdf:oos 'lisp-executable:create-executables-op "lisp-executable-example")
(run-program "~/quicklisp/dists/quicklisp/software/lisp-executable-20140113-git/example/example-program"))

