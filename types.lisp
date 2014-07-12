;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; types.lisp
;;;; OpenCV bindings
;;;; Type declarations and Trivial Garbage finalizers for memory 
;;;; management, Finalizers are still in process. If a function 
;;;; has a finalizer you can use it by passing a gc: or a t: 
;;;; before the functions name to activate it.


(in-package :lisp-cv)

;;Shadowed CL functions are being 
;;re-imported here for time being

(defgeneric length (self)
  (:documentation "Used to call the bindings for the C++ vector class 'size' member and CL:LENGTH."))

(defmethod length ((self sequence))
  (cl:length self))

(defun max (&rest args)
       (apply #'cl:max args))

(defun read (&rest args)
       (apply #'cl:read args))


;; NON GARBAGE COLLECTED TYPES


;; LINE-SEGMENT-DETECTOR
(defctype line-segment-detector :pointer)

;; MOUSE-CALLBACK
(defctype mouse-callback :pointer)

;; TRACKBAR-CALLBACK
(defctype trackbar-callback :pointer)



;; C++ INTEROP TYPES.



;; *STRING


(define-foreign-type *string ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser *string))


(defclass std-string ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-string) (c-type *string))
  (values  (c-pointer lisp-value) lisp-value))


    
(defmethod translate-from-foreign (c-pointer (c-type *string))
  (let ((string  (make-instance 'std-string :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize string (lambda () (del-std-string c-pointer))))
    string))



;; VECTOR-CHAR


(define-foreign-type vector-char ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-char))


(defclass std-vector-char ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-char) (c-type vector-char))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-char))
  (let ((vector-char  (make-instance 'std-vector-char :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-char (lambda () (del-vector-char c-pointer))))
    vector-char))



;; VECTOR-DMATCH


(define-foreign-type vector-dmatch ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-dmatch))


(defclass std-vector-dmatch ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-dmatch) (c-type vector-dmatch))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-dmatch))
  (let ((vector-dmatch  (make-instance 'std-vector-dmatch :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-dmatch (lambda () (del-vector-dmatch c-pointer))))
    vector-dmatch))



;; VECTOR-DOUBLE


(define-foreign-type vector-double ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-double))


(defclass std-vector-double ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-double) (c-type vector-double))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-double))
  (let ((vector-double  (make-instance 'std-vector-double :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-double (lambda () (del-vector-double c-pointer))))
    vector-double))



;; VECTOR-FLOAT


(define-foreign-type vector-float ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-float))


(defclass std-vector-float ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-float) (c-type vector-float))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-float))
  (let ((vector-float  (make-instance 'std-vector-float :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-float (lambda () (del-vector-float c-pointer))))
    vector-float))



;; VECTOR-INT


(define-foreign-type vector-int ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-int))


(defclass std-vector-int ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-int) (c-type vector-int))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-int))
  (let ((vector-int  (make-instance 'std-vector-int :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-int (lambda () (del-vector-int c-pointer))))
    vector-int))



;; VECTOR-KEY-POINT


(define-foreign-type vector-key-point ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-key-point))


(defclass std-vector-key-point ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-key-point) (c-type vector-key-point))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-key-point))
  (let ((vector-key-point  (make-instance 'std-vector-key-point :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-key-point (lambda () (del-vector-key-point c-pointer))))
    vector-key-point))



;; VECTOR-MAT


(define-foreign-type vector-mat ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-mat))


(defclass std-vector-mat ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-mat) (c-type vector-mat))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-mat))
  (let ((vector-mat  (make-instance 'std-vector-mat :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-mat (lambda () (del-vector-mat c-pointer))))
    vector-mat))



;; VECTOR-POINT


(define-foreign-type vector-point ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-point))


(defclass std-vector-point ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-point) (c-type vector-point))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-point))
  (let ((vector-point  (make-instance 'std-vector-point :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-point (lambda () (del-vector-point c-pointer))))
    vector-point))



;; VECTOR-POINT-2F


(define-foreign-type vector-point-2f ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-point-2f))


(defclass std-vector-point-2f ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-point-2f) (c-type vector-point-2f))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-point-2f))
  (let ((vector-point-2f  (make-instance 'std-vector-point-2f :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-point-2f (lambda () (del-vector-point-2f c-pointer))))
    vector-point-2f))



;; VECTOR-RECT


(define-foreign-type vector-rect ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-rect))


(defclass std-vector-rect ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-rect) (c-type vector-rect))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-rect))
  (let ((vector-rect  (make-instance 'std-vector-rect :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-rect (lambda () (del-vector-rect c-pointer))))
    vector-rect))



;; VECTOR-UCHAR


(define-foreign-type vector-uchar ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-uchar))


(defclass std-vector-uchar ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-uchar) (c-type vector-uchar))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-uchar))
  (let ((vector-uchar  (make-instance 'std-vector-uchar :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-uchar (lambda () (del-vector-uchar c-pointer))))
    vector-uchar))



;; VECTOR-VEC-2B


(define-foreign-type vector-vec-2b ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-2b))


(defclass std-vector-vec-2b ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-2b) (c-type vector-vec-2b))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-2b))
  (let ((vector-vec-2b (make-instance 'std-vector-vec-2b :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-2b (lambda () (del-vector-vec-2b c-pointer))))
    vector-vec-2b))



;; VECTOR-VEC-3B


(define-foreign-type vector-vec-3b ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-3b))


(defclass std-vector-vec-3b ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-3b) (c-type vector-vec-3b))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-3b))
  (let ((vector-vec-3b  (make-instance 'std-vector-vec-3b :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-3b (lambda () (del-vector-vec-3b c-pointer))))
    vector-vec-3b))



;; VECTOR-VEC-4B


(define-foreign-type vector-vec-4b ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-4b))


(defclass std-vector-vec-4b ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-4b) (c-type vector-vec-4b))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-4b))
  (let ((vector-vec-4b (make-instance 'std-vector-vec-4b :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-4b (lambda () (del-vector-vec-4b c-pointer))))
    vector-vec-4b))




;; VECTOR-VEC-2D


(define-foreign-type vector-vec-2d ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-2d))


(defclass std-vector-vec-2d ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-2d) (c-type vector-vec-2d))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-2d))
  (let ((vector-vec-2d (make-instance 'std-vector-vec-2d :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-2d (lambda () (del-vector-vec-2d c-pointer))))
    vector-vec-2d))



;; VECTOR-VEC-3D


(define-foreign-type vector-vec-3d ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-3d))


(defclass std-vector-vec-3d ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-3d) (c-type vector-vec-3d))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-3d))
  (let ((vector-vec-3d (make-instance 'std-vector-vec-3d :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-3d (lambda () (del-vector-vec-3d c-pointer))))
    vector-vec-3d))



;; VECTOR-VEC-4D


(define-foreign-type vector-vec-4d ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-4d))


(defclass std-vector-vec-4d ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-4d) (c-type vector-vec-4d))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-4d))
  (let ((vector-vec-4d (make-instance 'std-vector-vec-4d :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-4d (lambda () (del-vector-vec-4d c-pointer))))
    vector-vec-4d))



;; VECTOR-VEC-6D


(define-foreign-type vector-vec-6d ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-6d))


(defclass std-vector-vec-6d ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-6d) (c-type vector-vec-6d))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-6d))
  (let ((vector-vec-6d (make-instance 'std-vector-vec-6d :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-6d (lambda () (del-vector-vec-6d c-pointer))))
    vector-vec-6d))



;; VECTOR-VEC-2F


(define-foreign-type vector-vec-2f ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-2f))


(defclass std-vector-vec-2f ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-2f) (c-type vector-vec-2f))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-2f))
  (let ((vector-vec-2f (make-instance 'std-vector-vec-2f :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-2f (lambda () (del-vector-vec-2f c-pointer))))
    vector-vec-2f))



;; VECTOR-VEC-3F


(define-foreign-type vector-vec-3f ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-3f))


(defclass std-vector-vec-3f ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-3f) (c-type vector-vec-3f))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-3f))
  (let ((vector-vec-3f (make-instance 'std-vector-vec-3f :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-3f (lambda () (del-vector-vec-3f c-pointer))))
    vector-vec-3f))



;; VECTOR-VEC-4F


(define-foreign-type vector-vec-4f ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-4f))


(defclass std-vector-vec-4f ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-4f) (c-type vector-vec-4f))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-4f))
  (let ((vector-vec-4f (make-instance 'std-vector-vec-4f :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-4f (lambda () (del-vector-vec-4f c-pointer))))
    vector-vec-4f))



;; VECTOR-VEC-6F


(define-foreign-type vector-vec-6f ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-6f))


(defclass std-vector-vec-6f ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-6f) (c-type vector-vec-6f))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-6f))
  (let ((vector-vec-6f (make-instance 'std-vector-vec-6f :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-6f (lambda () (del-vector-vec-6f c-pointer))))
    vector-vec-6f))



;; VECTOR-VEC-2I


(define-foreign-type vector-vec-2i ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-2i))


(defclass std-vector-vec-2i ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-2i) (c-type vector-vec-2i))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-2i))
  (let ((vector-vec-2i (make-instance 'std-vector-vec-2i :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-2i (lambda () (del-vector-vec-2i c-pointer))))
    vector-vec-2i))



;; VECTOR-VEC-3I


(define-foreign-type vector-vec-3i ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-3i))


(defclass std-vector-vec-3i ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-3i) (c-type vector-vec-3i))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-3i))
  (let ((vector-vec-3i (make-instance 'std-vector-vec-3i :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-3i (lambda () (del-vector-vec-3i c-pointer))))
    vector-vec-3i))



;; VECTOR-VEC-4I


(define-foreign-type vector-vec-4i ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-4i))


(defclass std-vector-vec-4i ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-4i) (c-type vector-vec-4i))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-4i))
  (let ((vector-vec-4i (make-instance 'std-vector-vec-4i :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-4i (lambda () (del-vector-vec-4i c-pointer))))
    vector-vec-4i))



;; VECTOR-VEC-6I


(define-foreign-type vector-vec-6i ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-6i))


(defclass std-vector-vec-6i ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-6i) (c-type vector-vec-6i))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-6i))
  (let ((vector-vec-6i (make-instance 'std-vector-vec-6i :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-6i (lambda () (del-vector-vec-6i c-pointer))))
    vector-vec-6i))



;; VECTOR-VEC-8I


(define-foreign-type vector-vec-8i ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-8i))


(defclass std-vector-vec-8i ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-8i) (c-type vector-vec-8i))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-8i))
  (let ((vector-vec-8i (make-instance 'std-vector-vec-8i :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-8i (lambda () (del-vector-vec-8i c-pointer))))
    vector-vec-8i))



;; VECTOR-VEC-2S


(define-foreign-type vector-vec-2s ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-2s))


(defclass std-vector-vec-2s ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-2s) (c-type vector-vec-2s))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-2s))
  (let ((vector-vec-2s (make-instance 'std-vector-vec-2s :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-2s (lambda () (del-vector-vec-2s c-pointer))))
    vector-vec-2s))



;; VECTOR-VEC-3S


(define-foreign-type vector-vec-3s ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-3s))


(defclass std-vector-vec-3s ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-3s) (c-type vector-vec-3s))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-3s))
  (let ((vector-vec-3s (make-instance 'std-vector-vec-3s :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-3s (lambda () (del-vector-vec-3s c-pointer))))
    vector-vec-3s))



;; VECTOR-VEC-4S


(define-foreign-type vector-vec-4s ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-4s))


(defclass std-vector-vec-4s ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-4s) (c-type vector-vec-4s))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-4s))
  (let ((vector-vec-4s (make-instance 'std-vector-vec-4s :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-4s (lambda () (del-vector-vec-4s c-pointer))))
    vector-vec-4s))



;; VECTOR-VEC-2W


(define-foreign-type vector-vec-2w ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-2w))


(defclass std-vector-vec-2w ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-2w) (c-type vector-vec-2w))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-2w))
  (let ((vector-vec-2w (make-instance 'std-vector-vec-2w :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-2w (lambda () (del-vector-vec-2w c-pointer))))
    vector-vec-2w))



;; VECTOR-VEC-3W


(define-foreign-type vector-vec-3w ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-3w))


(defclass std-vector-vec-3w ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-3w) (c-type vector-vec-3w))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-3w))
  (let ((vector-vec-3w (make-instance 'std-vector-vec-3w :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-3w (lambda () (del-vector-vec-3w c-pointer))))
    vector-vec-3w))



;; VECTOR-VEC-4W


(define-foreign-type vector-vec-4w ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-vec-4w))


(defclass std-vector-vec-4w ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-vec-4w) (c-type vector-vec-4w))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-vec-4w))
  (let ((vector-vec-4w (make-instance 'std-vector-vec-4w :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-vec-4w (lambda () (del-vector-vec-4w c-pointer))))
    vector-vec-4w))



;; CORE



;; DMATCH

(define-foreign-type dmatch ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser dmatch))


(defclass cv-dmatch ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-dmatch) (c-type dmatch))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type dmatch))
  (let ((dmatch  (make-instance 'cv-dmatch :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize dmatch (lambda () (del-dmatch c-pointer))))
    dmatch))



;; FILE-NODE


(define-foreign-type file-node ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser file-node))


(defclass cv-file-node ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-file-node) (c-type file-node))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type file-node))
  (let ((file-node  (make-instance 'cv-file-node :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize file-node (lambda () (del-file-node c-pointer))))
    file-node))



;; FILE-STORAGE


(define-foreign-type file-storage ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser file-storage))


(defclass cv-file-storage ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-file-storage) (c-type file-storage))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type file-storage))
  (let ((file-storage  (make-instance 'cv-file-storage :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize file-storage (lambda () (del-file-storage c-pointer))))
    file-storage))


;; KEY-POINT


(define-foreign-type key-point ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser key-point))


(defclass cv-key-point ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-key-point) (c-type key-point))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type key-point))
  (let ((key-point  (make-instance 'cv-key-point :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize key-point (lambda () (del-key-point c-pointer))))
    key-point))



;; MAT


(define-foreign-type mat ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser mat))


(defclass cv-mat ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-mat) (c-type mat))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type mat))
  (let ((matrix  (make-instance 'cv-mat :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize matrix (lambda () (del-mat c-pointer))))
    matrix))



;; MAT-EXPR


(define-foreign-type mat-expr ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser mat-expr))


(defclass matrix-expressions ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value matrix-expressions) (c-type mat-expr))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type mat-expr))
  (let ((matrix-expressions  (make-instance 'matrix-expressions :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize matrix-expressions (lambda () (del-mat-expr c-pointer))))
    matrix-expressions))


;; POINT


(define-foreign-type point ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser point))


(defclass cv-point ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


 (defmethod translate-to-foreign ((lisp-value cv-point) (c-type point))
    (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type point))
  (let ((point (make-instance 'cv-point :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize point (lambda () (del-point c-pointer))))
    point))



;; POINT-2D


(define-foreign-type point-2d ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser point-2d))


(defclass cv-point-2d ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-point-2d) (c-type point-2d))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type point-2d))
  (let ((point-2d (make-instance 'cv-point-2d :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize point-2d (lambda () (del-point-2d c-pointer))))
    point-2d))



;; POINT-2F


(define-foreign-type point-2f ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser point-2f))


(defclass cv-point-2f ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-point-2f) (c-type point-2f))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type point-2f))
  (let ((point-2f (make-instance 'cv-point-2f :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize point-2f (lambda () (del-point-2f c-pointer))))
    point-2f))



;; POINT-3D


(define-foreign-type point-3d ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser point-3d))


(defclass cv-point-3d ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-point-3d) (c-type point-3d))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type point-3d))
  (let ((point-3d (make-instance 'cv-point-3d :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize point-3d (lambda () (del-point-3d c-pointer))))
    point-3d))



;; POINT-3F


(define-foreign-type point-3f ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser point-3f))


(defclass cv-point-3f ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-point-3f) (c-type point-3f))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type point-3f))
  (let ((point-3f (make-instance 'cv-point-3f :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize point-3f (lambda () (del-point-3f c-pointer))))
    point-3f))



;; POINT-3I


(define-foreign-type point-3i ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser point-3i))


(defclass cv-point-3i ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-point-3i) (c-type point-3i))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type point-3i))
  (let ((point-3i (make-instance 'cv-point-3i :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize point-3i (lambda () (del-point-3i c-pointer))))
    point-3i))



;; RANGE


(define-foreign-type range ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser range))


(defclass cv-range ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-range) (c-type range))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type range))
  (let ((range (make-instance 'cv-range :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize range (lambda () (del-range c-pointer))))
    range))



;; RECT


(define-foreign-type rect ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser rect))


(defclass cv-rect ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-rect) (c-type rect))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type rect))
  (let ((rectangle  (make-instance 'cv-rect :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize rectangle (lambda () (del-rect c-pointer))))
    rectangle))



;; RNG


(define-foreign-type rng ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser rng))


(defclass cv-rng ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-rng) (c-type rng))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type rng))
  (let ((rng (make-instance 'cv-rng :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize rng (lambda () (del-rng c-pointer))))
    rng))



;; ROTATED-RECT


(define-foreign-type rotated-rect ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser rotated-rect))


(defclass cv-rotated-rect ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-rotated-rect) (c-type rotated-rect))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type rotated-rect))
  (let ((rotated-rect (make-instance 'cv-rotated-rect :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize rotated-rect (lambda () (del-rot-rect c-pointer))))
    rotated-rect))



;; SCALAR


(define-foreign-type scalar ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser scalar))


(defclass cv-scalar ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-scalar) (c-type scalar))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type scalar))
  (let ((scalar (make-instance 'cv-scalar :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize scalar (lambda () (del-scalar c-pointer))))
    scalar))



;; SIZE


(define-foreign-type size ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser size))


(defclass cv-size ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-size) (c-type size))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type size))
  (let ((size (make-instance 'cv-size :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize size (lambda () (del-size c-pointer))))
    size))



;; SIZE-2F


(define-foreign-type size-2f ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser size-2f))


(defclass cv-size-2f ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-size-2f) (c-type size-2f))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type size-2f))
  (let ((size-2f (make-instance 'cv-size-2f :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize size-2f (lambda () (del-size-2f c-pointer))))
    size-2f))



;; TERM-CRITERIA


(define-foreign-type term-criteria ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser term-criteria))


(defclass cv-term-criteria ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-term-criteria) (c-type term-criteria))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type term-criteria))
  (let ((term-criteria (make-instance 'cv-term-criteria :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize term-criteria (lambda () (del-term-crit c-pointer))))
    term-criteria))



;; VEC-2B


(define-foreign-type vec-2b ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-2b))


(defclass cv-vec-2b ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-2b) (c-type vec-2b))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-2b))
  (let ((vec-2b (make-instance 'cv-vec-2b :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-2b (lambda () (del-vec-2b c-pointer))))
    vec-2b))



;; VEC-3B


(define-foreign-type vec-3b ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-3b))


(defclass cv-vec-3b ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-3b) (c-type vec-3b))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-3b))
  (let ((vec-3b (make-instance 'cv-vec-3b :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-3b (lambda () (del-vec-3b c-pointer))))
    vec-3b))



;; VEC-4B


(define-foreign-type vec-4b ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-4b))


(defclass cv-vec-4b ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-4b) (c-type vec-4b))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-4b))
  (let ((vec-4b (make-instance 'cv-vec-4b :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-4b (lambda () (del-vec-4b c-pointer))))
    vec-4b))




;; VEC-2D


(define-foreign-type vec-2d ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-2d))


(defclass cv-vec-2d ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-2d) (c-type vec-2d))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-2d))
  (let ((vec-2d (make-instance 'cv-vec-2d :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-2d (lambda () (del-vec-2d c-pointer))))
    vec-2d))



;; VEC-3D


(define-foreign-type vec-3d ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-3d))


(defclass cv-vec-3d ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-3d) (c-type vec-3d))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-3d))
  (let ((vec-3d (make-instance 'cv-vec-3d :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-3d (lambda () (del-vec-3d c-pointer))))
    vec-3d))



;; VEC-4D


(define-foreign-type vec-4d ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-4d))


(defclass cv-vec-4d ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-4d) (c-type vec-4d))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-4d))
  (let ((vec-4d (make-instance 'cv-vec-4d :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-4d (lambda () (del-vec-4d c-pointer))))
    vec-4d))



;; VEC-2F


(define-foreign-type vec-2f ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-2f))


(defclass cv-vec-2f ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-2f) (c-type vec-2f))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-2f))
  (let ((vec-2f (make-instance 'cv-vec-2f :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-2f (lambda () (del-vec-2f c-pointer))))
    vec-2f))



;; VEC-3F


(define-foreign-type vec-3f ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-3f))


(defclass cv-vec-3f ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-3f) (c-type vec-3f))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-3f))
  (let ((vec-3f (make-instance 'cv-vec-3f :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-3f (lambda () (del-vec-3f c-pointer))))
    vec-3f))




;; VEC-4F


(define-foreign-type vec-4f ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-4f))


(defclass cv-vec-4f ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-4f) (c-type vec-4f))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-4f))
  (let ((vec-4f (make-instance 'cv-vec-4f :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-4f (lambda () (del-vec-4f c-pointer))))
    vec-4f))



;; VEC-6D


(define-foreign-type vec-6d ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-6d))


(defclass cv-vec-6d ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-6d) (c-type vec-6d))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-6d))
  (let ((vec-6d (make-instance 'cv-vec-6d :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-6d (lambda () (del-vec-6d c-pointer))))
    vec-6d))



;; VEC-6F


(define-foreign-type vec-6f ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-6f))


(defclass cv-vec-6f ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-6f) (c-type vec-6f))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-6f))
  (let ((vec-6f (make-instance 'cv-vec-6f :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-6f (lambda () (del-vec-6f c-pointer))))
    vec-6f))



;; VEC-2I


(define-foreign-type vec-2i ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-2i))


(defclass cv-vec-2i ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-2i) (c-type vec-2i))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-2i))
  (let ((vec-2i (make-instance 'cv-vec-2i :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-2i (lambda () (del-vec-2i c-pointer))))
    vec-2i))



;; VEC-3I


(define-foreign-type vec-3i ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-3i))


(defclass cv-vec-3i ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-3i) (c-type vec-3i))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-3i))
  (let ((vec-3i (make-instance 'cv-vec-3i :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-3i (lambda () (del-vec-3i c-pointer))))
    vec-3i))



;; VEC-4I


(define-foreign-type vec-4i ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-4i))


(defclass cv-vec-4i ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-4i) (c-type vec-4i))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-4i))
  (let ((vec-4i (make-instance 'cv-vec-4i :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-4i (lambda () (del-vec-4i c-pointer))))
    vec-4i))



;; VEC-6I


(define-foreign-type vec-6i ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-6i))


(defclass cv-vec-6i ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-6i) (c-type vec-6i))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-6i))
  (let ((vec-6i (make-instance 'cv-vec-6i :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-6i (lambda () (del-vec-6i c-pointer))))
    vec-6i))



;; VEC-8I


(define-foreign-type vec-8i ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-8i))


(defclass cv-vec-8i ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-8i) (c-type vec-8i))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-8i))
  (let ((vec-8i (make-instance 'cv-vec-8i :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-8i (lambda () (del-vec-8i c-pointer))))
    vec-8i))



;; VEC-2S


(define-foreign-type vec-2s ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-2s))


(defclass cv-vec-2s ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-2s) (c-type vec-2s))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-2s))
  (let ((vec-2s (make-instance 'cv-vec-2s :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-2s (lambda () (del-vec-2s c-pointer))))
    vec-2s))



;; VEC-3S


(define-foreign-type vec-3s ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-3s))


(defclass cv-vec-3s ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-3s) (c-type vec-3s))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-3s))
  (let ((vec-3s (make-instance 'cv-vec-3s :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-3s (lambda () (del-vec-3s c-pointer))))
    vec-3s))



;; VEC-4S


(define-foreign-type vec-4s ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-4s))


(defclass cv-vec-4s ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-4s) (c-type vec-4s))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-4s))
  (let ((vec-4s (make-instance 'cv-vec-4s :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-4s (lambda () (del-vec-4s c-pointer))))
    vec-4s))



;; VEC-2W


(define-foreign-type vec-2w ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-2w))


(defclass cv-vec-2w ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-2w) (c-type vec-2w))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-2w))
  (let ((vec-2w (make-instance 'cv-vec-2w :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-2w (lambda () (del-vec-2w c-pointer))))
    vec-2w))



;; VEC-3W


(define-foreign-type vec-3w ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-3w))


(defclass cv-vec-3w ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-3w) (c-type vec-3w))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-3w))
  (let ((vec-3w (make-instance 'cv-vec-3w :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-3w (lambda () (del-vec-3w c-pointer))))
    vec-3w))



;; VEC-4W


(define-foreign-type vec-4w ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vec-4w))


(defclass cv-vec-4w ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-vec-4w) (c-type vec-4w))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vec-4w))
  (let ((vec-4w (make-instance 'cv-vec-4w :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vec-4w (lambda () (del-vec-4w c-pointer))))
    vec-4w))



;; IMGPROC


;; HIGHGUI



;; VIDEO-CAPTURE


(define-foreign-type video-capture ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser video-capture))


(defclass cv-video-capture ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-video-capture) (c-type video-capture))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type video-capture))
  (let ((video-capture (make-instance 'cv-video-capture :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize video-capture (lambda () (del-video-capture c-pointer))))
    video-capture))



;; VIDEO-WRITER


(define-foreign-type video-writer ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser video-writer))


(defclass cv-video-writer ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-video-writer) (c-type video-writer))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type video-writer))
  (let ((video-writer (make-instance 'cv-video-writer :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize video-writer (lambda () (del-video-writer c-pointer))))
    video-writer))



;; CALIB3D


;; FEATURES2D


;; BF-MATCHER



(define-foreign-type bf-matcher ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser bf-matcher))


(defclass cv-bf-matcher ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-bf-matcher) (c-type bf-matcher))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type bf-matcher))
  (let ((bf-matcher (make-instance 'cv-bf-matcher :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize bf-matcher (lambda () (del-bf-matcher c-pointer))))
    bf-matcher))



;; BRISK


(define-foreign-type brisk ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser brisk))


(defclass cv-brisk ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-brisk) (c-type brisk))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type brisk))
  (let ((brisk (make-instance 'cv-brisk :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize brisk (lambda () (del-brisk c-pointer))))
    brisk))


; SURF


(define-foreign-type surf ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser surf))


(defclass cv-surf ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-surf) (c-type surf))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type surf))
  (let ((surf (make-instance 'cv-surf :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize surf (lambda () (del-surf c-pointer))))
    surf))



;; OBJDETECT



;; CASCADE-CLASSIFEIER


(define-foreign-type cascade-classifier ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser cascade-classifier))


(defclass cv-cascade-classifier ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-cascade-classifier) (c-type cascade-classifier))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type cascade-classifier))
  (let ((cascade-classifier (make-instance 'cv-cascade-classifier :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize cascade-classifier (lambda () (del-cascade-classifier c-pointer))))
    cascade-classifier))



;; HOG-DESCRIPTOR


(define-foreign-type hog-descriptor ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser hog-descriptor))


(defclass cv-hog-descriptor ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-hog-descriptor) (c-type hog-descriptor))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type hog-descriptor))
  (let ((hog-descriptor (make-instance 'cv-hog-descriptor :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize hog-descriptor (lambda () (del-hog-descriptor c-pointer))))
    hog-descriptor))



;; ML



;; ANN-MLP


(define-foreign-type ann-mlp ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser ann-mlp))


(defclass cv-ann-mlp ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-ann-mlp) (c-type ann-mlp))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type ann-mlp))
  (let ((ann-mlp  (make-instance 'cv-ann-mlp :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize ann-mlp (lambda () (del-ann-mlp c-pointer))))
    ann-mlp))



;; ANN-MLP-TRAIN-PARAMS

(define-foreign-type ann-mlp-train-params ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser ann-mlp-train-params))


(defclass cv-ann-mlp-train-params ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-ann-mlp-train-params) (c-type ann-mlp-train-params))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type ann-mlp-train-params))
  (let ((ann-mlp-train-params  (make-instance 'cv-ann-mlp-train-params :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize ann-mlp-train-params (lambda () (del-ann-mlp-train-params c-pointer))))
    ann-mlp-train-params))



;; D-TREE

(define-foreign-type d-tree ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser d-tree))


(defclass cv-d-tree ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-d-tree) (c-type d-tree))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type d-tree))
  (let ((d-tree  (make-instance 'cv-d-tree :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize d-tree (lambda () (del-d-tree c-pointer))))
    d-tree))



;; D-TREE-PARAMS

(define-foreign-type d-tree-params ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser d-tree-params))


(defclass cv-d-tree-params ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-d-tree-params) (c-type d-tree-params))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type d-tree-params))
  (let ((d-tree-params  (make-instance 'cv-d-tree-params :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize d-tree-params (lambda () (del-d-tree-params c-pointer))))
    d-tree-params))



;; K-NEAREST

(define-foreign-type k-nearest ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser k-nearest))


(defclass cv-k-nearest ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-k-nearest) (c-type k-nearest))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type k-nearest))
  (let ((k-nearest  (make-instance 'cv-k-nearest :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize k-nearest (lambda () (del-k-nearest c-pointer))))
    k-nearest))



;; NORMAL-BAYES-CLASSIFIER

(define-foreign-type normal-bayes-classifier ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser normal-bayes-classifier))


(defclass cv-normal-bayes-classifier ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-normal-bayes-classifier) (c-type normal-bayes-classifier))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type normal-bayes-classifier))
  (let ((normal-bayes-classifier  (make-instance 'cv-normal-bayes-classifier :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize normal-bayes-classifier (lambda () (del-normal-bayes-classifier c-pointer))))
    normal-bayes-classifier))



;; NONFREE



;; CONTRIB




