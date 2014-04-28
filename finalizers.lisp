;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; finalizers.lisp
;;;; OpenCV bindings
;;;; Trivial Garbage finalizers for memory management,
;;;; and required type declarations
;;;; Finalizers are still in process. If a function has 
;;;; a finalizer you can use it by passing a :t after the 
;;;; functions parameters e.g. (mat :t)


(in-package :lisp-cv)

;; C++ INTTEROP TYPES


;; STRING


(define-foreign-type *string ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser *string))


(defclass c-string ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value c-string) (c-type *string))
  (c-pointer lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type *string))
  (let ((string  (make-instance 'c-string :c-pointer c-pointer)))
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
  (c-pointer lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-char))
  (let ((vector-char  (make-instance 'std-vector-char :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-char (lambda () (del-vec-char c-pointer))))
    vector-char))


;; VECTOR-DOUBLE


(define-foreign-type vector-double ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser vector-double))


(defclass std-vector-double ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value std-vector-double) (c-type vector-double))
  (c-pointer lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-double))
  (let ((vector-double  (make-instance 'std-vector-double :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-double (lambda () (del-vec-dbl c-pointer))))
    vector-double))



;; CORE


;; KEYPOINT


(define-foreign-type keypoint ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser keypoint))


(defclass cv-keypoint ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-keypoint) (c-type keypoint))
  (c-pointer lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type keypoint))
  (let ((keypoint  (make-instance 'cv-keypoint :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize keypoint (lambda () (del-kp c-pointer))))
    keypoint))



;; MAT


(define-foreign-type mat ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser mat))


(defclass cv-mat ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-mat) (c-type mat))
  (c-pointer lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type mat))
  (let ((matrix  (make-instance 'cv-mat :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize matrix (lambda () (del-mat c-pointer))))
    matrix))



;; MAT-EXPR


(define-foreign-type mat-expr ()
  ((garbage-collect  :reader garbage-collect :initform nil 
                     :initarg :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser mat-expr))


(defclass matrix-expressions ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value matrix-expressions) (c-type mat-expr))
  (c-pointer lisp-value))


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
  (c-pointer lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type point))
  (let ((point (make-instance 'cv-point :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize point (lambda () (del-point c-pointer))))
    point))


;; POINT2D


(define-foreign-type point2d ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser point2d))


(defclass cv-point2d ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-point2d) (c-type point2d))
  (c-pointer lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type point2d))
  (let ((point2d (make-instance 'cv-point2d :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize point2d (lambda () (del-point2d c-pointer))))
    point2d))


;; POINT2F


(define-foreign-type point2f ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser point2f))


(defclass cv-point2f ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-point2f) (c-type point2f))
  (c-pointer lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type point2f))
  (let ((point2f (make-instance 'cv-point2f :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize point2f (lambda () (del-point2f c-pointer))))
    point2f))


;; POINT3D


(define-foreign-type point3d ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser point3d))


(defclass cv-point3d ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-point3d) (c-type point3d))
  (c-pointer lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type point3d))
  (let ((point3d (make-instance 'cv-point3d :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize point3d (lambda () (del-point3d c-pointer))))
    point3d))


;; POINT3F


(define-foreign-type point3f ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser point3f))


(defclass cv-point3f ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-point3f) (c-type point3f))
  (c-pointer lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type point3f))
  (let ((point3f (make-instance 'cv-point3f :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize point3f (lambda () (del-point3f c-pointer))))
    point3f))


;; POINT3I


(define-foreign-type point3i ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser point3i))


(defclass cv-point3i ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-point3i) (c-type point3i))
  (c-pointer lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type point3i))
  (let ((point3i (make-instance 'cv-point3i :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize point3i (lambda () (del-point3i c-pointer))))
    point3i))


;; RECT


(define-foreign-type rect ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser rect))


(defclass cv-rect ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-rect) (c-type rect))
  (c-pointer lisp-value))


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
  (c-pointer lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type rng))
  (let ((rng (make-instance 'cv-rng :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize rng (lambda () (del-rng c-pointer))))
    rng))


;; SCALAR


(define-foreign-type scalar ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser scalar))


(defclass cv-scalar ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-scalar) (c-type scalar))
  (c-pointer lisp-value))


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
  (c-pointer lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type size))
  (let ((size (make-instance 'cv-size :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize size (lambda () (del-size c-pointer))))
    size))


;; IMGPROC


;; HIGHGUI


;; CALIB3D


;; FEATURES2D


;; FEATURE-DETECTOR


(define-foreign-type feature-detector ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser feature-detector))


(defclass cv-feature-detector ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-feature-detector) (c-type feature-detector))
  (c-pointer lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type feature-detector))
  (let ((feature-detector (make-instance 'cv-feature-detector :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize feature-detector (lambda () (del-feature-detector c-pointer))))
    feature-detector))


;; SURF


(define-foreign-type surf ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser surf))


(defclass cv-surf ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-surf) (c-type surf))
  (c-pointer lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type surf))
  (let ((surf (make-instance 'cv-surf :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize surf (lambda () (del-surf c-pointer))))
    surf))


;; OBJDETECT


;; NONFREE


;; CONTRIB

