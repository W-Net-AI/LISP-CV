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


;; *STRING


(define-foreign-type *string ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser *string))


(defclass c-string ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value c-string) (c-type *string))
  (values  (c-pointer lisp-value) lisp-value))


    
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
  (values  (c-pointer lisp-value) lisp-value))


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
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type vector-double))
  (let ((vector-double  (make-instance 'std-vector-double :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize vector-double (lambda () (del-vec-dbl c-pointer))))
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
      (tg:finalize vector-float (lambda () (del-vec-flt c-pointer))))
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
      (tg:finalize vector-int (lambda () (del-vec-int c-pointer))))
    vector-int))


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
      (tg:finalize vector-point (lambda () (del-vec-point c-pointer))))
    vector-point))


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
      (tg:finalize vector-uchar (lambda () (del-vec-uchar c-pointer))))
    vector-uchar))


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
      (tg:finalize key-point (lambda () (del-kp c-pointer))))
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
  ((garbage-collect  :reader garbage-collect :initform nil 
                     :initarg :garbage-collect))
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


;; POINT2D


(define-foreign-type point2d ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser point2d))


(defclass cv-point2d ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-point2d) (c-type point2d))
  (values  (c-pointer lisp-value) lisp-value))


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
  (values  (c-pointer lisp-value) lisp-value))


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
  (values  (c-pointer lisp-value) lisp-value))


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
  (values  (c-pointer lisp-value) lisp-value))


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
  (values  (c-pointer lisp-value) lisp-value))


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
  (:simple-parser scfalar))


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


;; SIZE2F


(define-foreign-type size2f ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser size2f))


(defclass cv-size2f ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-size2f) (c-type size2f))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type size2f))
  (let ((size2f (make-instance 'cv-size2f :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize size2f (lambda () (del-size2f c-pointer))))
    size2f))



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
      (tg:finalize video-capture (lambda () (del-vid-cap c-pointer))))
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
      (tg:finalize video-writer (lambda () (del-vid-writer c-pointer))))
    video-writer))


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
  (values  (c-pointer lisp-value) lisp-value))


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
      (tg:finalize cascade-classifier (lambda () (del-casc-class c-pointer))))
    cascade-classifier))



;; NONFREE


;; CONTRIB

