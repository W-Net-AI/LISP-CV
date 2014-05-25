;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; core.lisp
;;;; OpenCV bindings
;;;; Type declarations and Trivial Garbage finalizers for memory 
;;;; management, Finalizers are still in process. If a function 
;;;; has a finalizer you can use it by passing a gc: or a t: 
;;;; before the functions name to activate it.


(in-package :lisp-cv)


;; NON GARBAGE COLLECTED TYPES


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
      (tg:finalize vector-char (lambda () (del-vec-char c-pointer))))
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
      (tg:finalize vector-dmatch (lambda () (del-vec-dm c-pointer))))
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
      (tg:finalize vector-key-point (lambda () (del-vec-kp c-pointer))))
    vector-key-point))


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
      (tg:finalize vector-point-2f (lambda () (del-vec-point-2f c-pointer))))
    vector-point-2f))



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
      (tg:finalize vector-mat (lambda () (del-vec-mat c-pointer))))
    vector-mat))


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
      (tg:finalize vector-rect (lambda () (del-vec-rect c-pointer))))
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


;; FEATURE-2D


(define-foreign-type feature-2d ()
  ((garbage-collect  :reader garbage-collect :initform nil :initarg 
                     :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser feature-2d))


(defclass cv-feature-2d ()
  ((c-pointer :reader c-pointer :initarg :c-pointer)))


(defmethod translate-to-foreign ((lisp-value cv-feature-2d) (c-type feature-2d))
  (values  (c-pointer lisp-value) lisp-value))


(defmethod translate-from-foreign (c-pointer (c-type feature-2d))
  (let ((feature-2d (make-instance 'cv-feature-2d :c-pointer c-pointer)))
    (when (garbage-collect c-type)
      (tg:finalize feature-2d (lambda () (del-feature-2d c-pointer))))
    feature-2d))


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
      (tg:finalize ann-mlp (lambda () (del-std-string c-pointer))))
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




