;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; delete.lisp
;;;; C++ bindings
;;;; Delete functions for GC'ed types


(in-package :lisp-cv)


;; void operator delete  ( void* ptr )
;; void cv_delete(char* ptr)
(defcfun ("cv_delete" del) :void
  "Calls delete"
  (self :pointer))


;; void operator delete  ( void* ptr )
;; void cv_delete_CvANN_MLP(CvANN_MLP* ptr)
(defcfun ("cv_delete_CvANN_MLP" del-ann-mlp) :void
  "Calls delete on ANN-MLP"
  (self ann-mlp))


;; void operator delete  ( void* ptr )
;; void cv_delete_CvANN_MLP_TrainParams(CvANN_MLP_TrainParams* self)
(defcfun ("cv_delete_CvANN_MLP_TrainParams" del-ann-mlp-train-params) :void
  "Calls delete on ANN-MLP-TRAIN-PARAMS"
  (self ann-mlp-train-params))


;; void operator delete  ( void* ptr )
;; void cv_delete_CascadeClassifier(CascadeClassifier* ptr) 
(defcfun ("cv_delete_CascadeClassifier" del-casc-class) :void
  "Calls delete on CASCADE-CLASSIFIER"
  (self cascade-classifier))


;; void operator delete  ( void* ptr )
;; void cv_delete_DMatch(DMatch* self)
(defcfun ("cv_delete_DMatch" del-dmatch) :void
  "Calls delete on DMATCH"
  (self dmatch))


;; void operator delete  ( void* ptr )
;; void cv_delete_CvDTree(CvDTree* self)
(defcfun ("cv_delete_CvDTree" del-d-tree) :void
  "Calls delete on D-TREE"
  (self d-tree))


;; void operator delete  ( void* ptr )
;; void cv_delete_CvDTreeParams(CvDTreeParam* self)
(defcfun ("cv_delete_CvDTreeParams" del-d-tree-params) :void
  "Calls delete on D-TREE-PARAMS"
  (self d-tree-params))


;; void operator delete  ( void* ptr )
;; void cv_delete_Feature2D(Feature2D* self)
(defcfun ("cv_delete_Feature2D" del-feature-2d) :void
  "Calls delete on FEATURE-2D"
  (self feature-2d))


;; void operator delete  ( void* ptr )
;; void cv_delete_HOGDescriptor(HOGDescriptor* self)
(defcfun ("cv_delete_HOGDescriptor" del-hog-descriptor) :void
  "Calls delete on HOG-DESCRIPTOR"
  (self hog-descriptor))


;; void operator delete  ( void* ptr )
;; void cv_delete_CvKNearest(CvKNearest* self)
(defcfun ("cv_delete_CvKNearest" del-k-nearest) :void
  "Calls delete on K-NEAREST"
  (self k-nearest))


;; void operator delete  ( void* ptr )
;; void cv_delete_KeyPoint(KeyPoint* self)
(defcfun ("cv_delete_KeyPoint" del-kp) :void
  "Calls delete on KEY-POINT"
  (self key-point))


;; void operator delete  ( void* ptr )
;; void cv_delete_Mat(Mat* self)
(defcfun ("cv_delete_Mat" del-mat) :void
  "Calls delete on MAT"
  (self mat))


;; void operator delete  ( void* ptr )
;; void cv_delete_(MatExpr* self)
(defcfun ("cv_delete_MatExpr" del-mat-expr) :void
  "Calls delete on MAT-EXPR"
  (self mat-expr))


;; void operator delete  ( void* ptr )
;; void cv_delete_CvNormalBayesClassifier(CvNormalBayesClassifier* self)
(defcfun ("cv_delete_CvNormalBayesClassifier" del-normal-bayes-classifier) :void
  "Calls delete on NORMAL-BAYES-CLASSIFIER"
  (self normal-bayes-classifier))


;; void operator delete  ( void* ptr )
;; void cv_delete_Point(Point* self)
(defcfun ("cv_delete_Point" del-point) :void
  "Calls delete on POINT"
  (self point))


;; void operator delete  ( void* ptr )
;; void cv_delete_Point2d(Point2d* self)
(defcfun ("cv_delete_Point2d" del-point-2d) :void
  "Calls delete on POINT-2D"
  (self point-2d))


;; void operator delete  ( void* ptr )
;; void cv_delete_Point2f(Point2f* self)
(defcfun ("cv_delete_Point2f" del-point-2f) :void
  "Calls delete on POINT-2F"
  (self point-2f))


;; void operator delete  ( void* ptr )
;; void cv_delete_Point3d(Point3d* self)
(defcfun ("cv_delete_Point3d" del-point-3d) :void
  "Calls delete on POINT-3D"
  (self point-3d))


;; void operator delete  ( void* ptr )
;; void cv_delete_Point3f(Point3f* self)
(defcfun ("cv_delete_Point3f" del-point-3f) :void
  "Calls delete on POINT-3F"
  (self point-3f))


;; void operator delete  ( void* ptr )
;; void cv_delete_Point3i(Point3i* self)
(defcfun ("cv_delete_Point3i" del-point-3i) :void
  "Calls delete on POINT-3I"
  (self point-3i))


;; void operator delete  ( void* ptr )
;; void cv_delete_Rect(Rect* self)
(defcfun ("cv_delete_Rect" del-rect) :void
  "Calls delete on RECT"
  (self rect))


;; void operator delete  ( void* ptr )
;; void cv_delete_Range(Rect* self)
(defcfun ("cv_delete_Range" del-range) :void
  "Calls delete on RANGE"
  (self range))


;; void operator delete  ( void* ptr )
;; void cv_delete_RNG(RNG* self)
(defcfun ("cv_delete_RNG" del-rng) :void
  "Calls delete on RNG"
  (self rng))


;; void operator delete  ( void* ptr )
;; void cv_delete_RotatedRect(RotatedRect* self)
(defcfun ("cv_delete_RotatedRect" del-rot-rect) :void
  "Calls delete on ROTATED-RECT"
  (self rotated-rect))


;; void operator delete  ( void* ptr )
;; void cv_delete_Scalar(Scalar* self)
(defcfun ("cv_delete_Scalar" del-scalar) :void
  "Calls delete on SCALAR"
  (self scalar))


;; void operator delete  ( void* ptr )
;; void cv_delete_Size(Size* self)
(defcfun ("cv_delete_Size" del-size) :void
  "Calls delete on SIZE"
  (self size))


;; void operator delete  ( void* ptr )
;; void cv_delete_Size2f(Size2f* self)
(defcfun ("cv_delete_Size2f" del-size2f) :void
  "Calls delete on SIZE2F"
  (self size2f))


;; void operator delete  ( void* ptr )
;; void cv_delete_std_string(string* self)
(defcfun ("cv_delete_std_string" del-std-string) :void
  "Calls delete on *STRING"
  (self *string))


;; void operator delete  ( void* ptr )
;; void cv_delete_TermCriteria(TermCriteria* self)
(defcfun ("cv_delete_TermCriteria" del-term-crit) :void
  "Calls delete on TERM-CRITERIA"
  (self term-criteria))


;; void operator delete  ( void* ptr )
;; void cv_delete_Vec4i(Vec4i* self)
(defcfun ("cv_delete_Vec4i" del-vec-4i) :void
  "Calls delete on VEC-4I"
  (self vec-4i))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectorc" del-vec-char) :void
  "Calls delete on VECTOR-CHAR"
  (self vector-char))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectord" del-vec-dbl) :void
  "Calls delete on VECTOR-DOUBLE"
  (self vector-double))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectordm" del-vec-dm) :void
  "Calls delete on VECTOR-DMATCH"
  (self vector-dmatch))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectorf" del-vec-flt) :void
  "Calls delete on VECTOR-FLOAT"
  (self vector-float))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectori" del-vec-int) :void
  "Calls delete on VECTOR-INT"
  (self vector-int))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectorkp" del-vec-kp) :void
  "Calls delete on VECTOR-KEY-POINT"
  (self vector-key-point))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectorm" del-vec-mat) :void
  "Calls delete on VECTOR-MAT"
  (self vector-mat))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectorp" del-vec-point) :void
  "Calls delete on VECTOR-POINT"
  (self vector-point))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectorp2f" del-vec-point-2f) :void
  "Calls delete on VECTOR-POINT-2F"
  (self vector-point-2f))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectorr" del-vec-rect) :void
  "Calls delete on VECTOR-RECT"
  (self vector-rect))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectoru" del-vec-uchar) :void
  "Calls delete on VECTOR-UCHAR"
  (self vector-uchar))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectorv4i" del-vec-vec-4i) :void
  "Calls delete on VECTOR-VEC-4I"
  (self vector-vec-4i))


;; void operator delete  ( void* ptr )
;; void cv_delete_VideoCapture(VideoCapture* self)
(defcfun ("cv_delete_VideoCapture" del-vid-cap) :void
  "Calls delete on VIDEO-CAPTURE"
  (self video-capture))


;; void operator delete  ( void* ptr )
;; void delete_VideoWriter(VideoWriter* self)
(defcfun ("cv_delete_VideoWriter" del-vid-writer) :void
  "Calls delete on VIDEO-WRITER"
  (self video-writer))
