;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; delete.lisp
;;;; C++ bindings
;;;; Delete functions


(in-package :lisp-cv)


;; void operator delete  ( void* ptr )
;; void cv_delete(char* ptr)
(defcfun ("cv_delete" del) :void
  "Calls delete"
  (ptr :pointer))


;; void operator delete  ( void* ptr )
;; void cv_delete_BFMatcher(void* ptr)
(defcfun ("cv_delete_BFMatcher" del-bf-matcher) :void
  "Calls delete on a (:POINTER BF-MATCHER)"
  (ptr :pointer))


;; void operator delete  ( void* ptr )
;; void cv_delete_BRISK(void* ptr) 
(defcfun ("cv_delete_BRISK" del-brisk) :void
  "Calls delete on a (:POINTER BRISK)"
  (ptr :pointer))


;; void operator delete  ( void* ptr )
;; void cv_delete_KeyPoint(void* self)
(defcfun ("cv_delete_KeyPoint" del-kp) :void
  "Calls delete on KeyPoint"
  (self keypoint))


;; void operator delete  ( void* ptr )
;; void cv_delete_Mat(void* self)
(defcfun ("cv_delete_Mat" del-mat) :void
  "Calls delete on MAT"
  (self mat))


;; void operator delete  ( void* ptr )
;; void cv_delete_MatExpr(void* self)
(defcfun ("cv_delete_MatExpr" del-mat-expr) :void
  "Calls delete on MAT-EXPR"
  (self mat-expr))


;; void operator delete  ( void* ptr )
;; void cv_delete_Point(void* self)
(defcfun ("cv_delete_Point" del-point) :void
  "Calls delete on POINT"
  (self point))


;; void operator delete  ( void* ptr )
;; void cv_delete_Point2d(void* self)
(defcfun ("cv_delete_Point2d" del-point2d) :void
  "Calls delete on POINT2D"
  (self point2d))


;; void operator delete  ( void* ptr )
;; void cv_delete_Point2f(void* self)
(defcfun ("cv_delete_Point2f" del-point2f) :void
  "Calls delete on POINT2F"
  (self point2f))


;; void operator delete  ( void* ptr )
;; void cv_delete_Point3d(void* self)
(defcfun ("cv_delete_Point3d" del-point3d) :void
  "Calls delete on POINT3D"
  (self point3d))


;; void operator delete  ( void* ptr )
;; void cv_delete_Point3f(void* self)
(defcfun ("cv_delete_Point3f" del-point3f) :void
  "Calls delete on POINT3F"
  (self point3f))


;; void operator delete  ( void* ptr )
;; void cv_delete_Point3i(void* self)
(defcfun ("cv_delete_Point3i" del-point3i) :void
  "Calls delete on POINT3I"
  (self point3i))


;; void operator delete  ( void* ptr )
;; void cv_delete_Rect(void* self)
(defcfun ("cv_delete_Rect" del-rect) :void
  "Calls delete on RECT"
  (self rect))


;; void operator delete  ( void* ptr )
;; void cv_delete_RNG(void* self)
(defcfun ("cv_delete_RNG" del-rng) :void
  "Calls delete on RNG"
  (self rng))


;; void operator delete  ( void* ptr )
;; void cv_delete_Scalar(void* self)
(defcfun ("cv_delete_Scalar" del-scalar) :void
  "Calls delete on SCALAR"
  (self scalar))


;; void operator delete  ( void* ptr )
;; void cv_delete_Size(void* self)
(defcfun ("cv_delete_Size" del-size) :void
  "Calls delete on SIZE"
  (self size))


;; void operator delete  ( void* ptr )
;; void cv_delete_std_string(string* self)
(defcfun ("cv_delete_std_string" del-std-string) :void
  "Calls delete on *STRING"
  (ptr *string))


;; void operator delete  ( void* ptr )
;; void cv_delete_SURF(SURF* self)
(defcfun ("cv_delete_SURF" del-surf) :void
  "Calls delete on SURF"
  (ptr surf))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectorc" del-vec-char) :void
  "Calls delete on a VECTOR-CHAR"
  (self :pointer))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectord" del-vec-dbl) :void
  "Calls delete on a VECTOR-DOUBLE"
  (self :pointer))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectordm" del-vec-dm) :void
  "Calls delete on a (:POINTER VECTOR-DMATCH)"
  (self :pointer))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectorf" del-vec-flt) :void
  "Calls delete on a (:POINTER VECTOR-FLOAT)"
  (self :pointer))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectori" del-vec-int) :void
  "Calls delete on a (:POINTER VECTOR-INT)"
  (self :pointer))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectorkp" del-vec-kp) :void
  "Calls delete on a (:POINTER VECTOR-KEYPOINT)"
  (self :pointer))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectorm" del-vec-mat) :void
  "Calls delete on a (:POINTER VECTOR-MAT)"
  (self :pointer))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectorp" del-vec-point) :void
  "Calls delete on a (:POINTER VECTOR-POINT)"
  (self :pointer))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectorp2f" del-vec-point2f) :void
  "Calls delete on a (:POINTER VECTOR-POINT2F)"
  (self :pointer))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectorr" del-vec-rect) :void
  "Calls delete on a (:POINTER VECTOR-RECT)"
  (self :pointer))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectoru" del-vec-uchar) :void
  "Calls delete on a (:POINTER VECTOR-UCHAR)"
  (self :pointer))
