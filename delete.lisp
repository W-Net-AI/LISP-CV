;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; delete.lisp
;;;; C++ bindings
;;;; Delete functions


(in-package :lisp-cv)


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
;; void cv_delete_Mat(void* self)
(defcfun ("cv_delete_Mat" del-mat) :void
  "Calls delete on a (:POINTER MAT)"
  (self :pointer))


;; void operator delete  ( void* ptr )
;; void cv_delete_MatExpr(void* self)
(defcfun ("cv_delete_MatExpr" del-mat-expr) :void
  "Calls delete on a (:POINTER MAT-EXPR)"
  (self :pointer))


;; void operator delete  ( void* ptr )
;; void cv_delete_Point(void* self)
(defcfun ("cv_delete_Point" del-point) :void
  "Calls delete on a (:POINTER POINT)"
  (self :pointer))


;; void operator delete  ( void* ptr )
;; void cv_delete_Rect(void* self)
(defcfun ("cv_delete_Rect" del-rect) :void
  "Calls delete on a (:POINTER RECT)"
  (self :pointer))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectorc" del-vec-char) :void
  "Calls delete on a (:POINTER VECTOR-CHAR)"
  (self :pointer))


;; void operator delete  ( void* ptr )
;; void delete_std_vector##tn( vector_##t * v)
(defcfun ("delete_std_vectord" del-vec-dbl) :void
  "Calls delete on a (:POINTER VECTOR-DBL)"
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
