;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; destruct.lisp
;;;; OpenCV and C++ bindings
;;;; Destruct functions


(in-package :lisp-cv)


;; ~BFMatcher()
;; void cv_destruct_BFMatcher(Mat* self)
(defcfun ("cv_destruct_BFMatcher" dest-bf-matcher) :void
  "Destructor - calls release()"
  (self :pointer))


;; ~BRISK()
;; void cv_destruct_BRISK(Mat* self)
(defcfun ("cv_destruct_BRISK" dest-brisk) :void
  "Destructor - calls release()"
  (self :pointer))


;; ~Mat()
;; void cv_destruct_Mat(Mat* self)
(defcfun ("cv_destruct_Mat" dest-mat) :void
  "Destructor - calls release()"
  (self :pointer))


;; ~MatExpr()
;; void cv_destruct_MatExpr(MatExpr* self) 
(defcfun ("cv_destruct_MatExpr" dest-mat-expr) :void
  "Destructor - calls release()"
  (self :pointer))


;; ~Point()
;; void cv_destruct_Point(Mat* self)
(defcfun ("cv_destruct_Point" dest-point) :void
  "Destructor - calls release()"
  (self :pointer))


;; ~Rect()
;; void cv_destruct_Rect(Rect* self)
(defcfun ("cv_destruct_Rect" dest-rect) :void
  "Destructor - calls release()"
  (self :pointer))


;; std::vector::~vector
;; void destroy_std_vector##tn( vector_##t * v) 
(defcfun ("destroy_std_vectorc" dest-vec-char) :void
  "Destructor - calls release()"
  (self :pointer))


;; std::vector::~vector
;; void destroy_std_vector##tn( vector_##t * v) 
(defcfun ("destroy_std_vectord" dest-vec-dbl) :void
  "Destructor - calls release()"
  (self :pointer))


;; std::vector::~vector
;; void destroy_std_vector##tn( vector_##t * v) 
(defcfun ("destroy_std_vectordm" dest-vec-dm) :void
  "Destructor - calls release()"
  (self :pointer))


;; std::vector::~vector
;; void destroy_std_vector##tn( vector_##t * v) 
(defcfun ("destroy_std_vectorf" dest-vec-flt) :void
  "Destructor - calls release()"
  (self :pointer))


;; std::vector::~vector
;; void destroy_std_vector##tn( vector_##t * v) 
(defcfun ("destroy_std_vectori" dest-vec-int) :void
  "Destructor - calls release()"
  (self :pointer))


;; std::vector::~vector
;; void destroy_std_vector##tn( vector_##t * v) 
(defcfun ("destroy_std_vectorkp" dest-vec-kp) :void
  "Destructor - calls release()"
  (self :pointer))


;; std::vector::~vector
;; void destroy_std_vector##tn( vector_##t * v) 
(defcfun ("destroy_std_vectorm" dest-vec-mat) :void
  "Destructor - calls release()"
  (self :pointer))


;; std::vector::~vector
;; void destroy_std_vector##tn( vector_##t * v) 
(defcfun ("destroy_std_vectorp" dest-vec-p) :void
  "Destructor - calls release()"
  (self :pointer))


;; std::vector::~vector
;; void destroy_std_vector##tn( vector_##t * v) 
(defcfun ("destroy_std_vectorp2f" dest-vec-p2f) :void
  "Destructor - calls release()"
  (self :pointer))


;; std::vector::~vector
;; void destroy_std_vector##tn( vector_##t * v) 
(defcfun ("destroy_std_vectorr" dest-vec-rect) :void
  "Destructor - calls release()"
  (self :pointer))


;; std::vector::~vector
;; void destroy_std_vector##tn( vector_##t * v) 
(defcfun ("destroy_std_vectoru" dest-vec-uchar) :void
  "Destructor - calls release()"
  (self :pointer))
