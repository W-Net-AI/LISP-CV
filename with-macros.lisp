;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; with-macros.lisp
;;;; OpenCV bindings
;;;; WITH-* Macros for memory management.


(in-package :lisp-cv)


(defmacro with-bf-matcher ((bf-matcher-var bf-matcher) &body body)
  "Ensures DEL-BF-MATCHER gets called 
   when BF-MATCHER goes out of scope."
  `(let ((,bf-matcher-var ,bf-matcher))
     (unwind-protect
	 (progn ,@body)
       (del-bf-matcher ,bf-matcher-var))))


(defmacro with-brisk ((brisk-var brisk) &body body)
  "Ensures DEL-BRISK gets called 
   when BRISK goes out of scope."
  `(let ((,brisk-var ,brisk))
     (unwind-protect
	 (progn ,@body)
       (del-brisk ,brisk-var))))


(defmacro with-mat ((mat-var mat) &body body)
  "Ensures DEL-MAT gets called 
   when MAT goes out of scope."
  `(let ((,mat-var ,mat))
     (unwind-protect
	 (progn ,@body)
       (del-mat ,mat-var))))


(defmacro with-mat-expr ((mat-expr-var mat-expr) &body body)
  "Ensures DEL-MAT-EXPR gets called 
   when MAT-EXPR goes out of scope."
  `(let ((,mat-expr-var ,mat-expr))
     (unwind-protect
	 (progn ,@body)
       (del-mat-expr ,mat-expr-var))))


(defmacro with-rect ((rect-var rect) &body body)
  "Ensures DEL-RECT gets called 
   when RECT goes out of scope."
  `(let ((,rect-var ,rect))
     (unwind-protect
	 (progn ,@body)
       (del-rect ,rect-var))))


(defmacro with-point ((point-var point) &body body)
  "Ensures DEL-POINT gets called 
   when POINT goes out of scope."
  `(let ((,point-var ,point))
     (unwind-protect
	 (progn ,@body)
       (del-point ,point-var))))


(defmacro with-vec-char ((vec-char-var vec-char) &body body)
  "Ensures DEL-VEC-CHAR gets called 
   when VECTOR-CHAR goes out of scope."
  `(let ((,vec-char-var ,vec-char))
     (unwind-protect
	 (progn ,@body)
       (del-vec-char ,vec-char-var))))


(defmacro with-vec-dbl ((vec-dbl-var vec-dbl) &body body)
  "Ensures DEL-VEC-DBL gets called 
   when VECTOR-DOUBLE goes out of scope."
  `(let ((,vec-dbl-var ,vec-dbl))
     (unwind-protect
	 (progn ,@body)
       (del-vec-dbl ,vec-dbl-var))))


(defmacro with-vec-dmatch ((vec-dmatch-var vec-dmatch) &body body)
  "Ensures DEL-VEC-DM gets called 
   when VECTOR-DMATCH goes out of scope."
  `(let ((,vec-dmatch-var ,vec-dmatch))
     (unwind-protect
	 (progn ,@body)
       (del-vec-dm ,vec-dmatch-var))))


(defmacro with-vec-flt ((vec-flt-var vec-flt) &body body)
  "Ensures DEL-VEC-FLT gets called 
   when VECTOR-FLOAT goes out of scope."
  `(let ((,vec-flt-var ,vec-flt))
     (unwind-protect
	 (progn ,@body)
       (del-vec-flt ,vec-flt-var))))


(defmacro with-vec-int ((vec-int-var vec-int) &body body)
  "Ensures DEL-VEC-INT gets called 
   when VECTOR-INT goes out of scope."
  `(let ((,vec-int-var ,vec-int))
     (unwind-protect
	 (progn ,@body)
       (del-vec-int ,vec-int-var))))


(defmacro with-vec-keypoint ((vec-keypoint-var vec-keypoint) &body body)
  "Ensures DEL-VEC-KP gets called 
   when VECTOR-KEYPOINT goes out of scope."
  `(let ((,vec-keypoint-var ,vec-keypoint))
     (unwind-protect
	 (progn ,@body)
       (del-vec-kp ,vec-keypoint-var))))


(defmacro with-vec-mat ((vec-mat-var vec-mat) &body body)
  "Ensures DEL-VEC-MAT gets called 
   when VECTOR-MAT goes out of scope."
  `(let ((,vec-mat-var ,vec-mat))
     (unwind-protect
	 (progn ,@body)
       (del-vec-mat ,vec-mat-var))))


(defmacro with-vec-point ((vec-point-var vec-point) &body body)
  "Ensures DEL-VEC-POINT gets called 
   when VECTOR-POINT goes out of scope."
  `(let ((,vec-point-var ,vec-point))
     (unwind-protect
	 (progn ,@body)
       (del-vec-point ,vec-point-var))))


(defmacro with-vec-point2f ((vec-point2f-var vec-point2f) &body body)
  "Ensures DEL-VEC-POINT2F gets called 
   when VECTOR-POINT2F goes out of scope."
  `(let ((,vec-point2f-var ,vec-point2f))
     (unwind-protect
	 (progn ,@body)
       (del-vec-point2f ,vec-point2f-var))))


(defmacro with-vec-rect ((vec-rect-var vec-rect) &body body)
  "Ensures DEL-VEC-RECT gets called 
   when VECTOR-RECT goes out of scope."
  `(let ((,vec-rect-var ,vec-rect))
     (unwind-protect
	 (progn ,@body)
       (del-vec-rect ,vec-rect-var))))


(defmacro with-vec-uchar ((vec-uchar-var vec-uchar) &body body)
  "Ensures DEL-VEC-UCHAR gets called 
   when VECTOR-UCHAR goes out of scope."
  `(let ((,vec-uchar-var ,vec-uchar))
     (unwind-protect
	 (progn ,@body)
       (del-vec-uchar ,vec-uchar-var))))


(defmacro with-object ((object-var object) &body body)
  "Ensures DEL gets called when 
   OBJECT goes out of scope."
  `(let ((,object-var ,object))
     (unwind-protect
	 (progn ,@body)
       (del ,object-var))))
