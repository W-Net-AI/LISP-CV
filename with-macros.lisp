;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; with-macros.lisp
;;;; OpenCV bindings
;;;; WITH-* Macros for memory management.


(in-package :lisp-cv)



(defmacro with-bf-matcher (bind &body body)
  "Ensures DEL-BF-MATCHER gets called 
   when BF-MATCHER goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-bf-matcher %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-brisk (bind &body body)
  "Ensures DEL-BRISK gets called 
   when BRISK goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-brisk %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-cascade-classifier (bind &body body)
  "Ensures DEL-CASC-CLASS gets called 
   when CASCADE-CLASSIFIER goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-casc-class %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-dmatch (bind &body body)
  "Ensures DEL-DMATCH gets called 
   when DMATCH goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-dmatch %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-keypoint (bind &body body)
  "Ensures DEL-KP gets called 
   when KEYPOINT goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-kp %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-mat (bind &body body)
  "Ensures DEL-MAT gets called 
   when MAT goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-mat %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-mat-expr (bind &body body)
  "Ensures DEL-MAT-EXPR gets called 
   when MAT-EXPR goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-mat-expr %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-object (bind &body body)
  "Ensures DEL-OBJECT gets called 
   when OBJECT goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-point (bind &body body)
  "Ensures DEL-POINT gets called 
   when POINT goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-point %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-point2d (bind &body body)
  "Ensures DEL-POINT2D gets called 
   when POINT2D goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-point2d %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-point2f (bind &body body)
  "Ensures DEL-POINT2F gets called 
   when POINT2F goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-point2f %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-point3d (bind &body body)
  "Ensures DEL-POINT3D gets called 
   when POINT3D goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-point3d %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-point3f (bind &body body)
  "Ensures DEL-POINT3F gets called 
   when POINT3F goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-point3f %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-point3i (bind &body body)
  "Ensures DEL-POINT3I gets called 
   when POINT3I goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-point3i %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-rect (bind &body body)
  "Ensures DEL-RECT gets called 
   when RECT goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-rect %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-rng (bind &body body)
  "Ensures DEL-RNG gets called 
   when RNG goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-rng %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-rotated-rect (bind &body body)
  "Ensures DEL-ROT-RECT gets called 
   when ROTATED-RECT goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-rot-rect %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-scalar (bind &body body)
  "Ensures DEL-SCALAR gets called 
   when SCALAR goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-scalar %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-size (bind &body body)
  "Ensures DEL-SIZE gets called 
   when SIZE goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-size %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-size2f (bind &body body)
  "Ensures DEL-SIZE2F gets called 
   when SIZE2F goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-size2f %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-surf (bind &body body)
  "Ensures DEL-SURF gets called 
   when SURF goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-surf %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-term-criteria (bind &body body)
  "Ensures DEL-TERM-CRITERIA gets called 
   when TERM-CRITERIA goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-term-crit %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-char (bind &body body)
  "Ensures DEL-VEC-CHAR gets called 
   when VECTOR-CHAR goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-char %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-double (bind &body body)
  "Ensures DEL-VEC-DBL gets called 
   when VECTOR-DOUBLE goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-dbl %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-dmatch (bind &body body)
  "Ensures DEL-VEC-DM gets called 
   when VECTOR-DMATCH goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-dm %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-float (bind &body body)
  "Ensures DEL-VEC-FLT gets called 
   when VECTOR-FLOAT goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-flt %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-int (bind &body body)
  "Ensures DEL-VEC-INT gets called 
   when VECTOR-INT goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-int %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-key-point (bind &body body)
  "Ensures DEL-VEC-KP gets called 
   when VECTOR-KEY-POINT goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-kp %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-mat (bind &body body)
  "Ensures DEL-VEC-MAT gets called 
   when VECTOR-MAT goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-mat %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-point (bind &body body)
  "Ensures DEL-VEC-POINT gets called 
   when VECTOR-POINT goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-point %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-point2f (bind &body body)
  "Ensures DEL-VEC-POINT2F gets called 
   when VECTOR-POINT2F goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-point2f %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-rect (bind &body body)
  "Ensures DEL-VEC-RECT gets called 
   when VECTOR-RECT goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-rect %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-uchar (bind &body body)
  "Ensures DEL-VEC-UCHAR gets called 
   when VECTOR-UCHAR goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-uchar %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-video-capture (bind &body body)
  "Ensures DEL-VID-CAP gets called 
   when VIDEO-CAPTURE goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vid-cap %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-video-writer (bind &body body)
  "Ensures DEL-VID-WRITER gets called 
   when VIDEO-WRITER goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vid-writer %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))

