;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; with-macros.lisp
;;;; OpenCV bindings
;;;; WITH-* Macros for memory management


(in-package :lisp-cv)


(defmacro with-ann-mlp (bind &body body)
  "Ensures DEL-ANN-MLP gets called when 
   a ANN-MLP object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-ann-mlp %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-ann-mlp-train-params (bind &body body)
  "Ensures DEL-ANN-MLP-TRAIN-PARAMS gets called when 
   a ANN-MLP-TRAIN-PARAMS object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-ann-mlp-train-params %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-cascade-classifier (bind &body body)
  "Ensures DEL-CASC-CLASS gets called when a
   CASCADE-CLASSIFIER object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-casc-class %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-dmatch (bind &body body)
  "Ensures DEL-DMATCH gets called when 
   a DMATCH object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-dmatch %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-d-tree (bind &body body)
  "Ensures DEL-D-TREE gets called when 
   a D-TREE object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-d-tree %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-d-tree-params (bind &body body)
  "Ensures DEL-D-TREE-PARAMS gets called when 
   a D-TREE-PARAMS object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-d-tree-params %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-feature-2d (bind &body body)
  "Ensures DEL-FEATURE-2D gets called when 
   a FEATURE-2D object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-feature-2d %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-hog-descriptor (bind &body body)
  "Ensures DEL-HOG-DESCRIPTOR gets called when 
   a HOG-DESCRIPTOR object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-hog-descriptor %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-keypoint (bind &body body)
  "Ensures DEL-KP gets called when 
   a KEYPOINT object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-kp %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-k-nearest (bind &body body)
  "Ensures DEL-K-NEAREST gets called when 
   a K-NEAREST object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-k-nearest %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-mat (bind &body body)
  "Ensures DEL-MAT gets called when 
   a MAT object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-mat %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-mat-expr (bind &body body)
  "Ensures DEL-MAT-EXPR gets called when 
   a MAT-EXPR object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-mat-expr %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-normal-bayes-classifier (bind &body body)
  "Ensures DEL-NORMAL-BAYES-CLASSIFIER gets called when 
   a NORMAL-BAYES-CLASSIFIER object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-normal-bayes-classifier %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-object (bind &body body)
  "Ensures DEL-OBJECT gets called when 
   a OBJECT object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-point (bind &body body)
  "Ensures DEL-POINT gets called when 
   a POINT object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-point %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-point-2d (bind &body body)
  "Ensures DEL-POINT-2D gets called when 
   a POINT-2D object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-point-2d %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-point-2f (bind &body body)
  "Ensures DEL-POINT-2F gets called when 
   a POINT-2F object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-point-2f %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-point-3d (bind &body body)
  "Ensures DEL-POINT-3D gets called when 
   a POINT-3D object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-point-3d %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-point-3f (bind &body body)
  "Ensures DEL-POINT-3F gets called when 
   a POINT-3F object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-point-3f %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-point-3i (bind &body body)
  "Ensures DEL-POINT-3I gets called when 
   a POINT-3I object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-point-3i %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-range (bind &body body)
  "Ensures DEL-RANGE gets called when 
   a RANGE object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-range %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-rect (bind &body body)
  "Ensures DEL-RECT gets called when 
   a RECT object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-rect %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-rng (bind &body body)
  "Ensures DEL-RNG gets called when 
   a RNG object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-rng %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-rotated-rect (bind &body body)
  "Ensures DEL-ROT-RECT gets called when 
   a ROTATED-RECT object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-rot-rect %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-scalar (bind &body body)
  "Ensures DEL-SCALAR gets called when 
   a SCALAR object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-scalar %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-size (bind &body body)
  "Ensures DEL-SIZE gets called when 
   a SIZE object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-size %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-size-2f (bind &body body)
  "Ensures DEL-SIZE-2F gets called when 
   a SIZE-2F object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-size-2f %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))

(defmacro with-string (bind &body body)
  "Ensures DEL-STD-STRING gets called when 
   a *STRING object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-std-string %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-term-criteria (bind &body body)
  "Ensures DEL-TERM-CRITERIA gets called when 
   a TERM-CRITERIA object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-term-crit %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-2b (bind &body body)
  "Ensures DEL-VEC-2B gets called when 
   a VEC-2B object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-2b %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-3b (bind &body body)
  "Ensures DEL-VEC-3B gets called when 
   a VEC-3B object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-3b %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-4b (bind &body body)
  "Ensures DEL-VEC-4B gets called when 
   a VEC-4B object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-4b %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-2d (bind &body body)
  "Ensures DEL-VEC-2D gets called when 
   a VEC-2D object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-2d %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-3d (bind &body body)
  "Ensures DEL-VEC-3D gets called when 
   a VEC-3D object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-3d %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-4d (bind &body body)
  "Ensures DEL-VEC-4D gets called when 
   a VEC-4D object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-4d %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-6d (bind &body body)
  "Ensures DEL-VEC-6D gets called when 
   a VEC-6D object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-6d %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-2f (bind &body body)
  "Ensures DEL-VEC-2F gets called when 
   a VEC-2F object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-2f %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-3f (bind &body body)
  "Ensures DEL-VEC-3F gets called when 
   a VEC-3F object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-3f %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-4f (bind &body body)
  "Ensures DEL-VEC-4F gets called when 
   a VEC-4F object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-4f %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-6f (bind &body body)
  "Ensures DEL-VEC-6F gets called when 
   a VEC-6F object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-6f %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-2i (bind &body body)
  "Ensures DEL-VEC-2I gets called when 
   a VEC-2I object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-2i %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-3i (bind &body body)
  "Ensures DEL-VEC-3I gets called when 
   a VEC-3I object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-3i %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-4i (bind &body body)
  "Ensures DEL-VEC-4I gets called when 
   a VEC-4I object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-4i %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-6i (bind &body body)
  "Ensures DEL-VEC-6I gets called when 
   a VEC-6I object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-6i %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-8i (bind &body body)
  "Ensures DEL-VEC-8I gets called when 
   a VEC-8I object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-8i %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-2s (bind &body body)
  "Ensures DEL-VEC-2S gets called when 
   a VEC-2S object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-2s %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-3s (bind &body body)
  "Ensures DEL-VEC-3S gets called when 
   a VEC-3S object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-3s %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-4s (bind &body body)
  "Ensures DEL-VEC-4S gets called when 
   a VEC-4S object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-4s %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-2w (bind &body body)
  "Ensures DEL-VEC-2W gets called when 
   a VEC-2W object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-2w %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-3w (bind &body body)
  "Ensures DEL-VEC-3W gets called when 
   a VEC-3W object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-3w %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vec-4w (bind &body body)
  "Ensures DEL-VEC-4W gets called when 
   a VEC-4W object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vec-4w %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-char (bind &body body)
  "Ensures DEL-VECTOR-CHAR gets called when a a VEC
   TOR-CHAR object object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-char %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-double (bind &body body)
  "Ensures DEL-VECTOR-DBL gets called when 
   a VECTOR-DOUBLE object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-double %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-dmatch (bind &body body)
  "Ensures DEL-VECTOR-DM gets called when 
   a VECTOR-DMATCH object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-dmatch %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-float (bind &body body)
  "Ensures DEL-VECTOR-FLT gets called when 
   a VECTOR-FLOAT object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-float %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-int (bind &body body)
  "Ensures DEL-VECTOR-INT gets called when 
   a VECTOR-INT object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-int %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-key-point (bind &body body)
  "Ensures DEL-VECTOR-KP gets called when 
   a VECTOR-KEY-POINT object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-key-point %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-mat (bind &body body)
  "Ensures DEL-VECTOR-MAT gets called when 
   a VECTOR-MAT object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-mat %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-point (bind &body body)
  "Ensures DEL-VECTOR-POINT gets called when 
   a VECTOR-POINT object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-point %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-point-2f (bind &body body)
  "Ensures DEL-VECTOR-POINT-2F gets called when 
   a VECTOR-POINT-2F object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-point-2f %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-rect (bind &body body)
  "Ensures DEL-VECTOR-RECT gets called when 
   a VECTOR-RECT object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-rect %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-uchar (bind &body body)
  "Ensures DEL-VECTOR-UCHAR gets called when 
   a VECTOR-UCHAR object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-uchar %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-2b (bind &body body)
  "Ensures DEL-VECTOR-VEC-2B gets called when 
   a VECTOR-VEC-2B object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-2b %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-3b (bind &body body)
  "Ensures DEL-VECTOR-VEC-3B gets called when 
   a VECTOR-VEC-3B object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-3b %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-4b (bind &body body)
  "Ensures DEL-VECTOR-VEC-4B gets called when 
   a VECTOR-VEC-4B object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-4b %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-2d (bind &body body)
  "Ensures DEL-VECTOR-VEC-2D gets called when 
   a VECTOR-VEC-2D object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-2d %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-3d (bind &body body)
  "Ensures DEL-VECTOR-VEC-3D gets called when 
   a VECTOR-VEC-3D object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-3d %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-4d (bind &body body)
  "Ensures DEL-VECTOR-VEC-4D gets called when 
   a VECTOR-VEC-4D object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-4d %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-6d (bind &body body)
  "Ensures DEL-VECTOR-VEC-6D gets called when 
   a VECTOR-VEC-6D object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-6d %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-2f (bind &body body)
  "Ensures DEL-VECTOR-VEC-2F gets called when 
   a VECTOR-VEC-2F object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-2f %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-3f (bind &body body)
  "Ensures DEL-VECTOR-VEC-3F gets called when 
   a VECTOR-VEC-3F object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-3f %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-4f (bind &body body)
  "Ensures DEL-VECTOR-VEC-4F gets called when 
   a VECTOR-VEC-4F object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-4f %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-6f (bind &body body)
  "Ensures DEL-VECTOR-VEC-6F gets called when 
   a VECTOR-VEC-6F object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-6f %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-2i (bind &body body)
  "Ensures DEL-VECTOR-VEC-2I gets called when 
   a VECTOR-VEC-2I object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-2i %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-3i (bind &body body)
  "Ensures DEL-VECTOR-VEC-3I gets called when 
   a VECTOR-VEC-3I object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-3i %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-4i (bind &body body)
  "Ensures DEL-VECTOR-VEC-4I gets called when 
   a VECTOR-VEC-4I object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-4i %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-6i (bind &body body)
  "Ensures DEL-VECTOR-VEC-6I gets called when 
   a VECTOR-VEC-6I object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-6i %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-8i (bind &body body)
  "Ensures DEL-VECTOR-VEC-8I gets called when 
   a VECTOR-VEC-8I object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-8i %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-2s (bind &body body)
  "Ensures DEL-VECTOR-VEC-2S gets called when 
   a VECTOR-VEC-2S object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-2s %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-3s (bind &body body)
  "Ensures DEL-VECTOR-VEC-3S gets called when 
   a VECTOR-VEC-3S object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-3s %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-4s (bind &body body)
  "Ensures DEL-VECTOR-VEC-4S gets called when 
   a VECTOR-VEC-4S object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-4s %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-2w (bind &body body)
  "Ensures DEL-VECTOR-VEC-2W gets called when 
   a VECTOR-VEC-2W object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-2w %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-3w (bind &body body)
  "Ensures DEL-VECTOR-VEC-3W gets called when 
   a VECTOR-VEC-3W object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-3w %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-vector-vec-4w (bind &body body)
  "Ensures DEL-VECTOR-VEC-4W gets called when 
   a VECTOR-VEC-4W object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vector-vec-4w %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-video-capture (bind &body body)
  "Ensures DEL-VID-CAP gets called when 
   a VIDEO-CAPTURE object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vid-cap %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))


(defmacro with-video-writer (bind &body body)
  "Ensures DEL-VID-WRITER gets called when 
   a VIDEO-WRITER object goes out of scope."
  `(let* ,(mapcar #!(cons (car %1) (cdr %1)) bind)
     (unwind-protect (progn ,@body)
       (mapcar #!(del-vid-writer %1) ,(cons 'list (mapcar #!(car %1) bind)))
       (values))))

