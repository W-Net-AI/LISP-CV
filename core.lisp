;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; core.lisp
;;;; OpenCV bindings
;;;; The Core Functionality.


(in-package :lisp-cv)


;; Default parameters

(defvar *camera-index* 0)

(defvar *default-width* 640)

(defvar *default-height* 480)

(defvar *frames-per-second* 30)

(defvar *millis-per-frame* (round (/ 1000 *frames-per-second*)))

(defparameter *personalize-print-2d-mat* "#2M")

(defparameter *personalize-print-3d-mat* "#3M")
   
(defparameter *personalize-print-point* "#")

(defparameter *personalize-print-point-2d* "#")

(defparameter *personalize-print-point-2f* "#")

(defparameter *personalize-print-point-3d* "#")

(defparameter *personalize-print-point-3f* "#")

(defparameter *personalize-print-point-3i* "#")

(defparameter *personalize-print-scalar* "#")

(defparameter *personalize-print-vec-2b* "#")

(defparameter *personalize-print-vec-2d* "#")

(defparameter *personalize-print-vec-2f* "#")

(defparameter *personalize-print-vec-2i* "#")

(defparameter *personalize-print-vec-2s* "#")

(defparameter *personalize-print-vec-2w* "#")

(defparameter *personalize-print-vec-3b* "#")

(defparameter *personalize-print-vec-3d* "#")

(defparameter *personalize-print-vec-3f* "#")

(defparameter *personalize-print-vec-3i* "#")

(defparameter *personalize-print-vec-3s* "#")

(defparameter *personalize-print-vec-3w* "#")

(defparameter *personalize-print-vec-4b* "#")

(defparameter *personalize-print-vec-4d* "#")

(defparameter *personalize-print-vec-4f* "#")

(defparameter *personalize-print-vec-4i* "#")

(defparameter *personalize-print-vec-4s* "#")

(defparameter *personalize-print-vec-4w* "#")

(defparameter *personalize-print-vec-6d* "#")

(defparameter *personalize-print-vec-6f* "#")

(defparameter *personalize-print-vec-6i* "#")

(defparameter *personalize-print-vec-8i* "#")


;; Change default parameters

(defun def-params (width height &optional camera-index fps)
       (setf *default-width* width)
       (setf *default-height* height)
       (if camera-index (setf *camera-index* camera-index))
       (if fps (setf *frames-per-second* fps))
       (format t "*default-width* = ~a~%" width)
       (format t "*default-height* = ~a~%" height)
       (if camera-index (format t "*camera-index* = ~a~%" camera-index)
	   (format t "*camera-index* = ~a~%" *camera-index*))
	   (if fps (format t "*frames-per-second* = ~a~%" fps)
	       (format t "*frames-per-second* = ~a~%" *frames-per-second*)))



;; Live code editing

(defmacro continuable (&body body)
	  `(restart-case
	    (progn ,@body)
	    (continue () :report "Continue")))


(defun update-swank ()
       "Grabs SWANK connections and tells it to handle requests. 
   Call this every loop in the main loop of your program"
   (continuable
    (let ((connection (or swank::*emacs-connection*
			  (swank::default-connection))))
			  (when connection
			    (swank::handle-requests connection t)))))


;; C-Interop - *String


;; Version for internal use.

;; stdstring* create_std_string()
  "Creates a *STRING object."
(defcfun ("create_std_string" %string) *string)


;; Version for external use.

;; string* std_cstringToString(char* s, size_t len) 
(defcfun ("cstring_to_std_string" c-string-to-string) *string
  "Converts C string to C++"
  (s :string)
  (len :unsigned-int))


;; Version for internal use.

;; string* std_cstringToString(char* s, size_t len) 
(defcfun ("cstring_to_std_string" %c-string-to-string) (cv::*string :garbage-collect t)
  "Converts C string to C++"
  (s :string)
  (len :unsigned-int))


;;; C-Interop - CV-MAT

;; CvMat* cv_Mat_to_CvMat(Mat* self)
(defcfun ("cv_Mat_to_CvMat" mat-to-cv-mat) mat-struct
  (self mat))


;; C-Interop - CV-TERM-CRITERIA 

;; CvTermCriteria cv_TermCriteria_to_CvTermCriteria(TermCriteria* self)
(defcfun ("cv_TermCriteria_to_CvTermCriteria" term-crit-to-cv-term-crit) (:pointer (:struct term-criteria-struct))
  (self term-criteria))


;; C-Interop - MEM-AREF and MEM-REF macros with C-POINTER reader.

(defun resolve-pointer (ptr)
  (if (pointerp ptr) ptr (c-pointer ptr)))

(defmacro @ (ptr type &optional (index 0))
  `(mem-aref (resolve-pointer ,ptr) ,type ,index))

(defmacro @@ (ptr type &optional (offset 0))
  `(mem-ref (resolve-pointer ,ptr) ,type ,offset))


;;; Basic Structures


;; size_t cv_Mat_get_Step(Mat* self) 
(defcfun ("cv_Mat_get_Step" *step) :unsigned-int
  "Used to compute address of a matrix element"
  (self mat))


;; Scalar trace(InputArray mtx)
;; Scalar* cv_trace(Mat* mtx)
(defcfun ("cv_trace" *trace) scalar
  "Returns the trace of a matrix."
  (mtx mat))


;; Mat* force(MatExpr* expr)
(defcfun ("force" >>) mat
  "Coerces a MAT-EXPR to a MAT. 
   This is a shorthand version of the FORCE function."
   (mat-expr mat-expr))


;; MatExpr* promote(Mat* m) 
(defcfun ("promote" <<) mat-expr
  "Converts a MAT to a MAT-EXPR.
   This is a shorthand version of the PROMOTE function." 
   (mat mat))


;; MatExpr + operator
;; MatExpr* cv_Mat_add(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_add" add) mat-expr
  (m1 mat)
  (m2 mat))


;; Mat& Mat::adjustROI(int dtop, int dbottom, int dleft, int dright)
;; Mat* cv_Mat_adjustROI(Mat* self, int dtop, int dbottom, int dleft, int dright) 
(defcfun ("cv_Mat_adjustROI" adjust-roi) mat
  "Adjusts a submatrix size and position within the parent matrix."
  (self mat)
  (dtop :int)
  (dbottom :int)
  (dleft :int)
  (dright :int))


;; _Tp area() const
;; int cv_Size_area(Size* self)
(defcfun ("cv_Size_area" area) :int
  "Gets the area of a SIZE construct"
  (self size))


;; _Tp area() const
;; float cv_Size2f_area(Size2f* self) 
(defcfun ("cv_Size2f_area" area-2f) :float
  "Gets the area of a SIZE-2F construct"
  (self size-2f))


(defun arr-to-mat (arr)

  (let* ((array-dimensions (array-dimensions arr))
	 (x (car array-dimensions))
	 (y (cadr array-dimensions))
	 (z (caddr array-dimensions))
         (channels (if z z 1))
	 (area (* x y))
         (mat 0)
	 (ptr 0)
         (cffi-type  
	  (typecase arr
	    ((simple-array t) :uchar)
	    ((simple-array (unsigned-byte 8)) :uchar)
	    ((simple-array (signed-byte 8)) :char)
	    ((simple-array (unsigned-byte 16)) :ushort)
	    ((simple-array (signed-byte 16)) :short)
	    ((simple-array (signed-byte 32)) :int)
	    ((simple-array single-float) :float)
	    ((simple-array double-float) :double))))

    (case cffi-type

      (:uchar 
       (case channels (1
		       (setf mat (mat-typed x y +8uc1+)))
	     ((2 3 4)
	      (setf mat (mat-typed x y (case channels 
					 (2 #.+8uc2+)
					 (3 #.+8uc3+)
					 (4 #.+8uc4+))))))
       (setf ptr (%ptr mat 0))
       (dotimes (n (* area channels))
	 (setf (mem-aref ptr :uchar n) (row-major-aref arr n)))mat)

      (:char 
       (case channels (1
		       (setf mat (mat-typed x y +8sc1+)))
	     ((2 3 4)
	      (setf mat (mat-typed x y (case channels 
					 (2 #.+8sc2+)
					 (3 #.+8sc3+)
					 (4 #.+8sc4+))))))
       (setf ptr (%ptr mat 0))
       (dotimes (n (* area channels))
	 (setf (mem-aref ptr :char n) (row-major-aref arr n)))mat)

      (:ushort 
       (case channels (1
		       (setf mat (mat-typed x y +16uc1+)))
	     ((2 3 4)
	      (setf mat (mat-typed x y (case channels 
					 (2 #.+16uc2+)
					 (3 #.+16uc3+)
					 (4 #.+16uc4+))))))
       (setf ptr (%ptr mat 0))
       (dotimes (n (* area channels))
	 (setf (mem-aref ptr :ushort n) (row-major-aref arr n)))mat)

      (:short 
       (case channels (1
		       (setf mat (mat-typed x y +16sc1+)))
	     ((2 3 4)
	      (setf mat (mat-typed x y (case channels 
					 (2 #.+16sc2+)
					 (3 #.+16sc3+)
					 (4 #.+16sc4+))))))
       (setf ptr (%ptr mat 0))
       (dotimes (n (* area channels))
	 (setf (mem-aref ptr :short n) (row-major-aref arr n)))mat)

      (:int 
       (case channels (1
		       (setf mat (mat-typed x y +32sc1+)))
	     ((2 3 4)
	      (setf mat (mat-typed x y (case channels 
					 (2 #.+32sc2+)
					 (3 #.+32sc3+)
					 (4 #.+32sc4+))))))
       (setf ptr (%ptr mat 0))
       (dotimes (n (* area channels))
	 (setf (mem-aref ptr :int n) (row-major-aref arr n)))mat)

      (:float 
       (case channels (1
		       (setf mat (mat-typed x y +32fc1+)))
	     ((2 3 4)
	      (setf mat (mat-typed x y (case channels 
					 (2 #.+32fc2+)
					 (3 #.+32fc3+)
					 (4 #.+32fc4+))))))
       (setf ptr (%ptr mat 0))
       (dotimes (n (* area channels))
	 (setf (mem-aref ptr :float n) (row-major-aref arr n)))mat)

      (:double 
       (case channels (1
		       (setf mat (mat-typed x y +64fc1+)))
	     ((2 3 4)
	      (setf mat (mat-typed x y (case channels 
					 (2 #.+64fc2+)
					 (3 #.+64fc3+)
					 (4 #.+64fc4+))))))
       (setf ptr (%ptr mat 0))
       (dotimes (n (* area channels))
	 (setf (mem-aref ptr :double n) (row-major-aref arr n)))mat))))


;; Mat* cv_Mat_assign(Mat* self, Mat* m) 
(defcfun ("cv_Mat_assign" assgn) mat
  "Assign matrix data to another matrix."
  (self mat)
  (m mat))


;; Mat* cv_Mat_assignVal(Mat* self, Scalar* s)
(defcfun ("cv_Mat_assignVal" assgn-val) mat
  "Assign a scalar value to a matrix."
  (self mat)
  (s scalar))


;; uchar* ptr(int i0=0)
;; uchar* cv_Mat_ptr_index(Mat* self, int i)
(defmacro at (&optional self i j type)
	  `(mem-aref (%ptr ,self ,i) ,type ,j))


;; template<typename T> T& Mat::at(int i, int j)
;; char cv_Mat_at_char(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_char_2" at-char-2) :pointer
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))


;; template<typename T> T& Mat::at(int i, int j, int k)
;; char &cv_Mat_at_char_3(Mat* self, int i, int j, int k)
(defcfun ("cv_Mat_at_char_3" at-char-3) :pointer
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int)
  (k :int))
 

(defun at-char (self i j &optional k)
  (if k (@ (at-char-3 self i j k) :char)
      (@ (at-char-2 self i j) :char))) 


(defun (setf at-char) (val self i j &optional k)
  (if k (setf (@ (at-char-3 self i j k) :char) val)
  (setf (@ (at-char-2 self i j) :char) val)))


;; template<typename T> T& Mat::at(int i, int j) 
;; double cv_Mat_at_double_2(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_double_2" at-double-2) :pointer
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 

;; template<typename T> T& Mat::at(int i, int j, int k) 
;; double &cv_Mat_at_double_3(Mat* self, int i, int j, int k)
(defcfun ("cv_Mat_at_double_3" at-double-3) :pointer
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int)
  (k :int))

 
(defun at-double (self i j &optional k)
  (if k (@ (at-double-3 self i j k) :double)
      (@ (at-double-2 self i j) :double))) 


(defun (setf at-double) (val self i j &optional k)
  (if k (setf (@ (at-double-3 self i j k) :double) val)
  (setf (@ (at-double-2 self i j) :double) val)))


;; template<typename T> T& Mat::at(int i, int j)
;; float cv_Mat_at_float_2(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_float_2" at-float-2) :pointer
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; template<typename T> T& Mat::at(int i, int j, int k)
;; float &cv_Mat_at_float_3(Mat* self, int i, int j, int k)
(defcfun ("cv_Mat_at_float_3" at-float-3) :pointer
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int)
  (k :int))
 

(defun at-float (self i j &optional k)
  (if k (@ (at-float-3 self i j k) :float)
      (@ (at-float-2 self i j) :float))) 


(defun (setf at-float) (val self i j &optional k)
  (if k (setf (@ (at-float-3 self i j k) :float) val)
  (setf (@ (at-float-2 self i j) :float) val)))


;; template<typename T> T& Mat::at(int i, int j)
;; int cv_Mat_at_int_2(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_int_2" at-int-2) :pointer
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 

;; template<typename T> T& Mat::at(int i, int j, int k)
;; int &cv_Mat_at_int_3(Mat* self, int i, int j, int k)
(defcfun ("cv_Mat_at_int_3" at-int-3) :pointer
  "RReturns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int)
  (k :int))


(defun at-int (self i j &optional k)
  (if k (@ (at-int-3 self i j k) :int)
      (@ (at-int-2 self i j) :int))) 


(defun (setf at-int) (val self i j &optional k)
  (if k (setf (@ (at-int-3 self i j k) :int) val)
  (setf (@ (at-int-2 self i j) :int) val)))


;; template<typename T> T& Mat::at(int i, int j)
;; short cv_Mat_at_short_2(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_short_2" at-short-2) :pointer
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 

;; template<typename T> T& Mat::at(int i, int j, int k) 
;; short &cv_Mat_at_short_3(Mat* self, int i, int j, int k)
(defcfun ("cv_Mat_at_short_3" at-short-3) :pointer
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int)
  (k :int))


(defun at-short (self i j &optional k)
  (if k (@ (at-short-3 self i j k) :short)
      (@ (at-short-2 self i j) :short))) 


(defun (setf at-short) (val self i j &optional k)
  (if k (setf (@ (at-short-3 self i j k) :short) val)
  (setf (@ (at-short-2 self i j) :short) val)))


;; template<typename T> T& Mat::at(int i, int j)
;; uchar cv_Mat_at_uchar_2(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_uchar_2" at-uchar-2) :pointer
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))


;; template<typename T> T& Mat::at(int i, int j, int k)
;; uchar &cv_Mat_at_uchar_3(Mat* self, int i, int j, int k)
(defcfun ("cv_Mat_at_uchar_3" at-uchar-3) :pointer
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int)
  (k :int))


(defun at-uchar (self i j &optional k)
  (if k (@ (at-uchar-3 self i j k) :uchar)
      (@ (at-uchar-2 self i j) :uchar))) 


(defun (setf at-uchar) (val self i j &optional k)
  (if k (setf (@ (at-uchar-3 self i j k) :uchar) val)
  (setf (@ (at-uchar-2 self i j) :uchar) val)))


;; template<typename T> T& Mat::at(int i, int j)
;; ushort cv_Mat_at_ushort_2(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_ushort_2" at-ushort-2) :pointer
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; template<typename T> T& Mat::at(int i, int j, int k)
;; ushort &cv_Mat_at_ushort_3(Mat* self, int i, int j, int k)
(defcfun ("cv_Mat_at_ushort_3" at-ushort-3) :pointer
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int)
  (k :int))


(defun at-ushort (self i j &optional k)
  (if k (@ (at-ushort-3 self i j k) :ushort)
      (@ (at-ushort-2 self i j) :ushort))) 


(defun (setf at-ushort) (val self i j &optional k)
  (if k (setf (@ (at-ushort-3 self i j k) :ushort) val)
  (setf (@ (at-ushort-2 self i j) :ushort) val)))


;; template<typename T> T& Mat::at(int i, int j)
;; Scalar* cv_Mat_at_Scalar(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Scalar" at-scalar) scalar
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Scalar_set_Val(Mat* self, int i, int j, Scalar* val)
(defcfun ("cv_Mat_at_Scalar_set_Val" at-scalar-set-val) scalar
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val scalar))


(defun (setf at-scalar) (val self i j)
  (at-scalar-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Point* cv_Mat_at_Point(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Point" at-point) point
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Point_set_Val(Mat* self, int i, int j, Point* val)
(defcfun ("cv_Mat_at_Point_set_Val" at-point-set-val) point
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val point))


(defun (setf at-point) (val self i j)
  (at-point-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Point2d* cv_Mat_at_Point2d(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Point2d" at-point-2d) point-2d
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Point2d_set_Val(Mat* self, int i, int j, Point2d* val)
(defcfun ("cv_Mat_at_Point2d_set_Val" at-point-2d-set-val) point-2d
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val point-2d))


(defun (setf at-point-2d) (val self i j)
  (at-point-2d-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Point2f* cv_Mat_at_Point2f(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Point2f" at-point-2f) point-2f
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Point2f_set_Val(Mat* self, int i, int j, Point2f* val)
(defcfun ("cv_Mat_at_Point2f_set_Val" at-point-2f-set-val) point-2f
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val point-2f))


(defun (setf at-point-2f) (val self i j)
  (at-point-2f-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Point3d* cv_Mat_at_Point3d(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Point3d" at-point-3d) point-3d
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Point3d_set_Val(Mat* self, int i, int j, Point3d* val)
(defcfun ("cv_Mat_at_Point3d_set_Val" at-point-3d-set-val) point-3d
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val point-3d))


(defun (setf at-point-3d) (val self i j)
  (at-point-3d-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Point3f* cv_Mat_at_Point3f(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Point3f" at-point-3f) point-3f
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Point3f_set_Val(Mat* self, int i, int j, Point3f* val)
(defcfun ("cv_Mat_at_Point3f_set_Val" at-point-3f-set-val) point-3f
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val point-3f))


(defun (setf at-point-3f) (val self i j)
  (at-point-3f-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Point3i* cv_Mat_at_Point3i(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Point3i" at-point-3i) point-3i
  "RReturns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Point3i_set_Val(Mat* self, int i, int j, Point3i* val)
(defcfun ("cv_Mat_at_Point3i_set_Val" at-point-3i-set-val) point-3i
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val point-3i))


(defun (setf at-point-3i) (val self i j)
  (at-point-3i-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Vec2b* cv_Mat_at_Vec2b(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec2b" at-vec-2b) vec-2b
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Vec2b_set_Val(Mat* self, int i, int j, Vec2b* val)
(defcfun ("cv_Mat_at_Vec2b_set_Val" at-vec-2b-set-val) vec-2b
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-2b))


(defun (setf at-vec-2b) (val self i j)
  (at-vec-2b-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Vec2d cv_Mat_at_Vec2d(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec2d" at-vec-2d) vec-2d
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Vec2d_set_Val(Mat* self, int i, int j, Vec2d* val)
(defcfun ("cv_Mat_at_Vec2d_set_Val" at-vec-2d-set-val) vec-2d
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-2d))


(defun (setf at-vec-2d) (val self i j)
  (at-vec-2d-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Vec2f* cv_Mat_at_Vec2f(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec2f" at-vec-2f) vec-2f
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Vec2f_set_Val(Mat* self, int i, int j, Vec2f* val)
(defcfun ("cv_Mat_at_Vec2f_set_Val" at-vec-2f-set-val) vec-2f
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-2f))


(defun (setf at-vec-2f) (val self i j)
  (at-vec-2f-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Vec2i* cv_Mat_at_Vec2i(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec2i" at-vec-2i) vec-2i
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Vec2i_set_Val(Mat* self, int i, int j, Vec2i* val)
(defcfun ("cv_Mat_at_Vec2i_set_Val" at-vec-2i-set-val) vec-2i
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-2i))


(defun (setf at-vec-2i) (val self i j)
  (at-vec-2i-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Vec2s* cv_Mat_at_Vec2s(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec2s" at-vec-2s) vec-2s
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 

;; void cv_Mat_at_Vec2s_set_Val(Mat* self, int i, int j, Vec2s* val)
(defcfun ("cv_Mat_at_Vec2s_set_Val" at-vec-2s-set-val) vec-2s
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-2s))


(defun (setf at-vec-2s) (val self i j)
  (at-vec-2s-set-val self i j val))

 
;; template<typename T> T& Mat::at(int i, int j)
;; Vec2w* cv_Mat_at_Vec2w(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec2w" at-vec-2w) vec-2w
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Vec2w_set_Val(Mat* self, int i, int j, Vec2w* val)
(defcfun ("cv_Mat_at_Vec2w_set_Val" at-vec-2w-set-val) vec-2w
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-2w))


(defun (setf at-vec-2w) (val self i j)
  (at-vec-2w-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Vec3b* cv_Mat_at_Vec3b(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec3b" at-vec-3b) vec-3b
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))


;; void cv_Mat_at_Vec3b_set_Val(Mat* self, int i, int j, Vec3b* val)
(defcfun ("cv_Mat_at_Vec3b_set_Val" at-vec-3b-set-val) vec-3b
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-3b))


(defun (setf at-vec-3b) (val self i j)
  (at-vec-3b-set-val self i j val))
 
 
;; template<typename T> T& Mat::at(int i, int j)
;; Vec3d* cv_Mat_at_Vec3d(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec3d" at-vec-3d) vec-3d
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Vec3d_set_Val(Mat* self, int i, int j, Vec3d* val)
(defcfun ("cv_Mat_at_Vec3d_set_Val" at-vec-3d-set-val) vec-3d
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-3d))


(defun (setf at-vec-3d) (val self i j)
  (at-vec-3d-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Vec3f* cv_Mat_at_Vec3f(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec3f" at-vec-3f) vec-3f
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Vec3f_set_Val(Mat* self, int i, int j, Vec3f* val)
(defcfun ("cv_Mat_at_Vec3f_set_Val" at-vec-3f-set-val) vec-3f
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-3f))


(defun (setf at-vec-3f) (val self i j)
  (at-vec-3f-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Vec3i* cv_Mat_at_Vec3i(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec3i" at-vec-3i) vec-3i
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Vec3i_set_Val(Mat* self, int i, int j, Vec3i* val)
(defcfun ("cv_Mat_at_Vec3i_set_Val" at-vec-3i-set-val) vec-3i
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-3i))


(defun (setf at-vec-3i) (val self i j)
  (at-vec-3i-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Vec3s* cv_Mat_at_Vec3s(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec3s" at-vec-3s) vec-3s
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Vec3s_set_Val(Mat* self, int i, int j, Vec3s* val)
(defcfun ("cv_Mat_at_Vec3s_set_Val" at-vec-3s-set-val) vec-3s
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-3s))


(defun (setf at-vec-3s) (val self i j)
  (at-vec-3s-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Vec3w* cv_Mat_at_Vec3w(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec3w" at-vec-3w) vec-3w
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Vec3w_set_Val(Mat* self, int i, int j, Vec3w* val)
(defcfun ("cv_Mat_at_Vec3w_set_Val" at-vec-3w-set-val) vec-3w
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-3w))


(defun (setf at-vec-3w) (val self i j)
  (at-vec-3w-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Vec4b* cv_Mat_at_Vec4b(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec4b" at-vec-4b) vec-4b
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Vec4b_set_Val(Mat* self, int i, int j, Vec4b* val)
(defcfun ("cv_Mat_at_Vec4b_set_Val" at-vec-4b-set-val) vec-4b
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-4b))


(defun (setf at-vec-4b) (val self i j)
  (at-vec-4b-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Vec4d* cv_Mat_at_Vec4d(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec4d" at-vec-4d) vec-4d
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Vec4d_set_Val(Mat* self, int i, int j, Vec4d* val)
(defcfun ("cv_Mat_at_Vec4d_set_Val" at-vec-4d-set-val) vec-4d
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-4d))


(defun (setf at-vec-4d) (val self i j)
  (at-vec-4d-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Vec4f* cv_Mat_at_Vec4f(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec4f" at-vec-4f) vec-4f
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 

;; void cv_Mat_at_Vec4f_set_Val(Mat* self, int i, int j, Vec4f* val)
(defcfun ("cv_Mat_at_Vec4f_set_Val" at-vec-4f-set-val) vec-4f
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-4f))


(defun (setf at-vec-4f) (val self i j)
  (at-vec-4f-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Vec4i cv_Mat_at_Vec4i(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec4i" at-vec-4i) vec-4i
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int))
 
 
;; void cv_Mat_at_Vec4i_set_Val(Mat* self, int i, int j, Vec4i* val)
(defcfun ("cv_Mat_at_Vec4i_set_Val" at-vec-4i-set-val) vec-4i
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-4i))


(defun (setf at-vec-4i) (val self i j)
  (at-vec-4i-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Vec4s cv_Mat_at_Vec4s(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec4s" at-vec-4s) vec-4s
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int)) 


;; void cv_Mat_at_Vec4s_set_Val(Mat* self, int i, int j, Vec4s* val)
(defcfun ("cv_Mat_at_Vec4s_set_Val" at-vec-4s-set-val) vec-4s
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-4s))


(defun (setf at-vec-4s) (val self i j)
  (at-vec-4s-set-val self i j val))


;; template<typename T> T& Mat::at(int i, int j)
;; Vec4s cv_Mat_at_Vec4w(Mat* self, int row, int col)
(defcfun ("cv_Mat_at_Vec4w" at-vec-4w) vec-4w
  "Returns a reference to the specified array element."
  (self mat)
  (i :int)
  (j :int)) 


;; void cv_Mat_at_Vec4w_set_Val(Mat* self, int i, int j, Vec4w* val)
(defcfun ("cv_Mat_at_Vec4w_set_Val" at-vec-4w-set-val) vec-4w
  "Sets an array element."
  (self mat)
  (i :int)
  (j :int)
  (val vec-4w))


(defun (setf at-vec-4w) (val self i j)
  (at-vec-4w-set-val self i j val))


;; int cv_Mat_channels(Mat* self)
(defcfun ("cv_Mat_channels" channels) :int
  (self mat))


(defun class-id (self)
  (mem-aref (c-pointer self) :int 6))


;; Mat Mat::clone() const
;; Mat* cv_Mat_clone(Mat* self) 
(defcfun ("cv_Mat_clone" mat-clone) mat
  (self mat))


;; Rect::Rect(int x, int y, int width, int height)
;; int x, y, width, height
;; Rect* cv_Rect_clone(Rect* self)
(defcfun ("cv_Rect_clone" rect-clone) rect
  (self rect))


;; Mat Mat::colRange(int startcol, int endcol) const
;; Mat* cv_Mat_getColRange(Mat* self, int startcol, int endrow)
(defcfun ("cv_Mat_getColRange" col-range) mat
  "Creates a matrix header for the specified column span."
  (self mat)
  (startcol :int)
  (endcol :int))


;; int rows, cols;
;; int cv_Mat_cols(Mat* self)
(defcfun ("cv_Mat_cols" cols) :int
  (self mat))


;; void Mat::convertTo(OutputArray m, int rtype, double alpha=1, double beta=0 ) const
;; void cv_Mat_convertTo(Mat* self,Mat* m, int rtype, double alpha, double beta)
(defcfun ("cv_Mat_convertTo" %convert-to) :void
  (self mat)
  (m mat)
  (rtype :int)
  (alpha :double)
  (beta :double))


(defun convert-to (self m rtype &optional (alpha 1.0d0) (beta 0.0d0))
       "Converts an array to another data type with optional scaling."
       (%convert-to self m rtype alpha beta))


;; void Mat::copyTo(OutputArray m) const
;; void cv_Mat_copyTo(Mat* self, Mat* m)
(defcfun ("cv_Mat_copyTo" copy-to-2) :void
  "Copies the matrix to another one."
  (self mat)
  (m mat))


;; void Mat::copyTo(OutputArray m, InputArray mask) const
;; void cv_Mat_copyTo_masked(Mat* self, Mat* m, Mat* mask)
(defcfun ("cv_Mat_copyTo_masked" copy-to-3) :void
  "Copies the matrix to another one, masked."
  (self mat)
  (m mat)
  (mask mat))


(defun copy-to (&optional mat m mask)
       (cond ((eq mask nil)
	      (copy-to-2 mat m))
	      (t (copy-to-3 mat m mask))))


;; Mat Mat::cross(InputArray m) const
;; Mat* cv_Mat_cross(Mat* self, Mat* m)
(defcfun ("cv_Mat_cross" cross) mat
  "Computes a cross-product of two 3-element vectors."
  (self mat)
  (m mat))


;; uchar* data
;; uchar* cv_Mat_get_Data(Mat* self)
(defcfun ("cv_Mat_get_Data" data) :pointer
  "Pointer to the data."
  (self mat))


;; int Mat::depth() const
;; int cv_Mat_depth(Mat* self)
(defcfun ("cv_Mat_depth" depth) :int 
	 (self mat))


;; Mat* cv_Mat_diag_d(Mat* self, int d)
(defcfun ("cv_Mat_diag_d" %diag) mat
  "Extracts a diagonal from a matrix."
  (self mat)
  (d :int))


(defun diag (self &optional (d 0))
       "Extracts a diagonal from a matrix."
       (%diag self d))


(defun distance (self)
 (mem-aref (c-pointer self) :float 3))


;; MatExpr / operator
;; MatExpr* cv_Mat_div(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_div" div) mat-expr
  (m1 mat)
  (m2 mat))


;; DMatch::DMatch()
;; DMatch* cv_create_DMatch() 
(defcfun ("cv_create_DMatch" dmatch-0) dmatch)


;; DMatch( int _queryIdx, int _trainIdx, float _distance ) 
;; DMatch* cv_create_DMatch3(int _queryIdx, int _trainIdx, float _distance)
(defcfun ("cv_create_DMatch3" dmatch-3) dmatch
  (query-idx :int)
  (train-idx :int)
  (distance :float))


;; DMatch( int _queryIdx, int _trainIdx, int _imgIdx, float _distance )
;; DMatch* cv_create_DMatch4(int _queryIdx, int _trainIdx, int _imgIdx, float _distance)
(defcfun ("cv_create_DMatch4" dmatch-4) dmatch
  (query-idx :int)
  (train-idx :int)
  (img-idx :int)
  (distance :float))


(defun dmatch (&rest args)
  "DMatch constructor"
       (cond ((eq (first args) nil)
	      (dmatch-0))
	      ((and (first args) (not (fourth args)))
	       (dmatch-3 (first args) (second args) (third args)))
	       ((fourth args)
		(dmatch-4 (first args) (second args) (third args) (fourth args)))
		(t nil)))


(defun make-dmatch (&rest args)
  "DMatch constructor"
       (cond ((eq (first args) nil)
	      (dmatch-0))
	      ((and (first args) (not (fourth args)))
	       (dmatch-3 (first args) (second args) (third args)))
	       ((fourth args)
		(dmatch-4 (first args) (second args) (third args) (fourth args)))
		(t nil)))


;; _Tp dot(const Point_& pt) const
;; tn cv_Point2##t##_dot( Point2##t * self, Point2##t * other)
(defcfun ("cv_Point2i_dot" dot-2i) :int 
	 "Finds the dot product of a point."
	 (self point)
	 (other point))


;; _Tp dot(const Point_& pt) const
;; tn cv_Point2##t##_dot( Point2##t * self, Point2##t * other)
(defcfun ("cv_Point2d_dot" dot-2d) :double
  "Finds the dot product of a point-2d."
  (self point-2d)
  (other point-2d))


;; _Tp dot(const Point_& pt) const
;; tn cv_Point2##t##_dot( Point2##t * self, Point2##t * other)
(defcfun ("cv_Point2f_dot" dot-2f) :float
  "Finds the dot product of a point-2f."
  (self point-2f)
  (other point-2f))


;; _Tp dot(const Point3_& pt) const
;; tn cv_Point3##t##_dot( Point3##t * self, Point3##t * other)
(defcfun ("cv_Point3d_dot" dot-3d) :double 
	 "Finds the dot product of a point-3d."
	 (self point-3d)
	 (other point-3d))


;; _Tp dot(const Point3_& pt) const
;; tn cv_Point3##t##_dot( Point3##t * self, Point3##t * other)
(defcfun ("cv_Point3f_dot" dot-3f) :float 
	 "Finds the dot product of a point-3f."
	 (self point-3f)
	 (other point-3f))


;; _Tp dot(const Point3_& pt) const
;; tn cv_Point3##t##_dot( Point3##t * self, Point3##t * other)
(defcfun ("cv_Point3i_dot" dot-3i) :int 
	 "Finds the dot product of a point-3i."
	 (self point-3i)
	 (other point-3i))


;; Mat* force(MatExpr* expr)
(defcfun ("force" force) mat
  "Coerces a MAT-EXPR to a MAT."
  (mat-expr mat-expr))


(defun img-idx (self)
	 (mem-aref (c-pointer self) :int 2))


;; KeyPoint::KeyPoint()
;; KeyPoint* cv_create_KeyPoint()
(defcfun ("cv_create_KeyPoint" key-point-0) key-point
  "KEY-POINT constructor")


;; KeyPoint::KeyPoint(float x, float y, float _size, float _angle=-1, float _response=0, int _octave=0, int _class_id=-1)
;; KeyPoint* cv_create_KeyPoint7(float x, float y, float _size, float _angle, float _response, int _octave, int _class_id)
(defcfun ("cv_create_KeyPoint7" key-point-7) key-point
  "KEY-POINT constructor"
  (x :float)
  (y :float)
  (size :float)
  (angle :float)
  (response :float)
  (octave :int)
  (class-id :int))


(defun key-point (&optional x y size (angle -1f0) (response 0f0) (octave 0) (class-id -1))
       (cond ((eq x nil)
	      (key-point-0))
	      (x
	       (key-point-7 x y size angle response octave class-id))
	       (t nil)))


(defun make-key-point (&optional x y size (angle -1f0) (response 0f0) (octave 0) (class-id -1))
       (cond ((eq x nil)
	      (key-point-0))
	      (x
	       (key-point-7 x y size angle response octave class-id))
	       (t nil)))


;; void Mat::locateROI(Size& wholeSize, Point& ofs) const
;; void cv_Mat_locateROI(Mat* self, Size* s, Point* p) 
(defcfun ("cv_Mat_locateROI" locate-roi) :void 
	 "Locates the matrix header within a parent matrix."
	 (self mat)
	 (whole-size size)
	 (ofs point))


;; Mat::Mat()
;; Mat* cv_create_Mat()
(defcfun ("cv_create_Mat" %mat) mat
	 "MAT constructor")


;; Mat::Mat(int rows, int cols, int type, void* data) 
;; Mat* cv_create_Mat_with_data(int rows, int cols, int type, void* data)
(defcfun ("cv_create_Mat_with_data" mat-data) mat
	 (rows :int)
	 (cols :int)
	 (type :int)
	 (data :pointer))


;; double Mat::dot(InputArray m) const
;; double cv_Mat_dot(Mat* self, Mat* m)
(defcfun ("cv_Mat_dot" mat-dot) :double
  (self mat)
  (other mat))


;; bool Mat::empty() const
;; int cv_Mat_empty(Mat* self) 
(defcfun ("cv_Mat_empty" mat-empty) :boolean
  "Returns true if the array has no elements."
  (self mat))


;; Mat::t
;; MatExpr* cv_Mat_transpose_mat(Mat* self) 
(defcfun ("cv_Mat_transpose_mat" mat-expr-t)  mat-expr
  "Transposes a matrix."
  (self mat))


;; static MatExpr Mat::eye(int rows, int cols, int type) 
;; Mat* cv_create_identity(int rows, int cols, int type)
(defcfun ("cv_create_identity" mat-eye) mat
  "Returns an identity matrix of the specified size and type."
  (rows :int)
  (cols :int)
  (type :int))


(defun mat-info (mat)
  (let ((mat-rows (rows mat))
	(mat-cols (cols mat))
	(mat-type (mat-type mat)))

    (case  mat-type  
      (#.+8uc1+  (list mat-rows mat-cols mat-type :uchar 1 3)) 
      (#.+8sc1+  (list mat-rows mat-cols mat-type :char 1 3))
      (#.+16uc1+ (list mat-rows mat-cols mat-type :ushort 1 3)) 
      (#.+16sc1+ (list mat-rows mat-cols mat-type :short 1 3))
      (#.+32sc1+ (list mat-rows mat-cols mat-type :int 1 3)) 
      (#.+32fc1+ (list mat-rows mat-cols mat-type :float 1 3))
      (#.+64fc1+ (list mat-rows mat-cols mat-type :double 1 3))
      (#.+8uc2+  (list mat-rows mat-cols mat-type :uchar 2 6))
      (#.+8sc2+  (list mat-rows mat-cols mat-type :char 2 6))
      (#.+16uc2+ (list mat-rows mat-cols mat-type :ushort 2 6))
      (#.+16sc2+ (list mat-rows mat-cols mat-type :short 2 6))
      (#.+32sc2+ (list mat-rows mat-cols mat-type :int 2 6))
      (#.+32fc2+ (list mat-rows mat-cols mat-type :float 2 6))
      (#.+64fc2+ (list mat-rows mat-cols mat-type :double 2 6))
      (#.+8uc3+  (list mat-rows mat-cols mat-type :uchar 3 9))
      (#.+8sc3+  (list mat-rows mat-cols mat-type :char 3 9))
      (#.+16uc3+ (list mat-rows mat-cols mat-type :ushort 3 9))
      (#.+16sc3+ (list mat-rows mat-cols mat-type :short 3 9))
      (#.+32sc3+ (list mat-rows mat-cols mat-type :int 3 9))
      (#.+32fc3+ (list mat-rows mat-cols mat-type :float 3 9))
      (#.+64fc3+ (list mat-rows mat-cols mat-type :double 3 9))
      (#.+8uc4+  (list mat-rows mat-cols mat-type :uchar 4 12))
      (#.+8sc4+  (list mat-rows mat-cols mat-type :char 4 12))
      (#.+16uc4+ (list mat-rows mat-cols mat-type :ushort 4 12))
      (#.+16sc4+ (list mat-rows mat-cols mat-type :short 4 12))
      (#.+32sc4+ (list mat-rows mat-cols mat-type :int 4 12))
      (#.+32fc4+ (list mat-rows mat-cols mat-type :float 4 12))
      (#.+64fc4+ (list mat-rows mat-cols mat-type :double 4 12)))))


;; static MatExpr Mat::ones(int rows, int cols, int type)
;; Mat* cv_create_ones(int rows, int cols, int type)
(defcfun ("cv_create_ones" mat-ones) mat
  (rows :int)
  (cols :int)
  (type :int))


;; Mat::Mat(const Mat& m, const Range& rowRange, const Range& colRange=Range::all() )
;; Mat* cv_Mat_get_Range(Mat* self, Range* rowRange, Range* colRange)
(defcfun ("cv_Mat_with_Range" %mat-range) mat
  "MAT constructor with Range parameters."
  (self mat)
  (row-range range)
  (col-range range))


(defun mat-range (self row-range &optional (col-range (range-all) given-col-range))
  (let ((return (%mat-range self row-range col-range)))
    (if given-col-range nil (del-range col-range))
    return))


;; Size Mat::size() const
;; Size* cv_Mat_size(Mat* self)
(defcfun ("cv_Mat_size" mat-size) size
  "Returns a matrix size."
  (self mat))


(let ((previous nil))

  (defun  mat-to-arr (mat)

    (unless (equal mat (car previous))
      (setf previous (cons mat (mat-info mat))))

  (if (empty mat) 
      (return-from mat-to-arr 
	(format t "Matrix is empty.")))

  (let* ((mat-info (cdr previous))
	 (mat-rows (first mat-info))
	 (mat-cols (second mat-info))
         (mat-type (third mat-info))
	 (mat-area (* mat-rows mat-cols))
	 (channels (fifth mat-info))
         (arr 0)
	 (ptr (ptr mat 0))
         (cffi-type (case mat-type  

		      (#.+8uc1+ :uchar) 
		      (#.+8sc1+ :char)
		      (#.+16uc1+ :ushort)
		      (#.+16sc1+ :short)
		      (#.+32sc1+ :int)
		      (#.+32fc1+ :float)
		      (#.+64fc1+ :double)
		      (#.+8uc2+  :uchar)
		      (#.+8sc2+  :char)
		      (#.+16uc2+ :ushort)
		      (#.+16sc2+ :short)
		      (#.+32sc2+ :int)
		      (#.+32fc2+ :float)
		      (#.+64fc2+ :double)
		      (#.+8uc3+  :uchar)
		      (#.+8sc3+  :char)
		      (#.+16uc3+ :ushort)
		      (#.+16sc3+ :short)
		      (#.+32sc3+ :int)
		      (#.+32fc3+ :float)
		      (#.+64fc3+ :double)
		      (#.+8uc4+  :uchar)
		      (#.+8sc4+  :char)
		      (#.+16uc4+ :ushort)
		      (#.+16sc4+ :short)
		      (#.+32sc4+ :int)
		      (#.+32fc4+ :float)
		      (#.+64fc4+ :double))))
    (case cffi-type

      (:uchar 

       (setf arr (if (= channels 1) 
		     (make-array (list mat-rows mat-cols) 
				 :element-type '(unsigned-byte 8)) 
		     (make-array (list mat-rows mat-cols channels) 
				 :element-type '(unsigned-byte 8))))
       (dotimes (n (* mat-area channels))
	 (setf (row-major-aref arr n) (mem-aref ptr :uchar n)))arr)

      (:char 

       (setf arr (if (= channels 1) 
		     (make-array (list mat-rows mat-cols) 
				 :element-type '(signed-byte 8)) 
		     (make-array (list mat-rows mat-cols channels) 
				 :element-type '(signed-byte 8))))
       (dotimes (n (* mat-area channels))
	 (setf (row-major-aref arr n) (mem-aref ptr :char n)))arr)

      (:ushort 

       (setf arr (if (= channels 1) 
		     (make-array (list mat-rows mat-cols) 
				 :element-type '(unsigned-byte 16)) 
		     (make-array (list mat-rows mat-cols channels) 
				 :element-type '(unsigned-byte 16))))
       (dotimes (n (* mat-area channels))
	 (setf (row-major-aref arr n) (mem-aref ptr :ushort n)))arr)

      (:short 

       (setf arr (if (= channels 1) 
		     (make-array (list mat-rows mat-cols) 
				 :element-type '(signed-byte 16)) 
		     (make-array (list mat-rows mat-cols channels) 
				 :element-type '(signed-byte 16))))
       (dotimes (n (* mat-area channels))
	 (setf (row-major-aref arr n) (mem-aref ptr :short n)))arr)

      (:int 

       (setf arr (if (= channels 1) 
		     (make-array (list mat-rows mat-cols) 
				 :element-type '(signed-byte 32)) 
		     (make-array (list mat-rows mat-cols channels) 
				 :element-type '(signed-byte 32))))
       (dotimes (n (* mat-area channels))
	 (setf (row-major-aref arr n) (mem-aref ptr :int n)))arr)

      (:float 

       (setf arr (if (= channels 1) 
		     (make-array (list mat-rows mat-cols) 
				 :element-type 'single-float) 
		     (make-array (list mat-rows mat-cols channels) 
				 :element-type 'single-float)))
       (dotimes (n (* mat-area channels))
	 (setf (row-major-aref arr n) (mem-aref ptr :float n)))arr)

      (:double 

       (setf arr (if (= channels 1) 
		     (make-array (list mat-rows mat-cols) 
				 :element-type 'double-float) 
		     (make-array (list mat-rows mat-cols channels) 
				 :element-type 'double-float)))
       (dotimes (n (* mat-area channels))
	 (setf (row-major-aref arr n) (mem-aref ptr :double n)))arr)))))


(defun mat-type-to-cffi (mat)
  (let* ((mat-type (mat-type mat))
	 (cffi-type (case mat-type 
		      (#.+8uc1+ ':uchar)
		      (#.+8sc1+ ':char)
		      (#.+16uc1+ ':ushort)
		      (#.+16sc1+ ':short)
		      (#.+32sc1+ ':int)
		      (#.+32fc1+ ':float)
		      (#.+64fc1+ ':double)
		      (#.+8uc2+ ':uchar)
		      (#.+8sc2+ ':char)
		      (#.+16uc2+ ':ushort)
		      (#.+16sc2+ ':short)
		      (#.+32sc2+ ':int)
		      (#.+32fc2+ ':float)
		      (#.+64fc2+ ':double)
		      (#.+8uc3+ ':uchar)
		      (#.+8sc3+ ':char)
		      (#.+16uc3+ ':ushort)
		      (#.+16sc3+ ':short)
		      (#.+32sc3+ ':int)
		      (#.+32fc3+ ':float)
		      (#.+64fc3+ ':double)
		      (#.+8uc4+ ':uchar)
		      (#.+8sc4+ ':char)
		      (#.+16uc4+ ':ushort)
		      (#.+16sc4+ ':short)
		      (#.+32sc4+ ':int)
		      (#.+32fc4+ ':float)
		      (#.+64fc4+ ':double)))) 
    (multiple-value-bind (mat-type cffi-type) 
	(values mat-type cffi-type) 
      (list mat-type cffi-type))))


;; int Mat::type() const
;; int cv_Mat_type(Mat* self)
(defcfun ("cv_Mat_type" mat-type) :int
  (self mat))


;; Mat::Mat(int rows, int cols, int type)
;; Mat* cv_create_Mat_typed(int rows, int cols, int type)
(defcfun ("cv_create_Mat_typed" mat-typed) mat
  "MAT constructor with a row, column and type parameter."
  (rows :int)
  (cols :int)
  (type :int))


;; Mat::Mat(int rows, int cols, int type, const Scalar& s)
;; Mat* cv_create_Mat_with_value(int rows, int cols, int type, Scalar* s)
(defcfun ("cv_create_Mat_with_value" %mat-value) mat
	 (rows :int)
	 (cols :int)
	 (type :int)
	 (s scalar))


(defun mat-value (rows cols type values)
  (let* ((scalar (apply #'scalar values))					 
	(ret (%mat-value rows cols type scalar)))
    (del-scalar scalar)
ret))


;;; MAT


(defun mat (&optional arg1 arg2 arg3 arg4)
  
  "MAT constructor"  
  
  (cond ((eq arg1 nil) (%mat))

	((and (eq arg2 nil) 
	      (typep arg1 'cv-mat))

	 (mat-to-arr arg1))

	((and (eq arg2 nil) 

	      (typep arg1 'simple-array))

	 (arr-to-mat arg1))

	((typep arg2 'cv-range)

	 (apply #'mat-range arg1 arg2 arg3))
	
	((and (eq arg4 nil) arg1)

	 (mat-typed arg1 arg2 arg3))
	
	((typep arg4 'cv-scalar)

	 (%mat-value arg1 arg2 arg3 arg4))
	
	((listp arg4)

	 (mat-value arg1 arg2 arg3 arg4))
	
	((pointerp arg4)

	 (mat-data arg1 arg2 arg3 arg4))
	
	(t nil)))


(defun make-mat (&optional arg1 arg2 arg3 arg4)
  
  "MAT constructor"  
  
  (cond ((eq arg1 nil) (%mat))

	((and (eq arg2 nil) 
	      (typep arg1 'cv-mat))

	 (mat-to-arr arg1))

	((and (eq arg2 nil) 

	      (typep arg1 'simple-array))

	 (arr-to-mat arg1))

	((typep arg2 'cv-range)

	 (apply #'mat-range arg1 arg2 arg3))
	
	((and (eq arg4 nil) arg1)

	 (mat-typed arg1 arg2 arg3))
	
	((typep arg4 'cv-scalar)

	 (%mat-value arg1 arg2 arg3 arg4))
	
	((listp arg4)

	 (mat-value arg1 arg2 arg3 arg4))
	
	((pointerp arg4)

	 (mat-data arg1 arg2 arg3 arg4))
	
	(t nil)))


;; static MatExpr Mat::zeros(int rows, int cols, int type)
;; Mat* cv_create_zeros(int rows, int cols, int type)
(defcfun ("cv_create_zeros" mat-zeros) mat
  (rows :int)
  (cols :int)
  (type :int))


;; static MatExpr Mat::zeros(int rows, int cols, int type)
;; Mat* cv_create_zeros(int rows, int cols, int type)
(defcfun ("cv_create_zeros" make-mat-zeros) mat
  (rows :int)
  (cols :int)
  (type :int))


;; static MatExpr Mat::zeros(int rows, int cols, int type)
;; Mat* cv_create_zeros(int rows, int cols, int type)
(defcfun ("cv_create_zeros" %mat-zeros) mat
  (rows :int)
  (cols :int)
  (type :int))


;; MatExpr * operator
;; MatExpr* cv_Mat_mult(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_mult" mul) mat-expr
  (m1 mat)
  (m2 mat))


(defun octave (self)
  (mem-aref (c-pointer self) :int 5))


;; Point_()
;; Point2##t * cv_create_Point2##(tn x, tn y)
(defcfun ("cv_create_Point2i" point-0) point)


;; Point_(_Tp _x, _Tp _y)
;; Point2##t * cv_create_Point2##(tn x, tn y) 
(defcfun ("cv_create_Point2i" point-2) point
  (x :int)
  (y :int))


(defun point (&optional x y)
  "Point constructor"
       (cond ((eq (or x y) nil)
	      (point-0))
	      ((and x y)
	       (point-2 x y))
	       
	       (t nil)))


(defun make-point (&optional x y)
  "Point constructor"
       (cond ((eq (or x y) nil)
	      (point-0))
	      ((and x y)
	       (point-2 x y))
	       
	       (t nil)))


;; _Tp x, y
;; int cv_Point_getX(Point* self) 
(defcfun ("cv_Point2i_getX" point-x) :int 
	 "Retrieves X coordinate of a POINT object."
	 (self point))

;; _Tp x, y
;; int cv_Point_getY(Point* self)
(defcfun ("cv_Point2i_getY" point-y) :int 
         "Retrieves Y coordinate of a POINT object."
	 (self point))


;; typedef Point_<double> Point2d
;; Point2##t * cv_create_Point2##(tn x, tn y) 
(defcfun ("cv_create_Point2d" point-2d-0) point-2d 
	 "Point2d constructor")


;; typedef Point_<double> Point2d
;; Point2##t * cv_create_Point2##(tn x, tn y)  
(defcfun ("cv_create_Point2d" point-2d-2) point-2d 
	 "Point2d constructor"
	 (x :double)
	 (y :double))


(defun point-2d (&optional x y)
       (cond ((eq (or x y) nil)
	      (point-2d-0))
	      ((and x y)
	       (point-2d-2 x y))
	       (t nil)))


(defun make-point-2d (&optional x y)
       (cond ((eq (or x y) nil)
	      (point-2d-0))
	      ((and x y)
	       (point-2d-2 x y))
	       (t nil)))


;; _Tp x, y
;; double cv_Point2d_getX(Point2d* self) 
(defcfun ("cv_Point2d_getX" point-2d-x) :double
  "Retrieves X coordinate of a POINT-2D object."
  (self point-2d))


;; _Tp x, y
;; double cv_Point2d_getY(Point2d* self) 
(defcfun ("cv_Point2d_getY" point-2d-y) :double
  "Retrieves Y coordinate of a POINT-2D object."
  (self point-2d))


;; typedef Point_<float> Point2f
;; tn cv_Point2##t##_getX( Point2##t * self) 
(defcfun ("cv_create_Point2f" point-2f-0) point-2f 
	 "Point2f constructor")


;; typedef Point_<float> Point2f
;; Point2##t * cv_create_Point2##(tn x, tn y)  
(defcfun ("cv_create_Point2f" point-2f-2) point-2f 
	 "Point2f constructor"
	 (x :float)
	 (y :float))


(defun point-2f (&optional x y)
       (cond ((eq (or x y) nil)
	      (point-2f-0))
	      ((and x y)
	       (point-2f-2 x y))
	       (t nil)))


(defun make-point-2f (&optional x y)
       (cond ((eq (or x y) nil)
	      (point-2f-0))
	      ((and x y)
	       (point-2f-2 x y))
	       (t nil)))


;; _Tp x, y;
;; float cv_Point2f_getX(Point2f* self) 
(defcfun ("cv_Point2f_getX" point-2f-x) :float
  "Retrieves X coordinate of a POINT-2F object."
  (self point-2f))


;; _Tp x, y
;; float cv_Point2f_getY(Point2f* self) 
(defcfun ("cv_Point2f_getY" point-2f-y) :float
  "Retrieves Y coordinate of a POINT-2F object."
  (self point-2f))


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##(tn x, tn y, tn z) 
(defcfun ("cv_create_Point3d" point-3d-0) point-3d 
	 "Point3d constructotr")


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##(tn x, tn y, tn z) 
(defcfun ("cv_create_Point3d" point-3d-2) point-3d 
	 "Point3d constructor"
	 (x :double)
	 (y :double)
	 (z :double))


(defun point-3d (&optional x y z)
       (cond ((eq (or x y) nil)
	      (point-3d-0))
	      ((and x y)
	       (point-3d-2 x y z))
	       (t nil)))


(defun make-point-3d (&optional x y z)
       (cond ((eq (or x y) nil)
	      (point-3d-0))
	      ((and x y)
	       (point-3d-2 x y z))
	       (t nil)))


;; _Tp x, y, z
;; double cv_Point3d_getX(Point3d* self) 
(defcfun ("cv_Point3d_getX" point-3d-x) :double
  "Retrieves X coordinate of a POINT-3D object."
  (self point-3d))


;; _Tp x, y, z
;; double cv_Point3d_getY(Point3d* self) 
(defcfun ("cv_Point3d_getY" point-3d-y) :double
  "Retrieves Y coordinate of a POINT-3D object."
  (self point-3d))


;; _Tp x, y, z
;; double cv_Point3d_getZ(Point3d* self) 
(defcfun ("cv_Point3d_getZ" point-3d-z) :double
  "Retrieves Z coordinate of a POINT-3D object."
  (self point-3d))


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##(tn x, tn y, tn z)  
(defcfun ("cv_create_Point3f" point-3f-0) point-3f 
	 "Point3f constructor")


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##(tn x, tn y, tn z)  
(defcfun ("cv_create_Point3f" point-3f-2) point-3f 
	 "Point3f constructor"
	 (x :float)
	 (y :float)
	 (z :float))


(defun point-3f (&optional x y z)
       (cond ((eq (or x y) nil)
	      (point-3f-0))
	      ((and x y)
	       (point-3f-2 x y z))
	       (t nil)))


(defun make-point-3f (&optional x y z)
       (cond ((eq (or x y) nil)
	      (point-3f-0))
	      ((and x y)
	       (point-3f-2 x y z))
	       (t nil)))


;; _Tp x, y, z
;; float cv_Point3f_getX(Point3f* self) 
(defcfun ("cv_Point3f_getX" point-3f-x) :float
  "Retrieves X coordinate of a POINT-3F object."
  (self point-3f))


;; _Tp x, y, z
;; float cv_Point3f_getY(Point3f* self) 
(defcfun ("cv_Point3f_getY" point-3f-y) :float
  "Retrieves Y coordinate of a POINT-3F object."
  (self point-3f))


;; _Tp x, y, z
;; float cv_Point3f_getZ(Point3f* self) 
(defcfun ("cv_Point3f_getZ" point-3f-z) :float
  "Retrieves Z coordinate of a POINT-3F object."
  (self point-3f))


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##(tn x, tn y, tn z)  
(defcfun ("cv_create_Point3i" point-3i-0) point-3i 
	 "Point3i constructor")


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##(tn x, tn y, tn z)  
(defcfun ("cv_create_Point3i" point-3i-2) point-3i 
	 "Point3i constructor"
	 (x :int)
	 (y :int)
	 (z :int))


(defun point-3i (&optional x y z)
       (cond ((eq (or x y) nil)
	      (point-3i-0))
	      ((and x y)
	       (point-3i-2 x y z))
	       (t nil)))


(defun make-point-3i (&optional x y z)
       (cond ((eq (or x y) nil)
	      (point-3i-0))
	      ((and x y)
	       (point-3i-2 x y z))
	       (t nil)))


;; _Tp x, y, z
;; int cv_Point3i_getX(Point3i* self) 
(defcfun ("cv_Point3i_getX" point-3i-x) :int 
  "Retrieves X coordinate of a POINT-3I object."
	 (self point-3i))


;; _Tp x, y, z
;; int cv_Point3i_getY(Point3i* self) 
(defcfun ("cv_Point3i_getY" point-3i-y) :int
  "Retrieves Y coordinate of a POINT-3I object."
  (self point-3i))


;; _Tp x, y, z
;; int cv_Point3i_getZ(Point3i* self) 
(defcfun ("cv_Point3i_getZ" point-3i-z) :int
  "Retrieves Z coordinate of a POINT-3I object."
  (self point-3i))


(defun print-2d-arr-as-mat (arr &optional (*standard-output* *standard-output*))

	 (format t *personalize-print-2d-mat*)
	 (format t  "(")
	 (print-elements (i 0 (array-dimension arr 0)) ("(" (format nil ")~%    (") ")")
	   (print-elements (j 0 (array-dimension arr 1)) ("" " " "")
	     (prin1 (aref arr i j))))
	 (format t ")~%"))


(defun print-3d-arr-as-mat (arr &optional (*standard-output* *standard-output*))

	 (format t *personalize-print-3d-mat*)
	 (format t  "(")
	 (print-elements (i 0 (array-dimension arr 0)) ("(" (format nil ")~%    (") ")")
	   (print-elements (j 0 (array-dimension arr 1)) ("(" (format nil ")~%     (") ")")
	     (print-elements (k 0 (array-dimension arr 2)) ("" " " "")
	       (prin1 (aref arr i j k)))))
	 (format t ")~%"))


(defun print-mat (mat)
  (if (empty mat) 
      (return-from print-mat 
	(format t "Matrix is empty.")))
  (let ((arr (mat-to-arr mat))
	(channels (channels mat))) 
    (if (eq channels 1)
	(print-2d-arr-as-mat arr)
	(print-3d-arr-as-mat arr))))


(defun pm (mat)
  "Alias for PRINT-MAT."
  (if (empty mat) 
      (return-from pm 
	(format t "Matrix is empty.")))
  (let ((arr (mat-to-arr mat))
	(channels (channels mat))) 
    (if (eq channels 1)
	(print-2d-arr-as-mat arr)
	(print-3d-arr-as-mat arr))))


(defun print-point (point)
  (if (typep point 'cv-point)
      (format t "~a(~a ~a)" *personalize-print-point* (x point) (y point))
      (error "The value ~a is not of type CV-POINT." point)))


(defun print-point-2d (point-2d)
  (if (typep point-2d 'cv-point-2d)
      (format t "~a(~a ~a)" *personalize-print-point-2d* (x point-2d) (y point-2d))
      (error "The value ~a is not of type CV-POINT-2D." point-2d)))


(defun print-point-2f (point-2f)
  (if (typep point-2f 'cv-point-2f)
      (format t "~a(~a ~a)" *personalize-print-point-2f* (x point-2f) (y point-2f))
      (error "The value ~a is not of type CV-POINT-2F." point-2f)))


(defun print-point-3d (point-3d)
  (if (typep point-3d 'cv-point-3d)
      (format t "~a(~a ~a ~a)" *personalize-print-point-3d* (x point-3d) (y point-3d) (z point-3d))
      (error "The value ~a is not of type CV-POINT-3D." point-3d)))


(defun print-point-3f (point-3f)
  (if (typep point-3f 'cv-point-3f)
      (format t "~a(~a ~a ~a)" *personalize-print-point-3f* (x point-3f) (y point-3f) (z point-3f))
      (error "The value ~a is not of type CV-POINT-3F." point-3f)))


(defun print-point-3i (point-3i)
  (if (typep point-3i 'cv-point-3i)
      (format t "~a(~a ~a ~a)" *personalize-print-point-3i* (x point-3i) (y point-3i) (z point-3i))
      (error "The value ~a is not of type CV-POINT-3I." point-3i)))


(defun print-scalar (scalar)
  (format t "~a(~a ~a ~a ~a)~%" *personalize-print-scalar* 
	  (@ scalar :double) 
	  (@ scalar :double 1) 
	  (@ scalar :double 2)
	  (@ scalar :double 3)))


(defun print-vec-2b (vec-2b)
  (if (typep vec-2b 'cv-vec-2b)
      (format t "~a(~a ~a)" *personalize-print-vec-2b* (@ vec-2b :uchar) (@ vec-2b :uchar 1))
      (error "The value ~a is not of type CV-VEC-2B." vec-2b)))


(defun print-vec-2d (vec-2d)
  (if (typep vec-2d 'cv-vec-2d)
      (format t "~a(~a ~a)" *personalize-print-vec-2d* (@ vec-2d :double) (@ vec-2d :double 1))
      (error "The value ~a is not of type CV-VEC-2D." vec-2d)))


(defun print-vec-2f (vec-2f)
  (if (typep vec-2f 'cv-vec-2f)
      (format t "~a(~a ~a)" *personalize-print-vec-2f* (@ vec-2f :float) (@ vec-2f :float 1))
      (error "The value ~a is not of type CV-VEC-2F." vec-2f)))


(defun print-vec-2i (vec-2i)
  (if (typep vec-2i 'cv-vec-2i)
      (format t "~a(~a ~a)" *personalize-print-vec-2i* (@ vec-2i :int) (@ vec-2i :int 1))
      (error "The value ~a is not of type CV-VEC-2I." vec-2i)))


(defun print-vec-2s (vec-2s)
  (if (typep vec-2s 'cv-vec-2s)
      (format t "~a(~a ~a)" *personalize-print-vec-2s* (@ vec-2s :short) (@ vec-2s :short 1))
      (error "The value ~a is not of type CV-VEC-2S." vec-2s)))


(defun print-vec-2w (vec-2w)
  (if (typep vec-2w 'cv-vec-2w)
      (format t "~a(~a ~a)" *personalize-print-vec-2w* (@ vec-2w :ushort) (@ vec-2w :ushort 1))
      (error "The value ~a is not of type CV-VEC-2W." vec-2w)))


(defun print-vec-3b (vec-3b)
  (if (typep vec-3b 'cv-vec-3b)
      (format t "~a(~a ~a ~a)" *personalize-print-vec-3b* 
	      (@ vec-3b :uchar) 
	      (@ vec-3b :uchar 1) 
	      (@ vec-3b :uchar 2))
      (error "The value ~a is not of type CV-VEC-3B." vec-3b)))


(defun print-vec-3d (vec-3d)
  (if (typep vec-3d 'cv-vec-3d)
      (format t "~a(~a ~a ~a)" *personalize-print-vec-3d* 
	      (@ vec-3d :double) 
	      (@ vec-3d :double 1) 
	      (@ vec-3d :double 2))
      (error "The value ~a is not of type CV-VEC-3D." vec-3d)))


(defun print-vec-3f (vec-3f)
  (if (typep vec-3f 'cv-vec-3f)
      (format t "~a(~a ~a ~a)" *personalize-print-vec-3f* 
	      (@ vec-3f :float) 
	      (@ vec-3f :float 1) 
	      (@ vec-3f :float 2))
      (error "The value ~a is not of type CV-VEC-3F." vec-3f)))


(defun print-vec-3i (vec-3i)
  (if (typep vec-3i 'cv-vec-3i)
      (format t "~a(~a ~a ~a)" *personalize-print-vec-3i* 
	      (@ vec-3i :int) 
	      (@ vec-3i :int 1) 
	      (@ vec-3i :int 2))
      (error "The value ~a is not of type CV-VEC-3I." vec-3i)))


(defun print-vec-3s (vec-3s)
  (if (typep vec-3s 'cv-vec-3s)
      (format t "~a(~a ~a ~a)" *personalize-print-vec-3s* 
	      (@ vec-3s :short) 
	      (@ vec-3s :short 1) 
	      (@ vec-3s :short 2))
      (error "The value ~a is not of type CV-VEC-3S." vec-3s)))


(defun print-vec-3w (vec-3w)
  (if (typep vec-3w 'cv-vec-3w)
      (format t "~a(~a ~a ~a)" *personalize-print-vec-3w* 
	      (@ vec-3w :ushort) 
	      (@ vec-3w :ushort 1) 
	      (@ vec-3w :ushort 2))
      (error "The value ~a is not of type CV-VEC-3W." vec-3w)))


(defun print-vec-4b (vec-4b)
  (if (typep vec-4b 'cv-vec-4b)
      (format t "~a(~a ~a ~a ~a)" *personalize-print-vec-4b* 
	      (@ vec-4b :uchar) (@ vec-4b :uchar 1)
	      (@ vec-4b :uchar 2) (@ vec-4b :uchar 3))
      (error "The value ~a is not of type CV-VEC-4B." vec-4b)))


(defun print-vec-4d (vec-4d)
  (if (typep vec-4d 'cv-vec-4d)
      (format t "~a(~a ~a ~a ~a)" *personalize-print-vec-4d* 
	      (@ vec-4d :double) (@ vec-4d :double 1) 
	      (@ vec-4d :double 2) (@ vec-4d :double 3))
      (error "The value ~a is not of type CV-VEC-4D." vec-4d)))


(defun print-vec-4f (vec-4f)
  (if (typep vec-4f 'cv-vec-4f)
      (format t "~a(~a ~a ~a ~a)" *personalize-print-vec-4f* 
	      (@ vec-4f :float) (@ vec-4f :float 1) 
	      (@ vec-4f :float 2) (@ vec-4f :float 3))
      (error "The value ~a is not of type CV-VEC-4F." vec-4f)))


(defun print-vec-4i (vec-4i)
  (if (typep vec-4i 'cv-vec-4i)
      (format t "~a(~a ~a ~a ~a)" *personalize-print-vec-4i* 
	      (@ vec-4i :int) (@ vec-4i :int 1) 
	      (@ vec-4i :int 2) (@ vec-4i :int 3))
      (error "The value ~a is not of type CV-VEC-4I." vec-4i)))


(defun print-vec-4s (vec-4s)
  (if (typep vec-4s 'cv-vec-4s)
      (format t "~a(~a ~a ~a ~a)" *personalize-print-vec-4s* 
	      (@ vec-4s :short) (@ vec-4s :short 1) 
	      (@ vec-4s :short 2) (@ vec-4s :short 3))
      (error "The value ~a is not of type CV-VEC-4S." vec-4s)))


(defun print-vec-4w (vec-4w)
  (if (typep vec-4w 'cv-vec-4w)
      (format t "~a(~a ~a ~a ~a)" *personalize-print-vec-4w* 
	      (@ vec-4w :ushort) (@ vec-4w :ushort 1)
	      (@ vec-4w :ushort 2) (@ vec-4w :ushort 3))
      (error "The value ~a is not of type CV-VEC-4W." vec-4w)))


(defun print-vec-6d (vec-6d)
  (if (typep vec-6d 'cv-vec-6d)
      (format t "~a(~a ~a ~a ~a ~a ~a)" *personalize-print-vec-6d* 
	      (@ vec-6d :double) (@ vec-6d :double 1)
	      (@ vec-6d :double 2) (@ vec-6d :double 3)
	      (@ vec-6d :double 4) (@ vec-6d :double 5))
      (error "The value ~a is not of type CV-VEC-6D." vec-6d)))


(defun print-vec-6f (vec-6f)
  (if (typep vec-6f 'cv-vec-6f)
      (format t "~a(~a ~a ~a ~a ~a ~a)" *personalize-print-vec-6f* 
	      (@ vec-6f :float) (@ vec-6f :float 1) (@ vec-6f :float 2) 
	      (@ vec-6f :float 3) (@ vec-6f :float 4) (@ vec-6f :float 5))
      (error "The value ~a is not of type CV-VEC-6F." vec-6f)))


(defun print-vec-6i (vec-6i)
  (if (typep vec-6i 'cv-vec-6i)
      (format t "~a(~a ~a ~a ~a ~a ~a)" *personalize-print-vec-6i* 
	      (@ vec-6i :int) (@ vec-6i :int 1) (@ vec-6i :int 2) 
	      (@ vec-6i :int 3) (@ vec-6i :int 4) (@ vec-6i :int 5))
      (error "The value ~a is not of type CV-VEC-6I." vec-6i)))


(defun print-vec-8i (vec-8i)
  (if (typep vec-8i 'cv-vec-8i)
      (format t "~a(~a ~a ~a ~a ~a ~a ~a ~a)" *personalize-print-vec-8i* 
	      (@ vec-8i :int) (@ vec-8i :int 1) (@ vec-8i :int 2) (@ vec-8i :int 3) 
	      (@ vec-8i :int 4) (@ vec-8i :int 5) (@ vec-8i :int 6) (@ vec-8i :int 7))
      (error "The value ~a is not of type CV-VEC-8I." vec-8i)))


;; MatExpr* promote(Mat* m) 
(defcfun ("promote" promote) mat-expr
  "Converts a MAT to a MAT-EXPR."
  (mat mat))


;; uchar* Mat::ptr(int i0=0)
;; uchar* cv_Mat_ptr_index(Mat* self, int i)
(defcfun ("cv_Mat_ptr_index" %ptr) :pointer 
	 (self mat)
	 (i0 :int))


(defun ptr (self &optional (i0 0))
       "Returns pointer to i0-th submatrix along the dimension #0"
       (%ptr self i0))


;; void Mat::push_back(const Mat& m)
;; void cv_Mat_push_back(Mat* self, Mat* elem) 
(defcfun ("cv_Mat_push_back" push-back) :void
  (self mat)
  (m mat))


(defun query-idx (self)
	 (mem-aref (c-pointer self) :int))


;; Range::Range(int _start, int _end)
;; Range* cv_create_Range(int _start, int _end) 
(defcfun ("cv_create_Range" range) range
  "Range constructor"
  (start :int)
  (end :int))


;; Range::Range(int _start, int _end)
;; Range* cv_create_Range(int _start, int _end) 
(defcfun ("cv_create_Range" make-range) range
  "Range constructor"
  (start :int)
  (end :int))


;; static Range::Range all()
;; Range* cv_create_RangeAll()
(defcfun ("cv_create_RangeAll" range-all) range
  "Range constructor - Returns a special variable 
   that means “the whole sequence” or “the whole 
   range”")


;; static Range::Range all()
;; Range* cv_create_RangeAll()
(defcfun ("cv_create_RangeAll" make-range-all) range
  "Range constructor - Returns a special variable 
   that means “the whole sequence” or “the whole 
   range”")


;; bool Range::empty() const;
;; bool cv_Range_empty(Range* self)
(cffi:defcfun ("cv_Range_empty" range-empty) :boolean
  "Returns true if the matrix row or column span, 
   the RANGE object points to, has no elements"
  (self range))


(defun empty (arg)
       (cond ((typep arg 'cv-mat)
	      (mat-empty arg))
	      ((typep arg 'cv-range)
	       (range-empty arg))
	       (t nil)))


;; int Range::end
;; int cv_Range_getend(Range* self)
(cffi:defcfun ("cv_Range_getend" range-end) :int
  "Retrieves the exclusive right boundary of the range."
  (self range))


;; int Range::size() const;
;; int cv_Range_size(Range* self)
(cffi:defcfun ("cv_Range_size" range-size) :int
  "Returns the size of a matrix row or column 
   span that has been stored as a RANGE object."
  (self range))


;; int Range::start
;; int cv_Range_getstart(Range* self) 
(cffi:defcfun ("cv_Range_getstart" range-start) :int
  "Retrieves the inclusive left boundary of the range."
  (self range))


;; Rect_()
;; Rect* cv_create_Rect() 
(defcfun ("cv_create_Rect" rect-0) rect 
	 "RECT constructor.")


;; Rect_(_Tp _x, _Tp _y, _Tp _width, _Tp _height)
;; Rect* cv_create_Rect4(int x, int y, int width, int height) 
(defcfun ("cv_create_Rect4" rect-4) rect
  "RECT constructor."
  (x :int)
  (y :int)
  (width :int)
  (height :int))


(defun rect (&optional x y width height)
       (cond ((eq (or x y) nil)
	      (rect-0))
	      ((and x y)
	       (rect-4 x y width height))
	       (t nil)))


(defun make-rect (&optional x y width height)
       (cond ((eq (or x y) nil)
	      (rect-0))
	      ((and x y)
	       (rect-4 x y width height))
	       (t nil)))


;; Point_<_Tp> br() const
;; Point* cv_Rect_br(Rect* self) 
(defcfun ("cv_Rect_br" br) point 
	 "Retrievies the bottom-right corner of a rectangle."
	 (self rect))


;; _Tp x, y, width, height
;; int &cv_Rect_getHeight(Rect* self)
(defcfun ("cv_Rect_getHeight" rect-height) :int
  (self rect))


;; Size_<_Tp> size() const
;; Size* cv_Rect_size(Rect* self)  
(defcfun ("cv_Rect_size" rect-size) size 
	 "Size (width, height) of the rectangle."
	 (self rect))


;; Point_<_Tp> tl() const
;; Point* cv_Rect_tl(Rect* self) 
(defcfun ("cv_Rect_tl" tl) point 
	 "Retrievies the top-left corner of a rectangle."
	 (self rect))


;; _Tp x, y, width, height
;; int cv_Rect_getWidth(Rect* self)
(defcfun ("cv_Rect_getWidth" rect-width) :int
  (self rect))


;; _Tp x, y, width, height
;; int cv_Rect_getX(Rect* self) 
(defcfun ("cv_Rect_getX" rect-x) :int
  (self rect))


;; _Tp x, y, width, height
;; int cv_Rect_getY(Rect* self)
(defcfun ("cv_Rect_getY" rect-y) :int
  (self rect))



;; Mat Mat::reshape(int cn, int rows=0) const
;; Mat* cv_Mat_reshape(Mat* self, int cn) 
(defcfun ("cv_Mat_reshape" %reshape) mat
  (self mat)
  (cn :int))


;; Mat Mat::reshape(int cn, int rows=0) const
;; Mat* cv_Mat_reshape_rows(Mat* self, int cn, int rows) 
(defcfun ("cv_Mat_reshape_rows" reshape-rows) mat
  (self mat)
  (cn :int)
  (roms :int))


(defun reshape (&optional self cn (rows 0))
       (cond ((eq rows 0)
	      (%reshape self cn))
	      ((> rows 0)
	       (reshape-rows self cn rows))
	       (t nil)))


(defun response (self)
  (mem-aref (c-pointer self) :float 4))


;; Mat::Mat(const Mat& m, const Rect& roi)
;; Mat* cv_Mat_get_ROI(Mat* self, Rect* roi)
(defcfun ("cv_Mat_get_ROI" roi) mat 
	 "Returns matrix header corresponding to the rectangular sub-array of input MAT."
	 (self mat)
	 (roi rect))



;; RotatedRect(const Point2f& center, const Size2f& size, float angle)
;; RotatedRect* cv_create_RotatedRect(Point2f* center, Size2f* size, float angle)
(defcfun ("cv_create_RotatedRect" rotated-rect) rotated-rect
  (center point)
  (size size)
  (angle :float))


;; RotatedRect(const Point2f& center, const Size2f& size, float angle)
;; RotatedRect* cv_create_RotatedRect(Point2f* center, Size2f* size, float angle)
(defcfun ("cv_create_RotatedRect" make-rotated-rect) rotated-rect
  (center point)
  (size size)
  (angle :float))


;; RotatedRect::angle
;; float cv_RotatedRect_angle(RotatedRect* self)
(defcfun ("cv_RotatedRect_angle" rotated-rect-angle) :float
  (self rotated-rect))


;; Rect RotatedRect::boundingRect() const
;; Rect* cv_RotatedRect_boundingRect(RotatedRect* self)
(defcfun ("cv_RotatedRect_boundingRect" rotated-rect-bounding-rect) rect
  "Returns the minimal up-right rectangle containing the rotated rectangle"
  (self rotated-rect))


;; Point2f center;
;; Point* cv_RotatedRect_center(RotatedRect* self) 
(defcfun ("cv_RotatedRect_center" rotated-rect-center) point
  (self rotated-rect))


;; Size2f size;     
;; Size* cv_RotatedRect_size(RotatedRect* self) 
(defcfun ("cv_RotatedRect_size" rotated-rect-size) size
  (self rotated-rect))



;; Mat Mat::rowRange(int startrow, int endrow) const
;; Mat* cv_Mat_getRowRange(Mat* self, int startrow, int endrow)
(defcfun ("cv_Mat_getRowRange" row-range) mat
  "Creates a matrix header for the specified row span."
  (self mat)
  (startrow :int)
  (endrow :int))

;
;; Mat Mat::row(int y) const
;; Mat* cv_Mat_getRow(Mat* self, int y) 
(defcfun ("cv_Mat_getRow" row) mat
  (self mat)
  (y :int))


;; int rows, cols;
;; int cv_Mat_rows(Mat* self) 
(defcfun ("cv_Mat_rows" rows) :int
  (self mat))


;; Scalar::Scalar()
;; Scalar* cv_create_Scalar0()
(defcfun ("cv_create_Scalar0" scalar-0) scalar)


;; Scalar::Scalar(double v0, double v1, double v2, double v3)
;; Scalar* cv_create_Scalar4(double val0, (double val1, double val2, double val3)
(defcfun ("cv_create_Scalar4" scalar-4) scalar
	 (v0 :double)
	 (v1 :double)
	 (v2 :double)
	 (v3 :double))


(defun scalar (&optional (v0 0d0) (v1 0d0) (v2 0d0) (v3 0d0))
  "SCALAR constructor"
  (typecase v0 (null (scalar-0))
	    (integer 
	     (scalar-4 (coerce v0 'double-float) (coerce v1 'double-float) 
		       (coerce v2 'double-float) (coerce v3 'double-float)))
	    (double-float
	     (scalar-4 v0 v1 v2 v3))
            (t (error "The value ~a is not of type (SIGNED-BYTE 32) or DOUBLE-FLOAT." v0))))


(defun make-scalar (&optional (v0 0d0) (v1 0d0) (v2 0d0) (v3 0d0))
  "SCALAR constructor"
  (typecase v0 (null (scalar-0))
	    (integer 
	     (scalar-4 (coerce v0 'double-float) (coerce v1 'double-float) 
		       (coerce v2 'double-float) (coerce v3 'double-float)))
	    (double-float
	     (scalar-4 v0 v1 v2 v3))
            (t (error "The value ~a is not of type (SIGNED-BYTE 32) or DOUBLE-FLOAT." v0))))


;; Scalar::Scalar::all(double v0)
;; Scalar* cv_create_scalarAll(double val0123)
(defcfun ("cv_create_scalarAll" %scalar-all) scalar
  (v0 :double))


(defun scalar-all (v0)
       "SCALAR conctctor - Initializes all of 
        the scalar values 0...3 with V0"
       (%scalar-all (coerce v0 'double-float)))


(defun make-scalar-all (v0)
       "SCALAR conctctor - Initializes all of 
        the scalar values 0...3 with v0"
       (%scalar-all (coerce v0 'double-float)))


;; MatExpr * operator
;; MatExpr* cv_Mat_scale(MatExpr* m, double alpha)
(defcfun ("cv_Mat_scale" scale) mat-expr
  (m mat-expr)
  (alpha :double))


;; Size_()
;; Size* cv_create_Size() 
(defcfun ("cv_create_Size" size-0) size
  "Create SIZE construct")


;; Size_(_Tp _width, _Tp _height)
;; cv_create_Size2(double width, double height)
(defcfun ("cv_create_Size2" size-2) size
  "SIZE constructor"
  (width :double)
  (height :double))


(defun make-size (&optional arg1 arg2)
  (cond ((null arg1)
	 (size-0))
	
	((numberp arg1) 
	 (size-2 (coerce arg1 'double-float) 
		(coerce arg2 'double-float)))

	(t nil)))


;; Size_<float>()
;; Size2f* cv_create_Size2f(float width, float height)
(defcfun ("cv_create_Size2f" size-2f-0) size-2f)


;; Size_<float>(float width, float height)
;; Size2f* cv_create_Size2f(float width, float height)
(defcfun ("cv_create_Size2f" size-2f-2) size-2f
  (width :float)
  (height :float))


(defun size-2f (&optional (width nil) (height nil))
  "SIZE-2F constructor"
  (cond ((eq (or width height) nil)
	 (size-2f-0))
	 ((and width height)
	  (size-2f-2 width height))
	  (t nil)))


(defun make-size-2f (&optional (width nil) (height nil))
  "SIZE-2F constructor"
  (cond ((eq (or width height) nil)
	 (size-2f-0))
	 ((and width height)
	  (size-2f-2 width height))
	  (t nil)))


;; _Tp width, height
;; float cv_Size2f_height(Size* self) 
(defcfun ("cv_Size2f_height" size-2f-height) :float
  "Gets the height of a SIZE-2F"
  (self size-2f))


;; _Tp width, height
;; float cv_Size2f_width(Size* self) 
(defcfun ("cv_Size2f_width" size-2f-width) :float
  "Gets the width of a SIZE-2F"
  (self size-2f))


;; Size* cv_Size_assignTo(Size* self, Size* other) 
(defcfun ("cv_Size_assignTo" size-assgn-to) size
  "Assign data from one SIZE object to another,
   OTHER to SELF."
  (self size)
  (other size))


;; Size* cv_Size_fromPoint(Point* p)
(defcfun ("cv_Size_fromPoint" size-from-point) size
  "Create a SIZE object from POINT data."
  (p point))


;; _Tp width, height
;; double cv_Size_height(Size* self) 
(defcfun ("cv_Size_height" size-height) :double
  "Gets the height of a SIZE construct"
  (self size))


;; _Tp width, height
;; double cv_Size_width(Size* self) 
(defcfun ("cv_Size_width" size-width) :double
  "Gets the width of a SIZE construct"
  (self size)) 


;; size_t Mat::step1(int i=0 ) const
;; size_t cv_Mat_step1(Mat* self) 
(defcfun ("cv_Mat_step1" step1) :unsigned-int
  (self mat))


;; MatExpr - operator
;; MatExpr* cv_Mat_sub(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_sub" sub) mat-expr
  (m1 mat)
  (m2 mat))


;; TermCriteria::TermCriteria(int type, int maxCount, double epsilon)
;; TermCriteria* cv_create_TermCriteria(int type, int maxCount, double epsilon) 
(defcfun ("cv_create_TermCriteria" term-criteria-0) term-criteria)


;; TermCriteria::TermCriteria(int type, int maxCount, double epsilon)
;; TermCriteria* cv_create_TermCriteria(int type, int maxCount, double epsilon) 
(defcfun ("cv_create_TermCriteria3" term-criteria-3) term-criteria
  (type :int)
  (max-count :int)
  (epsilon :double))


(defun term-criteria (&optional type max-count epsilon)
       (cond ((eq type nil)
	      (term-criteria-0))
	      (type
	       (term-criteria-3 type max-count epsilon))
	       (t nil)))


(defun make-term-criteria (&optional type max-count epsilon)
       (cond ((eq type nil)
	      (term-criteria-0))
	      (type
	       (term-criteria-3 type max-count epsilon))
	       (t nil)))


;; size_t Mat::total() const
;; size_t cv_Mat_total(Mat* self)
(defcfun ("cv_Mat_total" total) :unsigned-int
  "Returns the total number of array elements."
  (self mat))


(defun train-idx (self)
	 (mem-aref (c-pointer self) :int 1))


;; typedef Vec<uchar, 2> Vec2b;
;; Vec2##t * cv_create_0_Vec2##t()
(defcfun ("cv_create_0_Vec2b" vec-2b-0) vec-2b)


;; typedef Vec<uchar, 2> Vec2b;
;; Vec2##t * cv_create_Vec2##t(tn v0, tn v1)
(defcfun ("cv_create_Vec2b" vec-2b-2) vec-2b
  (v0 :uchar)
  (v1 :uchar))


(defun vec-2b (&optional v0 v1)
  "VEC-2B constructor"
  (cond ((null v0)
	 (vec-2b-0))
	(v0
	 (vec-2b-2 v0 v1))
	(t nil)))


(defun make-vec-2b (&optional v0 v1)
  "VEC-2B constructor"
  (cond ((null v0)
	 (vec-2b-0))
	(v0
	 (vec-2b-2 v0 v1))
	(t nil)))


;; typedef Vec<uchar, 3> Vec3b;
;; Vec3##t * cv_create_0_Vec3##t()
(defcfun ("cv_create_0_Vec3b" vec-3b-0) vec-3b)


;; typedef Vec<uchar, 3> Vec3b;
;; Vec3##t * cv_create_Vec3##t(tn v0, tn v1, tn v2)
(defcfun ("cv_create_Vec3b" vec-3b-3) vec-3b
  (v0 :uchar)
  (v1 :uchar)
  (v2 :uchar))


(defun vec-3b (&optional v0 v1 v2)
  "VEC-3B constructor"
  (cond ((null v0)
	 (vec-3b-0))
	(v0
	 (vec-3b-3 v0 v1 v2))
	(t nil)))


(defun make-vec-3b (&optional v0 v1 v2)
  "VEC-3B constructor"
  (cond ((null v0)
	 (vec-3b-0))
	(v0
	 (vec-3b-3 v0 v1 v2))
	(t nil)))


;; typedef Vec<uchar, 4> Vec4b;
;; Vec4##t * cv_create_0_Vec4##()
(defcfun ("cv_create_0_Vec4b" vec-4b-0) vec-4b)


;; typedef Vec<uchar, 4> Vec4b;
;; Vec4##t * cv_create_Vec4##(tn v0, tn v1, tn v2, tn v3)
(defcfun ("cv_create_Vec4b" vec-4b-4) vec-4b
  (v0 :uchar)
  (v1 :uchar)
  (v2 :uchar)
  (v3 :uchar))


(defun vec-4b (&optional v0 v1 v2 v3)
  "VEC-4B constructor"
  (cond ((null v0)
	 (vec-4b-0))
	(v0
	 (vec-4b-4 v0 v1 v2 v3))
	(t nil)))


(defun make-vec-4b (&optional v0 v1 v2 v3)
  "VEC-4B constructor"
  (cond ((null v0)
	 (vec-4b-0))
	(v0
	 (vec-4b-4 v0 v1 v2 v3))
	(t nil)))


;; typedef Vec<double, 2> Vec2d;
;; Vec2##t * cv_create_0_Vec2##()
(defcfun ("cv_create_0_Vec2d" vec-2d-0) vec-2d)


;; typedef Vec<double, 2> Vec2d;
;; Vec2##t * cv_create_Vec2##(tn v0, tn v1)
(defcfun ("cv_create_Vec2d" vec-2d-2) vec-2d
  (v0 :double)
  (v1 :double))


(defun vec-2d (&optional v0 v1)
  "VEC-2D constructor"
  (cond ((null v0)
	 (vec-2d-0))
	(v0
	 (vec-2d-2 v0 v1))
	(t nil)))


(defun make-vec-2d (&optional v0 v1)
  "VEC-2D constructor"
  (cond ((null v0)
	 (vec-2d-0))
	(v0
	 (vec-2d-2 v0 v1))
	(t nil)))


;; typedef Vec<double, 3> Vec3d;
;; Vec3##t * cv_create_0_Vec3##()
(defcfun ("cv_create_0_Vec3d" vec-3d-0) vec-3d)


;; typedef Vec<double, 3> Vec3d;
;; Vec3##t * cv_create_Vec3##(tn v0, tn v1, tn v2)
(defcfun ("cv_create_Vec3d" vec-3d-3) vec-3d
  (v0 :double)
  (v1 :double)
  (v2 :double))


(defun vec-3d (&optional v0 v1 v2)
  "VEC-3D constructor"
  (cond ((null v0)
	 (vec-3d-0))
	(v0
	 (vec-3d-3 v0 v1 v2))
	(t nil)))


(defun make-vec-3d (&optional v0 v1 v2)
  "VEC-3D constructor"
  (cond ((null v0)
	 (vec-3d-0))
	(v0
	 (vec-3d-3 v0 v1 v2))
	(t nil)))


;; typedef Vec<double, 4> Vec4d;
;; Vec4##t * cv_create_0_Vec4##()
(defcfun ("cv_create_0_Vec4d" vec-4d-0) vec-4d)


;; typedef Vec<double, 4> Vec4d;
;; Vec4##t * cv_create_Vec4##(tn v0, tn v1, tn v2, tn v3)
(defcfun ("cv_create_Vec4d" vec-4d-4) vec-4d
  (v0 :double)
  (v1 :double)
  (v2 :double)
  (v3 :double))


(defun vec-4d (&optional v0 v1 v2 v3)
  "VEC-4D constructor"
  (cond ((null v0)
	 (vec-4d-0))
	(v0
	 (vec-4d-4 v0 v1 v2 v3))
	(t nil)))


(defun make-vec-4d (&optional v0 v1 v2 v3)
  "VEC-4D constructor"
  (cond ((null v0)
	 (vec-4d-0))
	(v0
	 (vec-4d-4 v0 v1 v2 v3))
	(t nil)))


;; typedef Vec<double, 6> Vec6d;
;; Vec6##t * cv_create_0_Vec6##()
(defcfun ("cv_create_0_Vec6d" vec-6d-0) vec-6d)


;; typedef Vec<double, 6> Vec6d;
;; Vec6##t * cv_create_Vec6##(tn v0, tn v1, tn v2, tn v3, tn v4, tn v5)
(defcfun ("cv_create_Vec6d" vec-6d-6) vec-6d
  (v0 :double)
  (v1 :double)
  (v2 :double)
  (v3 :double)
  (v4 :double)
  (v5 :double))


(defun vec-6d (&optional v0 v1 v2 v3 v4 v5)
  "VEC-6D constructor"
  (cond ((null v0)
	 (vec-6d-0))
	(v0
	 (vec-6d-6 v0 v1 v2 v3 v4 v5))
	(t nil)))


(defun make-vec-6d (&optional v0 v1 v2 v3 v4 v5)
  "VEC-6D constructor"
  (cond ((null v0)
	 (vec-6d-0))
	(v0
	 (vec-6d-6 v0 v1 v2 v3 v4 v5))
	(t nil)))


;; typedef Vec<float, 2> Vec2f
;; Vec2##t * cv_create_0_Vec2##()
(defcfun ("cv_create_0_Vec2f" vec-2f-0) vec-2f)


;; typedef Vec<float, 2> Vec2f
;; Vec2##t * cv_create_Vec2##(tn v0, tn v1)
(defcfun ("cv_create_Vec2f" vec-2f-2) vec-2f
  (v0 :float)
  (v1 :float))


(defun vec-2f (&optional v0 v1)
  "VEC-2F constructor"
  (cond ((null v0)
	 (vec-2f-0))
	(v0
	 (vec-2f-2 v0 v1))
	(t nil)))


(defun make-vec-2f (&optional v0 v1)
  "VEC-2F constructor"
  (cond ((null v0)
	 (vec-2f-0))
	(v0
	 (vec-2f-2 v0 v1))
	(t nil)))


;; typedef Vec<float, 3> Vec3f
;; Vec3##t * cv_create_0_Vec3##()
(defcfun ("cv_create_0_Vec3f" vec-3f-0) vec-3f)


;; typedef Vec<float, 3> Vec3f
;; Vec3##t * cv_create_Vec3##(tn v0, tn v1, tn v2)
(defcfun ("cv_create_Vec3f" vec-3f-3) vec-3f
  (v0 :float)
  (v1 :float)
  (v2 :float))


(defun vec-3f (&optional v0 v1 v2)
  "VEC-3F constructor"
  (cond ((null v0)
	 (vec-3f-0))
	(v0
	 (vec-3f-3 v0 v1 v2))
	(t nil)))


(defun make-vec-3f (&optional v0 v1 v2)
  "VEC-3F constructor"
  (cond ((null v0)
	 (vec-3f-0))
	(v0
	 (vec-3f-3 v0 v1 v2))
	(t nil)))


;; typedef Vec<float, 4> Vec4f
;; Vec4##t * cv_create_0_Vec4##()
(defcfun ("cv_create_0_Vec4f" vec-4f-0) vec-4f)


;; typedef Vec<float, 4> Vec4f
;; Vec4##t * cv_create_Vec4##(tn v0, tn v1, tn v2, tn v3)
(defcfun ("cv_create_Vec4f" vec-4f-4) vec-4f 
  (v0 :float)
  (v1 :float)
  (v2 :float)
  (v3 :float))


(defun vec-4f (&optional v0 v1 v2 v3)
  "VEC-4F constructor"
  (cond ((null v0)
	 (vec-4f-0))
	(v0
	 (vec-4f-4 v0 v1 v2 v3))
	(t nil)))


(defun make-vec-4f (&optional v0 v1 v2 v3)
  "VEC-4F constructor"
  (cond ((null v0)
	 (vec-4f-0))
	(v0
	 (vec-4f-4 v0 v1 v2 v3))
	(t nil)))


;; typedef Vec<float, 6> Vec6f;
;; Vec6##t * cv_create_0_Vec6##()
(defcfun ("cv_create_0_Vec6f" vec-6f-0) vec-6f)


;; typedef Vec<float, 6> Vec6f;
;; Vec6##t * cv_create_Vec6##(tn v0, tn v1, tn v2, tn v3, tn v4, tn v5)
(defcfun ("cv_create_Vec6f" vec-6f-6) vec-6f
  (v0 :float)
  (v1 :float)
  (v2 :float)
  (v3 :float)
  (v4 :float)
  (v5 :float))


(defun vec-6f (&optional v0 v1 v2 v3 v4 v5)
  "VEC-6F constructor"
  (cond ((null v0)
	 (vec-6f-0))
	(v0
	 (vec-6f-6 v0 v1 v2 v3 v4 v5))
	(t nil)))


(defun make-vec-6f (&optional v0 v1 v2 v3 v4 v5)
  "VEC-6F constructor"
  (cond ((null v0)
	 (vec-6f-0))
	(v0
	 (vec-6f-6 v0 v1 v2 v3 v4 v5))
	(t nil)))



;; typedef Vec<int, 2> Vec2i;
;; Vec2##t * cv_create_0_Vec2##()
(defcfun ("cv_create_0_Vec2i" vec-2i-0) vec-2i)


;; typedef Vec<int, 2> Vec2i;
;; Vec2##t * cv_create_Vec2##(tn v0, tn v1)
(defcfun ("cv_create_Vec2i" vec-2i-2) vec-2i
  (v0 :int)
  (v1 :int))


(defun vec-2i (&optional v0 v1)
  "VEC-2I constructor"
  (cond ((null v0)
	 (vec-2i-0))
	(v0
	 (vec-2i-2 v0 v1))
	(t nil)))


(defun make-vec-2i (&optional v0 v1)
  "VEC-2I constructor"
  (cond ((null v0)
	 (vec-2i-0))
	(v0
	 (vec-2i-2 v0 v1))
	(t nil)))


;; typedef Vec<int, 3> Vec3i;
;; Vec3##t * cv_create_0_Vec3##()
(defcfun ("cv_create_0_Vec3i" vec-3i-0) vec-3i)


;; typedef Vec<int, 3> Vec3i;
;; Vec3##t * cv_create_Vec3##(tn v0, tn v1, tn v2)
(defcfun ("cv_create_Vec3i" vec-3i-3) vec-3i
  (v0 :int)
  (v1 :int)
  (v2 :int))


(defun vec-3i (&optional v0 v1 v2)
  "VEC-3I constructor"
  (cond ((null v0)
	 (vec-3i-0))
	(v0
	 (vec-3i-3 v0 v1 v2))
	(t nil)))


(defun make-vec-3i (&optional v0 v1 v2)
  "VEC-3I constructor"
  (cond ((null v0)
	 (vec-3i-0))
	(v0
	 (vec-3i-3 v0 v1 v2))
	(t nil)))



;; typedef Vec<int, 4> Vec4i;
;; Vec4##t * cv_create_0_Vec4##()
(defcfun ("cv_create_0_Vec4i" vec-4i-0) vec-4i)


;; typedef Vec<int, 4> Vec4i;
;; Vec4##t * cv_create_Vec4##(tn v0, tn v1, tn v2, tn v3)
(defcfun ("cv_create_Vec4i" vec-4i-4) vec-4i
  (v0 :int)
  (v1 :int)
  (v2 :int)
  (v3 :int))

(defun vec-4i (&optional v0 v1 v2 v3)
  "VEC-4I constructor"
  (cond ((null v0)
	 (vec-4i-0))
	(v0
	 (vec-4i-4 v0 v1 v2 v3))
	(t nil)))


(defun make-vec-4i (&optional v0 v1 v2 v3)
  "VEC-4I constructor"
  (cond ((null v0)
	 (vec-4i-0))
	(v0
	 (vec-4i-4 v0 v1 v2 v3))
	(t nil)))


;; typedef Vec<int, 6> Vec6i;
;; Vec6##t * cv_create_0_Vec6##()
(defcfun ("cv_create_0_Vec6i" vec-6i-0) vec-6i)


;; typedef Vec<int, 6> Vec6i;
;; Vec6##t * cv_create_Vec6##(tn v0, tn v1, tn v2, tn v3, tn v4, tn v5)
(defcfun ("cv_create_Vec6i" vec-6i-6) vec-6i
  (v0 :int)
  (v1 :int)
  (v2 :int)
  (v3 :int)
  (v4 :int)
  (v5 :int))


(defun vec-6i (&optional v0 v1 v2 v3 v4 v5)
  "VEC-6I constructor"
  (cond ((null v0)
	 (vec-6i-0))
	(v0
	 (vec-6i-6 v0 v1 v2 v3 v4 v5))
	(t nil)))


(defun make-vec-6i (&optional v0 v1 v2 v3 v4 v5)
  "VEC-6I constructor"
  (cond ((null v0)
	 (vec-6i-0))
	(v0
	 (vec-6i-6 v0 v1 v2 v3 v4 v5))
	(t nil)))


;; typedef Vec<int, 8> Vec8i;
;; Vec8##t * cv_create_0_Vec8##() 
(defcfun ("cv_create_0_Vec8i" vec-8i-0) vec-8i)


;; typedef Vec<int, 8> Vec8i;
;; Vec8##t * cv_create_Vec8##(tn v0, tn v1, tn v2, tn v3, tn v4, tn v5, tn v6, tn v7)
(defcfun ("cv_create_Vec8i" vec-8i-8) vec-8i
  (v0 :int)
  (v1 :int)
  (v2 :int)
  (v3 :int)
  (v4 :int)
  (v5 :int)
  (v6 :int)
  (v7 :int))


(defun vec-8i (&optional v0 v1 v2 v3 v4 v5 v6 v7)
  "VEC-8I constructor"
  (cond ((null v0)
	 (vec-8i-0))
	(v0
	 (vec-8i-8 v0 v1 v2 v3 v4 v5 v6 v7))
	(t nil)))


(defun make-vec-8i (&optional v0 v1 v2 v3 v4 v5 v6 v7)
  "VEC-8I constructor"
  (cond ((null v0)
	 (vec-8i-0))
	(v0
	 (vec-8i-8 v0 v1 v2 v3 v4 v5 v6 v7))
	(t nil)))


;; typedef Vec<short, 2> Vec2s;
;; Vec2##t * cv_create_0_Vec2##()
(defcfun ("cv_create_0_Vec2s" vec-2s-0) vec-2s)


;; typedef Vec<short, 2> Vec2s;
;; Vec2##t * cv_create_Vec2##(tn v0, tn v1)
(defcfun ("cv_create_Vec2s" vec-2s-2) vec-2s
  (v0 :short)
  (v1 :short))


(defun vec-2s (&optional v0 v1)
  "VEC-2S constructor"
  (cond ((null v0)
	 (vec-2s-0))
	(v0
	 (vec-2s-2 v0 v1))
	(t nil)))


(defun make-vec-2s (&optional v0 v1)
  "VEC-2S constructor"
  (cond ((null v0)
	 (vec-2s-0))
	(v0
	 (vec-2s-2 v0 v1))
	(t nil)))


;; typedef Vec<short, 3> Vec3s;
;; Vec3##t * cv_create_0_Vec3##()
(defcfun ("cv_create_0_Vec3s" vec-3s-0) vec-3s)


;; typedef Vec<short, 3> Vec3s;
;; Vec3##t * cv_create_Vec3##(tn v0, tn v1, tn v2)
(defcfun ("cv_create_Vec3s" vec-3s-3) vec-3s
  (v0 :short)
  (v1 :short)
  (v2 :short))


(defun vec-3s (&optional v0 v1 v2)
  "VEC-3S constructor"
  (cond ((null v0)
	 (vec-3s-0))
	(v0
	 (vec-3s-3 v0 v1 v2))
	(t nil)))


(defun make-vec-3s (&optional v0 v1 v2)
  "VEC-3S constructor"
  (cond ((null v0)
	 (vec-3s-0))
	(v0
	 (vec-3s-3 v0 v1 v2))
	(t nil)))


;; typedef Vec<short, 4> Vec4s;
;; Vec4##t * cv_create_0_Vec4##()
(defcfun ("cv_create_0_Vec4s" vec-4s-0) vec-4s)


;; typedef Vec<short, 4> Vec4s;
;; Vec4##t * cv_create_Vec4##(tn v0, tn v1, tn v2, tn v3)
(defcfun ("cv_create_Vec4s" vec-4s-4) vec-4s
  (v0 :short)
  (v1 :short)
  (v2 :short)
  (v3 :short))


(defun vec-4s (&optional v0 v1 v2 v3)
  "VEC-4S constructor"
  (cond ((null v0)
	 (vec-4s-0))
	(v0
	 (vec-4s-4 v0 v1 v2 v3))
	(t nil)))


(defun make-vec-4s (&optional v0 v1 v2 v3)
  "VEC-4S constructor"
  (cond ((null v0)
	 (vec-4s-0))
	(v0
	 (vec-4s-4 v0 v1 v2 v3))
	(t nil)))


;; typedef Vec<ushort, 2> Vec2w;
;; Vec2##t * cv_create_0_Vec2##()
(defcfun ("cv_create_0_Vec2w" vec-2w-0) vec-2w)


;; typedef Vec<ushort, 2> Vec2w;
;; Vec2##t * cv_create_Vec2##(tn v0, tn v1)
(defcfun ("cv_create_Vec2w" vec-2w-2) vec-2w
  (v0 :ushort)
  (v1 :ushort))


(defun vec-2w (&optional v0 v1)
  "VEC-2W constructor"
  (cond ((null v0)
	 (vec-2w-0))
	(v0
	 (vec-2w-2 v0 v1))
	(t nil)))


(defun make-vec-2w (&optional v0 v1)
  "VEC-2W constructor"
  (cond ((null v0)
	 (vec-2w-0))
	(v0
	 (vec-2w-2 v0 v1))
	(t nil)))


;; typedef Vec<ushort, 3> Vec3w;
;; Vec3##t * cv_create_0_Vec3##()
(defcfun ("cv_create_0_Vec3w" vec-3w-0) vec-3w)


;; typedef Vec<ushort, 3> Vec3w;
;; Vec3##t * cv_create_Vec3##(tn v0, tn v1, tn v2)
(defcfun ("cv_create_Vec3w" vec-3w-3) vec-3w
  (v0 :ushort)
  (v1 :ushort)
  (v2 :ushort))


(defun vec-3w (&optional v0 v1 v2)
  "VEC-3W constructor"
  (cond ((null v0)
	 (vec-3w-0))
	(v0
	 (vec-3w-3 v0 v1 v2))
	(t nil)))


(defun make-vec-3w (&optional v0 v1 v2)
  "VEC-3W constructor"
  (cond ((null v0)
	 (vec-3w-0))
	(v0
	 (vec-3w-3 v0 v1 v2))
	(t nil)))


;; typedef Vec<ushort, 4> Vec4w;
;; Vec4##t * cv_create_0_Vec4##()
(defcfun ("cv_create_0_Vec4w" vec-4w-0) vec-4w)


;; typedef Vec<ushort, 4> Vec4w;
;; Vec4##t * cv_create_Vec4##(tn v0, tn v1, tn v2, tn v3)
(defcfun ("cv_create_Vec4w" vec-4w-4) vec-4w
  (v0 :ushort)
  (v1 :ushort)
  (v2 :ushort)
  (v3 :ushort))


(defun vec-4w (&optional v0 v1 v2 v3)
  "VEC-4W constructor"
  (cond ((null v0)
	 (vec-4w-0))
	(v0
	 (vec-4w-4 v0 v1 v2 v3))
	(t nil)))


(defun make-vec-4w (&optional v0 v1 v2 v3)
  "VEC-4W constructor"
  (cond ((null v0)
	 (vec-4w-0))
	(v0
	 (vec-4w-4 v0 v1 v2 v3))
	(t nil)))


;;; Basic Structures



;;; Operations on Arrays



;; MatExpr abs(const Mat& m)
;; MatExpr* cv_abs(Mat* m)
(defcfun ("cv_abs" %abs) mat-expr
  "Calculates an absolute value of each matrix element."
  (m mat))


;; void exp(InputArray src, OutputArray dst)
;; void cv_exp(Mat* src, Mat* dst)
(defcfun ("cv_exp" %exp) :void
  "Calculates the exponent of every array element."
  (src mat)
  (dest mat))


;; void log(InputArray src, OutputArray dst)
;; void cv_log(Mat* src, Mat* dst)
(defcfun ("cv_log" %log) :int
  "Calculates the natural logarithm of every array element."
  (src mat)
  (dest mat))


;; void max(InputArray src1, InputArray src2, OutputArray dst)
;; void cv_max(Mat* src1, Mat* src2, Mat* dst)
(defcfun ("cv_max" %max) :void 
	 "Calculates per-element maximum of two arrays."
	 (src1 mat)
	 (src2 mat)
	 (dest mat))


;; void min(InputArray src1, InputArray src2, OutputArray dst)
;; void cv_min(Mat* src1, Mat* src2, Mat* dst)
(defcfun ("cv_min" %min) :void 
	 "Calculates per-element minimum of two arrays."
	 (src1 mat)
	 (src2 mat)
	 (dest mat))


;; C++: void absdiff(InputArray src1, InputArray src2, OutputArray dst)
;; void cv_absdiff(Mat* src1, Mat* src2, Mat* dst) 
(defcfun ("cv_absdiff" absdiff) :void
  "Calculates the per-element absolute difference between two arrays or between an array and a scalar."
  (src1 mat)
  (src2 mat)
  (dest mat))


;; void addWeighted(InputArray src1, double alpha, InputArray src2, double beta, double gamma, OutputArray dst, int dtype=-1)
;; void cv_addWeighted(Mat* src1, double alpha, Mat* src2, double beta, double gamma, Mat* dst, int dtype)
(cffi:defcfun ("cv_addWeighted" %add-weighted) :void
  (src1 mat)
  (alpha :double)
  (src2 mat)
  (beta :double)
  (gamma :double)
  (dest mat)
  (dtype :int))


(defun add-weighted (src1 alpha src2 beta gamma dest &optional (dtype -1))
       "Calculates the weighted sum of two arrays."
       (%add-weighted src1 alpha src2 beta gamma dest dtype))


;; void bitwise_and(InputArray src1, InputArray src2, OutputArray dst, InputArray mask=noArray())
;; void cv_bitwise_and(Mat* src1, Mat* src2, Mat* dst, Mat* mask)
(defcfun ("cv_bitwise_and" %bitwise-and) :void 
  (src1 mat)
  (src2 mat)
  (dest mat)
  (mask mat))


(defun bitwise-and (src1 src2 dest &optional (mask (%mat) given-mask))
  "Calculates the per-element bit-wise conjunction of two arrays."
  (%bitwise-and src1 src2 dest mask)
  (if given-mask nil (del-mat mask)))


;; void bitwise_not(InputArray src, OutputArray dst, InputArray mask=noArray())
;; void cv_bitwise_not(Mat* src, Mat* dst, Mat* mask)
(defcfun ("cv_bitwise_not" %bitwise-not) :void 
  (src mat)
  (dest mat)
  (mask mat))


(defun bitwise-not (src dest &optional (mask (%mat) given-mask))
  "Inverts every bit of an array."
  (%bitwise-not src dest mask)
  (if given-mask nil (del-mat mask)))


;; void bitwise_or(InputArray src1, InputArray src2, OutputArray dst, InputArray mask=noArray())
;; void cv_bitwise_or(Mat* src1, Mat* src2, Mat* dst, Mat* mask)
(defcfun ("cv_bitwise_or" %bitwise-or) :void 
  (src1 mat)
  (src2 mat)
  (dest mat)
  (mask mat))

(defun bitwise-or (src1 src2 dest &optional (mask (%mat) given-mask))
  "Calculates the per-element bit-wise disjunction of two arrays."
  (%bitwise-or src1 src2 dest mask)
  (if given-mask nil (del-mat mask)))


;; void bitwise_xor(InputArray src1, InputArray src2, OutputArray dst, InputArray mask=noArray())
;; void cv_bitwise_xor(Mat* src1, Mat* src2, Mat* dst, Mat* mask)
(defcfun ("cv_bitwise_xor" %bitwise-xor) :void
  "Calculates the per-element bit-wise “exclusive or” operation on two arrays."
  (src1 mat)
  (src2 mat)
  (dest mat)
  (mask mat))


(defun bitwise-xor (src1 src2 dest &optional (mask (%mat) given-mask))
  "Calculates the per-element bit-wise disjunction of two arrays."
  (%bitwise-xor src1 src2 dest mask)
  (if given-mask nil (del-mat mask)))


;; void calcCovarMatrix(InputArray samples, OutputArray covar, InputOutputArray mean, int flags, int ctype=CV_64F)
;; void cv_calcCovarMatrix(Mat* samples, Mat* covar, Mat* mean, int flags, int ctype)
(cffi:defcfun ("cv_calcCovarMatrix" %calc-covar-matrix) :void
  (samples mat)
  (covar mat)
  (mean mat)
  (flags :int)
  (ctype :int))


(defun calc-covar-matrix (samples covar mean flags &optional (ctype +64f+))
       "Calculates the covariance matrix of a set of vectors."
       (%calc-covar-matrix samples covar mean flags ctype))


;; bool checkRange(InputArray a, bool quiet=true, Point* pos=0, double minVal=-DBL_MAX, double maxVal=DBL_MAX )
;; bool cv_checkRange(Mat* a, bool quiet, Point* pos, double minVal, double maxVal) 
(defcfun ("cv_checkRange" %check-range) :boolean
  (a mat)
  (quiet :boolean)
  (pos point)
  (min-val :double)
  (max-val :double))


(defun check-range (a &optional (quiet t) (pos (null-pointer)) (min-val +-dbl-max+) (max-val +dbl-max+))
  (%check-range a quiet pos min-val max-val))


;; void completeSymm(InputOutputArray mtx, bool lowerToUpper=false)
;; void cv_completeSymm(Mat* mtx, bool lowerToUpper)
(defcfun ("cv_completeSymm" %complete-symm) :void
  (mtx mat)
  (lower-to-upper :boolean))


(defun complete-symm (mtx &optional (lower-to-upper nil))
  (%complete-symm mtx lower-to-upper))


;; void convertScaleAbs(InputArray src, OutputArray dst, double alpha=1, double beta=0)
;; void cv_convertScaleAbs(Mat* src, Mat* dst, double alpha, double beta)
(defcfun ("cv_convertScaleAbs" %convert-scale-abs) :void
  (src mat)
  (dest mat)
  (alpha :double)
  (beta :double))


(defun convert-scale-abs (src dest &optional (alpha 1d0) (beta 0d0))
       "Scales, calculates absolute values, and converts the result to 8-bit."
       (%convert-scale-abs src dest alpha beta))


;; double determinant(InputArray mtx)
;; double cv_determinant(Mat* mtx) 
(defcfun ("cv_determinant" det) :double 
	 "Returns the determinant of a square floating-point matrix."
	 (mtx mat))


;; void divide(InputArray src1, InputArray src2, OutputArray dst, double scale=1, int dtype=-1)
;; void cv_divide(Mat* src1, Mat* src2, Mat* dst, double scale, int dtype)
(defcfun ("cv_divide" divide5) :void
  (src1 mat)
  (src2 mat)
  (dest mat)
  (scale :double)
  (dtype :int))


;; void divide(double scale, InputArray src2, OutputArray dst, int dtype=-1)
;; void cv_divide4(double scale, Mat* src2, Mat* dst, int dtype)
(defcfun ("cv_divide4" divide4) :void
  (scale :double)
  (src2 mat)
  (dest mat)
  (dtype :int))


(defun divide (&optional arg1 arg2 arg3 (arg4 1d0) (arg5 -1))
       (cond ((eq (type-of arg1) 'cv-mat)
	      (divide5 arg1 arg2 arg3 arg4 arg5))
	      ((eq (type-of arg1) 'double-float)
	       (divide4 arg1 arg2 arg3 arg5))
	       (t nil)))


;; void flip(InputArray src, OutputArray dst, int flipCode)
;; void cv_flip(Mat* src, Mat* dst, int flipCode)
(defcfun ("cv_flip" flip) :void
  "Flips a 2D array around vertical, horizontal, or both axes."
  (src mat)
  (dest mat)
  (flip-code :int))


;; void inRange(InputArray src, InputArray lowerb, InputArray upperb, OutputArray dst)
;; void cv_inRangeS(Mat* src, Scalar* lowerb, Scalar* upperb, Mat* dst)
(defcfun ("cv_inRangeS" in-range-s) :void
  "Checks if array elements lie between the elements of two scalar values."
  (src mat)
  (lowerb scalar)
  (upperb scalar)
  (dst mat))


;; MatExpr Mat::inv(int method=DECOMP_LU) const
;; MatExpr* cv_Mat_inv_mat(Mat* self, int method)
(defcfun ("cv_Mat_inv_mat" inv) mat-expr 
	 "Inverses a matrix."
	 (self mat)
	 (method :int))


;; double invert(InputArray src, OutputArray dst, int flags=DECOMP_LU)
;; double cv_invert(Mat* src, Mat* dst, int flags)
(defcfun ("cv_invert" %invert) :double
  (src mat)
  (dest mat)
  (flags :int))


(defun invert (src dest &optional (flags +decomp-lu+))
       "Finds the inverse or pseudo-inverse of a matrix."
       (%invert src dest flags))


;; bool Mat::isContinuous() const
;; bool cv_Mat_isContinuous(Mat* self) 
(defcfun ("cv_Mat_isContinuous" is-continuous) :boolean
  (self mat))


;; void magnitude(InputArray x, InputArray y, OutputArray magnitude)
;; void cv_magnitude(Mat* x, Mat* y, Mat* magnitude)
(cffi:defcfun ("cv_magnitude" magnitude) :void
  "Calculates the magnitude of 2D vectors."
  (x mat)
  (y mat)
  (magnitude mat))


;; double Mahalanobis(InputArray v1, InputArray v2, InputArray icovar)
;; double cv_Mahalanobis(Mat* v1, Mat* v2, Mat* icovar)
(cffi:defcfun ("cv_Mahalanobis" mahalanobis) :double
  (v1 mat)
  (v2 mat)
  (icovar mat))


;; Scalar mean(InputArray src, InputArray mask=noArray())
;; Scalar* cv_mean(Mat* src, Mat* mask)
(defcfun ("cv_mean" %mean) scalar
  (src mat)
  (mask mat))


(defun mean (src &optional (mask (%mat) given-mask))
  "Calculates an average mean of array elements."
  (let ((return (%mean src mask)))
  (if given-mask nil (del-mat mask))
  return))


;; void minMaxLoc(InputArray src, double* minVal, double* maxVal=0, Point* minLoc=0, Point* maxLoc=0, InputArray mask=noArray())
;; void cv_minMaxLoc(Mat* src, double* minVal, double* maxVal, Point* minLoc, Point* maxLoc, Mat* mask)
(defcfun ("cv_minMaxLoc" %min-max-loc) :void
  (src mat)
  (min-val :pointer)
  (max-val :pointer)
  (min-loc point)
  (max-loc point)
  (mask mat))


(defun min-max-loc (src min-val &optional (max-val (null-pointer)) (min-loc (null-pointer)) (max-loc (null-pointer)) (mask (%mat) given-mask))
  "Finds the global minimum and maximum in an array."
  (%min-max-loc src min-val max-val min-loc max-loc mask)
  (if given-mask nil (del-mat mask)))


;; void mulTransposed(InputArray src, OutputArray dst, bool aTa, InputArray delta=noArray(), double scale=1, int dtype=-1 )
;; void cv_mulTransposed(Mat* src, Mat* dst, bool aTa, Mat* delta, double scale, int dtype)
(defcfun ("cv_mulTransposed" %mul-transposed) :void
  (src mat)
  (dest mat)
  (a-t-a :boolean)
  (delta mat)
  (scale :double)
  (dtype :int))


(defun mul-transposed (src dest a-t-a &optional (delta (%mat) given-delta) (scale 1d0) (dtype -1)) 
       (%mul-transposed src dest a-t-a delta scale dtype)
       (if given-delta nil (del-mat delta)))


;; void multiply(InputArray src1, InputArray src2, OutputArray dst, double scale=1, int dtype=-1 )
;; void cv_multiply(Mat* src1, Mat* src2, Mat* dst, double scale, int dtype)
(defcfun ("cv_multiply" %multiply) :void
  (src1 mat)
  (src2 mat)
  (dest mat)
  (scale :double)
  (dtype :int))


(defun multiply (src1 src2 dest &optional (scale 1.0d0) (dtype -1))
       "Calculates the per-element scaled product of two arrays."
       (%multiply src1 src2 dest scale dtype))


;;double norm(InputArray src1, int normType=NORM_L2, InputArray mask=noArray())
;;double cv_norm(Mat* src1, int normType, Mat* mask) 
(defcfun ("cv_norm" %%norm3) :void 
  (src1 mat)
  (norm-type :int)
  (mask mat))


;;double norm(InputArray src1, InputArray src2, int normType=NORM_L2, InputArray mask=noArray())
;;double cv_norm4(Mat* src1, Mat* src2, int normType, Mat* mask)
(defcfun ("cv_norm4" %%norm4) :void 
  (src1 mat)
  (src2 mat)
  (norm-type :int)
  (mask mat))


(defun %norm3 (src1 &optional (norm-type +norm-l2+) (mask (%mat) given-mask) return) 
  (setf return (%%norm3 src1 norm-type mask))
  (if given-mask nil (del-mat mask)) 
  return)


(defun %norm4 (src1 src2 &optional (norm-type +norm-l2+) (mask (%mat) given-mask) return)
  (%%norm4 src1 src2 norm-type mask)
  (if given-mask nil (del-mat mask)) 
  return)


(defun norm (&rest args)
  "Calculates an absolute array norm, an absolute 
   difference norm, or a relative difference norm."
  (if (eq (type-of (second args)) 'cv-mat)
      (apply #'%norm4 args)
      (apply #'%norm3 args)))


;; void normalize(InputArray src, OutputArray dst, double alpha=1, double beta=0, int norm_type=NORM_L2, int dtype=-1, 
;; InputArray mask=noArray() )
;; void cv_normalize(Mat* src, Mat* dst, double alpha, double beta, int norm_type, int dtype, Mat* mask)
(defcfun ("cv_normalize" %normalize) :void
  (src mat)
  (dest mat)
  (alpha :double)
  (beta :double)
  (norm-type :int)
  (dtype :int)
  (mask mat))


(defun normalize (src dest &optional (alpha 1) (beta 0) (norm-type  +norm-l2+) (dtype -1) (mask (%mat) given-mask))
  "Normalizes the norm or value range of an array."
  (%normalize src dest alpha beta norm-type  dtype  mask)
  (if given-mask nil (del-mat mask)))


;; void phase(InputArray x, InputArray y, OutputArray angle, bool angleInDegrees=false)
;; void cv_phase(Mat* x, Mat* y, Mat* angle, bool angleInDegrees) 
(defcfun ("cv_phase" %phase) :void
  (x mat)
  (y mat)
  (angle mat)
  (angle-in-degrees :boolean))


(defun *phase (x y angle &optional (angle-in-degrees nil))
  (%phase x y angle angle-in-degrees))


;; void pow(InputArray src, double power, OutputArray dst)
;; void cv_pow(Mat* src, double power, Mat* dst)
(defcfun ("cv_pow" pow) :void
  "Raises every array element to a power."
  (src mat)
  (power :double)
  (dest mat))


;; void randu(InputOutputArray dst, InputArray low, InputArray high)
;; void cv_randu2(Mat* dst, Scalar* low, Scalar* high)
(defcfun ("cv_randu2" randu) :void
  (dest mat)
  (low scalar)
  (high scalar))


;; void repeat(InputArray src, int ny, int nx, OutputArray dst)
;; void cv_repeat(Mat* src, int ny, int nx, Mat* dst)
(defcfun ("cv_repeat" repeat) :void
  (src mat)
  (ny :int)
  (nx :int)
  (dest mat))


;; RNG::RNG()
(defcfun ("cv_create_RNG" %rng) rng 
	 "RNG constructor") 


;; RNG::RNG(uint64 state)
;; RNG* cv_create_RNG_state(uint64 state)
(defcfun ("cv_create_RNG_state" rng-state) rng 
	 "RNG constructor -  sets the RNG state to the specified value."
	 (state :uint64))


(defun rng (&optional (state nil))
       (cond ((eq state nil)
	      (%rng))
	      ((integerp state) (rng-state state))
	      (t nil)))


(defun make-rng (&optional (state nil))
       (cond ((eq state nil)
	      (%rng))
	      ((integerp state) (rng-state state))
	      (t nil)))


;; void sqrt(InputArray src, OutputArray dst)
;; void cv_sqrt(Mat* src, Mat* dst)
(defcfun ("cv_sqrt" %sqrt) :void
  "Calculates a square root of array elements."
  (src mat)
  (dest mat))


;; void scaleAdd(InputArray src1, double alpha, InputArray src2, OutputArray dst)
;; void cv_scaleAdd(Mat* src1, double alpha, Mat* src2, Mat* dst)
(defcfun ("cv_scaleAdd" scale-add) :void 
	 "Calculates the sum of a scaled array and another array."
	 (src1 mat)
	 (alpha :double)
	 (src2 mat)
	 (dest mat))


;; void subtract(InputArray src1, InputArray src2, OutputArray dst, InputArray mask=noArray(), int dtype=-1)
;; void cv_subtract(Mat* src1, Mat* src2, Mat* dst, Mat* mask, int dtype)
(defcfun ("cv_subtract" %subtract) :void
  "Calculates the per-element difference between two arrays."
  (src1 mat)
  (src2 mat)
  (dest mat)
  (mask mat)
  (dtype :int))


(defun subtract (src1 src2 dest &optional (mask (%mat) given-mask) (dtype -1))
  "Calculates the per-element difference between two arrays or array and a scalar."
  (%subtract src1 src2 dest mask dtype)
  (if given-mask nil (del-mat mask)))


;; Scalar sum(InputArray src)
;; Scalar* cv_sum(Mat* src)
(defcfun ("cv_sum" sum) scalar
  "Calculates the sum of array elements."
  (src mat))


;; double RNG::uniform(double a, double b)
;; double cv_RNG_uniform_double(RNG* self, double a, double b) 
(defcfun ("cv_RNG_uniform_double" uniform-d) :double
  "Returns the next random number sampled from the uniform distribution."
  (self rng)
  (a :double)
  (b :double))


;; float RNG::uniform(float a, float b)
;; float cv_RNG_uniform_float(RNG* self, float a, float b)
(defcfun ("cv_RNG_uniform_float" uniform-f) :float
  "Returns the next random number sampled from the uniform distribution."
  (self rng)
  (a :float)
  (b :float))


;; int RNG::uniform(int a, int b)
;; int cv_RNG_uniform_int(RNG* self, int a, int b)  
(defcfun ("cv_RNG_uniform_int" uniform-i) :int
  "Returns the next random number sampled from the uniform distribution."
  (self rng)
  (a :int)
  (b :int))


(defun uniform  (rng a b)
       (cond ((or (eq (type-of (and a b)) 'FIXNUM)
		  (integerp (and a b)))
	      (uniform-i rng a b))
	      ((eq (type-of (and a b)) 'single-float) (uniform-f rng a b))
	      ((eq (type-of (and a b)) 'double-float) (uniform-d rng a b))
	      (t nil)))


;;; Operations on Arrays


;;; Drawing Functions


(defmacro bgr (b g r)
	  "BGR value constructor macro"
	  `(scalar ,b ,g ,r))


(defmacro make-bgr (b g r)
	  "BGR value constructor macro"
	  `(scalar ,b ,g ,r))


(defmacro rgb (r g b)
	  "BGR value constructor macro"
	  `(scalar ,b ,g ,r))


(defmacro make-rgb (r g b)
	  "BGR value constructor macro"
	  `(scalar ,b ,g ,r))


;; void circle(Mat& img, Point center, int radius, const Scalar& color, int thickness=1, int lineType=8, int shift=0)
;; void cv_circle(Mat* img, Point* center, int radius, Scalar* color, int thickness, int lineType, int shift) 
(defcfun ("cv_circle" %circle) :void
  (img mat)
  (center point)
  (radius :int)
  (color scalar)
  (thickness :int)
  (line-type :int)
  (shift :int))


(defun circle (img center radius color &optional (thickness 1) (line-type 8) (shift 0))
       "Draws a circle."
       (%circle img center radius color thickness line-type shift))


;; bool clipLine(Rect imgRect, Point& pt1, Point& pt2)
;; bool cv_clipLine(Rect* imgRect, Point* pt1, Point* pt2)
(defcfun ("cv_clipLine" clip-line) :boolean
  (img-rect rect)
  (pt1 point)
  (pt2 point))


;; void ellipse(Mat& img, const RotatedRect& box, const Scalar& color, int thickness=1, int lineType=8)
;; void cv_ellipse5(Mat* img, RotatedRect* box, Scalar* color, int thickness, int lineType)
(defcfun ("cv_ellipse5" %ellipse5) :void
  (img mat)
  (box rotated-rect)
  (color scalar)
  (thickness :int) 
  (line-type :int))


(defun ellipse5 (img box color &optional (thickness 1) (line-type 8))
       "Fills an ellipse sector."
       (%ellipse5 img box color thickness line-type))


;; void ellipse(Mat& img, Point center, Size axes, double angle, double startAngle, double endAngle, const Scalar& color, 
;; int thickness=1, int lineType=8, int shift=0)
;; void cv_ellipse(Mat* img, Point* center, Size* axes, double angle, double startAngle, double endAngle, Scalar* color, int thickness, 
;; int lineType, int shift)
(defcfun ("cv_ellipse" %ellipse10) :void
  (img mat)
  (center point)
  (axes size)
  (angle :double)
  (start-angle :double)
  (end-angel :double)
  (color scalar)
  (thickness :int) 
  (line-type :int) 
  (shift :int))


(defun ellipse10 (img center axes angle start-angle end-angle color &optional (thickness 1) (line-type 8) (shift 0))
       "Draws a simple or thick elliptic arc."
       (%ellipse10 img center axes angle start-angle end-angle color thickness line-type shift))


(defun ellipse (&rest args)
       (case (length args)
	     ((3 4 5) (apply #'ellipse5 args))
	     ((7 8 9 10) (apply #'ellipse10 args))
	     (otherwise (error "Wrong number arguments to ELLIPSE (~A)" (length args)))))


;; Size getTextSize(const string& text, int fontFace, double fontScale, int thickness, int* baseLine)
;; Size* cv_getTextSize(String* text, int fontFace, double fontScale, int thickness, int* baseLine)
(defcfun ("cv_getTextSize" %get-text-size) size
  (text *string)
  (font-face :int)
  (font-scale :double)
  (thickness :int) 
  (base-line :pointer))


(defun get-text-size (text font-face font-scale thickness base-line)
  "Calculates the width and height of a text string."
  (%get-text-size (%c-string-to-string text (length text)) font-face font-scale thickness base-line))


;; void line(Mat& img, Point pt1, Point pt2, const Scalar& color, int thickness=1, int lineType=8, int shift=0)
;; void cv_line(Mat* img, Point* pt1, Point* pt2, Scalar* color, int thickness, int lineType, int shift) 
(defcfun ("cv_line" %line) :void
  (img mat)
  (pt-1 point)
  (pt-2 point)
  (color scalar)
  (thickness :int) 
  (line-type :int) 
  (shift :int))


(defun line (img pt-1 pt-2 color &optional (thickness 1) (line-type 8) (shift 0))
       "Draws a line segment connecting two points."
       (%line img pt-1 pt-2 color thickness line-type shift))


;; void putText(Mat& img, const string& text, Point org, int fontFace, double fontScale, Scalar color, int thickness=1, int lineType=8, 
;; bool bottomLeftOrigin=false)
;; void cv_putText(Mat* img, String* text, Point* org, int fontFace, double fontScale, Scalar* color, int thickness, int lineType, 
;; bool bottomLeftOrigin)


(defcfun ("cv_putText" %put-text) :void
  (img mat)
  (text *string)
  (org point)
  (font-face :int)
  (font-scale :double)
  (color scalar) 
  (thickness :int) 
  (line-type :int)
  (bottom-left-orign :boolean))


(defun put-text (img text org font-face font-scale color &optional (thickness 1) (line-type 8) (bottom-left-origin nil))
       "Draws a text string."
       (%put-text img (%c-string-to-string text (length text)) org font-face font-scale color thickness line-type bottom-left-origin))


;; void rectangle(Mat& img, Point pt1, Point pt2, const Scalar& color, int thickness=1, int lineType=8, int shift=0)
;; void cv_rectangle(Mat* img, Point* pt1, Point* pt2, Scalar* color, int thickness, int lineType, int shift)
(defcfun ("cv_rectangle" %rectangle) :void
  (img mat)
  (pt1 point)
  (pt2 point)
  (color scalar)
  (thickness :int) 
  (line-type :int) 
  (shift :int))


(defun rectangle (img pt1 pt2 color &optional (thickness 1) (line-type 8) (shift 0))
       "Draws a simple, thick, or filled up-right rectangle."
       (%rectangle img pt1 pt2 color thickness line-type shift))



;;; XML/YAML Persistence


;; FileStorage::FileStorage()
;; FileStorage* cv_create_FileStorage()
(defcfun ("cv_create_FileStorage" file-storage-0) file-storage)


;; FileStorage::FileStorage(const String& source, int flags, const String& encoding=String())
;; FileStorage* cv_create_FileStorage3(String* source, int flags, String* encoding)
(defcfun ("cv_create_FileStorage3" %file-storage-3) file-storage
  (source *string)
  (flags :int)
  (encoding *string))


(defun file-storage-3 (&optional source flags (encoding (%string) given-encoding))
  (let ((return (%file-storage-3 (%c-string-to-string source (length source)) flags 
				 (if given-encoding 
				     (%c-string-to-string encoding (length encoding)) 
				     encoding))))
    (if given-encoding nil (del-std-string encoding))
    return))


(defun file-storage (&rest args)
  (cond ((null args)
	 (file-storage-0))
	(args
	 (apply #'file-storage-3 args))
	(t nil)))


(defun make-file-storage (&rest args)
  (cond ((null args)
	 (file-storage-0))
	(args
	 (apply #'file-storage-3 args))
	(t nil)))


;; bool FileStorage::open(const String& filename, int flags, const String& encoding=String())
;; bool cv_FileStorage_open(FileStorage* self, String* filename, int flags, String* encoding)
(defcfun ("cv_FileStorage_open" %file-storage-open) :boolean
  (self file-storage)
  (filename *string)
  (flags :int)
  (encoding *string))


(defun file-storage-open (self &optional filename flags (encoding (%string) given-encoding))
       (let ((return (%file-storage-open self (%c-string-to-string filename (length filename)) 
					 flags 
					 (if given-encoding 
					     (%c-string-to-string encoding (length encoding)) 
					     encoding))))
	 (if given-encoding nil (del-std-string encoding))
	 return))


;; void FileStorage::release()
;; void cv_FileStorage_release(FileStorage* self)
(defcfun ("cv_FileStorage_release" file-storage-release) :void
  (self file-storage))



;; void write( FileStorage& fs, const String& name, double value )
;; void cv_FileNode_write_number_##(FileStorage* fs, String* name, tn value)
(defcfun ("cv_FileNode_write_number_d" file-storage-write-double) :void
  (fs file-storage)
  (name *string)
  (value :double))


;; void write( FileStorage& fs, const String& name, float value )
;; void cv_FileNode_write_number_##(FileStorage* fs, String* name, tn value)
(defcfun ("cv_FileNode_write_number_f" file-storage-write-float) :void
  (fs file-storage)
  (name *string)
  (value :float))


;; void write( FileStorage& fs, const String& name, int value )
;; void cv_FileNode_write_number_##(FileStorage* fs, String* name, tn value)
(defcfun ("cv_FileNode_write_number_i" file-storage-write-int) :void
  (fs file-storage)
  (name *string)
  (value :int))


;; void write( FileStorage& fs, const String& name, const std::vector<KeyPoint>& value)
;; void cv_FileNode_write_pointer_##(FileStorage* fs, String* name, tn* value)
(defcfun ("cv_FileNode_write_pointer_vkp" file-storage-write-vector-key-point) :void
  (fs file-storage)
  (name *string)
  (value vector-key-point))


;; void write( FileStorage& fs, const String& name, const std::vector<KeyPoint>& value)
;; void cv_FileNode_write_pointer_##(FileStorage* fs, String* name, tn* value)
(defcfun ("cv_FileNode_write_pointer_m" file-storage-write-mat) :void
  (fs file-storage)
  (name *string)
  (value mat))


;; void write( FileStorage& fs, const String& name, const String& value )
;; void cv_FileNode_write_pointer_##(FileStorage* fs, String* name, tn* value)
(defcfun ("cv_FileNode_write_pointer_s" file-storage-write-string) :void
  (fs file-storage)
  (name *string)
  (value *string))



;;; Utility and System Functions and Macros



;; bool checkHardwareSupport(int feature)
;; bool cv_checkHardwareSupport(int feature)
(defcfun ("cv_checkHardwareSupport" check-hardware-support) :boolean
  "Returns true if the specified feature is supported by the host hardware."
  (feature :int))


;; float cubeRoot(float val)
;; float cv_cubeRoot(float val)
(defcfun ("cv_cubeRoot" cube-root) :float
  "Computes the cube root of an argument."
  (value :float))


;; float fastAtan2(float y, float x)
;; float cv_fastAtan2(float y, float x)
(defcfun ("cv_fastAtan2" fast-atan2) :float 
	 "Calculates the angle of a 2D vector in degrees."
	 (x :float)
	 (y :float))


;; int getNumberOfCPUs()
;; cv_getNumberOfCPUs()
(defcfun ("cv_getNumberOfCPUs" get-number-of-cpu-s) :int)


;; int64 getTickCount()
;; int64 cv_getTickCount()
(defcfun ("cv_getTickCount" get-tick-count)  :int64
  "Returns the number of ticks.")


;; double getTickFrequency()
;; double cv_getTickFrequency()
(defcfun ("cv_getTickFrequency" get-tick-frequency)  :double
  "Returns the number of ticks per second.")



