;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; core.lisp
;;;; OpenCV bindings
;;;; Core functionality
(in-package :lisp-cv)




;; Default parameters

(defvar *camera-index* 0)
(defvar *default-width* 640)
(defvar *default-height* 480)
(defvar *frames-per-second* 30)
(defvar *millis-per-frame* (round (/ 1000 *frames-per-second*)))



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
     (continue () :report "Continue"  )))

(defun update-swank ()
  "Grabs SWANK connections and tells it to handle requests. 
   Call this every loop in the main loop of your program"
  (continuable
    (let ((connection (or swank::*emacs-connection*
			  (swank::default-connection))))
      (when connection
	(swank::handle-requests connection t)))))


;; Interop

;; template < class T, class Alloc = allocator<T> > class vector
;; vector_char* std_create_vectorc() 
(defcfun ("std_create_vectorc" vector-char) (:pointer vector-char))

;; template < class T, class Alloc = allocator<T> > class vector
;; vector_KeyPoint* std_create_vectork() 
(defcfun ("std_create_vectork" vector-keypoint) (:pointer vector-keypoint))

;; template < class T, class Alloc = allocator<T> > class vector
;; vector_DMatch* std_create_vectordm() 
(defcfun ("std_create_vectordm" vector-dmatch) (:pointer vector-dmatch))



;;; Basic Structures


;; MatExpr* promote(Mat* m) 
(defcfun ("promote" <<) (:pointer mat-expr)
  "Converts a (:POINTER MAT) to a (:POINTER MAT-EXPR).
   This is a shorthand version of the PROMOTE function." 
  (m (:pointer mat)))

;; Mat* force(MatExpr* expr)
(defcfun ("force" >>) (:pointer mat)
  "Coerces a (:POINTER MAT-EXPR) to a (:POINTER MAT). 
   This is a shorthand version of the FORCE function."
  (expr (:pointer mat-expr)))

;; MatExpr + operator
;; MatExpr* cv_Mat_add(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_add" add) (:pointer mat-expr)
  (m1 (:pointer mat))
  (m2 (:pointer mat)))

;; char cv_Mat_at_char(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_char1" at-char) :pointer
  "Returns a reference to a CHAR array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; int cv_Mat_at_double(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_double0" at-double) :double
  "Returns a reference to a DOUBLE array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; float cv_Mat_at_float(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_float0" at-float) :float
  "Returns a reference to a FLOAT array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; int cv_Mat_at_int(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_int0" at-int) :int
  "Returns a reference to a INT array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; short cv_Mat_at_short(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_short1" at-short) :pointer
  "Returns a reference to a SHORT array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; uint cv_Mat_at_uint(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_uint1" at-uint) :pointer
  "Returns a reference to a UINT array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; uchar cv_Mat_at_uchar(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_uchar1" at-uchar) :pointer
  "Returns a reference to a UCHAR array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; ushort cv_Mat_at_ushort(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_ushort1" at-ushort) :pointer
  "Returns a reference to a USHORT array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Scalar* cv_Mat_at_Scalar(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Scalar" at-scalar) (:pointer scalar)
  "Returns a reference to a SCALAR array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Point* cv_Mat_at_Point(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Point" at-point) (:pointer point)
  "Returns a reference to a POINT array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Point2d* cv_Mat_at_Point2d(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Point2d" at-point2d) (:pointer point2d)
  "Returns a reference to a POINT2D array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Point2f* cv_Mat_at_Point2f(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Point2f" at-point2f) (:pointer point2f)
  "Returns a reference to a POINT2F array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Point3d* cv_Mat_at_Point3d(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Point3d" at-point3d) (:pointer point3d)
  "Returns a reference to a POINT3D array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Point3f* cv_Mat_at_Point3f(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Point3f" at-point3f) (:pointer point3f)
  "Returns a reference to a POINT3F array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Point3i* cv_Mat_at_Point3i(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Point3i" at-point3i) (:pointer point3i)
  "Returns a reference to a POINT3I array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Vec2b* cv_Mat_at_Vec2b(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Vec2b" at-vec2b) (:pointer vec2b)
  "Returns a reference to a VEC2B array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Vec2d cv_Mat_at_Vec2d(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Vec2d" at-vec2d) (:pointer vec2d)
  "Returns a reference to a VEC2D array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Vec2f* cv_Mat_at_Vec2f(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Vec2f" at-vec2f) (:pointer vec2f)
  "Returns a reference to a VEC2F array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Vec2i* cv_Mat_at_Vec2i(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Vec2i" at-vec2i) (:pointer vec2i)
  "Returns a reference to a VEC2I array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Vec2s* cv_Mat_at_Vec2s(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Vec2s" at-vec2s) (:pointer vec2s)
  "Returns a reference to a VEC2S array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Vec2w* cv_Mat_at_Vec2w(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Vec2w" at-vec2w) (:pointer vec2s)
  "Returns a reference to a VEC2S array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Vec3b* cv_Mat_at_Vec3b(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Vec3b0" at-vec3b) (:pointer vec3b)
  "Returns a reference to a VEC3B array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Vec3d* cv_Mat_at_Vec3d(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Vec3d" at-vec3d) (:pointer vec3d)
  "Returns a reference to a VEC3D array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Vec3f* cv_Mat_at_Vec3f(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Vec3f" at-vec3f) (:pointer vec3f)
  "Returns a reference to a VEC3F array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Vec3i* cv_Mat_at_Vec3i(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Vec3i" at-vec3i) (:pointer vec3i)
  "Returns a reference to a VEC3I array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Vec3s* cv_Mat_at_Vec3s(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Vec3s" at-vec3s) (:pointer vec3s)
  "Returns a reference to a VEC3S array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Vec4b* cv_Mat_at_Vec4b(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Vec4b" at-vec4b) (:pointer vec4b)
  "Returns a reference to a VEC4B array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Vec4d* cv_Mat_at_Vec4d(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Vec4d" at-vec4d) (:pointer vec4d)
  "Returns a reference to a VEC4D array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Vec4f* cv_Mat_at_Vec4f(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Vec4f" at-vec4f) (:pointer vec4f)
  "Returns a reference to a VEC4F array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Vec4i cv_Mat_at_Vec4i(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Vec4i" at-vec4i) (:pointer vec4i)
  "Returns a reference to a VEC4I array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Point_<_Tp> br() const;
;; Point* cv_Rect_br(Rect* self) {
(defcfun ("cv_Rect_br" br) (:pointer point) 
  "Retrievies the bottom-right corner of a rectangle."
  (self (:pointer rect)))

;; Vec4s cv_Mat_at_Vec4s(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Vec4s" at-vec4s) (:pointer vec4s)
  "Returns a reference to a VEC4S array element."
  (self (:pointer mat))
  (i :int)
  (j :int))

;; Mat Mat::clone() const
;; Mat* cv_Mat_clone(Mat* self) 
(defcfun ("cv_Mat_clone" clone) (:pointer mat)
  "Creates a full copy of the array and the underlying data."
  (self (:pointer mat)))

;; int cv_Mat_cols(Mat* self)
(defcfun ("cv_Mat_cols" cols) :int
  (self (:pointer mat)))

;; void Mat::convertTo(OutputArray m, int rtype, double alpha=1, double beta=0 ) const
;; void cv_Mat_convertTo(Mat* self,Mat* m, int rtype, double alpha, double beta)
(defcfun ("cv_Mat_convertTo" %convert-to) :int
  (self (:pointer mat))
  (m (:pointer mat))
  (rtype :int)
  (alpha :double)
  (beta :double))

(defun convert-to (self m rtype &optional (alpha 1.0d0) (beta 0.0d0))
  "Converts an array to another data type with optional scaling."
  (%convert-to self m rtype alpha beta))

;; void operator delete  ( void* ptr )
;; void cv_delete_Mat(void* ptr)
(defcfun ("cv_delete_Mat" del-mat) :void
  (ptr :pointer))

;; Mat Mat::diag(int d=0 ) const
;; Mat* cv_Mat_diag_d(Mat* self, int d)
(defcfun ("cv_Mat_diag_d" %diag) (:pointer mat)
  "Extracts a diagonal from a matrix."
  (self (:pointer mat))
  (d :int))

(defun diag (self &optional (d 0))
  "Extracts a diagonal from a matrix."
   (%diag self d))

;; MatExpr / operator
;; MatExpr* cv_Mat_div(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_div" div) (:pointer mat-expr)
  (m1 (:pointer mat))
  (m2 (:pointer mat)))

;; _Tp dot(const Point_& pt) const;
;; int cv_Point_dot(Point* self, Point* other) 
(defcfun ("cv_Point_dot" dot) :int 
  "Finds the dot product of a point."
  (self (:pointer point))
  (other (:pointer point)))

;; _Tp dot(const Point_& pt) const;
;; double cv_Point2d_dot(Point2d* self, Point2d* other) 
(defcfun ("cv_Point2d_dot" dot2d) :double
  "Finds the dot product of a point2d."
  (self (:pointer point2d))
  (other (:pointer point2d)))

;; _Tp dot(const Point_& pt) const;
;; float cv_Point2f_dot(Point2f* self, Point2f* other)
(defcfun ("cv_Point2f_dot" dot2f) :float
  "Finds the dot product of a point2f."
  (self (:pointer point2f))
  (other (:pointer point2f)))

;; _Tp dot(const Point3_& pt) const;
;; double cv_Point3d_dot(Point3d* self, Point3d* other)
(defcfun ("cv_Point3d_dot" dot3d) :double 
  "Finds the dot product of a point3d."
  (self (:pointer point3d))
  (other (:pointer point3d)))

;; _Tp dot(const Point3_& pt) const;
;; float cv_Point3f_dot(Point3f* self, Point3f* other)
(defcfun ("cv_Point3f_dot" dot3f) :float 
  "Finds the dot product of a point3f."
  (self (:pointer point3f))
  (other (:pointer point3f)))

;; _Tp dot(const Point3_& pt) const;
;; int cv_Point3i_dot(Point3i* self, Point3i* other)
(defcfun ("cv_Point3i_dot" dot3i) :int 
  "Finds the dot product of a point3i."
  (self (:pointer point3i))
  (other (:pointer point3i)))

;; Mat* force(MatExpr* expr)
(defcfun ("force" force) (:pointer mat)
  "Coerces a (:POINTER MAT-EXPR) to a (:POINTER MAT)."
  (expr (:pointer mat-expr)))

;; Mat::Mat()
;; Mat* cv_create_Mat()
(defcfun ("cv_create_Mat" mat) (:pointer mat)
  "MAT constructor")

;; Mat::Mat(int rows, int cols, int type, void* data) 
;; Mat* cv_create_Mat_with_data(int rows, int cols, int type, void* data) ;todo...no step param
(defcfun ("cv_create_Mat_with_data" mat-data) (:pointer mat)
  (rows :int)
  (cols :int)
  (type :int)
  (data :pointer))

;; Mat::t
;; MatExpr* cv_Mat_transpose_mat(Mat* self) 
(defcfun ("cv_Mat_transpose_mat" mat-expr-t)  (:pointer mat-expr)
  "Transposes a matrix."
  (self (:pointer mat)))

;; static MatExpr Mat::eye(int rows, int cols, int type) 
;; Mat* cv_create_identity(int rows, int cols, int type)
(defcfun ("cv_create_identity" mat-eye-rc) (:pointer mat)
  "Returns an identity matrix of the specified size and type."
  (rows :int)
  (cols :int)
  (type :int))

;; static MatExpr Mat::eye(Size size, int type)
;; Mat* cv_create_sized_identity(Size* s, int type)
(defcfun ("cv_create_sized_identity" mat-eye-s)  (:pointer mat)
  "Returns an identity matrix of the specified size and type."
  (s (:pointer size))
  (type :int))

;; static MatExpr Mat::ones(int rows, int cols, int type)
;; Mat* cv_create_ones(int rows, int cols, int type)
(defcfun ("cv_create_ones" mat-ones) (:pointer mat)
  (rows :int)
  (cols :int)
  (type :int))

;; MatExpr * operator
;; MatExpr* cv_Mat_scale(MatExpr* m, double alpha)
(defcfun ("cv_Mat_scale" scale) (:pointer mat-expr)
  (m (:pointer mat-expr))
  (alpha :double))

;; int Mat::type() const 
;; int cv_Mat_type(Mat* self) 
(defcfun ("cv_Mat_type" mat-type) :int
  (self (:pointer mat)))

;; Mat::Mat(int rows, int cols, int type)
;; Mat* cv_create_Mat_typed(int rows, int cols, int type)
(defcfun ("cv_create_Mat_typed" mat-typed-0)  (:pointer mat)
  "MAT constructor with a row, column and type parameter."
  (rows :int)
  (cols :int)
  (type :int))

;; Mat::Mat(int rows, int cols, int type, const Scalar& s)
;; Mat* cv_create_Mat_with_value(int rows, int cols, int type, Scalar* s)
(defcfun ("cv_create_Mat_with_value" mat-value) (:pointer mat)
  (rows :int)
  (cols :int)
  (type :int)
  (s (:pointer scalar)))

;; static MatExpr Mat::zeros(int rows, int cols, int type)
;; Mat* cv_create_zeros(int rows, int cols, int type)
(defcfun ("cv_create_zeros" mat-zeros) (:pointer mat)
  (rows :int)
  (cols :int)
  (type :int))

;; MatExpr * operator
;; MatExpr* cv_Mat_mult(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_mult" mul) (:pointer mat-expr)
  (m1 (:pointer mat))
  (m2 (:pointer mat)))

;; Point* cv_create_Point(int x, int y) 
(defcfun ("cv_create_Point" point-init) (:pointer point)
  "Point constructor")

;; Point* cv_create_Point(int x, int y) 
(defcfun ("cv_create_Point2" point) (:pointer point)
  "Point constructor"
  (x :int)
  (y :int))

;; _Tp x, y
;; int cv_Point_getX(Point* self) 
(defcfun ("cv_Point_getX" point-x) :int 
  "Retrieves X coordinate of a point"
  (self (:pointer point)))

;; _Tp x, y
;; int cv_Point_getY(Point* self)
(defcfun ("cv_Point_getY" point-y) :int 
  "Retrieves y coordinate of a point"
  (self (:pointer point)))

;; typedef Point_<double> Point2d
;; Point2d* cv_create_Point2d(double x, double y) {
(defcfun ("cv_create_Point2d" point2d) (:pointer point2d) 
  "Point2d constructor"
  (x :double)
  (y :double))


;; _Tp x, y;
;; double cv_Point2d_getX(Point2d* self) 
(defcfun ("cv_Point2d_getX" point2d-x) :double
  "Retrieves x coordinate of a point2d"
  (self (:pointer point2d)))

;; _Tp x, y;
;; double cv_Point2d_getY(Point2d* self) 
(defcfun ("cv_Point2d_getY" point2d-y) :double
  "Retrieves y coordinate of a point2d"
  (self (:pointer point2d)))


;; typedef Point_<float> Point2f
;; Point2f* cv_create_Point2f(float x, float y) {
(defcfun ("cv_create_Point2f" point2f) (:pointer point2f) 
  "Point2f constructor"
  (x :float)
  (y :float))

;; _Tp x, y;
;; float cv_Point2f_getX(Point2f* self) 
(defcfun ("cv_Point2f_getX" point2f-x) :float
  "Retrieves x coordinate of a point2f"
  (self (:pointer point2f)))

;; _Tp x, y;
;; float cv_Point2f_getY(Point2f* self) 
(defcfun ("cv_Point2f_getY" point2f-y) :float
  "Retrieves y coordinate of a point2f"
  (self (:pointer point2f)))

;; typedef Point3_<double> Point3d
;; Point3d* cv_create_Point3d(double x, double y, double z) 
(defcfun ("cv_create_Point3d" point3d) (:pointer point3d) 
  "Point3d constructor"
  (x :double)
  (y :double)
  (z :double))

;; _Tp x, y, z
;; double cv_Point3d_getX(Point3d* self) 
(defcfun ("cv_Point3d_getX" point3d-x) :double
  "Retrieves x coordinate of a point3d"
  (self (:pointer point3d)))

;; _Tp x, y, z
;; double cv_Point3d_getY(Point3d* self) 
(defcfun ("cv_Point3d_getY" point3d-y) :double
  "Retrieves y coordinate of a point3d"
  (self (:pointer point3d)))

;; _Tp x, y, z
;; double cv_Point3d_getZ(Point3d* self) 
(defcfun ("cv_Point3d_getZ" point3d-z) :double
  "Retrieves z coordinate of a point3d"
  (self (:pointer point3d)))
   
;; Point3f* cv_create_Point3f(float x, float y, float z) {
(defcfun ("cv_create_Point3f" point3f) (:pointer point3f) 
  "Point3f constructor"
  (x :float)
  (y :float)
  (z :float))

;; _Tp x, y, z
;; float cv_Point3f_getX(Point3f* self) 
(defcfun ("cv_Point3f_getX" point3f-x) :float
  "Retrieves x coordinate of a point3f"
  (self (:pointer point3f)))

;; _Tp x, y, z
;; float cv_Point3f_getY(Point3f* self) 
(defcfun ("cv_Point3f_getY" point3f-y) :float
  "Retrieves y coordinate of a point3f"
  (self (:pointer point3f)))

;; _Tp x, y, z
;; float cv_Point3f_getZ(Point3f* self) 
(defcfun ("cv_Point3f_getZ" point3f-z) :float
  "Retrieves z coordinate of a point3f"
  (self (:pointer point3f)))
   

;; Point3i* cv_create_Point3i(int x, int y, int z) 
(defcfun ("cv_create_Point3i" point3i) (:pointer point3i) 
  "Poin3i constructor"
  (x :int)
  (y :int)
  (z :int))

;; _Tp x, y, z
;; int cv_Point3i_getX(Point3i* self) 
(defcfun ("cv_Point3i_getX" point3i-x) :int 
  "Retrieves y coordinate of a point3i"
  (self (:pointer point3i)))

;; _Tp x, y, z
;; int cv_Point3i_getY(Point3i* self) 
(defcfun ("cv_Point3i_getY" point3i-y) :int
  "Retrieves y coordinate of a point3i"
  (self (:pointer point3i)))

;; _Tp x, y, z
;; int cv_Point3i_getZ(Point3i* self) 
(defcfun ("cv_Point3i_getZ" point3i-z) :int
  "Retrieves z coordinate of a point3i"
  (self (:pointer point3i)))

;; MatExpr* promote(Mat* m) 
(defcfun ("promote" promote) (:pointer mat-expr)
  "Converts a (:POINTER MAT) to a (:POINTER MAT-EXPR)."
  (m (:pointer mat)))

;; Rect_(_Tp _x, _Tp _y, _Tp _width, _Tp _height)
;; Rect* cv_create_Rect4(int x, int y, int width, int height) 
(defcfun ("cv_create_Rect4" rect) (:pointer rect) 
  "RECT constructor."
  (x :int)
  (y :int)
  (width :int)
  (height :int))

;; Rect_();
;; Rect* cv_create_Rect() 
(defcfun ("cv_create_Rect" rect-init) (:pointer mat) 
  "RECT constructor.")

;; Mat::Mat(const Mat& m, const Rect& roi)
;; Mat* cv_Mat_get_ROI(Mat* self, Rect* roi)
(defcfun ("cv_Mat_get_ROI" roi) (:pointer mat) 
  "Returns matrix header corresponding to the rectangular sub-array of input MAT."
  (self (:pointer mat))
  (roi (:pointer rect)))

;; int cv_Mat_rows(Mat* self) 
(defcfun ("cv_Mat_rows" rows) :int
  (self (:pointer mat)))

;; Scalar* cv_create_Scalar(double val0, (double val1, double val2, double val3)
(defcfun ("cv_create_Scalar" %scalar) (:pointer scalar)
  (val0 :double)
  (val1 :double)
  (val2 :double)
  (val3 :double))

(defun scalar (val0 &optional (val1 0) (val2 0) (val3 0))
  "SCALAR constructor"
   (%scalar (coerce val0 'double-float) (coerce val1 'double-float) (coerce val2 'double-float) (coerce val3 'double-float)))

;; Scalar* cv_create_scalarAll(double val0123)
(defcfun ("cv_create_scalarAll" %scalar-all) (:pointer scalar)
  (val0123 :double))

(defun scalar-all (val0123)
  "SCALAR conctctor - initializes all of the scalar values 0...3 with val0123"
   (%scalar-all (coerce val0123 'double-float)))

;; cv_create_Size2(int width, int height)
(defcfun ("cv_create_Size2" size) (:pointer size)
  "SIZE constructor"
  (width :int)
  (height :int))
 
;; int cv_Size_height(Size* self) 
(defcfun ("cv_Size_height" size-height) :int
  "Gets the height of a (:POINTER SIZE)"
  (self (:pointer size)))

;; Size Mat::size() const
;; Size* cv_Mat_size(Mat* self)
(defcfun ("cv_Mat_size" size-mat) (:pointer size)
  (self (:pointer mat)))

;; int cv_Size_width(Size* self) 
(defcfun ("cv_Size_width" size-width) :int
  "Gets the width of a (:POINTER SIZE)"
  (self (:pointer size)))

;; MatExpr - operator
;; MatExpr* cv_Mat_sub(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_sub" sub) (:pointer mat-expr)
  (m1 (:pointer mat))
  (m2 (:pointer mat)))

;; Size_<_Tp> size() const;
;; Size* size(Rect* self) 
(defcfun ("size" sz) (:pointer rect) 
  "Size (width, height) of the rectangle."
  (self (:pointer rect)))

;; Point_<_Tp> tl() const;
;; Point* cv_Rect_tl(Rect* self) {
(defcfun ("cv_Rect_tl" tl) (:pointer point) 
  "Retrievies the top-left corner of a rectangle."
  (self (:pointer rect)))

;; size_t Mat::total() const
;; size_t cv_Mat_total(Mat* self)
(defcfun ("cv_Mat_total" total) :unsigned-int
  "Returns the total number of array elements."
  (self (:pointer mat)))



;;; Operations on Arrays

;; C++: void absdiff(InputArray src1, InputArray src2, OutputArray dst)
;; void cv_absdiff(Mat* src1, Mat* src2, Mat* dst) 
(defcfun ("cv_absdiff" absdiff) :void
  "Calculates the per-element absolute difference between two arrays or between an array and a scalar."
  (src1 (:pointer mat))
  (src2 (:pointer mat))
  (dest (:pointer mat)))
 
;; void inRange(InputArray src, InputArray lowerb, InputArray upperb, OutputArray dst)
;; void cv_inRangeS(Mat* src, Scalar* lowerb, Scalar* upperb, Mat* dst)
(defcfun ("cv_inRangeS" in-range-s) :void
  "Checks if array elements lie between the elements of two other arrays."
  (src (:pointer mat))
  (lowerb (:pointer scalar))
  (upperb (:pointer scalar))
  (dst :pointer mat))


;;; Drawing Functions


;; void circle(Mat& img, Point center, int radius, const Scalar& color, int thickness=1, int lineType=8, int shift=0)
;; void cv_circle(Mat* img, Point* center, int radius, Scalar* color, int thickness, int lineType, int shift) 
(defcfun ("cv_circle" %circle) :void
  (img (:pointer mat))
  (center (:pointer point))
  (radius :int)
  (color (:pointer scalar))
  (thickness :int)
  (line-type :int)
  (shift :int))


(defun circle (img center radius color &optional (thickness 1) (line-type 8) (shift 0))
  "Draws a circle."
  (%circle img center radius color thickness line-type shift))

;;; Utility and System Functions and Macros

;; int64 getTickCount()
;; int64 cv_getTickCount()
(defcfun ("cv_getTickCount" get-tick-count)  :int64
  "Returns the number of ticks.")


;; double getTickFrequency()
;; double cv_getTickFrequency()
(defcfun ("cv_getTickFrequency" get-tick-frequency)  :double
  "Returns the number of ticks per second.")
