;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; core.lisp
;;;; OpenCV bindings
;;;; The Core Functionality

(in-package :lisp-cv)

; Point* cv_Mat_at_Point(Mat* self, int row, int col);
(defcfun ("cv_Mat_at_Point0" at-point) (:pointer point)
  "Returns a reference to a POINT array element."
  (self (:pointer mat))
  (i :int)
  (j :int)) 

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



;; Interop - String*

;; string* std_cstringToString(char* s, size_t len) 
(defcfun ("std_cstringToString" c-string-to-string*) (:pointer string*) 
  "test"
  (s :string)
  (len :unsigned-int))



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


;; _Tp area() const
;; int cv_Size_area(Size* self)
(defcfun ("cv_Size_area" area) :int
  "Gets the area of a SIZE construct"
  (self (:pointer size)))


;; _Tp area() const
;; float cv_Size2f_area(Size2f* self) 
(defcfun ("cv_Size2f_area" area2f) :float
  "Gets the area of a SIZE2F construct"
  (self (:pointer size2f)))


 ;; Mat* cv_Mat_assignVal(Mat* self, Scalar* s)
(defcfun ("cv_Mat_assignVal" assgn-val) (:pointer mat)
  "Assign a scalar value to a matrix."
  (self (:pointer mat))
  (s (:pointer scalar)))

;; uchar* ptr(int i0=0)
;; uchar* cv_Mat_ptr_index(Mat* self, int i)
(defmacro at (&optional self i j type)
	       `(mem-aref (%ptr ,self ,i) ,type ,j))


;; Point_<_Tp> br() const;
;; Point* cv_Rect_br(Rect* self) {
(defcfun ("cv_Rect_br" br) (:pointer point) 
  "Retrievies the bottom-right corner of a rectangle."
  (self (:pointer rect)))


;; int cv_Mat_channels(Mat* self)
(defcfun ("cv_Mat_channels" channels) :int
  (self (:pointer mat)))


;; void Mat::copyTo(OutputArray m) const
;; void cv_Mat_copyTo(Mat* self, Mat* m)
(defcfun ("cv_Mat_copyTo" copy-to2) :void
  (self (:pointer mat))
  (m (:pointer mat)))


;; void Mat::copyTo(OutputArray m, InputArray mask) const
;; void cv_Mat_copyTo_masked(Mat* self, Mat* m, Mat* mask)
(defcfun ("cv_Mat_copyTo_masked" copy-to3) :void
  (self (:pointer mat))
  (m (:pointer mat))
  (mask (:pointer mat)))


(defun copy-to (&optional (mat nil) (m nil) (mask nil))
  (cond ((eq mask nil)
	 (copy-to2 mat m))
	(t (copy-to3 mat m mask))))


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


;; uchar* data
;; uchar* cv_Mat_get_Data(Mat* self)
(defcfun ("cv_Mat_get_Data" data) :pointer
  "Pointer to the data."
  (self (:pointer mat)))


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
(defcfun ("cv_Point_dot2i" dot) :int 
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


;; bool Mat::isContinuous() const
;; bool cv_Mat_isContinuous(Mat* self) 
(defcfun ("cv_Mat_isContinuous" is-continuous) :boolean
  (self (:pointer mat)))


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


;; bool Mat::empty() const
;; int cv_Mat_empty(Mat* self) 
(defcfun ("cv_Mat_empty" empty) :boolean
  "Returns true if the array has no elements."
  (self (:pointer mat)))


;; Mat* force(MatExpr* expr)
(defcfun ("force" force) (:pointer mat)
  "Coerces a (:POINTER MAT-EXPR) to a (:POINTER MAT)."
  (expr (:pointer mat-expr)))


;; _Tp width, height
;; double cv_Size_height(Size* self) 
(defcfun ("cv_Size_height" height) :double
  "Gets the height of a SIZE construct"
  (self (:pointer size)))


;; _Tp width, height
;; float cv_Size2f_height(Size* self) 
(defcfun ("cv_Size2f_height" height2f) :float
  "Gets the height of a (:POINTER SIZE2F)"
  (self (:pointer size2f)))


;; Mat::Mat()
;; Mat* cv_create_Mat()
(defcfun ("cv_create_Mat" mat) (:pointer mat)
  "MAT constructor")


;; Mat::Mat(int rows, int cols, int type, void* data) 
;; Mat* cv_create_Mat_with_data(int rows, int cols, int type, void* data)
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
(defcfun ("cv_create_identity" mat-eye3) (:pointer mat)
  "Returns an identity matrix of the specified size and type."
  (rows :int)
  (cols :int)
  (type :int))


;; static MatExpr Mat::eye(Size size, int type)
;; Mat* cv_create_sized_identity(Size* s, int type)
(defcfun ("cv_create_sized_identity" mat-eye2)  (:pointer mat)
  "Returns an identity matrix of the specified size and type."
  (s (:pointer size))
  (type :int))


(defun mat-eye (&rest args)
       (cond ((third args)
	      (mat-eye3 (first args) (second args) (third args)))
	      (t
	       (mat-eye2 (first args) (second args)))))


;; static MatExpr Mat::ones(int rows, int cols, int type)
;; Mat* cv_create_ones(int rows, int cols, int type)
(defcfun ("cv_create_ones" mat-ones) (:pointer mat)
  (rows :int)
  (cols :int)
  (type :int))


;; Size Mat::size() const
;; Size* cv_Mat_size(Mat* self)
(defcfun ("cv_Mat_size" mat-size) (:pointer size)
  (self (:pointer mat)))


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
(defcfun ("cv_create_Mat_typed" mat-typed)  (:pointer mat)
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

;; Point_();
;; Point2##t * cv_create_Point2##t ( tn x,  tn y)
(defcfun ("cv_create_Point2i" point0) (:pointer point)
  "Point constructor")

;; Point_(_Tp _x, _Tp _y);
;; Point2##t * cv_create_Point2##t ( tn x,  tn y) 
(defcfun ("cv_create_Point2i" point2) (:pointer point)
  "Point constructor"
  (x :int)
  (y :int))


(defun point (&optional x y)
	   (cond ((eq (or x y) nil)
		  (point0))
		 ((and x y)
		  (point2 x y))
		 (t nil)))


;; _Tp x, y
;; int cv_Point_getX(Point* self) 
(defcfun ("cv_Point2i_getX" point-x) :int 
  "Retrieves X coordinate of a point"
  (self (:pointer point)))


;; _Tp x, y
;; int cv_Point_getY(Point* self)
(defcfun ("cv_Point2i_getY" point-y) :int 
  "Retrieves y coordinate of a point"
  (self (:pointer point)))


;; typedef Point_<double> Point2d
;; Point2##t * cv_create_Point2##t ( tn x,  tn y) 
(defcfun ("cv_create_Point2d" point2d0) (:pointer point2d) 
  "Point2d constructor")


;; typedef Point_<double> Point2d
;; Point2##t * cv_create_Point2##t ( tn x,  tn y)  
(defcfun ("cv_create_Point2d" point2d2) (:pointer point2d) 
  "Point2d constructor"
  (x :double)
  (y :double))


(defun point2d (&optional x y)
	   (cond ((eq (or x y) nil)
		  (point2d0))
		 ((and x y)
		  (point2d2 x y))
		 (t nil)))


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


;; typedef Point_<float> Point2f;
;; tn cv_Point2##t##_getX( Point2##t * self) {
(defcfun ("cv_create_Point2f" point2f0) (:pointer point2f) 
  "Point2f constructor")



;; typedef Point_<float> Point2f
;; Point2##t * cv_create_Point2##t ( tn x,  tn y)  {
(defcfun ("cv_create_Point2f" point2f2) (:pointer point2f) 
  "Point2f constructor"
  (x :float)
  (y :float))


(defun point2f (&optional x y)
	   (cond ((eq (or x y) nil)
		  (point2f0))
		 ((and x y)
		  (point2f2 x y))
		 (t nil)))


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
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z) 
(defcfun ("cv_create_Point3d" point3d0) (:pointer point3d) 
  "Point3d constructotr")


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z) 
(defcfun ("cv_create_Point3d" point3d2) (:pointer point3d) 
  "Point3d constructor"
  (x :double)
  (y :double)
  (z :double))


(defun point3d (&optional x y z)
	   (cond ((eq (or x y) nil)
		  (point3d0))
		 ((and x y)
		  (point3d2 x y z))
		 (t nil)))


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


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3f" point3f0) (:pointer point3f) 
  "Point3f constructor")


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3f" point3f2) (:pointer point3f) 
  "Point3f constructor"
  (x :float)
  (y :float)
  (z :float))


(defun point3f (&optional x y z)
	   (cond ((eq (or x y) nil)
		  (point3f0))
		 ((and x y)
		  (point3f2 x y z))
		 (t nil)))


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


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3i" point3i0) (:pointer point3i) 
  "Poin3i constructor")


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3i" point3i2) (:pointer point3i) 
  "Poin3i constructor"
  (x :int)
  (y :int)
  (z :int))


(defun point3i (&optional x y z)
	   (cond ((eq (or x y) nil)
		  (point3i0))
		 ((and x y)
		  (point3i2 x y z))
		 (t nil)))


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

;; uchar* Mat::ptr(int i0=0)
;; uchar* cv_Mat_ptr_index(Mat* self, int i)
(defcfun ("cv_Mat_ptr_index" %ptr) :pointer 
	 (self (:pointer mat))
	 (i0 :int))

(defun ptr (self &optional (i0 0))
       "Returns pointer to i0-th submatrix along the dimension #0"
       (ptr-index self i0))

;; Rect_();
;; Rect* cv_create_Rect() 
(defcfun ("cv_create_Rect" rect0) (:pointer mat) 
  "RECT constructor.")


;; Rect_(_Tp _x, _Tp _y, _Tp _width, _Tp _height)
;; Rect* cv_create_Rect4(int x, int y, int width, int height) 
(defcfun ("cv_create_Rect4" rect4) (:pointer rect) 
  "RECT constructor."
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defun rect (&optional (x nil) (y nil) (width nil) (height nil))
	   (cond ((eq (or x y) nil)
		  (rect0))
		 ((and x y)
		  (rect4 x y width height))
		 (t nil)))

;; Mat Mat::reshape(int cn, int rows=0) const
;; Mat* cv_Mat_reshape(Mat* self, int cn) 
(defcfun ("cv_Mat_reshape" %reshape) (:pointer mat)
  (self (:pointer mat))
  (cn :int))

;; Mat Mat::reshape(int cn, int rows=0) const
;; Mat* cv_Mat_reshape_rows(Mat* self, int cn, int rows) 
(defcfun ("cv_Mat_reshape_rows" reshape-rows) (:pointer mat)
  (self (:pointer mat))
  (cn :int)
  (roms :int))


(defun reshape (&optional self cn (rows 0))
	   (cond ((eq rows 0)
		  (%reshape self cn))
		 ((> rows 0)
		  (reshape-rows self cn rows))
		 (t nil)))


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


;; Size_()
;; Size* cv_create_Size() {
(defcfun ("cv_create_Size" size0) (:pointer size)
  "Create SIZE construct")


;; Size_(_Tp _width, _Tp _height)
;; cv_create_Size2(double width, double height)
(defcfun ("cv_create_Size2" size2) (:pointer size)
  "SIZE constructor"
  (width :double)
  (height :double))


(defun size (&optional arg1 arg2)
  (cond ((eq (or arg1 arg2) nil)
	 (size0))
	((numberp (or arg1 arg2)) 
	 (size2 (coerce arg1 'double-float) 
		(coerce arg2 'double-float)))
	((pointerp arg1) (mat-size arg1))
	(t nil)))


;; Size_<float>();
;; Size2f* cv_create_Size2f(float width, float height)
(defcfun ("cv_create_Size2f" size2f0) (:pointer size2f)
  "Size2f constructor")


;; Size_<float>(float width, float height);
;; Size2f* cv_create_Size2f(float width, float height)
(defcfun ("cv_create_Size2f" size2f2) (:pointer size2f)
  "Size2f constructor"
	 (width :float)
	 (height :float))


(defun size2f (&optional (width nil) (height nil))
	   (cond ((eq (or width height) nil)
		  (size2f0))
		 ((and width height)
		  (size2f2 width height))
		 (t nil)))


;; size_t cv_Mat_get_Step(Mat* self) 
(defcfun ("cv_Mat_get_Step" step*) :unsigned-int
  "Used to compute address of a matrix element"
  (self (:pointer mat)))


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

;; _Tp width, height;
;; double cv_Size_width(Size* self) 
(defcfun ("cv_Size_width" width) :double
  "Gets the width of a SIZE construct"
  (self (:pointer size)))


;; _Tp width, height
;; float cv_Size2f_width(Size* self) 
(defcfun ("cv_Size2f_width" width2f) :float
  "Gets the width of a (:POINTER SIZE2F)"
  (self (:pointer size2f)))


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


;; Scalar mean(InputArray src, InputArray mask=noArray())
;; Scalar* cv_mean(Mat* src, Mat* mask)
(defcfun ("cv_mean" %mean) (:pointer scalar)
  "Calculates an average (mean) of array elements."
  (src (:pointer mat))
  (mask (:pointer mat)))

(defun mean (src &optional (mask (mat)))
  "Calculates an average (mean) of array elements."
  (%mean src mask))


;; RNG::RNG()
(defcfun ("cv_create_RNG" %rng) (:pointer rng) 
  "RNG constructor")


;; RNG::RNG(uint64 state)
;; RNG* cv_create_RNG_state(uint64 state)
(defcfun ("cv_create_RNG_state" rng-state) (:pointer rng) 
  "RNG constructor -  sets the RNG state to the specified value."
  (state :uint64))


(defun rng (&optional (state nil))
  (cond ((eq state nil)
         (%rng))
	((integerp state) (rng-state state))
        (t nil)))


;; double RNG::uniform(double a, double b)
;; double cv_RNG_uniform_double(RNG* self, double a, double b) 
(defcfun ("cv_RNG_uniform_double" uniform-d) :double
  "Returns the next random number sampled from the uniform distribution."
  (self (:pointer rng))
  (a :double)
  (b :double))


;; float RNG::uniform(float a, float b)
;; float cv_RNG_uniform_float(RNG* self, float a, float b)
(defcfun ("cv_RNG_uniform_float" uniform-f) :float
  "Returns the next random number sampled from the uniform distribution."
  (self (:pointer rng))
  (a :float)
  (b :float))


;; int RNG::uniform(int a, int b)
;; int cv_RNG_uniform_int(RNG* self, int a, int b)  
(defcfun ("cv_RNG_uniform_int" uniform-i) :int
  "Returns the next random number sampled from the uniform distribution."
  (self (:pointer rng))
  (a :int)
  (b :int))

(defun uniform  (rng a b)
  (cond ((or (eq (type-of (and a b)) 'FIXNUM)
	     (integerp (and a b)))
         (uniform-i rng a b))
	((eq (type-of (and a b)) 'SINGLE-FLOAT) (uniform-f rng a b))
	((eq (type-of (and a b)) 'DOUBLE-FLOAT) (uniform-d rng a b))
        (t nil)))


;;; Drawing Functions


;; BGR value constructor macro 

(defmacro bgr (b g r)
  "BGR value constructor macro"
  `(scalar ,b ,g ,r))


;; RGB value constructor macro 

(defmacro rgb (r g b)
  "BGR value constructor macro"
  `(scalar ,b ,g ,r))


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


;; void ellipse(Mat& img, const RotatedRect& box, const Scalar& color, int thickness=1, int lineType=8)
;; void cv_ellipse5(Mat* img, RotatedRect* box, Scalar* color, int thickness, int lineType)
(defcfun ("cv_ellipse5" %ellipse5) :void
  (img (:pointer mat))
  (box (:pointer rotated-rect))
  (color (:pointer scalar))
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
  (img (:pointer mat))
  (center (:pointer point))
  (axes (:pointer size))
  (angle :double)
  (start-angle :double)
  (end-angel :double)
  (color (:pointer scalar))
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
    (otherwise (error "Wrong number arguments to ellipse (~A)" (length args)))))


;; Size getTextSize(const string& text, int fontFace, double fontScale, int thickness, int* baseLine)
;; Size* cv_getTextSize(String* text, int fontFace, double fontScale, int thickness, int* baseLine)
(defcfun ("cv_getTextSize" %get-text-size) (:pointer size)
  (text (:pointer string*))
  (font-face :int)
  (font-scale :double)
  (thickness :int) 
  (base-line :pointer))

(defun get-text-size (text font-face font-scale thickness base-line)
  "Calculates the width and height of a text string."
  (%get-text-size text font-face font-scale thickness base-line))


;; void line(Mat& img, Point pt1, Point pt2, const Scalar& color, int thickness=1, int lineType=8, int shift=0)
;; void cv_line(Mat* img, Point* pt1, Point* pt2, Scalar* color, int thickness, int lineType, int shift) 
(defcfun ("cv_line" %line) :void
  (img (:pointer mat))
  (pt1 (:pointer point))
  (pt2 (:pointer point))
  (color (:pointer scalar))
  (thickness :int) 
  (line-type :int) 
  (shift :int))

(defun line (img pt1 pt2 color &optional (thickness 1) (line-type 8) (shift 0))
  "Draws a line segment connecting two points."
  (%line img pt1 pt2 color thickness line-type shift))


;; void putText(Mat& img, const string& text, Point org, int fontFace, double fontScale, Scalar color, int thickness=1, int lineType=8, 
;; bool bottomLeftOrigin=false)
;; void cv_putText(Mat* img, String* text, Point* org, int fontFace, double fontScale, Scalar* color, int thickness, int lineType, 
;; bool bottomLeftOrigin)

(defcfun ("cv_putText" %put-text) :void
  (img (:pointer mat))
  (text (:pointer string*))
  (org (:pointer point))
  (font-face :int)
  (font-scale :double)
  (color (:pointer scalar)) 
  (thickness :int) 
  (line-type :int)
  (bottom-left-orign :boolean))

(defun put-text (img text org font-face font-scale color &optional (thickness 1) (line-type 8) (bottom-left-origin nil))
  "Draws a text string."
  (%put-text img (foreign-alloc :string :initial-element text) org font-face font-scale color thickness line-type bottom-left-origin))


;; void rectangle(Mat& img, Point pt1, Point pt2, const Scalar& color, int thickness=1, int lineType=8, int shift=0)
;; void cv_rectangle(Mat* img, Point* pt1, Point* pt2, Scalar* color, int thickness, int lineType, int shift)
(defcfun ("cv_rectangle" %rectangle) :void
  (img (:pointer mat))
  (pt1 (:pointer point))
  (pt2 (:pointer point))
  (color (:pointer scalar))
  (thickness :int) 
  (line-type :int) 
  (shift :int))


(defun rectangle (img pt1 pt2 color &optional (thickness 1) (line-type 8) (shift 0))
  "Draws a simple, thick, or filled up-right rectangle."
  (%rectangle img pt1 pt2 color thickness line-type shift))



;;; Utility and System Functions and Macros

;; int64 getTickCount()
;; int64 cv_getTickCount()
(defcfun ("cv_getTickCount" get-tick-count)  :int64
  "Returns the number of ticks.")


;; double getTickFrequency()
;; double cv_getTickFrequency()
(defcfun ("cv_getTickFrequency" get-tick-frequency)  :double
  "Returns the number of ticks per second.")
