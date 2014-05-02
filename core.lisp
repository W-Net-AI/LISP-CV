;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; core.lisp
;;;; OpenCV bindings
;;;; The Core Functionality


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



;; Interop - String*


;; string* std_cstringToString(char* s, size_t len) 
(defcfun ("cstring_to_std_string" c-string-to-string) *string
  "Converts C string to C++"
  (s :string)
  (len :unsigned-int))


;;; Basic Structures


;; MatExpr* promote(Mat* m) 
(defcfun ("promote" <<) mat-expr
  "Converts a MAT to a MAT-EXPR.
   This is a shorthand version of the PROMOTE function." 
  (mat mat))


;; Mat* force(MatExpr* expr)
(defcfun ("force" >>) mat
  "Coerces a MAT-EXPR to a MAT. 
   This is a shorthand version of the FORCE function."
  (mat-expr mat-expr))


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
(defcfun ("cv_Size2f_area" area2f) :float
  "Gets the area of a SIZE2F construct"
  (self size2f))


 ;; Mat* cv_Mat_assignVal(Mat* self, Scalar* s)
(defcfun ("cv_Mat_assignVal" assgn-val) mat
  "Assign a scalar value to a matrix."
  (self mat)
  (s scalar))

;; uchar* ptr(int i0=0)
;; uchar* cv_Mat_ptr_index(Mat* self, int i)
(defmacro at (&optional self i j type)
	       `(mem-aref (%ptr ,self ,i) ,type ,j))


;; Point_<_Tp> br() const
;; Point* cv_Rect_br(Rect* self) 
(defcfun ("cv_Rect_br" rect-br) point 
  "Retrievies the bottom-right corner of a rectangle."
  (self rect))


;; int cv_Mat_channels(Mat* self)
(defcfun ("cv_Mat_channels" channels) :int
  (self mat))


;; Mat Mat::clone() const
;; Mat* cv_Mat_clone(Mat* self) 
(defcfun ("cv_Mat_clone" clone) mat
  "Creates a full copy of the array and the underlying data."
  (self mat))


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
(defcfun ("cv_Mat_convertTo" %convert-to) :int
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
(defcfun ("cv_Mat_copyTo" copy-to2) :void
  (self mat)
  (m mat))


;; void Mat::copyTo(OutputArray m, InputArray mask) const
;; void cv_Mat_copyTo_masked(Mat* self, Mat* m, Mat* mask)
(defcfun ("cv_Mat_copyTo_masked" copy-to3) :void
  (self mat)
  (m mat)
  (mask mat))


(defun copy-to (&optional mat m mask)
  (cond ((eq mask nil)
	 (copy-to2 mat m))
	(t (copy-to3 mat m mask))))


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


;; Mat Mat::diag(int d=0 ) const
;; Mat* cv_Mat_diag_d(Mat* self, int d)
(defcfun ("cv_Mat_diag_d" %diag) mat
  "Extracts a diagonal from a matrix."
  (self mat)
  (d :int))

(defun diag (self &optional (d 0))
  "Extracts a diagonal from a matrix."
  (%diag self d))


;; MatExpr / operator
;; MatExpr* cv_Mat_div(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_div" div) mat-expr
  (m1 mat)
  (m2 mat))


;; DMatch::DMatch()
;; DMatch* cv_create_DMatch() 
(defcfun ("cv_create_DMatch" dmatch0) dmatch
	 "DMatch constructor")

;; DMatch( int _queryIdx, int _trainIdx, float _distance ) 
;; DMatch* cv_create_DMatch3(int _queryIdx, int _trainIdx, float _distance)
(defcfun ("cv_create_DMatch3" dmatch3) dmatch
	 "DMatch constructor"
	 (query-idx :int)
	 (train-idx :int)
	 (distance :float))

;; DMatch( int _queryIdx, int _trainIdx, int _imgIdx, float _distance )
;; DMatch* cv_create_DMatch4(int _queryIdx, int _trainIdx, int _imgIdx, float _distance)
(defcfun ("cv_create_DMatch4" dmatch4) dmatch
	 "DMatch constructor"
	 (query-idx :int)
	 (train-idx :int)
	 (img-idx :int)
	 (distance :float))

(defun dmatch (&rest args)
       (cond ((eq (first args) nil)
	      (dmatch0))
	      ((and (first args) (not (fourth args)))
	       (dmatch3 (first args) (second args) (third args)))
	      ((fourth args)
	       (dmatch4 (first args) (second args) (third args) (fourth args)))
	       (t nil)))


;; _Tp dot(const Point_& pt) const
;; int cv_Point_dot(Point* self, Point* other) 
(defcfun ("cv_Point2i_dot" dot) :int 
  "Finds the dot product of a point."
  (self point)
  (other point))


;; _Tp dot(const Point_& pt) const
;; double cv_Point2d_dot(Point2d* self, Point2d* other) 
(defcfun ("cv_Point2d_dot" dot2d) :double
  "Finds the dot product of a point2d."
  (self point2d)
  (other point2d))


;; _Tp dot(const Point_& pt) const
;; float cv_Point2f_dot(Point2f* self, Point2f* other)
(defcfun ("cv_Point2f_dot" dot2f) :float
  "Finds the dot product of a point2f."
  (self point2f)
  (other point2f))


;; _Tp dot(const Point3_& pt) const
;; double cv_Point3d_dot(Point3d* self, Point3d* other)
(defcfun ("cv_Point3d_dot" dot3d) :double 
  "Finds the dot product of a point3d."
  (self point3d)
  (other point3d))


;; bool Mat::isContinuous() const
;; bool cv_Mat_isContinuous(Mat* self) 
(defcfun ("cv_Mat_isContinuous" is-continuous) :boolean
  (self mat))


;; _Tp dot(const Point3_& pt) const
;; float cv_Point3f_dot(Point3f* self, Point3f* other)
(defcfun ("cv_Point3f_dot" dot3f) :float 
  "Finds the dot product of a point3f."
  (self point3f)
  (other point3f))


;; _Tp dot(const Point3_& pt) const
;; int cv_Point3i_dot(Point3i* self, Point3i* other)
(defcfun ("cv_Point3i_dot" dot3i) :int 
  "Finds the dot product of a point3i."
  (self point3i)
  (other point3i))


;; bool Mat::empty() const
;; int cv_Mat_empty(Mat* self) 
(defcfun ("cv_Mat_empty" empty) :boolean
  "Returns true if the array has no elements."
  (self mat))


;; Mat* force(MatExpr* expr)
(defcfun ("force" force) mat
  "Coerces a MAT-EXPR to a MAT."
  (mat-expr mat-expr))


;; _Tp width, height
;; double cv_Size_height(Size* self) 
(defcfun ("cv_Size_height" height) :double
  "Gets the height of a SIZE construct"
  (self size))


;; _Tp width, height
;; float cv_Size2f_height(Size* self) 
(defcfun ("cv_Size2f_height" height2f) :float
  "Gets the height of a SIZE2F"
  (self size2f))


;; KeyPoint::KeyPoint()
;; KeyPoint* cv_create_KeyPoint()
(defcfun ("cv_create_KeyPoint" key-point0) key-point
  "KEY-POINT constructor")

;; KeyPoint::KeyPoint(float x, float y, float _size, float _angle=-1, float _response=0, int _octave=0, int _class_id=-1)
;; KeyPoint* cv_create_KeyPoint7(float x, float y, float _size, float _angle, float _response, int _octave, int _class_id)
(defcfun ("cv_create_KeyPoint7" key-point7) key-point
  "KEY-POINT constructor"
  (x :float)
  (y :float)
  (size :float)
  (angle :float)
  (response :float)
  (octave :int)
  (class-id :int))

(defun key-point (&optional x y size (angle -1) (response 0) (octave 0) (class-id -1))
	   (cond ((eq x nil)
		  (key-point0))
		 (x
		  (key-point7 x y size angle response octave class-id))
		 (t nil)))


;; Mat::Mat()
;; Mat* cv_create_Mat()
(defcfun ("cv_create_Mat" %mat) mat
  "MAT constructor")

;; Mat::Mat()
;; Mat* cv_create_Mat()
(defcfun ("cv_create_Mat" %%mat) (mat :garbage-collect t)
  "MAT constructor")

(defun mat (&rest args)
  (cond ((eq (first args) nil) (%mat))
	((eq :t (car args)) (%%mat))
	(t nil)))


;; Mat::Mat(int rows, int cols, int type, void* data) 
;; Mat* cv_create_Mat_with_data(int rows, int cols, int type, void* data)
(defcfun ("cv_create_Mat_with_data" mat-data) mat
  (rows :int)
  (cols :int)
  (type :int)
  (data :pointer))


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


;; void Mat::locateROI(Size& wholeSize, Point& ofs) const
;; void cv_Mat_locateROI(Mat* self, Size* s, Point* p) 
(defcfun ("cv_Mat_locateROI" locate-roi) :void 
	 "Locates the matrix header within a parent matrix."
	 (self mat)
	 (whole-size size)
	 (ofs point))


;; static MatExpr Mat::ones(int rows, int cols, int type)
;; Mat* cv_create_ones(int rows, int cols, int type)
(defcfun ("cv_create_ones" mat-ones) mat
  (rows :int)
  (cols :int)
  (type :int))


;; Size Mat::size() const
;; Size* cv_Mat_size(Mat* self)
(defcfun ("cv_Mat_size" mat-size) size
  (self mat))


;; MatExpr * operator
;; MatExpr* cv_Mat_scale(MatExpr* m, double alpha)
(defcfun ("cv_Mat_scale" scale) mat-expr
  (m mat-expr)
  (alpha :double))


;; int Mat::type() const 
;; int cv_Mat_type(Mat* self) 
(defcfun ("cv_Mat_type" mat-type) :int
  (self mat))


;; Mat::Mat(int rows, int cols, int type)
;; Mat* cv_create_Mat_typed(int rows, int cols, int type)
(defcfun ("cv_create_Mat_typed" mat-typed)  mat
  "MAT constructor with a row, column and type parameter."
  (rows :int)
  (cols :int)
  (type :int))


;; Mat::Mat(int rows, int cols, int type, const Scalar& s)
;; Mat* cv_create_Mat_with_value(int rows, int cols, int type, Scalar* s)
(defcfun ("cv_create_Mat_with_value" mat-value) mat
  (rows :int)
  (cols :int)
  (type :int)
  (s scalar))

;; static MatExpr Mat::zeros(int rows, int cols, int type)
;; Mat* cv_create_zeros(int rows, int cols, int type)
(defcfun ("cv_create_zeros" mat-zeros) mat
  (rows :int)
  (cols :int)
  (type :int))


;; MatExpr * operator
;; MatExpr* cv_Mat_mult(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_mult" mul) mat-expr
  (m1 mat)
  (m2 mat))

;; Point_()
;; Point2##t * cv_create_Point2##t ( tn x,  tn y)
(defcfun ("cv_create_Point2i" point0) point
  "Point constructor")

;; Point_(_Tp _x, _Tp _y)
;; Point2##t * cv_create_Point2##t ( tn x,  tn y) 
(defcfun ("cv_create_Point2i" point2) point
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
  (self point))


;; _Tp x, y
;; int cv_Point_getY(Point* self)
(defcfun ("cv_Point2i_getY" point-y) :int 
  "Retrieves y coordinate of a point"
  (self point))


;; typedef Point_<double> Point2d
;; Point2##t * cv_create_Point2##t ( tn x,  tn y) 
(defcfun ("cv_create_Point2d" point2d0) point2d 
  "Point2d constructor")


;; typedef Point_<double> Point2d
;; Point2##t * cv_create_Point2##t ( tn x,  tn y)  
(defcfun ("cv_create_Point2d" point2d2) point2d 
  "Point2d constructor"
  (x :double)
  (y :double))


(defun point2d (&optional x y)
	   (cond ((eq (or x y) nil)
		  (point2d0))
		 ((and x y)
		  (point2d2 x y))
		 (t nil)))


;; _Tp x, y
;; double cv_Point2d_getX(Point2d* self) 
(defcfun ("cv_Point2d_getX" point2d-x) :double
  "Retrieves x coordinate of a point2d"
  (self point2d))


;; _Tp x, y
;; double cv_Point2d_getY(Point2d* self) 
(defcfun ("cv_Point2d_getY" point2d-y) :double
  "Retrieves y coordinate of a point2d"
  (self point2d))


;; typedef Point_<float> Point2f
;; tn cv_Point2##t##_getX( Point2##t * self) 
(defcfun ("cv_create_Point2f" point2f0) point2f 
  "Point2f constructor")



;; typedef Point_<float> Point2f
;; Point2##t * cv_create_Point2##t ( tn x,  tn y)  
(defcfun ("cv_create_Point2f" point2f2) point2f 
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
  (self point2f))


;; _Tp x, y;
;; float cv_Point2f_getY(Point2f* self) 
(defcfun ("cv_Point2f_getY" point2f-y) :float
  "Retrieves y coordinate of a point2f"
  (self point2f))


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z) 
(defcfun ("cv_create_Point3d" point3d0) point3d 
  "Point3d constructotr")


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z) 
(defcfun ("cv_create_Point3d" point3d2) point3d 
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
  (self point3d))


;; _Tp x, y, z
;; double cv_Point3d_getY(Point3d* self) 
(defcfun ("cv_Point3d_getY" point3d-y) :double
  "Retrieves y coordinate of a point3d"
  (self point3d))


;; _Tp x, y, z
;; double cv_Point3d_getZ(Point3d* self) 
(defcfun ("cv_Point3d_getZ" point3d-z) :double
  "Retrieves z coordinate of a point3d"
  (self point3d))


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3f" point3f0) point3f 
  "Point3f constructor")


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3f" point3f2) point3f 
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
  (self point3f))


;; _Tp x, y, z
;; float cv_Point3f_getY(Point3f* self) 
(defcfun ("cv_Point3f_getY" point3f-y) :float
  "Retrieves y coordinate of a point3f"
  (self point3f))


;; _Tp x, y, z
;; float cv_Point3f_getZ(Point3f* self) 
(defcfun ("cv_Point3f_getZ" point3f-z) :float
  "Retrieves z coordinate of a point3f"
  (self point3f))


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3i" point3i0) point3i 
  "Poin3i constructor")


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3i" point3i2) point3i 
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
  (self point3i))


;; _Tp x, y, z
;; int cv_Point3i_getY(Point3i* self) 
(defcfun ("cv_Point3i_getY" point3i-y) :int
  "Retrieves y coordinate of a point3i"
  (self point3i))


;; _Tp x, y, z
;; int cv_Point3i_getZ(Point3i* self) 
(defcfun ("cv_Point3i_getZ" point3i-z) :int
  "Retrieves z coordinate of a point3i"
  (self point3i))


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


;; Rect_()
;; Rect* cv_create_Rect() 
(defcfun ("cv_create_Rect" rect0) rect 
  "RECT constructor.")


;; Rect_(_Tp _x, _Tp _y, _Tp _width, _Tp _height)
;; Rect* cv_create_Rect4(int x, int y, int width, int height) 
(defcfun ("cv_create_Rect4" rect4) rect
  "RECT constructor."
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defun rect (&optional x y width height)
	   (cond ((eq (or x y) nil)
		  (rect0))
		 ((and x y)
		  (rect4 x y width height))
		 (t nil)))



;; Rect_(_Tp _x, _Tp _y, _Tp _width, _Tp _height)
;; _Tp x, y, width, height
;; Rect* cv_Rect_clone(Rect* self)
(defcfun ("cv_Rect_clone" rect-clone) rect
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
(defcfun ("cv_Rect_tl" rect-tl) point 
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


;; Mat::Mat(const Mat& m, const Rect& roi)
;; Mat* cv_Mat_get_ROI(Mat* self, Rect* roi)
(defcfun ("cv_Mat_get_ROI" roi) mat 
  "Returns matrix header corresponding to the rectangular sub-array of input MAT."
  (self mat)
  (roi rect))


;; RotatedRect(const Point2f& center, const Size2f& size, float angle)
;; RotatedRect* cv_create_RotatedRect(Point2f* center, Size2f* size, float angle)
(defcfun ("cv_create_RotatedRect" %rotated-rect) rotated-rect
  (center point)
  (size size)
  (angle :float))


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


(defun rotated-rect (&rest args)
  (cond
    ((and (third args) (not (fourth args)))
     (%rotated-rect (first args) (second args) (third args)))
    ((or (eq ':c (first args)) (eq ':center (first args)))
     (rotated-rect-center (second args)))
    ((or (eq :s (first args)) (eq :size (first args))) 
     (rotated-rect-size (second args)))
    ((or (eq :br (first args)) (eq :bounding-rect (first args))) 
     (rotated-rect-bounding-rect (second args)))
    (t nil)))


;; Mat Mat::rowRange(int startrow, int endrow) const
;; Mat* cv_Mat_getRowRange(Mat* self, int startrow, int endrow)
(defcfun ("cv_Mat_getRowRange" row-range) mat
  "Creates a matrix header for the specified row span."
  (self mat)
  (startrow :int)
  (endrow :int))

;; int rows, cols;
;; int cv_Mat_rows(Mat* self) 
(defcfun ("cv_Mat_rows" rows) :int
  (self mat))

;; Scalar_<_Tp>::Scalar_(_Tp v0, _Tp v1, _Tp v2, _Tp v3)
;; Scalar* cv_create_Scalar(double val0, (double val1, double val2, double val3)
(defcfun ("cv_create_Scalar" %scalar) scalar
  (val0 :double)
  (val1 :double)
  (val2 :double)
  (val3 :double))


(defun scalar (val0 &optional (val1 0) (val2 0) (val3 0))
  "SCALAR constructor"
  (%scalar (coerce val0 'double-float) (coerce val1 'double-float) (coerce val2 'double-float) (coerce val3 'double-float)))

;; Scalar_<_Tp> Scalar_<_Tp>::all(_Tp v0)
;; Scalar* cv_create_scalarAll(double val0123)
(defcfun ("cv_create_scalarAll" %scalar-all) scalar
  (val0123 :double))


(defun scalar-all (val0123)
  "SCALAR conctctor - initializes all of the scalar values 0...3 with val0123"
  (%scalar-all (coerce val0123 'double-float)))


;; Size_()
;; Size* cv_create_Size() 
(defcfun ("cv_create_Size" size0) size
  "Create SIZE construct")


;; Size_(_Tp _width, _Tp _height)
;; cv_create_Size2(double width, double height)
(defcfun ("cv_create_Size2" size2) size
  "SIZE constructor"
  (width :double)
  (height :double))


(defun size (&optional arg1 arg2)
  (cond ((eq (or arg1 arg2) nil)
	 (size0))
	((numberp (or arg1 arg2)) 
	 (size2 (coerce arg1 'double-float) 
		(coerce arg2 'double-float)))
	((eq (type-of arg1) 'cv-mat) (mat-size arg1))
	(t nil)))


;; Size_<float>()
;; Size2f* cv_create_Size2f(float width, float height)
(defcfun ("cv_create_Size2f" size2f0) size2f
  "Size2f constructor")


;; Size_<float>(float width, float height)
;; Size2f* cv_create_Size2f(float width, float height)
(defcfun ("cv_create_Size2f" size2f2) size2f
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
  (self mat))


;; MatExpr - operator
;; MatExpr* cv_Mat_sub(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_sub" sub) mat-expr
  (m1 mat)
  (m2 mat))


;; TermCriteria::TermCriteria(int type, int maxCount, double epsilon)
;; TermCriteria* cv_create_TermCriteria(int type, int maxCount, double epsilon) 
(defcfun ("cv_create_TermCriteria" term-criteria0) term-criteria)


;; TermCriteria::TermCriteria(int type, int maxCount, double epsilon)
;; TermCriteria* cv_create_TermCriteria(int type, int maxCount, double epsilon) 
(defcfun ("cv_create_TermCriteria3" term-criteria3) term-criteria
	 (type :int)
	 (max-count :int)
	 (epsilon :double))


(defun term-criteria (&optional type max-count epsilon)
	   (cond ((eq type nil)
		  (term-criteria0))
		 (type
		  (term-criteria3 type max-count epsilon))
		 (t nil)))


;; size_t Mat::total() const
;; size_t cv_Mat_total(Mat* self)
(defcfun ("cv_Mat_total" total) :unsigned-int
  "Returns the total number of array elements."
  (self mat))


;; _Tp width, height
;; double cv_Size_width(Size* self) 
(defcfun ("cv_Size_width" width) :double
  "Gets the width of a SIZE construct"
  (self size))


;; _Tp width, height
;; float cv_Size2f_width(Size* self) 
(defcfun ("cv_Size2f_width" width2f) :float
  "Gets the width of a SIZE2F"
  (self size2f))


;;; Basic Structures


;;; Operations on Arrays


;; MatExpr abs(const Mat& m)
;; MatExpr* cv_abs(Mat* m)
(defcfun ("cv_abs" *abs) mat-expr
  (m mat))


;; void max(InputArray src1, InputArray src2, OutputArray dst)
;; void cv_max(Mat* src1, Mat* src2, Mat* dst)
(defcfun ("cv_max" *max) :void 
  "Calculates per-element maximum of two arrays."
  (src1 mat)
  (src2 mat)
  (dest mat))


;; void min(InputArray src1, InputArray src2, OutputArray dst)
;; void cv_min(Mat* src1, Mat* src2, Mat* dst)
(defcfun ("cv_min" *min) :void 
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


;; void bitwise_and(InputArray src1, InputArray src2, OutputArray dst, InputArray mask=noArray())
;; void cv_bitwise_and(Mat* src1, Mat* src2, Mat* dst, Mat* mask)
(defcfun ("cv_bitwise_and" %bitwise-and) :void 
	 (src1 mat)
	 (src2 mat)
     (dest mat)
	 (mask mat))

(defun bitwise-and (src1 src2 dest &optional (mask (mat)))
  "Calculates the per-element bit-wise conjunction of two arrays."
  (%bitwise-or src1 src2 dest mask))


;; void bitwise_not(InputArray src, OutputArray dst, InputArray mask=noArray())
;; void cv_bitwise_not(Mat* src, Mat* dst, Mat* mask)
(defcfun ("cv_bitwise_not" %bitwise-not) :void 
	 (src mat)
     (dest mat)
	 (mask mat))

(defun bitwise-not (src dest &optional (mask (mat)))
  "Inverts every bit of an array."
  (%bitwise-not src dest mask))


;; void bitwise_or(InputArray src1, InputArray src2, OutputArray dst, InputArray mask=noArray())
;; void cv_bitwise_or(Mat* src1, Mat* src2, Mat* dst, Mat* mask)
(defcfun ("cv_bitwise_or" %bitwise-or) :void 
	 (src1 mat)
	 (src2 mat)
     (dest mat)
	 (mask mat))

(defun bitwise-or (src1 src2 dest &optional (mask (mat)))
  "Calculates the per-element bit-wise disjunction of two arrays."
  (%bitwise-or src1 src2 dest mask))


;; void bitwise_xor(InputArray src1, InputArray src2, OutputArray dst, InputArray mask=noArray())
;; void cv_bitwise_xor(Mat* src1, Mat* src2, Mat* dst, Mat* mask)
(defcfun ("cv_bitwise_xor" %bitwise-xor) :void
  "Calculates the per-element bit-wise “exclusive or” operation on two arrays."
  (src1 mat)
  (src2 mat)
  (dest mat)
  (mask mat))

(defun bitwise-xor (src1 src2 dest &optional (mask (mat)))
  "Calculates the per-element bit-wise disjunction of two arrays."
  (%bitwise-xor src1 src2 dest mask))


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


;; void exp(InputArray src, OutputArray dst)
;; void cv_exp(Mat* src, Mat* dst)
(defcfun ("cv_exp" *exp) :void
  "Calculates the exponent of every array element."
  (src mat)
  (dest mat))


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
  "Checks if array elements lie between the elements of two other arrays."
  (src mat)
  (lowerb scalar)
  (upperb scalar)
  (dst :pointer mat))


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


;; void log(InputArray src, OutputArray dst)
;; void cv_log(Mat* src, Mat* dst)
(defcfun ("cv_log" *log) :int
  "Calculates the natural logarithm of every array element."
  (src mat)
  (dest mat))


;; Scalar mean(InputArray src, InputArray mask=noArray())
;; Scalar* cv_mean(Mat* src, Mat* mask)
(defcfun ("cv_mean" %mean) scalar
  "Calculates an average (mean) of array elements."
  (src mat)
  (mask mat))

(defun mean (src &optional (mask (mat)))
  "Calculates an average (mean) of array elements."
  (%mean src mask))


;; void minMaxLoc(InputArray src, double* minVal, double* maxVal=0, Point* minLoc=0, Point* maxLoc=0, InputArray mask=noArray())
;; void cv_minMaxLoc(Mat* src, double* minVal, double* maxVal, Point* minLoc, Point* maxLoc, Mat* mask)
(defcfun ("cv_minMaxLoc" %min-max-loc) :void
  (src mat)
  (min-val :pointer)
  (max-val :pointer)
  (min-loc point)
  (max-loc point)
  (mask mat))

(defun min-max-loc (src min-val &optional (max-val (null-pointer)) (min-loc (null-pointer)) (max-loc (null-pointer)) (mask (mat)))
       "Finds the global minimum and maximum in an array."
       (%min-max-loc src min-val max-val min-loc max-loc mask))


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


(defun normalize (src dest &optional (alpha 1) (beta 0) (norm-type  +norm-l2+) (dtype -1) (mask (mat)))
       "Normalizes the norm or value range of an array."
       (%normalize src dest alpha beta norm-type  dtype  mask))


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

(defun subtract (src1 src2 dest &optional (mask (mat)) (dtype -1))
       (%subtract src1 src2 dest mask dtype))


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




(defun get-text-size (text font-face font-scale thickness base-line)
  "Calculates the width and height of a text string."
  (%get-text-size text font-face font-scale thickness base-line))


;; void line(Mat& img, Point pt1, Point pt2, const Scalar& color, int thickness=1, int lineType=8, int shift=0)
;; void cv_line(Mat* img, Point* pt1, Point* pt2, Scalar* color, int thickness, int lineType, int shift) 
(defcfun ("cv_line" %line) :void
  (img mat)
  (pt1 point)
  (pt2 point)
  (color scalar)
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
  (%put-text img (c-string-to-string text (length text)) org font-face font-scale color thickness line-type bottom-left-origin))


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


;; int64 getTickCount()
;; int64 cv_getTickCount()
(defcfun ("cv_getTickCount" get-tick-count)  :int64
  "Returns the number of ticks.")


;; double getTickFrequency()
;; double cv_getTickFrequency()
(defcfun ("cv_getTickFrequency" get-tick-frequency)  :double
  "Returns the number of ticks per second.")


;; void sqrt(InputArray src, OutputArray dst)
;; void cv_sqrt(Mat* src, Mat* dst)
(defcfun ("cv_sqrt" *sqrt) :void
  "Calculates a square root of array elements."
  (src mat)
  (dest mat))
