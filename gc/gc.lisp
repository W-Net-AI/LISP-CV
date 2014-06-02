;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; gc.lisp
;;;; OpenCV bindings
;;;; Garbage collected versions of functions.

(in-package :gc)


;; Interop - String*


;; string* std_cstringToString(char* s, size_t len) 
(defcfun ("cstring_to_std_string" c-string-to-string) (cv::*string :garbage-collect t)
	 "Converts C string to C++"
	 (s :string)
	 (len :unsigned-int))


;;; CORE - Basic Structures


;; Mat* force(MatExpr* expr)
(defcfun ("force" >>) (cv::mat :garbage-collect t)
	 "Coerces a MAT-EXPR to a MAT. 
   This is a shorthand version of the FORCE function."
   (mat-expr cv::mat-expr))


;; MatExpr* promote(Mat* m) 
(defcfun ("promote" <<) (cv::mat-expr :garbage-collect t)
	 "Converts a MAT to a MAT-EXPR.
   This is a shorthand version of the PROMOTE function." 
   (mat cv::mat))


;; MatExpr + operator
;; MatExpr* cv_Mat_add(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_add" add) (cv::mat-expr :garbage-collect t)
	 (m1 cv::mat)
	 (m2 cv::mat))


;; Mat& Mat::adjustROI(int dtop, int dbottom, int dleft, int dright)
;; Mat* cv_Mat_adjustROI(Mat* self, int dtop, int dbottom, int dleft, int dright) 
(defcfun ("cv_Mat_adjustROI" adjust-roi) (cv::mat :garbage-collect t)
	 "Adjusts a submatrix size and position within the parent matrix."
	 (self cv::mat)
	 (dtop :int)
	 (dbottom :int)
	 (dleft :int)
	 (dright :int))


;; Mat Mat::clone() const
;; Mat* cv_Mat_clone(Mat* self) 
(defcfun ("cv_Mat_clone" clone) (cv::mat :garbage-collect t)
	 "Creates a full copy of the array and the underlying data."
	 (self cv::mat))


;; Mat Mat::colRange(int startcol, int endcol) const
;; Mat* cv_Mat_getColRange(Mat* self, int startcol, int endrow)
(defcfun ("cv_Mat_getColRange" col-range) (cv::mat :garbage-collect t)
	 "Creates a matrix header for the specified column span."
	 (self cv::mat)
	 (startcol :int)
	 (endcol :int))


;; Mat Mat::cross(InputArray m) const
;; Mat* cv_Mat_cross(Mat* self, Mat* m)
(defcfun ("cv_Mat_cross" cross) (cv::mat :garbage-collect t)
	 "Computes a cross-product of two 3-element vectors."
	 (self cv::mat)
	 (m cv::mat))


;; Mat* cv_Mat_diag_d(Mat* self, int d)
(defcfun ("cv_Mat_diag_d" %diag) (cv::mat :garbage-collect t)
	 "Extracts a diagonal from a matrix."
	 (self cv::mat)
	 (d :int))

(defun diag (self &optional (d 0))
       "Extracts a diagonal from a matrix."
       (%diag self d))


;; MatExpr / operator
;; MatExpr* cv_Mat_div(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_div" div) (cv::mat-expr :garbage-collect t)
	 (m1 cv::mat)
	 (m2 cv::mat))


;; DMatch::DMatch()
;; DMatch* cv_create_DMatch() 
(defcfun ("cv_create_DMatch" dmatch0) (cv::dmatch :garbage-collect t)
	 "DMatch constructor")

;; DMatch( int _queryIdx, int _trainIdx, float _distance ) 
;; DMatch* cv_create_DMatch3(int _queryIdx, int _trainIdx, float _distance)
(defcfun ("cv_create_DMatch3" dmatch3) (cv::dmatch :garbage-collect t)
	 "DMatch constructor"
	 (query-idx :int)
	 (train-idx :int)
	 (distance :float))

;; DMatch( int _queryIdx, int _trainIdx, int _imgIdx, float _distance )
;; DMatch* cv_create_DMatch4(int _queryIdx, int _trainIdx, int _imgIdx, float _distance)
(defcfun ("cv_create_DMatch4" dmatch4) (cv::dmatch :garbage-collect t)
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


;; Mat* force(MatExpr* expr)
(defcfun ("force" force) (cv::mat :garbage-collect t)
	 "Coerces a MAT-EXPR to a MAT."
	 (mat-expr cv::mat-expr))


;; KeyPoint::KeyPoint()
;; KeyPoint* cv_create_KeyPoint()
(defcfun ("cv_create_KeyPoint" key-point0) (cv::key-point :garbage-collect t)
	 "KEY-POINT constructor")

;; KeyPoint::KeyPoint(float x, float y, float _size, float _angle=-1, float _response=0, int _octave=0, int _class_id=-1)
;; KeyPoint* cv_create_KeyPoint7(float x, float y, float _size, float _angle, float _response, int _octave, int _class_id)
(defcfun ("cv_create_KeyPoint7" key-point7) (cv::key-point :garbage-collect t)
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
(defcfun ("cv_create_Mat" %mat) (cv::mat :garbage-collect t)
	 "MAT constructor")


;; Mat::Mat(int rows, int cols, int type, void* data) 
;; Mat* cv_create_Mat_with_data(int rows, int cols, int type, void* data)
(defcfun ("cv_create_Mat_with_data" %mat-data) (cv::mat :garbage-collect t)
	 (rows :int)
	 (cols :int)
	 (type :int)
	 (data :pointer))


(let ((previous nil))
     (defun %%mat-data (rows cols type &rest args)
	    (unless (equal (second args) (car previous))
		    (setf previous (cons (second args) (foreign-alloc (first args) 
								      :initial-contents (second args)))))
								      (%mat-data rows cols type (cdr previous))))


;; Mat::t
;; MatExpr* cv_Mat_transpose_mat(Mat* self) 
(defcfun ("cv_Mat_transpose_mat" mat-expr-t) (cv::mat-expr :garbage-collect t)
	 "Transposes a matrix."
	 (self cv::mat))


;; static MatExpr Mat::eye(int rows, int cols, int type) 
;; Mat* cv_create_identity(int rows, int cols, int type)
(defcfun ("cv_create_identity" mat-eye) (cv::mat :garbage-collect t)
	 "Returns an identity matrix of the specified size and type."
	 (rows :int)
	 (cols :int)
	 (type :int))


;; static MatExpr Mat::ones(int rows, int cols, int type)
;; Mat* cv_create_ones(int rows, int cols, int type)
(defcfun ("cv_create_ones" mat-ones) (cv::mat :garbage-collect t)
	 (rows :int)
	 (cols :int)
	 (type :int))


;; Mat::Mat(const Mat& m, const Range& rowRange, const Range& colRange=Range::all() )
;; Mat* cv_Mat_get_Range(Mat* self, Range* rowRange, Range* colRange)
(defcfun ("cv_Mat_with_Range" %mat-range) (cv::mat :garbage-collect t)
  "MAT constructor with Range parameters."
  (self cv::mat)
  (row-range cv::range)
  (col-range cv::range))


(defun mat-range (self row-range &optional (col-range (range-all) given-col-range))
  (let ((return (%mat-range self row-range col-range)))
    (if given-col-range nil (cv::del-range col-range))
    return))



;; Size Mat::size() const
;; Size* cv_Mat_size(Mat* self)
(defcfun ("cv_Mat_size" mat-size) (cv::size :garbage-collect t)
	 (self cv::mat))


;; MatExpr * operator
;; MatExpr* cv_Mat_scale(MatExpr* m, double alpha)
(defcfun ("cv_Mat_scale" scale) (cv::mat-expr :garbage-collect t)
	 (m cv::mat-expr)
	 (alpha :double))


;; Mat::Mat(int rows, int cols, int type)
;; Mat* cv_create_Mat_typed(int rows, int cols, int type)
(defcfun ("cv_create_Mat_typed" %mat-typed) (cv::mat :garbage-collect t)
	 "MAT constructor with a row, column and type parameter."
	 (rows :int)
	 (cols :int)
	 (type :int))


;; Mat::Mat(int rows, int cols, int type, const Scalar& s)
;; Mat* cv_create_Mat_with_value(int rows, int cols, int type, Scalar* s)
(defcfun ("cv_create_Mat_with_value" %mat-value) (cv::mat :garbage-collect t)
	 (rows :int)
	 (cols :int)
	 (type :int)
	 (s cv::scalar))


(let ((previous nil))
     (defun %%mat-value (rows cols type s)
	    (unless (equal s (car previous))
		    (setf previous (cons s (scalar (if (first s) (first s) 0) 
						   (if (second s) (second s) 0) 
						   (if (third s) (third s) 0) 
						   (if (fourth s) (fourth s) 0)))))
						   (%mat-value rows cols type (cdr previous))))


;;; MAT


(defun mat (&rest args)
  
  "MAT constructor"  
  
  (cond ((eq (first args) nil) (%mat))

	((typep (second args) 'cv::cv-range)
	 (apply #'mat-range args))
	
	((and (eq (fourth args) nil) (first args)) 
	 (apply #'%mat-typed args))
	
	((typep (fourth args) 'cv::cv-scalar)
	 (apply #'%mat-value args))
	
	((listp (fourth args))
	 (apply #'%%mat-value args))
	
	((pointerp (fourth args))
	 (apply #'%mat-data args))
	
	((listp (fifth args))
	 (apply #'%%mat-data args))

	(t nil)))


;;; MAT


;; static MatExpr Mat::zeros(int rows, int cols, int type)
;; Mat* cv_create_zeros(int rows, int cols, int type)
(defcfun ("cv_create_zeros" mat-zeros) (cv::mat :garbage-collect t)
	 (rows :int)
	 (cols :int)
	 (type :int))


;; MatExpr * operator
;; MatExpr* cv_Mat_mult(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_mult" mul) (cv::mat-expr :garbage-collect t)
	 (m1 cv::mat)
	 (m2 cv::mat))


;; Point_()
;; Point2##t * cv_create_Point2##t ( tn x,  tn y)
(defcfun ("cv_create_Point2i" point0) (cv::point :garbage-collect t)
	 "Point constructor")

;; Point_(_Tp _x, _Tp _y)
;; Point2##t * cv_create_Point2##t ( tn x,  tn y) 
(defcfun ("cv_create_Point2i" point2) (cv::point :garbage-collect t)
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
	 (self cv::point))


;; _Tp x, y
;; int cv_Point_getY(Point* self)
(defcfun ("cv_Point2i_getY" point-y) :int 
	 "Retrieves y coordinate of a point"
	 (self cv::point))


;; typedef Point_<double> Point2d
;; Point2##t * cv_create_Point2##t ( tn x,  tn y) 
(defcfun ("cv_create_Point2d" point-2d0) (cv::point-2d :garbage-collect t) 
	 "Point2d constructor")


;; typedef Point_<double> Point2d
;; Point2##t * cv_create_Point2##t ( tn x,  tn y)  
(defcfun ("cv_create_Point2d" point-2d2) (cv::point-2d :garbage-collect t) 
	 "Point2d constructor"
	 (x :double)
	 (y :double))


(defun point-2d (&optional x y)
       (cond ((eq (or x y) nil)
	      (point-2d0))
	      ((and x y)
	       (point-2d2 x y))
	       (t nil)))


;; _Tp x, y
;; double cv_Point2d_getX(Point2d* self) 
(defcfun ("cv_Point2d_getX" point-2d-x) :double
  "Retrieves x coordinate of a point-2d"
  (self cv::point-2d))


;; _Tp x, y
;; double cv_Point2d_getY(Point2d* self) 
(defcfun ("cv_Point2d_getY" point-2d-y) :double
  "Retrieves y coordinate of a point-2d"
  (self cv::point-2d))


;; typedef Point_<float> Point2f
;; tn cv_Point2##t##_getX( Point2##t * self) 
(defcfun ("cv_create_Point2f" point-2f0) (cv::point-2f :garbage-collect t) 
	 "Point2f constructor")



;; typedef Point_<float> Point2f
;; Point2##t * cv_create_Point2##t ( tn x,  tn y)  
(defcfun ("cv_create_Point2f" point-2f2) (cv::point-2f :garbage-collect t) 
	 "Point2f constructor"
	 (x :float)
	 (y :float))


(defun point-2f (&optional x y)
       (cond ((eq (or x y) nil)
	      (point-2f0))
	      ((and x y)
	       (point-2f2 x y))
	       (t nil)))


;; _Tp x, y;
;; float cv_Point2f_getX(Point2f* self) 
(defcfun ("cv_Point2f_getX" point-2f-x) :float
  "Retrieves x coordinate of a point-2f"
  (self cv::point-2f))


;; _Tp x, y;
;; float cv_Point2f_getY(Point2f* self) 
(defcfun ("cv_Point2f_getY" point-2f-y) :float
  "Retrieves y coordinate of a point-2f"
  (self cv::point-2f))


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z) 
(defcfun ("cv_create_Point3d" point-3d0) (cv::point-3d :garbage-collect t) 
	 "Point3d constructotr")


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z) 
(defcfun ("cv_create_Point3d" point-3d2) (cv::point-3d :garbage-collect t) 
	 "Point3d constructor"
	 (x :double)
	 (y :double)
	 (z :double))


(defun point-3d (&optional x y z)
       (cond ((eq (or x y) nil)
	      (point-3d0))
	      ((and x y)
	       (point-3d2 x y z))
	       (t nil)))


;; _Tp x, y, z
;; double cv_Point3d_getX(Point3d* self) 
(defcfun ("cv_Point3d_getX" point-3d-x) :double
  "Retrieves x coordinate of a point-3d"
  (self cv::point-3d))


;; _Tp x, y, z
;; double cv_Point3d_getY(Point3d* self) 
(defcfun ("cv_Point3d_getY" point-3d-y) :double
  "Retrieves y coordinate of a point-3d"
  (self cv::point-3d))


;; _Tp x, y, z
;; double cv_Point3d_getZ(Point3d* self) 
(defcfun ("cv_Point3d_getZ" point-3d-z) :double
  "Retrieves z coordinate of a point-3d"
  (self cv::point-3d))


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3f" point-3f0) (cv::point-3f :garbage-collect t) 
	 "Point3f constructor")


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3f" point-3f2) (cv::point-3f :garbage-collect t)
	 "Point3f constructor"
	 (x :float)
	 (y :float)
	 (z :float))


(defun point-3f (&optional x y z)
       (cond ((eq (or x y) nil)
	      (point-3f0))
	      ((and x y)
	       (point-3f2 x y z))
	       (t nil)))


;; _Tp x, y, z
;; float cv_Point3f_getX(Point3f* self) 
(defcfun ("cv_Point3f_getX" point-3f-x) :float
  "Retrieves x coordinate of a point-3f"
  (self cv::point-3f))


;; _Tp x, y, z
;; float cv_Point3f_getY(Point3f* self) 
(defcfun ("cv_Point3f_getY" point-3f-y) :float
  "Retrieves y coordinate of a point-3f"
  (self cv::point-3f))


;; _Tp x, y, z
;; float cv_Point3f_getZ(Point3f* self) 
(defcfun ("cv_Point3f_getZ" point-3f-z) :float
  "Retrieves z coordinate of a point-3f"
  (self cv::point-3f))


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3i" point-3i0) (cv::point-3i :garbage-collect t) 
	 "Point3i constructor")


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3i" point-3i2) (cv::point-3i :garbage-collect t)
	 "Point3i constructor"
	 (x :int)
	 (y :int)
	 (z :int))


(defun point-3i (&optional x y z)
       (cond ((eq (or x y) nil)
	      (point-3i0))
	      ((and x y)
	       (point-3i2 x y z))
	       (t nil)))


;; _Tp x, y, z
;; int cv_Point3i_getX(Point3i* self) 
(defcfun ("cv_Point3i_getX" point-3i-x) :int 
	 "Retrieves y coordinate of a point-3i"
	 (self cv::point-3i))


;; _Tp x, y, z
;; int cv_Point3i_getY(Point3i* self) 
(defcfun ("cv_Point3i_getY" point-3i-y) :int
  "Retrieves y coordinate of a point-3i"
  (self cv::point-3i))


;; _Tp x, y, z
;; int cv_Point3i_getZ(Point3i* self) 
(defcfun ("cv_Point3i_getZ" point-3i-z) :int
  "Retrieves z coordinate of a point-3i"
  (self cv::point-3i))


;; MatExpr* promote(Mat* m) 
(defcfun ("promote" promote) (cv::mat-expr :garbage-collect t)
	 "Converts a MAT to a MAT-EXPR."
	 (mat cv::mat))


;; uchar* Mat::ptr(int i0=0)
;; uchar* cv_Mat_ptr_index(Mat* self, int i)
(defcfun ("cv_Mat_ptr_index" %ptr) :pointer 
	 (self cv::mat)
	 (i0 :int))

(defun ptr (self &optional (i0 0))
       "Returns pointer to i0-th submatrix along the dimension #0"
       (%ptr self i0))


;; Range::Range(int _start, int _end)
;; Range* cv_create_Range(int _start, int _end) 
(defcfun ("cv_create_Range" range) (cv::range :garbage-collect t)
  "Range constructor"
  (start :int)
  (end :int))


;; static Range::Range all()
;; Range* cv_create_RangeAll()
(defcfun ("cv_create_RangeAll" range-all) (cv::range :garbage-collect t)
  "Range constructor - Returns a special variable 
   that means “the whole sequence” or “the whole 
   range”")


;; Rect_()
;; Rect* cv_create_Rect() 
(defcfun ("cv_create_Rect" rect0) (cv::rect :garbage-collect t) 
	 "RECT constructor.")


;; Rect_(_Tp _x, _Tp _y, _Tp _width, _Tp _height)
;; Rect* cv_create_Rect4(int x, int y, int width, int height) 
(defcfun ("cv_create_Rect4" rect4) (cv::rect :garbage-collect t)
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


;; Point_<_Tp> br() const
;; Point* cv_Rect_br(Rect* self) 
(defcfun ("cv_Rect_br" rect-br) (cv::point :garbage-collect t)
	 "Retrievies the bottom-right corner of a rectangle."
	 (self cv::rect))


;; Rect_(_Tp _x, _Tp _y, _Tp _width, _Tp _height)
;; _Tp x, y, width, height
;; Rect* cv_Rect_clone(Rect* self)
(defcfun ("cv_Rect_clone" rect-clone) (cv::rect :garbage-collect t)
	 (self cv::rect))


;; Size_<_Tp> size() const
;; Size* cv_Rect_size(Rect* self)  
(defcfun ("cv_Rect_size" rect-size) (cv::size :garbage-collect t) 
	 "Size (width, height) of the rectangle."
	 (self cv::rect))


;; Point_<_Tp> tl() const
;; Point* cv_Rect_tl(Rect* self) 
(defcfun ("cv_Rect_tl" rect-tl) (cv::point :garbage-collect t) 
	 "Retrievies the top-left corner of a rectangle."
	 (self cv::rect))


;; Mat Mat::reshape(int cn, int rows=0) const
;; Mat* cv_Mat_reshape(Mat* self, int cn) 
(defcfun ("cv_Mat_reshape" %reshape) (cv::mat :garbage-collect t)
	 (self cv::mat)
	 (cn :int))

;; Mat Mat::reshape(int cn, int rows=0) const
;; Mat* cv_Mat_reshape_rows(Mat* self, int cn, int rows) 
(defcfun ("cv_Mat_reshape_rows" reshape-rows) (cv::mat :garbage-collect t)
	 (self cv::mat)
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
(defcfun ("cv_Mat_get_ROI" roi) (cv::mat :garbage-collect t) 
	 "Returns matrix header corresponding to the rectangular sub-array of input MAT."
	 (self cv::mat)
	 (roi cv::rect))


;; RotatedRect(const Point2f& center, const Size2f& size, float angle)
;; RotatedRect* cv_create_RotatedRect(Point2f* center, Size2f* size, float angle)
(defcfun ("cv_create_RotatedRect" %rotated-rect) (cv::rotated-rect :garbage-collect t)
	 (center cv::point)
	 (size cv::size)
	 (angle :float))


;; Rect RotatedRect::boundingRect() const
;; Rect* cv_RotatedRect_boundingRect(RotatedRect* self)
(defcfun ("cv_RotatedRect_boundingRect" rotated-rect-bounding-rect) (cv::rotated-rect :garbage-collect t)
	 "Returns the minimal up-right rectangle containing the rotated rectangle"
	 (self cv::rotated-rect))


;; Point2f center;
;; Point* cv_RotatedRect_center(RotatedRect* self) 
(defcfun ("cv_RotatedRect_center" rotated-rect-center) (cv::point :garbage-collect t)
	 (self cv::rotated-rect))


;; Size2f size;     
;; Size* cv_RotatedRect_size(RotatedRect* self) 
(defcfun ("cv_RotatedRect_size" rotated-rect-size) (cv::size :garbage-collect t)
	 (self cv::rotated-rect))


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


;; Mat Mat::row(int y) const
;; Mat* cv_Mat_getRow(Mat* self, int y) 
(defcfun ("cv_Mat_getRow" row) (cv::mat :garbage-collect t)
  (self cv::mat)
  (y :int))


;; Mat Mat::rowRange(int startrow, int endrow) const
;; Mat* cv_Mat_getRowRange(Mat* self, int startrow, int endrow)
(defcfun ("cv_Mat_getRowRange" row-range) (cv::mat :garbage-collect t)
	 "Creates a matrix header for the specified row span."
	 (self cv::mat)
	 (startrow :int)
	 (endrow :int))


;; Scalar_<_Tp>::Scalar_(_Tp v0, _Tp v1, _Tp v2, _Tp v3)
;; Scalar* cv_create_Scalar(double val0, (double val1, double val2, double val3)
(defcfun ("cv_create_Scalar" %scalar) (cv::scalar :garbage-collect t) 
	 (val0 :double)
	 (val1 :double)
	 (val2 :double)
	 (val3 :double))


(defun scalar (&optional (val1 0d0) (val2 0d0) (val3 0d0) (val4 0d0))
       "SCALAR constructor"
       (%scalar (coerce val1 'double-float) (coerce val2 'double-float) (coerce val3 'double-float) (coerce val4 'double-float)))


;; Scalar_<_Tp> Scalar_<_Tp>::all(_Tp v0)
;; Scalar* cv_create_scalarAll(double val0123)
(defcfun ("cv_create_scalarAll" %scalar-all) (cv::scalar :garbage-collect t)
	 (val0123 :double))


(defun scalar-all (val0123)
       "SCALAR conctctor - initializes all of the scalar values 0...3 with val0123"
       (%scalar-all (coerce val0123 'double-float)))


;; Size_()
;; Size* cv_create_Size() 
(defcfun ("cv_create_Size" size0) (cv::size :garbage-collect t)
	 "Create SIZE construct")


;; Size_(_Tp _width, _Tp _height)
;; cv_create_Size2(double width, double height)
(defcfun ("cv_create_Size2" size2) (cv::size :garbage-collect t)
	 "SIZE constructor"
	 (width :double)
	 (height :double))


(defun size (&optional arg1 arg2)
       (cond ((eq (or arg1 arg2) nil)
	      (size0))
	      
	      ((numberp (or arg1 arg2)) 
	       (size2 (coerce arg1 'double-float) 
		      (coerce arg2 'double-float)))
		      
		      ((eq (type-of arg1) 'cv::cv-mat) (mat-size arg1))
		      (t nil)))


;; Size* cv_Size_assignTo(Size* self, Size* other) 
(defcfun ("cv_Size_assignTo" size-assgn-to) (cv::size :garbage-collect t)
	 "Assign data from one SIZE construct to another,
   OTHER to SELF."
   (self cv::size)
   (other cv::size))


;; Size* cv_Size_fromPoint(Point* p)
(defcfun ("cv_Size_fromPoint" size-from-point) (cv::size :garbage-collect t)
	 "Create a SIZE construct from POINT data."
	 (p cv::point))


;; Size_<float>()
;; Size2f* cv_create_Size2f(float width, float height)
(defcfun ("cv_create_Size2f" size2f0) (cv::size2f :garbage-collect t)
	 "Size2f constructor")


;; Size_<float>(float width, float height)
;; Size2f* cv_create_Size2f(float width, float height)
(defcfun ("cv_create_Size2f" size2f2) (cv::size2f :garbage-collect t)
	 "Size2f constructor"
	 (width :float)
	 (height :float))


(defun size2f (&optional (width nil) (height nil))
       (cond ((eq (or width height) nil)
	      (size2f0))
	      ((and width height)
	       (size2f2 width height))
	       (t nil)))


;; MatExpr - operator
;; MatExpr* cv_Mat_sub(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_sub" sub) (cv::mat-expr :garbage-collect t)
	 (m1 cv::mat)
	 (m2 cv::mat))


;; TermCriteria::TermCriteria(int type, int maxCount, double epsilon)
;; TermCriteria* cv_create_TermCriteria(int type, int maxCount, double epsilon) 
(defcfun ("cv_create_TermCriteria" term-criteria0) (cv::term-criteria :garbage-collect t))


;; TermCriteria::TermCriteria(int type, int maxCount, double epsilon)
;; TermCriteria* cv_create_TermCriteria(int type, int maxCount, double epsilon) 
(defcfun ("cv_create_TermCriteria3" term-criteria3) (cv::term-criteria :garbage-collect t)
	 (type :int)
	 (max-count :int)
	 (epsilon :double))


(defun term-criteria (&optional type max-count epsilon)
       (cond ((eq type nil)
	      (term-criteria0))
	      (type
	       (term-criteria3 type max-count epsilon))
	       (t nil)))


;; typedef Vec<int, 4> Vec4i;
;; Vec4i* cv_create_Vec4i()
(defcfun ("cv_create_Vec4i" vec-4i) (cv::vec-4i :garbage-collect t)
	 "Vec4i constructor")



;;; Basic Structures


;;; CORE - Operations on Arrays


;; MatExpr abs(const Mat& m)
;; MatExpr* cv_abs(Mat* m)
(defcfun ("cv_abs" *abs) (cv::mat-expr :garbage-collect t)
	 (m cv::mat))


;; MatExpr Mat::inv(int method=DECOMP_LU) const
;; MatExpr* cv_Mat_inv_mat(Mat* self, int method)
(defcfun ("cv_Mat_inv_mat" inv) (cv::mat-expr :garbage-collect t) 
	 "Inverses a matrix."
	 (self cv::mat)
	 (method :int))


;; Scalar mean(InputArray src, InputArray mask=noArray())
;; Scalar* cv_mean(Mat* src, Mat* mask)
(defcfun ("cv_mean" %mean) (cv::scalar :garbage-collect t)
	 (src cv::mat)
	 (mask cv::mat))

(defun mean (src &optional (mask (cv::mat) given-mask) return)
  "Calculates an average mean of array elements."
  (setf return (%mean src mask))
  (if given-mask nil (cv::del-mat mask))
  return)


;; RNG::RNG()
(defcfun ("cv_create_RNG" %rng) (cv::rng :garbage-collect t) 
	 "RNG constructor")


;; RNG::RNG(uint64 state)
;; RNG* cv_create_RNG_state(uint64 state)
(defcfun ("cv_create_RNG_state" rng-state) (cv::rng :garbage-collect t) 
	 "RNG constructor -  sets the RNG state to the specified value."
	 (state :uint64))


(defun rng (&optional (state nil))
       (cond ((eq state nil)
	      (%rng))
	      ((integerp state) (rng-state state))
	      (t nil)))


;; Scalar sum(InputArray src)
;; Scalar* cv_sum(Mat* src)
(defcfun ("cv_sum" sum) (cv::scalar :garbage-collect t) 
	 "Calculates the sum of array elements."
	 (src cv::mat))




;;; CORE - Drawing Functions


;; BGR value constructor macro 

(defmacro bgr (b g r)
	  "BGR value constructor macro"
	  `(scalar ,b ,g ,r))


;; RGB value constructor macro 

(defmacro rgb (r g b)
	  "BGR value constructor macro"
	  `(scalar ,b ,g ,r))


;;; CORE - Utility and System Functions and Macros



;;; IMGPROC - Macros

;; static inline Scalar morphologyDefaultBorderValue() { return Scalar::all(DBL_MAX); }
;; Scalar* cv_create_morphologyDefaultBorderValue()
(defcfun ("cv_create_morphologyDefaultBorderValue" morphology-default-border-value) (cv::scalar :garbage-collect t))


;;; IMGPROC - Image Filtering


;; Mat getStructuringElement(int shape, Size ksize, Point anchor=Point(-1,-1))
;; Mat* cv_getStructuringElement(int shape, Size* ksize, Point* anchor)
(defcfun ("cv_getStructuringElement" %get-structuring-element) (cv::mat :garbage-collect t)
	 (shape :int)
	 (ksize cv::size)
	 (kernel cv::point))

(defun get-structuring-element (shape ksize &optional (kernel (cv::point -1 -1) given-kernel) return) 
  "Returns a structuring element of the specified size and shape for morphological operations."
  (setf return (%get-structuring-element shape ksize kernel))
  (if given-kernel nil (cv::del-point kernel)) 
  return)



;;; HIGHGUI


;;; HIGHGUI - User Interface


;;; HIGHGUI - Reading and Writing Images and Video


;; VideoCapture::VideoCapture()
;; VideoCapture* cv_create_VideoCapture() 
(defcfun ("cv_create_VideoCapture" video-capture0) (cv::video-capture :garbage-collect t) 
	 "VideoCapture constructor")


;; VideoCapture::VideoCapture(int device)
;; VideoCapture* cv_create_VideoCapture1_0(int device)
(cffi:defcfun ("cv_create_VideoCapture1_0" video-capture-dev) (cv::video-capture :garbage-collect t)
	      "VideoCapture constructor"
	      (device :int))


;; VideoCapture::VideoCapture(const string& filename)
;; VideoCapture* cv_create_VideoCapture1(String* filename) 
(defcfun ("cv_create_VideoCapture1" video-capture-fn) (cv::video-capture :garbage-collect t)
	 "VideoCapture constructor"
	 (filename cv::*string))


(defun video-capture (&optional src)
       (cond ((eq src nil)
	      (video-capture0))
	      ((numberp src)
	       (video-capture-dev src))
	       ((stringp src) 
		(video-capture-fn (c-string-to-string src (length src))))
		(t nil)))

;; Mat imread(const string& filename, int flags=1)
;; mat cv_imread (const char* filename, int flags)
(defcfun ("cv_imread" %imread) (cv::mat :garbage-collect t)
	 (filename cv::*string)
	 (flags :int))

(defun imread (filename &optional (flags 1))
       (%imread (c-string-to-string filename (length filename)) flags))


;; VideoWriter* cv_create_VideoWriter() 
(defcfun ("cv_create_VideoWriter" video-writer0) (cv::video-writer :garbage-collect t)
	 "VIDEO-WRITER constructor")


;; VideoWriter::VideoWriter(const string& filename, int fourcc, double fps, Size frameSize, bool isColor) 
;; VideoWriter* cv_create_VideoWriter5(String* filename, int fourcc, double fps, Size* frameSize, bool isColor)
(defcfun ("cv_create_VideoWriter5" video-writer5) (cv::video-writer :garbage-collect t)
	 (filename cv::*string)
	 (fourcc :int)
	 (fps :double)
	 (frame-size cv::size)
	 (is-color :boolean))

(defun video-writer (&optional filename fourcc fps frame-size (is-color t))
       "VIDEO-WRITER constructor"  
       (cond ((eq filename nil)
	      (video-writer0))
	      (filename
	       (video-writer5 (c-string-to-string filename (length filename)) fourcc fps frame-size is-color))
	       (t nil)))


;;; HIGHGUI - Qt New Functions


;;; FEATURES2D


;;; FEATURES2D - Feature Detection and Description


;; BFMatcher::BFMatcher(int normType=NORM_L2, bool crossCheck=false )
;; BFMatcher* cv_create_BFMatcher(int normType, bool crossCheck) 
(defcfun ("cv_create_BFMatcher" %bf-matcher) (cv::feature-2d :garbage-collect t)
	 (norm-type :int)
	 (cross-check :boolean))

(defun bf-matcher (&optional (norm-type cv::+norm-l2+) (cross-check nil))
       "Brute-force matcher constructor."
       (%bf-matcher norm-type cross-check))


;; BRISK::BRISK(int thresh=30, int octaves=3, float patternScale=1.0f)
;; BRISK* cv_create_BRISK(int thresh, int octaves, float patternScale)
(defcfun ("cv_create_BRISK" %brisk) (cv::feature-2d :garbage-collect t)
	 (thresh :int)
	 (octaves :int)
	 (pattern-scale :float))


(defun brisk (&optional (thresh 30) (octaves 3) (pattern-scale 1.0f0))
       "The BRISK constructor"
       (%brisk thresh octaves pattern-scale))


;;; FEATURES2D - Common Interfaces of Feature Detectors


;;; FEATURES2D - Common Interfaces of Descriptor Extractors


;;; FEATURES2D - Common Interfaces of Descriptor Matchers


;;; OBJDETECT


;;; OBJDETECT - Cascade Classification


;; CascadeClassifier::CascadeClassifier()
;; CascadeClassifier* cv_create_CascadeClassifier() 
(defcfun ("cv_create_CascadeClassifier" cascade-classifier0) (cv::cascade-classifier :garbage-collect t)
	 "CASCADE-CLASSIFIER construct.")

;; CascadeClassifier::CascadeClassifier(const string& filename)
;; CascadeClassifier* cv_create_CascadeClassifier1(String* filename)
(defcfun ("cv_create_CascadeClassifier1" cascade-classifier1) (cv::cascade-classifier :garbage-collect t)
	 "Loads a classifier from a file."
	 (filename cv::*string))

(defun cascade-classifier (&optional filename)
       (cond ((eq filename nil)
	      (cascade-classifier0))
	      (filename
	       (cascade-classifier1 (c-string-to-string filename (length filename))))
	       (t nil)))


;;; ML


;;; ML - Normal Bayes Classifier


;; CvNormalBayesClassifier::CvNormalBayesClassifier()
;; CvNormalBayesClassifier* cv_create_CvNormalBayesClassifier()
(defcfun ("cv_create_CvNormalBayesClassifier" %normal-bayes-classifier) (cv::normal-bayes-classifier :garbage-collect t))

;; CvNormalBayesClassifier::CvNormalBayesClassifier(const Mat& trainData, const Mat& responses, const Mat& varIdx=Mat(), 
;;                                                  const Mat& sampleIdx=Mat() )
;; CvNormalBayesClassifier* cv_create_CvNormalBayesClassifier4(Mat* trainData, Mat* responses, Mat* varIdx, Mat* sampleIdx) 
(defcfun ("cv_create_CvNormalBayesClassifier4" normal-bayes-classifier4) (cv::normal-bayes-classifier :garbage-collect t)
  (train-data cv::mat)
  (responses cv::mat)
  (var-idx cv::mat)
  (sample-idx cv::mat))

(defun normal-bayes-classifier (&optional train-data responses  (var-idx (cv::mat) given-var-idx) (sample-idx (cv::mat) given-sample-idx))
  (let ((return (if train-data
		    (normal-bayes-classifier4 train-data responses  var-idx sample-idx)
		    (%normal-bayes-classifier))))
    (if given-var-idx nil (cv::del-mat var-idx))
    (if given-sample-idx nil (cv::del-mat sample-idx)) 
    return))


;;; ML - K-Nearest Neighbors



;; CvKNearest::CvKNearest()
;; CvKNearest* cv_create_CvKNearest() 
(defcfun ("cv_create_CvKNearest" %k-nearest) (cv::k-nearest :garbage-collect t))

;; CvKNearest::CvKNearest(const CvMat* trainData, const CvMat* responses, const CvMat* sampleIdx=0, bool isRegression=false, 
;;                        int max_k=32 )
;; CvKNearest* cv_create_CvKNearest5(Mat* trainData, Mat* responses, Mat* sampleIdx, bool isRegression, int max_k) 
(defcfun ("cv_create_CvKNearest5" k-nearest5) (cv::k-nearest :garbage-collect t)
  (train-data cv::mat)
  (responses cv::mat)
  (sample-idx cv::mat)
  (is-regression :boolean)
  (max-k :int))

(defun k-nearest (&optional train-data responses (sample-idx (null-pointer)) (is-regression nil) (max-k 32))
  (if train-data
      (k-nearest5 train-data responses sample-idx is-regression max-k)
      (%k-nearest)))


;;; ML - Decision Trees


;; CvDTree:CvDTree()
;; CvDTree* cv_create_CvDTree() 
(defcfun ("cv_create_CvDTree" d-tree) (cv::d-tree :garbage-collect t))


;; CvDTreeParams::CvDTreeParams()
;; CvDTreeParams* cv_create_CvDTreeParams()
(defcfun ("cv_create_CvDTreeParams" %d-tree-params) (cv::d-tree-params :garbage-collect t))

;; CvDTreeParams::CvDTreeParams(int max_depth, int min_sample_count, float regression_accuracy, bool use_surrogates, 
;;                              int max_categories, int cv_folds, bool use_1se_rule, bool truncate_pruned_tree, const float* priors)
;; CvDTreeParams* cv_create_CvDTreeParams9(int max_depth, int min_sample_count, float regression_accuracy, bool use_surrogates, 
;;                                         int max_categories, int cv_folds, bool use_1se_rule, bool truncate_pruned_tree, 
;;                                         const float* priors)
(defcfun ("cv_create_CvDTreeParams9" d-tree-params9) (cv::d-tree-params :garbage-collect t)
  (max-depth :int)
  (min-sample-count :int)
  (regression-accuracy :float)
  (use-surrogates :boolean)
  (max-categories :int)
  (folds :int)
  (use-1se-rule :boolean)
  (truncate-pruned-tree :boolean)
  (priors :pointer))

(defun d-tree-params (&optional max-depth min-sample-count regression-accuracy use-surrogates max-categories folds use-1se-rule
			truncate-pruned-tree priors)
  (if max-depth
      (d-tree-params9 max-depth min-sample-count regression-accuracy use-surrogates max-categories folds use-1se-rule
                      truncate-pruned-tree priors)
      (%d-tree-params)))



;;; ML - Neural Networks


;; CvANN_MLP::CvANN_MLP()
;; CvANN_MLP* cv_create_CvANN_MLP()
(defcfun ("cv_create_CvANN_MLP" %ann-mlp) (cv::ann-mlp :garbage-collect t))

;; CvANN_MLP::CvANN_MLP(const CvMat* layerSizes, int activateFunc=CvANN_MLP::SIGMOID_SYM, double fparam1=0, double fparam2=0 )
;; CvANN_MLP* cv_create_CvANN_MLP4(Mat* layerSizes, int activateFunc, double fparam1, double fparam2) 
(defcfun ("cv_create_CvANN_MLP4" ann-mlp4) (cv::ann-mlp :garbage-collect t)
  (layer-sizes cv::mat)
  (activate-func :int)
  (fparam1 :double)
  (fparam2 :double))

(defun ann-mlp (&optional layer-sizes (activate-func cv::+ann-mlp-sigmoid-sym+) (fparam1 0d0) (fparam2 0d0))
  (if layer-sizes
      (ann-mlp4 layer-sizes activate-func fparam1 fparam2)
      (%ann-mlp)))


;; CvANN_MLP_TrainParams::CvANN_MLP_TrainParams()
;; CvANN_MLP_TrainParams* cv_create_CvANN_MLP_TrainParams()
(defcfun ("cv_create_CvANN_MLP_TrainParams" %ann-mlp-train-params) (cv::ann-mlp-train-params :garbage-collect t))

;; CvANN_MLP_TrainParams::CvANN_MLP_TrainParams(CvTermCriteria term_crit, int train_method, double param1, double param2=0 )
;; CvANN_MLP_TrainParams* cv_create_CvANN_MLP_TrainParams4(TermCriteria* term_crit, int train_method, double param1, double param2)
(defcfun ("cv_create_CvANN_MLP_TrainParams" ann-mlp-train-params4) (cv::ann-mlp-train-params :garbage-collect t)
  (term-crit cv::term-criteria)
  (train-method :int)
  (param1 :double)
  (param2 :double))

(defun ann-mlp-train-params (&optional term-crit train-method param1 (param2 0))
       (if term-crit
	   (ann-mlp-train-params4 term-crit train-method param1 param2)
	   (%ann-mlp-train-params)))



;;; NON-FREE - Feature Detection and Description


;; SURF::SURF()
;; SURF* cv_create_SURF() 
(defcfun ("cv_create_SURF" surf0) (cv::feature-2d :garbage-collect t))


;; SURF::SURF(double hessianThreshold, int nOctaves=4, int nOctaveLayers=2, bool extended=true, bool upright=false )
;; SURF* cv_create_SURF5(double hessianThreshold, int nOctaves, int nOctaveLayers, bool extended, bool upright)
(defcfun ("cv_create_SURF5" surf5) (cv::feature-2d :garbage-collect t)
	 (hessian-threshold :double)
	 (n-octaves :int)
	 (n-octave-layers :int)
	 (extended :boolean)
	 (upright :boolean))


(defun surf (&optional hessian-threshold (n-octaves 4) (n-octave-layers 2) (extended t) (upright nil))
       (cond ((eq hessian-threshold nil)
	      (surf0))
	      (hessian-threshold
	       (surf5 hessian-threshold n-octaves n-octave-layers extended upright))
	       (t nil)))


;;; VECTORS


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorc" %vec-char) (cv::vector-char :garbage-collect t))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorc" c-arr-to-vec-char) (cv::vector-char :garbage-collect t)
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-char (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :char :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-char (cdr previous) (length a))))


(defmacro vec-char (&rest args)
  (if (fourth args)
      (error "odd number of args to VEC-CHAR")
      nil)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let ((,x (first (list ,@args)))
	   (,y (second (list ,@args)))
           (,z (third (list ,@args))))
       (cond ((eq ,x nil)
	      (%vec-char))
	     ((or (vectorp ,x) (listp ,x))
	      (arr-to-vec-char ,x))
	     ((eq :length ,x)
	      (cv::vec-char-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (cv::vec-char-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (cv::vec-char-to-lisp-vec ,y ,z))
	     ((typep ,x 'cv::std-vector-char)
	      (if (eq ,y nil)
		  (mem-aref (cv::vec-char-to-c-arr ,x) :char) 
		  (mem-aref (cv::vec-char-to-c-arr ,x) :char ,y)))
	     (t (error "incorrect input. 
                   ~%See VEC-CHAR documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectordm" %vec-dmatch) (cv::vector-dmatch :garbage-collect t))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectordm" c-arr-to-vec-dmatch) (cv::vector-dmatch :garbage-collect t)
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-dmatch (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv::c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-dmatch (cdr previous) (length a))))


(defmacro vec-dmatch (&rest args)
  (if (fifth args)
      (error "odd number of args to VECTOR-DMATCH")
      nil)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym))
        (type (gensym)))
    `(let ((,x (first (list ,@args)))
	   (,y (second (list ,@args)))
           (,z (third (list ,@args)))
           (,type (fourth (list ,@args))))
       (cond ((eq ,x nil)
	      (%vec-dmatch))
	     ((or (vectorp ,x) (listp ,x))
	      (arr-to-vec-dmatch ,x))
	     ((eq :length ,x)
	      (cv::vec-dmatch-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (cv::vec-dmatch-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (cv::vec-dmatch-to-lisp-vec ,y))
	     ((and (typep ,x 'cv::std-vector-dmatch) ,y)
	      (if (eq ,z nil)
		  (mem-aref (cv::vec-dmatch-to-c-arr ,x) 'cv::dmatch ,y)
		  (if (null ,type)
		      (mem-aref (cv::c-pointer (mem-aref (cv::vec-dmatch-to-c-arr ,x) 'cv::dmatch ,y)) :int ,z)
		      (mem-aref (cv::c-pointer (mem-aref (cv::vec-dmatch-to-c-arr ,x) 'cv::dmatch ,y)) ,type ,z)))) 
	     (t (error "incorrect input. 
  ~%See VEC-DMATCH documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectord" %vec-double) (cv::vector-double :garbage-collect t))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectord" c-arr-to-vec-double) (cv::vector-double :garbage-collect t)
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-double (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :double :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-double (cdr previous) (length a))))


(defmacro vec-double (&rest args)
  (if (third args)
      (error "odd number of args to VEC-DOUBLE")
      nil)
  (let ((x (gensym))
        (y (gensym)))
    `(let ((,x (first (list ,@args)))
	   (,y (second (list ,@args))))
       (cond ((eq ,x nil)
	      (%vec-double))
	     ((or (vectorp ,x) (listp ,x))
	      (arr-to-vec-double ,x))
	     ((eq :length ,x)
	      (cv::vec-double-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (cv::vec-double-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (cv::vec-double-to-lisp-vec ,y))
	     ((typep ,x 'cv::std-vector-double)
	      (if (eq ,y nil)
		  (mem-aref (cv::vec-double-to-c-arr ,x) :double) 
		  (mem-aref (cv::vec-double-to-c-arr ,x) :double ,y)))
	     (t (error "incorrect input. 
                   ~%See VEC-DOUBLE documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorf" %vec-float) (cv::vector-float :garbage-collect t))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorf" c-arr-to-vec-float) (cv::vector-float :garbage-collect t)
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-float (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :float :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-float (cdr previous) (length a))))


(defmacro vec-float (&rest args)
  (if (third args)
      (error "odd number of args to VEC-FLOAT")
      nil)
  (let ((x (gensym))
        (y (gensym)))
    `(let ((,x (first (list ,@args)))
	   (,y (second (list ,@args))))
       (cond ((eq ,x nil)
	      (%vec-float))
	     ((or (vectorp ,x) (listp ,x))
	      (arr-to-vec-float ,x))
	     ((eq :length ,x)
	      (cv::vec-float-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (cv::vec-float-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (cv::vec-float-to-lisp-vec ,y))
	     ((typep ,x 'cv::std-vector-float)
	      (if (eq ,y nil)
		  (mem-aref (cv::vec-float-to-c-arr ,x) :float) 
		  (mem-aref (cv::vec-float-to-c-arr ,x) :float ,y)))
	     (t (error "incorrect input. 
                   ~%See VEC-FLOAT documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectori" %vec-int) (cv::vector-int :garbage-collect t))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectori" c-arr-to-vec-int) (cv::vector-int :garbage-collect t)
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-int (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :int :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-int (cdr previous) (length a))))


(defmacro vec-int (&rest args)
  (if (third args)
      (error "odd number of args to VEC-INT")
      nil)
  (let ((x (gensym))
        (y (gensym)))
    `(let ((,x (first (list ,@args)))
	   (,y (second (list ,@args))))
       (cond ((eq ,x nil)
	      (%vec-int))
	     ((or (vectorp ,x) (listp ,x))
	      (arr-to-vec-int ,x))
	     ((eq :length ,x)
	      (cv::vec-int-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (cv::vec-int-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (cv::vec-int-to-lisp-vec ,y))
	     ((typep ,x 'cv::std-vector-int)
	      (if (eq ,y nil)
		  (mem-aref (cv::vec-int-to-c-arr ,x) :int) 
		  (mem-aref (cv::vec-int-to-c-arr ,x) :int ,y)))
	     (t (error "incorrect input. 
                   ~%See VEC-INT documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorkp" %vec-key-point) (cv::vector-key-point :garbage-collect t))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorkp" c-arr-to-vec-key-point) (cv::vector-key-point :garbage-collect t)
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-key-point (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv::c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-key-point (cdr previous) (length a))))


(defmacro vec-key-point (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-KEY-POINT")
      nil)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let ((,x (first (list ,@args)))
	   (,y (second (list ,@args)))
           (,z (third (list ,@args))))
       (cond ((eq ,x nil)
	      (%vec-key-point))
	     ((or (vectorp ,x) (listp ,x))
	      (arr-to-vec-key-point ,x))
	     ((eq :length ,x)
	      (cv::vec-key-point-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (cv::vec-key-point-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (cv::vec-key-point-to-lisp-vec ,y))
	     ((and (typep ,x 'cv::std-vector-key-point) ,y)
	      (if (eq ,z nil)
		  (mem-aref (cv::vec-key-point-to-c-arr ,x) 'cv::key-point ,y)
		  (if (< ,z 5)  (mem-aref (cv::c-pointer 
					   (mem-aref (cv::vec-key-point-to-c-arr ,x) 
						     'cv::key-point ,y)) :float ,z)
		      (mem-aref (cv::c-pointer 
				 (mem-aref (cv::vec-key-point-to-c-arr ,x) 'cv::key-point ,y)) :int ,z))))
	     (t (error "incorrect input. 
  ~%See VEC-KEY-POINT documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorp" %vec-point) (cv::vector-point :garbage-collect t))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorp" c-arr-to-vec-point) (cv::vector-point :garbage-collect t)
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-point (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv::c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-point (cdr previous) (length a))))


(defmacro vec-point (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-POINT")
      nil)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let ((,x (first (list ,@args)))
	   (,y (second (list ,@args)))
           (,z (third (list ,@args))))
       (cond ((eq ,x nil)
	      (%vec-point))
	     ((or (vectorp ,x) (listp ,x))
	      (arr-to-vec-point ,x))
	     ((eq :length ,x)
	      (cv::vec-point-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (cv::vec-point-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (cv::vec-point-to-lisp-vec ,y))
	     ((and (typep ,x 'cv::std-vector-point) ,y)
	      (if (eq ,z nil)
		  (mem-aref (cv::vec-point-to-c-arr ,x) 'cv::point ,y)
		  (mem-aref (cv::c-pointer (mem-aref (cv::vec-point-to-c-arr ,x) 'cv::point ,y)) :int ,z)))
	     (t (error "incorrect input. 
  ~%See VEC-POINT documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorp2f" %vec-point-2f) (cv::vector-point-2f :garbage-collect t))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorp2f" c-arr-to-vec-point-2f) (cv::vector-point-2f :garbage-collect t)
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-point-2f (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv::c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-point-2f (cdr previous) (length a))))


(defmacro vec-point-2f (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-POINT-2F")
      nil)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let ((,x (first (list ,@args)))
	   (,y (second (list ,@args)))
           (,z (third (list ,@args))))
       (cond ((eq ,x nil)
	      (%vec-point-2f))
	     ((or (vectorp ,x) (listp ,x))
	      (arr-to-vec-point-2f ,x))
	     ((eq :length ,x)
	      (cv::vec-point-2f-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (cv::vec-point-2f-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (cv::vec-point-2f-to-lisp-vec ,y))
	     ((and (typep ,x 'cv::std-vector-point-2f) ,y)
	      (if (eq ,z nil)
		  (mem-aref (cv::vec-point-2f-to-c-arr ,x) 'cv::point-2f ,y)
		  (mem-aref (cv::c-pointer (mem-aref (cv::vec-point-2f-to-c-arr ,x) 'cv::point-2f ,y)) :float ,z)))
	     (t (error "incorrect input. 
  ~%See VEC-POINT-2F documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorr" %vec-rect) (cv::vector-rect :garbage-collect t))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorr" c-arr-to-vec-rect) (cv::vector-rect :garbage-collect t)
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-rect (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv::c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-rect (cdr previous) (length a))))


(defmacro vec-rect (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-RECT")
      nil)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let ((,x (first (list ,@args)))
	   (,y (second (list ,@args)))
           (,z (third (list ,@args))))
       (cond ((eq ,x nil)
	      (%vec-rect))
	     ((or (vectorp ,x) (listp ,x))
	      (arr-to-vec-rect ,x))
	     ((eq :length ,x)
	      (cv::vec-rect-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (cv::vec-rect-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (cv::vec-rect-to-lisp-vec ,y))
	     ((and (typep ,x 'cv::std-vector-rect) ,y)
	      (if (eq ,z nil)
		  (mem-aref (cv::vec-rect-to-c-arr ,x) 'cv::rect ,y)
		  (mem-aref (cv::c-pointer (mem-aref (cv::vec-rect-to-c-arr ,x) 'cv::rect ,y)) :int ,z)))
	     (t (error "incorrect input. 
  ~%See VEC-RECT documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv4i" %vec-vec-4i) (cv::vector-vec-4i :garbage-collect t))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv4i" c-arr-to-vec-vec-4i) (cv::vector-vec-4i :garbage-collect t)
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-4i (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv::c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-4i (cdr previous) (length a))))



(defmacro vec-vec-4i (&rest args)
  (if (fourth args)
      (error "odd number of args to VEC-VEC-4I")
      nil)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let ((,x (first (list ,@args)))
	   (,y (second (list ,@args)))
           (,z (third (list ,@args))))
       (cond ((eq ,x nil)
	      (%vec-vec-4i))
	     ((or (vectorp ,x) (listp ,x))
	      (arr-to-vec-vec-4i ,x))
	     ((eq :length ,x)
	      (cv::vec-vec-4i-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (cv::vec-vec-4i-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (cv::vec-vec-4i-to-lisp-vec ,y))
	     ((and (typep ,x 'cv::std-vector-vec-4i) ,y)
	      (if (eq ,z nil)
		  (mem-aref (cv::vec-vec-4i-to-c-arr ,x) 'cv::vec-4i ,y)
		  (mem-aref (cv::c-pointer (mem-aref (cv::vec-vec-4i-to-c-arr ,x) 'cv::vec-4i ,y)) :int ,z)))
	     (t (error "incorrect input. 
  ~%See VEC-VEC-4I documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%"))))))


