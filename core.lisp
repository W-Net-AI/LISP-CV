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



;; C-Interop - String*


;; Regular version 

;; string* std_cstringToString(char* s, size_t len) 
(defcfun ("cstring_to_std_string" c-string-to-string) *string
  "Converts C string to C++"
  (s :string)
  (len :unsigned-int))

;; Version for internal GC in the LISP-CV package

;; string* std_cstringToString(char* s, size_t len) 
(defcfun ("cstring_to_std_string" %c-string-to-string) (cv::*string :garbage-collect t)
  "Converts C string to C++"
  (s :string)
  (len :unsigned-int))


;;; DEFGENERIC (*defgeneric functions are placed here so the whole file can use them*)


(defgeneric dot (self other)
  (:documentation "Used for all bindings with a dot member"))

(defgeneric size (arg &rest args)
  (:documentation "Used for all bindings with a size member."))

(defgeneric clone (self)
  (:documentation "Used for all bindings with a clone member."))


;;; Basic Structures


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


;; int cv_Mat_channels(Mat* self)
(defcfun ("cv_Mat_channels" channels) :int
  (self mat))


;; Mat Mat::clone() const
;; Mat* cv_Mat_clone(Mat* self) 
(defcfun ("cv_Mat_clone" clone-mat) mat
  (self mat))

(defmethod clone ((self cv-mat))
  "Creates a full copy of the array and the underlying data."
  (clone-mat self))


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


(defmethod dot ((self cv-point-2d) (other cv-point-2d))
  (dot-2d self other))

(defmethod dot ((self cv-point-2f) (other cv-point-2f))
  (dot-2f self other))

(defmethod dot ((self cv-point) (other cv-point))
  (dot-2i self other))

(defmethod dot ((self cv-point-3d) (other cv-point-3d))
  (dot-3d self other))

(defmethod dot ((self cv-point-3f) (other cv-point-3f))
  (dot-3f self other))

(defmethod dot ((self cv-point-3i) (other cv-point-3i))
  (dot-3i self other))


;; bool Mat::empty() const
;; int cv_Mat_empty(Mat* self) 
(defcfun ("cv_Mat_empty" mat-empty) :boolean
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
(defcfun ("cv_Size2f_height" height-2f) :float
  "Gets the height of a SIZE-2F"
  (self size-2f))


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


(defun key-point (&optional x y size (angle -1) (response 0) (octave 0) (class-id -1))
       (cond ((eq x nil)
	      (key-point-0))
	      (x
	       (key-point-7 x y size angle response octave class-id))
	       (t nil)))


(defun make-key-point (&optional x y size (angle -1) (response 0) (octave 0) (class-id -1))
       (cond ((eq x nil)
	      (key-point-0))
	      (x
	       (key-point-7 x y size angle response octave class-id))
	       (t nil)))

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


(let ((previous nil))
     (defun make-mat-data (rows cols type &rest args)
	    (unless (equal (second args) (car previous))
		    (setf previous (cons (second args) (foreign-alloc (first args) 
								      :initial-contents (second args)))))
								      (mat-data rows cols type (cdr previous))))


;; double Mat::dot(InputArray m) const
;; double cv_Mat_dot(Mat* self, Mat* m)
(defcfun ("cv_Mat_dot" mat-dot) :double
  (self mat)
  (other mat))

(defmethod dot ((self cv-mat) (other cv-mat))
  (mat-dot self other))


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


;; Mat::Mat(const Mat& m, const Range& rowRange, const Range& colRange=Range::all() )
;; Mat* cv_Mat_get_Range(Mat* self, Range* rowRange, Range* colRange)
(defcfun ("cv_Mat_with_Range" mat-range) mat
  "MAT constructor with Range parameters."
  (self mat)
  (row-range range)
  (col-range range))


(defun make-mat-range (self row-range &optional (col-range (range-all) given-col-range))
  (let ((return (mat-range self row-range col-range)))
    (if given-col-range nil (del-range col-range))
    return))


;; Size Mat::size() const
;; Size* cv_Mat_size(Mat* self)
(defcfun ("cv_Mat_size" mat-size) size
  "Returns a matrix size."
  (self mat))

(defmethod size ((arg cv-mat) &rest args)
  args
  (mat-size arg))


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


;; Mat::Mat(int rows, int cols, int type)
;; Mat* cv_create_Mat_typed(int rows, int cols, int type)
(defcfun ("cv_create_Mat_typed" make-mat-typed) mat
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


(let ((previous nil))
     (defun make-mat-value (rows cols type s)
	    (unless (equal s (car previous))
		    (setf previous (cons s (scalar (if (first s) (first s) 0) 
						   (if (second s) (second s) 0) 
						   (if (third s) (third s) 0) 
						   (if (fourth s) (fourth s) 0)))))
						   (mat-value rows cols type (cdr previous))))


;;; MAT


(defun mat (&rest args)
  
  "MAT constructor"  
  
  (cond ((eq (first args) nil) (%mat))

	((typep (second args) 'cv-range)
	 (apply #'make-mat-range args))
	
	((and (eq (fourth args) nil) (first args)) 
	 (apply #'mat-typed args))
	
	((typep (fourth args) 'cv-scalar)
	 (apply #'mat-value args))
	
	((listp (fourth args))
	 (apply #'make-mat-value args))
	
	((pointerp (fourth args))
	 (apply #'mat-data args))
	
	((listp (fifth args))
	 (apply #'make-mat-data args))
	
	(t nil)))


(defun make-mat (&rest args)
  
  "MAT constructor"  
  
  (cond ((eq (first args) nil) (%mat))

	((typep (second args) 'cv-range)
	 (apply #'make-mat-range args))
	
	((and (eq (fourth args) nil) (first args)) 
	 (apply #'mat-typed args))
	
	((typep (fourth args) 'cv-scalar)
	 (apply #'mat-value args))
	
	((listp (fourth args))
	 (apply #'make-mat-value args))
	
	((pointerp (fourth args))
	 (apply #'mat-data args))
	
	((listp (fifth args))
	 (apply #'make-mat-data args))
	
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


;; Point_()
;; Point2##t * cv_create_Point2##t ( tn x,  tn y)
(defcfun ("cv_create_Point2i" point-0) point)

;; Point_(_Tp _x, _Tp _y)
;; Point2##t * cv_create_Point2##t ( tn x,  tn y) 
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
	 "Retrieves X coordinate of a point"
	 (self point))


;; _Tp x, y
;; int cv_Point_getY(Point* self)
(defcfun ("cv_Point2i_getY" point-y) :int 
	 "Retrieves y coordinate of a point"
	 (self point))


;; typedef Point_<double> Point2d
;; Point2##t * cv_create_Point2##t ( tn x,  tn y) 
(defcfun ("cv_create_Point2d" point-2d-0) point-2d 
	 "Point2d constructor")

;; typedef Point_<double> Point2d
;; Point2##t * cv_create_Point2##t ( tn x,  tn y)  
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
  "Retrieves x coordinate of a point-2d"
  (self point-2d))


;; _Tp x, y
;; double cv_Point2d_getY(Point2d* self) 
(defcfun ("cv_Point2d_getY" point-2d-y) :double
  "Retrieves y coordinate of a point-2d"
  (self point-2d))


;; typedef Point_<float> Point2f
;; tn cv_Point2##t##_getX( Point2##t * self) 
(defcfun ("cv_create_Point2f" point-2f-0) point-2f 
	 "Point2f constructor")

;; typedef Point_<float> Point2f
;; Point2##t * cv_create_Point2##t ( tn x,  tn y)  
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
  "Retrieves x coordinate of a point-2f"
  (self point-2f))


;; _Tp x, y;wordpress how to I make multiple pages private
;; float cv_Point2f_getY(Point2f* self) 
(defcfun ("cv_Point2f_getY" point-2f-y) :float
  "Retrieves y coordinate of a point-2f"
  (self point-2f))


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z) 
(defcfun ("cv_create_Point3d" point-3d-0) point-3d 
	 "Point3d constructotr")

;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z) 
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
  "Retrieves x coordinate of a point-3d"
  (self point-3d))


;; _Tp x, y, z
;; double cv_Point3d_getY(Point3d* self) 
(defcfun ("cv_Point3d_getY" point-3d-y) :double
  "Retrieves y coordinate of a point-3d"
  (self point-3d))


;; _Tp x, y, z
;; double cv_Point3d_getZ(Point3d* self) 
(defcfun ("cv_Point3d_getZ" point-3d-z) :double
  "Retrieves z coordinate of a point-3d"
  (self point-3d))


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3f" point-3f-0) point-3f 
	 "Point3f constructor")

;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
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
  "Retrieves x coordinate of a point-3f"
  (self point-3f))


;; _Tp x, y, z
;; float cv_Point3f_getY(Point3f* self) 
(defcfun ("cv_Point3f_getY" point-3f-y) :float
  "Retrieves y coordinate of a point-3f"
  (self point-3f))


;; _Tp x, y, z
;; float cv_Point3f_getZ(Point3f* self) 
(defcfun ("cv_Point3f_getZ" point-3f-z) :float
  "Retrieves z coordinate of a point-3f"
  (self point-3f))


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3i" point-3i-0) point-3i 
	 "Point3i constructor")

;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
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
	 "Retrieves y coordinate of a point-3i"
	 (self point-3i))


;; _Tp x, y, z
;; int cv_Point3i_getY(Point3i* self) 
(defcfun ("cv_Point3i_getY" point-3i-y) :int
  "Retrieves y coordinate of a point-3i"
  (self point-3i))


;; _Tp x, y, z
;; int cv_Point3i_getZ(Point3i* self) 
(defcfun ("cv_Point3i_getZ" point-3i-z) :int
  "Retrieves z coordinate of a point-3i"
  (self point-3i))


(defun print-mat (mat type &key to-file)

  (cond (to-file
         (let ((*print-case* :downcase))
           (with-open-file (str (cat *lisp-cv-src-dir* 
                                     "/data/" 
                                     (write-to-string to-file))
				:direction :output
				:if-exists :supersede
				:if-does-not-exist :create)
	     (dotimes (i (rows mat))
	       (dotimes (j (cols mat))
		 (format str "~a" (at mat i j type))
		 (format str " "))
	       (format str "~%")))))
        
        ((eq to-file nil)
         (dotimes (i (rows mat))
           (dotimes (j (cols mat))
             (format t "~a" (at mat i j type))
             (princ #\Space))
           (princ #\Newline)))
        
        (t nil)))


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

(defmethod size ((arg cv-range) &rest args)
  args
  (range-size arg))


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
(defcfun ("cv_Rect_br" rect-br) point 
	 "Retrievies the bottom-right corner of a rectangle."
	 (self rect))


;; Rect_(_Tp _x, _Tp _y, _Tp _width, _Tp _height)
;; _Tp x, y, width, height
;; Rect* cv_Rect_clone(Rect* self)
(defcfun ("cv_Rect_clone" clone-rect) rect
  (self rect))

(defmethod clone ((self cv-rect))
  "Creates a full copy of a RECT object"
  (clone-rect self))


;; _Tp x, y, width, height
;; int &cv_Rect_getHeight(Rect* self)
(defcfun ("cv_Rect_getHeight" rect-height) :int
  (self rect))


;; Size_<_Tp> size() const
;; Size* cv_Rect_size(Rect* self)  
(defcfun ("cv_Rect_size" rect-size) size 
	 "Size (width, height) of the rectangle."
	 (self rect))

(defmethod size ((arg cv-rect) &rest args)
  args
  (rect-size arg))


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
(defcfun ("cv_create_RotatedRect" make-rotated-rect) rotated-rect
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
	 (make-rotated-rect (first args) (second args) (third args)))
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


;; Mat Mat::row(int y) const
;; Mat* cv_Mat_getRow(Mat* self, int y) 
(defcfun ("cv_Mat_getRow" row) mat
  (self mat)
  (y :int))


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

(defun scalar (&optional (val1 0d0) (val2 0d0) (val3 0d0) (val4 0d0))
       "SCALAR constructor"
	      (%scalar (coerce val1 'double-float) (coerce val2 'double-float) (coerce val3 'double-float) (coerce val4 'double-float)))

(defun make-scalar (&optional (val1 0d0) (val2 0d0) (val3 0d0) (val4 0d0))
       "SCALAR constructor"
	      (%scalar (coerce val1 'double-float) (coerce val2 'double-float) (coerce val3 'double-float) (coerce val4 'double-float)))


;; Scalar_<_Tp> Scalar_<_Tp>::all(_Tp v0)
;; Scalar* cv_create_scalarAll(double val0123)
(defcfun ("cv_create_scalarAll" %scalar-all) scalar
  (val0123 :double))

(defun scalar-all (val0123)
       "SCALAR conctctor - Initializes all of 
        the scalar values 0...3 with val0123"
       (%scalar-all (coerce val0123 'double-float)))

(defun make-scalar-all (val0123)
       "SCALAR conctctor - Initializes all of 
        the scalar values 0...3 with val0123"
       (%scalar-all (coerce val0123 'double-float)))


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

(defmethod size ((arg null) &rest args)
  args
  (if (eq arg nil) (size-0) nil))

(defmethod size ((arg real) &rest args)
  (size-2 (coerce arg 'double-float) (coerce (or (first args) 0) 'double-float)))

(defun make-size (&optional arg1 arg2)
  (cond ((null arg1)
	 (size-0))
	
	((numberp arg1) 
	 (size-2 (coerce arg1 'double-float) 
		(coerce arg2 'double-float)))

	(t nil)))


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


;; size_t cv_Mat_get_Step(Mat* self) 
(defcfun ("cv_Mat_get_Step" *step) :unsigned-int
  "Used to compute address of a matrix element"
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


;; typedef Vec<uchar, 2> Vec2b;
;; Vec2b* cv_create_Vec2b()
(defcfun ("cv_create_Vec2b" vec-2b-0) vec-2b)

;; typedef Vec<uchar, 2> Vec2b;
;; Vec2b* cv_create_Vec2b_2(uchar val0, uchar val1)
(defcfun ("cv_create_Vec2b_2" vec-2b-2) vec-2b
  (val0 :uchar)
  (val1 :uchar))

(defun vec-2b (&optional val0 val1)
  "VEC-2B constructor"
  (cond ((null val0)
	 (vec-2b-0))
	(val0
	 (vec-2b-2 val0 val1))
	(t nil)))

(defun make-vec-2b (&optional val0 val1)
  "VEC-2B constructor"
  (cond ((null val0)
	 (vec-2b-0))
	(val0
	 (vec-2b-2 val0 val1))
	(t nil)))


;; typedef Vec<uchar, 3> Vec3b;
;; Vec3b* cv_create_Vec3b()
(defcfun ("cv_create_Vec3b" vec-3b-0) vec-3b)

;; typedef Vec<uchar, 3> Vec3b;
;; Vec3b* cv_create_Vec3b_3(uchar val0, uchar val1, uchar val2)
(defcfun ("cv_create_Vec3b_3" vec-3b-3) vec-3b
  (val0 :uchar)
  (val1 :uchar)
  (val2 :uchar))

(defun vec-3b (&optional val0 val1 val2)
  "VEC-3B constructor"
  (cond ((null val0)
	 (vec-3b-0))
	(val0
	 (vec-3b-3 val0 val1 val2))
	(t nil)))

(defun make-vec-3b (&optional val0 val1 val2)
  "VEC-3B constructor"
  (cond ((null val0)
	 (vec-3b-0))
	(val0
	 (vec-3b-3 val0 val1 val2))
	(t nil)))


;; typedef Vec<uchar, 4> Vec4b;
;; Vec4b* cv_create_Vec4b()
(defcfun ("cv_create_Vec4b" vec-4b-0) vec-4b)

;; typedef Vec<uchar, 4> Vec4b;
;; Vec4b* cv_create_Vec4b_4(uchar val0, uchar val1, uchar val2, uchar val3)
(defcfun ("cv_create_Vec4b_4" vec-4b-4) vec-4b
  (val0 :uchar)
  (val1 :uchar)
  (val2 :uchar)
  (val3 :uchar))

(defun vec-4b (&optional val0 val1 val2 val3)
  "VEC-4B constructor"
  (cond ((null val0)
	 (vec-4b-0))
	(val0
	 (vec-4b-4 val0 val1 val2 val3))
	(t nil)))

(defun make-vec-4b (&optional val0 val1 val2 val3)
  "VEC-4B constructor"
  (cond ((null val0)
	 (vec-4b-0))
	(val0
	 (vec-4b-4 val0 val1 val2 val3))
	(t nil)))


;; typedef Vec<double, 2> Vec2d;
;; Vec2d* cv_create_Vec2d()
(defcfun ("cv_create_Vec2d" vec-2d-0) vec-2d)

;; typedef Vec<double, 2> Vec2d;
;; Vec2d* cv_create_Vec2d_2(double val0, double val1)
(defcfun ("cv_create_Vec2d_2" vec-2d-2) vec-2d
  (val0 :double)
  (val1 :double))

(defun vec-2d (&optional val0 val1)
  "VEC-2D constructor"
  (cond ((null val0)
	 (vec-2d-0))
	(val0
	 (vec-2d-2 val0 val1))
	(t nil)))

(defun make-vec-2d (&optional val0 val1)
  "VEC-2D constructor"
  (cond ((null val0)
	 (vec-2d-0))
	(val0
	 (vec-2d-2 val0 val1))
	(t nil)))


;; typedef Vec<double, 3> Vec3d;
;; Vec3d* cv_create_Vec3d()
(defcfun ("cv_create_Vec3d" vec-3d-0) vec-3d)

;; typedef Vec<double, 3> Vec3d;
;; Vec3d* cv_create_Vec3d_3(double val0, double val1, double val2)
(defcfun ("cv_create_Vec3d_3" vec-3d-3) vec-3d
  (val0 :double)
  (val1 :double)
  (val2 :double))

(defun vec-3d (&optional val0 val1 val2)
  "VEC-3D constructor"
  (cond ((null val0)
	 (vec-3d-0))
	(val0
	 (vec-3d-3 val0 val1 val2))
	(t nil)))

(defun make-vec-3d (&optional val0 val1 val2)
  "VEC-3D constructor"
  (cond ((null val0)
	 (vec-3d-0))
	(val0
	 (vec-3d-3 val0 val1 val2))
	(t nil)))


;; typedef Vec<double, 4> Vec4d;
;; Vec4d* cv_create_Vec4d()
(defcfun ("cv_create_Vec4d" vec-4d-0) vec-4d)

;; typedef Vec<double, 4> Vec4d;
;; Vec4d* cv_create_Vec4d_4(double val0, double val1, double val2, double val3)
(defcfun ("cv_create_Vec4d_4" vec-4d-4) vec-4d
  (val0 :double)
  (val1 :double)
  (val2 :double)
  (val3 :double))

(defun vec-4d (&optional val0 val1 val2 val3)
  "VEC-4D constructor"
  (cond ((null val0)
	 (vec-4d-0))
	(val0
	 (vec-4d-4 val0 val1 val2 val3))
	(t nil)))

(defun make-vec-4d (&optional val0 val1 val2 val3)
  "VEC-4D constructor"
  (cond ((null val0)
	 (vec-4d-0))
	(val0
	 (vec-4d-4 val0 val1 val2 val3))
	(t nil)))


;; typedef Vec<double, 6> Vec6d;
;; Vec6d* cv_create_Vec6d()
(defcfun ("cv_create_Vec6d" vec-6d-0) vec-6d)

;; typedef Vec<double, 6> Vec6d;
;; Vec6d* cv_create_Vec6d_6(double val0, double val1, double val2, double val3, double val4, double val5)
(defcfun ("cv_create_Vec6d_6" vec-6d-6) vec-6d
  (val0 :double)
  (val1 :double)
  (val2 :double)
  (val3 :double)
  (val4 :double)
  (val5 :double))

(defun vec-6d (&optional val0 val1 val2 val3 val4 val5)
  "VEC-6D constructor"
  (cond ((null val0)
	 (vec-6d-0))
	(val0
	 (vec-6d-6 val0 val1 val2 val3 val4 val5))
	(t nil)))

(defun make-vec-6d (&optional val0 val1 val2 val3 val4 val5)
  "VEC-6D constructor"
  (cond ((null val0)
	 (vec-6d-0))
	(val0
	 (vec-6d-6 val0 val1 val2 val3 val4 val5))
	(t nil)))


;; typedef Vec<float, 2> Vec2f
;; Vec2f* cv_create_Vec2f()
(defcfun ("cv_create_Vec2f" vec-2f-0) (cv::vec-2f :garbage-collect t))

;; typedef Vec<float, 2> Vec2f
;; Vec2f* cv_create_Vec2f_2(float val0, float val1)
(defcfun ("cv_create_Vec2f_2" vec-2f-2) (cv::vec-2f :garbage-collect t)
  (val0 :float)
  (val1 :float))

(defun vec-2f (&optional val0 val1)
  "VEC-2F constructor"
  (cond ((null val0)
	 (vec-2f-0))
	(val0
	 (vec-2f-2 val0 val1))
	(t nil)))

(defun make-vec-2f (&optional val0 val1)
  "VEC-2F constructor"
  (cond ((null val0)
	 (vec-2f-0))
	(val0
	 (vec-2f-2 val0 val1))
	(t nil)))


;; typedef Vec<float, 3> Vec3f
;; Vec3f* cv_create_Vec3f()
(defcfun ("cv_create_Vec3f" vec-3f-0) (cv::vec-3f :garbage-collect t))

;; typedef Vec<float, 3> Vec3f
;; Vec3f* cv_create_Vec3f_3(float val0, float val1, int val2)
(defcfun ("cv_create_Vec3f_3" vec-3f-3) (cv::vec-3f :garbage-collect t)
  (val0 :float)
  (val1 :float)
  (val2 :float))

(defun vec-3f (&optional val0 val1 val2)
  "VEC-3F constructor"
  (cond ((null val0)
	 (vec-3f-0))
	(val0
	 (vec-3f-3 val0 val1 val2))
	(t nil)))

(defun make-vec-3f (&optional val0 val1 val2)
  "VEC-3F constructor"
  (cond ((null val0)
	 (vec-3f-0))
	(val0
	 (vec-3f-3 val0 val1 val2))
	(t nil)))


;; typedef Vec<float, 4> Vec4f
;; Vec4f* cv_create_Vec4f()
(defcfun ("cv_create_Vec4f" vec-4f-0) vec-4f)

;; typedef Vec<float, 4> Vec4f
;; Vec4f* cv_create_Vec4f_4(float val0, float val1, float val2, float val3)
(defcfun ("cv_create_Vec4f_4" vec-4f-4) vec-4f 
  (val0 :float)
  (val1 :float)
  (val2 :float)
  (val3 :float))

(defun vec-4f (&optional val0 val1 val2 val3)
  "VEC-4F constructor"
  (cond ((null val0)
	 (vec-4f-0))
	(val0
	 (vec-4f-4 val0 val1 val2 val3))
	(t nil)))

(defun make-vec-4f (&optional val0 val1 val2 val3)
  "VEC-4F constructor"
  (cond ((null val0)
	 (vec-4f-0))
	(val0
	 (vec-4f-4 val0 val1 val2 val3))
	(t nil)))


;; typedef Vec<float, 6> Vec6f;
;; Vec6f* cv_create_Vec6f()
(defcfun ("cv_create_Vec6f" vec-6f-0) vec-6f)

;; typedef Vec<float, 6> Vec6f;
;; Vec6f* cv_create_Vec6f_6(float val0, float val1, float val2, float val3, float val4, float val5)
(defcfun ("cv_create_Vec6f_6" vec-6f-6) vec-6f
  (val0 :float)
  (val1 :float)
  (val2 :float)
  (val3 :float)
  (val4 :float)
  (val5 :float))

(defun vec-6f (&optional val0 val1 val2 val3 val4 val5)
  "VEC-6F constructor"
  (cond ((null val0)
	 (vec-6f-0))
	(val0
	 (vec-6f-6 val0 val1 val2 val3 val4 val5))
	(t nil)))

(defun make-vec-6f (&optional val0 val1 val2 val3 val4 val5)
  "VEC-6F constructor"
  (cond ((null val0)
	 (vec-6f-0))
	(val0
	 (vec-6f-6 val0 val1 val2 val3 val4 val5))
	(t nil)))


;; typedef Vec<int, 2> Vec2i;
;; Vec2i* cv_create_Vec2i()
(defcfun ("cv_create_Vec2i" vec-2i-0) vec-2i)

;; typedef Vec<int, 2> Vec2i;
;; Vec2i* cv_create_Vec2i_2(int val0, int val1)
(defcfun ("cv_create_Vec2i_2" vec-2i-2) vec-2i
  (val0 :int)
  (val1 :int))

(defun vec-2i (&optional val0 val1)
  "VEC-2I constructor"
  (cond ((null val0)
	 (vec-2i-0))
	(val0
	 (vec-2i-2 val0 val1))
	(t nil)))

(defun make-vec-2i (&optional val0 val1)
  "VEC-2I constructor"
  (cond ((null val0)
	 (vec-2i-0))
	(val0
	 (vec-2i-2 val0 val1))
	(t nil)))


;; typedef Vec<int, 3> Vec3i;
;; Vec3i* cv_create_Vec3i()
(defcfun ("cv_create_Vec3i" vec-3i-0) vec-3i)

;; typedef Vec<int, 3> Vec3i;
;; Vec3i* cv_create_Vec3i_3(int val0, int val1, int val2)
(defcfun ("cv_create_Vec3i_3" vec-3i-3) vec-3i
  (val0 :int)
  (val1 :int)
  (val2 :int))

(defun vec-3i (&optional val0 val1 val2)
  "VEC-3I constructor"
  (cond ((null val0)
	 (vec-3i-0))
	(val0
	 (vec-3i-3 val0 val1 val2))
	(t nil)))

(defun make-vec-3i (&optional val0 val1 val2)
  "VEC-3I constructor"
  (cond ((null val0)
	 (vec-3i-0))
	(val0
	 (vec-3i-3 val0 val1 val2))
	(t nil)))


;; typedef Vec<int, 4> Vec4i;
;; Vec4i* cv_create_Vec4i()
(defcfun ("cv_create_Vec4i" vec-4i-0) vec-4i)

;; typedef Vec<int, 4> Vec4i;
;; Vec4i* cv_create_Vec4i_4(int val0, int val1, int val2, int val3)
(defcfun ("cv_create_Vec4i_4" vec-4i-4) vec-4i
  (val0 :int)
  (val1 :int)
  (val2 :int)
  (val3 :int))

(defun vec-4i (&optional val0 val1 val2 val3)
  "VEC-4I constructor"
  (cond ((null val0)
	 (vec-4i-0))
	(val0
	 (vec-4i-4 val0 val1 val2 val3))
	(t nil)))

(defun make-vec-4i (&optional val0 val1 val2 val3)
  "VEC-4I constructor"
  (cond ((null val0)
	 (vec-4i-0))
	(val0
	 (vec-4i-4 val0 val1 val2 val3))
	(t nil)))


;; typedef Vec<int, 6> Vec6i;
;; Vec6i* cv_create_Vec6i()
(defcfun ("cv_create_Vec6i" vec-6i-0) vec-6i)

;; typedef Vec<int, 6> Vec6i;
;; Vec6i* cv_create_Vec6i_6(int val0, int val1, int val2, int val3, int val4, int val5)
(defcfun ("cv_create_Vec6i_6" vec-6i-6) vec-6i
  (val0 :int)
  (val1 :int)
  (val2 :int)
  (val3 :int)
  (val4 :int)
  (val5 :int))

(defun vec-6i (&optional val0 val1 val2 val3 val4 val5)
  "VEC-6I constructor"
  (cond ((null val0)
	 (vec-6i-0))
	(val0
	 (vec-6i-6 val0 val1 val2 val3 val4 val5))
	(t nil)))

(defun make-vec-6i (&optional val0 val1 val2 val3 val4 val5)
  "VEC-6I constructor"
  (cond ((null val0)
	 (vec-6i-0))
	(val0
	 (vec-6i-6 val0 val1 val2 val3 val4 val5))
	(t nil)))


;; typedef Vec<int, 8> Vec8i;
;; Vec8i* cv_create_Vec8i() 
(defcfun ("cv_create_Vec8i" vec-8i-0) vec-8i)

;; typedef Vec<int, 8> Vec8i;
;; Vec8i* cv_create_Vec8i_8(int val0, int val1, int val2, int val3, int val4, int val5, int val6, int val7) 
(defcfun ("cv_create_Vec8i_8" vec-8i-8) vec-8i
  (val0 :int)
  (val1 :int)
  (val2 :int)
  (val3 :int)
  (val4 :int)
  (val5 :int)
  (val6 :int)
  (val7 :int))

(defun vec-8i (&optional val0 val1 val2 val3 val4 val5 val6 val7)
  "VEC-8I constructor"
  (cond ((null val0)
	 (vec-8i-0))
	(val0
	 (vec-8i-8 val0 val1 val2 val3 val4 val5 val6 val7))
	(t nil)))

(defun make-vec-8i (&optional val0 val1 val2 val3 val4 val5 val6 val7)
  "VEC-8I constructor"
  (cond ((null val0)
	 (vec-8i-0))
	(val0
	 (vec-8i-8 val0 val1 val2 val3 val4 val5 val6 val7))
	(t nil)))


;; typedef Vec<short, 2> Vec2s;
;; Vec2s* cv_create_Vec2s()
(defcfun ("cv_create_Vec2s" vec-2s-0) vec-2s)

;; typedef Vec<short, 2> Vec2s;
;; Vec2s* cv_create_Vec2s_2(short val0, short val1)
(defcfun ("cv_create_Vec2s_2" vec-2s-2) vec-2s
  (val0 :short)
  (val1 :short))

(defun vec-2s (&optional val0 val1)
  "VEC-2S constructor"
  (cond ((null val0)
	 (vec-2s-0))
	(val0
	 (vec-2s-2 val0 val1))
	(t nil)))

(defun make-vec-2s (&optional val0 val1)
  "VEC-2S constructor"
  (cond ((null val0)
	 (vec-2s-0))
	(val0
	 (vec-2s-2 val0 val1))
	(t nil)))


;; typedef Vec<short, 3> Vec3s;
;; Vec3s* cv_create_Vec3s()
(defcfun ("cv_create_Vec3s" vec-3s-0) vec-3s)

;; typedef Vec<short, 3> Vec3s;
;; Vec3s* cv_create_Vec3s_3(short val0, short val1, short val2)
(defcfun ("cv_create_Vec3s_3" vec-3s-3) vec-3s
  (val0 :short)
  (val1 :short)
  (val2 :short))

(defun vec-3s (&optional val0 val1 val2)
  "VEC-3S constructor"
  (cond ((null val0)
	 (vec-3s-0))
	(val0
	 (vec-3s-3 val0 val1 val2))
	(t nil)))

(defun make-vec-3s (&optional val0 val1 val2)
  "VEC-3S constructor"
  (cond ((null val0)
	 (vec-3s-0))
	(val0
	 (vec-3s-3 val0 val1 val2))
	(t nil)))


;; typedef Vec<short, 4> Vec4s;
;; Vec4s* cv_create_Vec4s()
(defcfun ("cv_create_Vec4s" vec-4s-0) vec-4s)

;; typedef Vec<short, 4> Vec4s;
;; Vec4s* cv_create_Vec4s_4(short val0, short val1, short val2, short val3)
(defcfun ("cv_create_Vec4s_4" vec-4s-4) vec-4s
  (val0 :short)
  (val1 :short)
  (val2 :short)
  (val3 :short))

(defun vec-4s (&optional val0 val1 val2 val3)
  "VEC-4S constructor"
  (cond ((null val0)
	 (vec-4s-0))
	(val0
	 (vec-4s-4 val0 val1 val2 val3))
	(t nil)))

(defun make-vec-4s (&optional val0 val1 val2 val3)
  "VEC-4S constructor"
  (cond ((null val0)
	 (vec-4s-0))
	(val0
	 (vec-4s-4 val0 val1 val2 val3))
	(t nil)))


;; typedef Vec<ushort, 2> Vec2w;
;; Vec2w* cv_create_Vec2w()
(defcfun ("cv_create_Vec2w" vec-2w-0) vec-2w)

;; typedef Vec<ushort, 2> Vec2w;
;; Vec2w* cv_create_Vec2w_2(ushort val0, ushort val1)
(defcfun ("cv_create_Vec2w_2" vec-2w-2) vec-2w
  (val0 :ushort)
  (val1 :ushort))

(defun vec-2w (&optional val0 val1)
  "VEC-2W constructor"
  (cond ((null val0)
	 (vec-2w-0))
	(val0
	 (vec-2w-2 val0 val1))
	(t nil)))

(defun make-vec-2w (&optional val0 val1)
  "VEC-2W constructor"
  (cond ((null val0)
	 (vec-2w-0))
	(val0
	 (vec-2w-2 val0 val1))
	(t nil)))


;; typedef Vec<ushort, 3> Vec3w;
;; Vec3w* cv_create_Vec3w()
(defcfun ("cv_create_Vec3w" vec-3w-0) vec-3w)

;; typedef Vec<ushort, 3> Vec3w;
;; Vec3w* cv_create_Vec3w_3(ushort val0, ushort val1, ushort val2)
(defcfun ("cv_create_Vec3w_3" vec-3w-3) vec-3w
  (val0 :ushort)
  (val1 :ushort)
  (val2 :ushort))

(defun vec-3w (&optional val0 val1 val2)
  "VEC-3W constructor"
  (cond ((null val0)
	 (vec-3w-0))
	(val0
	 (vec-3w-3 val0 val1 val2))
	(t nil)))

(defun make-vec-3w (&optional val0 val1 val2)
  "VEC-3W constructor"
  (cond ((null val0)
	 (vec-3w-0))
	(val0
	 (vec-3w-3 val0 val1 val2))
	(t nil)))


;; typedef Vec<ushort, 4> Vec4w;
;; Vec4w* cv_create_Vec4w()
(defcfun ("cv_create_Vec4w" vec-4w-0) vec-4w)

;; typedef Vec<ushort, 4> Vec4w;
;; Vec4w* cv_create_Vec4w_4(ushort val0, ushort val1, ushort val2, ushort val3)
(defcfun ("cv_create_Vec4w_4" vec-4w-4) vec-4w
  (val0 :ushort)
  (val1 :ushort)
  (val2 :ushort)
  (val3 :ushort))

(defun vec-4w (&optional val0 val1 val2 val3)
  "VEC-4W constructor"
  (cond ((null val0)
	 (vec-4w-0))
	(val0
	 (vec-4w-4 val0 val1 val2 val3))
	(t nil)))

(defun make-vec-4w (&optional val0 val1 val2 val3)
  "VEC-4W constructor"
  (cond ((null val0)
	 (vec-4w-0))
	(val0
	 (vec-4w-4 val0 val1 val2 val3))
	(t nil)))


;; _Tp width, height
;; double cv_Size_width(Size* self) 
(defcfun ("cv_Size_width" width) :double
  "Gets the width of a SIZE construct"
  (self size))


;; _Tp width, height
;; float cv_Size2f_width(Size* self) 
(defcfun ("cv_Size2f_width" width-2f) :float
  "Gets the width of a SIZE-2F"
  (self size-2f))


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


;; void log(InputArray src, OutputArray dst)
;; void cv_log(Mat* src, Mat* dst)
(defcfun ("cv_log" *log) :int
  "Calculates the natural logarithm of every array element."
  (src mat)
  (dest mat))


;; void magnitude(InputArray x, InputArray y, OutputArray magnitude)
;; void cv_magnitude(Mat* x, Mat* y, Mat* magnitude)
(cffi:defcfun ("cv_magnitude" %magnititude) :void
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

