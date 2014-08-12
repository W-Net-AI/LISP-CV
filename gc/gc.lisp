;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; gc.lisp
;;;; OpenCV bindings
;;;; Garbage collected versions of functions.

(in-package :gc)


;;; DEFGENERIC (*defgeneric functions are placed here so the whole file can use them*)

(defgeneric abs (self)
  (:documentation "Used to call the GC'ed binding for the OpenCV function 'abs'"))

(defgeneric clone (self)
  (:documentation "Used for all bindings with a clone member."))

(defgeneric mean (self &rest args)
  (:documentation "Used to overload the MEAN name for functions, members, and member functions."))

(defgeneric (setf mean) (val self)
  (:documentation "Used to setf the MEAN value of class bindings with an MEAN member."))

(defgeneric size (arg &rest args)
  (:documentation "Used for all bindings with a size member."))


;; Interop - String*


;; stdstring* create_std_string()
  "Creates a STRING* object."
(defcfun ("create_std_string" %string) (cv:string* :garbage-collect t))


;; string* std_cstringToString(char* s, size_t len) 
(defcfun ("cstring_to_std_string" c-string-to-string) (cv:string* :garbage-collect t)
	 "Converts C string to C++"
	 (s :string)
	 (len :unsigned-int))


(defun c-string (string)
  "If you need a speed boost and decide to use low-level functions, 
   use this to create a C string for  their STRING* parameters."
  (c-string-to-string string (cl:length string)))


;; CFFI - FOREIGN-ALLOC

(defmacro alloc (&optional type value)
  "A GC'ed version of foreign-alloc."
       (cond ((listp value)
	      `(cv:gced-foreign-alloc ,type ,:initial-contents ,value))
	     (t `(cv:gced-foreign-alloc ,type ,:initial-element ,value))))


;;; CORE - Basic Structures


;; Mat* force(MatExpr* expr)
(defcfun ("force" >>) (cv:mat :garbage-collect t)
	 "Coerces a MAT-EXPR to a MAT. 
   This is a shorthand version of the FORCE function."
   (mat-expr cv:mat-expr))


;; MatExpr* promote(Mat* m) 
(defcfun ("promote" <<) (cv:mat-expr :garbage-collect t)
	 "Converts a MAT to a MAT-EXPR.
   This is a shorthand version of the PROMOTE function." 
   (mat cv:mat))


;; MatExpr + operator
;; MatExpr* cv_Mat_add(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_add" add) (cv:mat-expr :garbage-collect t)
	 (m1 cv:mat)
	 (m2 cv:mat))


;; Mat& Mat::adjustROI(int dtop, int dbottom, int dleft, int dright)
;; Mat* cv_Mat_adjustROI(Mat* self, int dtop, int dbottom, int dleft, int dright) 
(defcfun ("cv_Mat_adjustROI" adjust-roi) (cv:mat :garbage-collect t)
	 "Adjusts a submatrix size and position within the parent matrix."
	 (self cv:mat)
	 (dtop :int)
	 (dbottom :int)
	 (dleft :int)
	 (dright :int))


(defun arr-to-mat (arr &optional mat-type)

  (let* ((array-element-type (array-element-type arr))
        (array-dimensions (array-dimensions arr))
	 (x (car array-dimensions))
	 (y (cadr array-dimensions))
	 (z (caddr array-dimensions))
         (channels (if z z 1))
	 (area (* x y))
         (mat 0)
	 (ptr 0)
         (cffi-type  
	  (typecase arr
	    ((simple-array (unsigned-byte 8)) :uchar)
	    ((simple-array (signed-byte 8)) :char)
	    ((simple-array (unsigned-byte 16)) :ushort)
	    ((simple-array (signed-byte 16)) :short)
	    ((simple-array (signed-byte 32)) :int)
	    ((simple-array single-float) :float)
	    ((simple-array double-float) :double))))

    (if (and (not mat-type) (eq array-element-type t))
	(error "~%If (EQ ARRAY-ELEMENT-TYPE T): you must specify the type of the output matrix.~%"))
    (if (eq array-element-type t) 

(setf cffi-type (case mat-type 
		      (#.cv:+8uc1+ :uchar) 
		      (#.cv:+8sc1+ :char)
		      (#.cv:+16uc1+ :ushort)
		      (#.cv:+16sc1+ :short)
		      (#.cv:+32sc1+ :int)
		      (#.cv:+32fc1+ :float)
		      (#.cv:+64fc1+ :double)
		      (#.cv:+8uc2+  :uchar)
		      (#.cv:+8sc2+  :char)
		      (#.cv:+16uc2+ :ushort)
		      (#.cv:+16sc2+ :short)
		      (#.cv:+32sc2+ :int)
		      (#.cv:+32fc2+ :float)
		      (#.cv:+64fc2+ :double)
		      (#.cv:+8uc3+  :uchar)
		      (#.cv:+8sc3+  :char)
		      (#.cv:+16uc3+ :ushort)
		      (#.cv:+16sc3+ :short)
		      (#.cv:+32sc3+ :int)
		      (#.cv:+32fc3+ :float)
		      (#.cv:+64fc3+ :double)
		      (#.cv:+8uc4+  :uchar)
		      (#.cv:+8sc4+  :char)
		      (#.cv:+16uc4+ :ushort)
		      (#.cv:+16sc4+ :short)
		      (#.cv:+32sc4+ :int)
		      (#.cv:+32fc4+ :float)
		      (#.cv:+64fc4+ :double))))



    (case cffi-type

      (:uchar 
       (if mat-type (setf mat (create-mat-typed x y mat-type))
	   (case channels (1
			   (setf mat (create-mat-typed x y cv:+8uc1+)))
		 ((2 3 4)
		  (setf mat (create-mat-typed x y (case channels 
					     (2 #.cv:+8uc2+)
					     (3 #.cv:+8uc3+)
					     (4 #.cv:+8uc4+)))))))
       (setf ptr (cv:%ptr mat 0))
       (dotimes (n (* area channels))
	 (setf (mem-aref ptr :uchar n) (row-major-aref arr n)))mat)

      (:char 
       (if mat-type (setf mat (create-mat-typed x y mat-type))
       (case channels (1
		       (setf mat (create-mat-typed x y cv:+8sc1+)))
	     ((2 3 4)
	      (setf mat (create-mat-typed x y (case channels 
					 (2 #.cv:+8sc2+)
					 (3 #.cv:+8sc3+)
					 (4 #.cv:+8sc4+)))))))
       (setf ptr (cv:%ptr mat 0))
       (dotimes (n (* area channels))
	 (setf (mem-aref ptr :char n) (row-major-aref arr n)))mat)

      (:ushort 
       (if mat-type (setf mat (create-mat-typed x y mat-type))
       (case channels (1
		       (setf mat (create-mat-typed x y cv:+16uc1+)))
	     ((2 3 4)
	      (setf mat (create-mat-typed x y (case channels 
					 (2 #.cv:+16uc2+)
					 (3 #.cv:+16uc3+)
					 (4 #.cv:+16uc4+)))))))
       (setf ptr (cv:%ptr mat 0))
       (dotimes (n (* area channels))
	 (setf (mem-aref ptr :ushort n) (row-major-aref arr n)))mat)

      (:short 
       (if mat-type (setf mat (create-mat-typed x y mat-type))
       (case channels (1
		       (setf mat (create-mat-typed x y cv:+16sc1+)))
	     ((2 3 4)
	      (setf mat (create-mat-typed x y (case channels 
					 (2 #.cv:+16sc2+)
					 (3 #.cv:+16sc3+)
					 (4 #.cv:+16sc4+)))))))
       (setf ptr (cv:%ptr mat 0))
       (dotimes (n (* area channels))
	 (setf (mem-aref ptr :short n) (row-major-aref arr n)))mat)

      (:int 
       (if mat-type (setf mat (create-mat-typed x y mat-type))
       (case channels (1
		       (setf mat (create-mat-typed x y cv:+32sc1+)))
	     ((2 3 4)
	      (setf mat (create-mat-typed x y (case channels 
					 (2 #.cv:+32sc2+)
					 (3 #.cv:+32sc3+)
					 (4 #.cv:+32sc4+)))))))
       (setf ptr (cv:%ptr mat 0))
       (dotimes (n (* area channels))
	 (setf (mem-aref ptr :int n) (row-major-aref arr n)))mat)

      (:float 
       (if mat-type (setf mat (create-mat-typed x y mat-type))
       (case channels (1
		       (setf mat (create-mat-typed x y cv:+32fc1+)))
	     ((2 3 4)
	      (setf mat (create-mat-typed x y (case channels 
					 (2 #.cv:+32fc2+)
					 (3 #.cv:+32fc3+)
					 (4 #.cv:+32fc4+)))))))
       (setf ptr (cv:%ptr mat 0))
       (dotimes (n (* area channels))
	 (setf (mem-aref ptr :float n) (row-major-aref arr n)))mat)

      (:double 
       (if mat-type (setf mat (create-mat-typed x y mat-type))
       (case channels (1
		       (setf mat (create-mat-typed x y cv:+64fc1+)))
	     ((2 3 4)
	      (setf mat (create-mat-typed x y (case channels 
					 (2 #.cv:+64fc2+)
					 (3 #.cv:+64fc3+)
					 (4 #.cv:+64fc4+)))))))
       (setf ptr (cv:%ptr mat 0))
       (dotimes (n (* area channels))
	 (setf (mem-aref ptr :double n) (row-major-aref arr n)))mat))))


;; Mat Mat::clone() const
;; Mat* cv_Mat_clone(Mat* self) 
(defcfun ("cv_Mat_clone" clone-mat) (cv:mat :garbage-collect t)
	 (self cv:mat))


(defmethod clone ((self cv:cv-mat))
  "Creates a full copy of the array and the underlying data."
  (clone-mat self))


;; Mat Mat::colRange(int startcol, int endcol) const
;; Mat* cv_Mat_getColRange(Mat* self, int startcol, int endrow)
(defcfun ("cv_Mat_getColRange" col-range) (cv:mat :garbage-collect t)
	 "Creates a matrix header for the specified column span."
	 (self cv:mat)
	 (startcol :int)
	 (endcol :int))


;; Mat Mat::cross(InputArray m) const
;; Mat* cv_Mat_cross(Mat* self, Mat* m)
(defcfun ("cv_Mat_cross" cross) (cv:mat :garbage-collect t)
	 "Computes a cross-product of two 3-element vectors."
	 (self cv:mat)
	 (m cv:mat))


;; Mat* cv_Mat_diag_d(Mat* self, int d)
(defcfun ("cv_Mat_diag_d" %diag) (cv:mat :garbage-collect t)
	 "Extracts a diagonal from a matrix."
	 (self cv:mat)
	 (d :int))


(defun diag (self &optional (d 0))
       "Extracts a diagonal from a matrix."
       (%diag self d))


;; MatExpr / operator
;; MatExpr* cv_Mat_div(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_div" div) (cv:mat-expr :garbage-collect t)
	 (m1 cv:mat)
	 (m2 cv:mat))


;; DMatch::DMatch()
;; DMatch* cv_create_DMatch() 
(defcfun ("cv_create_DMatch" dmatch-0) (cv:dmatch :garbage-collect t)
	 "DMatch constructor")


;; DMatch( int _queryIdx, int _trainIdx, float _distance ) 
;; DMatch* cv_create_DMatch3(int _queryIdx, int _trainIdx, float _distance)
(defcfun ("cv_create_DMatch3" dmatch-3) (cv:dmatch :garbage-collect t)
	 "DMatch constructor"
	 (query-idx :int)
	 (train-idx :int)
	 (distance :float))


;; DMatch( int _queryIdx, int _trainIdx, int _imgIdx, float _distance )
;; DMatch* cv_create_DMatch4(int _queryIdx, int _trainIdx, int _imgIdx, float _distance)
(defcfun ("cv_create_DMatch4" dmatch-4) (cv:dmatch :garbage-collect t)
	 "DMatch constructor"
	 (query-idx :int)
	 (train-idx :int)
	 (img-idx :int)
	 (distance :float))


(defun dmatch (&rest args)
       (cond ((eq (first args) nil)
	      (dmatch-0))
	      ((and (first args) (not (fourth args)))
	       (dmatch-3 (first args) (second args) (third args)))
	       ((fourth args)
		(dmatch-4 (first args) (second args) (third args) (fourth args)))
		(t nil)))


(defun make-dmatch (&rest args)
       (cond ((eq (first args) nil)
	      (dmatch-0))
	      ((and (first args) (not (fourth args)))
	       (dmatch-3 (first args) (second args) (third args)))
	       ((fourth args)
		(dmatch-4 (first args) (second args) (third args) (fourth args)))
		(t nil)))


;; Mat* force(MatExpr* expr)
(defcfun ("force" force) (cv:mat :garbage-collect t)
	 "Coerces a MAT-EXPR to a MAT."
	 (mat-expr cv:mat-expr))


;; KeyPoint::KeyPoint()
;; KeyPoint* cv_create_KeyPoint()
(defcfun ("cv_create_KeyPoint" key-point-0) (cv:key-point :garbage-collect t)
	 "KEY-POINT constructor")


;; KeyPoint::KeyPoint(float x, float y, float _size, float _angle=-1, float _response=0, int _octave=0, int _class_id=-1)
;; KeyPoint* cv_create_KeyPoint7(float x, float y, float _size, float _angle, float _response, int _octave, int _class_id)
(defcfun ("cv_create_KeyPoint7" key-point-7) (cv:key-point :garbage-collect t)
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


;; Mat::Mat()
;; Mat* cv_create_Mat()
(defcfun ("cv_create_Mat" %mat) (cv:mat :garbage-collect t)
	 "MAT constructor")


;; Mat::Mat(int rows, int cols, int type, void* data) 
;; Mat* cv_create_Mat_with_data(int rows, int cols, int type, void* data)
(defcfun ("cv_create_Mat_with_data" create-mat-with-data) (cv:mat :garbage-collect t)
	 (rows :int)
	 (cols :int)
	 (type :int)
	 (data :pointer))


(let ((previous nil))
  (defun %create-mat-with-data-uchar (rows cols type data)

    (unless (equal data (car previous))
      (setf previous (cons data (cv:gced-foreign-alloc :uchar
						    :initial-contents data))))
    (create-mat-with-data rows cols type (cdr previous))))


(let ((previous nil))
  (defun %create-mat-with-data-char (rows cols type data)

    (unless (equal data (car previous))
      (setf previous (cons data (cv:gced-foreign-alloc :char
						    :initial-contents data))))
    (create-mat-with-data rows cols type (cdr previous))))


(let ((previous nil))
  (defun %create-mat-with-data-ushort (rows cols type data)

    (unless (equal data (car previous))
      (setf previous (cons data (cv:gced-foreign-alloc :ushort
						    :initial-contents data))))
    (create-mat-with-data rows cols type (cdr previous))))


(let ((previous nil))
  (defun %create-mat-with-data-short (rows cols type data)

    (unless (equal data (car previous))
      (setf previous (cons data (cv:gced-foreign-alloc :short
						    :initial-contents data))))
    (create-mat-with-data rows cols type (cdr previous))))


(let ((previous nil))
  (defun %create-mat-with-data-int (rows cols type data)

    (unless (equal data (car previous))
      (setf previous (cons data (cv:gced-foreign-alloc :int
						    :initial-contents data))))
    (create-mat-with-data rows cols type (cdr previous))))


(let ((previous nil))
  (defun %create-mat-with-data-float (rows cols type data)

    (unless (equal data (car previous))
      (setf previous (cons data (cv:gced-foreign-alloc :float
						    :initial-contents data))))
    (create-mat-with-data rows cols type (cdr previous))))


(let ((previous nil))
  (defun %create-mat-with-data-double (rows cols type data)

    (unless (equal data (car previous))
      (setf previous (cons data (cv:gced-foreign-alloc :double
						    :initial-contents data))))
    (create-mat-with-data rows cols type (cdr previous))))


(defun %create-mat-with-data (rows cols type data-list)
  (let ((cffi-type (case type 
		     (#.cv:+8uc1+ ':uchar)
		     (#.cv:+8sc1+ ':char)
		     (#.cv:+16uc1+ ':ushort)
		     (#.cv:+16sc1+ ':short)
		     (#.cv:+32sc1+ ':int)
		     (#.cv:+32fc1+ ':float)
		     (#.cv:+64fc1+ ':double)
		     (#.cv:+8uc2+ ':uchar)
		     (#.cv:+8sc2+ ':char)
		     (#.cv:+16uc2+ ':ushort)
		     (#.cv:+16sc2+ ':short)
		     (#.cv:+32sc2+ ':int)
		     (#.cv:+32fc2+ ':float)
		     (#.cv:+64fc2+ ':double)
		     (#.cv:+8uc3+ ':uchar)
		     (#.cv:+8sc3+ ':char)
		     (#.cv:+16uc3+ ':ushort)
		     (#.cv:+16sc3+ ':short)
		     (#.cv:+32sc3+ ':int)
		     (#.cv:+32fc3+ ':float)
		     (#.cv:+64fc3+ ':double)
		     (#.cv:+8uc4+ ':uchar)
		     (#.cv:+8sc4+ ':char)
		     (#.cv:+16uc4+ ':ushort)
		     (#.cv:+16sc4+ ':short)
		     (#.cv:+32sc4+ ':int)
		     (#.cv:+32fc4+ ':float)
		     (#.cv:+64fc4+ ':double))))
    (case cffi-type 
      (:uchar
       (%create-mat-with-data-uchar rows cols type data-list))
      (:char
       (%create-mat-with-data-char rows cols type data-list))
      (:short
       (%create-mat-with-data-short rows cols type data-list))
      (:ushort
       (%create-mat-with-data-ushort rows cols type data-list))
      (:int
       (%create-mat-with-data-int rows cols type data-list))
      (:float
       (%create-mat-with-data-float rows cols type data-list))
      (:double
       (%create-mat-with-data-double rows cols type data-list)))))


;; Mat::t
;; MatExpr* cv_Mat_transpose_mat(Mat* self) 
(defcfun ("cv_Mat_transpose_mat" mat-expr-t) (cv:mat-expr :garbage-collect t)
	 "Transposes a matrix."
	 (self cv:mat))


;; static MatExpr Mat::eye(int rows, int cols, int type) 
;; Mat* cv_create_identity(int rows, int cols, int type)
(defcfun ("cv_create_identity" mat-eye) (cv:mat :garbage-collect t)
	 "Returns an identity matrix of the specified size and type."
	 (rows :int)
	 (cols :int)
	 (type :int))


;; static MatExpr Mat::ones(int rows, int cols, int type)
;; Mat* cv_create_ones(int rows, int cols, int type)
(defcfun ("cv_create_ones" mat-ones) (cv:mat :garbage-collect t)
	 (rows :int)
	 (cols :int)
	 (type :int))


;; Mat::Mat(const Mat& m, const Range& rowRange, const Range& colRange=Range::all() )
;; Mat* cv_Mat_get_Range(Mat* self, Range* rowRange, Range* colRange)
(defcfun ("cv_Mat_with_Range" %create-mat-with-range) (cv:mat :garbage-collect t)
  "MAT constructor with Range parameters."
  (self cv:mat)
  (row-range cv:range)
  (col-range cv:range))


(defun create-mat-with-range (self row-range &optional (col-range (cv:range-all) given-col-range))
  (let ((return (%create-mat-with-range self row-range col-range)))
    (if given-col-range nil (cv:del-range col-range))
    return))


;; Size Mat::size() const
;; Size* cv_Mat_size(Mat* self)
(defcfun ("cv_Mat_size" mat-size) (cv:size :garbage-collect t)
	 (self cv:mat))


(defmethod size ((arg cv:cv-mat) &rest args)
  args
  (mat-size arg))


;; MatExpr * operator
;; MatExpr* cv_Mat_scale(MatExpr* m, double alpha)
(defcfun ("cv_Mat_scale" scale) (cv:mat-expr :garbage-collect t)
	 (m cv:mat-expr)
	 (alpha :double))


;; Mat::Mat(int rows, int cols, int type)
;; Mat* cv_create_Mat_typed(int rows, int cols, int type)
(defcfun ("cv_create_Mat_typed" create-mat-typed) (cv:mat :garbage-collect t)
	 "MAT constructor with a row, column and type parameter."
	 (rows :int)
	 (cols :int)
	 (type :int))


;; Mat::Mat(int rows, int cols, int type, const Scalar& s)
;; Mat* cv_create_Mat_with_value(int rows, int cols, int type, Scalar* s)
(defcfun ("cv_create_Mat_with_value" create-mat-with-value) cv:mat
	 (rows :int)
	 (cols :int)
	 (type :int)
	 (s cv:scalar))


(defun create-mat-with-element (rows cols type value)
  (let* ((scalar (apply #'cv:scalar (list value)))					 
	(ret (create-mat-with-value rows cols type scalar)))
    (cv:del-scalar scalar)
ret))


;;; MAT


(defun mat (&optional arg1 arg2 arg3 arg4)
  
  "MAT constructor"  
  
  (cond ((eq arg1 nil) (%mat))

	((and (eq arg2 nil) 
	      (typep arg1 'cv:cv-mat))

	 (cv:mat-to-arr arg1))

	((and (or (not arg2) (typep arg2 'integer))

	      (typep arg1 'simple-array))

	 (arr-to-mat arg1 arg2))

	((typep arg2 'cv:cv-range)

	 (apply #'create-mat-with-range arg1 arg2 arg3))
	
	((and (eq arg4 nil) arg1)

	 (create-mat-typed arg1 arg2 arg3))
	
	((typep arg4 'cv:cv-scalar)

	 (create-mat-with-value arg1 arg2 arg3 arg4))
	
	((listp arg4)

	 (%create-mat-with-data arg1 arg2 arg3 arg4))
	
	((pointerp arg4)

	 (create-mat-with-data arg1 arg2 arg3 arg4))

	((integerp arg4)

	 (create-mat-with-element arg1 arg2 arg3 arg4))
	
	(t nil)))


(defun make-mat (&optional arg1 arg2 arg3 arg4)
  
  "MAT constructor"  
  
  (cond ((eq arg1 nil) (%mat))

	((and (eq arg2 nil) 
	      (typep arg1 'cv:cv-mat))

	 (cv:mat-to-arr arg1))

	((and (or (not arg2) (typep arg2 'integer))

	      (typep arg1 'simple-array))

	 (arr-to-mat arg1 arg2))

	((typep arg2 'cv:cv-range)

	 (apply #'create-mat-with-range arg1 arg2 arg3))
	
	((and (eq arg4 nil) arg1)

	 (create-mat-typed arg1 arg2 arg3))
	
	((typep arg4 'cv:cv-scalar)

	 (create-mat-with-value arg1 arg2 arg3 arg4))
	
	((listp arg4)

	 (%create-mat-with-data arg1 arg2 arg3 arg4))
	
	((pointerp arg4)

	 (create-mat-with-data arg1 arg2 arg3 arg4))

	((integerp arg4)

	 (create-mat-with-element arg1 arg2 arg3 arg4))
	
	(t nil)))


;; static MatExpr Mat::zeros(int rows, int cols, int type)
;; Mat* cv_create_zeros(int rows, int cols, int type)
(defcfun ("cv_create_zeros" mat-zeros) (cv:mat :garbage-collect t)
	 (rows :int)
	 (cols :int)
	 (type :int))


;; static MatExpr Mat::zeros(int rows, int cols, int type)
;; Mat* cv_create_zeros(int rows, int cols, int type)
(defcfun ("cv_create_zeros" make-mat-zeros) (cv:mat :garbage-collect t)
	 (rows :int)
	 (cols :int)
	 (type :int))


;; MatExpr * operator
;; MatExpr* cv_Mat_mult(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_mult" mul) (cv:mat-expr :garbage-collect t)
	 (m1 cv:mat)
	 (m2 cv:mat))


;; Point_()
;; Point2##t * cv_create_Point2##t ( tn x,  tn y)
(defcfun ("cv_create_Point2i" point-0) (cv:point :garbage-collect t))


;; Point_(_Tp _x, _Tp _y)
;; Point2##t * cv_create_Point2##t ( tn x,  tn y) 
(defcfun ("cv_create_Point2i" point-2) (cv:point :garbage-collect t)
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
	 (self cv:point))


;; _Tp x, y
;; int cv_Point_getY(Point* self)
(defcfun ("cv_Point2i_getY" point-y) :int 
	 "Retrieves y coordinate of a point"
	 (self cv:point))


;; typedef Point_<double> Point2d
;; Point2##t * cv_create_Point2##t ( tn x,  tn y) 
(defcfun ("cv_create_Point2d" point-2d-0) (cv:point-2d :garbage-collect t) 
	 "Point2d constructor")


;; typedef Point_<double> Point2d
;; Point2##t * cv_create_Point2##t ( tn x,  tn y)  
(defcfun ("cv_create_Point2d" point-2d-2) (cv:point-2d :garbage-collect t) 
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
  (self cv:point-2d))


;; _Tp x, y
;; double cv_Point2d_getY(Point2d* self) 
(defcfun ("cv_Point2d_getY" point-2d-y) :double
  "Retrieves y coordinate of a point-2d"
  (self cv:point-2d))


;; typedef Point_<float> Point2f
;; tn cv_Point2##t##_getX( Point2##t * self) 
(defcfun ("cv_create_Point2f" point-2f-0) (cv:point-2f :garbage-collect t) 
	 "Point2f constructor")


;; typedef Point_<float> Point2f
;; Point2##t * cv_create_Point2##t ( tn x,  tn y)  
(defcfun ("cv_create_Point2f" point-2f-2) (cv:point-2f :garbage-collect t) 
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
  (self cv:point-2f))


;; _Tp x, y;
;; float cv_Point2f_getY(Point2f* self) 
(defcfun ("cv_Point2f_getY" point-2f-y) :float
  "Retrieves y coordinate of a point-2f"
  (self cv:point-2f))


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z) 
(defcfun ("cv_create_Point3d" point-3d-0) (cv:point-3d :garbage-collect t) 
	 "Point3d constructotr")


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z) 
(defcfun ("cv_create_Point3d" point-3d-2) (cv:point-3d :garbage-collect t) 
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
  (self cv:point-3d))


;; _Tp x, y, z
;; double cv_Point3d_getY(Point3d* self) 
(defcfun ("cv_Point3d_getY" point-3d-y) :double
  "Retrieves y coordinate of a point-3d"
  (self cv:point-3d))


;; _Tp x, y, z
;; double cv_Point3d_getZ(Point3d* self) 
(defcfun ("cv_Point3d_getZ" point-3d-z) :double
  "Retrieves z coordinate of a point-3d"
  (self cv:point-3d))


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3f" point-3f-0) (cv:point-3f :garbage-collect t) 
	 "Point3f constructor")


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3f" point-3f-2) (cv:point-3f :garbage-collect t)
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
  (self cv:point-3f))


;; _Tp x, y, z
;; float cv_Point3f_getY(Point3f* self) 
(defcfun ("cv_Point3f_getY" point-3f-y) :float
  "Retrieves y coordinate of a point-3f"
  (self cv:point-3f))


;; _Tp x, y, z
;; float cv_Point3f_getZ(Point3f* self) 
(defcfun ("cv_Point3f_getZ" point-3f-z) :float
  "Retrieves z coordinate of a point-3f"
  (self cv:point-3f))


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3i" point-3i-0) (cv:point-3i :garbage-collect t) 
	 "Point3i constructor")


;; typedef Point3_<double> Point3d
;; Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z)  
(defcfun ("cv_create_Point3i" point-3i-2) (cv:point-3i :garbage-collect t)
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
	 (self cv:point-3i))


;; _Tp x, y, z
;; int cv_Point3i_getY(Point3i* self) 
(defcfun ("cv_Point3i_getY" point-3i-y) :int
  "Retrieves y coordinate of a point-3i"
  (self cv:point-3i))


;; _Tp x, y, z
;; int cv_Point3i_getZ(Point3i* self) 
(defcfun ("cv_Point3i_getZ" point-3i-z) :int
  "Retrieves z coordinate of a point-3i"
  (self cv:point-3i))


;; MatExpr* promote(Mat* m) 
(defcfun ("promote" promote) (cv:mat-expr :garbage-collect t)
	 "Converts a MAT to a MAT-EXPR."
	 (mat cv:mat))


;; Range::Range(int _start, int _end)
;; Range* cv_create_Range(int _start, int _end) 
(defcfun ("cv_create_Range" range) (cv:range :garbage-collect t)
  "Range constructor"
  (start :int)
  (end :int))


;; Range::Range(int _start, int _end)
;; Range* cv_create_Range(int _start, int _end) 
(defcfun ("cv_create_Range" make-range) (cv:range :garbage-collect t)
  "Range constructor"
  (start :int)
  (end :int))


;; static Range::Range all()
;; Range* cv_create_RangeAll()
(defcfun ("cv_create_RangeAll" range-all) (cv:range :garbage-collect t)
  "Range constructor - Returns a special variable 
   that means “the whole sequence” or “the whole 
   range”")


;; static Range::Range all()
;; Range* cv_create_RangeAll()
(defcfun ("cv_create_RangeAll" make-range-all) (cv:range :garbage-collect t)
  "Range constructor - Returns a special variable 
   that means “the whole sequence” or “the whole 
   range”")


;; Rect_()
;; Rect* cv_create_Rect() 
(defcfun ("cv_create_Rect" rect-0) (cv:rect :garbage-collect t) 
	 "RECT constructor.")


;; Rect_(_Tp _x, _Tp _y, _Tp _width, _Tp _height)
;; Rect* cv_create_Rect4(int x, int y, int width, int height) 
(defcfun ("cv_create_Rect4" rect-4) (cv:rect :garbage-collect t)
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


;; Point Rect::br() const
;; Point* cv_Rect_br(Rect* self) 
(defcfun ("cv_Rect_br" rect-br) (cv:point :garbage-collect t)
  "Retrieves the bottom-right corner of a rectangle."
  (self cv:rect))


;; Point Rect::br() const
;; Point* cv_Rect_br(Rect* self) 
(defcfun ("cv_Rect_br" br) (cv:point :garbage-collect t)
  "Retrieves the bottom-right corner of a rectangle."
  (self cv:rect))


;; Rect(int x, int y, int width, int height)
;; Rect::x, Rect::y, Rect::width, Rect::height
;; Rect* cv_Rect_clone(Rect* self)
(defcfun ("cv_Rect_clone" clone-rect) (cv:rect :garbage-collect t)
	 (self cv:rect))


(defmethod clone ((self cv:cv-rect))
  "Creates a full copy of a RECT object."
  (clone-rect self))


;; Size_<_Tp> size() const
;; Size* cv_Rect_size(Rect* self)  
(defcfun ("cv_Rect_size" rect-size) (cv:size :garbage-collect t) 
	 "Size (width, height) of the rectangle."
	 (self cv:rect))


(defmethod size ((arg cv:cv-rect) &rest args)
  args
  (rect-size arg))


;; Point Rect::tl() const
;; Point* cv_Rect_tl(Rect* self) 
(defcfun ("cv_Rect_tl" rect-tl) (cv:point :garbage-collect t)
  "Retrieves the top-left corner of a rectangle."
  (self cv:rect))


;; Point Rect::tl() const
;; Point* cv_Rect_tl(Rect* self) 
(defcfun ("cv_Rect_tl" tl) (cv:point :garbage-collect t)
  "Retrieves the top-left corner of a rectangle."
  (self cv:rect))


;; Mat Mat::reshape(int cn, int rows=0) const
;; Mat* cv_Mat_reshape(Mat* self, int cn) 
(defcfun ("cv_Mat_reshape" %reshape) (cv:mat :garbage-collect t)
	 (self cv:mat)
	 (cn :int))


;; Mat Mat::reshape(int cn, int rows=0) const
;; Mat* cv_Mat_reshape_rows(Mat* self, int cn, int rows) 
(defcfun ("cv_Mat_reshape_rows" reshape-rows) (cv:mat :garbage-collect t)
	 (self cv:mat)
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
(defcfun ("cv_Mat_get_ROI" roi) (cv:mat :garbage-collect t) 
	 "Returns matrix header corresponding to the rectangular sub-array of input MAT."
	 (self cv:mat)
	 (roi cv:rect))


;; RotatedRect(const Point2f& center, const Size2f& size, float angle)
;; RotatedRect* cv_create_RotatedRect(Point2f* center, Size2f* size, float angle)
(defcfun ("cv_create_RotatedRect" make-rotated-rect) (cv:rotated-rect :garbage-collect t)
	 (center cv:point)
	 (size cv:size)
	 (angle :float))


;; Rect RotatedRect::boundingRect() const
;; Rect* cv_RotatedRect_boundingRect(RotatedRect* self)
(defcfun ("cv_RotatedRect_boundingRect" rotated-rect-bounding-rect) (cv:rotated-rect :garbage-collect t)
	 "Returns the minimal up-right rectangle containing the rotated rectangle"
	 (self cv:rotated-rect))


;; Point2f center;
;; Point* cv_RotatedRect_center(RotatedRect* self) 
(defcfun ("cv_RotatedRect_center" rotated-rect-center) (cv:point :garbage-collect t)
	 (self cv:rotated-rect))


;; Size2f size;     
;; Size* cv_RotatedRect_size(RotatedRect* self) 
(defcfun ("cv_RotatedRect_size" rotated-rect-size) (cv:size :garbage-collect t)
	 (self cv:rotated-rect))


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


;; Mat Mat::row(int y) const
;; Mat* cv_Mat_getRow(Mat* self, int y) 
(defcfun ("cv_Mat_getRow" row) (cv:mat :garbage-collect t)
  (self cv:mat)
  (y :int))


;; Mat Mat::rowRange(int startrow, int endrow) const
;; Mat* cv_Mat_getRowRange(Mat* self, int startrow, int endrow)
(defcfun ("cv_Mat_getRowRange" row-range) (cv:mat :garbage-collect t)
	 "Creates a matrix header for the specified row span."
	 (self cv:mat)
	 (startrow :int)
	 (endrow :int))


;; Scalar::Scalar()
;; Scalar* cv_create_Scalar0()
(defcfun ("cv_create_Scalar0" scalar-0) (cv:scalar :garbage-collect t))


;; Scalar_<_Tp>::Scalar_(_Tp v0, _Tp v1, _Tp v2, _Tp v3)
;; Scalar* cv_create_Scalar4(double val0, (double val1, double val2, double val3)
(defcfun ("cv_create_Scalar4" scalar-4) (cv:scalar :garbage-collect t) 
	 (val0 :double)
	 (val1 :double)
	 (val2 :double)
	 (val3 :double))


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


;; Scalar_<_Tp> Scalar_<_Tp>::all(_Tp v0)
;; Scalar* cv_create_scalarAll(double val0123)
(defcfun ("cv_create_scalarAll" %scalar-all) (cv:scalar :garbage-collect t)
	 (val0123 :double))


(defun scalar-all (val0123)
       "SCALAR conctctor - initializes all of the scalar values 0...3 with val0123"
       (%scalar-all (coerce val0123 'double-float)))


(defun make-scalar-all (val0123)
       "SCALAR conctctor - initializes all of the scalar values 0...3 with val0123"
       (%scalar-all (coerce val0123 'double-float)))


;; Size_()
;; Size* cv_create_Size() 
(defcfun ("cv_create_Size" size-0) (cv:size :garbage-collect t)
	 "Create SIZE construct")


;; Size_(_Tp _width, _Tp _height)
;; cv_create_Size2(double width, double height)
(defcfun ("cv_create_Size2" size-2) (cv:size :garbage-collect t)
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


;; Size* cv_Size_fromPoint(Point* p)
(defcfun ("cv_Size_fromPoint" size-from-point) (cv:size :garbage-collect t)
	 "Create a SIZE construct from POINT data."
	 (p cv:point))


;; MatExpr - operator
;; MatExpr* cv_Mat_sub(Mat* m1, Mat* m2)
(defcfun ("cv_Mat_sub" sub) (cv:mat-expr :garbage-collect t)
	 (m1 cv:mat)
	 (m2 cv:mat))


;; TermCriteria::TermCriteria(int type, int maxCount, double epsilon)
;; TermCriteria* cv_create_TermCriteria(int type, int maxCount, double epsilon) 
(defcfun ("cv_create_TermCriteria" term-criteria-0) (cv:term-criteria :garbage-collect t))


;; TermCriteria::TermCriteria(int type, int maxCount, double epsilon)
;; TermCriteria* cv_create_TermCriteria(int type, int maxCount, double epsilon) 
(defcfun ("cv_create_TermCriteria3" term-criteria-3) (cv:term-criteria :garbage-collect t)
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


;; typedef Vec<uchar, 2> Vec2b;
;; Vec2##t * cv_create_0_Vec2##t()
(defcfun ("cv_create_0_Vec2b" vec-2b-0) (cv:vec-2b :garbage-collect t))


;; typedef Vec<uchar, 2> Vec2b;
;; Vec2##t * cv_create_Vec2##t(tn v0,  tn v1)
(defcfun ("cv_create_Vec2b" vec-2b-2) (cv:vec-2b :garbage-collect t)
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
(defcfun ("cv_create_0_Vec3b" vec-3b-0) (cv:vec-3b :garbage-collect t))


;; typedef Vec<uchar, 3> Vec3b;
;; Vec3##t * cv_create_Vec3##t(tn v0,  tn v1, tn v2)
(defcfun ("cv_create_Vec3b" vec-3b-3) (cv:vec-3b :garbage-collect t)
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
(defcfun ("cv_create_0_Vec4b" vec-4b-0) (cv:vec-4b :garbage-collect t))


;; typedef Vec<uchar, 4> Vec4b;
;; Vec4##t * cv_create_Vec4##(tn v0,  tn v1, tn v2, tn v3)
(defcfun ("cv_create_Vec4b" vec-4b-4) (cv:vec-4b :garbage-collect t)
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
(defcfun ("cv_create_0_Vec2d" vec-2d-0) (cv:vec-2d :garbage-collect t))


;; typedef Vec<double, 2> Vec2d;
;; Vec2##t * cv_create_Vec2##(tn v0,  tn v1)
(defcfun ("cv_create_Vec2d" vec-2d-2) (cv:vec-2d :garbage-collect t)
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
(defcfun ("cv_create_0_Vec3d" vec-3d-0) (cv:vec-3d :garbage-collect t))


;; typedef Vec<double, 3> Vec3d;
;; Vec3##t * cv_create_Vec3##(tn v0,  tn v1, tn v2)
(defcfun ("cv_create_Vec3d" vec-3d-3) (cv:vec-3d :garbage-collect t)
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
(defcfun ("cv_create_0_Vec4d" vec-4d-0) (cv:vec-4d :garbage-collect t))


;; typedef Vec<double, 4> Vec4d;
;; Vec4##t * cv_create_Vec4##(tn v0,  tn v1, tn v2, tn v3)
(defcfun ("cv_create_Vec4d" vec-4d-4) (cv:vec-4d :garbage-collect t)
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
(defcfun ("cv_create_0_Vec6d" vec-6d-0) (cv:vec-6d :garbage-collect t))


;; typedef Vec<double, 6> Vec6d;
;; Vec6##t * cv_create_Vec6##(tn v0,  tn v1, tn v2, tn v3, tn v4, tn v5)
(defcfun ("cv_create_Vec6d" vec-6d-6) (cv:vec-6d :garbage-collect t)
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
(defcfun ("cv_create_0_Vec2f" vec-2f-0) (cv:vec-2f :garbage-collect t))


;; typedef Vec<float, 2> Vec2f
;; Vec2##t * cv_create_Vec2##(tn v0,  tn v1)
(defcfun ("cv_create_Vec2f" vec-2f-2) (cv:vec-2f :garbage-collect t)
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
(defcfun ("cv_create_0_Vec3f" vec-3f-0) (cv:vec-3f :garbage-collect t))


;; typedef Vec<float, 3> Vec3f
;; Vec3##t * cv_create_Vec3##(tn v0,  tn v1, tn v2)
(defcfun ("cv_create_Vec3f" vec-3f-3) (cv:vec-3f :garbage-collect t)
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
(defcfun ("cv_create_0_Vec4f" vec-4f-0) (cv:vec-4f :garbage-collect t))


;; typedef Vec<float, 4> Vec4f
;; Vec4##t * cv_create_Vec4##(tn v0,  tn v1, tn v2, tn v3)
(defcfun ("cv_create_Vec4f" vec-4f-4) (cv:vec-4f :garbage-collect t) 
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
(defcfun ("cv_create_0_Vec6f" vec-6f-0) (cv:vec-6f :garbage-collect t))


;; typedef Vec<float, 6> Vec6f;
;; Vec6##t * cv_create_Vec6##(tn v0,  tn v1, tn v2, tn v3, tn v4, tn v5)
(defcfun ("cv_create_Vec6f" vec-6f-6) (cv:vec-6f :garbage-collect t)
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
(defcfun ("cv_create_0_Vec2i" vec-2i-0) (cv:vec-2i :garbage-collect t))


;; typedef Vec<int, 2> Vec2i;
;; Vec2##t * cv_create_Vec2##(tn v0,  tn v1)
(defcfun ("cv_create_Vec2i" vec-2i-2) (cv:vec-2i :garbage-collect t)
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
(defcfun ("cv_create_0_Vec3i" vec-3i-0) (cv:vec-3i :garbage-collect t))


;; typedef Vec<int, 3> Vec3i;
;; Vec3##t * cv_create_Vec3##(tn v0,  tn v1, tn v2)
(defcfun ("cv_create_Vec3i" vec-3i-3) (cv:vec-3i :garbage-collect t)
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
(defcfun ("cv_create_0_Vec4i" vec-4i-0) (cv:vec-4i :garbage-collect t))


;; typedef Vec<int, 4> Vec4i;
;; Vec4##t * cv_create_Vec4##(tn v0,  tn v1, tn v2, tn v3)
(defcfun ("cv_create_Vec4i" vec-4i-4) (cv:vec-4i :garbage-collect t)
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
(defcfun ("cv_create_0_Vec6i" vec-6i-0) (cv:vec-6i :garbage-collect t))


;; typedef Vec<int, 6> Vec6i;
;; Vec6##t * cv_create_Vec6##(tn v0,  tn v1, tn v2, tn v3, tn v4, tn v5)
(defcfun ("cv_create_Vec6i" vec-6i-6) (cv:vec-6i :garbage-collect t)
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
(defcfun ("cv_create_0_Vec8i" vec-8i-0) (cv:vec-8i :garbage-collect t))


;; typedef Vec<int, 8> Vec8i;
;; Vec8##t * cv_create_Vec8##(tn v0,  tn v1, tn v2, tn v3, tn v4, tn v5, tn v6, tn v7)
(defcfun ("cv_create_Vec8i" vec-8i-8) (cv:vec-8i :garbage-collect t)
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
(defcfun ("cv_create_0_Vec2s" vec-2s-0) (cv:vec-2s :garbage-collect t))


;; typedef Vec<short, 2> Vec2s;
;; Vec2##t * cv_create_Vec2##(tn v0,  tn v1)
(defcfun ("cv_create_Vec2s" vec-2s-2) (cv:vec-2s :garbage-collect t)
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
(defcfun ("cv_create_0_Vec3s" vec-3s-0) (cv:vec-3s :garbage-collect t))


;; typedef Vec<short, 3> Vec3s;
;; Vec3##t * cv_create_Vec3##(tn v0,  tn v1, tn v2)
(defcfun ("cv_create_Vec3s" vec-3s-3) (cv:vec-3s :garbage-collect t)
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
(defcfun ("cv_create_0_Vec4s" vec-4s-0) (cv:vec-4s :garbage-collect t))


;; typedef Vec<short, 4> Vec4s;
;; Vec4##t * cv_create_Vec4##(tn v0,  tn v1, tn v2, tn v3)
(defcfun ("cv_create_Vec4s" vec-4s-4) (cv:vec-4s :garbage-collect t)
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
(defcfun ("cv_create_0_Vec2w" vec-2w-0) (cv:vec-2w :garbage-collect t))


;; typedef Vec<ushort, 2> Vec2w;
;; Vec2##t * cv_create_Vec2##(tn v0,  tn v1)
(defcfun ("cv_create_Vec2w" vec-2w-2) (cv:vec-2w :garbage-collect t)
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
(defcfun ("cv_create_0_Vec3w" vec-3w-0) (cv:vec-3w :garbage-collect t))


;; typedef Vec<ushort, 3> Vec3w;
;; Vec3##t * cv_create_Vec3##(tn v0,  tn v1, tn v2)
(defcfun ("cv_create_Vec3w" vec-3w-3) (cv:vec-3w :garbage-collect t)
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
(defcfun ("cv_create_0_Vec4w" vec-4w-0) (cv:vec-4w :garbage-collect t))


;; typedef Vec<ushort, 4> Vec4w;
;; Vec4##t * cv_create_Vec4##(tn v0,  tn v1, tn v2, tn v3)
(defcfun ("cv_create_Vec4w" vec-4w-4) (cv:vec-4w :garbage-collect t)
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


;;; CORE - Basic Structures


;;; CORE - Operations on Arrays


;; MatExpr abs(const Mat& m)
;; MatExpr* cv_abs(Mat* m)
(defcfun ("cv_abs" %abs) (cv:mat-expr :garbage-collect t)
	 (m cv:mat))


(defmethod abs ((self cv:cv-mat))
  "Calculates an absolute value of each matrix element."
  (%abs self))


;; Scalar trace(InputArray mtx)
;; Scalar* cv_trace(Mat* mtx)
(defcfun ("cv_trace" trace*) (cv:scalar :garbage-collect t)
  "Returns the trace of a matrix."
  (mtx cv:mat))


;; MatExpr Mat::inv(int method=DECOMP_LU) const
;; MatExpr* cv_Mat_inv_mat(Mat* self, int method)
(defcfun ("cv_Mat_inv_mat" inv) (cv:mat-expr :garbage-collect t) 
	 "Inverses a matrix."
	 (self cv:mat)
	 (method :int))


;; Scalar mean(InputArray src, InputArray mask=noArray())
;; Scalar* cv_mean(Mat* src, Mat* mask)
(defcfun ("cv_mean" %%mean) (cv:scalar :garbage-collect t)
	 (src cv:mat)
	 (mask cv:mat))


(defun %mean (src &optional (mask (cv:%mat) given-mask) return)
  "Calculates an average mean of matrix elements."
  (setf return (%%mean src mask))
  (if given-mask nil (cv:del-mat mask))
  return)


(defmethod mean ((self cv:cv-mat) &rest args)
  "Calculates an average mean of matrix elements."
  (apply #'%mean self args))


;; PCA::PCA()
;; PCA* cv_create_PCA()
(defcfun ("cv_create_PCA" pca-0) (cv:pca :garbage-collect t))


;; PCA::PCA(InputArray data, InputArray mean, int flags, double retainedVariance)
;; PCA* cv_create_PCA4(Mat* data, Mat* mean, int flags, double retainedVariance) 
(defcfun ("cv_create_PCA4" pca-4) (cv:pca :garbage-collect t)
  (data cv:mat)
  (mean cv:mat)
  (flags :int)
  (retained-variance :double))


(defun pca (&optional data mean flags retained-variance)
  "PCA constructor"
  (cond (data
	 (pca-4 data mean flags retained-variance))
	(t
	 (pca-0))))


(defun make-pca (&optional data mean flags retained-variance)
  "PCA constructor"
  (cond (data
	 (pca-4 data mean flags retained-variance))
	(t
	 (pca-0))))


;; Mat PCA::eigenvalues
;; Mat* cv_PCA_get_eigenvalues(PCA* self)
(defcfun ("cv_PCA_get_eigenvalues" pca-eigenvalues) (cv:mat :garbage-collect t) 
  "Gets the eigenvalues of the covariation matrix(PCA)."
  (self cv:pca))
 

;; Mat PCA::eigenvalues
;; Mat* cv_PCA_get_eigenvalues(PCA* self)
(defcfun ("cv_PCA_get_eigenvalues" eigenvalues) (cv:mat :garbage-collect t) 
  "Gets the eigenvalues of the covariation matrix(PCA)."
  (self cv:pca))


;; Mat* cv_PCA_set_Eigenvalues(PCA* self, Mat* val)
(defcfun ("cv_PCA_set_Eigenvalues" pca-set-eigenvalues) (cv:mat :garbage-collect t) 
  "Sets the eigenvalues of the covariation matrix(PCA)."
  (self cv:pca)
  (val cv:mat))


(defun (setf pca-eigenvalues) (val self)
  (pca-set-eigenvalues self val))


(defun (setf eigenvalues) (val self)
  (pca-set-eigenvalues self val))


;; Mat PCA::eigenvectors
;; Mat* cv_PCA_get_eigenvectors(PCA* self)
(defcfun ("cv_PCA_get_eigenvectors" pca-eigenvectors) (cv:mat :garbage-collect t) 
  "Gets the eigenvectors of the covariation matrix(PCA)."
  (self cv:pca))

 
;; Mat PCA::eigenvectors
;; Mat* cv_PCA_get_eigenvectors(PCA* self)
(defcfun ("cv_PCA_get_eigenvectors" eigenvectors) (cv:mat :garbage-collect t) 
  "Gets the eigenvectors of the covariation matrix(PCA)."
  (self cv:pca))


;; Mat* cv_PCA_set_Eigenvectors(PCA* self, Mat* val)
(defcfun ("cv_PCA_set_Eigenvectors" pca-set-eigenvectors) (cv:mat :garbage-collect t) 
  "Sets the eigenvectors of the covariation matrix(PCA)."
  (self cv:pca)
  (val cv:mat))


(defun (setf pca-eigenvectors) (val self)
  (pca-set-eigenvectors self val))


(defun (setf eigenvectors) (val self)
  (pca-set-eigenvectors self val))


;; Mat PCA::mean
;; Mat* cv_PCA_get_Mean(PCA* self)
(defcfun ("cv_PCA_get_Mean" pca-mean) (cv:mat :garbage-collect t) 
  "This function gets the mean value subtracted before 
   the projection and added after the back projection.
   (PCA)"
  (self cv:pca))
 
 
;; Mat* cv_PCA_set_Mean(PCA* self, Mat* val)
(defcfun ("cv_PCA_set_Mean" pca-set-mean) (cv:mat :garbage-collect t) 
  "This function sets the mean value subtracted before 
   the projection and added after the back projection.
   (PCA)"
  (self cv:pca)
  (val cv:mat))


(defun (setf pca-mean) (val self)
  (pca-set-mean self val))


(defmethod mean ((self cv:cv-pca) &rest args)
  args
  (pca-mean self))


(defmethod (setf mean) ((val cv:cv-mat) (self cv:cv-pca))
  (pca-set-mean self val))


;; Mat PCA::backProject(InputArray vec) const
;; Mat* cv_PCA_BackProject1(PCA* self, Mat* vec)
(defcfun ("cv_PCA_BackProject1" pca-back-project-1) (cv:mat :garbage-collect t) 
  "Reconstructs vectors from their PC projections."
  (self cv:pca)
  (vec cv:mat))


(defun pca-back-project (self vec &optional result)
  (if result
      (cv:pca-back-project-2 self vec result)
      (pca-back-project-1 self vec)))


(defun back-project (self vec &optional result)
  (if result
      (cv:pca-back-project-2 self vec result)
      (pca-back-project-1 self vec)))


;; Mat PCA::project(InputArray vec) const
;; Mat* cv_PCA_project1(PCA* self, Mat* vec)
(defcfun ("cv_PCA_Project1" pca-project-1) (cv:mat :garbage-collect t) 
  "Projects vector(s) to the principal component subspace."
  (self cv:pca)
  (vec cv:mat))


(defun pca-project (self vec &optional result)
  (if result
      (cv:pca-project-2 self vec result)
      (pca-project-1 self vec)))


(defun project (self vec &optional result)
  (if result
      (cv:pca-project-2 self vec result)
      (pca-project-1 self vec)))


;; RNG::RNG()
(defcfun ("cv_create_RNG" %rng) (cv:rng :garbage-collect t) 
	 "RNG constructor")


;; RNG::RNG(uint64 state)
;; RNG* cv_create_RNG_state(uint64 state)
(defcfun ("cv_create_RNG_state" rng-state) (cv:rng :garbage-collect t) 
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


;; Scalar sum(InputArray src)
;; Scalar* cv_sum(Mat* src)
(defcfun ("cv_sum" sum) (cv:scalar :garbage-collect t) 
	 "Calculates the sum of array elements."
	 (src cv:mat))



;;; CORE - Drawing Functions


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


;;; CORE - XML/YAML Persistence


;; FileStorage::FileStorage()
;; FileStorage* cv_create_FileStorage()
(defcfun ("cv_create_FileStorage" file-storage-0) (cv:file-storage :garbage-collect t))


;; FileStorage::FileStorage(const String& source, int flags, const String& encoding=String())
;; FileStorage* cv_create_FileStorage3(String* source, int flags, String* encoding)
(defcfun ("cv_create_FileStorage3" %file-storage-3) (cv:file-storage :garbage-collect t) 
  (source cv:string*)
  (flags :int)
  (encoding cv:string*))


(defun file-storage-3 (&optional source flags (encoding (cv:%string) given-encoding))
  (let ((return (%file-storage-3 (c-string-to-string source (length source)) flags 
				 (if given-encoding 
				     (c-string-to-string encoding (length encoding)) 
				     encoding))))
    (if given-encoding nil (cv:del-std-string encoding))
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


;;; CORE - Utility and System Functions and Macros


;;; IMGPROC - Macros


;; static inline Scalar morphologyDefaultBorderValue() { return Scalar::all(DBL_MAX); }
;; Scalar* cv_create_morphologyDefaultBorderValue()
(defcfun ("cv_create_morphologyDefaultBorderValue" morphology-default-border-value) (cv:scalar :garbage-collect t))


;; static inline Scalar morphologyDefaultBorderValue() { return Scalar::all(DBL_MAX); }
;; Scalar* cv_create_morphologyDefaultBorderValue()
(defcfun ("cv_create_morphologyDefaultBorderValue" make-morphology-default-border-value) (cv:scalar :garbage-collect t))


;;; IMGPROC - Image Filtering


;; Mat getStructuringElement(int shape, Size ksize, Point anchor=Point(-1,-1))
;; Mat* cv_getStructuringElement(int shape, Size* ksize, Point* anchor)
(defcfun ("cv_getStructuringElement" %get-structuring-element) (cv:mat :garbage-collect t)
	 (shape :int)
	 (ksize cv:size)
	 (kernel cv:point))


(defun get-structuring-element (shape ksize &optional (kernel (cv:point -1 -1) given-kernel) return) 
  "Returns a structuring element of the specified size and shape for morphological operations."
  (setf return (%get-structuring-element shape ksize kernel))
  (if given-kernel nil (cv:del-point kernel)) 
  return)


;;; IMGPROC - Geometric Image Transformations


;; Mat getAffineTransform(InputArray src, InputArray dst)
;; Mat* cv_getAffineTransform(Mat* src, Mat* dst) 
(defcfun ("cv_getAffineTransform" get-affine-transform) (cv:mat :garbage-collect t)
  "Calculates an affine transform from three pairs of the corresponding points."
  (src cv:mat)
  (dest cv:mat))


;; Mat getPerspectiveTransform(InputArray src, InputArray dst)
;; Mat* cv_getPerspectiveTransform(Mat* src, Mat* dst)
(defcfun ("cv_getPerspectiveTransform" get-perspective-transform) (cv:mat :garbage-collect t)
  "Calculates a perspective transform from four pairs of the corresponding points."
  (src cv:mat)
  (dest cv:mat))


;; Mat getRotationMatrix2D(Point2f center, double angle, double scale)
;; Mat* cv_getRotationMatrix2D(Point2f* center, double angle, double scale)
(defcfun ("cv_getRotationMatrix2D" get-rotation-matrix-2d) (cv:mat :garbage-collect t)
  "Calculates an affine matrix of 2D rotation."
  (center cv:point-2f)
  (angle :double)  
  (scale :double))


;;; Motion Analysis and Object Tracking


;; Point2d phaseCorrelate(InputArray src1, InputArray src2, InputArray window=noArray(), double* response=0)
;; Point2d* cv_phaseCorrelate(Mat* src1, Mat* src2, Mat* window, double* response) 
(defcfun ("cv_phaseCorrelate" %phase-correlate) (cv:point-2d :garbage-collect t)
  (src-1 cv:mat)
  (src-2 cv:mat)
  (window cv:mat)
  (response :pointer))


(defun phase-correlate (src-1 src-2 &optional (window (mat) given-window) (response (null-pointer)))
       (let ((return (%phase-correlate src-1 src-2 window response)))
	 (if given-window nil (cv:del-mat window))
	 return))


;;; HIGHGUI


;;; HIGHGUI - User Interface


;;; HIGHGUI - Reading and Writing Images and Video


;; Mat imdecode(InputArray buf, int flags)
;; Mat* cv_imdecode_2(vector_uchar* buf, int flags) 
(defcfun ("cv_imdecode_2" imdecode) (cv:mat :garbage-collect t) 
  (buf cv:vector-uchar)
  (flags :int))


;; VideoCapture::VideoCapture()
;; VideoCapture* cv_create_VideoCapture() 
(defcfun ("cv_create_VideoCapture" video-capture-0) (cv:video-capture :garbage-collect t) 
	 "VideoCapture constructor")


;; VideoCapture::VideoCapture(int device)
;; VideoCapture* cv_create_VideoCapture1_0(int device)
(cffi:defcfun ("cv_create_VideoCapture1_0" video-capture-dev) (cv:video-capture :garbage-collect t)
	      "VideoCapture constructor"
	      (device :int))


;; VideoCapture::VideoCapture(const string& filename)
;; VideoCapture* cv_create_VideoCapture1(String* filename) 
(defcfun ("cv_create_VideoCapture1" video-capture-file) (cv:video-capture :garbage-collect t)
	 "VideoCapture constructor"
	 (filename cv:string*))


(defun video-capture (&optional src)
       (cond ((eq src nil)
	      (video-capture-0))
	      ((numberp src)
	       (video-capture-dev src))
	       ((stringp src) 
		(video-capture-file (c-string-to-string src (length src))))
		(t nil)))


(defun make-video-capture (&optional src)
       (cond ((eq src nil)
	      (video-capture-0))
	      ((numberp src)
	       (video-capture-dev src))
	       ((stringp src) 
		(video-capture-file (c-string-to-string src (length src))))
		(t nil)))


;; Mat imread(const string& filename, int flags=1)
;; mat cv_imread (const char* filename, int flags)
(defcfun ("cv_imread" %imread) (cv:mat :garbage-collect t)
	 (filename cv:string*)
	 (flags :int))


(defun imread (filename &optional (flags 1))
       (%imread (c-string-to-string filename (length filename)) flags))


;; VideoWriter* cv_create_VideoWriter() 
(defcfun ("cv_create_VideoWriter" video-writer-0) (cv:video-writer :garbage-collect t)
	 "VIDEO-WRITER constructor")


;; VideoWriter::VideoWriter(const string& filename, int fourcc, double fps, Size frameSize, bool isColor) 
;; VideoWriter* cv_create_VideoWriter5(String* filename, int fourcc, double fps, Size* frameSize, bool isColor)
(defcfun ("cv_create_VideoWriter5" video-writer-5) (cv:video-writer :garbage-collect t)
	 (filename cv:string*)
	 (fourcc :int)
	 (fps :double)
	 (frame-size cv:size)
	 (is-color :boolean))


(defun video-writer (&optional filename fourcc fps frame-size (is-color t))
       "VIDEO-WRITER constructor"  
       (cond ((eq filename nil)
	      (video-writer-0))
	      (filename
	       (video-writer-5 (c-string-to-string filename (length filename)) fourcc fps frame-size is-color))
	       (t nil)))


(defun make-video-writer (&optional filename fourcc fps frame-size (is-color t))
       "VIDEO-WRITER constructor"  
       (cond ((eq filename nil)
	      (video-writer-0))
	      (filename
	       (video-writer-5 (c-string-to-string filename (length filename)) fourcc fps frame-size is-color))
	       (t nil)))


;;; HIGHGUI - Qt New Functions


;;; FEATURES2D


;;; FEATURES2D - Feature Detection and Description


;; BFMatcher::BFMatcher(int normType=NORM_L2, bool crossCheck=false )
;; BFMatcher* cv_create_BFMatcher(int normType, bool crossCheck) 
(defcfun ("cv_create_BFMatcher" %bf-matcher) (cv:bf-matcher :garbage-collect t)
	 (norm-type :int)
	 (cross-check :boolean))


(defun bf-matcher (&optional (norm-type cv:+norm-l2+) (cross-check nil))
       "Brute-force matcher constructor."
       (%bf-matcher norm-type cross-check))


(defun make-bf-matcher (&optional (norm-type cv:+norm-l2+) (cross-check nil))
       "Brute-force matcher constructor."
       (%bf-matcher norm-type cross-check))


;; BRISK::BRISK(int thresh=30, int octaves=3, float patternScale=1.0f)
;; BRISK* cv_create_BRISK(int thresh, int octaves, float patternScale)
(defcfun ("cv_create_BRISK" %brisk) (cv:brisk :garbage-collect t)
	 (thresh :int)
	 (octaves :int)
	 (pattern-scale :float))


(defun brisk (&optional (thresh 30) (octaves 3) (pattern-scale 1.0f0))
       "The BRISK constructor"
       (%brisk thresh octaves pattern-scale))


(defun make-brisk (&optional (thresh 30) (octaves 3) (pattern-scale 1.0f0))
       "The BRISK constructor"
       (%brisk thresh octaves pattern-scale))


;;; FEATURES2D - Common Interfaces of Feature Detectors


;;; FEATURES2D - Common Interfaces of Descriptor Extractors


;;; FEATURES2D - Common Interfaces of Descriptor Matchers


;;; OBJDETECT


;;; OBJDETECT - Cascade Classification


;; CascadeClassifier::CascadeClassifier()
;; CascadeClassifier* cv_create_CascadeClassifier() 
(defcfun ("cv_create_CascadeClassifier" cascade-classifier-0) (cv:cascade-classifier :garbage-collect t)
	 "CASCADE-CLASSIFIER construct.")


;; CascadeClassifier::CascadeClassifier(const string& filename)
;; CascadeClassifier* cv_create_CascadeClassifier1(String* filename)
(defcfun ("cv_create_CascadeClassifier1" cascade-classifier-1) (cv:cascade-classifier :garbage-collect t)
	 "Loads a classifier from a file."
	 (filename cv:string*))


(defun cascade-classifier (&optional filename)
       (cond ((eq filename nil)
	      (cascade-classifier-0))
	      (filename
	       (cascade-classifier-1 (c-string-to-string filename (length filename))))
	       (t nil)))


(defun make-cascade-classifier (&optional filename)
       (cond ((eq filename nil)
	      (cascade-classifier-0))
	      (filename
	       (cascade-classifier-1 (c-string-to-string filename (length filename))))
	       (t nil)))



;;; ML


;;; ML - LISP-CV specific

(declaim (ftype (function (string symbol cv:cv-size &optional boolean)
                          cv:cv-mat) make-training-matrix))

(defun make-training-matrix (directory directory-contents dsize &optional (test nil))

  (let* ((pathname-list (cv:make-pathname-list :directory directory 
					    :directory-contents directory-contents))
	 (num-of-files (cv:length pathname-list))
	 (img-height (round (cv:size-height dsize)))
	 (img-width (round (cv:size-width dsize)))
	 (img-area (* img-height img-width))
         (training-data (mat 0 img-area cv:+32f+))
         (i 0))

    (dotimes (n num-of-files)
      (cv:with-mat ((img (cv:imread (nth n pathname-list) cv:+load-image-grayscale+)))
	(cv:convert-to img img cv:+32f+)
	(cv:%resize img img dsize 0d0 0d0 cv:+inter-linear+)
	(cv:with-mat ((reshaped-img (cv:reshape-rows img 0 1)))
	  (cv:push-back training-data reshaped-img) 
	  (princ #\NewLine) 
	  (princ "Pushing image ")
	  (princ (+ n 1))
	  (princ " into training matrix.")
	  (princ #\NewLine))))
    (if (null test)
	(return-from make-training-matrix training-data)
	(let ((window-name "Testing..."))
	  (cv:with-named-window (window-name cv:+window-normal+)
	    (cv:move-window window-name 759 175)
	    (loop
	       (let* ((row (cv:row training-data i))
		      (reshaped-img (cv:reshape-rows row 0 img-height)))
		 (cv:imshow window-name reshaped-img)
		 (cv:del-mat reshaped-img)
		 (cv:del-mat row))
	       (if (< i (- num-of-files 1)) (incf i))
	       (let ((c (cv:wait-key 1)))
		 (when (or (= c 27) (= i (- num-of-files 1)))
		   (return-from make-training-matrix 
		     training-data)
		   (return)))))))))


;;; ML - Normal Bayes Classifier


;; CvNormalBayesClassifier::CvNormalBayesClassifier()
;; CvNormalBayesClassifier* cv_create_CvNormalBayesClassifier()
(defcfun ("cv_create_CvNormalBayesClassifier" normal-bayes-classifier-0) (cv:normal-bayes-classifier :garbage-collect t))


;; CvNormalBayesClassifier::CvNormalBayesClassifier(const Mat& trainData, const Mat& responses, const Mat& varIdx=Mat(), 
;;                                                  const Mat& sampleIdx=Mat() )
;; CvNormalBayesClassifier* cv_create_CvNormalBayesClassifier4(Mat* trainData, Mat* responses, Mat* varIdx, Mat* sampleIdx) 
(defcfun ("cv_create_CvNormalBayesClassifier4" normal-bayes-classifier-4) (cv:normal-bayes-classifier :garbage-collect t)
  (train-data cv:mat)
  (responses cv:mat)
  (var-idx cv:mat)
  (sample-idx cv:mat))


(defun normal-bayes-classifier (&optional train-data responses  (var-idx (cv:%mat) given-var-idx) (sample-idx (cv:%mat) given-sample-idx))
  (let ((return (if train-data
		    (normal-bayes-classifier-4 train-data responses  var-idx sample-idx)
		    (normal-bayes-classifier-0))))
    (if given-var-idx nil (cv:del-mat var-idx))
    (if given-sample-idx nil (cv:del-mat sample-idx)) 
    return))


(defun make-normal-bayes-classifier (&optional train-data responses  (var-idx (cv:%mat) given-var-idx) (sample-idx (cv:%mat) given-sample-idx))
  (let ((return (if train-data
		    (normal-bayes-classifier-4 train-data responses  var-idx sample-idx)
		    (normal-bayes-classifier-0))))
    (if given-var-idx nil (cv:del-mat var-idx))
    (if given-sample-idx nil (cv:del-mat sample-idx)) 
    return))


;;; ML - K-Nearest Neighbors



;; CvKNearest::CvKNearest()
;; CvKNearest* cv_create_CvKNearest() 
(defcfun ("cv_create_CvKNearest" k-nearest-0) (cv:k-nearest :garbage-collect t))


;; CvKNearest::CvKNearest(const CvMat* trainData, const CvMat* responses, const CvMat* sampleIdx=0, bool isRegression=false, 
;;                        int max_k=32 )
;; CvKNearest* cv_create_CvKNearest5(Mat* trainData, Mat* responses, Mat* sampleIdx, bool isRegression, int max_k) 
(defcfun ("cv_create_CvKNearest5" k-nearest-5) (cv:k-nearest :garbage-collect t)
  (train-data cv:mat)
  (responses cv:mat)
  (sample-idx cv:mat)
  (is-regression :boolean)
  (max-k :int))


(defun k-nearest (&optional train-data responses (sample-idx (null-pointer)) (is-regression nil) (max-k 32))
  (if train-data
      (k-nearest-5 train-data responses sample-idx is-regression max-k)
      (k-nearest-0)))


(defun make-k-nearest (&optional train-data responses (sample-idx (null-pointer)) (is-regression nil) (max-k 32))
  (if train-data
      (k-nearest-5 train-data responses sample-idx is-regression max-k)
      (k-nearest-0)))



;;; ML - Support Vector Machines


;; CvSVM::CvSVM()
;; CvSVM* cv_create_CvSVM() 
(defcfun ("cv_create_CvSVM" svm-0) (cv:svm :garbage-collect t))


;; CvSVM::CvSVM(const Mat& trainData, const Mat& responses, const Mat& varIdx=Mat(), const Mat& sampleIdx=Mat(), 
;;              CvSVMParams params=CvSVMParams() )
;; CvSVM* cv_create_CvSVM5(Mat* trainData, Mat* responses, Mat* varIdx, Mat* sampleIdx, CvSVMParams* params)
(defcfun ("cv_create_CvSVM5" svm-5) (cv:svm :garbage-collect t)
  (train-data cv:mat)
  (responses cv:mat)
  (var-idx cv:mat)
  (sample-idx cv:mat)
  (params cv:svm-params))


(defun svm (&optional train-data responses (var-idx (cv:%mat) given-var-idx) (sample-idx (cv:%mat) given-sample-idx)
	      (params (cv:svm-params-0) given-params))
  (let ((return (if train-data 
		    (svm-5 train-data responses var-idx sample-idx params)
		    (svm-0))))
    (if given-var-idx nil (cv:del-mat var-idx))
    (if given-sample-idx nil (cv:del-mat sample-idx))
    (if given-params nil (cv:del-svm-params params))
    return))


(defun make-svm (&optional train-data responses (var-idx (cv:%mat) given-var-idx) (sample-idx (cv:%mat) given-sample-idx)
	      (params (cv:svm-params-0) given-params))
  (let ((return (if train-data 
		    (svm-5 train-data responses var-idx sample-idx params)
		    (svm-0))))
    (if given-var-idx nil (cv:del-mat var-idx))
    (if given-sample-idx nil (cv:del-mat sample-idx))
    (if given-params nil (cv:del-svm-params params))
    return))


;; CvSVMParams::CvSVMParams()
;; CvSVMParams* cv_create_CvSVMParams() 
(defcfun ("cv_create_CvSVMParams" svm-params-0) (cv:svm-params :garbage-collect t))


;;  CvSVMParams::CvSVMParams(int svm_type, int kernel_type, double degree, double gamma, double coef0, double Cvalue, double nu, 
;;                           double p, CvMat* class_weights, CvTermCriteria term_crit)
;; CvSVMParams* cv_create_CvSVMParams10(int svm_type, int kernel_type, double degree, double gamma, double coef0, double Cvalue, 
;;                                      double nu, double p, CvMat* class_weights, CvTermCriteria term_crit)
(defcfun ("cv_create_CvSVMParams10" svm-params-10) (cv:svm-params :garbage-collect t)
  (svm-type :int)
  (kernel-type :int)
  (degree :double)
  (gamma :double)
  (coef-0 :double)
  (c-value :double)
  (nu :double)
  (p :double)
  (class-weights cv:mat-struct)
  (term-crit (:pointer (:struct cv:term-criteria-struct))))


(defun svm-params (&optional svm-type kernel-type degree gamma coef-0 c-value nu p class-weights term-crit)
  (cond ((null svm-type)
	 (svm-params-0))
	(svm-type
	 (svm-params-10 svm-type kernel-type degree gamma coef-0 c-value nu p class-weights term-crit))
	(t nil)))


(defun make-svm-params (&optional svm-type kernel-type degree gamma coef-0 c-value nu p class-weights term-crit)
  (cond ((null svm-type)
	 (svm-params-0))
	(svm-type
	 (svm-params-10 svm-type kernel-type degree gamma coef-0 c-value nu p class-weights term-crit))
	(t nil)))


;;; ML - Decision Trees


;; CvDTree:CvDTree()
;; CvDTree* cv_create_CvDTree() 
(defcfun ("cv_create_CvDTree" d-tree) (cv:d-tree :garbage-collect t))


;; CvDTree:CvDTree()
;; CvDTree* cv_create_CvDTree() 
(defcfun ("cv_create_CvDTree" make-d-tree) (cv:d-tree :garbage-collect t))


;; CvDTreeParams::CvDTreeParams()
;; CvDTreeParams* cv_create_CvDTreeParams()
(defcfun ("cv_create_CvDTreeParams" d-tree-params-0) (cv:d-tree-params :garbage-collect t))


;; CvDTreeParams::CvDTreeParams(int max_depth, int min_sample_count, float regression_accuracy, bool use_surrogates, 
;;                              int max_categories, int cv_folds, bool use_1se_rule, bool truncate_pruned_tree, const float* priors)
;; CvDTreeParams* cv_create_CvDTreeParams9(int max_depth, int min_sample_count, float regression_accuracy, bool use_surrogates, 
;;                                         int max_categories, int cv_folds, bool use_1se_rule, bool truncate_pruned_tree, 
;;                                         const float* priors)
(defcfun ("cv_create_CvDTreeParams9" d-tree-params-9) (cv:d-tree-params :garbage-collect t)
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
      (d-tree-params-9 max-depth min-sample-count regression-accuracy use-surrogates max-categories folds use-1se-rule
                      truncate-pruned-tree priors)
      (d-tree-params-0)))



;;; ML - Neural Networks


;; CvANN_MLP::CvANN_MLP()
;; CvANN_MLP* cv_create_CvANN_MLP()
(defcfun ("cv_create_CvANN_MLP" ann-mlp-0) (cv:ann-mlp :garbage-collect t))


;; CvANN_MLP::CvANN_MLP(const CvMat* layerSizes, int activateFunc=CvANN_MLP::SIGMOID_SYM, double fparam1=0, double fparam2=0 )
;; CvANN_MLP* cv_create_CvANN_MLP4(Mat* layerSizes, int activateFunc, double fparam1, double fparam2) 
(defcfun ("cv_create_CvANN_MLP4" ann-mlp-4) (cv:ann-mlp :garbage-collect t)
  (layer-sizes cv:mat)
  (activate-func :int)
  (fparam1 :double)
  (fparam2 :double))


(defun ann-mlp (&optional layer-sizes (activate-func cv:+ann-mlp-sigmoid-sym+) (fparam1 0d0) (fparam2 0d0))
  (if layer-sizes
      (ann-mlp-4 layer-sizes activate-func fparam1 fparam2)
      (ann-mlp-0)))


(defun make-ann-mlp (&optional layer-sizes (activate-func cv:+ann-mlp-sigmoid-sym+) (fparam1 0d0) (fparam2 0d0))
  (if layer-sizes
      (ann-mlp-4 layer-sizes activate-func fparam1 fparam2)
      (ann-mlp-0)))


;; CvANN_MLP_TrainParams::CvANN_MLP_TrainParams()
;; CvANN_MLP_TrainParams* cv_create_CvANN_MLP_TrainParams()
(defcfun ("cv_create_CvANN_MLP_TrainParams" ann-mlp-train-params-0) (cv:ann-mlp-train-params :garbage-collect t))


;; CvANN_MLP_TrainParams::CvANN_MLP_TrainParams(CvTermCriteria term_crit, int train_method, double param1, double param2=0 )
;; CvANN_MLP_TrainParams* cv_create_CvANN_MLP_TrainParams4(TermCriteria* term_crit, int train_method, double param1, double param2)
(defcfun ("cv_create_CvANN_MLP_TrainParams" ann-mlp-train-params-4) (cv:ann-mlp-train-params :garbage-collect t)
  (term-crit cv:term-criteria)
  (train-method :int)
  (param1 :double)
  (param2 :double))


(defun ann-mlp-train-params (&optional term-crit train-method param1 (param2 0))
       (if term-crit
	   (ann-mlp-train-params-4 term-crit train-method param1 param2)
	   (ann-mlp-train-params-0)))


(defun make-ann-mlp-train-params (&optional term-crit train-method param1 (param2 0))
       (if term-crit
	   (ann-mlp-train-params-4 term-crit train-method param1 param2)
	   (ann-mlp-train-params-0)))



;;; NON-FREE - Feature Detection and Description


;; SURF::SURF()
;; SURF* cv_create_SURF() 
(defcfun ("cv_create_SURF" surf-0) (cv:surf :garbage-collect t))


;; SURF::SURF(double hessianThreshold, int nOctaves=4, int nOctaveLayers=2, bool extended=true, bool upright=false )
;; SURF* cv_create_SURF5(double hessianThreshold, int nOctaves, int nOctaveLayers, bool extended, bool upright)
(defcfun ("cv_create_SURF5" surf-5) (cv:surf :garbage-collect t)
	 (hessian-threshold :double)
	 (n-octaves :int)
	 (n-octave-layers :int)
	 (extended :boolean)
	 (upright :boolean))


(defun surf (&optional hessian-threshold (n-octaves 4) (n-octave-layers 2) (extended t) (upright nil))
       (cond ((eq hessian-threshold nil)
	      (surf-0))
	      (hessian-threshold
	       (surf-5 hessian-threshold n-octaves n-octave-layers extended upright))
	       (t nil)))


(defun make-surf (&optional hessian-threshold (n-octaves 4) (n-octave-layers 2) (extended t) (upright nil))
       (cond ((eq hessian-threshold nil)
	      (surf-0))
	      (hessian-threshold
	       (surf-5 hessian-threshold n-octaves n-octave-layers extended upright))
	       (t nil)))



;;; VECTORS



;; template < class T, class Alloc = allocator<T> > class vector.
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorc" make-vector-char) cv:vector-char)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorc" c-arr-to-vec-char) cv:vector-char
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-char (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :char :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-char (cdr previous) (length a))))


(defun vector-char (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-CHAR")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-char))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-char x))
	     ((eq :length x)
	      (cv:vec-char-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-char-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-char-to-lisp-vec y z))
	     ((typep x 'cv:std-vector-char)
	      (if (eq y nil)
		  (mem-aref (cv:vec-char-to-c-arr x) :char) 
		  (mem-aref (cv:vec-char-to-c-arr x) :char y)))
	     (t (error "incorrect input. 
                   ~%See VECTOR-CHAR documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectordm" make-vector-dmatch) cv:vector-dmatch)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectordm" c-arr-to-vec-dmatch) cv:vector-dmatch
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-dmatch (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-dmatch (cdr previous) (length a))))


(defun vector-dmatch (&rest args)
  (if (fifth args)
      (error "odd number of args to VECTOR-DMATCH")
      nil)
    (let ((w (first args))
           (x (second args))
	   (y (third args))
           (z (fourth args)))
       (cond ((eq w nil)
	      (make-vector-dmatch))
	     ((and (or (vectorp w) (listp w)) (null x))
	      (arr-to-vec-dmatch w))
	     ((eq :length w)
	      (cv:vec-dmatch-length x))
	     ((and (eq :to-lisp-list w))
	      (cv:vec-dmatch-to-lisp-list x))
	     ((and (eq :to-lisp-vec w))
	      (cv:vec-dmatch-to-lisp-vec x))
	     ((and (typep w 'cv:std-vector-dmatch) x)
	      (if (eq y nil)
		  (mem-aref (cv:vec-dmatch-to-c-arr w) 'cv:dmatch x)
		  (if z
		      (mem-aref (cv:c-pointer (mem-aref (cv:vec-dmatch-to-c-arr w) 'cv:dmatch x)) z y)
		      (error "must supply a type as the fourth parameter")))) 
	     (t (error "incorrect input. 
  ~%See VECTOR-DMATCH documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectord" make-vector-double) cv:vector-double)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectord" c-arr-to-vec-double) cv:vector-double
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-double (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :double :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-double (cdr previous) (length a))))


(defun vector-double (&rest args)
  (if (third args)
      (error "odd number of args to VECTOR-DOUBLE")
      nil)
    (let ((x (first args))
	   (y (second args)))
       (cond ((null x)
	      (make-vector-double))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-double x))
	     ((eq :length x)
	      (cv:vec-double-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-double-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-double-to-lisp-vec y))
	     ((typep x 'cv:std-vector-double)
	      (if (eq y nil)
		  (mem-aref (cv:vec-double-to-c-arr x) :double) 
		  (mem-aref (cv:vec-double-to-c-arr x) :double y)))
	     (t (error "incorrect input. 
                   ~%See VECTOR-DOUBLE documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorf" make-vector-float) cv:vector-float)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorf" c-arr-to-vec-float) cv:vector-float
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-float (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :float :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-float (cdr previous) (length a))))


(defun vector-float (&rest args)
  (if (third args)
      (error "odd number of args to VECTOR-FLOAT")
      nil)
    (let ((x (first args))
	   (y (second args)))
       (cond ((null x)
	      (make-vector-float))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-float x))
	     ((eq :length x)
	      (cv:vec-float-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-float-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-float-to-lisp-vec y))
	     ((typep x 'cv:std-vector-float)
	      (if (eq y nil)
		  (mem-aref (cv:vec-float-to-c-arr x) :float) 
		  (mem-aref (cv:vec-float-to-c-arr x) :float y)))
	     (t (error "incorrect input. 
                   ~%See VECTOR-FLOAT documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectori" make-vector-int) cv:vector-int)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectori" c-arr-to-vec-int) cv:vector-int
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-int (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :int :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-int (cdr previous) (length a))))


(defun vector-int (&rest args)
  (if (third args)
      (error "odd number of args to VECTOR-INT")
      nil)
    (let ((x (first args))
	   (y (second args)))
       (cond ((null x)
	      (make-vector-int))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-int x))
	     ((eq :length x)
	      (cv:vec-int-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-int-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-int-to-lisp-vec y))
	     ((typep x 'cv:std-vector-int)
	      (if (eq y nil)
		  (mem-aref (cv:vec-int-to-c-arr x) :int) 
		  (mem-aref (cv:vec-int-to-c-arr x) :int y)))
	     (t (error "incorrect input. 
                   ~%See VECTOR-INT documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorkp" make-vector-key-point) cv:vector-key-point)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorkp" c-arr-to-vec-key-point) cv:vector-key-point
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-key-point (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-key-point (cdr previous) (length a))))


(defun vector-key-point (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-KEY-POINT")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-key-point))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-key-point x))
	     ((eq :length x)
	      (cv:vec-key-point-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-key-point-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-key-point-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-key-point) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-key-point-to-c-arr x) 'cv:key-point y)
		  (if (< z 5)  (mem-aref (cv:c-pointer 
					   (mem-aref (cv:vec-key-point-to-c-arr x) 
						     'cv:key-point y)) :float z)
		      (mem-aref (cv:c-pointer 
				 (mem-aref (cv:vec-key-point-to-c-arr x) 'cv:key-point y)) :int z))))
	     (t (error "incorrect input. 
  ~%See VECTOR-KEY-POINT documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorm" make-vector-mat) cv:vector-mat)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorm" c-arr-to-vec-mat) cv:vector-mat
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-mat (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-mat (cdr previous) (length a))))


(defun vector-mat (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-MAT")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-mat))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-mat x))
	     ((eq :length x)
	      (cv:vec-mat-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-mat-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-mat-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-mat) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-mat-to-c-arr x) 'cv:mat y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-mat-to-c-arr x) 'cv:mat y)) :int z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-MAT documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorp" make-vector-point) cv:vector-point)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorp" c-arr-to-vec-point) cv:vector-point
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-point (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-point (cdr previous) (length a))))


(defun vector-point (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-POINT")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-point))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-point x))
	     ((eq :length x)
	      (cv:vec-point-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-point-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-point-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-point) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-point-to-c-arr x) 'cv:point y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-point-to-c-arr x) 'cv:point y)) :int z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-POINT documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorp2f" make-vector-point-2f) cv:vector-point-2f)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorp2f" c-arr-to-vec-point-2f) cv:vector-point-2f
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-point-2f (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-point-2f (cdr previous) (length a))))


(defun vector-point-2f (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-POINT-2F")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-point-2f))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-point-2f x))
	     ((eq :length x)
	      (cv:vec-point-2f-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-point-2f-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-point-2f-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-point-2f) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-point-2f-to-c-arr x) 'cv:point-2f y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-point-2f-to-c-arr x) 'cv:point-2f y)) :float z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-POINT-2F documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorr" make-vector-rect) cv:vector-rect)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorr" c-arr-to-vec-rect) cv:vector-rect
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-rect (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-rect (cdr previous) (length a))))


(defun vector-rect (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-RECT")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-rect))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-rect x))
	     ((eq :length x)
	      (cv:vec-rect-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-rect-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-rect-to-lisp-vec y z))
	     ((and (typep x 'cv:std-vector-rect) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-rect-to-c-arr x) 'cv:rect y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-rect-to-c-arr x) 'cv:rect y)) :int z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-RECT documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectoru" make-vector-uchar) cv:vector-uchar)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectoru" c-arr-to-vec-uchar) cv:vector-uchar
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-uchar (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :uchar :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-uchar (cdr previous) (length a))))


(defun vector-uchar (&rest args)
  (if (third args)
      (error "odd number of args to VECTOR-UCHAR")
      nil)
    (let ((x (first args))
	   (y (second args)))
       (cond ((null x)
	      (make-vector-uchar))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-uchar x))
	     ((eq :length x)
	      (cv:vec-uchar-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-uchar-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-uchar-to-lisp-vec y))
	     ((typep x 'cv:std-vector-uchar)
	      (if (eq y nil)
		  (mem-aref (cv:vec-uchar-to-c-arr x) :uchar) 
		  (mem-aref (cv:vec-uchar-to-c-arr x) :uchar y)))
	     (t (error "incorrect input. 
                   ~%See VECTOR-UCHAR documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv2d" make-vector-vec-2d) cv:vector-vec-2d)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv2d" c-arr-to-vec-vec-2d) cv:vector-vec-2d
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-2d (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-2d (cdr previous) (length a))))


(defun vector-vec-2d (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-VEC-2D")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-vec-2d))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-vec-2d x))
	     ((eq :length x)
	      (cv:vec-vec-2d-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-vec-2d-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-vec-2d-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-vec-2d) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-vec-2d-to-c-arr x) 'cv:vec-2d y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-vec-2d-to-c-arr x) 'cv:vec-2d y)) :double z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-2D documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv3d" make-vector-vec-3d) cv:vector-vec-3d)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv3d" c-arr-to-vec-vec-3d) cv:vector-vec-3d
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-3d (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-3d (cdr previous) (length a))))


(defun vector-vec-3d (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-VEC-3D")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-vec-3d))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-vec-3d x))
	     ((eq :length x)
	      (cv:vec-vec-3d-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-vec-3d-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-vec-3d-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-vec-3d) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-vec-3d-to-c-arr x) 'cv:vec-3d y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-vec-3d-to-c-arr x) 'cv:vec-3d y)) :double z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-3D documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv4d" make-vector-vec-4d) cv:vector-vec-4d)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv4d" c-arr-to-vec-vec-4d) cv:vector-vec-4d
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-4d (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-4d (cdr previous) (length a))))


(defun vector-vec-4d (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-VEC-4D")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-vec-4d))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-vec-4d x))
	     ((eq :length x)
	      (cv:vec-vec-4d-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-vec-4d-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-vec-4d-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-vec-4d) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-vec-4d-to-c-arr x) 'cv:vec-4d y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-vec-4d-to-c-arr x) 'cv:vec-4d y)) :double z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-4D documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv6d" make-vector-vec-6d) cv:vector-vec-6d)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv6d" c-arr-to-vec-vec-6d) cv:vector-vec-6d
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-6d (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-6d (cdr previous) (length a))))


(defun vector-vec-6d (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-VEC-6D")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-vec-6d))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-vec-6d x))
	     ((eq :length x)
	      (cv:vec-vec-6d-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-vec-6d-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-vec-6d-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-vec-6d) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-vec-6d-to-c-arr x) 'cv:vec-6d y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-vec-6d-to-c-arr x) 'cv:vec-6d y)) :double z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-6D documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv2f" make-vector-vec-2f) cv:vector-vec-2f)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv2f" c-arr-to-vec-vec-2f) cv:vector-vec-2f
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-2f (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-2f (cdr previous) (length a))))


(defmacro vec-vec-2f (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-VEC-2F")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-vec-2f))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-vec-2f x))
	     ((eq :length x)
	      (cv:vec-vec-2f-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-vec-2f-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-vec-2f-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-vec-2f) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-vec-2f-to-c-arr x) 'cv:vec-2f y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-vec-2f-to-c-arr x) 'cv:vec-2f y)) :float z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-2F documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv3f" make-vector-vec-3f) cv:vector-vec-3f)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv3f" c-arr-to-vec-vec-3f) cv:vector-vec-3f
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-3f (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-3f (cdr previous) (length a))))


(defun vector-vec-3f (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-VEC-3F")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-vec-3f))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-vec-3f x))
	     ((eq :length x)
	      (cv:vec-vec-3f-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-vec-3f-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-vec-3f-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-vec-3f) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-vec-3f-to-c-arr x) 'cv:vec-3f y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-vec-3f-to-c-arr x) 'cv:vec-3f y)) :float z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-3F documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv4f" make-vector-vec-4f) cv:vector-vec-4f)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv4f" c-arr-to-vec-vec-4f) cv:vector-vec-4f
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-4f (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-4f (cdr previous) (length a))))


(defun vector-vec-4f (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-VEC-4F")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-vec-4f))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-vec-4f x))
	     ((eq :length x)
	      (cv:vec-vec-4f-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-vec-4f-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-vec-4f-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-vec-4f) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-vec-4f-to-c-arr x) 'cv:vec-4f y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-vec-4f-to-c-arr x) 'cv:vec-4f y)) :float z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-4F documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv6f" make-vector-vec-6f) cv:vector-vec-6f)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv6f" c-arr-to-vec-vec-6f) cv:vector-vec-6f
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-6f (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-6f (cdr previous) (length a))))


(defun vector-vec-6f (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-VEC-6F")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-vec-6f))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-vec-6f x))
	     ((eq :length x)
	      (cv:vec-vec-6f-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-vec-6f-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-vec-6f-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-vec-6f) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-vec-6f-to-c-arr x) 'cv:vec-6f y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-vec-6f-to-c-arr x) 'cv:vec-6f y)) :float z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-6F documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv2i" make-vector-vec-2i) cv:vector-vec-2i)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv2i" c-arr-to-vec-vec-2i) cv:vector-vec-2i
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-2i (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-2i (cdr previous) (length a))))


(defun vector-vec-2i (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-VEC-2I")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-vec-2i))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-vec-2i x))
	     ((eq :length x)
	      (cv:vec-vec-2i-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-vec-2i-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-vec-2i-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-vec-2i) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-vec-2i-to-c-arr x) 'cv:vec-2i y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-vec-2i-to-c-arr x) 'cv:vec-2i y)) :int z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-2I documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv3i" make-vector-vec-3i) cv:vector-vec-3i)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv3i" c-arr-to-vec-vec-3i) cv:vector-vec-3i
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-3i (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-3i (cdr previous) (length a))))


(defun vector-vec-3i (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-VEC-3I")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-vec-3i))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-vec-3i x))
	     ((eq :length x)
	      (cv:vec-vec-3i-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-vec-3i-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-vec-3i-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-vec-3i) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-vec-3i-to-c-arr x) 'cv:vec-3i y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-vec-3i-to-c-arr x) 'cv:vec-3i y)) :int z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-3I documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv4i" make-vector-vec-4i) cv:vector-vec-4i)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv4i" c-arr-to-vec-vec-4i) cv:vector-vec-4i
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-4i (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-4i (cdr previous) (length a))))


(defun vector-vec-4i (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-VEC-4I")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-vec-4i))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-vec-4i x))
	     ((eq :length x)
	      (cv:vec-vec-4i-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-vec-4i-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-vec-4i-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-vec-4i) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-vec-4i-to-c-arr x) 'cv:vec-4i y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-vec-4i-to-c-arr x) 'cv:vec-4i y)) :int z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-4I documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv6i" make-vector-vec-6i) cv:vector-vec-6i)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv6i" c-arr-to-vec-vec-6i) cv:vector-vec-6i
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-6i (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-6i (cdr previous) (length a))))


(defun vector-vec-6i (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-VEC-6I")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-vec-6i))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-vec-6i x))
	     ((eq :length x)
	      (cv:vec-vec-6i-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-vec-6i-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-vec-6i-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-vec-6i) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-vec-6i-to-c-arr x) 'cv:vec-6i y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-vec-6i-to-c-arr x) 'cv:vec-6i y)) :int z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-6I documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv8i" make-vector-vec-8i) cv:vector-vec-8i)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv8i" c-arr-to-vec-vec-8i) cv:vector-vec-8i
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-8i (a)
    (unless (equal a (car previous))
      (setf previous (cons a (cv:gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(cv:c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-8i (cdr previous) (length a))))


(defun vector-vec-8i (&rest args)
  (if (fourth args)
      (error "odd number of args to VECTOR-VEC-8I")
      nil)
    (let ((x (first args))
	   (y (second args))
           (z (third args)))
       (cond ((null x)
	      (make-vector-vec-8i))
	     ((and (or (vectorp x) (listp x)) (null y))
	      (arr-to-vec-vec-8i x))
	     ((eq :length x)
	      (cv:vec-vec-8i-length y))
	     ((and (eq :to-lisp-list x))
	      (cv:vec-vec-8i-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (cv:vec-vec-8i-to-lisp-vec y))
	     ((and (typep x 'cv:std-vector-vec-8i) y)
	      (if (eq z nil)
		  (mem-aref (cv:vec-vec-8i-to-c-arr x) 'cv:vec-8i y)
		  (mem-aref (cv:c-pointer (mem-aref (cv:vec-vec-8i-to-c-arr x) 'cv:vec-8i y)) :int z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-8I documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))


