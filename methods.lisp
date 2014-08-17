;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; methods.lisp
;;;; Methods for the OpenCV bindings


(in-package :lisp-cv)


;;; DEFGENERIC

(defgeneric @@@ (self arg1 &optional arg2 arg3 arg4)
    (:documentation "Used to overload the @ name. Used for accessor type functions."))

(defgeneric (setf @@@) (val self arg1 &optional arg2 arg3 arg4)
    (:documentation "Used to overload the @ name. Used to make accessor type functions setf-able."))

(defgeneric abs (self)
  (:documentation "Used to call the binding for the OpenCV function 'abs' and CL:ABS."))

(defgeneric angle (self)
  (:documentation "Used for all class bindings with an 'angle' member."))

(defgeneric (setf angle) (val self)
  (:documentation "Used to setf the ANGLE value of class bindings with an 'angle' member."))

(defgeneric assign (self other)
  (:documentation "Used for bindings of the OpenCV = operator."))

(defgeneric apply* (self arg1 arg2 &optional arg3)
  (:documentation "Used for all class bindings with an 'apply' member function."))

(defgeneric bounding-rect (self)
  (:documentation "Used for the BOUNDING-RECT member of ROTATED-RECT."))

(defgeneric center (self)
  (:documentation "Used for all class bindings with an 'center' member."))

(defgeneric (setf center) (val self)
  (:documentation "Used to setf the CENTER value of class bindings with an 'center' member."))

(defgeneric clone (self)
  (:documentation "Used for all class bindings with a 'clone' member function."))

(defgeneric compute (type self &rest args)
  (:documentation "Used for all class bindings with a 'compute' member function."))

(defgeneric create (type self &rest args)
  (:documentation "Used for all class bindings with a 'create' member function."))

(defgeneric data (self)
  (:documentation "Used for all class bindings with a 'data' member function."))

(defgeneric detect (self &rest args)
  (:documentation "Used for all class bindings with a 'detect' member function."))

(defgeneric descriptor-extractor-compute (self image keypoints descriptors)
  (:documentation "Used for all DESCRIPTOR-EXTRACTOR-COMPUTE methods."))

(defgeneric descriptor-extractor-create (self descriptor-extractor-type)
  (:documentation "Used for all DESCRIPTOR-EXTRACTOR-CREATE methods."))

(defgeneric descriptor-matcher-create (self descriptor-matcher-type)
  (:documentation "Used for all DESCRIPTOR-MATCHER-CREATE methods."))

(defgeneric descriptor-matcher-match (self query-descriptors train-descriptors matches &optional mask)
  (:documentation "Used for all DESCRIPTOR-MATCHER-MATCH methods.")) 

(defgeneric descriptor-matcher-knn-match (self query-descriptors train-descriptors matches k &optional mask compact-result)
  (:documentation "Used for all DESCRIPTOR-MATCHER-KNN-MATCH methods.")) 

(defgeneric dot (self other)
  (:documentation "Used for all class bindings with a 'dot' member function."))

(defgeneric feature-2d-compute (self image keypoints descriptors)
  (:documentation "Used for all FEATURE-2D-COMPUTE methods."))

(defgeneric feature-2d-create (self name)
  (:documentation "Used for all FEATURE-2D-CREATE methods."))

(defgeneric feature-detector-create (self detector-type)
  (:documentation "Used for all FEATURE-DETECTOR-CREATE methods."))

(defgeneric feature-detector-detect (self image keypoints &optional mask)
  (:documentation "Used for all FEATURE-DETECTOR-DETECT methods."))

(defgeneric get* (self value)
  (:documentation "Used for all class bindings with a 'get' member function."))

(defgeneric height (self)
  (:documentation "Used for all class bindings with an 'height' member."))

(defgeneric (setf height) (val self)
  (:documentation "Used to setf the HEIGHT value of class bindings with an 'height' member."))

(defgeneric is-opened (self)
  (:documentation "Used for all class bindings with an 'isOpened' member function."))

(defgeneric file-storage-write (fs name value)
  (:documentation "Used for all FILE-STORAGE-WRITE methods."))

(defgeneric knn-match (self &rest args)
  (:documentation "Used for all class bindings with a knnMatch member function."))

(defgeneric match (self &rest args)
  (:documentation "Used for all class bindings with a 'match' member function."))

(defgeneric mean (self &rest args)
  (:documentation "Used to overload the MEAN name for functions, members, and member functions."))

(defgeneric (setf mean) (val self)
  (:documentation "Used to setf the MEAN value of class bindings with a MEAN member."))

(defgeneric open (self &rest args)
  (:documentation "Used for all class bindings with a 'open' member function."))

(defgeneric predict (self &rest args)
  (:documentation "Used for all class bindings with a 'predict' member function."))

(defgeneric pt (self)
  (:documentation "Used for all class bindings with a 'pt' member."))

(defgeneric (setf pt) (val self)
  (:documentation "Used to setf the PT value of class bindings with a PT member."))

(defgeneric push-back (self val)
  (:documentation "Used for all class bindings with a 'push_back' member function."))

(defgeneric *read (self arg)
  (:documentation "Used for all class bindings with an 'read' member function."))

(defgeneric release (self)
  (:documentation "Used for all class bindings with a 'release' member function."))

(defgeneric save (self &rest args)
  (:documentation "Used for all class bindings with a 'save' member function."))

(defgeneric size (arg &rest args)
  (:documentation "Used for all class bindings with a 'size' member."))

(defgeneric (setf size) (val self)
  (:documentation "Used to setf the SIZE value of class bindings with an 'size' member."))

(defgeneric train (self &rest args)
  (:documentation "Used for all class bindings with a 'train' member function."))

(defgeneric type* (self)
  (:documentation "Used for all class bindings with a 'type' member."))

(defgeneric (setf type*) (val self)
  (:documentation "Used to setf the TYPE of class bindings with an 'type' member."))

(defgeneric width (self)
  (:documentation "Used for all class bindings with an 'width' member."))

(defgeneric (setf width) (val self)
  (:documentation "Used to setf the WIDTH value of class bindings with an 'width' member."))

(defgeneric x (self)
  (:documentation "Used for all class bindings with an'x' member."))

(defgeneric (setf x) (val self)
  (:documentation "Used to setf the x coordinate of class bindings with an 'x' member."))

(defgeneric (setf y) (val self)
  (:documentation "Used to setf the y coordinate of class bindings with an 'y' member."))

(defgeneric y (self)
  (:documentation "Used for all class bindings with an 'y' member."))

(defgeneric z (self)
  (:documentation "Used for all class bindings with an 'z' member."))

(defgeneric (setf z) (val self)
  (:documentation "Used to setf the z coordinate of class bindings with an 'z' member."))


;;;DEFMETHODS


(defmethod @@@ ((self (eql :char)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (cond (arg4 
	 (mem-aref (foreign-funcall "cv_Mat_at_char_3" 
				    :pointer (c-pointer arg1) :int arg2 :int arg3 :int arg4 
				    :pointer) :char))

	((and arg3 (not arg4))
	 (mem-aref (foreign-funcall "cv_Mat_at_char_2" 
				    :pointer (c-pointer arg1) :int arg2 :int arg3 
				    :pointer) :char))
	(t
	 (mem-aref (foreign-funcall "cv_Mat_at_char_1" 
				    :pointer (c-pointer arg1) :int (or arg2 0)
				    :pointer) :char))))


(defmethod (setf @@@) (val (self (eql :char)) (arg1 cv-mat) &optional arg2 arg3 arg4)
 (cond  
   (arg4 
     (let ((temp (foreign-funcall "cv_Mat_at_char_3" 
				  :pointer (c-pointer arg1) :int arg2 :int arg3 :int arg4 
				  :pointer)))
       (setf (mem-aref temp :char) val)))

    ((and arg3 (not arg4))
     (let ((temp (foreign-funcall "cv_Mat_at_char_2" 
				  :pointer (c-pointer arg1) :int arg2 :int arg3 
				  :pointer)))
       (setf (mem-aref temp :char) val)))

    (t
     (let ((temp (foreign-funcall "cv_Mat_at_char_1" 
				  :pointer (c-pointer arg1) :int (or arg2 0)
				  :pointer)))
       (setf (mem-aref temp :char) val)))))


(defmethod @@@ ((self (eql :double)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (cond (arg4 
	 (mem-aref (foreign-funcall "cv_Mat_at_double_3" 
				    :pointer (c-pointer arg1) :int arg2 :int arg3 :int arg4 
				    :pointer) :double))

	((and arg3 (not arg4))
	 (mem-aref (foreign-funcall "cv_Mat_at_double_2" 
				    :pointer (c-pointer arg1) :int arg2 :int arg3 
				    :pointer) :double))
	(t
	 (mem-aref (foreign-funcall "cv_Mat_at_double_1" 
				    :pointer (c-pointer arg1) :int (or arg2 0)
				    :pointer) :double))))


(defmethod (setf @@@) (val (self (eql :double)) (arg1 cv-mat) &optional arg2 arg3 arg4)
 (cond  
   (arg4 
     (let ((temp (foreign-funcall "cv_Mat_at_double_3" 
				  :pointer (c-pointer arg1) :int arg2 :int arg3 :int arg4 
				  :pointer)))
       (setf (mem-aref temp :double) val)))

    ((and arg3 (not arg4))
     (let ((temp (foreign-funcall "cv_Mat_at_double_2" 
				  :pointer (c-pointer arg1) :int arg2 :int arg3 
				  :pointer)))
       (setf (mem-aref temp :double) val)))

    (t
     (let ((temp (foreign-funcall "cv_Mat_at_double_1" 
				  :pointer (c-pointer arg1) :int (or arg2 0)
				  :pointer)))
       (setf (mem-aref temp :double) val)))))


(defmethod @@@ ((self (eql :float)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (cond (arg4 
	 (mem-aref (foreign-funcall "cv_Mat_at_float_3" 
				    :pointer (c-pointer arg1) :int arg2 :int arg3 :int arg4 
				    :pointer) :float))

	((and arg3 (not arg4))
	 (mem-aref (foreign-funcall "cv_Mat_at_float_2" 
				    :pointer (c-pointer arg1) :int arg2 :int arg3 
				    :pointer) :float))
	(t
	 (mem-aref (foreign-funcall "cv_Mat_at_float_1" 
				    :pointer (c-pointer arg1) :int (or arg2 0)
				    :pointer) :float))))


(defmethod (setf @@@) (val (self (eql :float)) (arg1 cv-mat) &optional arg2 arg3 arg4)
 (cond  
   (arg4 
     (let ((temp (foreign-funcall "cv_Mat_at_float_3" 
				  :pointer (c-pointer arg1) :int arg2 :int arg3 :int arg4 
				  :pointer)))
       (setf (mem-aref temp :float) val)))

    ((and arg3 (not arg4))
     (let ((temp (foreign-funcall "cv_Mat_at_float_2" 
				  :pointer (c-pointer arg1) :int arg2 :int arg3 
				  :pointer)))
       (setf (mem-aref temp :float) val)))

    (t
     (let ((temp (foreign-funcall "cv_Mat_at_float_1" 
				  :pointer (c-pointer arg1) :int (or arg2 0)
				  :pointer)))
       (setf (mem-aref temp :float) val)))))


(defmethod @@@ ((self (eql :int)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (cond (arg4 
	 (mem-aref (foreign-funcall "cv_Mat_at_int_3" 
				    :pointer (c-pointer arg1) :int arg2 :int arg3 :int arg4 
				    :pointer) :int))

	((and arg3 (not arg4))
	 (mem-aref (foreign-funcall "cv_Mat_at_int_2" 
				    :pointer (c-pointer arg1) :int arg2 :int arg3 
				    :pointer) :int))
	(t
	 (mem-aref (foreign-funcall "cv_Mat_at_int_1" 
				    :pointer (c-pointer arg1) :int (or arg2 0)
				    :pointer) :int))))


(defmethod (setf @@@) (val (self (eql :int)) (arg1 cv-mat) &optional arg2 arg3 arg4)
 (cond  
   (arg4 
     (let ((temp (foreign-funcall "cv_Mat_at_int_3" 
				  :pointer (c-pointer arg1) :int arg2 :int arg3 :int arg4 
				  :pointer)))
       (setf (mem-aref temp :int) val)))

    ((and arg3 (not arg4))
     (let ((temp (foreign-funcall "cv_Mat_at_int_2" 
				  :pointer (c-pointer arg1) :int arg2 :int arg3 
				  :pointer)))
       (setf (mem-aref temp :int) val)))

    (t
     (let ((temp (foreign-funcall "cv_Mat_at_int_1" 
				  :pointer (c-pointer arg1) :int (or arg2 0)
				  :pointer)))
       (setf (mem-aref temp :int) val)))))


(defmethod @@@ ((self (eql :short)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (cond (arg4 
	 (mem-aref (foreign-funcall "cv_Mat_at_short_3" 
				    :pointer (c-pointer arg1) :int arg2 :int arg3 :int arg4 
				    :pointer) :short))

	((and arg3 (not arg4))
	 (mem-aref (foreign-funcall "cv_Mat_at_short_2" 
				    :pointer (c-pointer arg1) :int arg2 :int arg3 
				    :pointer) :short))
	(t
	 (mem-aref (foreign-funcall "cv_Mat_at_short_1" 
				    :pointer (c-pointer arg1) :int (or arg2 0)
				    :pointer) :short))))


(defmethod (setf @@@) (val (self (eql :short)) (arg1 cv-mat) &optional arg2 arg3 arg4)
 (cond  
   (arg4 
     (let ((temp (foreign-funcall "cv_Mat_at_short_3" 
				  :pointer (c-pointer arg1) :int arg2 :int arg3 :int arg4 
				  :pointer)))
       (setf (mem-aref temp :short) val)))

    ((and arg3 (not arg4))
     (let ((temp (foreign-funcall "cv_Mat_at_short_2" 
				  :pointer (c-pointer arg1) :int arg2 :int arg3 
				  :pointer)))
       (setf (mem-aref temp :short) val)))

    (t
     (let ((temp (foreign-funcall "cv_Mat_at_short_1" 
				  :pointer (c-pointer arg1) :int (or arg2 0)
				  :pointer)))
       (setf (mem-aref temp :short) val)))))


(defmethod @@@ ((self (eql :uchar)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (cond (arg4 
	 (mem-aref (foreign-funcall "cv_Mat_at_uchar_3" 
				    :pointer (c-pointer arg1) :int arg2 :int arg3 :int arg4 
				    :pointer) :uchar))

	((and arg3 (not arg4))
	 (mem-aref (foreign-funcall "cv_Mat_at_uchar_2" 
				    :pointer (c-pointer arg1) :int arg2 :int arg3 
				    :pointer) :uchar))
	(t
	 (mem-aref (foreign-funcall "cv_Mat_at_uchar_1" 
				    :pointer (c-pointer arg1) :int (or arg2 0)
				    :pointer) :uchar))))


(defmethod (setf @@@) (val (self (eql :uchar)) (arg1 cv-mat) &optional arg2 arg3 arg4)
 (cond  
   (arg4 
     (let ((temp (foreign-funcall "cv_Mat_at_uchar_3" 
				  :pointer (c-pointer arg1) :int arg2 :int arg3 :int arg4 
				  :pointer)))
       (setf (mem-aref temp :uchar) val)))

    ((and arg3 (not arg4))
     (let ((temp (foreign-funcall "cv_Mat_at_uchar_2" 
				  :pointer (c-pointer arg1) :int arg2 :int arg3 
				  :pointer)))
       (setf (mem-aref temp :uchar) val)))

    (t
     (let ((temp (foreign-funcall "cv_Mat_at_uchar_1" 
				  :pointer (c-pointer arg1) :int (or arg2 0)
				  :pointer)))
       (setf (mem-aref temp :uchar) val)))))


(defmethod @@@ ((self (eql :ushort)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (cond (arg4 
	 (mem-aref (foreign-funcall "cv_Mat_at_ushort_3" 
				    :pointer (c-pointer arg1) :int arg2 :int arg3 :int arg4 
				    :pointer) :ushort))

	((and arg3 (not arg4))
	 (mem-aref (foreign-funcall "cv_Mat_at_ushort_2" 
				    :pointer (c-pointer arg1) :int arg2 :int arg3 
				    :pointer) :ushort))
	(t
	 (mem-aref (foreign-funcall "cv_Mat_at_ushort_1" 
				    :pointer (c-pointer arg1) :int (or arg2 0)
				    :pointer) :ushort))))


(defmethod (setf @@@) (val (self (eql :ushort)) (arg1 cv-mat) &optional arg2 arg3 arg4)
 (cond  
   (arg4 
     (let ((temp (foreign-funcall "cv_Mat_at_ushort_3" 
				  :pointer (c-pointer arg1) :int arg2 :int arg3 :int arg4 
				  :pointer)))
       (setf (mem-aref temp :ushort) val)))

    ((and arg3 (not arg4))
     (let ((temp (foreign-funcall "cv_Mat_at_ushort_2" 
				  :pointer (c-pointer arg1) :int arg2 :int arg3 
				  :pointer)))
       (setf (mem-aref temp :ushort) val)))

    (t
     (let ((temp (foreign-funcall "cv_Mat_at_ushort_1" 
				  :pointer (c-pointer arg1) :int (or arg2 0)
				  :pointer)))
       (setf (mem-aref temp :ushort) val)))))


(defmethod @@@ ((self (eql :point)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-point :c-pointer 
			  (foreign-funcall "cv_Mat_at_Point_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-point :c-pointer 
			  (foreign-funcall "cv_Mat_at_Point_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :point)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3
	 (make-instance 'cv-point :c-pointer 
			(foreign-funcall "cv_Mat_at_Point_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-point :c-pointer 
			(foreign-funcall "cv_Mat_at_Point_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :point-2d)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-point-2d :c-pointer 
			  (foreign-funcall "cv_Mat_at_Point2d_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-point-2d :c-pointer 
			  (foreign-funcall "cv_Mat_at_Point2d_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :point-2d)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3
	 (make-instance 'cv-point-2d :c-pointer 
			(foreign-funcall "cv_Mat_at_Point2d_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-point-2d :c-pointer 
			(foreign-funcall "cv_Mat_at_Point2d_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :point-2f)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-point-2f :c-pointer 
			  (foreign-funcall "cv_Mat_at_Point2f_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-point-2f :c-pointer 
			  (foreign-funcall "cv_Mat_at_Point2f_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :point-2f)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3
	 (make-instance 'cv-point-2f :c-pointer 
			(foreign-funcall "cv_Mat_at_Point2f_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-point-2f :c-pointer 
			(foreign-funcall "cv_Mat_at_Point2f_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :point-3d)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-point-3d :c-pointer 
			  (foreign-funcall "cv_Mat_at_Point3d_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-point-3d :c-pointer 
			  (foreign-funcall "cv_Mat_at_Point3d_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :point-3d)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3
	 (make-instance 'cv-point-3d :c-pointer 
			(foreign-funcall "cv_Mat_at_Point3d_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-point-3d :c-pointer 
			(foreign-funcall "cv_Mat_at_Point3d_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :point-3f)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-point-3f :c-pointer 
			  (foreign-funcall "cv_Mat_at_Point3f_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-point-3f :c-pointer 
			  (foreign-funcall "cv_Mat_at_Point3f_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :point-3f)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3
	 (make-instance 'cv-point-3f :c-pointer 
			(foreign-funcall "cv_Mat_at_Point3f_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-point-3f :c-pointer 
			(foreign-funcall "cv_Mat_at_Point3f_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :point-3i)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-point-3i :c-pointer 
			  (foreign-funcall "cv_Mat_at_Point3i_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-point-3i :c-pointer 
			  (foreign-funcall "cv_Mat_at_Point3i_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :point-3i)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3
	 (make-instance 'cv-point-3i :c-pointer 
			(foreign-funcall "cv_Mat_at_Point3i_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-point-3i :c-pointer 
			(foreign-funcall "cv_Mat_at_Point3i_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-2b)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-2b :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec2b_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-2b :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec2b_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-2b)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-2b :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec2b_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-2b :c-pointer
			(foreign-funcall "cv_Mat_at_Vec2b_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-3b)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-3b :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec3b_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-3b :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec3b_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-3b)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-3b :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec3b_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-3b :c-pointer
			(foreign-funcall "cv_Mat_at_Vec3b_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-4b)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-4b :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec4b_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-4b :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec4b_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-4b)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-4b :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec4b_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-4b :c-pointer
			(foreign-funcall "cv_Mat_at_Vec4b_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-2d)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-2d :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec2d_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-2d :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec2d_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-2d)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-2d :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec2d_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-2d :c-pointer
			(foreign-funcall "cv_Mat_at_Vec2d_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-3d)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-3d :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec3d_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-3d :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec3d_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-3d)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-3d :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec3d_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-3d :c-pointer
			(foreign-funcall "cv_Mat_at_Vec3d_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))

(defmethod @@@ ((self (eql :vec-4d)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-4d :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec4d_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-4d :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec4d_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-4d)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-4d :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec4d_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-4d :c-pointer
			(foreign-funcall "cv_Mat_at_Vec4d_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-2f)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-2f :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec2f_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-2f :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec2f_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-2f)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-2f :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec2f_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-2f :c-pointer
			(foreign-funcall "cv_Mat_at_Vec2f_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-3f)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-3f :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec3f_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-3f :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec3f_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-3f)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-3f :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec3f_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-3f :c-pointer
			(foreign-funcall "cv_Mat_at_Vec3f_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-4f)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-4f :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec4f_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-4f :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec4f_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-4f)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-4f :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec4f_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-4f :c-pointer
			(foreign-funcall "cv_Mat_at_Vec4f_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-2i)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-2i :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec2i_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-2i :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec2i_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-2i)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-2i :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec2i_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-2i :c-pointer
			(foreign-funcall "cv_Mat_at_Vec2i_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-3i)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-3i :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec3i_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-3i :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec3i_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-3i)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-3i :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec3i_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-3i :c-pointer
			(foreign-funcall "cv_Mat_at_Vec3i_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-4i)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-4i :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec4i_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-4i :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec4i_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-4i)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-4i :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec4i_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-4i :c-pointer
			(foreign-funcall "cv_Mat_at_Vec4i_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-2s)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-2s :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec2s_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-2s :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec2s_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-2s)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-2s :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec2s_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-2s :c-pointer
			(foreign-funcall "cv_Mat_at_Vec2s_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-3s)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-3s :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec3s_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-3s :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec3s_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-3s)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-3s :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec3s_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-3s :c-pointer
			(foreign-funcall "cv_Mat_at_Vec3s_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-4s)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-4s :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec4s_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-4s :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec4s_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-4s)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-4s :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec4s_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-4s :c-pointer
			(foreign-funcall "cv_Mat_at_Vec4s_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-2w)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-2w :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec2w_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-2w :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec2w_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-2w)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-2w :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec2w_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-2w :c-pointer
			(foreign-funcall "cv_Mat_at_Vec2w_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-3w)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-3w :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec3w_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-3w :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec3w_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-3w)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-3w :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec3w_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-3w :c-pointer
			(foreign-funcall "cv_Mat_at_Vec3w_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self (eql :vec-4w)) (arg1 cv-mat) &optional arg2 arg3 arg4)
    (if arg4 (error "invalid number of arguments: 5"))
    (cond (arg3 
	   (make-instance 'cv-vec-4w :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec4w_2" :pointer (c-pointer arg1) 
					   :int arg2 :int arg3 :pointer)))
	  (t 
	   (make-instance 'cv-vec-4w :c-pointer 
			  (foreign-funcall "cv_Mat_at_Vec4w_1" :pointer (c-pointer arg1) 
					   :int (or arg2 0) :pointer)))))


(defmethod (setf @@@) (val (self (eql :vec-4w)) (arg1 cv-mat) &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments: 5"))
  (cond (arg3 
	 (make-instance 'cv-vec-4w :c-pointer 
			(foreign-funcall "cv_Mat_at_Vec4w_set_Val_2" :pointer (c-pointer arg1) 
					 :int arg2 :int arg3 :pointer (c-pointer val) :pointer)))
	(t 
	 (make-instance 'cv-vec-4w :c-pointer
			(foreign-funcall "cv_Mat_at_Vec4w_set_Val_1" :pointer (c-pointer arg1)
					 :int (or arg2 0) :pointer (c-pointer val) :pointer)))))


(defmethod @@@ ((self std-vector-char) arg1 &optional arg2 arg3 arg4)
    "Returns the element at position n in the vector."
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  (let ((temp (foreign-funcall "std_vectorc_to_carray" 
			       :pointer (c-pointer self) 
			       :pointer))) 
    (mem-aref temp :char (or arg1 0))))


(defmethod (setf @@@) (val (self std-vector-char) arg1 &optional arg2 arg3 arg4)
  "Sets the element at position n in the vector."
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  (let ((self (foreign-funcall "cv_vector_c_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0)
			       :pointer)))
    (setf (mem-aref self :char) val)))


(defmethod @@@ ((self std-vector-dmatch) arg1 &optional arg2 arg3 arg4)
    "Returns the object at position n in the vector.
     This method can return any element in the object."
 (if arg3 (error "invalid number of arguments."))
  arg3 arg4
  (let ((temp (foreign-funcall "cv_vector_dm_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (if arg2
	(case arg2 
	  (0 (%dmatch-query-idx temp))
	  (1 (%dmatch-train-idx temp))
	  (2 (%dmatch-img-idx temp))
	  (3 (%dmatch-distance temp)))
	(make-instance 'cv-dmatch :c-pointer temp))))


(defmethod (setf @@@) (val (self std-vector-dmatch) arg1 &optional arg2 arg3 arg4)
  "Sets the element at position n in the vector."
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  (let ((temp (foreign-funcall "cv_vector_dm_at_set_Val" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0)
			       :pointer (c-pointer val)
			       :pointer)))
    (make-instance 'cv-dmatch :c-pointer temp)))


(defmethod @@@ ((self std-vector-double) arg1 &optional arg2 arg3 arg4)
    "Returns the element at position n in the vector."
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  (let ((temp (foreign-funcall "std_vectord_to_carray" 
			       :pointer (c-pointer self) 
			       :pointer)))
    (mem-aref temp :double (or arg1 0))))


(defmethod (setf @@@) (val (self std-vector-double) arg1 &optional arg2 arg3 arg4)
  "Sets the element at position n in the vector."
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  (let ((vec (foreign-funcall "cv_vector_d_at" 
			      :pointer (c-pointer self) 
			      :int (or arg1 0)
			      :pointer)))
    (setf (mem-aref vec :double) val)))


(defmethod @@@ ((self std-vector-float) arg1 &optional arg2 arg3 arg4)
    "Returns the element at position n in the vector."
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  (let ((temp (foreign-funcall "std_vectorf_to_carray" 
			       :pointer (c-pointer self) 
			       :pointer)))
    (mem-aref temp :float (or arg1 0))))


(defmethod (setf @@@) (val (self std-vector-float) arg1 &optional arg2 arg3 arg4)
  "Sets the element at position n in the vector."
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  (let ((vec (foreign-funcall "cv_vector_f_at" 
			      :pointer (c-pointer self) 
			      :int (or arg1 0)
			      :pointer)))
    (setf (mem-aref vec :float) val)))


(defmethod @@@ ((self std-vector-int) arg1 &optional arg2 arg3 arg4)
  "Sets the element at position n in the vector."
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  (let ((temp (foreign-funcall "std_vectori_to_carray" 
			       :pointer (c-pointer self) 
			       :pointer)))
    (mem-aref temp :int (or arg1 0))))


(defmethod (setf @@@) (val (self std-vector-int) arg1 &optional arg2 arg3 arg4)
  "Sets the element at position n in the vector."
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  (let ((vec (foreign-funcall "cv_vector_i_at" 
				   :pointer (c-pointer self) 
				   :int (or arg1 0)
				   :pointer)))
	(setf (mem-aref vec :int) val)))


(defmethod @@@ ((self std-vector-key-point) arg1 &optional arg2 arg3 arg4)
  "Sets the element at position n in the vector."
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  (let ((temp (foreign-funcall "cv_vector_kp_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (if arg2
	(case arg2 
	  (0 (mem-aref (c-pointer 
			(%key-point-pt temp)) :float))
	  (1 (mem-aref (c-pointer 
			(%key-point-pt temp)) :float 1))
	  (2 (%key-point-size temp))
	  (3 (%key-point-angle temp))
	  (4 (%key-point-response temp))
	  (5 (%key-point-octave temp))
	  (6 (%key-point-class-id temp)))
	(make-instance 'cv-key-point :c-pointer temp))))


(defmethod (setf @@@) (val (self std-vector-key-point) arg1 &optional arg2 arg3 arg4)
  "Sets the element at position n in the vector."
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  (let ((temp (foreign-funcall "cv_vector_kp_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int (or arg1 0)
		       :pointer (c-pointer val)
		       :pointer)))
    (make-instance 'cv-key-point :c-pointer temp)))


(defmethod @@@ ((self std-vector-mat) arg1 &optional arg2 arg3 arg4)
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  (let ((temp (foreign-funcall "cv_vector_m_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (make-instance 'cv-mat :c-pointer temp)))


(defmethod (setf @@@) (val (self std-vector-mat) arg1 &optional arg2 arg3 arg4)
  "Sets the element at position n in the vector."
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  (let ((temp (foreign-funcall "cv_vector_m_at_set_Val" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0)
			       :pointer (c-pointer val)
			       mat)))
    (make-instance 'cv-mat :c-pointer temp)))


(defmethod @@@ ((self std-vector-point) arg1 &optional arg2 arg3 arg4)
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (if arg3 (error "invalid number of arguments."))
  arg3 arg4
  (let ((temp (foreign-funcall "cv_vector_p_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0)
			       :pointer)))
    (if arg2
	(case arg2
	  (0 (mem-aref temp :int))
	  (1 (mem-aref temp :int 1)))
	(make-instance 'cv-point :c-pointer temp))))


(defmethod (setf @@@) (val (self std-vector-point) arg1 &optional arg2 arg3 arg4)
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  "Sets the element at position n in the vector."
  (let ((temp (foreign-funcall "cv_vector_p_at_set_Val" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0)
			       :pointer (c-pointer val)
			       :pointer)))
    (make-instance 'cv-point :c-pointer temp)))


(defmethod @@@ ((self std-vector-point-2f) arg1 &optional arg2 arg3 arg4)
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (if arg3 (error "invalid number of arguments."))
  arg3 arg4
  (let ((temp (foreign-funcall "cv_vector_p2f_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0)
			       :pointer)))
    (if arg2
	(case arg2
	  (0 (mem-aref temp :float))
	  (1 (mem-aref temp :float 1)))
	(make-instance 'cv-point-2f :c-pointer temp))))


(defmethod (setf @@@) (val (self std-vector-point-2f) arg1 &optional arg2 arg3 arg4)
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  "Sets the element at position n in the vector."
  (let ((temp (foreign-funcall "cv_vector_p2f_at_set_Val" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0)
			       :pointer (c-pointer val)
			       :pointer)))
    (make-instance 'cv-point-2f :c-pointer temp)))


(defmethod @@@ ((self std-vector-rect) arg1 &optional arg2 arg3 arg4)
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (if arg3 (error "invalid number of arguments."))
  arg3 arg4
  (let ((temp (foreign-funcall "cv_vector_r_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (if arg2
	(case arg2
	  (0 (mem-aref temp :int))
	  (1 (mem-aref temp :int 1))
	  (2 (mem-aref temp :int 2))
	  (3 (mem-aref temp :int 3)))
	(make-instance 'cv-rect :c-pointer temp))))


(defmethod (setf @@@) (val (self std-vector-rect) arg1 &optional arg2 arg3 arg4)
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  "Sets the element at position n in the vector."
  (let ((temp (foreign-funcall "cv_vector_r_at_set_Val" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0)
			       :pointer (c-pointer val)
			       rect)))
    (make-instance 'cv-rect :c-pointer temp)))


(defmethod @@@ ((self std-vector-uchar) arg1 &optional arg2 arg3 arg4)
    "Returns the element at position n in the vector."
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  (let ((temp (foreign-funcall "std_vectoru_to_carray" 
			       :pointer (c-pointer self) 
			       :pointer)))
    (mem-aref temp :uchar (or arg1 0))))


(defmethod (setf @@@) (val (self std-vector-uchar) arg1 &optional arg2 arg3 arg4)
  "Returns the element at position n in the vector."
  (if arg2 (error "invalid number of arguments."))
  arg2 arg3 arg4
  (let ((vec (foreign-funcall "cv_vector_u_at" 
			      :pointer (c-pointer self) 
			      :int (or arg1 0)
			      :pointer)))
    (setf (mem-aref vec :uchar) val)))


(defmethod @@@ ((self std-vector-vec-2d) arg1 &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments."))
  arg3 arg4
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (let ((temp (foreign-funcall "cv_vector_v2d_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (if arg2 (case arg2
	       (0 (mem-aref temp :double))
	       (1 (mem-aref temp :double 1)))
	(make-instance 'cv-vec-2d :c-pointer temp))))


(defmethod @@@ ((self std-vector-vec-3d) arg1 &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments."))
  arg3 arg4
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (let ((temp (foreign-funcall "cv_vector_v3d_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (if arg2 (case arg2
	       (0 (mem-aref temp :double))
	       (1 (mem-aref temp :double 1)))
	(make-instance 'cv-vec-3d :c-pointer temp))))


(defmethod @@@ ((self std-vector-vec-4d) arg1 &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments."))
  arg3 arg4
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (let ((temp (foreign-funcall "cv_vector_v4d_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (if arg2 (case arg2
	       (0 (mem-aref temp :double))
	       (1 (mem-aref temp :double 1)))
	(make-instance 'cv-vec-4d :c-pointer temp))))


(defmethod @@@ ((self std-vector-vec-6d) arg1 &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments."))
  arg3 arg4
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (let ((temp (foreign-funcall "cv_vector_v6d_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (if arg2 (case arg2
	       (0 (mem-aref temp :double))
	       (1 (mem-aref temp :double 1)))
	(make-instance 'cv-vec-6d :c-pointer temp))))


(defmethod @@@ ((self std-vector-vec-2f) arg1 &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments."))
  arg3 arg4
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (let ((temp (foreign-funcall "cv_vector_v2f_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (if arg2 (case arg2
	       (0 (mem-aref temp :float))
	       (1 (mem-aref temp :float 1)))
	(make-instance 'cv-vec-2f :c-pointer temp))))


(defmethod @@@ ((self std-vector-vec-3f) arg1 &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments."))
  arg3 arg4
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (let ((temp (foreign-funcall "cv_vector_v3f_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (if arg2 (case arg2
	       (0 (mem-aref temp :float))
	       (1 (mem-aref temp :float 1)))
	(make-instance 'cv-vec-3f :c-pointer temp))))


(defmethod @@@ ((self std-vector-vec-4f) arg1 &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments."))
  arg3 arg4
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (let ((temp (foreign-funcall "cv_vector_v4f_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (if arg2 (case arg2
	       (0 (mem-aref temp :float))
	       (1 (mem-aref temp :float 1)))
	(make-instance 'cv-vec-4f :c-pointer temp))))


(defmethod @@@ ((self std-vector-vec-6f) arg1 &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments."))
  arg3 arg4
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (let ((temp (foreign-funcall "cv_vector_v6f_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (if arg2 (case arg2
	       (0 (mem-aref temp :float))
	       (1 (mem-aref temp :float 1)))
	(make-instance 'cv-vec-6f :c-pointer temp))))


(defmethod @@@ ((self std-vector-vec-2i) arg1 &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments."))
  arg3 arg4
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (let ((temp (foreign-funcall "cv_vector_v2i_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (if arg2 (case arg2
	       (0 (mem-aref temp :int))
	       (1 (mem-aref temp :int 1)))
	(make-instance 'cv-vec-2i :c-pointer temp))))


(defmethod @@@ ((self std-vector-vec-3i) arg1 &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments."))
  arg3 arg4
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (let ((temp (foreign-funcall "cv_vector_v3i_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (if arg2 (case arg2
	       (0 (mem-aref temp :int))
	       (1 (mem-aref temp :int 1)))
	(make-instance 'cv-vec-3i :c-pointer temp))))


(defmethod @@@ ((self std-vector-vec-4i) arg1 &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments."))
  arg3 arg4
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (let ((temp (foreign-funcall "cv_vector_v4i_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (if arg2 (case arg2
	       (0 (mem-aref temp :int))
	       (1 (mem-aref temp :int 1)))
	(make-instance 'cv-vec-4i :c-pointer temp))))


(defmethod @@@ ((self std-vector-vec-6i) arg1 &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments."))
  arg3 arg4
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (let ((temp (foreign-funcall "cv_vector_v6i_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (if arg2 (case arg2
	       (0 (mem-aref temp :int))
	       (1 (mem-aref temp :int 1)))
	(make-instance 'cv-vec-6i :c-pointer temp))))


(defmethod @@@ ((self std-vector-vec-8i) arg1 &optional arg2 arg3 arg4)
  (if arg4 (error "invalid number of arguments."))
  arg3 arg4
    "Returns the object at position n in the vector.
     This method can return any element in the object."
  (let ((temp (foreign-funcall "cv_vector_v8i_at" 
			       :pointer (c-pointer self) 
			       :int (or arg1 0) 
			       :pointer)))
    (if arg2 (case arg2
	       (0 (mem-aref temp :int))
	       (1 (mem-aref temp :int 1)))
	(make-instance 'cv-vec-8i :c-pointer temp))))


(defmethod angle ((self cv-rotated-rect))
  "Gets the rotation angle of a ROTATED-RECT object."
  (rotated-rect-angle self))


(defmethod (setf angle) ((val single-float) (self cv-rotated-rect))
  "Sets the rotation angle of a ROTATED-RECT object."
  (rotated-rect-set-angle self val))


(defmethod angle ((self cv-key-point))
  (%key-point-angle self))


(defmethod (setf angle) ((val single-float) (self cv-key-point))
  (key-point-set-angle self val))


(defmethod assign ((self cv-mat) (other cv-mat))
  "Assign matrix data to another matrix."
  (mat-assign self other))


(defmethod assign ((self cv-mat) (other cv-scalar))
  "Assigns a scalar value to a matrix."
  (mat-assign-val self other))


(defmethod assign ((self cv-size) (other cv-size))
  "Assign data from one SIZE object to another,
   OTHER to SELF."
  (size-assign-to self other))


(defmethod bounding-rect ((self cv-rotated-rect))
  "Returns the minimal up-right rectangle 
   containing the rotated rectangle."
  (rotated-rect-bounding-rect self))


(defmethod center ((self cv-rotated-rect))
  "Gets the center of a ROTATED-RECT object."
  (rotated-rect-center self))

;; Using mem-aref to access the member may not be safe if OpenCv changes something.
(defmethod (setf center) ((val cv-point) (self cv-rotated-rect))
  (setf (mem-aref (c-pointer self) :float) (coerce (mem-aref (c-pointer val) :int) 'single-float))
  (setf (mem-aref (c-pointer self) :float 1) (coerce (mem-aref (c-pointer val) :int 1) 'single-float))
  val)


(defmethod clone ((self cv-mat))
  "Creates a full copy of the array and the underlying data."
  (mat-clone self))


(defmethod clone ((self cv-rect))
  "Creates a full copy of a RECT object"
  (rect-clone self))


(defmethod compute ((type symbol) (self cv-bf-matcher) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (apply #'descriptor-extractor-compute-bf-matcher self args))
	((eq :feature-2d type)
	 (apply #'feature-2d-compute-bf-matcher self args))))


(defmethod compute ((type symbol) (self cv-brisk) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (apply #'descriptor-extractor-compute-brisk self args))
	((eq :feature-2d type)
	 (apply #'feature-2d-compute-brisk self args))))


(defmethod compute ((type symbol) (self cv-surf) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (apply #'descriptor-extractor-compute-surf self args))
	((eq :feature-2d type)
	 (apply #'feature-2d-compute-surf self args))))


(defmethod create ((type symbol) (self cv-bf-matcher) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (%descriptor-extractor-create-bf-matcher self (%c-string-to-string (first args)
									    (length (first args)))))
	((eq :descriptor-matcher type)
	 (%descriptor-matcher-create-bf-matcher self (%c-string-to-string (first args) 
									  (length (first args)))))
	((eq :feature-2d type)
	 (%feature-2d-create-bf-matcher self (first args)))
	((eq :feature-detector type)
	 (%feature-detector-create-bf-matcher self (first args)))))


(defmethod create ((type symbol) (self cv-brisk) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (%descriptor-extractor-create-brisk self (%c-string-to-string (first args) 
								       (length (first args)))))
	((eq :descriptor-matcher type)
	 (%descriptor-matcher-create-brisk self (%c-string-to-string (first args) 
								     (length (first args)))))
	((eq :feature-2d type)
	 (%feature-2d-create-brisk self (first args)))
	((eq :feature-detector type)
	 (%feature-detector-create-brisk self (first args)))))


(defmethod create ((type symbol) (self cv-surf) &rest args)
  (cond ((eq :descriptor-extractor type)
	 (%descriptor-extractor-create-surf self (%c-string-to-string (first args) 
								      (length (first args)))))
	((eq :descriptor-matcher type)
	 (%descriptor-matcher-create-surf self (%c-string-to-string (first args) 
								    (length (first args)))))
	((eq :feature-2d (first args))
	 (%feature-2d-create-surf self (first args)))
	((eq :feature-detector type)
	 (%feature-detector-create-surf self (first args)))))


(defmethod data ((self cv-mat))
  (mat-data self))


(defmethod data ((self std-vector-char))
  (vec-char-to-c-arr self))


(defmethod data ((self std-vector-dmatch))
  (vec-dmatch-to-c-arr self))


(defmethod data ((self std-vector-double))
  (vec-double-to-c-arr self))


(defmethod data ((self std-vector-float))
  (vec-float-to-c-arr self))


(defmethod data ((self std-vector-int))
  (vec-int-to-c-arr self))


(defmethod data ((self std-vector-key-point))
  (vec-key-point-to-c-arr self))


(defmethod data ((self std-vector-mat))
  (vec-mat-to-c-arr self))


(defmethod data ((self std-vector-point))
  (vec-point-to-c-arr self))


(defmethod data ((self std-vector-point-2f))
  (vec-point-2f-to-c-arr self))


(defmethod data ((self std-vector-rect))
  (vec-rect-to-c-arr self))


(defmethod data ((self std-vector-uchar))
  (vec-uchar-to-c-arr self))


(defmethod data ((self std-vector-vec-2d))
  (vec-vec-2d-to-c-arr self))


(defmethod data ((self std-vector-vec-3d))
  (vec-vec-3d-to-c-arr self))


(defmethod data ((self std-vector-vec-4d))
  (vec-vec-4d-to-c-arr self))


(defmethod data ((self std-vector-vec-6d))
  (vec-vec-6d-to-c-arr self))


(defmethod data ((self std-vector-vec-2f))
  (vec-vec-2f-to-c-arr self))


(defmethod data ((self std-vector-vec-3f))
  (vec-vec-3f-to-c-arr self))


(defmethod data ((self std-vector-vec-4f))
  (vec-vec-4f-to-c-arr self))


(defmethod data ((self std-vector-vec-6f))
  (vec-vec-6f-to-c-arr self))


(defmethod data ((self std-vector-vec-2i))
  (vec-vec-2i-to-c-arr self))


(defmethod data ((self std-vector-vec-3i))
  (vec-vec-3i-to-c-arr self))


(defmethod data ((self std-vector-vec-4i))
  (vec-vec-4i-to-c-arr self))


(defmethod data ((self std-vector-vec-6i))
  (vec-vec-6i-to-c-arr self))


(defmethod data ((self std-vector-vec-8i))
  (vec-vec-8i-to-c-arr self))


(defmethod descriptor-extractor-compute ((self cv-bf-matcher) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (descriptor-extractor-compute-bf-matcher self image keypoints descriptors))


(defmethod descriptor-extractor-compute ((self cv-brisk) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (descriptor-extractor-compute-brisk self image keypoints descriptors))


(defmethod descriptor-extractor-compute ((self cv-surf) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (descriptor-extractor-compute-surf self image keypoints descriptors))


(defmethod descriptor-extractor-create ((self cv-bf-matcher) descriptor-extractor-type)
  (%descriptor-extractor-create-bf-matcher self (%c-string-to-string descriptor-extractor-type (length descriptor-extractor-type))))


(defmethod descriptor-extractor-create ((self cv-brisk) descriptor-extractor-type)
  (%descriptor-extractor-create-brisk self (%c-string-to-string descriptor-extractor-type (length descriptor-extractor-type))))


(defmethod descriptor-extractor-create ((self cv-surf) descriptor-extractor-type)
  (%descriptor-extractor-create-surf self (%c-string-to-string descriptor-extractor-type (length descriptor-extractor-type))))


(defmethod descriptor-matcher-create ((self cv-bf-matcher) descriptor-matcher-type)
  (%descriptor-matcher-create-bf-matcher self (%c-string-to-string descriptor-matcher-type (length descriptor-matcher-type))))


(defmethod descriptor-matcher-create ((self cv-brisk) descriptor-matcher-type)
  (%descriptor-matcher-create-brisk self (%c-string-to-string descriptor-matcher-type (length descriptor-matcher-type))))


(defmethod descriptor-matcher-create ((self cv-surf) descriptor-matcher-type)
  (%descriptor-matcher-create-surf self (%c-string-to-string descriptor-matcher-type (length descriptor-matcher-type))))


(defmethod descriptor-matcher-match ((self cv-bf-matcher) query-descriptors train-descriptors matches 
				     &optional (mask (%mat) given-mask))
  "Finds the best match for each descriptor from a query set."
  (%descriptor-matcher-match-bf-matcher self query-descriptors train-descriptors matches mask)
  (if given-mask nil (del-mat mask)))


(defmethod descriptor-matcher-match ((self cv-flann-based-matcher) query-descriptors train-descriptors matches 
				     &optional (mask (%mat) given-mask))
  "Finds the best match for each descriptor from a query set."
  (%descriptor-matcher-match-flann-based-matcher self query-descriptors train-descriptors matches mask)
  (if given-mask nil (del-mat mask)))


(defmethod detect ((self cv-bf-matcher) &rest args)
  "Detects keypoints in an image."
  (apply #'feature-detector-detect self args))


(defmethod detect ((self cv-brisk) &rest args)
  "Detects keypoints in an image."
  (apply #'feature-detector-detect self args))


(defmethod detect ((self cv-fast-feature-detector) &rest args)
  "Detects keypoints in an image."
  (apply #'feature-detector-detect self args))


(defmethod detect ((self cv-surf) &rest args)
  "Detects keypoints in an image."
  (apply #'feature-detector-detect self args))


(defmethod dot ((self cv-mat) (other cv-mat))
  (mat-dot self other))


(defmethod dot ((self cv-point) (other cv-point))
  (dot-2i self other))


(defmethod dot ((self cv-point-2d) (other cv-point-2d))
  (dot-2d self other))


(defmethod dot ((self cv-point-2f) (other cv-point-2f))
  (dot-2f self other))


(defmethod dot ((self cv-point-3d) (other cv-point-3d))
  (dot-3d self other))


(defmethod dot ((self cv-point-3f) (other cv-point-3f))
  (dot-3f self other))


(defmethod dot ((self cv-point-3i) (other cv-point-3i))
  (dot-3i self other))


(defmethod feature-2d-compute ((self cv-bf-matcher) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (feature-2d-compute-bf-matcher self image keypoints descriptors))


(defmethod feature-2d-compute ((self cv-brisk) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (feature-2d-compute-brisk self image keypoints descriptors))


(defmethod feature-2d-compute ((self cv-surf) image keypoints descriptors)
  "Computes the descriptors for a set of keypoints detected in an image."
  (feature-2d-compute-surf self image keypoints descriptors))


(defmethod feature-2d-create ((self cv-bf-matcher) name)
  (%feature-2d-create-bf-matcher self name))


(defmethod feature-2d-create ((self cv-brisk) name)
  (%feature-2d-create-brisk self name))


(defmethod feature-2d-create ((self cv-surf) name)
  (%feature-2d-create-surf self name))


(defmethod feature-detector-create ((self cv-bf-matcher) detector-type)
  (%feature-detector-create-bf-matcher self detector-type))


(defmethod feature-detector-create ((self cv-brisk) detector-type)
  (%feature-detector-create-brisk self detector-type))


(defmethod feature-detector-create ((self cv-surf) detector-type)
  (%feature-detector-create-surf self detector-type))


(defmethod feature-detector-detect ((self cv-bf-matcher) image keypoints &optional (mask (%mat) given-mask))
  "Detects keypoints in an image."
  (%feature-detector-detect-bf-matcher self image keypoints mask)
  (if given-mask nil (del-mat mask)))


(defmethod feature-detector-detect ((self cv-brisk) image keypoints &optional (mask (%mat) given-mask))
  "Detects keypoints in an image."
  (%feature-detector-detect-brisk self image keypoints mask)
  (if given-mask nil (del-mat mask)))


(defmethod feature-detector-detect ((self cv-fast-feature-detector) image keypoints &optional (mask (%mat) given-mask))
  "Detects keypoints in an image."
  (%feature-detector-detect-fast-feature-detector self image keypoints mask)
  (if given-mask nil (del-mat mask)))


(defmethod feature-detector-detect ((self cv-surf) image keypoints &optional (mask (%mat) given-mask))
  "Detects keypoints in an image."
  (%feature-detector-detect-surf self image keypoints mask)
  (if given-mask nil (del-mat mask)))


(defmethod file-storage-write ((fs cv-file-storage) (name string) (value float))
  (if (typep value 'double-float)
      (file-storage-write-double fs (%c-string-to-string name (length name)) value)
      (file-storage-write-float fs (%c-string-to-string name (length name)) value)))


(defmethod file-storage-write ((fs cv-file-storage) (name string) (value integer))
  (file-storage-write-int fs (%c-string-to-string name (length name)) value))


(defmethod file-storage-write ((fs cv-file-storage) (name string) (value cv-mat))
  (file-storage-write-mat fs (%c-string-to-string name (length name)) value))


(defmethod file-storage-write ((fs cv-file-storage) (name string) (value string))
  (file-storage-write-string fs (%c-string-to-string name (length name)) 
			     (%c-string-to-string value (length value))))


(defmethod file-storage-write ((fs cv-file-storage) (name string) (value std-vector-key-point))
  (file-storage-write-vector-key-point fs (%c-string-to-string name (length name)) value))


(defmethod get* ((self cv-video-capture) (value integer))
  (video-capture-get self value))


(defmethod height ((self cv-rect))
  "Retrieves HEIGHT value of a RECT object."
  (mem-aref (c-pointer self) :int 3))


(defmethod (setf height) ((val integer) (self cv-rect))
  "Sets HEIGHT value of a RECT object."
  (rect-set-height self val))


(defmethod height ((self cv-size))
  "Retrieves HEIGHT value of a SIZE object."
  (size-height self))


(defmethod (setf height) ((val double-float) (self cv-size))
  "Sets HEIGHT value of a SIZE object."
  (size-set-height self val))


(defmethod is-opened ((self cv-video-capture))
  (video-capture-is-opened self))


(defmethod is-opened ((self cv-video-writer))
  (video-writer-is-opened self))


(defmethod load ((self cv-svm) &rest args)
  (apply #'stat-model-load self args))


(defmethod length ((self std-vector-char))
  (vec-char-length self))


(defmethod length ((self std-vector-dmatch))
  (vec-dmatch-length self))


(defmethod length ((self std-vector-double))
  (vec-double-length self))


(defmethod length ((self std-vector-float))
  (vec-float-length self))


(defmethod length ((self std-vector-int))
  (vec-int-length self))


(defmethod length ((self std-vector-key-point))
  (vec-key-point-length self))


(defmethod length ((self std-vector-mat))
  (vec-mat-length self))


(defmethod length ((self std-vector-point))
  (vec-point-length self))


(defmethod length ((self std-vector-point-2f))
  (vec-point-2f-length self))


(defmethod length ((self std-vector-rect))
  (vec-rect-length self))


(defmethod length ((self std-vector-uchar))
  (vec-uchar-length self))


(defmethod length ((self std-vector-vec-2d))
  (vec-vec-2d-length self))


(defmethod length ((self std-vector-vec-3d))
  (vec-vec-3d-length self))


(defmethod length ((self std-vector-vec-4d))
  (vec-vec-4d-length self))


(defmethod length ((self std-vector-vec-6d))
  (vec-vec-6d-length self))


(defmethod length ((self std-vector-vec-2f))
  (vec-vec-2f-length self))


(defmethod length ((self std-vector-vec-3f))
  (vec-vec-3f-length self))


(defmethod length ((self std-vector-vec-4f)) 
  (vec-vec-4f-length self))


(defmethod length ((self std-vector-vec-6f))
  (vec-vec-6f-length self))


(defmethod length ((self std-vector-vec-2i))
  (vec-vec-2i-length self))


(defmethod length ((self std-vector-vec-3i))
  (vec-vec-3i-length self))


(defmethod length ((self std-vector-vec-4i))
  (vec-vec-4i-length self))


(defmethod length ((self std-vector-vec-6i))
  (vec-vec-6i-length self))


(defmethod length ((self std-vector-vec-8i))
  (vec-vec-8i-length self))


(defmethod match ((self cv-flann-based-matcher) &rest args)
  "Finds the best match for each descriptor from a query set."
  (apply #'descriptor-matcher-match self args))


(defmethod mean ((self cv-mat) &rest args)
  "Calculates an average mean of matrix elements."
  (apply #'%mean self args))


(defmethod mean ((self cv-pca) &rest args)
  args
  (pca-mean self))


(defmethod (setf mean) ((val cv-mat) (self cv-pca))
  (pca-set-mean self val))


(defmethod open ((self cv-file-storage) &rest args)
  (apply #'file-storage-open self args))


(defmethod open (self &rest args)
  (apply #'cl::open self args))


(defmethod pt ((self cv-key-point))
  "Retrieves PT value of a KEY-POINT object."
  (%key-point-pt self))


(defmethod (setf pt) ((val single-float) (self cv-key-point))
  "Sets PT value of a KEY-POINT object."
  (key-point-set-pt self val))


(defmethod push-back ((self cv-mat) (val cv-mat))
  "Adds a new element at the end of the vector."
  (mat-push-back self val))


(defmethod push-back ((self std-vector-char) val)
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_c_push_back_num" 
		   :pointer (c-pointer self) 
		   :char  val))


(defmethod push-back ((self std-vector-dmatch) (val cv-dmatch))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_dm_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-double) (val double-float))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_d_push_back_num" 
		   :pointer (c-pointer self) 
		   :double val))


(defmethod push-back ((self std-vector-float) (val single-float))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_f_push_back_num" 
		   :pointer (c-pointer self) 
		   :float val))


(defmethod push-back ((self std-vector-int) (val integer))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_i_push_back_num" 
		   :pointer (c-pointer self) 
		   :int val))


(defmethod push-back ((self std-vector-key-point) (val cv-key-point))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_kp_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-mat) (val cv-mat))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_m_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-point) (val cv-point))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_p_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-point-2f) (val cv-point-2f))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_p2f_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-rect) (val cv-rect))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_r_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-vec-2d) (val cv-vec-2d))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_v2d_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-vec-3d) (val cv-vec-3d))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_v3d_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-vec-4d) (val cv-vec-4d))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_v4d_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-vec-6d) (val cv-vec-6d))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_v6d_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-vec-2f) (val cv-vec-2f))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_v2f_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-vec-3f) (val cv-vec-3f))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_v3f_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-vec-4f) (val cv-vec-4f))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_v4f_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-vec-6f) (val cv-vec-6f))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_v6f_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-vec-2i) (val cv-vec-2i))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_v2i_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-vec-3i) (val cv-vec-3i))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_v3i_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-vec-4i) (val cv-vec-4i))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_v4i_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-vec-6i) (val cv-vec-6i))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_v6i_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-vec-8i) (val cv-vec-8i))
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_v8i_push_back" 
		   :pointer (c-pointer self) 
		   :pointer (c-pointer val)))


(defmethod push-back ((self std-vector-uchar) val)
  "Adds a new element at the end of the vector."
  (foreign-funcall "cv_vector_u_push_back_num" 
		   :pointer (c-pointer self) 
		   :uchar  val))


(defmethod release ((self cv-file-storage))
  (file-storage-release self))


(defmethod release ((self cv-video-capture))
  (video-capture-release self))


(defmethod save ((self cv-svm) &rest args)
  (apply #'stat-model-save self args))


(defmethod size ((arg cv-key-point) &rest args)
  args
  (%key-point-size arg))


(defmethod (setf size) ((val single-float) (self cv-key-point))
  (key-point-set-size self val))


(defmethod size ((arg cv-mat) &rest args)
  args
  (mat-size arg))


(defmethod size ((arg cv-range) &rest args)
  args
  (range-size arg))


(defmethod size ((arg cv-rect) &rest args)
  args
  (rect-size arg))


(defmethod (setf size) ((val cv-size) (self cv-rect))
  (setf (@ self :int 0) (@ self :int 0)
	(@ self :int 1) (@ self :int 1)
	(@ self :int 2) (@ val :int 0)
	(@ self :int 3) (@ val :int 1))val)


(defmethod size ((arg cv-rotated-rect) &rest args)
  "Gets the size of a ROTATED-RECT object."
  args
  (rotated-rect-size arg))


;; Using mem-aref to access the member may not be safe if OpenCv changes something.
(defmethod (setf size) ((val cv-size) (self cv-rotated-rect))
  (setf (mem-aref (c-pointer self) :float 2) (coerce (mem-aref (c-pointer val) :int) 'single-float))
  (setf (mem-aref (c-pointer self) :float 3) (coerce (mem-aref (c-pointer val) :int 1) 'single-float))
  val)


(defmethod size ((arg null) &rest args)
  args
  (if (eq arg nil) (size-0) nil))


(defmethod size ((arg real) &rest args)
  (size-2 (coerce arg 'double-float) (coerce (or (first args) 0) 'double-float)))


(defmethod train ((self cv-svm) &rest args)
  (apply #'svm-train self args))


(defmethod type* ((self cv-term-criteria))
  "Gets the type of a TERM-CRITERIA object."
  (term-criteria-type self))


(defmethod (setf type*) ((val integer) (self cv-term-criteria))
  "Sets the type of a TERM-CRITERIA object."
  (term-criteria-set-type self val))


(defmethod (setf type*) ((val integer) (self cv-term-criteria))
  "Sets the type of a TERM-CRITERIA object."
  (term-criteria-set-type self val))


(defmethod type* ((self cv-mat))
  "Gets the type of a MAT object."
  (mat-type self))


(defmethod (setf type*) ((val integer) (self cv-mat))
  "Sets the type of a MAT object."
  (mat-set-type self val))


(defmethod width ((self cv-rect))
  "Retrieves WIDTH value of a RECT object."
  (rect-width self))


(defmethod (setf width) ((val integer) (self cv-rect))
  "Sets WIDTH value of a RECT object."
  (rect-set-width self val))


(defmethod width ((self cv-size))
  "Retrieves WIDTH value of a SIZE object."
  (size-width self))


(defmethod (setf width) ((val double-float) (self cv-size))
  "Sets WIDTH value of a SIZE object."
  (size-set-width self val))


(defmethod x ((self cv-key-point))
  "Retrieves X coordinate of a KEY-POINT object."
  (point-2f-x (%key-point-pt self)))


(defmethod (setf x) ((val single-float) (self cv-key-point))
  "Sets X coordinate of a KEY-POINT object."
  (point-2f-x (key-point-set-pt self val)))


(defmethod x ((self cv-point))
  "Retrieves X coordinate of a POINT object."
  (point-x self))


(defmethod (setf x) ((val integer) (self cv-point))
  "Sets X coordinate of a POINT object."
  (point-set-x self val))


(defmethod x ((self cv-point-2d))
  "Retrieves X coordinate of a POINT-2D object."
  (point-2d-x self))


(defmethod (setf x) ((val double-float) (self cv-point-2d))
  "Sets X coordinate of a POINT-2D object."
  (point-2d-set-x self val))


(defmethod x ((self cv-point-2f))
  "Retrieves X coordinate of a POINT-2F object."
  (point-2f-x self))


(defmethod (setf x) ((val single-float) (self cv-point-2f))
  "Sets X coordinate of a POINT-2F object."
  (point-2f-set-x self val))


(defmethod x ((self cv-point-3d))
  "Retrieves X coordinate of a POINT-3D object."
  (point-3d-x self))


(defmethod (setf x) ((val double-float) (self cv-point-3d))
  "Sets X coordinate of a POINT-3D object."
  (point-3d-set-x self val))


(defmethod x ((self cv-point-3f))
  "Retrieves X coordinate of a POINT-3F object."
  (point-3f-x self))


(defmethod (setf x) ((val single-float) (self cv-point-3f))
  "Sets X coordinate of a POINT-3F object."
  (point-3f-set-x self val))


(defmethod x ((self cv-point-3i))
  "Retrieves X coordinate of a POINT-3I object."
  (point-3i-x self))


(defmethod (setf x) ((val integer) (self cv-point-3i))
  "Sets X coordinate of a POINT-3I object."
  (point-3i-set-x self val))


(defmethod x ((self cv-rect))
  "Retrieves X coordinate of a RECT object."
  (rect-x self))


(defmethod (setf x) ((val integer) (self cv-rect))
  "Sets x coordinate of a RECT object."
  (rect-set-x self val))


(defmethod y ((self cv-key-point))
  "Retrieves Y coordinate of a KEY-POINT object."
  (point-2f-y (%key-point-pt self)))


(defmethod (setf y) ((val single-float) (self cv-key-point))
  "Sets Y coordinate of a KEY-POINT object."
  (point-2f-y (key-point-set-pt self val)))


(defmethod y ((self cv-point))
  "Retrieves Y coordinate of a POINT object."
  (point-y self))


(defmethod (setf y) ((val integer) (self cv-point))
  "Sets Y coordinate of a POINT object."
  (point-set-y self val))


(defmethod y ((self cv-point-2d))
  "Retrieves Y coordinate of a POINT-2D object."
  (point-2d-y self))


(defmethod (setf y) ((val double-float) (self cv-point-2d))
  "Sets Y coordinate of a POINT-2D object."
  (point-2d-set-y self val))


(defmethod y ((self cv-point-2f))
  "Retrieves Y coordinate of a POINT-2F object."
  (point-2f-y self))


(defmethod (setf y) ((val single-float) (self cv-point-2f))
  "Sets Y coordinate of a POINT-2F object."
  (point-2f-set-y self val))


(defmethod y ((self cv-point-3d))
  "Retrieves Y coordinate of a POINT-3D object."
  (point-3d-y self))


(defmethod (setf y) ((val double-float) (self cv-point-3d))
  "Sets Y coordinate of a POINT-3D object."
  (point-3d-set-y self val))


(defmethod y ((self cv-point-3f))
  "Retrieves Y coordinate of a POINT-3F object."
  (point-3f-y self))


(defmethod (setf y) ((val single-float) (self cv-point-3f))
  "Sets Y coordinate of a POINT-3F object."
  (point-3f-set-y self val))


(defmethod y ((self cv-point-3i))
  "Retrieves Y coordinate of a POINT-3I object."
  (point-3i-y self))


(defmethod (setf y) ((val integer) (self cv-point-3i))
  "Sets Y coordinate of a POINT-3I object."
  (point-3i-set-y self val))


(defmethod y ((self cv-rect))
  "Retrieves Y coordinate of a RECT object."
  (rect-y self))


(defmethod (setf y) ((val integer) (self cv-rect))
  "Sets y coordinate of a RECT object."
  (rect-set-y self val))


(defmethod z ((self cv-point-3d))
  "Retrieves Z coordinate of a POINT-3D object."
  (point-3d-z self))


(defmethod (setf z) ((val double-float) (self cv-point-3d))
  "Sets Z coordinate of a POINT-3D object."
  (point-3d-set-z self val))


(defmethod z ((self cv-point-3f))
  "Retrieves Z coordinate of a POINT-3F object."
  (point-3f-z self))


(defmethod (setf z) ((val single-float) (self cv-point-3f))
  "Sets Z coordinate of a POINT-3F object."
  (point-3f-set-z self val))


(defmethod z ((self cv-point-3i))
  "Retrieves Z coordinate of a POINT-3I object."
  (point-3i-z self))


(defmethod (setf z) ((val integer) (self cv-point-3i))
  "Sets Z coordinate of a POINT-3I object."
  (point-3i-set-z self val))


;;; Functions and methods used to 
;;; re-import shadowed symbols.


(defmethod abs ((self cv-mat))
  "Calculates an absolute value of each matrix element."
  (%abs self))


(defmethod abs ((self number))
  (cl:abs self))


(defun exp (&rest args)
  (cond ((typep (first args) 'cv-mat)
	 (apply #'%exp args))
	(t (apply #'cl:exp args))))


(defun fill (arg &rest args)
  (cond ((or (listp arg) (vectorp arg))
	 (apply #'cl:fill arg args))
	((typep arg 'cv-rng)
	 (apply #'rng-fill arg args))))


(defun log (&rest args)
  (cond ((typep (first args) 'cv-mat)
	 (apply #'%log args))
	(t (apply #'cl:log args))))


(defun max (&rest args)
  (cond ((realp (first args)) (apply #'cl:max args))
	((typep (first args) 'cv-mat) (apply #'%max args))))


(defun min (&rest args)
  (cond ((realp (first args)) (apply #'cl:min args))
	((typep (first args) 'cv-mat) (apply #'%min args))))


(defun read (&rest args)
  (cond ((typep (first args) 'cv-video-capture)
	 (apply #'video-capture-read args))
	(t (apply #'cl:read args))))


(defun set (&rest args)
  (cond ((symbolp (first args))
	 (apply #'cl:set args))
	((typep (first args) 'cv-video-capture) 
	 (video-capture-set (first args) (second args) (coerce (or (third args) 0d0) 'double-float)))))


(defun write (&rest args)

  (let ((val (second args)))

    (cond ((keywordp val)
	   (apply #'cl:write args))

	  ((null (cdr args))
	   (apply #'cl:write args))

	  ((typep val 'string) (apply #'file-storage-write args))

          ((typep val 'cv-mat) (apply #'video-writer-write args)))))


