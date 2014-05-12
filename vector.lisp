;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; vector.lisp
;;;; OpenCV bindings
;;;; Bindings for the C++ Vector Class

(in-package :lisp-cv)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorc" %vec-char) vector-char)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorc" c-arr-to-vec-char) vector-char
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-char (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :char :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-char (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorc_to_carray" vec-char-to-c-arr) :pointer 
  (v vector-char))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorc_length" vec-char-length) :unsigned-int
  (self vector-char))


(defmacro vec-char-to-lisp-list (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-char-length ,x))
            (,z (list)))
       (dotimes (i ,y)
	 (push (mem-aref (vec-char-to-c-arr ,x) :char i) ,z))
       (reverse ,z))))


(defmacro vec-char-to-lisp-vec (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-char-length ,x))
            (,z (make-array ,y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i ,y)
	 (vector-push (mem-aref (vec-char-to-c-arr ,x) :char i) ,z))
       ,z)))


(defmacro vec-char (&rest args)
  (if (third args)
      (error "odd number of args to VEC-CHAR")
      nil)
  (let ((x (gensym))
        (y (gensym)))
    `(let ((,x (first (list ,@args)))
	   (,y (second (list ,@args))))
       (cond ((eq ,x nil)
	      (%vec-char))
	     ((or (vectorp ,x) (listp ,x))
	      (arr-to-vec-char ,x))
	     ((eq :length ,x)
	      (vec-char-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (vec-char-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (vec-char-to-lisp-vec ,y))
	     ((typep ,x 'std-vector-char)
	      (if (eq ,y nil)
		  (mem-aref (vec-char-to-c-arr ,x) :char) 
		  (mem-aref (vec-char-to-c-arr ,x) :char ,y)))
	     (t (error "incorrect input. 
                   ~%See VEC-CHAR documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectordm" %vec-dmatch) vector-dmatch)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectordm" c-arr-to-vec-dmatch) vector-dmatch
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-dmatch (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-dmatch (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectordm_to_carray" vec-dmatch-to-c-arr) :pointer 
  (v vector-dmatch))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectordm_length" vec-dmatch-length) :unsigned-int
  (self vector-dmatch))


(defmacro vec-dmatch-to-lisp-list (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-dmatch-length ,x))
            (,z (list)))
       (dotimes (i ,y)
	 (push (mem-aref (vec-dmatch-to-c-arr ,x) 'dmatch i) ,z))
       (reverse ,z))))


(defmacro vec-dmatch-to-lisp-vec (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-dmatch-length ,x))
            (,z (make-array ,y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i ,y)
	 (vector-push (mem-aref (vec-dmatch-to-c-arr ,x) 'dmatch i) ,z))
       ,z)))


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
	      (vec-dmatch-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (vec-dmatch-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (vec-dmatch-to-lisp-vec ,y))
	     ((and (typep ,x 'std-vector-dmatch) ,y)
	      (if (eq ,z nil)
		  (mem-aref (vec-dmatch-to-c-arr ,x) 'dmatch ,y)
		  (if (null ,type)
		      (mem-aref (c-pointer (mem-aref (vec-dmatch-to-c-arr ,x) 'dmatch ,y)) :int ,z)
		      (mem-aref (c-pointer (mem-aref (vec-dmatch-to-c-arr ,x) 'dmatch ,y)) ,type ,z)))) 
	     (t (error "incorrect input. 
  ~%See VEC-DMATCH documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectord" %vec-double) vector-double)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectord" c-arr-to-vec-double) vector-double
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-double (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :double :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-double (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectord_to_carray" vec-double-to-c-arr) :pointer 
  (v vector-double))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectord_length" vec-double-length) :unsigned-int
  (self vector-double))


(defmacro vec-double-to-lisp-list (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-double-length ,x))
            (,z (list)))
       (dotimes (i ,y)
	 (push (mem-aref (vec-double-to-c-arr ,x) :double i) ,z))
       (reverse ,z))))


(defmacro vec-double-to-lisp-vec (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-double-length ,x))
            (,z (make-array ,y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i ,y)
	 (vector-push (mem-aref (vec-double-to-c-arr ,x) :double i) ,z))
       ,z)))


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
	      (vec-double-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (vec-double-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (vec-double-to-lisp-vec ,y))
	     ((typep ,x 'std-vector-double)
	      (if (eq ,y nil)
		  (mem-aref (vec-double-to-c-arr ,x) :double) 
		  (mem-aref (vec-double-to-c-arr ,x) :double ,y)))
	     (t (error "incorrect input. 
                   ~%See VEC-DOUBLE documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorf" %vec-float) vector-float)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorf" c-arr-to-vec-float) vector-float
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-float (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :float :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-float (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorf_to_carray" vec-float-to-c-arr) :pointer 
  (v vector-float))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorf_length" vec-float-length) :unsigned-int
  (self vector-float))


(defmacro vec-float-to-lisp-list (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)jessemorris77
	    (,y (vec-float-length ,x))
            (,z (list)))
       (dotimes (i ,y)
	 (push (mem-aref (vec-float-to-c-arr ,x) :float i) ,z))
       (reverse ,z))))


(defmacro vec-float-to-lisp-vec (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-float-length ,x))
            (,z (make-array ,y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i ,y)
	 (vector-push (mem-aref (vec-float-to-c-arr ,x) :float i) ,z))
       ,z)))


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
	      (vec-float-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (vec-float-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (vec-float-to-lisp-vec ,y))
	     ((typep ,x 'std-vector-float)
	      (if (eq ,y nil)
		  (mem-aref (vec-float-to-c-arr ,x) :float) 
		  (mem-aref (vec-float-to-c-arr ,x) :float ,y)))
	     (t (error "incorrect input. 
                   ~%See VEC-FLOAT documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectori" %vec-int) vector-int)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectori" c-arr-to-vec-int) vector-int
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-int (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :int :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-int (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectori_to_carray" vec-int-to-c-arr) :pointer 
  (v vector-int))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectori_length" vec-int-length) :unsigned-int
  (self vector-int))


(defmacro vec-int-to-lisp-list (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-int-length ,x))
            (,z (list)))
       (dotimes (i ,y)
	 (push (mem-aref (vec-int-to-c-arr ,x) :int i) ,z))
       (reverse ,z))))


(defmacro vec-int-to-lisp-vec (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-int-length ,x))
            (,z (make-array ,y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i ,y)
	 (vector-push (mem-aref (vec-int-to-c-arr ,x) :int i) ,z))
       ,z)))


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
	      (vec-int-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (vec-int-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (vec-int-to-lisp-vec ,y))
	     ((typep ,x 'std-vector-int)
	      (if (eq ,y nil)
		  (mem-aref (vec-int-to-c-arr ,x) :int) 
		  (mem-aref (vec-int-to-c-arr ,x) :int ,y)))
	     (t (error "incorrect input. 
                   ~%See VEC-INT documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorkp" %vec-key-point) vector-key-point)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorkp" c-arr-to-vec-key-point) vector-key-point
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-key-point (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-key-point (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorkp_to_carray" vec-key-point-to-c-arr) :pointer 
  (v vector-key-point))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorkp_length" vec-key-point-length) :unsigned-int
  (self vector-key-point))


(defmacro vec-key-point-to-lisp-list (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-key-point-length ,x))
            (,z (list)))
       (dotimes (i ,y)
	 (push (mem-aref (vec-key-point-to-c-arr ,x) 'key-point i) ,z))
       (reverse ,z))))


(defmacro vec-key-point-to-lisp-vec (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-key-point-length ,x))
            (,z (make-array ,y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i ,y)
	 (vector-push (mem-aref (vec-key-point-to-c-arr ,x) 'key-point i) ,z))
       ,z)))


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
	      (vec-key-point-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (vec-key-point-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (vec-key-point-to-lisp-vec ,y))
	     ((and (typep ,x 'std-vector-key-point) ,y)
	      (if (eq ,z nil)
		  (mem-aref (vec-key-point-to-c-arr ,x) 'key-point ,y)
		  (if (< ,z 5)  (mem-aref (c-pointer 
					   (mem-aref (vec-key-point-to-c-arr ,x) 
						     'key-point ,y)) :float ,z)
		      (mem-aref (c-pointer 
				 (mem-aref (vec-key-point-to-c-arr ,x) 'key-point ,y)) :int ,z))))
	     (t (error "incorrect input. 
  ~%See VEC-KEY-POINT documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorp" %vec-point) vector-point)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorp" c-arr-to-vec-point) vector-point
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-point (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-point (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorp_to_carray" vec-point-to-c-arr) :pointer 
  (v vector-point))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorp_length" vec-point-length) :unsigned-int
  (self vector-point))


(defmacro vec-point-to-lisp-list (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-point-length ,x))
            (,z (list)))
       (dotimes (i ,y)
	 (push (mem-aref (vec-point-to-c-arr ,x) 'point i) ,z))
       (reverse ,z))))


(defmacro vec-point-to-lisp-vec (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-point-length ,x))
            (,z (make-array ,y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i ,y)
	 (vector-push (mem-aref (vec-point-to-c-arr ,x) 'point i) ,z))
       ,z)))


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
	      (vec-point-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (vec-point-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (vec-point-to-lisp-vec ,y))
	     ((and (typep ,x 'std-vector-point) ,y)
	      (if (eq ,z nil)
		  (mem-aref (vec-point-to-c-arr ,x) 'point ,y)
		  (mem-aref (c-pointer (mem-aref (vec-point-to-c-arr ,x) 'point ,y)) :int ,z)))
	     (t (error "incorrect input. 
  ~%See VEC-POINT documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorp2f" %vec-point-2f) vector-point-2f)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorp2f" c-arr-to-vec-point-2f) vector-point-2f
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-point-2f (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-point-2f (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorp2f_to_carray" vec-point-2f-to-c-arr) :pointer 
  (v vector-point-2f))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorp2f_length" vec-point-2f-length) :unsigned-int
  (self vector-point-2f))


(defmacro vec-point-2f-to-lisp-list (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-point-2f-length ,x))
            (,z (list)))
       (dotimes (i ,y)
	 (push (mem-aref (vec-point-2f-to-c-arr ,x) 'point-2f i) ,z))
       (reverse ,z))))


(defmacro vec-point-2f-to-lisp-vec (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-point-2f-length ,x))
            (,z (make-array ,y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i ,y)
	 (vector-push (mem-aref (vec-point-2f-to-c-arr ,x) 'point-2f i) ,z))
       ,z)))


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
	      (vec-point-2f-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (vec-point-2f-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (vec-point-2f-to-lisp-vec ,y))
	     ((and (typep ,x 'std-vector-point-2f) ,y)
	      (if (eq ,z nil)
		  (mem-aref (vec-point-2f-to-c-arr ,x) 'point-2f ,y)
		  (mem-aref (c-pointer (mem-aref (vec-point-2f-to-c-arr ,x) 'point-2f ,y)) :float ,z)))
	     (t (error "incorrect input. 
  ~%See VEC-POINT-2F documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorr" %vec-rect) vector-rect)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorr" c-arr-to-vec-rect) vector-rect
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-rect (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-rect (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorr_to_carray" vec-rect-to-c-arr) :pointer 
  (v vector-rect))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorr_length" vec-rect-length) :unsigned-int
  (self vector-rect))


(defmacro vec-rect-to-lisp-list (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-rect-length ,x))
            (,z (list)))
       (dotimes (i ,y)
	 (push (mem-aref (vec-rect-to-c-arr ,x) 'rect i) ,z))
       (reverse ,z))))


(defmacro vec-rect-to-lisp-vec (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-rect-length ,x))
            (,z (make-array ,y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i ,y)
	 (vector-push (mem-aref (vec-rect-to-c-arr ,x) 'rect i) ,z))
       ,z)))


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
	      (vec-rect-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (vec-rect-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (vec-rect-to-lisp-vec ,y))
	     ((and (typep ,x 'std-vector-rect) ,y)
	      (if (eq ,z nil)
		  (mem-aref (vec-rect-to-c-arr ,x) 'rect ,y)
		  (mem-aref (c-pointer (mem-aref (vec-rect-to-c-arr ,x) 'rect ,y)) :int ,z)))
	     (t (error "incorrect input. 
  ~%See VEC-RECT documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectoru" %vec-uchar) vector-uchar)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectoru" c-arr-to-vec-uchar) vector-uchar
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-uchar (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :uchar :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-uchar (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectoru_to_carray" vec-uchar-to-c-arr) :pointer 
  (v vector-uchar))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectoru_length" vec-uchar-length) :unsigned-int
  (self vector-uchar))


(defmacro vec-uchar-to-lisp-list (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-uchar-length ,x))
            (,z (list)))
       (dotimes (i ,y)
	 (push (mem-aref (vec-uchar-to-c-arr ,x) :uchar i) ,z))
       (reverse ,z))))


(defmacro vec-uchar-to-lisp-vec (c-arr)
  (let ((x (gensym))
        (y (gensym))
        (z (gensym)))
    `(let* ((,x ,c-arr)
	    (,y (vec-uchar-length ,x))
            (,z (make-array ,y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i ,y)
	 (vector-push (mem-aref (vec-uchar-to-c-arr ,x) :uchar i) ,z))
       ,z)))


(defmacro vec-uchar (&rest args)
  (if (third args)
      (error "odd number of args to VEC-UCHAR")
      nil)
  (let ((x (gensym))
        (y (gensym)))
    `(let ((,x (first (list ,@args)))
	   (,y (second (list ,@args))))
       (cond ((eq ,x nil)
	      (%vec-uchar))
	     ((or (vectorp ,x) (listp ,x))
	      (arr-to-vec-uchar ,x))
	     ((eq :length ,x)
	      (vec-uchar-length ,y))
	     ((and (eq :to-lisp-list ,x))
	      (vec-uchar-to-lisp-list ,y))
	     ((and (eq :to-lisp-vec ,x))
	      (vec-uchar-to-lisp-vec ,y))
	     ((typep ,x 'std-vector-uchar)
	      (if (eq ,y nil)
		  (mem-aref (vec-uchar-to-c-arr ,x) :uchar) 
		  (mem-aref (vec-uchar-to-c-arr ,x) :uchar ,y)))
	     (t (error "incorrect input. 
                   ~%See VEC-UCHAR documentation in <LISP-CV-SOURCE-DIR>/EXAMPLES.LISP~%"))))))



