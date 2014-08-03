;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; vector.lisp
;;;; OpenCV bindings
;;;; Bindings for the C++ Vector Class

(in-package :lisp-cv)


;; template < class T, class Alloc = allocator<T> > class vector.
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorc" make-vector-char) vector-char)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorc" c-arr-to-vec-char) vector-char
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-char (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :char :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-char (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorc_to_carray" vec-char-to-c-arr) :pointer 
  (v vector-char))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorc_length" vec-char-length) :unsigned-int
  (self vector-char))


(defun vec-char-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-char-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-char-to-c-arr x) :char i) z))
       (reverse z)))


(defun vec-char-to-lisp-vec (vec length)
    (let* ((w vec)
	    (x (vec-char-length w))
	    (y (if length length x))
	    (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))

       (dotimes (i x)
	 (vector-push (mem-aref (vec-char-to-c-arr w) :char i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-char-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-char-to-lisp-vec y z))
	     ((typep x 'std-vector-char)
	      (if (eq y nil)
		  (mem-aref (vec-char-to-c-arr x) :char) 
		  (mem-aref (vec-char-to-c-arr x) :char y)))
	     (t (error "incorrect input. 
                   ~%See VECTOR-CHAR documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectordm" make-vector-dmatch) vector-dmatch)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectordm" c-arr-to-vec-dmatch) vector-dmatch
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-dmatch (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-dmatch (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectordm_to_carray" vec-dmatch-to-c-arr) :pointer 
  (v vector-dmatch))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectordm_length" vec-dmatch-length) :unsigned-int
  (self vector-dmatch))


(defun vec-dmatch-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-dmatch-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-dmatch-to-c-arr x) 'dmatch i) z))
       (reverse z)))


(defun vec-dmatch-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-dmatch-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-dmatch-to-c-arr x) 'dmatch i) z))
       z))


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
	     ((and (eq :to-lisp-list w))
	      (vec-dmatch-to-lisp-list x))
	     ((and (eq :to-lisp-vec w))
	      (vec-dmatch-to-lisp-vec x))
	     ((and (typep w 'std-vector-dmatch) x)
	      (if (eq y nil)
		  (mem-aref (vec-dmatch-to-c-arr w) 'dmatch x)
		  (if z
		      (mem-aref (c-pointer (mem-aref (vec-dmatch-to-c-arr w) 'dmatch x)) z y)
		      (error "must supply a type as the fourth parameter")))) 
	     (t (error "incorrect input. 
  ~%See VECTOR-DMATCH documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectord" make-vector-double) vector-double)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectord" c-arr-to-vec-double) vector-double
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-double (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :double :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-double (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectord_to_carray" vec-double-to-c-arr) :pointer 
  (v vector-double))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectord_length" vec-double-length) :unsigned-int
  (self vector-double))


(defun vec-double-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-double-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-double-to-c-arr x) :double i) z))
       (reverse z)))


(defun vec-double-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-double-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-double-to-c-arr x) :double i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-double-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-double-to-lisp-vec y))
	     ((typep x 'std-vector-double)
	      (if (eq y nil)
		  (mem-aref (vec-double-to-c-arr x) :double) 
		  (mem-aref (vec-double-to-c-arr x) :double y)))
	     (t (error "incorrect input. 
                   ~%See VECTOR-DOUBLE documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorf" make-vector-float) vector-float)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorf" c-arr-to-vec-float) vector-float
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-float (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :float :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-float (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorf_to_carray" vec-float-to-c-arr) :pointer 
  (v vector-float))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorf_length" vec-float-length) :unsigned-int
  (self vector-float))


(defun vec-float-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-float-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-float-to-c-arr x) :float i) z))
       (reverse z)))


(defun vec-float-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-float-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-float-to-c-arr x) :float i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-float-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-float-to-lisp-vec y))
	     ((typep x 'std-vector-float)
	      (if (eq y nil)
		  (mem-aref (vec-float-to-c-arr x) :float) 
		  (mem-aref (vec-float-to-c-arr x) :float y)))
	     (t (error "incorrect input. 
                   ~%See VECTOR-FLOAT documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectori" make-vector-int) vector-int)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectori" c-arr-to-vec-int) vector-int
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-int (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :int :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-int (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectori_to_carray" vec-int-to-c-arr) :pointer 
  (v vector-int))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectori_length" vec-int-length) :unsigned-int
  (self vector-int))


(defun vec-int-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-int-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-int-to-c-arr x) :int i) z))
       (reverse z)))


(defun vec-int-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-int-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-int-to-c-arr x) :int i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-int-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-int-to-lisp-vec y))
	     ((typep x 'std-vector-int)
	      (if (eq y nil)
		  (mem-aref (vec-int-to-c-arr x) :int) 
		  (mem-aref (vec-int-to-c-arr x) :int y)))
	     (t (error "incorrect input. 
                   ~%See VECTOR-INT documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorkp" make-vector-key-point) vector-key-point)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorkp" c-arr-to-vec-key-point) vector-key-point
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-key-point (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-key-point (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorkp_to_carray" vec-key-point-to-c-arr) key-point
  (v vector-key-point))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorkp_length" vec-key-point-length) :unsigned-int
  (self vector-key-point))


(defun vec-key-point-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-key-point-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-key-point-to-c-arr x) 'key-point i) z))
       (reverse z)))


(defun vec-key-point-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-key-point-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-key-point-to-c-arr x) 'key-point i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-key-point-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-key-point-to-lisp-vec y))
	     ((and (typep x 'std-vector-key-point) y)
	      (if (eq z nil)
		  (mem-aref (vec-key-point-to-c-arr x) 'key-point y)
		  (if (< z 5)  (mem-aref (c-pointer 
					   (mem-aref (vec-key-point-to-c-arr x) 
						     'key-point y)) :float z)
		      (mem-aref (c-pointer 
				 (mem-aref (vec-key-point-to-c-arr x) 'key-point y)) :int z))))
	     (t (error "incorrect input. 
  ~%See VECTOR-KEY-POINT documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorm" make-vector-mat) vector-mat)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_Mat* std_carrayTovectorm(Mat** a, size_t len)
(defcfun ("std_carrayTovectorm1" c-arr-to-vec-mat) vector-mat
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-mat (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-mat (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorm_to_carray" vec-mat-to-c-arr) :pointer 
  (v vector-mat))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorm_length" vec-mat-length) :unsigned-int
  (self vector-mat))


(defun vec-mat-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-mat-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-mat-to-c-arr x) 'mat i) z))
       (reverse z)))


(defun vec-mat-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-mat-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-mat-to-c-arr x) 'mat i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-mat-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-mat-to-lisp-vec y))
	     ((and (typep x 'std-vector-mat) y)
	      (if (eq z nil)
		  (mem-aref (vec-mat-to-c-arr x) 'mat y)
		  (mem-aref (c-pointer (mem-aref (vec-mat-to-c-arr x) 'mat y)) :int z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-MAT documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorp" make-vector-point) vector-point)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorp" c-arr-to-vec-point) vector-point
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-point (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-point (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorp_to_carray" vec-point-to-c-arr) :pointer 
  (v vector-point))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorp_length" vec-point-length) :unsigned-int
  (self vector-point))


(defun vec-point-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-point-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-point-to-c-arr x) 'point i) z))
       (reverse z)))


(defun vec-point-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-point-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-point-to-c-arr x) 'point i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-point-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-point-to-lisp-vec y))
	     ((and (typep x 'std-vector-point) y)
	      (if (eq z nil)
		  (mem-aref (vec-point-to-c-arr x) 'point y)
		  (mem-aref (c-pointer (mem-aref (vec-point-to-c-arr x) 'point y)) :int z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-POINT documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorp2f" make-vector-point-2f) vector-point-2f)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorp2f" c-arr-to-vec-point-2f) vector-point-2f
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-point-2f (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-point-2f (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorp2f_to_carray" vec-point-2f-to-c-arr) :pointer 
  (v vector-point-2f))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorp2f_length" vec-point-2f-length) :unsigned-int
  (self vector-point-2f))


(defun vec-point-2f-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-point-2f-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-point-2f-to-c-arr x) 'point-2f i) z))
       (reverse z)))


(defun vec-point-2f-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-point-2f-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-point-2f-to-c-arr x) 'point-2f i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-point-2f-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-point-2f-to-lisp-vec y))
	     ((and (typep x 'std-vector-point-2f) y)
	      (if (eq z nil)
		  (mem-aref (vec-point-2f-to-c-arr x) 'point-2f y)
		  (mem-aref (c-pointer (mem-aref (vec-point-2f-to-c-arr x) 'point-2f y)) :float z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-POINT-2F documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorr" make-vector-rect) vector-rect)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorr" c-arr-to-vec-rect) vector-rect
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-rect (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-rect (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorr_to_carray" vec-rect-to-c-arr) :pointer 
  (v vector-rect))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorr_length" vec-rect-length) :unsigned-int
  (self vector-rect))


(defun vec-rect-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-rect-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (c-pointer x) 'rect i) z))
       (reverse z)))


(defun vec-rect-to-lisp-vec (vec length)
    (let* ((w vec)
	    (x (vec-rect-length w))
	    (y (if length length x))
	    (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))

       (dotimes (i x)
	 (vector-push (mem-aref (vec-rect-to-c-arr w) 'rect i) z))
       z))



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
	     ((and (eq :to-lisp-list x))
	      (vec-rect-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-rect-to-lisp-vec y z))
	     ((and (typep x 'std-vector-rect) y)
	      (if (eq z nil)
		  (mem-aref (vec-rect-to-c-arr x) 'rect y)
		  (mem-aref (c-pointer (mem-aref (vec-rect-to-c-arr x) 'rect y)) :int z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-RECT documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectoru" make-vector-uchar) vector-uchar)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectoru" c-arr-to-vec-uchar) vector-uchar
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-uchar (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :uchar :initial-contents 
					    (coerce a 'list)))))
    (c-arr-to-vec-uchar (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectoru_to_carray" vec-uchar-to-c-arr) :pointer 
  (v vector-uchar))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectoru_length" vec-uchar-length) :unsigned-int
  (self vector-uchar))


(defun vec-uchar-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-uchar-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-uchar-to-c-arr x) :uchar i) z))
       (reverse z)))


(defun vec-uchar-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-uchar-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-uchar-to-c-arr x) :uchar i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-uchar-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-uchar-to-lisp-vec y))
	     ((typep x 'std-vector-uchar)
	      (if (eq y nil)
		  (mem-aref (vec-uchar-to-c-arr x) :uchar) 
		  (mem-aref (vec-uchar-to-c-arr x) :uchar y)))
	     (t (error "incorrect input. 
                   ~%See VECTOR-UCHAR documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv2d" make-vector-vec-2d) vector-vec-2d)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv2d" c-arr-to-vec-vec-2d) vector-vec-2d
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-2d (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-2d (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorv2d_to_carray" vec-vec-2d-to-c-arr) :pointer 
  (v vector-vec-2d))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorv2d_length" vec-vec-2d-length) :unsigned-int
  (self vector-vec-2d))


(defun vec-vec-2d-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-vec-2d-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-vec-2d-to-c-arr x) 'vec-2d i) z))
       (reverse z)))


(defun vec-vec-2d-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-vec-2d-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-vec-2d-to-c-arr x) 'vec-2d i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-vec-2d-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-vec-2d-to-lisp-vec y))
	     ((and (typep x 'std-vector-vec-2d) y)
	      (if (eq z nil)
		  (mem-aref (vec-vec-2d-to-c-arr x) 'vec-2d y)
		  (mem-aref (c-pointer (mem-aref (vec-vec-2d-to-c-arr x) 'vec-2d y)) :double z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-2D documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv3d" make-vector-vec-3d) vector-vec-3d)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv3d" c-arr-to-vec-vec-3d) vector-vec-3d
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-3d (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-3d (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorv3d_to_carray" vec-vec-3d-to-c-arr) :pointer 
  (v vector-vec-3d))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorv3d_length" vec-vec-3d-length) :unsigned-int
  (self vector-vec-3d))


(defun vec-vec-3d-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-vec-3d-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-vec-3d-to-c-arr x) 'vec-3d i) z))
       (reverse z)))


(defun vec-vec-3d-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-vec-3d-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-vec-3d-to-c-arr x) 'vec-3d i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-vec-3d-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-vec-3d-to-lisp-vec y))
	     ((and (typep x 'std-vector-vec-3d) y)
	      (if (eq z nil)
		  (mem-aref (vec-vec-3d-to-c-arr x) 'vec-3d y)
		  (mem-aref (c-pointer (mem-aref (vec-vec-3d-to-c-arr x) 'vec-3d y)) :double z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-3D documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv4d" make-vector-vec-4d) vector-vec-4d)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv4d" c-arr-to-vec-vec-4d) vector-vec-4d
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-4d (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-4d (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorv4d_to_carray" vec-vec-4d-to-c-arr) :pointer 
  (v vector-vec-4d))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorv4d_length" vec-vec-4d-length) :unsigned-int
  (self vector-vec-4d))


(defun vec-vec-4d-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-vec-4d-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-vec-4d-to-c-arr x) 'vec-4d i) z))
       (reverse z)))


(defun vec-vec-4d-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-vec-4d-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-vec-4d-to-c-arr x) 'vec-4d i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-vec-4d-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-vec-4d-to-lisp-vec y))
	     ((and (typep x 'std-vector-vec-4d) y)
	      (if (eq z nil)
		  (mem-aref (vec-vec-4d-to-c-arr x) 'vec-4d y)
		  (mem-aref (c-pointer (mem-aref (vec-vec-4d-to-c-arr x) 'vec-4d y)) :double z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-4D documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv6d" make-vector-vec-6d) vector-vec-6d)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv6d" c-arr-to-vec-vec-6d) vector-vec-6d
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-6d (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-6d (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorv6d_to_carray" vec-vec-6d-to-c-arr) :pointer 
  (v vector-vec-6d))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorv6d_length" vec-vec-6d-length) :unsigned-int
  (self vector-vec-6d))


(defun vec-vec-6d-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-vec-6d-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-vec-6d-to-c-arr x) 'vec-6d i) z))
       (reverse z)))


(defun vec-vec-6d-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-vec-6d-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-vec-6d-to-c-arr x) 'vec-6d i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-vec-6d-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-vec-6d-to-lisp-vec y))
	     ((and (typep x 'std-vector-vec-6d) y)
	      (if (eq z nil)
		  (mem-aref (vec-vec-6d-to-c-arr x) 'vec-6d y)
		  (mem-aref (c-pointer (mem-aref (vec-vec-6d-to-c-arr x) 'vec-6d y)) :double z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-6D documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv2f" make-vector-vec-2f) vector-vec-2f)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv2f" c-arr-to-vec-vec-2f) vector-vec-2f
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-2f (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-2f (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorv2f_to_carray" vec-vec-2f-to-c-arr) :pointer 
  (v vector-vec-2f))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorv2f_length" vec-vec-2f-length) :unsigned-int
  (self vector-vec-2f))


(defun vec-vec-2f-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-vec-2f-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-vec-2f-to-c-arr x) 'vec-2f i) z))
       (reverse z)))


(defun vec-vec-2f-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-vec-2f-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-vec-2f-to-c-arr x) 'vec-2f i) z))
       z))



(defun vector-vec-2f (&rest args)
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
	     ((and (eq :to-lisp-list x))
	      (vec-vec-2f-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-vec-2f-to-lisp-vec y))
	     ((and (typep x 'std-vector-vec-2f) y)
	      (if (eq z nil)
		  (mem-aref (vec-vec-2f-to-c-arr x) 'vec-2f y)
		  (mem-aref (c-pointer (mem-aref (vec-vec-2f-to-c-arr x) 'vec-2f y)) :float z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-2F documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv3f" make-vector-vec-3f) vector-vec-3f)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv3f" c-arr-to-vec-vec-3f) vector-vec-3f
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-3f (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-3f (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorv3f_to_carray" vec-vec-3f-to-c-arr) :pointer 
  (v vector-vec-3f))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorv3f_length" vec-vec-3f-length) :unsigned-int
  (self vector-vec-3f))


(defun vec-vec-3f-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-vec-3f-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-vec-3f-to-c-arr x) 'vec-3f i) z))
       (reverse z)))


(defun vec-vec-3f-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-vec-3f-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-vec-3f-to-c-arr x) 'vec-3f i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-vec-3f-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-vec-3f-to-lisp-vec y))
	     ((and (typep x 'std-vector-vec-3f) y)
	      (if (eq z nil)
		  (mem-aref (vec-vec-3f-to-c-arr x) 'vec-3f y)
		  (mem-aref (c-pointer (mem-aref (vec-vec-3f-to-c-arr x) 'vec-3f y)) :float z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-3F documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv4f" make-vector-vec-4f) vector-vec-4f)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv4f" c-arr-to-vec-vec-4f) vector-vec-4f
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-4f (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-4f (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorv4f_to_carray" vec-vec-4f-to-c-arr) :pointer 
  (v vector-vec-4f))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorv4f_length" vec-vec-4f-length) :unsigned-int
  (self vector-vec-4f))


(defun vec-vec-4f-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-vec-4f-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-vec-4f-to-c-arr x) 'vec-4f i) z))
       (reverse z)))


(defun vec-vec-4f-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-vec-4f-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-vec-4f-to-c-arr x) 'vec-4f i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-vec-4f-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-vec-4f-to-lisp-vec y))
	     ((and (typep x 'std-vector-vec-4f) y)
	      (if (eq z nil)
		  (mem-aref (vec-vec-4f-to-c-arr x) 'vec-4f y)
		  (mem-aref (c-pointer (mem-aref (vec-vec-4f-to-c-arr x) 'vec-4f y)) :float z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-4F documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv6f" make-vector-vec-6f) vector-vec-6f)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv6f" c-arr-to-vec-vec-6f) vector-vec-6f
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-6f (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-6f (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorv6f_to_carray" vec-vec-6f-to-c-arr) :pointer 
  (v vector-vec-6f))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorv6f_length" vec-vec-6f-length) :unsigned-int
  (self vector-vec-6f))


(defun vec-vec-6f-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-vec-6f-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-vec-6f-to-c-arr x) 'vec-6f i) z))
       (reverse z)))


(defun vec-vec-6f-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-vec-6f-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-vec-6f-to-c-arr x) 'vec-6f i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-vec-6f-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-vec-6f-to-lisp-vec y))
	     ((and (typep x 'std-vector-vec-6f) y)
	      (if (eq z nil)
		  (mem-aref (vec-vec-6f-to-c-arr x) 'vec-6f y)
		  (mem-aref (c-pointer (mem-aref (vec-vec-6f-to-c-arr x) 'vec-6f y)) :float z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-6F documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv2i" make-vector-vec-2i) vector-vec-2i)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv2i" c-arr-to-vec-vec-2i) vector-vec-2i
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-2i (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-2i (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorv2i_to_carray" vec-vec-2i-to-c-arr) :pointer 
  (v vector-vec-2i))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorv2i_length" vec-vec-2i-length) :unsigned-int
  (self vector-vec-2i))


(defun vec-vec-2i-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-vec-2i-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-vec-2i-to-c-arr x) 'vec-2i i) z))
       (reverse z)))


(defun vec-vec-2i-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-vec-2i-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-vec-2i-to-c-arr x) 'vec-2i i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-vec-2i-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-vec-2i-to-lisp-vec y))
	     ((and (typep x 'std-vector-vec-2i) y)
	      (if (eq z nil)
		  (mem-aref (vec-vec-2i-to-c-arr x) 'vec-2i y)
		  (mem-aref (c-pointer (mem-aref (vec-vec-2i-to-c-arr x) 'vec-2i y)) :int z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-2I documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv3i" make-vector-vec-3i) vector-vec-3i)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv3i" c-arr-to-vec-vec-3i) vector-vec-3i
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-3i (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-3i (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorv3i_to_carray" vec-vec-3i-to-c-arr) :pointer 
  (v vector-vec-3i))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorv3i_length" vec-vec-3i-length) :unsigned-int
  (self vector-vec-3i))


(defun vec-vec-3i-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-vec-3i-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-vec-3i-to-c-arr x) 'vec-3i i) z))
       (reverse z)))


(defun vec-vec-3i-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-vec-3i-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-vec-3i-to-c-arr x) 'vec-3i i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-vec-3i-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-vec-3i-to-lisp-vec y))
	     ((and (typep x 'std-vector-vec-3i) y)
	      (if (eq z nil)
		  (mem-aref (vec-vec-3i-to-c-arr x) 'vec-3i y)
		  (mem-aref (c-pointer (mem-aref (vec-vec-3i-to-c-arr x) 'vec-3i y)) :int z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-3I documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv4i" make-vector-vec-4i) vector-vec-4i)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv4i" c-arr-to-vec-vec-4i) vector-vec-4i
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-4i (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-4i (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorv4i_to_carray" vec-vec-4i-to-c-arr) :pointer 
  (v vector-vec-4i))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorv4i_length" vec-vec-4i-length) :unsigned-int
  (self vector-vec-4i))


(defun vec-vec-4i-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-vec-4i-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-vec-4i-to-c-arr x) 'vec-4i i) z))
       (reverse z)))


(defun vec-vec-4i-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-vec-4i-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-vec-4i-to-c-arr x) 'vec-4i i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-vec-4i-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-vec-4i-to-lisp-vec y))
	     ((and (typep x 'std-vector-vec-4i) y)
	      (if (eq z nil)
		  (mem-aref (vec-vec-4i-to-c-arr x) 'vec-4i y)
		  (mem-aref (c-pointer (mem-aref (vec-vec-4i-to-c-arr x) 'vec-4i y)) :int z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-4I documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv6i" make-vector-vec-6i) vector-vec-6i)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv6i" c-arr-to-vec-vec-6i) vector-vec-6i
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-6i (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-6i (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorv6i_to_carray" vec-vec-6i-to-c-arr) :pointer 
  (v vector-vec-6i))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorv6i_length" vec-vec-6i-length) :unsigned-int
  (self vector-vec-6i))


(defun vec-vec-6i-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-vec-6i-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-vec-6i-to-c-arr x) 'vec-6i i) z))
       (reverse z)))


(defun vec-vec-6i-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-vec-6i-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-vec-6i-to-c-arr x) 'vec-6i i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-vec-6i-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-vec-6i-to-lisp-vec y))
	     ((and (typep x 'std-vector-vec-6i) y)
	      (if (eq z nil)
		  (mem-aref (vec-vec-6i-to-c-arr x) 'vec-6i y)
		  (mem-aref (c-pointer (mem-aref (vec-vec-6i-to-c-arr x) 'vec-6i y)) :int z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-6I documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorv8i" make-vector-vec-8i) vector-vec-8i)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorv8i" c-arr-to-vec-vec-8i) vector-vec-8i
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun arr-to-vec-vec-8i (a)
    (unless (equal a (car previous))
      (setf previous (cons a (gced-foreign-alloc :pointer :initial-contents
					    (mapcar #!(c-pointer %1) (coerce a 'list))))))
    (c-arr-to-vec-vec-8i (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorv8i_to_carray" vec-vec-8i-to-c-arr) :pointer 
  (v vector-vec-8i))


;; size_t std_vector##tn##_length( vector_##t * v)
(defcfun ("std_vectorv8i_length" vec-vec-8i-length) :unsigned-int
  (self vector-vec-8i))


(defun vec-vec-8i-to-lisp-list (vec)
    (let* ((x vec)
	    (y (vec-vec-8i-length x))
            (z (list)))
       (dotimes (i y)
	 (push (mem-aref (vec-vec-8i-to-c-arr x) 'vec-8i i) z))
       (reverse z)))


(defun vec-vec-8i-to-lisp-vec (vec)
    (let* ((x vec)
	    (y (vec-vec-8i-length x))
            (z (make-array y :element-type t :fill-pointer 0
			    :initial-element   
			    nil)))
       (dotimes (i y)
	 (vector-push (mem-aref (vec-vec-8i-to-c-arr x) 'vec-8i i) z))
       z))


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
	     ((and (eq :to-lisp-list x))
	      (vec-vec-8i-to-lisp-list y))
	     ((and (eq :to-lisp-vec x))
	      (vec-vec-8i-to-lisp-vec y))
	     ((and (typep x 'std-vector-vec-8i) y)
	      (if (eq z nil)
		  (mem-aref (vec-vec-8i-to-c-arr x) 'vec-8i y)
		  (mem-aref (c-pointer (mem-aref (vec-vec-8i-to-c-arr x) 'vec-8i y)) :int z)))
	     (t (error "incorrect input. 
  ~%See VECTOR-VEC-8I documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))


