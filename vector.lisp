;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; vector.lisp
;;;; OpenCV bindings
;;;; Bindings for the C++ Vector Class

(in-package :lisp-cv)


;;; AT functions(for internal use)


(defun at-vector-char (self idx)
  (if (typep self 'std-vector-char)
      (mem-aref (foreign-funcall "cv_vector_c_at" 
				 :pointer (c-pointer self) 
				 :int idx 
				 :pointer) :char)))


(defun (setf at-vector-char) (val self idx)
  (if (typep self 'std-vector-char)
      (let ((self (foreign-funcall "cv_vector_c_at" 
				   :pointer (c-pointer self) 
				   :int idx 
				   :pointer)))
	(setf (mem-aref self :char) val))))


(defun at-vector-dmatch (self idx)
  (if (typep self 'std-vector-dmatch)
      (foreign-funcall "cv_vector_dm_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       dmatch)))


(defun (setf at-vector-dmatch) (val self idx)
  (if (typep self 'std-vector-dmatch)
      (foreign-funcall "cv_vector_DMatch_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       dmatch)))


(defun at-vector-double (self idx)
  (if (typep self 'std-vector-double)
      (mem-aref (foreign-funcall "cv_vector_d_at" 
				 :pointer (c-pointer self) 
				 :int idx 
				 :pointer) :double)))


(defun (setf at-vector-double) (val self idx)
  (if (typep self 'std-vector-double)
      (let ((self (foreign-funcall "cv_vector_d_at" 
				   :pointer (c-pointer self) 
				   :int idx 
				   :pointer)))
	(setf (mem-aref self :double) val))))


(defun at-vector-float (self idx)
  (if (typep self 'std-vector-float)
      (mem-aref (foreign-funcall "cv_vector_f_at" 
				 :pointer (c-pointer self) 
				 :int idx 
				 :pointer) :float)))


(defun (setf at-vector-float) (val self idx)
  (if (typep self 'std-vector-float)
      (let ((self (foreign-funcall "cv_vector_f_at" 
				   :pointer (c-pointer self) 
				   :int idx 
				   :pointer)))
	(setf (mem-aref self :float) val))))


(defun at-vector-int (self idx)
  (if (typep self 'std-vector-int)
      (mem-aref (foreign-funcall "cv_vector_i_at" 
				 :pointer (c-pointer self) 
				 :int idx 
				 :pointer) :int)))


(defun (setf at-vector-int) (val self idx)
  (if (typep self 'std-vector-int)
      (let ((self (foreign-funcall "cv_vector_i_at" 
				   :pointer (c-pointer self) 
				   :int idx 
				   :pointer)))
	(setf (mem-aref self :int) val))))


(defun at-vector-key-point (self idx)
  (if (typep self 'std-vector-key-point)
      (foreign-funcall "cv_vector_kp_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       key-point)))


(defun (setf at-vector-key-point) (val self idx)
  (if (typep self 'std-vector-key-point)
      (foreign-funcall "cv_vector_kp_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       key-point)))


(defun at-vector-mat (self idx)
  (if (typep self 'std-vector-mat)
      (foreign-funcall "cv_vector_m_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       mat)))


(defun (setf at-vector-mat) (val self idx)
  (if (typep self 'std-vector-mat)
      (foreign-funcall "cv_vector_m_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       mat)))


(defun at-vector-point (self idx)
  (if (typep self 'std-vector-point)
      (foreign-funcall "cv_vector_p_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       point)))


(defun (setf at-vector-point) (val self idx)
  (if (typep self 'std-vector-point)
      (foreign-funcall "cv_vector_p_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       point)))


(defun at-vector-point-2f (self idx)
  (if (typep self 'std-vector-point-2f)
      (foreign-funcall "cv_vector_p2f_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       point-2f)))


(defun (setf at-vector-point-2f) (val self idx)
  (if (typep self 'std-vector-point-2f)
      (foreign-funcall "cv_vector_p2f_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       point-2f)))


(defun at-vector-rect (self idx)
  (if (typep self 'std-vector-rect)
      (foreign-funcall "cv_vector_r_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       rect)))


(defun (setf at-vector-rect) (val self idx)
  (if (typep self 'std-vector-rect)
      (foreign-funcall "cv_vector_r_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       rect)))


(defun at-vector-vec-2d (self idx)
  (if (typep self 'std-vector-vec-2d)
      (foreign-funcall "cv_vector_v2d_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       vec-2d)))


(defun (setf at-vector-vec-2d) (val self idx)
  (if (typep self 'std-vector-vec-2d)
      (foreign-funcall "cv_vector_v2d_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       vec-2d)))


(defun at-vector-vec-3d (self idx)
  (if (typep self 'std-vector-vec-3d)
      (foreign-funcall "cv_vector_v3d_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       vec-3d)))


(defun (setf at-vector-vec-3d) (val self idx)
  (if (typep self 'std-vector-vec-3d)
      (foreign-funcall "cv_vector_v3d_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       vec-3d)))


(defun at-vector-vec-4d (self idx)
  (if (typep self 'std-vector-vec-4d)
      (foreign-funcall "cv_vector_v4d_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       vec-4d)))


(defun (setf at-vector-vec-4d) (val self idx)
  (if (typep self 'std-vector-vec-4d)
      (foreign-funcall "cv_vector_v4d_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       vec-4d)))


(defun at-vector-vec-6d (self idx)
  (if (typep self 'std-vector-vec-6d)
      (foreign-funcall "cv_vector_v6d_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       vec-6d)))


(defun (setf at-vector-vec-6d) (val self idx)
  (if (typep self 'std-vector-vec-6d)
      (foreign-funcall "cv_vector_v6d_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       vec-6d)))


(defun at-vector-vec-2f (self idx)
  (if (typep self 'std-vector-vec-2f)
      (foreign-funcall "cv_vector_v2f_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       vec-2f)))


(defun (setf at-vector-vec-2f) (val self idx)
  (if (typep self 'std-vector-vec-2f)
      (foreign-funcall "cv_vector_v2f_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       vec-2f)))


(defun at-vector-vec-3f (self idx)
  (if (typep self 'std-vector-vec-3f)
      (foreign-funcall "cv_vector_v3f_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       vec-3f)))


(defun (setf at-vector-vec-3f) (val self idx)
  (if (typep self 'std-vector-vec-3f)
      (foreign-funcall "cv_vector_v3f_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       vec-3f)))


(defun at-vector-vec-4f (self idx)
  (if (typep self 'std-vector-vec-4f)
      (foreign-funcall "cv_vector_v4f_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       vec-4f)))


(defun (setf at-vector-vec-4f) (val self idx)
  (if (typep self 'std-vector-vec-4f)
      (foreign-funcall "cv_vector_v4f_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       vec-6f)))


(defun at-vector-vec-6f (self idx)
  (if (typep self 'std-vector-vec-6f)
      (foreign-funcall "cv_vector_v6f_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       vec-6f)))


(defun (setf at-vector-vec-6f) (val self idx)
  (if (typep self 'std-vector-vec-6f)
      (foreign-funcall "cv_vector_v6f_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       vec-6f)))


(defun at-vector-vec-2i (self idx)
  (if (typep self 'std-vector-vec-2i)
      (foreign-funcall "cv_vector_v2i_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       vec-2i)))


(defun (setf at-vector-vec-2i) (val self idx)
  (if (typep self 'std-vector-vec-2i)
      (foreign-funcall "cv_vector_v2i_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       vec-2i)))


(defun at-vector-vec-3i (self idx)
  (if (typep self 'std-vector-vec-3i)
      (foreign-funcall "cv_vector_v3i_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       vec-3i)))


(defun (setf at-vector-vec-3i) (val self idx)
  (if (typep self 'std-vector-vec-3i)
      (foreign-funcall "cv_vector_v3i_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       vec-3i)))


(defun at-vector-vec-4i (self idx)
  (if (typep self 'std-vector-vec-4i)
      (foreign-funcall "cv_vector_v4i_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       vec-4i)))


(defun (setf at-vector-vec-4i) (val self idx)
  (if (typep self 'std-vector-vec-4i)
      (foreign-funcall "cv_vector_v4i_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       vec-4i)))


(defun at-vector-vec-6i (self idx)
  (if (typep self 'std-vector-vec-6i)
      (foreign-funcall "cv_vector_v6i_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       vec-6i)))


(defun (setf at-vector-vec-6i) (val self idx)
  (if (typep self 'std-vector-vec-6i)
      (foreign-funcall "cv_vector_v6i_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       vec-6i)))


(defun at-vector-vec-8i (self idx)
  (if (typep self 'std-vector-vec-8i)
      (foreign-funcall "cv_vector_v8i_at" 
		       :pointer (c-pointer self) 
		       :int idx 
		       vec-8i)))


(defun (setf at-vector-vec-8i) (val self idx)
  (if (typep self 'std-vector-vec-8i)
      (foreign-funcall "cv_vector_v8i_at_set_Val" 
		       :pointer (c-pointer self) 
		       :int idx
		       :pointer (c-pointer val)
		       vec-8i)))


(defun at-vector-uchar (self idx)
  (if (typep self 'std-vector-uchar)
      (mem-aref (foreign-funcall "cv_vector_u_at" 
				 :pointer (c-pointer self) 
				 :int idx 
				 :pointer) :uchar)))


(defun (setf at-vector-uchar) (val self idx)
  (if (typep self 'std-vector-uchar)
      (let ((self (foreign-funcall "cv_vector_u_at" 
				   :pointer (c-pointer self) 
				   :int idx 
				   :pointer)))
	(setf (mem-aref self :uchar) val))))


;;; PUSH-BACK functions.


(defun vec-char-push-back (self val)
  (if (typep self 'std-vector-char)
      (foreign-funcall "cv_vector_c_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-dmatch-push-back (self val)
  (if (typep self 'std-vector-dmatch)
      (foreign-funcall "cv_vector_dm_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-double-push-back (self val)
  (if (typep self 'std-vector-double)
      (foreign-funcall "cv_vector_d_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-float-push-back (self val)
  (if (typep self 'std-vector-float)
      (foreign-funcall "cv_vector_f_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-int-push-back (self val)
  (if (typep self 'std-vector-int)
      (foreign-funcall "cv_vector_i_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-key-point-push-back (self val)
  (if (typep self 'std-vector-key-point)
      (foreign-funcall "cv_vector_kp_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-mat-push-back (self val)
  (if (typep self 'std-vector-mat)
      (foreign-funcall "cv_vector_m_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-point-push-back (self val)
  (if (typep self 'std-vector-point)
      (foreign-funcall "cv_vector_p_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-point-2f-push-back (self val)
  (if (typep self 'std-vector-point-2f)
      (foreign-funcall "cv_vector_p2f_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-rect-push-back (self val)
  (if (typep self 'std-vector-rect)
      (foreign-funcall "cv_vector_r_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-vec-2d-push-back (self val)
  (if (typep self 'std-vector-vec-2d)
      (foreign-funcall "cv_vector_v2d_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-vec-3d-push-back (self val)
  (if (typep self 'std-vector-vec-3d)
      (foreign-funcall "cv_vector_v3d_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-vec-4d-push-back (self val)
  (if (typep self 'std-vector-vec-4d)
      (foreign-funcall "cv_vector_v4d_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-vec-6d-push-back (self val)
  (if (typep self 'std-vector-vec-6d)
      (foreign-funcall "cv_vector_v6d_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-vec-2f-push-back (self val)
  (if (typep self 'std-vector-vec-2f)
      (foreign-funcall "cv_vector_v2f_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-vec-3f-push-back (self val)
  (if (typep self 'std-vector-vec-3f)
      (foreign-funcall "cv_vector_v3f_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-vec-4f-push-back (self val)
  (if (typep self 'std-vector-vec-4f)
      (foreign-funcall "cv_vector_v4f_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-vec-6f-push-back (self val)
  (if (typep self 'std-vector-vec-6f)
      (foreign-funcall "cv_vector_v6f_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-vec-2i-push-back (self val)
  (if (typep self 'std-vector-vec-2i)
      (foreign-funcall "cv_vector_v2i_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-vec-3i-push-back (self val)
  (if (typep self 'std-vector-vec-3i)
      (foreign-funcall "cv_vector_v3i_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-vec-4i-push-back (self val)
  (if (typep self 'std-vector-vec-4i)
      (foreign-funcall "cv_vector_v4i_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-vec-6i-push-back (self val)
  (if (typep self 'std-vector-vec-6i)
      (foreign-funcall "cv_vector_v6i_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-vec-8i-push-back (self val)
  (if (typep self 'std-vector-vec-8i)
      (foreign-funcall "cv_vector_v8i_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


(defun vec-uchar-push-back (self val)
  (if (typep self 'std-vector-uchar)
      (foreign-funcall "cv_vector_u_push_back" 
		       :pointer (c-pointer self) 
		       :pointer (c-pointer val))))


;; template < class T, class Alloc = allocator<T> > class vector.
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorc" make-vector-char) vector-char)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorc" c-arr-to-vec-char) vector-char
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun seq-to-vec-char (a)
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
	   (seq-to-vec-char x))
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


(defun seq-to-vec-dmatch (l)
  (coerce l 'list)
  (let ((vec (make-vector-dmatch)))
    (dotimes (n (cl:length l))
      (vec-dmatch-push-back vec (nth n l)))vec))


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
	(y (third args)))
    (cond ((eq w nil)
	   (make-vector-dmatch))
	  ((and (or (vectorp w) (listp w)) (null x))
	   (seq-to-vec-dmatch w))
	  ((and (eq :to-lisp-list w))
	   (vec-dmatch-to-lisp-list x))
	  ((and (eq :to-lisp-vec w))
	   (vec-dmatch-to-lisp-vec x))
	  ((and (typep w 'std-vector-dmatch) x)
	   (let ((temp (foreign-funcall "cv_vector_dm_at" 
					:pointer (c-pointer w) 
					:int x 
					:pointer)))
	     (if y
		 (case y 
		   (0 (%dmatch-query-idx temp))
		   (1 (%dmatch-train-idx temp))
		   (2 (%dmatch-img-idx temp))
		   (3 (%dmatch-distance temp)))
	       (make-instance 'cv-dmatch :c-pointer temp))))
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
  (defun seq-to-vec-double (a)
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
	   (seq-to-vec-double x))
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
  (defun seq-to-vec-float (a)
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
	   (seq-to-vec-float x))
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
  (defun seq-to-vec-int (a)
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
	   (seq-to-vec-int x))
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


(defun seq-to-vec-key-point (l)
  (coerce l 'list)
  (let ((vec (make-vector-key-point)))
    (dotimes (n (cl:length l))
      (vec-key-point-push-back vec (nth n l)))vec))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorkp_to_carray" vec-key-point-to-c-arr) :pointer
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
  (if (fifth args)
      (error "odd number of args to VECTOR-KEY-POINT")
    nil)
  (let ((w (first args))
	(x (second args))
	(y (third args)))
    (cond ((eq w nil)
	   (make-vector-key-point))
	  ((and (or (vectorp w) (listp w)) (null x))
	   (seq-to-vec-key-point w))
	  ((and (eq :to-lisp-list w))
	   (vec-key-point-to-lisp-list x))
	  ((and (eq :to-lisp-vec w))
	   (vec-key-point-to-lisp-vec x))
	  ((and (typep w 'std-vector-key-point) x)
	   (let ((temp (foreign-funcall "cv_vector_kp_at" 
					:pointer (c-pointer w) 
					:int x 
					:pointer)))
	     (if y
		 (case y 
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
	  (t (error "incorrect input. 
  ~%See VECTOR-KEY-POINT documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorm" make-vector-mat) vector-mat)


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_Mat* std_carrayTovectorm(Mat** a, size_t len)
(defcfun ("carray_to_std_vectorm" c-arr-to-vec-mat) vector-mat
  (a :pointer)
  (len :unsigned-int))


(defun seq-to-vec-mat (l)
  (coerce l 'list)
  (let ((vec (make-vector-mat)))
    (dotimes (n (cl:length l))
      (vec-mat-push-back vec (nth n l)))vec))


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
	(y (second args)))
    (cond ((null x)
	   (make-vector-mat))
	  ((and (or (vectorp x) (listp x)) (null y))
	   (seq-to-vec-mat x))
	  ((and (eq :to-lisp-list x))
	   (vec-mat-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-mat-to-lisp-vec y))
	  ((and (typep x 'std-vector-mat) y)
	   (let ((temp (foreign-funcall "cv_vector_m_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (make-instance 'cv-mat :c-pointer temp)))
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


(defun seq-to-vec-point (l)
  (coerce l 'list)
  (let ((vec (make-vector-point)))
    (dotimes (n (cl:length l))
      (vec-point-push-back vec (nth n l)))vec))


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
	   (seq-to-vec-point x))
	  ((and (eq :to-lisp-list x))
	   (vec-point-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-point-to-lisp-vec y))
	  ((and (typep x 'std-vector-point) y)
	   (let ((temp (foreign-funcall "cv_vector_p_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (if z
		 (case z
		   (0 (mem-aref temp :int))
		   (1 (mem-aref temp :int 1)))
	       (make-instance 'cv-point :c-pointer temp))))
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


(defun seq-to-vec-point-2f (l)
  (coerce l 'list)
  (let ((vec (make-vector-point-2f)))
    (dotimes (n (cl:length l))
      (vec-point-2f-push-back vec (nth n l)))vec))


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
	   (seq-to-vec-point-2f x))
	  ((and (eq :to-lisp-list x))
	   (vec-point-2f-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-point-2f-to-lisp-vec y))
	  ((and (typep x 'std-vector-point-2f) y)
	   (let ((temp (foreign-funcall "cv_vector_p2f_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (if z
		 (case z
		   (0 (mem-aref temp :float))
		   (1 (mem-aref temp :float 1)))
	       (make-instance 'cv-point-2f :c-pointer temp))))
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


(defun seq-to-vec-rect (l)
  (coerce l 'list)
  (let ((vec (make-vector-rect)))
    (dotimes (n (cl:length l))
      (vec-rect-push-back vec (nth n l)))vec))


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
	   (seq-to-vec-rect x))
	  ((and (eq :to-lisp-list x))
	   (vec-rect-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-rect-to-lisp-vec y z))
	  ((and (typep x 'std-vector-rect) y)
	   (let ((temp (foreign-funcall "cv_vector_r_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (if z
		 (case z
		   (0 (mem-aref temp :int))
		   (1 (mem-aref temp :int 1))
		   (2 (mem-aref temp :int 2))
		   (3 (mem-aref temp :int 3)))
	       (make-instance 'cv-rect :c-pointer temp))))
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
  (defun seq-to-vec-uchar (a)
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
	   (seq-to-vec-uchar x))
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


(defun seq-to-vec-vec-2d (l)
  (coerce l 'list)
  (let ((vec (make-vector-vec-2d)))
    (dotimes (n (cl:length l))
      (vec-vec-2d-push-back vec (nth n l)))vec))


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
	   (seq-to-vec-vec-2d x))
	  ((and (eq :to-lisp-list x))
	   (vec-vec-2d-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-vec-2d-to-lisp-vec y))
	  ((and (typep x 'std-vector-vec-2d) y)
	   (let ((temp (foreign-funcall "cv_vector_v2d_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (if z (case z
		     (0 (mem-aref temp :double))
		     (1 (mem-aref temp :double 1)))
	       (make-instance 'cv-vec-2d :c-pointer temp))))
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


(defun seq-to-vec-vec-3d (l)
  (coerce l 'list)
  (let ((vec (make-vector-vec-3d)))
    (dotimes (n (cl:length l))
      (vec-vec-3d-push-back vec (nth n l)))vec))


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
	   (seq-to-vec-vec-3d x))
	  ((and (eq :to-lisp-list x))
	   (vec-vec-3d-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-vec-3d-to-lisp-vec y))
	  ((and (typep x 'std-vector-vec-3d) y)
	   (let ((temp (foreign-funcall "cv_vector_v3d_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (if z (case z
		     (0 (mem-aref temp :double))
		     (1 (mem-aref temp :double 1)))
	       (make-instance 'cv-vec-3d :c-pointer temp))))
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


(defun seq-to-vec-vec-4d (l)
  (coerce l 'list)
  (let ((vec (make-vector-vec-4d)))
    (dotimes (n (cl:length l))
      (vec-vec-4d-push-back vec (nth n l)))vec))


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
	   (seq-to-vec-vec-4d x))
	  ((and (eq :to-lisp-list x))
	   (vec-vec-4d-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-vec-4d-to-lisp-vec y))
	  ((and (typep x 'std-vector-vec-4d) y)
	   (let ((temp (foreign-funcall "cv_vector_v4d_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (if z (case z
		     (0 (mem-aref temp :double))
		     (1 (mem-aref temp :double 1)))
	       (make-instance 'cv-vec-4d :c-pointer temp))))
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


(defun seq-to-vec-vec-6d (l)
  (coerce l 'list)
  (let ((vec (make-vector-vec-6d)))
    (dotimes (n (cl:length l))
      (vec-vec-6d-push-back vec (nth n l)))vec))


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
	   (seq-to-vec-vec-6d x))
	  ((and (eq :to-lisp-list x))
	   (vec-vec-6d-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-vec-6d-to-lisp-vec y))
	  ((and (typep x 'std-vector-vec-6d) y)
	   (let ((temp (foreign-funcall "cv_vector_v6d_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (if z (case z
		     (0 (mem-aref temp :double))
		     (1 (mem-aref temp :double 1)))
	       (make-instance 'cv-vec-6d :c-pointer temp))))
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


(defun seq-to-vec-vec-2f (l)
  (coerce l 'list)
  (let ((vec (make-vector-vec-2f)))
    (dotimes (n (cl:length l))
      (vec-vec-2f-push-back vec (nth n l)))vec))


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
	   (seq-to-vec-vec-2f x))
	  ((and (eq :to-lisp-list x))
	   (vec-vec-2f-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-vec-2f-to-lisp-vec y))
	  ((and (typep x 'std-vector-vec-2f) y)
	   (let ((temp (foreign-funcall "cv_vector_v2f_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (if z (case z
		     (0 (mem-aref temp :float))
		     (1 (mem-aref temp :float 1)))
	       (make-instance 'cv-vec-2f :c-pointer temp))))
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


(defun seq-to-vec-vec-3f (l)
  (coerce l 'list)
  (let ((vec (make-vector-vec-3f)))
    (dotimes (n (cl:length l))
      (vec-vec-3f-push-back vec (nth n l)))vec))


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
	   (seq-to-vec-vec-3f x))
	  ((and (eq :to-lisp-list x))
	   (vec-vec-3f-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-vec-3f-to-lisp-vec y))
	  ((and (typep x 'std-vector-vec-3f) y)
	   (let ((temp (foreign-funcall "cv_vector_v3f_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (if z (case z
		     (0 (mem-aref temp :float))
		     (1 (mem-aref temp :float 1)))
	       (make-instance 'cv-vec-3f :c-pointer temp))))
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


(defun seq-to-vec-vec-4f (l)
  (coerce l 'list)
  (let ((vec (make-vector-vec-4f)))
    (dotimes (n (cl:length l))
      (vec-vec-4f-push-back vec (nth n l)))vec))


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
	   (seq-to-vec-vec-4f x))
	  ((and (eq :to-lisp-list x))
	   (vec-vec-4f-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-vec-4f-to-lisp-vec y))
	  ((and (typep x 'std-vector-vec-4f) y)
	   (let ((temp (foreign-funcall "cv_vector_v4f_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (if z (case z
		     (0 (mem-aref temp :float))
		     (1 (mem-aref temp :float 1)))
	       (make-instance 'cv-vec-4f :c-pointer temp))))
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


(defun seq-to-vec-vec-6f (l)
  (coerce l 'list)
  (let ((vec (make-vector-vec-6f)))
    (dotimes (n (cl:length l))
      (vec-vec-6f-push-back vec (nth n l)))vec))


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
	   (seq-to-vec-vec-6f x))
	  ((and (eq :to-lisp-list x))
	   (vec-vec-6f-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-vec-6f-to-lisp-vec y))
	  ((and (typep x 'std-vector-vec-6f) y)
	   (let ((temp (foreign-funcall "cv_vector_v6f_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (if z (case z
		     (0 (mem-aref temp :float))
		     (1 (mem-aref temp :float 1)))
	       (make-instance 'cv-vec-6f :c-pointer temp))))
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


(defun seq-to-vec-vec-2i (l)
  (coerce l 'list)
  (let ((vec (make-vector-vec-2i)))
    (dotimes (n (cl:length l))
      (vec-vec-2i-push-back vec (nth n l)))vec))


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
	   (seq-to-vec-vec-2i x))
	  ((and (eq :to-lisp-list x))
	   (vec-vec-2i-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-vec-2i-to-lisp-vec y))
	  ((and (typep x 'std-vector-vec-2i) y)
	   (let ((temp (foreign-funcall "cv_vector_v2i_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (if z (case z
		     (0 (mem-aref temp :int))
		     (1 (mem-aref temp :int 1)))
	       (make-instance 'cv-vec-2i :c-pointer temp))))
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


(defun seq-to-vec-vec-3i (l)
  (coerce l 'list)
  (let ((vec (make-vector-vec-3i)))
    (dotimes (n (cl:length l))
      (vec-vec-3i-push-back vec (nth n l)))vec))


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
	   (seq-to-vec-vec-3i x))
	  ((and (eq :to-lisp-list x))
	   (vec-vec-3i-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-vec-3i-to-lisp-vec y))
	  ((and (typep x 'std-vector-vec-3i) y)
	   (let ((temp (foreign-funcall "cv_vector_v3i_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (if z (case z
		     (0 (mem-aref temp :int))
		     (1 (mem-aref temp :int 1)))
	       (make-instance 'cv-vec-3i :c-pointer temp))))
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


(defun seq-to-vec-vec-4i (l)
  (coerce l 'list)
  (let ((vec (make-vector-vec-4i)))
    (dotimes (n (cl:length l))
      (vec-vec-4i-push-back vec (nth n l)))vec))


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
	   (seq-to-vec-vec-4i x))
	  ((and (eq :to-lisp-list x))
	   (vec-vec-4i-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-vec-4i-to-lisp-vec y))
	  ((and (typep x 'std-vector-vec-4i) y)
	   (let ((temp (foreign-funcall "cv_vector_v4i_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (if z (case z
		     (0 (mem-aref temp :int))
		     (1 (mem-aref temp :int 1)))
	       (make-instance 'cv-vec-4i :c-pointer temp))))
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


(defun seq-to-vec-vec-6i (l)
  (coerce l 'list)
  (let ((vec (make-vector-vec-6i)))
    (dotimes (n (cl:length l))
      (vec-vec-6i-push-back vec (nth n l)))vec))


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
	   (seq-to-vec-vec-6i x))
	  ((and (eq :to-lisp-list x))
	   (vec-vec-6i-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-vec-6i-to-lisp-vec y))
	  ((and (typep x 'std-vector-vec-6i) y)
	   (let ((temp (foreign-funcall "cv_vector_v6i_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (if z (case z
		     (0 (mem-aref temp :int))
		     (1 (mem-aref temp :int 1)))
	       (make-instance 'cv-vec-6i :c-pointer temp))))
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


(defun seq-to-vec-vec-8i (l)
  (coerce l 'list)
  (let ((vec (make-vector-vec-8i)))
    (dotimes (n (cl:length l))
      (vec-vec-8i-push-back vec (nth n l)))vec))


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
	   (seq-to-vec-vec-8i x))
	  ((and (eq :to-lisp-list x))
	   (vec-vec-8i-to-lisp-list y))
	  ((and (eq :to-lisp-vec x))
	   (vec-vec-8i-to-lisp-vec y))
	  ((and (typep x 'std-vector-vec-8i) y)
	   (let ((temp (foreign-funcall "cv_vector_v8i_at" 
					:pointer (c-pointer x) 
					:int y 
					:pointer)))
	     (if z (case z
		     (0 (mem-aref temp :int))
		     (1 (mem-aref temp :int 1)))
	       (make-instance 'cv-vec-8i :c-pointer temp))))
	  (t (error "incorrect input. 
  ~%See VECTOR-VEC-8I documentation in <LISP-CV-SOURCE-DIR>EXAMPLES/EXAMPLES.LISP~%")))))



