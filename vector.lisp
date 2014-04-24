;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; vector.lisp
;;;; OpenCV bindings
;;;; Bindings for the C++ Vector Class

(in-package :lisp-cv)



;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectordm" %vector-dmatch) (:pointer vector-dmatch))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectordm" %c-arr-to-vector-dmatch) (:pointer vector-dmatch)
  (a :pointer)
  (len :unsigned-int))

(let ((previous nil))
  (defun c-arr-to-vector-dmatch (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents a))))
    (%c-arr-to-vector-dmatch (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectordm_to_carray" %vector-dmatch-to-c-array) :pointer 
  (s (:pointer vector-DMatch)))


(defun vector-dmatch (&optional arg i n)
	   (cond ((eq arg nil) (return-from vector-dmatch (%vector-dmatch)))
		 ((listp arg)
		  (return-from vector-dmatch (c-arr-to-vector-dmatch arg)))
		 ((and (pointerp arg) i (not n)) (mem-aref (%vector-dmatch-to-c-array arg) :pointer i))
		 ((and (pointerp arg) i n) (mem-aref (mem-aref (%vector-dmatch-to-c-array arg) :pointer i) :float n))
		 (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorkp" %vector-keypoint) (:pointer vector-keypoint))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorkp" %c-arr-to-vector-keypoint) (:pointer vector-keypoint)
  (a :pointer)
  (len :unsigned-int))

(let ((previous nil))
  (defun c-arr-to-vector-keypoint (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents a))))
    (%c-arr-to-vector-keypoint (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorkp_to_carray" %vector-Keypoint-to-c-array) :pointer 
  (s (:pointer vector-keypoint)))


(defun vector-keypoint (&optional arg i n)
	   (cond ((eq arg nil) (return-from vector-keypoint (%vector-keypoint)))
		 ((listp arg)
		  (return-from vector-keypoint (c-arr-to-vector-keypoint arg)))
		 ((and (pointerp arg) i (not n)) (mem-aref (%vector-keypoint-to-c-array arg) :pointer i))
		 ((and (pointerp arg) i n) (mem-aref (mem-aref (%vector-keypoint-to-c-array arg) :pointer i) :float n))
		 (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectord" %vector-double) (:pointer vector-double))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectord" %c-arr-to-vector-double) (:pointer vector-double)
  (a :pointer)
  (len :unsigned-int))

(let ((previous nil))
  (defun c-arr-to-vector-double (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :double :initial-contents a))))
    (%c-arr-to-vector-double (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectord_to_carray" %vector-double-to-c-array) :pointer 
  (s (:pointer vector-double)))


(defun vector-double (&optional arg (i 0))
  (cond ((eq arg nil) (return-from vector-double (%vector-double)))
	((listp arg)
	 (return-from vector-double (c-arr-to-vector-double arg)))
	((pointerp arg) (mem-aref (%vector-double-to-c-array arg) :double i))
    (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorf" %vector-float) (:pointer vector-float))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorf" %c-arr-to-vector-float) (:pointer vector-float)
  (a :pointer)
  (len :unsigned-int))

(let ((previous nil))
  (defun c-arr-to-vector-float (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :float :initial-contents a))))
    (%c-arr-to-vector-float (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorf_to_carray" %vector-float-to-c-array) :pointer 
  (s (:pointer vector-float)))


(defun vector-float (&optional arg (i 0))
  (cond ((eq arg nil) (return-from vector-float (%vector-float)))
	((listp arg)
	 (return-from vector-float (c-arr-to-vector-float arg)))
	((pointerp arg) (mem-aref (%vector-float-to-c-array arg) :float i))
    (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorc" %vector-char) (:pointer vector-char))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorc" %c-arr-to-vector-char) (:pointer vector-char)
  (a :pointer)
  (len :unsigned-int))

(let ((previous nil))
  (defun c-arr-to-vector-char (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :char :initial-contents a))))
    (%c-arr-to-vector-char (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorc_to_carray" %vector-char-to-c-array) :pointer 
  (s (:pointer vector-char)))


(defun vector-char (&optional arg (i 0))
  (cond ((eq arg nil) (return-from vector-char (%vector-char)))
	((listp arg)
	 (return-from vector-char (c-arr-to-vector-char arg)))
	((pointerp arg) (mem-aref (%vector-char-to-c-array arg) :char i))
    (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectori" %vector-int) (:pointer vector-int))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectori" %c-arr-to-vector-int) (:pointer vector-int)
  (a :pointer)
  (len :unsigned-int))

(let ((previous nil))
  (defun c-arr-to-vector-int (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :int :initial-contents a))))
    (%c-arr-to-vector-int (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectori_to_carray" %vector-int-to-c-array) :pointer 
  (s (:pointer vector-int)))


(defun vector-int (&optional arg (i 0))
  (cond ((eq arg nil) (return-from vector-int (%vector-int)))
	((listp arg)
	 (return-from vector-int (c-arr-to-vector-int arg)))
	((pointerp arg) (mem-aref (%vector-int-to-c-array arg) :int i))
    (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorp" %vector-point) (:pointer vector-point))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorp" %c-arr-to-vector-point) (:pointer vector-point)
  (a :pointer)
  (len :unsigned-int))

(let ((previous nil))
  (defun c-arr-to-vector-point (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents a))))
    (%c-arr-to-vector-point (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorp_to_carray" %vector-point-to-c-array) :pointer 
  (s (:pointer vector-point)))


(defun vector-point (&optional arg i n)
	   (cond ((eq arg nil) (return-from vector-point (%vector-point)))
		 ((listp arg)
		  (return-from vector-point (c-arr-to-vector-point arg)))
		 ((and (pointerp arg) i (not n)) (mem-aref (%vector-point-to-c-array arg) :pointer i))
		 ((and (pointerp arg) i n) (mem-aref (mem-aref (%vector-point-to-c-array arg) :pointer i) :int n))
		 (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorp2f" %vector-point2f) (:pointer vector-point2f))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorp2f" %c-arr-to-vector-point2f) (:pointer vector-point2f)
  (a :pointer)
  (len :unsigned-int))

(let ((previous nil))
  (defun c-arr-to-vector-point2f (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents a))))
    (%c-arr-to-vector-point2f (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorp2f_to_carray" %vector-point2f-to-c-array) :pointer 
  (s (:pointer vector-point2f)))


(defun vector-point2f (&optional arg i n)
	   (cond ((eq arg nil) (return-from vector-point2f (%vector-point2f)))
		 ((listp arg)
		  (return-from vector-point2f (c-arr-to-vector-point2f arg)))
		 ((and (pointerp arg) i (not n)) (mem-aref (%vector-point2f-to-c-array arg) :pointer i))
		 ((and (pointerp arg) i n) (mem-aref (mem-aref (%vector-point2f-to-c-array arg) :pointer i) :float n))
		 (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorm" %vector-mat) (:pointer vector-mat))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorm" %c-arr-to-vector-mat) (:pointer vector-mat)
  (a :pointer)
  (len :unsigned-int))

(let ((previous nil))
  (defun c-arr-to-vector-mat (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents a))))
    (%c-arr-to-vector-mat (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorm_to_carray" %vector-mat-to-c-array) :pointer 
  (s (:pointer vector-mat)))


(defun vector-mat (&optional arg i n)
	   (cond ((eq arg nil) (return-from vector-mat (%vector-mat)))
		 ((listp arg)
		  (return-from vector-mat (c-arr-to-vector-mat arg)))
		 ((and (pointerp arg) i (not n)) (mem-aref (%vector-mat-to-c-array arg) :pointer i))
		 ((and (pointerp arg) i n) (mem-aref (mem-aref (%vector-mat-to-c-array arg) :pointer i) :int n))
		 (t nil)))

;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectoru" %vector-uchar) (:pointer vector-uchar))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectoru" %c-arr-to-vector-uchar) (:pointer vector-uchar)
  (a :pointer)
  (len :unsigned-int))

(let ((previous nil))
  (defun c-arr-to-vector-uchar (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :uchar :initial-contents a))))
    (%c-arr-to-vector-uchar (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectoru_to_carray" %vector-uchar-to-c-array) :pointer 
  (s (:pointer vector-uchar)))


(defun vector-uchar (&optional arg (i 0))
  (cond ((eq arg nil) (return-from vector-uchar (%vector-uchar)))
	((listp arg)
	 (return-from vector-uchar (c-arr-to-vector-uchar arg)))
	((pointerp arg) (mem-aref (%vector-uchar-to-c-array arg) :uchar i))
    (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * create_std_vector##tn()
(defcfun ("create_std_vectorr" %vector-rect) (:pointer vector-rect))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_##t * carray_to_std_vector##tn( t * a, size_t len )
(defcfun ("carray_to_std_vectorr" %c-arr-to-vector-rect) (:pointer vector-rect)
  (a :pointer)
  (len :unsigned-int))

(let ((previous nil))
  (defun c-arr-to-vector-rect (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents a))))
    (%c-arr-to-vector-rect (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; t * std_vector##tn##_to_carray( vector_##t * v ) 
(defcfun ("std_vectorr_to_carray" %vector-rect-to-c-array) (:pointer rect) 
  (s (:pointer vector-rect)))


(defun vector-rect (&optional arg i n)
	   (cond ((eq arg nil) (return-from vector-rect (%vector-rect)))
		 ((listp arg)
		  (return-from vector-rect (c-arr-to-vector-rect arg)))
		 ((and (pointerp arg) i (not n)) (mem-aref (%vector-rect-to-c-array arg) :pointer i))
		 ((and (pointerp arg) i n) (mem-aref (mem-aref (%vector-rect-to-c-array arg) :pointer i) :int n))
		 (t nil)))

