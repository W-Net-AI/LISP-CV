;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; vector.lisp
;;;; OpenCV bindings
;;;; Bindings for the C++ Vector Class

(in-package :lisp-cv)





;; template < class T, class Alloc = allocator<T> > class vector
;; vector_DMatch* std_create_vectordm() 
(defcfun ("std_create_vectordm" %vector-dmatch) (:pointer vector-dmatch))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_DMatch* std_carrayTovectordm(DMatch* a, size_t len)
(defcfun ("std_carrayTovectordm" %c-arr-to-vector-dmatch) (:pointer vector-dmatch)
  (a :pointer)
  (len :unsigned-int))


;; template < class T, class Alloc = allocator<T> > class vector
;; DMatch* std_vectordmToCArray(vector_DMatch* s) 
(defcfun ("std_vectordmToCArray" %vector-dmatch-to-c-array) :pointer 
  (s (:pointer vector-DMatch)))


(let ((previous nil))
  (defun c-arr-to-vector-dmatch (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents a))))
    (%c-arr-to-vector-dmatch (cdr previous) (length a))))


(defun vector-dmatch (&optional arg (n nil) (i nil))
	   (cond ((eq arg nil) (return-from vector-dmatch (%vector-dmatch)))
		 ((listp arg)
		  (return-from vector-dmatch (c-arr-to-vector-dmatch arg)))
		 ((and (pointerp arg) n (not i)) (mem-aref (%vector-dmatch-to-c-array arg) :pointer n))
		 ((and (pointerp arg) n i) (mem-aref (mem-aref (%vector-dmatch-to-c-array arg) :pointer n) :float i))
		 (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_KeyPoint* std_create_vectorkp() 
(defcfun ("std_create_vectorkp" %vector-keypoint) (:pointer vector-keypoint))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_KeyPoint* std_carrayTovectorkp(float* a, size_t len)
(defcfun ("std_carrayTovectorkp" %c-arr-to-vector-keypoint) (:pointer vector-keypoint)
  (a :pointer)
  (len :unsigned-int))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_double* std_create_vectord() 
(defcfun ("std_create_vectord" %vector-double) (:pointer vector-double))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_double* std_carrayTovectord(double* a, size_t len)
(defcfun ("std_carrayTovectord" %c-arr-to-vector-double) (:pointer vector-double)
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun c-arr-to-vector-double (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :double :initial-contents a))))
    (%c-arr-to-vector-double (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; double* std_vectordToCArray(vector_double* s) 
(defcfun ("std_vectordToCArray" %vector-double-to-c-array) :pointer 
  (s (:pointer vector-double)))


(defun vector-double (&optional arg (i 0))
  (cond ((eq arg nil) (return-from vector-double (%vector-double)))
	((listp arg)
	 (return-from vector-double (c-arr-to-vector-double arg)))
	((pointerp arg) (mem-aref (%vector-double-to-c-array arg) :double i))
    (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; KeyPoint* std_vectorkpToCArray(vector_KeyPoint* s) 
(defcfun ("std_vectorkpToCArray" %vector-Keypoint-to-c-array) :pointer 
  (s (:pointer vector-keypoint)))


(let ((previous nil))
  (defun c-arr-to-vector-keypoint (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents a))))
    (%c-arr-to-vector-keypoint (cdr previous) (length a))))


(defun vector-keypoint (&optional arg (n nil) (i nil))
	   (cond ((eq arg nil) (return-from vector-keypoint (%vector-keypoint)))
		 ((listp arg)
		  (return-from vector-keypoint (c-arr-to-vector-keypoint arg)))
		 ((and (pointerp arg) n (not i)) (mem-aref (%vector-keypoint-to-c-array arg) :pointer n))
		 ((and (pointerp arg) n i) (mem-aref (mem-aref (%vector-keypoint-to-c-array arg) :pointer n) :float i))
		 (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_float* std_create_vectorf() 
(defcfun ("std_create_vectorf" %vector-float) (:pointer vector-float))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_float* std_carrayTovectorf(float* a, size_t len)
(defcfun ("std_carrayTovectorf" %c-arr-to-vector-float) (:pointer vector-float)
  (a :pointer)
  (len :unsigned-int))


;; template < class T, class Alloc = allocator<T> > class vector
;; float* std_vectorfToCArray(vector_float* s) 
(defcfun ("std_vectorfToCArray" %vector-float-to-c-array) :pointer 
  (s (:pointer vector-float)))


(let ((previous nil))
  (defun c-arr-to-vector-float (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :float :initial-contents a))))
    (%c-arr-to-vector-float (cdr previous) (length a))))


(defun vector-float (&optional arg (i 0))
  (cond ((eq arg nil) (return-from vector-float (%vector-float)))
	((listp arg)
	 (return-from vector-float (c-arr-to-vector-float arg)))
	((pointerp arg) (mem-aref (%vector-float-to-c-array arg) :float i))
    (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_char* std_create_vectorc() 
(defcfun ("std_create_vectorc" %vector-char) (:pointer vector-char))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_char* std_carrayTovectorc(char* a, size_t len)
(defcfun ("std_carrayTovectorc" %c-arr-to-vector-char) (:pointer vector-char)
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun c-arr-to-vector-char (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :char :initial-contents a))))
    (%c-arr-to-vector-char (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; char* std_vectorcToCArray(vector_char* s) 
(defcfun ("std_vectorcToCArray" %vector-char-to-c-array) :pointer 
  (s (:pointer vector-char)))


(defun vector-char (&optional arg (i 0))
  (cond ((eq arg nil) (return-from vector-char (%vector-char)))
	((listp arg)
	 (return-from vector-char (c-arr-to-vector-char arg)))
	((pointerp arg) (mem-aref (%vector-char-to-c-array arg) :char i))
    (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_int* std_create_vector() 
(defcfun ("std_create_vector" %vector-int) (:pointer vector-int))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_int* std_carrayTovector(int* a, size_t len)
(defcfun ("std_carrayTovector" %c-arr-to-vector-int) (:pointer vector-int)
  (a :pointer)
  (len :unsigned-int))


;; template < class T, class Alloc = allocator<T> > class vector
;; int* std_vectorToCArray(vector_int* s) 
(defcfun ("std_vectorToCArray" %vector-int-to-c-array) :pointer 
  (s (:pointer vector-int)))


(let ((previous nil))
  (defun c-arr-to-vector-int (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :int :initial-contents a))))
    (%c-arr-to-vector-int (cdr previous) (length a))))


(defun vector-int (&optional arg (i 0))
  (cond ((eq arg nil) (return-from vector-int (%vector-int)))
	((listp arg)
	 (return-from vector-int (c-arr-to-vector-int arg)))
	((pointerp arg) (mem-aref (%vector-int-to-c-array arg) :int i))
    (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_Point* std_create_vectorp() 
(defcfun ("std_create_vectorp" %vector-point) (:pointer vector-point))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_Point* std_carrayTovectorp(Point* a, size_t len)
(defcfun ("std_carrayTovectorp" %c-arr-to-vector-point) (:pointer vector-point)
  (a :pointer)
  (len :unsigned-int))


;; template < class T, class Alloc = allocator<T> > class vector
;; Point2f* std_vectorpToCArray(vector_Point* s) 
(defcfun ("std_vectorpToCArray" %vector-point-to-c-array) :pointer 
  (s (:pointer vector-point)))


(let ((previous nil))
  (defun c-arr-to-vector-point (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents a))))
    (%c-arr-to-vector-point (cdr previous) (length a))))


(defun vector-point (&optional arg (n nil) (i nil))
	   (cond ((eq arg nil) (return-from vector-point (%vector-point)))
		 ((listp arg)
		  (return-from vector-point (c-arr-to-vector-point arg)))
		 ((and (pointerp arg) n (not i)) (mem-aref (%vector-point-to-c-array arg) :pointer n))
		 ((and (pointerp arg) n i) (mem-aref (mem-aref (%vector-point-to-c-array arg) :pointer n) :int i))
		 (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_Point2f* std_create_vectorp2f() 
(defcfun ("std_create_vectorp2f" %vector-point2f) (:pointer vector-point2f))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_Point2f* std_carrayTovectorp2f(Point2f* a, size_t len)
(defcfun ("std_carrayTovectorp2f" %c-arr-to-vector-point2f) (:pointer vector-point2f)
  (a :pointer)
  (len :unsigned-int))


;; template < class T, class Alloc = allocator<T> > class vector
;; Point2f* std_vectorp2fToCArray(vector_Point2f* s) 
(defcfun ("std_vectorp2fToCArray" %vector-point2f-to-c-array) :pointer 
  (s (:pointer vector-point2f)))


(let ((previous nil))
  (defun c-arr-to-vector-point2f (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents a))))
    (%c-arr-to-vector-point2f (cdr previous) (length a))))


(defun vector-point2f (&optional arg (n nil) (i nil))
	   (cond ((eq arg nil) (return-from vector-point2f (%vector-point2f)))
		 ((listp arg)
		  (return-from vector-point2f (c-arr-to-vector-point2f arg)))
		 ((and (pointerp arg) n (not i)) (mem-aref (%vector-point2f-to-c-array arg) :pointer n))
		 ((and (pointerp arg) n i) (mem-aref (mem-aref (%vector-point2f-to-c-array arg) :pointer n) :float i))
		 (t nil)))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_Mat* std_create_vectorm() 
(defcfun ("std_create_vectorm" %vector-mat) (:pointer vector-mat))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_Mat* std_carrayTovectorm(Mat* a, size_t len)
(defcfun ("std_carrayTovectorm" %c-arr-to-vector-mat) (:pointer vector-mat)
  (a :pointer)
  (len :unsigned-int))


;; template < class T, class Alloc = allocator<T> > class vector
;; Mat* std_vectormToCArray(vector_Mat* s) 
(defcfun ("std_vectormToCArray" %vector-mat-to-c-array) :pointer 
  (s (:pointer vector-mat)))


(let ((previous nil))
  (defun c-arr-to-vector-mat (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :pointer :initial-contents a))))
    (%c-arr-to-vector-mat (cdr previous) (length a))))


(defun vector-mat (&optional arg (n nil) (i nil))
	   (cond ((eq arg nil) (return-from vector-mat (%vector-mat)))
		 ((listp arg)
		  (return-from vector-mat (c-arr-to-vector-mat arg)))
		 ((and (pointerp arg) n (not i)) (mem-aref (%vector-mat-to-c-array arg) :pointer n))
		 ((and (pointerp arg) n i) (mem-aref (mem-aref (%vector-mat-to-c-array arg) :pointer n) :float i))
		 (t nil)))

;; template < class T, class Alloc = allocator<T> > class vector
;; vector_uchar* std_create_vectoru() 
(defcfun ("std_create_vectoru" %vector-uchar) (:pointer vector-uchar))


;; template < class T, class Alloc = allocator<T> > class vector
;; vector_uchar* std_carrayTovectoru(uchar* a, size_t len)
(defcfun ("std_carrayTovectoru" %c-arr-to-vector-uchar) (:pointer vector-uchar)
  (a :pointer)
  (len :unsigned-int))


(let ((previous nil))
  (defun c-arr-to-vector-uchar (a)
    (unless (equal a (car previous))
      (setf previous (cons a (foreign-alloc :uchar :initial-contents a))))
    (%c-arr-to-vector-uchar (cdr previous) (length a))))


;; template < class T, class Alloc = allocator<T> > class vector
;; uchar* std_vectoruToCArray(vector_uchar* s) 
(defcfun ("std_vectoruToCArray" %vector-uchar-to-c-array) :pointer 
  (s (:pointer vector-uchar)))


(defun vector-uchar (&optional arg (i 0))
  (cond ((eq arg nil) (return-from vector-uchar (%vector-uchar)))
	((listp arg)
	 (return-from vector-uchar (c-arr-to-vector-uchar arg)))
	((pointerp arg) (mem-aref (%vector-uchar-to-c-array arg) :uchar i))
    (t nil)))

