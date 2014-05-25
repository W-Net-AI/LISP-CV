;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; examples.lisp
;;;; Documentation and Examples(In process) -> For now, if you want to know if a Lisp binding exists 
;;;; for a specified OpenCV C++ function search this file for the OpenCV C++ function name to find its 
;;;; Lisp name, documentation and an example program.

There are 3 major types of memory management, manual, with-* macros and Trivial Garbage finalizers
Most examples so far use manual memory management, which is unsafe, but long-lived and fast. I'll 
be adding some examples from now on, using the new with-* macro syntax as well as examples that use 
Trivial Garbage finalizers. Trivial Garbage finalizers automatically free memory without having to 
use with-* macros or manual memory management, but, though they are long-lived and safe, are a little 
slower than with-* macros or manual MM. I'll be offering all three forms of memory management in this 
library as well as provide many, many examples of each type. For reference, here is an example that 
shows the camera output in a window using the with-* macro syntax(See MAT-EXAMPLE in this file for 
an example that uses TG finalizers):


(defun with-macro-example (&optional 
			     (cam *camera-index*) 
			     (width *default-width*)
			     (height *default-height*))

  (with-captured-camera (cap cam :width width :height height)
    (if (not (cap-is-open cap)) 
	(return-from with-macro-example 
	  (format t "Cannot open the video camera")))      
    (let ((window-name "WITH-MACRO Example"))
      (format t "~%Frame Size : ~ax~a~%~%" 
	      (cap-get cap +cap-prop-frame-width+)
	      (cap-get cap +cap-prop-frame-height+))
      (with-named-window (window-name +window-normal+)
	(move-window window-name 759 175)
	(loop
	   (with-mat ((frame (mat)))
	     (cap-read cap frame)
	     (imshow window-name frame)
	     (let ((c (wait-key 33)))
	       (when (= c 27)
		 (return)))))))))



CORE - BASIC STRUCTURES:



ADD

Adds two matrices.

C++: MatExpr + operator

LISP-CV: (ADD (M1 MAT) (M2 MAT)) => MAT-EXPR


    Parameters:	

        M1 - A matrix.

        M2 - A matrix.


The function ADD adds the elements of matrix M1 to the elements of matrix M2 in order. Both 
matrices must have the same number of rows and columns. You may need to coerce the result of 
ADD, the return value, back to type MAT with the function (FORCE), (or the shorthand version 
(>>)) to use in other functions. 


(defun add-example ()

  "Matrix M1 and M2 are added together with the 
   function ADD. Matrix M1, M2 and RESULT are t-
   hen printed."

  (with-object ((data (alloc :uint '(1 2 3 4 5 6 7 8 9))))
    (with-mat ((m1 (mat 3 3 +32s+ data))
	       (m2 (mat 3 3 +32s+ data)))
      (with-mat-expr ((result (add m1 m2)))
	(format t "~%M1 =~%~%")
	(print-mat m1 :int)
	(format t "~%M2 = ~%~%")
	(print-mat m2 :int)
	(format t "~%RESULT = ~%~%")
	(with-mat ((forced-result (>> result)))
	  (print-mat forced-result :int)
	  (format t "~%"))))))



ADJUST-ROI

Adjusts a submatrix size and position within the parent matrix.

C++: Mat& Mat::adjustROI(int dtop, int dbottom, int dleft, int dright)

LISP-CV: (ADJUST-ROI (SELF MAT) (DTOP :INT) (DBOTTOM :INT) (DLEFT :INT) (DRIGHT :INT)) => MAT

    Parameters:	

        SELF - A Matrix

        DTOP - Shift of the top submatrix boundary upwards.

        DBOTTOM - Shift of the bottom submatrix boundary downwards.

        DLEFT - Shift of the left submatrix boundary to the left.

        DRIGHT - Shift of the right submatrix boundary to the right.

The method is complimentary to the function (LOCATE-ROI). The typical use of these functions is to 
determine the submatrix position within the parent matrix and then shift the position. Typically, it 
can be required for filtering operations when pixels outside of the ROI should be taken into account. 
When all the method parameters are positive, the ROI needs to grow in all directions by the specified 
amount, for example:

(ADJUST-ROI A 2 2 2 2)

In this example, the matrix size is increased by 2 elements in each direction. The matrix is shifted 
by 2 elements to the left and 2 elements up, which brings in all the necessary pixels for filtering 
with the 5x5 kernel.

ADJUST-ROI forces the adjusted ROI to be inside of the parent matrix that is in the boundaries of the 
adjusted ROI are constrained by boundaries of the parent matrix. For example, if the submatrix A is 
located in the first row of a parent matrix and you called (ADJUST-ROI A 2 2 2 2) then A will not be 
increased in the upward direction.

The function is used internally by the OpenCV filtering functions, like (FILTER-2D) , morphological 
operations, and so on.

See also

(COPY-MAKE-BORDER)


(defun adjust-roi-example (&optional 
			     (camera-index *camera-index*) 
			     (width *default-width*)
			     (height *default-height*))
  ;Set camera feed to CAP
  (with-capture (cap (video-capture camera-index))
    (let ((window-name-1 "Original FRAME - ADJUST-ROI Example")
	  (window-name-2 "Region of interest - ADJUST-ROI Example")
	  (window-name-3 "FRAME readjusted to original dimensions - ADJUST-ROI Example"))
      (if (not (cap-is-open cap)) 
	  (return-from adjust-roi-example 
	    (format t "Cannot open the video camera")))
      ;Set width and height of CAP
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-height+ height)
      (format t "~%Frame Size : ~ax~a~%~%" 
	      (cap-get cap +cap-prop-frame-width+)
	      (cap-get cap +cap-prop-frame-height+))
      (named-window window-name-1 +window-normal+)
      (named-window window-name-2 +window-normal+)
      (named-window window-name-3 +window-normal+)
      (move-window window-name-1 310 175)
      (move-window window-name-2 760 175)
      (move-window window-name-3 1210 175)
      (do* ((frame 0)
            ;Create rectangle RECT
            (rect (rect (round (/ width 4)) 
			(round (/ height 4))  
			(round (/ width 2)) 
			(round (/ height 2)))))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
        ;Set camera feed to FRAME
	(setf frame (mat))
	(cap-read cap frame)
        ;Show original FRAME in window
	(imshow window-name-1 frame)
        ;Set FRAME region of interest to RECT, half 
        ;the size of FRAME, positioned in the middle 
        ;of FRAME
	(setf frame (roi frame rect))
        ;;Show adjusted FRAME in window
	(imshow window-name-2 frame)
        ;Readjust frame back to original dimensions
	(adjust-roi frame 120 120 160 160)
        ;Show readjusted FRAME in a window
	(imshow window-name-3 frame))
      (destroy-all-windows))))



ASSGN-VAL

Assign a scalar value to a matrix.

C++: MatExpr = operator

LISP-CV: (ASSGN-VAL (SELF MAT) (S SCALAR)) => MAT

    Parameters:	

        SELF - A matrix

        S - A scalar.


Use the function SCALAR-ALL, as the S parameter value, to set each matrix element to the same value
for example: (SCALAR-ALL -1) will assign -1 to every element of the matrix. Use the function SCALAR 
to assign a specific color value to each element of the matrix i.e. (SCALAR 0 255 0) will set every 
matrix element to green. This is useful when you need to add/subtract a certain color value from an 
image. The matrix you assign the scalar value to will be overwritten by the operation, there is no 
need to access the return value of ASSGN-VAL to complete the operation,


(defun assgn-val-example (filename)

  (let* ((window-name-1 "IMAGE - ASSGN-VAL Example")
         (window-name-2 "MAT - ASSGN-VAL Example")
         (b (alloc :int 0))
         (g (alloc :int 0))
	 (r (alloc :int 0)))
    ;; Load an image
    (with-mat ((image (imread filename 1))
	       ;; Create a matrix to fill with a 
               ;; scalar value defined below
	       (mat (mat (rows image) (cols image) +8uc3+)))
      ;; Print IMAGE type. It is important to know this
      ;; Before subtracting a matrix from an image. You 
      ;; must set the matrix size and type to be the sa-
      ;; me as the image
      (format t "~%IMAGE type = ~a(+8UC3+)~%~%" (mat-type image))
      (with-named-window (window-name-1 +window-normal+)
	(with-named-window (window-name-2 +window-normal+)	
	  (move-window window-name-1 533 175)
	  (move-window window-name-2 984 175)
          ;; Create trackbars to change the BGR 
          ;; values we will later subtract from 
          ;; IMAGE
	  (create-trackbar  "B" window-name-2 b 255)
	  (create-trackbar  "G" window-name-2 g 255)
	  (create-trackbar  "R" window-name-2 r 255)
	  (loop
	     ;; Set all elements of MAT to SCALAR.
	     (with-scalar ((scalar (scalar (? b :int) 
					   (? g :int) 
					   (? r :int))))
	       (assgn-val mat scalar))
	     ;; Subtract MAT from IMAGE
	     (with-mat-expr ((result (sub image mat)))
	       ;; Show results
	       (imshow window-name-1 image)
               ;; Coerce RESULT to a MAT 
               ;; before showing in window
	       (with-mat ((forced-result (>> result)))
		 (imshow window-name-2 forced-result)
		 (let ((c (wait-key 33)))
		   (when (= c 27)
		     (return)))))))))))


AT

Returns a reference to the specified array element.

C++: uchar* Mat::ptr(int i0=0)

CFFI: mem-aref ptr type &optional (index 0)

CFFI: (setf (mem-aref ptr type &optional (index 0)) new-value) 

LISP-CV: (AT (SELF MAT) (I :INT) (J :INT) (TYPE :KEYWORD)) 

LISP-CV: (AT (SELF MAT) (I :INT) (J :INT) (TYPE :KEYWORD)) 


    Parameters:	

        I - Index along the dimension 0

        J - Index along the dimension 1


This isn't a binding for the OpenCV Mat::at functions. It was decided, when making the C bindings 
for the C++ interface that LISP-CV wraps around, that they wouldn't be included. The reason was 
they were slower than accessing the data directly with the Mat class ptr member. So this function 
offers the same functionality as Mat::at but it is a binding for the Mat class ptr member. It is 
setf-able, meaning you can retrieve a matrix element but also set data to a matrix element with it 
as well using the SETF function. That functionality is gained by the inclusion the CFFI function 
MEM-AREF in the binding. The use of typenames in OpenCV is simulated here with keyword parameters:

The typenames associated with AT include(so far):

:char     :int16    :short    :uint32
:double   :int32    :uchar    :uint64
:float    :int64    :uint     :ullong
:int      :llong    :uint8    :ulong
:int8     :long     :uint16   :ushort


The example below initializes a Hilbert matrix:


(defun at-example ()
  (let ((h (mat 5 5 +64f+)))
    (dotimes (i (rows h))
      (dotimes (j (cols h))
	(setf (at h i j :double) (/ 1.0d0 (+ i j 1)))
	(princ (at h i j :double))
	(princ #\Space))
      (princ #\Newline)))) 



CHANNELS

Returns the number of matrix channels.

C++: int Mat::channels() const

LISP-CV: (CHANNELS (SELF MAT)) => :INT

    Parameters:	

        SELF - A matrix.  

The method returns the number of matrix channels.


(defun channels-example (filename)

  "All BRG images are 3 channel,(BLUE, GREEN, RED). When 
   you convert an image to grayscale you also convert it 
   to a 1 channel image. This example shows that with th-
   e function CVT-COLOR and the function CHANNELS."

  ;; Read image
  (with-mat ((img (imread filename 1)))
    (if (eq 1 (channels img)) 
	(return-from channels-example 
	  (format t "Must load full color image")))
    (let ((window-name-1 "Original image - CHANNELS Example")
	  (window-name-2 "Grayscale image - CHANNELS Example"))
      (with-named-window (window-name-1 +window-normal+)
	(with-named-window (window-name-2 +window-normal+)	
	  (move-window window-name-1 514 175)
	  (move-window window-name-2 966 175)
	  ;;Print the original number of channels of IMG
	  (format t "~%The original number of channels of IMG = ~a~%~%" 
		  (channels img))
	  (imshow window-name-1 img)
	  ;;Convert IMG to a grayscale image
	  (cvt-color img img +bgr2gray+)
	  ;;Print the number of channels of a grayscale IMG
	  (format t "Grayscale IMG number of channels = ~a~%~%" 
		  (channels img)) 
	  (imshow window-name-2 img)
	  (loop
	     (let ((c (wait-key 33)))
	       (when (= c 27)
		 (return)))))))))


COL-RANGE

Creates a matrix header for the specified column span.

C++: Mat Mat::colRange(int startcol, int endcol) const

LISP-CV: (COL-RANGE (SELF MAT) (STARTCOL :INT) (ENDCOL :INT)) => MAT


    Parameters:	

        SELF - A matrix

        STARTCOL - An inclusive 0-based start index of the column span.

        ENDCOL - An exclusive 0-based ending index of the column span.

    
The method makes a new header for the specified column span of the matrix. Similarly to (ROW) and 
(COL) functions, this is an O(1) operation.


(defun col-range-example ()
        ;Create matrix data
  (let* ((data (alloc :uchar '(1 2 3 4 5 6 7 8 9)))
         ;Create matrix
	 (mat (mat 3 3 +8u+ data)))
    (princ #\Newline)
    ;Print matrix normally by 
    ;accessing entire matrix
    (dotimes (i (rows mat))
      (dotimes (j (cols mat))
	(format t "~a" (at mat i j :uchar))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    ;Retrieve all 3 columns of MAT 
    ;with COL-RANGE to print matrix
    (dotimes (i (rows mat))
      (dotimes (j (cols mat))
	(format t "~a" (at 
			(col-range mat 0 (cols mat)) 
			i j :uchar))
	(princ #\Space))
      (princ #\Newline))
      (format t "~%")
      (free data)))



COLS

Returns number or cols in MAT.

C++: int rows, cols

LISP-CV:  (COLS (SELF MAT)) => :INT


    Parameters:	

        SELF - A matrix(MAT).


The function COLS finds the number of columns in a matrix or -1 when the array has more than 2 dime-
nsions. 


(defun cols-example ()

  "Uses COLS to find the number of columns in the matrix MAT"

  (with-mat ((mat (mat 3 4 +64f+ (scalar 5))))
    (format t "~%The number of columns in MAT = ~a~%~%" (cols mat))))




COPY-TO

Copies the matrix to another one.

C++: void Mat::copyTo(OutputArray m) const

LISP-CV: (COPY-TO (SELF MAT) (M MAT)) => :VOID

C++: void Mat::copyTo(OutputArray m, InputArray mask) const

LISP-CV: (COPY-TO (SELF MAT) (M MAT) (MASK MAT)) => :VOID


    Parameters:	

        SELF - A matrix.         

        M - Destination matrix. If it does not have a proper size or type before the operation, it 
            is reallocated.

        MASK - Operation mask. Its non-zero elements - which matrix elements need to be copied.


The method copies the matrix data to another matrix. Before copying the data, the method invokes:

(CREATE (MAT-SIZE THIS) (MAT-TYPE THIS))

So that the destination matrix is reallocated if needed. While (COPY-TO M M) works flawlessly, the 
function does not handle the case of a partial overlap between the source and destination matrices. 
When the operation mask is specified, and the (CREATE) call shown above reallocated the matrix, the 
newly allocated matrix is initialized with all zeros before copying the data.


(defun copy-to-example ()
  ;; initialize data for matrices
  (with-object ((data (alloc :int '(10 20 30 40))))
    ;; initialize MAT-1 with DATA.
    (with-mat ((mat-1 (mat 2 2 +32s+ data))
	       ;; initialize MAT-2, a second, 
	       ;; identical matrix with DATA
	       (mat-2 (mat 2 2 +32s+ data))
	       ;; create empty matrices M-1 and M-2
	       (m-1 (mat))
	       (m-2 (mat))
	       ;; create a mask for MAT-2 COPY-TO 
	       ;; operation, an identity matrix. 
	       ;; its non-zero elements indicate 
	       ;; which matrix elements to copy
	       (mask (mat-eye 2 2 +8u+)))
      ;; print contents of MAT-1.
      (format t "~%MAT-1 =~%~%")
      (print-mat mat-1 :int)
      (format t "~%")
      ;; copy data from MAT-1 to M-1.
      (copy-to mat-1 m-1)
      ;; print contents of of M-1.
      (format t "M-1 =~%~%")
      (print-mat m-1 :int)
      (format t "~%")
      ;; print contents of MAT-2.
      (format t "MAT-2 =~%~%")
      (print-mat mat-2 :int)
      (format t "~%")
      ;; print contents of MASK.
      (format t "MASK =~%~%")
      (print-mat mask :uchar)
      (format t "~%")
      ;; copy data from MAT-2 
      ;; to M-2 using mask.
      (copy-to mat-2 m-2 mask)
      ;; print final contents of M-2.
      (format t "M-2 =~%~%")
      (print-mat m-2 :int)
      (format t "~%"))))


CROSS

Computes a cross-product of two 3-element vectors.

C++: Mat Mat::cross(InputArray m) const

LISP-CV: (CROSS (SELF MAT) (M MAT)) => MAT


    Parameters:	

        SELF - A 3-element vector

        m - Another cross-product operand.


The function CROSS computes a cross-product of two 3-element vectors. The vectors must be 3-element 
floating-point vectors of the same shape and size. The result is another 3-element vector of the same 
shape and type as operands.


(defun cross-example ()

  (with-object ((data1 (alloc :double '(7d0 3d0 -4d0)))
		(data2 (alloc :double '(1d0 0d0 6d0))))
    (with-mat ((vec1 (mat 1 3 +64f+ data1))
	       (vec2 (mat 1 3 +64f+ data2)))
					;Print VEC1
      (format t "~%VEC1 = ~%~%")
      (dotimes (j (cols vec1))
	(format t "~a" (at vec1 0 j :double))
	(princ #\Space))
					;Print VEC2
      (format t "~%~%VEC2 = ~%~%")
      (dotimes (j (cols vec2))
	(format t "~a" (at vec2 0 j :double))
	(princ #\Space))
					;Find and print cross product 
					;of VEC1 and VEC2
      (with-mat ((cp (cross vec1 vec2)))
	(format t "~%~%The cross product is: ~%~%")
	(dotimes (j (cols cp))
	  (format t "~a" (at cp 0 j :double))
	  (princ #\Space))
	(format t "~%~%")))))



DATA

Pointer to MAT data.

C++: uchar* data

LISP-CV: (DATA (SELF MAT) ) => :POINTER

    Parameters:	

        SELF  a pointer to matrix(MAT construct)


Once a matrix is created, it will be automatically managed by using a reference-counting mechanism
(unless the matrix header is built on top of user-allocated data, in which case you should handle t-
he data by yourself). The matrix data will be deallocated when no one points to it; if you want to 
release the data pointed by a matrix header before the matrix destructor is called, use the functio-
n (RELEASE).

The next important thing to learn about the matrix class is element access. Here is how the matrix 
is stored. The elements are stored in row-major order (row by row). The (DATA) function points to 
the first element of the first row, the (ROWS) function contains the number of matrix rows, (COLS) - 
the number of matrix columns. There is yet another function, (STEP), that is used to actually compu-
te the address of a matrix element. (COLS) is needed because the matrix can be part of another matr-
ix or because there can be some padding space in the end of each row for a proper alignment.



(defun data-example (filename)

  (let ((window-name "DATA Example")
	;;Variables used to hold the 
	;;BGR image pixel values
	(b 0)
	(g 0)
	(r 0)
        ;;INPUT is a pointer 
        ;;to the IMG data
	(input (data img))) 
    ;;Read image
    (with-mat ((img (imread filename 1))) 
      (if (empty img) 
	  (return-from data-example 
	    (format t "Image not loaded")))
      (with-named-window (window-name +window-normal+)
	(move-window window-name 759 175)
	;;In a loop access IMG pixel data using the STEP* 
	;;function and print to a file, instead of the co-
	;;nsole, so your implementation doesn't freeze.
	(with-open-file (str (cat *lisp-cv-src-dir* 
				  "/data/pixel-values.txt")
			     :direction :output
			     :if-exists :supersede
			     :if-does-not-exist :create)
	  (dotimes (i (rows img))
	    (dotimes (j (cols img))
	      (setf b (mem-aref input :uchar 
				(+  (* (*step img) i) (* 3 j) )))
	      (setf g (mem-aref input :uchar 
				(+ (+  (* (*step img) i) (* 3 j) ) 1)))
	      (setf r (mem-aref input :uchar 
				(+ (+  (* (*step img) i) (* 3 j) ) 2)))
	      (format str "(~a,~a,~a)~%" b g r))))
	(imshow window-name img)
	(loop
	   (let ((c (wait-key 33)))
	     (when (= c 27)
	       (return))))))))



DEPTH

Returns the depth of a matrix element.

C++: int Mat::depth() const

LISP-CV: (DEPTH (SELF MAT)) => :INT

The method returns the identifier of the matrix element depth (the type of each individual channel). 
For example, for a 16-bit signed element array, the method returns +16S+ . A complete list of matrix 
types contains the following values:

    +8U+ - 8-bit unsigned integers ( 0..255 )

    +8S+ - 8-bit signed integers ( -128..127 )

    +16U+ - 16-bit unsigned integers ( 0..65535 )

    +16S+ - 16-bit signed integers ( -32768..32767 )

    +32S+ - 32-bit signed integers ( -2147483648..2147483647 )

    +32F+ - 32-bit floating-point numbers ((* +FLT-MAX+ -1)..+FLT-MAX+, INF, NAN) 

    +64F+ - 64-bit floating-point numbers ((* +DBL-MAX+ 1)..+DBL-MAX+, INF, NAN)

;; INF and NAN C++ constants and are not currently implimented in LISP-CV


Example:


LISP-CV> (DEFPARAMETER A (MAT 3 3 +8SC1+)) ;Initialize 1 channel matrix of 8-bit signed integer type

A

LISP-CV> (MAT-TYPE A) 

1   ;The type of the matrix is 1(+8SC1+) - 1 channel matrix with 8-bit signed integer elements

LISP-CV> (DEPTH A)

1   ;The type of the matrix elements are 1(+8S+) - 8-bit signed integer


LISP-CV> (DEFPARAMETER A (MAT 3 3 +8SC3+)) ;Initialize 3 channel matrix of 8-bit signed integer type

A

LISP-CV> (MAT-TYPE A)  

17   ;The type of the matrix is 17(+8SC1+) - 3 channel matrix with 8-bit signed integer elements

LISP-CV> (DEPTH A)

1   ;The type of the matrix elements are 1(+8S+) - 8-bit signed integer



DMATCH


DMATCH constructor.


C++: DMatch() : queryIdx(-1), trainIdx(-1), imgIdx(-1),

LISP-CV: (DMATCH) => DMATCH

C++: DMatch( int _queryIdx, int _trainIdx, float _distance ) 

LISP-CV: (DMATCH (QUERY-IDX :INT) (TRAIN-IDX :INT) (DISTANCE :FLOAT)) => DMATCH

C++: DMatch( int _queryIdx, int _trainIdx, int _imgIdx, float _distance )

LISP-CV: (DMATCH (QUERY-IDX :INT) (TRAIN-IDX :INT) (IMG-IDX :INT) (DISTANCE :FLOAT)) => DMATCH


    Parameters:	

        QUERY-IDX - Query descriptor index

        TRAIN-IDX - Train descriptor index
       
        IMG-IDX - Train descriptor index
    
        DISTANCE - Distance between descriptors. The lower, the better it is.



Used for matching keypoint descriptors: query descriptor index, train descriptor index, train image 
index, and distance between descriptors.


Example:

TODO(Write an example showing how to create DMatch manually, add to vector and send to DRAW-MATCHES)



ELEM-SIZE


Returns the matrix element size in bytes.


C++: size_t Mat::elemSize() const

LISP-CV: (ELEM-SIZE (SELF MAT)) => :UNSIGNED-INT


    Parameters: 

        SELF - A matrix.


The method returns the matrix element size in bytes. For example, if the matrix type is +16SC3+ , the 
method returns 3*sizeof(short) or 6.


Example:

See STEP1 example.



ELEM-SIZE1


Returns the size of each matrix element channel in bytes.


C++: size_t Mat::elemSize1() const

LISP-CV: (ELEM-SIZE1 (SELF MAT)) => :UNSIGNED-INT


    Parameters:

        SELF - A matrix


The method returns the matrix element channel size in bytes, that is, it ignores the number of channels. 
For example, if the matrix type is +16SC3+ , the method returns (SIZE-OF :SHORT) or 2.


Example:

See STEP1 example.



EMPTY

Returns true if the array has no elements.

C++: bool Mat::empty() const

LISP-CV: (EMPTY (SELF MAT)) => :BOOLEAN


(defun empty-example (filename)
  ;; load image 
  (let* ((image (imread filename 1))
	 (window-name "EMPTY Example"))
    ;; if image is not loaded correctly 
    ;; the return of EMPTY is true and 
    ;; the function is exited
    (if (empty image) 
	(return-from empty-example 
	  (format t "Image not loaded")))
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    (imshow window-name image)
    (loop while (not (= (wait-key 0) 27)))
    (del-mat image)
    (destroy-window window-name)))



INV

Inverses a matrix.

C++: MatExpr Mat::inv(int method=DECOMP_LU) const

LISP-CV: (INV (SELF MAT) (METHOD :INT)) => MAT-EXPR 

    Parameters:	

        SELF - A matrix.

        METHOD -

        Matrix inversion method. Possible values are the following:

            +DECOMP-LU+ is the LU decomposition. The matrix must be non-singular.

            +DECOMP-CHOLESKY+ is the Cholesky decomposition for symmetrical positively 
                              defined matrices only. This type is about twice faster than 
                              +DECOMP-LU+ on big matrices.

            +DECOMP-SVD+ is the SVD decomposition. If the matrix is singular or even 
                         non-square, the pseudo inversion is computed.


The method performs a matrix inversion by means of Matrix Expressions. This means that a temporary 
matrix inversion object is returned by the method and can be used further as a part of more complex 
Matrix Expressions or can be assigned to a matrix. You may need to coerce the return value of INV 
back to type MAT with the function (FORCE), (or the shorthand version (>>)) to use in other functions.


(defun inv-example (filename)

  "Matrix division, which is undefined in vector mathematics, 
   is simulated here by finding the inverse of a matrix, DIV-
   ISOR and then multiplying another matrix, MAT by it. We a-
   lso find the inverse of an image, IMAGE and show in a win-
   dow just to see what it looks like."

	 ;Read in image
  (let* ((image (imread filename 1))
	 ;Create matrix data
	 (data-1 (alloc :float '(1f0 2f0 3f0 3f0 2f0 1f0 2f0 1f0 3f0)))
	 (data-2 (alloc :float '(4f0 5f0 6f0 6f0 5f0 4f0 4f0 6f0 5f0)))
	 ;Create numerator matrix
         (numerator (mat 3 3 +32f+ data-1))
	 ;Create divisor matrix
         (divisor (mat 3 3 +32f+ data-2)) 
	 ;Find determinant of DIVISOR       
	 (determinant (det divisor)) 
	 (divisor-inv 0)
         (identity-mat 0)
	 (quotient 0)
         (image-inv 0)
	 (n (coerce (/ 1 255) 'double-float))
	 (window-name-1 "Original IMAGE - INV Example")
         (window-name-2 "Inverse of IMAGE - INV Example"))
    (if (empty image) 
	(return-from inv-example 
	  (format t "Image not loaded")))
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)
    (move-window window-name-1 533 175)
    (move-window window-name-2 984 175)
    ;Print NUMERATOR
    (format t "NUMERATOR =~%~%")
    (dotimes (i (rows numerator))
      (dotimes (j (cols numerator))
	(princ (at numerator i j :float))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    ;Print DIVISOR
    (format t "DIVISOR =~%~%")
    (dotimes (i (rows divisor))
      (dotimes (j (cols divisor))
	(princ (at divisor i j :float))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
   ;Check if the determinant of divisor is 0. If not, 
   ;an inverse cannot be determined, so exit program
    (if (= 0.0d0 determinant) 
	(return-from inv-example 
	  (progn (format t "The determinant of DIVISOR is 0.0d0.~%")
		 (format t "Cannot find its inverse.~%")))
	(format t "The determinant of DIVISOR = ~a~%~%" determinant))
   ;Find inverse of DIVISOR and print it
    (setf divisor-inv (>> (inv divisor +decomp-lu+)))
    (format t "The inverse of DIVISOR =~%~%")
    (dotimes (i (rows divisor-inv))
      (dotimes (j (cols divisor-inv))
	(princ (at divisor-inv i j :float))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
   ;Multiply DIVISOR by the inverse of divisor
   ;Output is a float identity matrix
    (setf identity-mat (>> (mul divisor divisor-inv)))
   ;Print the identity matrix
    (format t "Product of DIVISOR and its inverse =~%~%")
    (dotimes (i (rows identity-mat))
      (dotimes (j (cols identity-mat))
	(princ (at identity-mat i j :float))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    (setf quotient (>> (mul numerator (>> (inv divisor +decomp-lu+)))))
    (format t "Simulated quotient of NUMERATOR and DIVISOR =~%~%")
    (dotimes (i (rows quotient))
      (dotimes (j (cols quotient))
	(princ (at quotient i j :float))
	(princ #\Space))
      (princ #\Newline))
    ;Show original image
    (imshow window-name-1 image)
    ;Convert image to 1 channel
    (cvt-color image image +bgr2gray+)
    ;Convert image to float
    (convert-to image image +32f+ n)
    ;Find inverse of IMAGE
    (setf image-inv (>> (inv image +decomp-svd+)))
    ;Show inverse of IMAGE in window
    (imshow window-name-2 image-inv)
    (loop while (not (= (wait-key 0) 27)))
    (free data-1)
    (free data-2)
    (destroy-all-windows)))



IS-CONTINOUS

Reports whether the matrix is continuous or not.

C++: bool Mat::isContinuous() const

LISP-CV: (IS-CONTINUOUS) => :BOOLEAN

The method returns true if the matrix elements are stored continuously without gaps at the end of 
each row. Otherwise, it returns false. Obviously, 1x1 or 1xN matrices are always continuous. Matrices 
created with (CREATE) are always continuous. But if you extract a part of the matrix using (COL),
(DIAG) , and so on, or constructed a matrix header for externally allocated data, such matrices may 
no longer have this property.


(defun is-continuous-example (filename)
  (let* (;;Allocate matrix data
	 (data (alloc :uchar '(1 2 3 4 5 6 7 8 9)))
	 ;;Create initialized matrix
	 (mat (mat 3 3 +8u+ data))
         ;;Extract a 2x2 roi of MAT
	 (mat-roi (roi mat (rect 0 0 2 2)))
	 ;;Read in image
	 (img (imread filename 1))
         ;;Extraxt a 100x100 centered roi of IMG
	 (roi-size 100)
	 (x (- (/ (cols img) 2) (/ roi-size 2)))
	 (y (- (/ (rows img) 2) (/ roi-size 2)))
	 (img-roi (roi img (rect x y roi-size roi-size)))
	 (window-name-1 "3x3 Matrix - RESHAPE Example")
	 (window-name-2 "2x2 roi of MAT - RESHAPE Example")
	 (window-name-3 "Original image - RESHAPE Example")
	 (window-name-4 "Centered 100x100 roi of IMG - RESHAPE Example"))      
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)
    (named-window window-name-3 +window-normal+)
    (named-window window-name-4 +window-normal+)
    (move-window window-name-1 485 98)
    (move-window window-name-2 894 98)
    (move-window window-name-3 485 444)
    (move-window window-name-4 894 444)
    ;;Show original 3x3 matrix
    (imshow window-name-1 mat)
    ;;Show 2x2 roi of MAT
    (imshow window-name-2 mat-roi)
    ;;Show original image
    (imshow window-name-3 img)
    ;;Show a centered 100x100 roi of image
    (imshow window-name-4 img-roi)
    ;;This shows that submats(roi) are intrinsically non-continuous
    (format t "Are MAT matrix elements stored continuously: ~a~%~%" 
	    (is-continuous mat))
    (format t "Are MAT-ROI matrix elements stored continuously: ~a~%~%" 
	    (is-continuous mat-roi))
    (format t "Are IMG matrix elements stored continuously: ~a~%~%" 
	    (is-continuous img))
    (format t "Are IMG-ROI matrix elements stored continuously: ~a~%~%" 
	    (is-continuous img-roi))
    (loop while (not (= (wait-key 0) 27)))
    (destroy-all-windows)))




KEYPOINT


KEYPOINT constructor.


C++: KeyPoint::KeyPoint()

LISP-CV: (KEY-POINT) => KEY-POINT

C++: KeyPoint::KeyPoint(float x, float y, float _size, float _angle=-1, float _response=0, int _octave=0, int _class_id=-1)

LISP-CV: (KEY-POINT (X :FLOAT) (Y :FLOAT) (SIZE :FLOAT) &OPTIONAL ((ANGLE :FLOAT) -1) ((RESPONSE :FLOAT) 0) ((OCTAVE :INT) 0) 
        ((CLASS-ID :INT) -1)) => KEY-POINT



    Parameters:	

        X - X-coordinate of the keypoint

        Y - Y-coordinate of the keypoint

        PT - X & y coordinates of the keypoint

        SIZE - Keypoint diameter

        ANGLE - Keypoint orientation

        RESPONSE - Keypoint detector response on the keypoint (that is, strength of the keypoint)

        OCTAVE - Pyramid octave in which the keypoint has been detected

        CLASS-ID - Object id




Used for matching keypoint descriptors: query descriptor index, train descriptor index, train image 
index, and distance between descriptors.


Example:

TODO(Write example using DRAW KEYPOINTS to draw random keypoints)


LOCATE-ROI

Locates the matrix header within a parent matrix.

C++: void Mat::locateROI(Size& wholeSize, Point& ofs) const

LISP-CV: (LOCATE-ROI (SELF MAT) (S SIZE) (P POINT)) => :VOID


    Parameters:	

        SELF - A matrix

        WHOLE-SIZE - Output parameter that contains the size of the whole matrix containing *this as a part.

        OFS - Output parameter that contains an offset of *this inside the whole matrix.

After you have extracted a submatrix from a matrix MAT using (ROWS MAT), (COLS MAT), (ROW-RANGE MAT) 
,(COL-RANGE MAT), and others, the resultant submatrix points just to the part of the original parent 
matrix. However, each submatrix contains information (represented by the OpenCv Mat class DATASTART 
and DATAEND members) that helps reconstruct the original matrix size and the position of the extracted 
submatrix within the original matrix. The function LOCATE-ROI does exactly that.


(defun locate-roi-example (&optional 
			     (camera-index *camera-index*) 
			     (width *default-width*)
			     (height *default-height*))
  ;Set camera feed to CAP
  (with-capture (cap (video-capture camera-index))
    (let ((window-name-1 "Original FRAME - LOCATE-ROI Example")
	  (window-name-2 "Submatrix - LOCATE-ROI Example"))
      (if (not (cap-is-open cap)) 
	  (return-from locate-roi-example 
	    (format t "Cannot open the video camera")))
      ;Set width and height of CAP
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-height+ height)
      (format t "~%Frame Size : ~ax~a~%~%" 
	      (cap-get cap +cap-prop-frame-width+)
	      (cap-get cap +cap-prop-frame-height+))
      ;;Create windows and move to specified positions
      (named-window window-name-1 +window-normal+)
      (named-window window-name-2 +window-normal+)
      (move-window window-name-1 514 175)
      (move-window window-name-2 966 175)
      (do* ((frame 0)
            ;Create rectangle RECT
            (rect (rect (round (/ width 4)) 
			(round (/ height 4))  
			(round (/ width 2)) 
			(round (/ height 2))))
            ;;Create variables to hold the size and location 
            ;;information derived by LOCATE-ROI
            (roi-size (size 0 0))
             (roi-loc (point 0 0)))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
        ;Set camera feed to FRAME
	(setf frame (mat))
	(cap-read cap frame)
        ;Show original FRAME in window
	(imshow window-name-1 frame)
        ;Extract submatrix(roi) from FRAME
        (setf frame (roi frame rect))
        ;Locate the position of the submatrix 
        ;inside FRAME we just extracted as well 
        ;as the size of its parent matrix which 
        ;is, in this case FRAME 
	(locate-roi frame roi-size roi-loc)
        ;Print location of submatrix
        (format t "Location of FRAME region of interest (~a, ~a)
        ~%~%" (point-x roi-loc) (point-y roi-loc))
        ;Print size of parent matrix
        (format t "Size of FRAME (~a, ~a)
        ~%~%" (width roi-size) (height roi-size))
        ;;Show submatrix in window
	(imshow window-name-2 frame))
      (destroy-all-windows))))


MAT

Creates a matrix

C++: Mat::Mat()

LISP-CV: (MAT) => MAT

C++: Mat::Mat(int rows, int cols, int type)

LISP-CV: (MAT (ROWS :INT) (COLS :INT) (TYPE :INT)) => MAT

C++: Mat::Mat(int rows, int cols, int type, const Scalar& s)

LISP-CV: (MAT (ROWS :INT) (COLS :INT) (TYPE :INT) (S SCALAR)) => MAT

C++: Mat::Mat(int rows, int cols, int type, void* data, size_t step=AUTO_STEP)

LISP-CV: (MAT (ROWS :INT) (COLS :INT) (TYPE :INT) (DATA :POINTER)) => MAT


    Parameters:	

        ROWS - The number of rows
    
        COLS - The number of colounns

        TYPE - The type of the matrix

        S - A scalar

        DATA - A pointer to an array of numbers


Speed notes:

The speed of the function will improve as I fine tune it's operation. I time how long 2,592,000 runs
of (MAT :t) takes in the example below. 2,592,000 is the number of matrices that would be created, if 
you created 1 matrix for every 30fps video frame in a 24 hour period. (MAT :t) is the finalized version 
of (MAT) and finalizers are the slowest of the three forms of MM in this library. You can get a 33% speed 
increase in this library by compiling your programs to an executable before you run them(See the RUN macro 
in <lisp-cv-source-dir>/macros.lisp for details on that. Also, try looking into Paralella boards, a cheaper 
solution for mind blowing processing speed(I know, sounds like a commercial...but trust me:)).



(defun mat-example ()

  "In this example, I show how all the different MAT 
   types are used. I use some finalizer versions of 
   the MAT functions here. Their memory is automatic-
   ally managed so you don't need WITH-* macros or m-
   anual memory management to clean up. They are slo-
   wer though. The gc: prefix, signals automatic mem-
   ory management is activated. I prove it is activa-
   ted by creating millions of MAT objects at the en-
   d of the example. You should notice your RAM fluc-
   tuate, but not rise to any dangerous level. You c-
   an also use the shorter t: prefix(finalizer true) 
   to enable automatic GC in functions that support 
   it. All functions in this library that need to be 
   memory managed have a finalized version.

   Note: Automatic memory management is the slowest, 
         WITH-* macros are the quickest, and manual 
         MM is part way between the two."

  (with-object ((data (alloc :double '(1d0 2d0 3d0 4d0 5d0 
				       6d0 7d0 8d0 9d0))))
	  ;Create matrices
    (let* ((mat (gc:mat))
	   (mat-typed (gc:mat 4 4 +32s+))
	   (mat-value1 (gc:mat 3 3 +32f+ (scalar 255)))
	   (mat-value2 (gc:mat 3 3 +32f+ '(255)))
	   (mat-data1 (gc:mat 3 3 +64f+ data))
	   (mat-data2 (gc:mat 3 3 +64f+ :double 
			       '(1d0 2d0 3d0 4d0 5d0 
				 6d0 7d0 8d0 9d0))))
      ;Print matrices, gcc:: must be added to the front 
      ;of the PRINT-MAT function if you are printing a 
      ;finalizer 
      (format t "~%~%MAT = ~%~%")
      (print-mat mat :uchar)
      (format t "~%~%MAT-TYPED = ~%~%")
      (print-mat mat-typed :int)
      (format t "~%~%MAT-VALUE1 = ~%~%")
      (print-mat mat-value1 :float)
      (format t "~%~%MAT-VALUE2 = ~%~%")
      (print-mat mat-value2 :float)
      (format t "~%~%MAT-DATA1 = ~%~%")
      (print-mat mat-data1 :double)
      (format t "~%~%MAT-DATA2 = ~%~%")
      (print-mat mat-data2 :double)
      (format t "~%~%")
      ;Time how long it takes to create 2,592,000
      ;finalized matrices using the CL:TIME macro 
      ;`$`. I use the 't:' prefix here to enable 
      ;finalization.
     ($ (t:mat) 2592000))))


MUL

Finds the product of two matrices.

C++: MatExpr * operator

LISP-CV: (MUL (M1 MAT) (M2 MAT)) => MAT-EXPR


    Parameters:	

        M1 - A single float or double float matrix.

        M2 - A single float or double float matrix.


To perform matrix multiplication on two Matrices, the number of columns in the first matrix must be 
the same as the number of rows in the second Matrix. The matrices to be multiplied must also be of 
type Single Float(+32F+) or Double Float(+64f+). You may need to coerce the result of MUL, the return 
value, back to type MAT with the function (FORCE), (or the shorthand version (>>)) to use in other 
functions. 


(defun mul-example ()

  "In this example, MUL is used to find 
   the product of matrix, M1 and M2."

  (with-object ((data (alloc :float '(1.0f0 2.0f0 3.0f0 
				      4.0f0 5.0f0 6.0f0 
				      7.0f0 8.0f0 9.0f0))))
    (with-mat ((m1 (mat 3 3 +32f+ data))
	       (m2 (mat 3 3 +32f+ data)))
      (with-mat-expr ((result (mul m1 m2)))
	(format t "~%M1 = ~%~%")
	(print-mat m1 :float)
	(format t "~%~%M2 = ~%~%")
	(print-mat m2 :float)
	(format t "~%RESULT ~%~%")
	(with-mat ((forced-result (>> result)))
	  (print-mat forced-result :float)
	  (format t "~%"))))))


POINT


POINT constructor.


C++: Point_()

LISP-CV: (POINT) => POINT

C++: Point_(_Tp _x, _Tp _y)

LISP-CV:  (POINT (X :INT) (Y :INT)) => POINT

C++: _Tp x, y;

LISP-CV: (POINT-X (SELF POINT)) => :INT

C++: _Tp x, y;

LISP-CV: (POINT-Y (SELF POINT)) => :INT


    Parameters:	

        SELF - A POINT construct.

        X - X-coordinate of the point.

        Y -	Y-coordinate of the point.


POINT creates a 2D point with integer coordinates (usually zero-based). The functions POINT-X and  
POINT-Y are used to extract the x,y coordinates of a point.


(defun point-example (x y)

  "In this example we create an unitialized 
   POINT with the function POINT. Then crea-
   tes a point with the function POINT. Fin-
   ally, lists the x,y coordinates with the 
   POINT functions POINT-X and POINT-Y."

  (let* ((initialized-point (point))
	 (point (point x y)))
    (format t "~%Pointer to initialized point: ~a~%~%" 
	    initialized-point)
    (format t "POINT (x, y) = (~a, ~a)~%~%" 
	    (point-x point)
	    (point-y point))))


POINT-2D


POINT-2D constructor.


C++: typedef Point_<double> Point2d

LISP-CV: (POINT-2D (X :INT) (Y :INT)) => POINT-2D

C++: _Tp x, y

LISP-CV: (POINT-2D-X (SELF POINT-2D)) => :DOUBLE

C++: _Tp x, y

LISP-CV: (POINT-2D-Y (SELF POINT-2D)) => :DOUBLE


    Parameters:	

        SELF - A POINT-2D construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.


POINT-2D creates a 2D point with double-float coordinates (usually zero-based). Functions POINT-2D-X 
and  POINT-2D-Y are used to extract the x,y coordinates of the point.


(defun point-2d-example (x y)

  "In this example we create an uninitialized 
   point-2d with the function POINT-2D. Then, w-
   e create an initialized point-2d and list t-
   he x,y,z coordinates with the functions PO-
   INT2D-X and POINT-2D-Y."

  (let* ((point-2d-un-init (point-2d))
	 (point-2d (point-2d x y)))
    (format t "~%Pointer to POINT-2D: ~a~%~%" 
	    point-2d-un-init)
    (format t "POINT-2D (x, y) = (~a, ~a)~%~%" 
	    (point-2d-x point-2d)
	    (point-2d-y point-2d))))



POINT-2F


POINT-2F constructor.


C++: typedef Point_<float> Point2f

LISP-CV:  (POINT-2F (X :INT) (Y :INT)) => POINT-2F

C++: _Tp x, y

LISP-CV: (POINT-2F-X (SELF POINT-2F)) => :INT

C++: _Tp x, y

LISP-CV: (POINT-2F-Y (SELF POINT-2F)) => :INT


    Parameters:	

        SELF - A POINT-2F construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.


POINT-2F creates a 2D point with float coordinates (usually zero-based). Functions POINT-2F-X and POI-
NT2F-Y are used to extract the x,y coordinates the point.


(defun point-2f-example (x y)

  "In this example we create an uninitialized 
   point-2f with the function POINT-2F. Then, w-
   e create an initialized point-2f and list t-
   he x,y coordinates with the functions POIN-
   T2F-X and POINT-2F-Y."

  (let* ((point-2f-un-init (point-2f))
	 (point-2f (point-2f x y)))
    (format t "~%Pointer to POINT-2F: ~a~%~%" 
	    point-2f-un-init)
    (format t "POINT-2F (x, y) = (~a, ~a)~%~%" 
	    (point-2f-x point-2f)
	    (point-2f-y point-2f))))



POINT-3D

when
POINT-3D constructor.


C++: typedef Point3_<double> Point3d

LISP-CV:  (POINT-3D (X :INT) (Y :INT) (Z :INT)) => POINT-3D

C++: _Tp x, y, z

LISP-CV: (POINT-3D-X (SELF POINT-3D)) => :DOUBLE

C++: _Tp x, y, z

LISP-CV: (POINT-3D-Y (SELF POINT-3D)) => :DOUBLE


    Parameters:	

        SELF - A POINT-3D construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.
        
        Z - Z-coordinate of the point.


POINT-3D creates a 3D point with double-float coordinates (usually zero-based). Functions POINT-3D-X, 
POINT-3D-Y AND POINT-3D-Z are used to extract the x,y,Z coordinates the point.


(defun point-3d-example (x y z)

  "In this example we create an uninitialized 
   point-3d with the function POINT-3D. Then, w-
   e create an initialized point-3d and list t-
   he x,y,z coordinates with the functions PO-
   INT3D-X, POINT-3D-Y and POINT-3D-Z."

  (let* ((point-3d-un-init (point-3d))
        (point-3d (point-3d x y z)))
    (format t "~%Pointer to POINT-3D: ~a~%~%" 
	    point-3d-un-init)
    (format t "POINT-3D (x, y, z) = (~a, ~a, ~a)~%~%" 
	    (point-3d-x point-3d)
	    (point-3d-y point-3d)
            (point-3d-z point-3d))))



POINT-3F


POINT-3F constructor.


C++: typedef Point3_<float> Point3f

LISP-CV:  (POINT-3F (X :INT) (Y :INT) (Z :INT)) => POINT-3F

C++: _Tp x, y, z

LISP-CV: (POINT-3F-X (SELF POINT-3F)) => :FLOAT

C++: _Tp x, y, z

LISP-CV: (POINT-3F-Y (SELF POINT)) => :FLOAT


    Parameters:	

        SELF - A POINT-3F construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.
        
        Z - Z-coordinate of the point.


POINT-3F creates a 3D point with float coordinates (usually zero-based). Functions POINT-3F-X, POINT3-
F-Y AND POINT-3F-Z are used to extract the x,y,Z coordinates the point.


(defun point-3f-example (x y z)

  "In this example we create an uninitialized 
   point-3f with the function POINT-3F. Then, w-
   e create an initialized point-3f and list t-
   he x,y,z coordinates with the functions PO-
   INT3F-X, POINT-3F-Y and POINT-3F-Z."

  (let* ((point-3f-un-init (point-3f))
	 (point-3f (point-3f x y z)))
    (format t "~%Pointer to POINT-3F: ~a~%~%" 
	    point-3f-un-init)
    (format t "POINT-3F (x, y, z) = (~a, ~a, ~a)~%~%" 
	    (point-3f-x point-3f)
	    (point-3f-y point-3f)
            (point-3f-z point-3f))))



POINT-3I


POINT-3I constructor.


C++: typedef Point3_<int> Point3i;

LISP-CV:  (POINT-3I (X :INT) (Y :INT) (Z :INT)) => POINT-3I

C++: _Tp x, y, z

LISP-CV: (POINT-3I-X (SELF POINT-3I)) => :INT

C++: _Tp x, y, z

LISP-CV: (POINT-3I-Y (SELF POINT-3I)) => :INT


    Parameters:	

        SELF - A POINT-3I construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.
        
        Z - Z-coordinate of the point.


POINT-3I creates a 3D point with integer coordinates (usually zero-based). Functions POINT-3I-X, POIN-
T3I-Y and POINT-3I-Z are used to extract the x,y,Z coordinates of the point.


(defun point-3i-example (x y z)

  "In this example we create an uninitialized 
   point-3i with the function POINT-3I. Then, w-
   e create an initialized point-3i and list t-
   he x,y,z coordinates with the functions PO-
   INT3I-X, POINT-3I-Y and POINT-3I-Z."

  (let* ((point-3i-un-init (point-3i))
	 (point-3i (point-3i x y z)))
    (format t "~%Pointer to POINT-3I: ~a~%~%" 
	    point-3i-un-init)
    (format t "POINT-3I (x, y, z) = (~a, ~a, ~a)~%~%" 
	    (point-3i-x point-3i)
	    (point-3i-y point-3i)
            (point-3i-z point-3i))))



PROMOTE 

Coverts a MAT to MAT-EXPR

LISP-CV: (PROMOTE (SELF MAT)) => MAT-EXPR

LISP-CV: (<< (SELF MAT)) => MAT-EXPR

The function PROMOTE converts a functions return from MAT to MAT-EXPR. This is useful if you would 
like to do math computation on a matrix with a MAT type using a fast Matrix Expressions(MAT-EXPR) 
function. Some Matrix Expressions functions will only accept a MAT-EXPR type as input and that is 
what makes this function necessary.  You can then convert back to MAT with the function FORCE to use
the result in a function that only accepts a MAT as input i.e. IMSHOW. The function << is an identical 
shorthand version of the PROMOTE function supplied for ease of use. 

   Parameters:	

        SELF - A MAT pointer.


Example:


(defun promote-example ()

  "In this example a matrix filled with ones(MAT) is 
   created. PROMOTE or actually the shorthand versio-
   n of the function PROMOTE << is then used to coer-
   ce MAT to a MAT-EXPR type so it can then be multi-
   plied by the scalar S with the function SCALE. Th-
   is is necessary since SCALE only accepts the MAT-
   EXPR types as it's input. The output of SCALE is 
   then coerced back to MAT type, with the function 
   FORCE, for the opposite reason, so it can then be 
   shown in a window with IMSHOW."

  (let* ((mat (mat-ones 3 3 +8u+))
         (s 255.0d0)
         (out (scale (<< mat) s))
	 (window-name "PROMOTE Example"))
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    (imshow window-name  (>> out))
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))



PTR

Returns a pointer to the specified matrix row.

C++: uchar* Mat::ptr(int i0=0)

LISP-CV: (PTR (SELF MAT) &OPTIONAL ((I0 :INT) 0)) => :POINTER

    Parameters:	

        SELF - A matrix.

        i0 - A 0-based row index.


This function returns a pointer to the specified matrix row.

when

;;Must supply a filename parameter for the image you 
;;will be using in this example and one for the file 
;;the image's pixel value will be written it.
(defun ptr-example (filename-1 filename-2)
  ;;Read in image
  (let* ((img (imread filename-1 1))
	 (window-name-1 "Original image - PTR Example")
	 (window-name-2 "All white image - PTR Example")
         ;;Variables used to hold the 
         ;;BGR image pixel values.
	 (b 0)
         (g 0)
         (r 0)
	 (p 0))
    (if (empty img) 
	(return-from ptr-example 
	  (format t "Image not loaded")))
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)
    (move-window window-name-1 514 175)
    (move-window window-name-2 966 175)
    ;;Access the first BGR pixel value 
    ;;with the function PTR and print it.
    ;;Note: the '?' is a CFFI:MEM-AREF
    ;;macro.
    (setf b (? (ptr img 0) :uchar))
    (setf g (? (ptr img 0) :uchar 1))
    (setf r (? (ptr img 0) :uchar 2))
    (format t "First BGR pixel value = (~a,~a,~a)
~%~% " b g r)
    ;;Access the second BGR pixel value 
    ;;with the function PTR and print it.
    (setf b (? (ptr img 0) :uchar 3))
    (setf g (? (ptr img 0) :uchar 4))
    (setf r (? (ptr img 0) :uchar 5))
    (format t "Second BGR pixel value = (~a,~a,~a)
 ~%~%" b g r)
    ;;Access the third BGR pixel value 
    ;;with the function PTR and print it.
    (setf b (? (ptr img 0) :uchar 6))
    (setf g (? (ptr img 0) :uchar 7))
    (setf r (? (ptr img 0) :uchar 8))
    (format t "Third BGR pixel value = (~a,~a,~a)
 ~%~%" b g r)
    ;;Access all BGR pixel values with the function PTR 
    ;;and print them to a file so you can verify the fi-
    ;;rst 3 BGR values in the file will match the 3 pri-
    ;;nted here.
    (with-open-file (str filename-2
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (dotimes (row (rows img))  
	(dotimes (col (* (cols img) 3))
	  (setf p (? (ptr img row) :uchar col))
	  (format str "~a~%" p))))
    ;;Show original image in a window.
    (imshow window-name-1 img)
    ;;Here we set every pixel of IMG to white, the same
    ;;way we saved every pixel to a file so you can ver-
    ;;if every pixel was included.
    (dotimes (row (rows img))  
      (dotimes (col (* (cols img) 3))
	(setf (? (ptr img row) :uchar col) 255)
	(setf p (? (ptr img row) :uchar col))))
    ;;Show the all white image in a window.
    (imshow window-name-2 img)
    (loop while (not (= (wait-key 0) 27)))
    (del-mat img)
    (destroy-all-windows)))



Usage: 

   (PTR-EXAMPLE "/HOME/W/IMG.JPG" "~/DATA.TXT")



RESHAPE

Changes the shape and/or the number of channels of a 2D matrix without copying the data.

C++: Mat Mat::reshape(int cn, int rows=0) const

LISP-CV: (RESHAPE (SELF MAT) (CN :INT) &OPTIONAL ((ROWS :INT) 0)) => MAT


    Parameters:	

        SELF - A matrix.       

        CN - New number of channels. If the parameter is 0, the number of channels remains the same.

        ROWS - New number of rows. If the parameter is 0, the number of rows remains the same.


The method makes a new matrix header for *this elements. The new matrix may have a different size 
and/or different number of channels. Any combination is possible if:


    No extra elements are included into the new matrix and no elements are excluded. Consequently, 
    the product (* (ROWS) (COLS) (CHANNELS)) must stay the same after the transformation.

    No data is copied. That is, this is an O(1) operation. Consequently, if you change the number of 
    rows, or the operation changes the indices of elements row in some other way, the matrix must be 
    continuous. See (IS-CONTINUOUS).


(defun reshape-example (filename)
  ;;Read in image
  (let* ((img (imread filename 1))
         ;;Allocate matrix data
         (data (alloc :uchar '(1 2 3 4 5 6 7 8 9)))
         ;;Create initialized matrix
         (mat (mat 3 3 +8u+ data))
         (window-name-1 "3x3 Matrix - RESHAPE Example")
         (window-name-2 "1x9 Vector - RESHAPE Example")
	 (window-name-3 "Original image - RESHAPE Example")
         (window-name-4 "1 channel image - RESHAPE Example"))      
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)
    (named-window window-name-3 +window-normal+)
    (named-window window-name-4 +window-normal+)
    (move-window window-name-1 485 98)
    (move-window window-name-2 894 98)
    (move-window window-name-3 485 444)
    (move-window window-name-4 894 444)
    ;;Show original 3x3 matrix
    (imshow window-name-1 mat)
    ;;Show 3x3 matrix reshaped to 1x9 
    (imshow window-name-2 (reshape mat 1 9))
    ;;Show original image
    (imshow window-name-3 img)
    ;;Show 3 channel image reshaped to 1 channel
    (imshow window-name-4 (reshape img 1))
    (loop while (not (= (wait-key 0) 27)))
    (destroy-all-windows)))




ROW

Creates a matrix header for the specified matrix row.

C++: Mat Mat::row(int y) const

LISP-CV: (ROW (Y :INT)) => MAT

    Parameters:	

        Y - A 0-based row index.

The method makes a new header for the specified matrix row and returns it. This is an O(1) operation,
regardless of the matrix size. The underlying data of the new matrix is shared with the original matrix. 
Here is the example of one of the classical basic matrix processing operations, axpy, used by LU and many 
other algorithms:

inline void matrix_axpy(Mat& A, int i, int j, double alpha)
{
    A.row(i) += A.row(j)*alpha;
}

Note: In the current implementation, the following code does not work as expected:

Mat A;
...
A.row(i) = A.row(j); // will not work

This happens because A.row(i) forms a temporary header that is further assigned to another header. Remember that each of these operations is O(1), that is, no data is copied. Thus, the above assignment is not true if you may have expected the j-th row to be copied to the i-th row. To achieve that, you should either turn this simple assignment into an expression or use the Mat::copyTo() method:

Mat A;
...
// works, but looks a bit obscure.
A.row(i) = A.row(j) + 0;

// this is a bit longer, but the recommended method.
A.row(j).copyTo(A.row(i));





ROW-RANGE

Creates a matrix header for the specified row span.

C++: Mat Mat::rowRange(int startrow, int endrow) const

LISP-CV: (ROW-RANGE (SELF MAT) (STARTROW :INT) (ENDROW :INT)) => MAT


    Parameters:	

        SELF - A matrix.

        STARTROW - An inclusive 0-based start index of the row span.

        ENDROW - An exclusive 0-based ending index of the row span.


The method makes a new header for the specified row span of the matrix. Similarly to (ROW) and (COL) 
functions, this is an O(1) operation.


(defun row-range-example ()
        ;Create matrix data
  (let* ((data (alloc :uchar '(1 2 3 4 5 6 7 8 9)))
         ;Create matrix
	 (mat (mat 3 3 +8u+ data)))
    (princ #\Newline)
    ;Print matrix normally by 
    ;accessing entire matrix
    (dotimes (i (rows mat))
      (dotimes (j (cols mat))
	(format t "~a" (at mat i j :uchar))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    ;Retrieve all 3 rows of MAT with 
    ;ROW-RANGE to print matrix
    (dotimes (i (rows mat))
      (dotimes (j (cols mat))
	(format t "~a" (at 
			(row-range mat 0 (rows mat)) 
			i j :uchar))
	(princ #\Space))
      (princ #\Newline))
      (free data)
      (format t "~%")))



ROWS


Returns number or rows in MAT.

C++: int rows, cols

LISP-CV: (ROWS (SELF MAT)) => :INT

    Parameters:	

        SELF - A MAT construct.


The function ROWS finds the number of rows in a matrix or -1 when the array has more than 2 dimensi-
ons. 


(defun rows-example ()

  "Uses ROWS to find the number of rows in the matrix MAT"

  (let* ((mat (mat 3 4 +64f+ (scalar 100)) ))
          (format t "The number of rows in MAT = ~a" (rows mat))))



SIZE

SIZE constructor

C++: Size_(_Tp _width, _Tp _height);
     typedef Size_<int> Size2i;
     typedef Size2i Size;

LISP-CV: (SIZE (WIDTH :INT) (HEIGHT :INT)) => SIZE

C++: _Tp width, height

LISP-CV: (WIDTH (SELF SIZE)) => :INT

C++: _Tp width, height

C: Size* self, other;

   *self = *other;

LISP-CV: (SIZE-ASSGN-TO (SELF SIZE) (OTHER SIZE)) => SIZE


    Parameters:	

        SELF - A SIZE construct.

        WIDTH - The width of SIZE.
        
        HEIGHT - The height of SIZE.


The function SIZE creates and also retrieves size values and stores the values in SIZE construct.

The function WIDTH Finds the width of a SIZE construct.

The function HEIGHT Finds the height of a SIZE construct.


The function SIZE contains the functionality of both the OpenCV class Size_ and the OpenCV MAT class 
method size. It can return a pointer to an uninitialized SIZE construct, an initialized SIZE object 
holding (WIDTH, HEIGHT) values and also determines the SIZE value of any MAT object passed to it, When 
returning a MAT size the columns are listed first and  the rows are listed second(COLS, ROWS). For a tiny
bit faster matrix size accessor choose the MAT-SIZE Dfunction.


(defun size-example ()
  
  "In the code below the (COLS, ROWS) values of MAT are 
   accessed and stored in a SIZE construct. Their value-
   s are accessed with the WIDTH and HEIGHT functions. 
   Then an uninitialized and an initialized SIZE constr-
   uct are created. Their values are also printed."
  
  (let* ((mat (mat 5 5 +8u+ (scalar 100 100 100)))
	 (mat-size (size mat))
	 (size-un-init (size))
	 (size (size 640d0 480d0)))
    ;;The '?' is a macro for CFFI:MEM-AREF
    (format t "~MAT (COLS,ROWS) = (~a ~a)~%~%" 
	    (width mat-size)
	    (height mat-size))
    (format t "Return of an uninitialized SIZE construct: ~a
               ~%" size-un-init) 
    (format t "Width of SIZE = ~a~%" (width size))
    (format t "Height of SIZE = ~a~%~%" (height size))))



SIZE-ASSGN-TO


Create a SIZE object from another SIZE objects data.


C: Size* cv_Size_assignTo(Size* self, Size* other)

LISP-CV: (SIZE-ASSGN-TO (SELF SIZE) (OTHER SIZE)) => SIZE


    Parameters: 

        SELF - A SIZE object

        OTHER - A SIZE object


Example:


LISP-CV> (DEFPARAMETER A (SIZE 640 480))

A

LISP-CV> A

#<CV-SIZE {10042FE323}>

LISP-CV> (DEFPARAMETER B (SIZE))

B

LISP-CV> B

#<CV-SIZE {100431E313}>

LISP-CV> (DEFPARAMETER C (SIZE-ASSGN-TO B A))

C

LISP-CV> C

#<CV-SIZE {1004345E93}>

LISP-CV> (WIDTH C)

640.0d0

LISP-CV> (HEIGHT C)

480.0d0



SIZE-FROM-POINT


Create a SIZE object from POINT data.


C: Size* cv_Size_fromPoint(Point* p)

LISP-CV: (SIZE-FROM-POINT (P POINT)) => SIZE


    Parameters: 

        P - A POINT object


Example:


LISP-CV> (DEFPARAMETER A (POINT 1 2))

A

LISP-CV> (DEFPARAMETER B (SIZE-FROM-POINT A))

B

LISP-CV> (WIDTH B)

1.0d0

LISP-CV> (HEIGHT B)

2.0d0




STEP*

Used to compute address of a matrix element

C++: MStep step

LISP-CV: (STEP (SELF MAT)) => :UNSIGNED-INT

    Parameters:	

        SELF  a pointer to matrix(MAT construc


This function is used to compute the address of a matrix element. The image step gives you the dist-
ance in bytes between the first element of one row and the first element of the next row. This func-
tion is named STEP*, because the name STEP conflicts with a Lisp Macro.


(defun step*-example (filename)
  ;; load image
  (let* ((img (imread filename 1))    (loop while (not (= (wait-key 0) 27)))
	 ;; variables used to access a pixel value.
         ;; BGR - Blue,Green,Red is the default co-
         ;; lor format in LisP-CV.
	 (input (data img))
         ;; variables used to hold the BGR image pixel value
	 (b 0)
	 (g 0)
	 (r 0)
	 (window-name "STEP* Example"))
    (if (empty img) 
	(return-from step*-example 
	  (format t "Image not loaded")))
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    ;; access pixel value at x = 0, y = 0 using the STEP* function
    (setf b (mem-aref input :uchar 
		      (+  (* (step* img) 0) 0)))
    (setf g (mem-aref input :uchar 
		      (+ (+  (* (step* img) 0) 0) 1)))
    (setf r (mem-aref input :uchar 
		      (+ (+  (* (step* img) 0) 0) 2)))
    ;; print the 0,0 pixel value
    (format t "The pixel value at 0,0 is: (~a,~a,~a) " b g r)
    (imshow window-name img)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))



STEP1

Returns a normalized step.


C++: size_t Mat::step1(int i=0 ) const

LISP-CV: (STEP1 (SELF MAT)) => :UNSIGNED-INT


The method returns a matrix step divided by (ELEM-SIZE1) . It can be useful to quickly access an 
arbitrary matrix element.


Example:


LISP-CV> (DEFPARAMETER M (MAT 7 2 +8UC1+))

M

LISP-CV> (ELEM-SIZE M) 

1

LISP-CV> (ELEM-SIZE1 M) 

1

LISP-CV> (STEP1 M)

7

LISP-CV> (DEFPARAMETER M (MAT 7 2 +8UC1+))

M

LISP-CV> (ELEM-SIZE M) 

1

LISP-CV> (ELEM-SIZE1 M) 

1

LISP-CV> (STEP1 M)

7

LISP-CV> (DEFPARAMETER M (MAT 7 2 +32FC1+))

M

LISP-CV> (ELEM-SIZE M) 

4

LISP-CV> (ELEM-SIZE1 M) 

4

LISP-CV> (STEP1 M)

7

LISP-CV> (DEFPARAMETER M (MAT 7 2 +32FC3+))

M

LISP-CV> (ELEM-SIZE M) 

12

LISP-CV> (ELEM-SIZE1 M) 

4

LISP-CV> (STEP1 M)

21



TERM-CRITERIA


TERM-CRITERIA constructors.


C++: TermCriteria::TermCriteria()

LISP-CV: (TERM-CRITERIA) => TERM-CRITERIA

C++: TermCriteria::TermCriteria(int type, int maxCount, double epsilon)

LISP-CV: (TERM-CRITERIA (TYPE :INT) (MAX-COUNT :INT) (EPSILON :INT)) => TERM-CRITERIA



    Parameters:	

        TYPE - The type of termination criteria: +COUNT+, +EPS+ or (+ +COUNT+ +EPS+).  

               Note: The above constants are part of the TermCriteria class in C++. 
               In the OpenCV documentation the above constants are as follows:

               TermCriteria::COUNT, TermCriteria::EPS or TermCriteria::COUNT + TermCriteria::EPS

        MAX-COUNT - The maximum number of iterations or elements to compute.

        EPSILON - The desired accuracy or change in parameters at which the iterative algorithm stops.


Example:

TODO(Reference example of function that uses TERM-CRITERIA)



CORE - OPERATIONS ON ARRAYS:


*ABS

Calculates an absolute value of each matrix element.

C++: MatExpr abs(const Mat& m)

LISP-CV: (*ABS (M MAT)) => MAT-EXPR

    Parameters:	

        M - matrix.

*ABS is a meta-function that is expanded to one of (ABS-DIFF) or (CONVERT-SCALE-ABS) forms:

        (DEFPARAMETER C (*ABS (>> (SUB A B)))) is equivalent to (ABSDIFF A B C)

        (DEFPARAMETER C (*ABS A)) is equivalent to (ABSDIFF A (SCALAR-ALL 0) C)


The output matrix has the same size and the same type as the input one except for the last case, 
where C is (EQ DEPTH +8U+). 


Note: This function is named *ABS instead of ABS because, ABS is the name of a Common Lisp function.


See also:

Matrix Expressions(MAT-EXPR), (ABS-DIFF), (CONVERT-SCALE-ABS)


(defun *abs-example ()

  ;;Allocate data and create a 2x2 matrix.
  (with-object ((data (alloc :float '(4f0 -7f0 2f0 -3f0))))
    (with-mat ((mat (mat 2 2 +32f+ data)))
      ;;Print MAT.
      (format t "~%MAT = ~%~%")
      (print-mat mat :float)
      ;;Find absolute value of all MAT elements.
      (with-mat-expr ((abs-val (*abs mat)))
	(with-mat ((forced-abs-val (>> abs-val)))
	  ;;Print MAT's absolute value.
          (format t "~%The absolute of MAT = ~%~%")
	  (print-mat forced-abs-val :float)
	  (format t "~%"))))))


*EXP

Calculates the exponent of every array element.


C++: void exp(InputArray src, OutputArray dst)

LISP-CV: (*EXP (SRC MAT) (DEST MAT)) => :VOID


    Parameters:	

        SRC - Input array.

        DEST - Output array of the same size and type as SRC.


The function *EXP calculates the exponent of every element of the input array:

See OpenCV documentation:

http://docs.opencv.org/modules/core/doc/operations_on_arrays.html?highlight=log#exp 

for a description and formula.


Note: This function is named *EXP instead of EXP because, EXP is the name of a Common Lisp function.

See also:

(*LOG) , (CART-TO-POLAR) , (POLAR-TO-CART) , (PHASE) , (POW) , (*SQRT) , (MAGNITUDE)


(defun *exp-example (filename)

        ;Create double float matrix data
  (let* ((data (alloc :double '(1d0 2d0 3d0 4d0 5d0 
                               6d0 7d0 8d0 9d0)))
         ;Create double float matrix
	 (mat (mat 3 3 +64f+ data))
	 (window-name-1 "Original Image - *EXP Example")
	 (window-name-2 "Image after CONVERT-TO - *EXP Example")
	 (window-name-3 "Image after LOG - *EXP Example")
	 (window-name-4 "Image after EXP and LOG - *EXP Example")
         (image (imread filename 1))
         (dest (mat 3 3 +64f+)))
      (named-window window-name-1 +window-normal+)
      (named-window window-name-2 +window-normal+)
      (named-window window-name-3 +window-normal+)
      (named-window window-name-4 +window-normal+)
      (move-window window-name-1 485 98)
      (move-window window-name-2 894 98)
      (move-window window-name-3 485 444)
      (move-window window-name-4 894 444)
    (format t "~MAT: ~%~%")
    ;Print MAT
    (dotimes (i (rows mat))
      (dotimes (j (cols mat))
	(format t "~a" (at mat i j :double))
	(princ #\Space))
      (princ #\Newline))
    ;Calculate exponent of each element of 
    ;MAT, using MAT as destination matrix
    (format t "~%Calculate exponents: ~%~%")
    (*exp mat mat)
    (format t "MAT after *EXP: ~%~%")
    ;Print MAT
    (dotimes (i (rows mat))
      (dotimes (j (cols mat))
	(format t "~a" (at mat i j :double))
	(princ #\Space))
      (princ #\Newline))
    ;Calculating natural logarithm of the 
    ;exponent of each matrix element of M-
    ;AT, virtually reverts MAT to it's or-
    ;iginal state
    (format t "~%Calculate natural logarithm: ~%~%")
    (*log mat mat)
    (format t "MAT after *LOG: ~%~%")
    ;Print MAT
    (dotimes (i (rows mat))
      (dotimes (j (cols mat))
	(format t "~a" (at mat i j :double))
	(princ #\Space))
      (princ #\Newline))
    (free data)
    (format t "~%")
    ;Show original IMAGE in window
    (imshow window-name-1 image)
    ;Convert IMAGE to 1 channel
    (cvt-color image image +bgr2gray+)
    ;Convert IMAGE to double float
    ;and show image in window
    (convert-to image image +64f+)
    (imshow window-name-2 image)
    ;Find natural logarithm of 
    ;IMAGE and show in window
    (*log image dest)
    (imshow window-name-3 dest)
    ;Finding natural logarithm of each exponent 
    ;of each element of IMAGE, reverts IMAGE ba-
    ;ck to its original state after CONVERT-TO
    (*exp image image)
    (*log image image)
    ;Show result in window
    (imshow window-name-4 image)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-all-windows)))



*LOG

Calculates the natural logarithm of every array element.

C++: void log(InputArray src, OutputArray dst)

LISP-CV: (*LOG (SRC MAT) (DEST MAT)) => :VOID


    Parameters:	

        SRC - Input array.

        DST - Output array of the same size and type as SRC.


The function *LOG calculates the natural logarithm of the absolute value of every element of the input 
array:

See OpenCV documentation:

http://docs.opencv.org/modules/core/doc/operations_on_arrays.html?highlight=log#log

for a description and formula.


Note: This function is named *LOG instead of LOG because, LOG is the name of a Common Lisp function.


See also:

(*EXP), (CART-TO-POLAR), (POLAR-TO-CART), (PHASE), (POW), (*SQRT), (MAGNITUDE)



(defun *log-example (filename)

	 ;Create double float matrix data
  (let* ((data (alloc :double '(1d0 2d0 3d0 4d0 5d0 
                               6d0 7d0 8d0 9d0)))
         ;Create double float matrix
	 (mat (mat 3 3 +64f+ data))
	 (window-name-1 "Original Image - *LOG Example")
	 (window-name-2 "Natural logarithm of image - *LOG Example")
         (image (imread filename 1)))
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)
    (move-window window-name-1 533 175)
    (move-window window-name-2 984 175)
    (format t "~MAT = ~%~%")
    ;Print MAT
    (dotimes (i (rows mat))
      (dotimes (j (cols mat))
	(format t "~a" (at mat i j :double))
	(princ #\Space))
      (princ #\Newline))
    ;Calculate natural logarithm of each element 
    ;of MAT, using MAT as destination matrix
    (format t "~%Calculate natural logarithm: ~%~%")
    (*log mat mat)
    (format t "MAT = ~%~%")
    ;Print MAT
    (dotimes (i (rows mat))
      (dotimes (j (cols mat))
	(format t "~a" (at mat i j :double))
	(princ #\Space))
      (princ #\Newline))
    (free data)
    (format t "~%")
    ;Show original IMAGE in window
    (imshow window-name-1 image)
    ;Convert IMAGE to 1 channel
    (cvt-color image image +bgr2gray+)
    ;Convert IMAGE to double floatwhen
    (convert-to image image +64f+)
    ;Find natural logarithm of each element of 
    ;IMAGE, just to see what it looks like
    (*log image image)
    ;Show IMAGE
    (imshow window-name-2 image)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-all-windows)))



*MAX

Calculates per-element maximum of two arrays.

C++: void max(InputArray src1, InputArray src2, OutputArray dst)

LISP-CV: (*MAX (SRC1 MAT) (SRC2 MAT) (DEST MAT)) => :VOID    (loop while (not (= (wait-key 0) 27)))


    Parameters:	

        SRC1 - First input array.

        SRC2 - Second input array of the same size and type as SRC1.

        DEST - Output array of the same size and type as SRC1.


The function *MAX calculates the per-element maximum of two arrays. When the input array is multi-channel, 
each channel is compared with value independently.

Note: This function is named *MAX instead of MAX because, MAX is the name of a Common Lisp function.


See also:

(*MIN), (COMPARE), (INRANGE), (MIN-MAX-LOC), Matrix Expressions(MAT-EXPR)


(defun *max-example (&optional (cam 0)
		       (width *default-width*)
		       (height *default-height*))

  "Look at the first window and notice that whatever is black
   in the first window has a beautiful glow in the third wind-
   ow. You can change the effect by altering the color of the 
   matrix MAT-3 in the middle window with the trackbar .The t-
   rackbar changes the scalar value the ASSGN-VAL function us-
   es to decide what to set each element of MAT-3 to."

  ;Create camera capture, CAP, set CAP to default width and height
  (with-captured-camera (cap cam :width width :height height)  
    (let* ((window-name-1 "MAT-3 after THRESHOLD - *MAX-Example")
	   (window-name-2 "MAT-5 after ASSGN-VAL - *MAX-Example")
	   (window-name-3 "MAT-4 after *MAX - *MAX-Example"))
      ;Create two matrices: MAT-1 and MAT-2(used to show how *MAX works)
      (with-mat ((mat-1 (mat 3 3 +32s+ (alloc :int '(1 2 3 4 5 6 7 8 9))))
		 (mat-2 (mat 3 3 +32s+ (alloc :int '(9 8 7 6 5 4 3 2 1))))
		 ;Create destination matrix of same size and type: DEST
		 (dest (mat 3 3 +32s+))
		;Create 3 matrices used to hold 
		;data we use later in the example
		 (mat-3 (mat height width +8u+))
		 (mat-4 (mat height width +8u+))
		 (mat-5 (mat height width +8u+))) 
        ;Create windows and move to specified locations
	(with-named-window (window-name-1 +window-normal+)
	  (with-named-window (window-name-2 +window-normal+)
	    (with-named-window (window-name-3 +window-normal+)
	      (move-window window-name-1 310 175)
	      (move-window window-name-2 760 175)
	      (move-window window-name-3 1210 175)
	      ;Print MAT-1
	      (format t "MAT-1:~%~%")
	      (print-mat mat-1 :int)
	      (format t "~%~%")
	      ;Print MAT-2
	      (format t "MAT-2:~%~%")
	      (print-mat mat-2 :int)
	      (format t "~%~%")
	      ;Find per element maximum of 
	      ;MAT-1 and MAT-2, set to DEST
	      (*max mat-1 mat-2 dest)
	      ;Print DEST
	      (format t "Per element maximum of MAT-1 and  MAT-2:~%~%")
	      (print-mat dest :int)
	      (format t "~%~%")
	      ;Allocate :int pointer for trackbar to change
	      (with-object ((val (alloc :int 67)))
	        ;Create trackbar on middle window which changes 
	        ;the scalar value ASSGN-VAL uses in loop
		(create-trackbar "Value of mat-3" window-name-2 val 255)
		(loop
		   ;Set camera feed to FRAME
		   (with-mat ((frame (mat)))
		     (cap-read cap frame)
		     ;Convert FRAME to 1 channel 
		     ;grayscale image, set to mat-1
		     ;FRAME stays the same
		     (cvt-color frame mat-3 +bgr2gray+)
		     ;Convert FRAME to 1 channel 
		     ;grayscale image, set to MAT-4
		     ;FRAME stays the same
		     (cvt-color frame mat-4  +bgr2gray+)
		     ;Apply a fixed-level threshold to 
		     ;each array element of MAT-3
		     (threshold mat-3 mat-3 128d0 255d0 +thresh-binary-inv+)
		     ;Assign each element of MAT-5 a scalar value
		     (assgn-val mat-5 (scalar (mem-aref val :int)))
		     ;Find the maximum of each element 
		     ;of MAT-4 and MAT-5, set to MAT-4
		     (*max mat-4 mat-5 mat-4)
		     ;Show MAT-3, MAT-5 and MAT-4 in windows
		     (imshow window-name-1 mat-3)
		     (imshow window-name-2 mat-5)
		     (imshow window-name-3 mat-4)
		     (let ((c (wait-key 33)))
		       (when (= c 27)
			 (return)))))))))))))



*MIN

Calculates per-element minimum of two arrays.

C++: void min(InputArray src1, InputArray src2, OutputArray dst)

LISP-CV: (*MIN (SRC1 MAT) (SRC2 MAT) (DEST MAT)) => :VOID


    Parameters:	

        SRC1 - First input array.

        SRC2 - Second input array of the same size and type as SRC1.

        DEST - Output array of the same size and type as SRC1.


The function *MIN calculates the per-element minimum of two arrays. When the input array is multi-channel, 
each channel is compared with value independently.

Note: This function is named *MIN instead of MIN because, MIN is the name of a Common Lisp function.


See also:

(*MAX), (COMPARE), (INRANGE), (MIN-MAX-LOC), Matrix Expressions(MAT-EXPR)



(defun *min-example (&optional (camera-index 0)
		       (width *default-width*)
		       (height *default-height*))

  "Look at the first window and notice that whatever is black
   in the first window has a beautiful glow in the third wind-
   ow. You can change the effect by altering the color of the 
   matrix MAT-3 in the middle window with the trackbar .The t-
   rackbar changes the scalar value the ASSGN-VAL function us-
   es to decide what to set each element of MAT-3 to."

  (with-capture (cap (video-capture camera-index))   
       ;Create two matrices: MAT-1 and MAT-2(used to show how *MIN works)
    (let* ((mat-1 (mat 3 3 +32s+ (alloc :int '(1 2 3 4 5 6 7 8 9))))
	   (mat-2 (mat 3 3 +32s+ (alloc :int '(9 8 7 6 5 4 3 2 1))))
           ;Create destination matrix of same size and type: DEST
           (dest (mat 3 3 +32s+))
           ;Create 3 matrices used to hold 
           ;data we use later in the example
           (mat-3 (mat height width +8u+))
           (mat-4 (mat height width +8u+))
           (mat-5 (mat height width +8u+))
           ;Allocate :int pointer for trackbar to change
           (val (alloc :int '(128)))
	   (window-name-1 "MAT-3 after THRESHOLD - *MIN-Example")
	   (window-name-2 "MAT-5 after ASSGN-VAL - *MIN-Example")
	   (window-name-3 "MAT-4 after *MIN - *MIN-Example")) 
      ;Set CAP to default width and height
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-height+ height)
      ;Create windows and move to specified locations
      (named-window window-name-1 +window-normal+)
      (named-window window-name-2 +window-normal+)
      (named-window window-name-3 +window-normal+)
      (move-window window-name-1 310 175)
      (move-window window-name-2 760 175)
      (move-window window-name-3 1210 175)
      ;Print MAT-1
      (format t "MAT-1:~%~%")
      (dotimes (i (cols mat-1))
	(dotimes (j (rows mat-1))
	  (princ (at mat-1 i j :int))
	  (princ #\Space))
	(princ #\Newline))
      (format t "~%~%")
      ;Print MAT-2
      (format t "MAT-2:~%~%")
      (dotimes (i (cols mat-2))
	(dotimes (j (rows mat-2))
	  (princ (at mat-2 i j :int))
	  (princ #\Space))
	(princ #\Newline))
      (format t "~%~%")
      ;Find per element minimum of 
      ;MAT-1 and MAT-2, set to DEST
      (*min mat-1 mat-2 dest)
      ;Print DEST
      (format t "Per element minimum of MAT-1 and  MAT-2:~%~%")
      (dotimes (i (cols dest))
	(dotimes (j (rows dest))
	  (princ (at dest i j :int))
	  (princ #\Space))
	(princ #\Newline))
      (format t "~%~%")
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
        ;Set camera feed to FRAME
	(setf frame (mat))
	(cap-read cap frame)
        ;Convert FRAME to 1 channel 
        ;grayscale image, set to mat-1
        ;FRAME stays the same
	(cvt-color frame mat-3 +bgr2gray+)
        ;Convert FRAME to 1 channel 
        ;grayscale image, set to mat-4
        ;FRAME stays the same
	(cvt-color frame mat-4  +bgr2gray+)
        ;Apply a fixed-level threshold to 
        ;each array element of mat-3
	(threshold mat-3 mat-3 128d0 255d0 +thresh-binary-inv+)
        ;Create trackbar on middle window which changes 
        ;the scalar value ASSGN-VAL uses in the next step
        (create-trackbar "Value of mat-3" window-name-2 val 255)
        ;Assign each element of mat-5 a scalar value
	(assgn-val mat-5 (scalar (mem-aref val :int)))
        ;Find the minimum of each element 
        ;of mat-4 AND mat-5, set to mat-4
        (*min mat-4 mat-5 mat-4)
        ;Show mat-3, mat-5 and mat-4 in windows
	(imshow window-name-1 mat-3)
	(imshow window-name-2 mat-5)
	(imshow window-name-3 mat-4)) 
      (destroy-all-windows))))



ABSDIFF

Calculates the per-element absolute difference between two arrays or between an array and a scalar.

C++: void absdiff(InputArray src1, InputArray src2, OutputArray dst)

LISP-CV: (ABSDIFF (SRC1 MAT) (SRC2 MAT) (DEST MAT))

    Parameters:	

        SRC1 - first input array or a scalar.

        SRC2 - second input array or a scalar.

        DEST - output array that has the same size and type as input arrays.

The function absdiff calculates:

        Absolute difference between two arrays when they have the same size and type.

        Absolute difference between an array and a scalar when the second array is constructed from
        Scalar or has as many elements as the number of channels in SRC1:

        Absolute difference between a scalar and an array when the first array is constructed from 
        Scalar or has as many elements as the number of channels in SRC2:

        where I is a multi-dimensional index of array elements. In case of multi-channel arrays, ea-
        ch channel is processed independently.

Note:

Saturation is not applied when the arrays have the depth +32S+. You may even get a negative value i-
n the case of overflow.

See also:

(ABS) 


(defun absdiff-example (&optional 
			  (camera-index 
			   *camera-index*) 
			  (width *default-width*)
			  (height *default-height*))

  "The function ABSDIFF calculates the per-element absolute 
   difference between FRAME(the camera stream) and SCALAR a-
   nd outputs the result to a window...Makes for quite an i-
   nteresting effect."

  (with-capture (cap (video-capture camera-index))
    (let ((scalar (mat 1 1 +64f+ (scalar 128 128 128)))
	  (window-name "ABSDIFF Example"))
      (if (not (cap-is-open cap)) 
	  (return-from absdiff-example 
	    (format t "Cannot open the video camera")))
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-height+ height)
      (named-window window-name +window-normal+)
      (move-window window-name 759 175)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(absdiff frame scalar frame)
	(imshow window-name frame))
      (destroy-window window-name))))



ADD-WEIGHTED

Calculates the weighted sum of two arrays.

C++: void addWeighted(InputArray src1, double alpha, InputArray src2, double beta, double gamma, OutputArray dst, int dtype=-1)

LISP-CV: (ADD-WEIGHTED (SRC1 MAT) (ALPHA :DOUBLE) (SRC2 MAT) (BETA :DOUBLE) (GAMMA :DOUBLE) (DEST MAT) &OPTIONAL ((DTYPE :INT) -1)) 
          => :VOID

    Parameters:	

        SRC1 - First input array.

        ALPHA - Weight of the first array elements.

        SRC2 - Second input array of the same size and channel number as SRC1.

        BETA - Weight of the second array elements.

        DEST - Output array that has the same size and number of channels as the input arrays.

        GAMMA- Scalar added to each sum.

        DTYPE - Optional depth of the output array; when both input arrays have the same depth, DTYPE 
                can be set to -1, which will be equivalent to (DEPTH SRC1).


The function ADD-WEIGHTED calculates the weighted sum of two arrays as follows:


See OpenCV documentation for a description and formula:

http://docs.opencv.org/modules/core/doc/operations_on_arrays.html?highlight=addwe#addweighted


See also:

(ADD), (SUBTRACT), (SCALE-ADD), (CONVERT-TO), Matrix Expressions(MAT-EXPR)



(defun add-weighted-example (filename1 filename2)

  "Try using the <lisp-cv-source-directory>/ubuntu-logo.jpg 
   and the <lisp-cv-source-directory>/blue.jpg to get a nic-
   e effect on this example."

  (let ((window-name "Linear Blend - ADD-WEIGHTED Example"))
    (with-named-window (window-name +window-normal+)
      (set-window-property window-name +wnd-prop-fullscreen+ 
			   +window-fullscreen+)
      (set-window-property window-name +wnd-prop-aspectratio+ 
			   +window-freeratio+)
      (with-object ((alpha (alloc :int 62))
		    (beta (alloc :int 175))
		    (gamma (alloc :int 0)))
	(create-trackbar "Alpha" window-name alpha 150)
	(create-trackbar "Beta" window-name beta 1500)
	(create-trackbar "Gamma" window-name gamma 215)
	(with-mat ((src1 (imread filename1))
		   (src2 (imread filename2))
		   (dest (mat)))
	  (if (empty src1) 
	      (return-from add-weighted-example 
		(format t "~%Error loading SRC1~%~%")))
	  (if (empty src2) 
	      (return-from add-weighted-example 
		(format t "~%Error loading SRC2~%~%")))
	  (loop
	     (add-weighted src1 (coerce (/ (? alpha :int) 100) 'double-float) 
                           src2 (coerce (/  (? beta :int) 100) 'double-float) 
			   (coerce (? gamma :int) 'double-float) dest)
	     (imshow window-name dest)
	     (let ((c (wait-key 33)))
	       (when (= c 27)
		 (return)))))))))



BITWISE-AND

Calculates the per-element bit-wise conjunction of two arrays.

C++: void bitwise_and(InputArray src1, InputArray src2, OutputArray dst, InputArray mask=noArray())

LISP-CV: (BITWISE-AND (SRC1 MAT) (SRC2 MAT) (DEST MAT) &OPTIONAL ((MASK MAT) (MAT))) => :VOID


    Parameters:	

        SRC1 - First input array.

        SRC2 - Second input array.

        DEST - Output array that has the same size and type as the input arrays.

        MASK - Optional operation mask, 8-bit single channel array, that 
               specifies elements of the output array to be changed.


The function calculates the per-element bit-wise logical conjunction for two arrays when SRC1 and SRC2 
have the same size. In case of floating-point arrays, their machine-specific bit representations (usually 
IEEE754-compliant) are used for the operation. In case of multi-channel arrays, each channel is processed 
independently.


(defun bitwise-and-example (filename-1 filename-2)

  "Calculates the per-element bit-wise conjunction of two 
   images.

   Note: You are encouraged to use the Black and White.png 
   and the White and Black.png in the LISP-CV images direc-
   tory to get the full effect of this example."

  (let* ((image-1 (imread filename-1 1))
	 (image-2 (imread filename-2 1))
	 (dest (mat (rows image-1) (cols image-1) +8uc3+))
	 (window-name-1 "IMAGE-1 - BITWISE-AND Example")
	 (window-name-2 "IMAGE-2 - BITWISE-AND Example")
	 (window-name-3 "DEST - BITWISE-AND Example"))
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)
    (named-window window-name-3 +window-normal+)
    (move-window window-name-1 310 175)
    (move-window window-name-2 760 175)
    (move-window window-name-3 1210 175)
    (bitwise-and image-1 image-2 dest)
    (imshow window-name-1 image-1)
    (imshow window-name-2 image-2)
    (imshow window-name-3 dest)
    (loop while (not (= (wait-key 0) 27)))
    (del-mat image-1)
    (del-mat image-2)
    (del-mat dest)
    (destroy-all-windows)))



BITWISE-NOT

Inverts every bit of an array.

C++: void bitwise_not(InputArray src, OutputArray dst, InputArray mask=noArray())

LISP-CV: (BITWISE-NOT (SRC MAT) (DEST MAT) &OPTIONAL ((MASK MAT) (MAT))) => :VOID


    Parameters:	

        SRC - Input array.

        DEST - Output array that has the same size and type as the input array.

        MASK - Optional operation mask, 8-bit single channel array, that 
               specifies elements of the output array to be changed.


The function calculates per-element bit-wise inversion of the input array. In case of a floating-point 
input array, its machine-specific bit representation (usually IEEE754-compliant) is used for the operation. 
In case of multi-channel arrays, each channel is processed independently.


(defun bitwise-not-example (filename-1 filename-2)

  "Inverts every bit of IMAGE-1 and IMAGE-2.

   Note: You are encouraged to use the Black and White.png 
   in the LISP-CV images directory and an image of your ow-
   n choosing to get the full effect of this example."

  (let* ((image-1 (imread filename-1 1))
	 (image-2 (imread filename-2 1))
	 (dest-1 (mat (rows image-1) (cols image-1) +8uc3+))
         (dest-2 (mat (rows image-2) (cols image-2) +8uc3+))
	 (window-name-1 "IMAGE-1 - BITWISE-NOT Example")
	 (window-name-2 "IMAGE-2 - BITWISE-NOT Example")
	 (window-name-3 "DEST-1 - BITWISE-NOT Example")
	 (window-name-4 "DEST-2 - BITWISE-NOT Example"))
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)
    (named-window window-name-3 +window-normal+)
    (named-window window-name-4 +window-normal+)
    (move-window window-name-1 485 98)
    (move-window window-name-2 894 98)
    (move-window window-name-3 485 444)
    (move-window window-name-4 894 444)
    (bitwise-not image-1 dest-1)
    (bitwise-not image-2 dest-2)
    (imshow window-name-1 image-1)
    (imshow window-name-2 dest-1)
    (imshow window-name-3 image-2)
    (imshow window-name-4 dest-2)
    (loop while (not (= (wait-key 0) 27)))
    (del-mat image-1)
    (del-mat image-2)
    (del-mat dest-1)
    (del-mat dest-2)
    (destroy-all-windows)))



BITWISE-OR

Calculates the per-element bit-wise disjunction of two arrays.

C++: void bitwise_or(InputArray src1, InputArray src2, OutputArray dst, InputArray mask=noArray())

LISP-CV: (BITWISE-OR (SRC1 MAT) (SRC2 MAT) (DEST MAT) &OPTIONAL ((MASK MAT) (MAT))) => :VOID

when
    Parameters:	

        SRC1 - First input array.

        SRC2 - Second input array.

        DEST - Output array that has the same size and type as the input arrays.

        MASK - Optional operation mask, 8-bit single channel array, that 
               specifies elements of the output array to be changed.


The function calculates the per-element bit-wise logical disjunction for two arrays when SRC1 and SRC2 
have the same size. In case of floating-point arrays, their machine-specific bit representations (usually 
IEEE754-compliant) are used for the operation. In case of multi-channel arrays, each channel is processed 
independently.


(defun bitwise-or-example (filename-1 filename-2)

  "Calculates the per-element bit-wise disjunction of two 
   images.

   Note: You are encouraged to use the Black and White.png 
   and the White and Black.png in the LISP-CV images direc-
   tory to get the full effect of this example."

  (let* ((image-1 (imread filename-1 1))
	 (image-2 (imread filename-2 1))
	 (dest (mat (rows image-1) (cols image-1) +8uc3+))
	 (window-name-1 "IMAGE-1 - BITWISE-OR Example")
	 (window-name-2 "IMAGE-2 - BITWISE-OR Example")
	 (window-name-3 "DEST - BITWISE-OR Example"))
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)
    (named-window window-name-3 +window-normal+)
    (move-window window-name-1 310 175)
    (move-window window-name-2 760 175)
    (move-window window-name-3 1210 175)
    (bitwise-or image-1 image-2 dest)
    (imshow window-name-1 image-1)
    (imshow window-name-2 image-2)
    (imshow window-name-3 dest)
    (loop while (not (= (wait-key 0) 27)))
    (del-mat image-1)
    (del-mat image-2)
    (del-mat dest)
    (destroy-all-windows)))



BITWISE-XOR

Calculates the per-element bit-wise “exclusive or” operation on two arrays.

C++: void bitwise_xor(InputArray src1, InputArray src2, OutputArray dst, InputArray mask=noArray())

LISP-CV: (BITWISE-XOR (SRC1 MAT) (SRC2 MAT) (DEST MAT) &OPTIONAL (MASK MAT)) => :VOID

    Parameters:	

        SRC1 - First input array.

        SRC2 - Second input array.

        DEST - Output array that has the same size and type as the input arrays.

        MASK - Optional operation mask, 8-bit single channel array, that 
               specifies elements of the output array to be changed.


The function calculates the per-element bit-wise logical “exclusive-or” operation for two arrays 
when SRC1 and SRC2 have the same size. In case of floating-point arrays, their machine-specific bit 
representations (usually IEEE754-compliant) are used for the operation. In case of multi-channel 
arrays, each channel is processed independently.


(defun bitwise-xor-example (filename-1 filename-2)

  "Calculates the per-element bit-wise “exclusive or” oper-
   ation of two images.

   Note: You are encouraged to use the Black and White.png 
   and the White and Black.png in the LISP-CV images direc-
   tory to get the full effect of this example."

  (let* ((image-1 (imread filename-1 1))
	 (image-2 (imread filename-2 1))
	 (dest (mat (rows image-1) (cols image-1) +8uc3+))
	 (window-name-1 "IMAGE-1 - BITWISE-XOR Example")
	 (window-name-2 "IMAGE-2 - BITWISE-XOR Example")
	 (window-name-3 "DEST - BITWISE-XOR Example"))
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)
    (named-window window-name-3 +window-normal+)
    (move-window window-name-1 310 175)
    (move-window window-name-2 760 175)
    (move-window window-name-3 1210 175)
    (bitwise-xor image-1 image-2 dest)
    (imshow window-name-1 image-1)
    (imshow window-name-2 image-2)
    (imshow window-name-3 dest)
    (loop while (not (= (wait-key 0) 27)))
    (del-mat image-1)
    (del-mat image-2)
    (del-mat dest)
    (destroy-all-windows)))



CONVERT-SCALE-ABS

Scales, calculates absolute values, and converts the result to 8-bit.

C++: void convertScaleAbs(InputArray src, OutputArray dst, double alpha=1, double beta=0)

LISP-CV: (CONVERT-SCALE-ABS  (SRC MAT) (DEST MAT) &OPTIONAL ((ALPHA :DOUBLE) 1.0D0) ((BETA :DOUBLE) 0.0D0)) => :VOID

    Parameters:	

        SRC - input array.

        DEST - output array.

        ALPHA - optional scale factor.

        BETA - optional delta added to the scaled values.

On each element of the input array, the function CONVERT-SCALE-ABS performs three operations sequentially: 
scaling, taking an absolute value, conversion to an unsigned 8-bit type. In case of multi-channel arrays, 
the function processes each channel independently. 



(defun convert-scale-abs-example (&optional (cam *camera-index*) 
				    (width *default-width*)
				    (height *default-height*))

  (with-captured-camera (cap cam :width width :height height)
    (if (not (cap-is-open cap)) 
	(return-from convert-scale-abs-example 
	  (format t "Cannot open the video camera")))  
    (let ((window-name "CONVERT-SCALE-ABS Example"))
      (with-named-window (window-name +window-normal+)
	(move-window window-name 759 175)
	;;Create a matrix
	(with-mat ((mat (mat-ones 6 4 +32f+)))
	  ;;Print original type of MAT
	  (format t "~%MAT type before conversion = ~a(or +32f+)~%~%" 
		  (mat-type mat))
	  ;;Print original MAT before conversion
	  (format t "Printing MAT before the conversion~%~%")
	  (print-mat mat :float)
	  ;;CONVERT-SCALE-ABS, scales by 2 and 
	  ;;adds 5, to every elemement of MAT,
          ;;and converts the result to 8-bit
	  (convert-scale-abs mat mat 2d0 5d0)
	  ;;Print converted MAT type which is 8-bit
	  (format t "~%MAT type after conversion = ~a(or +8u+)" 
		  (mat-type mat))
	  ;;Print converted MAT
	  (format t "~%~%Printing MAT after the conversion~%~%")
	  (print-mat mat :uchar)
          (format t "~%~%")
	  (loop
	     (with-mat ((frame (mat)))
	       (cap-read cap frame)
	       ;;Run CONVERT-SCALE-ABS on the camera
               ;;output, just to see what happens
	       (convert-scale-abs frame frame 2d0 5d0)
	       (imshow window-name frame))
	     (let ((c (wait-key 33)))
	       (when (= c 27)
		 (return)))))))))



DET

Returns the determinant of a square floating-point matrix.

C++: double determinant(InputArray mtx)

LISP-CV: (DET (MTX MAT)) => :DOUBLE

    Parameters:	

        MYX - Input matrix that must have +32FC1+ or +64FC1+ type and square size.
 

The function determinant calculates and returns the determinant of the specified matrix. For small 
matrices, the direct method is used. For larger matrices, the function uses LU factorization with 
partial pivoting.

For symmetric positively-determined matrices, it is also possible to use (EIGEN) decomposition to 
calculate the determinant.

See also:

(TRACE), (INVERT), (SOLVE), (EIGEN), Matrix Expressions(MAT-EXPR)


(defun det-example ()
	;Create matrix data.
  (let* ((data-1 (alloc :float '(1f0 2f0 3f0 4f0 5f0 6f0 5f0 7f0 9f0)))
	 (data-2 (alloc :float '(4f0 5f0 6f0 6f0 5f0 4f0 4f0 6f0 5f0)))
	;Create matrix with zero determinant.
         (zero-det-mat (mat 3 3 +32f+ data-1))
	;Create matrix with non-zero determinant.
         (mat (mat 3 3 +32f+ data-2))      
	 (zero-det-mat-inv 0)
         (mat-inv 0))
	;Print MAT.
    (format t "MAT =~%~%")
    (dotimes (i (rows mat))
      (dotimes (j (cols mat))
	(princ (at mat i j :float))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
	;Print ZERO-DET-MAT.
    (format t "ZERO-DET-MAT =~%~%")
    (dotimes (i (rows zero-det-mat))
      (dotimes (j (cols zero-det-mat))
	(princ (at zero-det-mat i j :float))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
	;Check if the determinant of ZERO-DET-MAT is 0. 
	;It is not, so an inverse cannot be determined.
    (format t "The determinant of ZER0-DET-MAT = ~a~%~%" (det zero-det-mat))
	;Check if the determinant of MAT is 0. It 
	;is, so an inverse can be determined.
    (format t "The determinant of MAT = ~a~%~%" (det mat))
	;Find inverse of ZERO-DET-MAT and 
        ;print it. It will be all zeros.
    (setf zero-det-mat-inv (>> (inv zero-det-mat +decomp-lu+)))
    (format t "The inverse of ZERO-DET-MAT =~%~%")
    (dotimes (i (rows zero-det-mat-inv))
      (dotimes (j (cols zero-det-mat-inv))
	(princ (at zero-det-mat-inv i j :float))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
	;Find inverse of MAT and print it.
    (setf mat-inv (>> (inv mat +decomp-lu+)))
    (format t "The inverse of MAT =~%~%")
    (dotimes (i (rows mat-inv))
      (dotimes (j (cols mat-inv))
	(princ (at mat-inv i j :float))
	(princ #\Space))
      (princ #\Newline))
      (princ #\Newline)
      (free data-1)
      (free data-2)))




DIVIDE

Performs per-element division of two arrays or a scalar by an array.


C++: void divide(InputArray src1, InputArray src2, OutputArray dst, double scale=1, int dtype=-1)

LISP-CV: (DIVIDE (SRC1 MAT) (SRC2 MAT) (DEST MAT) &OPTIONAL ((SCALE :DOUBLE) 1) ((DTYPE :INT) -1)) => :VOID


					;"/home/w/Pictures/100_0229.JPG"  <---money mike



;Global variables



;Number of file to be saved
(defparameter filenumber 0)
;Name of file to be saved 
(defparameter filename 0) 
(defparameter crop (mat))



(defun detect-and-draw (img cascade nested-cascade scale)

  (let ((num-buffers 2)
        (size-factor 1.1d0)

C++: void divide(double scale, InputArray src2, OutputArray dst, int dtype=-1)

LISP-CV: (DIVIDE (SCALE :DOUBLE) (SRC2 MAT) (DEST MAT) &OPTIONAL ((DTYPE :INT) -1)) => :VOID


    Parameters:	

        SRC1 - First input array.

        SRC2 - Second input array of the same size and type as SRC2.

        SCALE - Scalar factor.

        DST - Output array of the same size and type as SRC2.

        DTYPE - Optional depth of the output array; if -1, DEST will have depth (DEPTH SRC2), but in 
                case of an array-by-array division, you can only pass -1 when (EQ (DEPTH SRC1) (DEPTH SRC2)).


See OpenCv documenetation:

http://docs.opencv.org/trunk/modules/core/doc/operations_on_arrays.html?highlight=divide#divide

for a description and formulae:


See also:

(MULTIPLY), (ADD), (SUBTRACT), Matrix Expressions(MAT-EXPR)


(defun divide-example ()

  "In this example, DIVIDE is used to find 
   the per element quotient of M1 and M2,
   Then DIVIDE is used to find the per ele-
   ment quotient of a scalar and a new M1."

  (with-scalar ((scalar (scalar 5)))
    (with-mat ((m1 (mat-ones 3 3 +32f+))
	       (m2 (assgn-val (mat-ones 3 3 +32f+) scalar)))
      (with-mat ((result (mat 3 3 +32f+)))
	(divide m1 m2 result)
	(format t "~%M1 = ~%~%")
	(print-mat m1 :float)
	(format t "~%~%M2 = ~%~%")
	(print-mat m2 :float)
	(format t "~%RESULT ~%~%")
	(print-mat result :float)
	(format t "~%")))
    (with-mat ((m1 (assgn-val 
		    (mat-ones 3 3 +32f+) 
		    scalar))
	       (result (mat 3 3 +32f+)))
      (divide 7d0 m1 result)
      (format t "~%M1 = ~%~%")
      (print-mat m1 :float)
      (format t "~%RESULT ~%~%")
      (print-mat result :float)
      (format t "~%"))))


FLIP

Flips a 2D array around vertical, horizontal, or both axes.

C++: void flip(InputArray src, OutputArray dst, int flipCode)

LISP-CV: (FLIP (SRC MAT) (DEST MAT) (FLIP-CODE :INT)) => :VOID

    Parameters:	

        SRC - Input array.

        DEST - Output array of the same size and type as SRC.

        FLIP-CODE - A flag to specify how to flip the array; 0 means flipping around the x-axis and
                    positive value (for example, 1) means flipping around y-axis. Negative value 
                    (for example, -1) means flipping around both axes (see the discussion below for 
                    the formulas).

The function FLIP flips the array in one of three different ways (row and column indices are 0-based).

The example scenarios of using the function are the following:

        Vertical flipping of the image (EQ FLIP-CODE 0) to switch between top-left and bottom-left 
        image origin. This is a typical operation in video processing on Microsoft Windows* OS.

        Horizontal flipping of the image with the subsequent horizontal shift and absolute difference 
        calculation to check for a vertical-axis symmetry (> FLIP-CODE 0).

        Simultaneous horizontal and vertical flipping of the image with the subsequent shift and absolute 
        difference calculation to check for a central symmetry (< FLIP-CODE  0).

        Reversing the order of point arrays ((> FLIP-CODE 0) OR (EQ FLIP-CODE 0)).

See also:

(TRANSPOSE) , (REPEAT) , (COMPLETE-SYMM)


(defun flip-example (&optional (camera-index *camera-index*))
  ;;Capture camera feed
  (with-capture (cap (video-capture camera-index))
    ;;Make array of window names
    (let* ((window-name-arr 
	    (make-array 6 :initial-contents 

			(list
                         "Matrix flipped on both axes - FLIP Example"
			 "Matrix flipped on x-axis - FLIP Example"
			 "Matrix flipped on y-axis - FLIP Example"
                         "Camera output flipped on both axes - FLIP Example"
			 "Camera output flipped on x-axis - FLIP Example"
			 "Camera output flipped on y-axis - FLIP Example")))
	   ;;Allocate matrix data
	   (data (alloc :uchar '(1 2 3 4 5 6 7 8 9)))
	   ;;Create a data matrix, MAT
	   (mat (mat 3 3 +8u+ data))    (loop while (not (= (wait-key 0) 27)))
           ;;Create array of MAT clones
           (mat-clone-arr (make-array 3 :initial-contents 
				      (list 
				       (clone mat) (clone mat) (clone mat)))))
      ;;Create array of windows
      (dotimes (i 6)
	(named-window (aref window-name-arr i) +window-normal+))
      ;;Move windows to specified locations
      (move-window (aref window-name-arr 0) 288 150)
      (move-window (aref window-name-arr 1) 738 150)
      (move-window (aref window-name-arr 2) 1188 150)
      (move-window (aref window-name-arr 3) 288 518)
      (move-window (aref window-name-arr 4) 738 518)
      (move-window (aref window-name-arr 5) 1188 515)
      ;;Flip the first, second and third MAT clones we created 
      ;;around the x, y and both axes, respectively
      (dotimes (i 3)
	(flip (aref mat-clone-arr i) (aref mat-clone-arr i) (- i 1)))
      ;;Show the first, second and third 
      ;;MAT clones in the top windows
      (dotimes (i 3)
	(imshow (aref window-name-arr i) (aref mat-clone-arr i)))
      ;;Initialize variable FRAME to hold camera feed
      (do ((frame 0)
	   ;;Initialize array of FRAME clones
	   (frame-clone-arr (make-array 3 :initial-contents '(0 0 0))))
	  ((plusp (wait-key *millis-per-frame*)) 
	   (format t "Key is pressed by user"))
        ;;Assign camera feed to FRAME
	(setf frame (mat))
	(cap-read cap frame)
        ;;Make 3 frame clones
	(dotimes (i 3)
	  (setf (aref frame-clone-arr i) (clone frame)))
        ;;Flip the first, second and third FRAME clones we 
        ;;created around the x, y and both axes respectively
	(dotimes (i 3)
	  (flip (aref frame-clone-arr i) (aref frame-clone-arr i) (- i 1)))
        ;;Show all FRAME clones in windows
	(dotimes (i 3)
	  (imshow (aref window-name-arr (+ i 3)) (aref frame-clone-arr i)))
	;;Clean up used memory
	(dotimes (i 3)
	  (del-mat (aref frame-clone-arr i))))
      (destroy-all-windows))))



IN-RANGE-S

Checks if array elements lie between the elements of two other arrays.

C++: void inRange(InputArray src, InputArray lowerb, InputArray upperb, OutputArray dst)

LISP-CV: (IN-RANGE-S (SRC MAT) (LOWERB SCALAR) (UPPERB SCALAR) (DEST MAT)) => :VOID


    Parameters:	

        SRC - First input array.

        LOWERB - A scalar.

        UPPERB - A scalar.

        DEST - Output array of the same size as SRC and +8U+ type.


All the arrays must have the same type, except the destination, and the same size (or ROI size).


(defun in-range-s-example (&optional 
			     (cam *camera-index*) 
			     (width *default-width*)
			     (height *default-height*))

  ;; Set camera feed to CAP and set camera feed width/height
  (with-captured-camera (cap cam :width width :height height)  
    (let ((window-name-1 "Original camera feed - IN-RANGE-S Example")
	  (window-name-2 "Only red objects - IN-RANGE-S Example"))
      (if (not (cap-is-open cap)) 
	  (return-from in-range-s-example 
	    (format t "Cannot open the video camera")))
      (format t "~%Frame Size : ~ax~a~%~%" 
	      (cap-get cap +cap-prop-frame-width+)
	      (cap-get cap +cap-prop-frame-height+))
      (with-named-window (window-name-1 +window-normal+)
	(with-named-window (window-name-2 +window-normal+)
	  (move-window window-name-1 533 175)
	  (move-window window-name-2 984 175)
	  ;; Iterate through each frames of the video
	  (loop
	     ;; Set camera feed to FRAME
	     (with-mat ((frame (mat)))
	       (cap-read cap frame)
	       (with-mat ((src (clone frame))
			  (img-hsv (mat))
			  (img-thresh (mat)))
		 (with-scalar ((lower-hsv (scalar 170 160 60))
			       (upper-hsv (scalar 180 2556 256)))
		   ;; Smooth the original image using Gaussian kernel
		   (gaussian-blur src src (size 5 5) 0.0d0 0.0d0)
		   ;; Change the color format from BGR to HSV
		   (cvt-color src img-hsv +bgr2hsv+)
		   ;; Threshold the HSV image and create a binary image
		   (in-range-s img-hsv lower-hsv upper-hsv img-thresh)
		   ;; Smooth the binary image using Gaussian kernel
		   (gaussian-blur img-thresh img-thresh (size 5 5) 0.0d0 0.0d0)
		   (imshow window-name-1 src)
		   (imshow window-name-2 img-thresh)))
	       (let ((c (wait-key 33)))
		 (when (= c 27)
		   (return))))))))))



INVERT

Finds the inverse or pseudo-inverse of a matrix.

C++: double invert(InputArray src, OutputArray dst, int flags=DECOMP_LU)

LISP-CV: (INVERT (SRC MAT) (DEST MAT) &OPTIONAL ((FLAGS :INT) +DECOMP-LU+))) => :DOUBLE

    Parameters:	

        SRC - Input floating-point M x N matrix.

        DEST - Output matrix of N x M size and the same type as SRC.

        flags -

        Inversion method :

            +DECOMP-LU+ Gaussian elimination with the optimal pivot element chosen.

            +DECOMP-SVD+ singular value decomposition (SVD) method.

            +DECOMP-CHOLESKY+ Cholesky decomposition; the matrix must 
                              be symmetrical and positively defined.


The function INVERT inverts the matrix SRC and stores the result in DEST . When the matrix SRC is 
singular or non-square, the function calculates the pseudo-inverse matrix (the DEST matrix) so that 
(NORM (- (* SRC DST) I)) is minimal, where I is an identity matrix.

In case of the +DECOMP-LU+ method, the function returns non-zero value if the inverse has been successfully 
calculated and 0 if SRC is singular.


In case of the +DECOMP-SVD+ method, the function returns the inverse condition number of SRC (the 
ratio of the smallest singular value to the largest singular value) and 0 if SRC is singular. The 
SVD method calculates a pseudo-inverse matrix if SRC is singular.

Similarly to +DECOMP-LU+ , the method +DECOMP-CHOLESKY+ works only with non-singular square matrices 
that should also be symmetrical and positively defined. In this case, the function stores the inverted 
matrix in DEST and returns non-zero. Otherwise, it returns 0.

See also:

(SOLVE), SVD


(defun invert-example (&optional 
			 (camera-index *camera-index*)) 

  (with-capture (cap (video-capture camera-index))
    ;;Allocate matrix data and create a square matrix
    (let* ((data (alloc :float '(4f0 -7f0 2f0 -3f0)))
	   (mat (mat 2 2 +32f+ data))
           ;;Create a destination matrix 
           ;;the same size/type as MAT
	   (dest-1 (mat 2 2 +32f+))
	   (invert-return 0)
	   (identity-mat 0)
           ;;Create an array of window names
	   (window-name-arr 
	    (make-array 6 :initial-contents 
			(list
                         "Original MAT - INVERT Example"
			 "Inverted MAT - INVERT Example"
                         "IDENTITY-MAT - INVERT Example"
                         "Camera output - 1 channel float - INVERT Example"
			 "Camera output inverted - INVERT Example"
			 "Identity MAT - INVERT Example"))))
      (if (not (cap-is-open cap)) 
	  (return-from invert-example 
	    (format t "Cannot open the video camera")))
      ;;Create array of windows
      (dotimes (i 6)
	(named-window (aref window-name-arr i) +window-normal+))
      ;;Move windows to specified locations
      (move-window (aref window-name-arr 0) 288 150)
      (move-window (aref window-name-arr 1) 738 150)
      (move-window (aref window-name-arr 2) 1188 150)
      (move-window (aref window-name-arr 3) 288 518)
      (move-window (aref window-name-arr 4) 738 518)
      (move-window (aref window-name-arr 5) 1188 515)
      ;;Find inverse of MAT
      (setf invert-return (invert mat dest-1 +decomp-lu+))
      ;;Print the return of INVERT(if non-zero the operation was successful)
      (format t "Return from Invert Function = ~a~%" invert-return)
      ;;Print the inverse of MAT
      (dotimes (i (cols dest-1))
	(dotimes (j (rows dest-1))
	  (princ (at dest-1 i j :float))
	  (princ #\Space))
	(princ #\Newline))
      (format t "~%~%")
      ;;Multiply MAT by its inverse. If the inversion was 
      ;;a success the output will be an identity matrix
      (setf identity-mat (>> (mul mat dest-1))) 
      ;;Operation was a success!, print the identity matrix
      (dotimes (i (cols identity-mat))
	(dotimes (j (rows identity-mat))
	  (princ (at identity-mat i j :float))
	  (princ #\Space))
	(princ #\Newline))
      (format t "~%~%")
      ;;Show MAT, its inverse and the identity 
      ;;matrix in the top windows
      (imshow (aref window-name-arr 0) mat)
      (imshow (aref window-name-arr 1) dest-1)
      (imshow (aref window-name-ar
r 2) identity-mat)
      (do* ((frame 0)
	    (dest-2 0)
            (roi 0)
            (result 0)
            (n (coerce (/ 1 255) 'double-float)))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	;;Read in camera feed, call it FRAME
	(setf frame (mat))
	(cap-read cap frame)
        ;;Make a clone of the camera feed, DEST-2
	(setf dest-2 (clone frame))
        ;;Crop FRAME to make it a square matrix, set to ROI
        (setf roi (roi frame (rect 0 0 (rows frame) (rows frame))))
        ;;Convert ROI to 1 channel image
	(cvt-color roi roi +bgr2gray+)
        ;;Convert ROI to a float(+32F+) matrix
        (convert-to roi roi +32f+ n)
        ;;Show the float matrix in bottom-left window
       	(imshow (aref window-name-arr 3) roi)
        ;;Find the inverse of ROI and show it in 
        ;;bottom-center window, just for fun
        (invert roi dest-2 +decomp-lu+)
	(imshow (aref window-name-arr 4) dest-2)
        ;;Find if inverse was a success by multiplying the 
        ;;original ROI by the output of the INVERT function. 
        ;;Then show the resulting identity matrix in the 
        ;;bottom-right window
	(setf result (mul roi dest-2))
	(setf identity-mat (>> result))
 	(imshow (aref window-name-arr 5) identity-mat)
        ;;Clean up used memory
	(del-mat roi)
        (del-mat dest-2)
	(del-mat identity-mat)
        (del-mat-expr result))
      (destroy-all-windows))))



MEAN

Calculates an average (mean) of array elements.

C++: Scalar mean(InputArray src, InputArray mask=noArray())

LISP-CV: (MEAN (SRC MAT) &OPTIONAL ((MASK (:POINTER (MAT)) (MAT)))) => SCALAR


    Parameters:	

        SRC - Input array that should have from 1 to 4 channels so that the result can be stored in SCALAR.

        MASK - Optional operation mask.

The function mean calculates the mean value M of array elements, independently for each channel, and 
returns it. When all the mask elements are 0’s, the function returns (SCALAR-ALL 0) .

See also:

(COUNT-NON-ZERO), (MEAN-STD-DEV), (NORM), (MIN-MAX-LOC)


Example:


(defun mean-example (&optional (cam *camera-index*) 
		       (width *default-width*)
		       (height *default-height*))

  "Position the rectangle in the window by moving the trackbar 
   sliders. The rectangle gets its color from the averaging of 
   the pixels in the region of the rectangle. This averaging is
   calculated by the function MEAN. For example if you position 
   the rectangle over some thing red, the rectangle will turn a 
   shade of red. The rectangle starts at 0,0 X,Y coordinates."

  (with-captured-camera (cap cam :width width :height height)
    (let* ((window-name "IMG - MEAN Example")
           (n 10))
      ;;Initialize the rectangle location/
      ;;dimension variables
      (with-rect ((rect-x (alloc :int '(0)))
		  (rect-y (alloc :int '(0)))
		  (rect-width (alloc :int (list (round (/ width n)))))
		  (rect-height (alloc :int (list (round (/ height n))))))      
	;;Create fullscreen window
	(with-named-window (window-name +window-autosize+)
	  (set-window-property window-name +wnd-prop-fullscreen+ 
			       +window-fullscreen+)
	  (move-window window-name 624 100)
	  (loop
	     (with-mat ((frame (mat)))
	       ;;Set FRAME to a frame of the camera feed
	       (cap-read cap frame)
	       ;;Print rectangle location/dimensions
	       (format t "RECT-X: ~a~%~%" (mem-ref rect-x :int))
	       (format t "RECT-Y: ~a~%~%" (mem-ref rect-y :int))
	       (format t "RECT-WIDTH: ~a~%~%" (mem-ref rect-width :int))
	       (format t "RECT-HEIGHT: ~a~%~%" (mem-ref rect-height :int))
	       ;;Create trackbars to control the rectangle location/dimensions
	       (create-trackbar "RECT-X" window-name rect-x width)
	       (create-trackbar "RECT-Y" window-name rect-y height)
	       (create-trackbar "RECT-WIDTH" window-name rect-width width)
	       (create-trackbar "RECT-HEIGHT" window-name rect-height height)
	       ;;Instantiate logic for the location/dimensions 
	       ;;of the rectangle based on the trackbar input
	       (if (equal (mem-ref rect-x :int) 0) 
		   (setf (mem-ref rect-x :int) 0))
	       (if (> (mem-ref rect-x :int) 
		      (- width (mem-ref rect-width :int))) 
		   (setf (mem-ref rect-x :int) 
			 (- width (mem-ref rect-width :int))))
	       (if (equal (mem-ref rect-y :int) 0) 
		   (setf (mem-ref rect-y :int) 1))
	       (if (> (mem-ref rect-y :int) 
		      (- height (mem-ref rect-height :int))) 
		   (setf (mem-ref rect-y :int) 
			 (- height (mem-ref rect-height :int))))
	       (if (< (mem-ref rect-width :int) 1) 
		   (setf (mem-ref rect-width :int) 1))
	       (if (< (mem-ref rect-height :int) 1) 
		   (setf (mem-ref rect-height :int) 1))
	       ;;Create an empty matrix
	       (with-mat ((img (mat)))
		 ;;Make a copy of FRAME, IMG, to use 
		 ;;for the fullscreen camera output
		 (copy-to frame img)
		 ;;Set position parameters of the RECTANGLE we will create, 
		 ;;that will be the color of the mean of the pixels in FRAME, 
		 ;;to that of the position of the region of interest of FRAME
		 (with-point ((point-1 (point (mem-ref rect-x :int) 
					      (mem-ref rect-y :int)))
			      (point-2 (point (+ (mem-ref rect-x :int) 
						 (mem-ref rect-width :int)) 
					      (+ (mem-ref rect-y :int) 
						 (mem-ref rect-height :int)))))
		 ;;Set region of interest of FRAME to the rectangle 
		 ;;location/dimensions we specified
		 (with-rect ((roi (rect (mem-ref rect-x :int) (mem-ref rect-y :int)
					(mem-ref rect-width :int) (mem-ref rect-height :int))))
		   ;;Set region of interest of FRAME to ROI. This region of 
		   ;;interest is the where we find the mean of the pixels. 
		   (with-mat ((frame (roi frame roi)))
		   ;;Find mean of FRAME and set to 
		   ;;COLOR parameter of RECTANGLE
		  (with-scalar ((color (mean frame (mat))))
		     ;;Create a rectangle the color of 
		     ;;the mean of the pixels it covers
		     (rectangle img point-1 point-2 color +filled+ 4 0)
		 (imshow window-name img)))))))
    	     (let ((c (wait-key 33)))
	       (when (= c 27)
		 (return)))))))))


MIN-MAX-LOC

Finds the global minimum and maximum in an array.

C++: void minMaxLoc(InputArray src, double* minVal, double* maxVal=0, Point* minLoc=0, Point* maxLoc=0, InputArray mask=noArray())

LISP-CV: (MIN-MAX-LOC (SRC MAT) (MIN-VAL :DOUBLE) &OPTIONAL ((MAX-VAL :DOUBLE) (NULL-POINTER)) ((MIN-LOC POINT) (NULL-POINTER)) 
        ((MAX-LOC POINT) (NULL-POINTER)) ((MASK MAT) (MAT))) :VOID

    Parameters:	

        SRC - Input single-channel array.

        MIN-VAL - Pointer to the returned minimum value; (NULL-POINTER) is used if not required.

        MAX-VAL - Pointer to the returned maximum value; (NULL-POINTER) is used if not required.

        MIN-LOC - Pointer to the returned minimum location (in 2D case); (NULL-POINTER) is used if not required.

        MAX-LOC - Pointer to the returned maximum location (in 2D case); (NULL-POINTER) is used if not required.

        MASK - Optional mask used to select a sub-array.


The functions MIN-MAX-LOC find the minimum and maximum element values and their positions. The extremums 
are searched across the whole array or, if mask is not an empty array, in the specified array region.

The functions do not work with multi-channel arrays. If you need to find minimum or maximum elements across 
all the channels, use (RESHAPE) first to reinterpret the array as single-channel. Or extract the particular 
channel using either (EXTRACT-IMAGE-COI), or (MIX-CHANNELS), or (SPLIT).

See also

(MAX), (MIN), (COMPARE), (IN-RANGE), (EXTRACT-IMAGE-COI), (MIX-CHANNELS), (SPLIT), (RESHAPE)


(defun min-max-loc-example (filename &optional (cam 0))

  "Printing the 'checkerboard.png' image in the LISP-CV images 
   directory to use for the object you want to track and using 
   the 'resized-checkerboard.png' for the template image gives
   a nice effect in this example. It is very basic, so you'll
   have to play with the distance you hold the object from the 
   camera to get a good track and then move the object slowly
   Probably good to be in a well lighted room."

  (with-captured-camera (cap cam :width 640 :height 480) 
	  ;Create windows
    (let* ((window-name-1 "TPL - MIN-MAX-LOC Example")
	   (window-name-2 "FRAME - MIN-MAX-LOC Example")
	   (window-name-3 "MATCHES - MIN-MAX-LOC Example")
	   ;Initialize size parameters 
	   ;for the matches
           (iwidth 0)
	   (iheight 0))      
      ;;Create windows
      (with-named-window (window-name-1 +window-autosize+)
	(with-named-window (window-name-2 +window-autosize+) 
	  (with-named-window (window-name-3 +window-autosize+) 
	    ;Move windows to specified locations     
	    (move-window window-name-1 325 0)
	    (move-window window-name-2 325 260)
	    (move-window window-name-3 969 260)
	    (with-point ((minloc (point))
			 (maxloc (point)))
	      (with-object ((minval (alloc :double 0d0))
			    (maxval (alloc :double 0d0)))
		;Set rectangle color
		(with-scalar ((color (scalar 0 0 255)))
                  ;Load template image
		  (with-mat ((tpl (imread filename 1)))
		    (loop
		       ;Set camera feed to FRAME
		       (with-mat ((frame (mat)))
			 (cap-read cap frame)
			 (setf iwidth (+ (- (cols frame) (cols tpl)) 1))
			 (setf iheight (+ (- (rows frame) (rows tpl)) 1))
			 ;Create  matrix to hold all of the matches
			 (with-mat ((matches (mat iheight iwidth +32f+)))
			  ;Run MATCH-TEMPLATE on each frame of the camera 
                          ;feed and run NORMALIZE on each match 
			   (match-template frame tpl matches +tm-ccoeff-normed+)
			   (normalize matches matches 0d0 1d0 +norm-minmax+)
                           ;Run MIN-MAX-LOC to set point 
                           ;coordinates for each match
			   (min-max-loc matches minval maxval minloc maxloc)
			   (rectangle frame (gc:point (point-x minloc) (point-y minloc)) 
				      (gc:point (+ (point-x minloc) (cols tpl)) 
					     (+ (point-y minloc) (rows tpl))) 
				      color 10 0 0)
			   ;Show image, template and matches in a window
			   (imshow window-name-1 tpl)
			   (imshow window-name-2 frame)
			   (imshow window-name-3 matches)
			   (let ((c (wait-key 33)))
			     (when (= c 27)
			       (return))))))))))))))))



MULTIPLY

Calculates the per-element scaled product of two arrays.

C++: void multiply(InputArray src1, InputArray src2, OutputArray dst, double scale=1, int dtype=-1 )

LISP-CV: (MULTIPLY (SRC1 MAT) (SRC2 MAT) (DEST MAT) &OPTIONAL ((SCALE :DOUBLE) 1) 
         ((DTYPE :INT) -1)) => :VOID


    Parameters:	

        SRC1 - First input array.

        SRC2 - Second input array of the same size and the same type as SRC1.

        DEST - Output array of the same size and type as SRC1.

        SCALE - Optional scale factor.
  
        DTYPE - Destination matrix type, default is -1 i.e. (EQ (MAT-TYPE DEST) (MAT-TYPE SRC))
                You can change DTYPE to a double float(+64F+) to achieve higher precision).


The function multiply calculates the per-element product of two arrays. There is also a Matrix 
Expressions (MAT-EXPR) variant of this function. See (MUL). For a not-per-element matrix product, 
see (GEMM).

Note:

Saturation is not applied when the output array has the depth +32S+. You may even get result of an 
incorrect sign in the case of overflow.

See also:

(ADD), (SUBTRACT), (DIVIDE), MAT-EXPR, (SCALE-ADD), (ADD-WEIGHTED), (ACCUMULATE), (ACCUMULATE-PRODUCT), 
(ACCUMULATE-SQUARE), (CONVERT-TO)


(defun multiply-example ()
  ;;Allocate int, double float and unsigned char matrix data
  (let* ((int-data (alloc :int '(1 2 3 4 5 6 7 8 9)))
	 (double-data (alloc :double '(1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0 9d0)))
	 (uchar-data (alloc :uchar '(1 2 3 4 5 6 7 8 9)))
	 ;;Create 2 identical src matrices and 
	 ;;1 dest matrix for each data type
	 (int-mat-1 (mat 3 3 +32s+ int-data))
         (int-mat-2 (mat 3 3 +32s+ int-data))
         (dest1 (mat))
	 (double-mat-1 (mat 3 3 +64f+ double-data))
         (double-mat-2 (mat 3 3 +64f+ double-data))
         (dest2 (mat))
	 (uchar-mat-1 (mat 3 3 +8u+ uchar-data))
         (uchar-mat-2 (mat 3 3 +8u+ uchar-data))
         (dest3 (mat)))
    ;;Multiply int matrix by identical matrix using 
    ;;a default dtype(destination type) parameter
    (multiply int-mat-1 int-mat-2 dest1 1d0 -1)
    ;;Multiply double float matrix by identical matrix using 
    ;;a dtype parameter of 6(+64F+) or double float type
    (multiply double-mat-1 double-mat-2 dest2 1d0 6)
    ;;Multiply uchar matrix by identical matrix using 
    ;;a default dtype parameter and a scalar, 2d0
    (multiply uchar-mat-1 uchar-mat-2 dest3 2d0 -1)
    ;;Print results
    (format t "~%")
    (dotimes (i (rows dest1))
      (dotimes (j (cols dest1))    (loop while (not (= (wait-key 0) 27)))
	(format t "~a" (at dest1 i j :int))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    (dotimes (i (rows dest2))
      (dotimes (j (cols dest2))
	(format t "~a" (at dest2 i j :double))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    (dotimes (i (rows dest3))
      (dotimes (j (cols dest3))
	(format t "~a" (at dest3 i j :uchar))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%")
    ;;Clean up used memory
    (free int-data)
    (free double-data)
    (free uchar-data)))




NORM

Calculates an absolute array norm, an absolute difference norm, or a relative difference norm.

C++: double norm(InputArray src1, int normType=NORM_L2, InputArray mask=noArray())

LISP-CV: (NORM (SRC1 MAT) &OPTIONAL ((NORM-TYPE :INT) +NORM-L2+) ((MASK MAT) (MAT))) => :DOUBLE

C++: double norm(InputArray src1, InputArray src2, int normType=NORM_L2, InputArray mask=noArray() )

LISP-CV: (NORM (SRC1 MAT) (SRC2 MAT) &OPTIONAL ((NORM-TYPE :INT) +NORM-L2+) ((MASK MAT) (MAT))) => :DOUBLE


    Parameters:	

        SRC1 - First input array.

        SRC2 - Second input array of the same size and the same type as SRC1.

        NORM-TYPE - Type of the norm (see the details below).

        MASK - Optional operation mask; it must have the same size as SRC1 and +8UC1+ type.

The functions NORM calculate an absolute norm of SRC1 (when there is no SRC2):

See OpenCV documentation:

http://docs.opencv.org/trunk/modules/core/doc/operations_on_arrays.html?highlight=norm#norm

for a description and formulae


(defun norm-example (filename &optional (cam 0))

  "This example is an improvement on the way MIN-MAX-LOC tracks 
   an object in the MIN-MAX-LOC example, in a couple ways. Firs
   t, if you use the 'resized-checkerboard.png' in the Lisp-CV 
   IMAGES directory, you'll notice the result of MIN-MAX-LOC is 
   better defined in the MATCHES window as a result of NORM bei-
   ng used instead of NORMALIZE. You'll still have to play with 
   the distance you hold the object from the camera to get a go-
   od track. However, you can improve the objects track if you 
   adjusting the Brightness and Contrast sliders. Also, if you 
   look at the MATCHES window while trying to get a track, you 
   notice the visual clues will aid you toward that endeavor."

  (with-captured-camera (cap cam :width 640 :height 480) 
					;Create windows
    (let* ((window-name-1 "TEMPL - NORM Example")
	   (window-name-2 "FRAME - NORM Example")
	   (window-name-3 "Move Trackbars to change Brightness and Contrast - NORM Example")
	   (window-name-4 "MATCHES - NORM Example")
					;Initialize size parameters 
					;for the matches
           (iwidth 0)
	   (iheight 0)
	   (brightness 0)
	   (contrast 0))      
      ;;Create windows
      (with-named-window (window-name-1 +window-autosize+)
	(with-named-window (window-name-2 +window-autosize+) 
	  (with-named-window (window-name-3 +window-autosize+) 
	    (with-named-window (window-name-4 +window-autosize+) 
	      ;Move windows to specified locations     
	      (move-window window-name-1 250 29)
	      (move-window window-name-2 99 260)
	      (move-window window-name-3 743 260)
	      (move-window window-name-4 1387 260)
	      (with-point ((minloc (point))
			   (maxloc (point)))
		(with-object ((minval (alloc :double 0d0))
			      (maxval (alloc :double 0d0))
			      (val1 (alloc :int 50))
			      (val2 (alloc :int 50)))
		  ;Set rectangle color
		  (with-scalar ((color (scalar 0 0 255)))
		    ;Load template image
		    (with-mat ((templ (imread filename 1))
			       (dest (mat)))
		      (create-trackbar "Brightness" window-name-3 val1 100)
		      (create-trackbar  "Contrast" window-name-3 val2 100)
		      (loop
			 ;Set camera feed to FRAME
			 (with-mat ((frame (mat)))
			   (cap-read cap frame)
			   ;Set brightness and contrast values 
			   ;based on trackbar input
			   (setf brightness (- (mem-ref val1 :int) 50))
			   (setf contrast (/ (mem-ref val2 :int) 50))
			   (copy-to frame dest)
			   (convert-to frame dest -1 (coerce contrast 'double-float)  
				       (coerce brightness 'double-float))
			   (setf iwidth (+ (- (cols frame) (cols templ)) 1))
			   (setf iheight (+ (- (rows frame) (rows templ)) 1))
			   ;Create  matrix to hold all of the matches
			   (with-mat ((matches (mat iheight iwidth +32f+)))
			     ;Run MATCH-TEMPLATE on each frame of the camera 
			     ;feed and run NORM on each match 
			     (match-template dest templ matches +tm-ccoeff-normed+)
			     (norm matches matches +norm-l2+)
			     ;Run MIN-MAX-LOC to set point 
			     ;coordinates for each match
			     (min-max-loc matches minval maxval minloc maxloc)
			     (rectangle frame (gc:point (point-x minloc) (point-y minloc)) 
					(gc:point (+ (point-x minloc) (cols templ)) 
						  (+ (point-y minloc) (rows templ))) 
					color 10 0 0)
			     ;Show image, template and matches and DEST in a window
			     (imshow window-name-1 templ)
			     (imshow window-name-2 frame)
			     (imshow window-name-3 dest)
			     (imshow window-name-4 matches)
			     (let ((c (wait-key 33)))
			       (when (= c 27)
				 (return)))))))))))))))))



NORMALIZE

Normalizes the norm or value range of an array.

C++: void normalize(InputArray src, OutputArray dst, double alpha=1, double beta=0, int norm_type=NORM_L2, int dtype=-1, 
     InputArray mask=noArray())

LISP-CV: (NORMALIZE (SRC MAT) (DEST MAT) &OPTIONAL ((ALPHA :DOUBLE) 1) ((BETA :DOUBLE) 0) 
         ((NORM-TYPE :INT) +NORM-L2+) ((DTYPE :INT) -1) ((MASK MAT) (MAT))) => :VOID 

    Parameters:	

        SRC - Input array.

        DEST - Output array of the same size as SRC.

        ALPHA - Norm value to normalize to or the lower range boundary in case of the range normalization.

        BETA - Upper range boundary in case of the range normalization; it is not used for the norm 
               normalization.

        NORM-TYPE - Normalization type (see the details below).

        DTYPE - When negative, the output array has the same type as SRC; otherwise, it has the same 
                number of channels as SRC and the depth =CV_MAT_DEPTH(dtype)(internal, See OpenCV source 
                code at: <opencv_source_directory>/modules/core/include/opencv2/core/cvdef.h

        MASK - Optional operation mask.


See OpenCV documentation:

http://docs.opencv.org/modules/core/doc/operations_on_arrays.html?highlight=normalize#normalize

for a description and formulae.

See also:

(NORM), (CONVER-TO)

Example:

See MATCH-TEMPLATE-EXAMPLE for example usage of NORMALIZE.



POW

Raises every array element to a power.

C++: void pow(InputArray src, double power, OutputArray dst)

LISP-CV: (POW (SRC MAT) (POWER :DOUBLE) (DEST MAT)) => :VOID


    Parameters:	

        SRC - Input array.

        POWER - Exponent of power.

        DEST - Output array of the same size and type as SRC.


See OpenCV documentation:

http://docs.opencv.org/modules/core/doc/operations_on_arrays.html?highlight=pow#pow

for description and formula.


See also:

(*SQRT), (*EXP), (*LOG), (CART-TO-POLAR), (POLAR-TO-CART)




RANDU

Generates a single uniformly-distributed random number or an array of random numbers.

C++: void randu(InputOutputArray dst, InputArray low, InputArray high)

LISP-CV: (RANDU (DEST MAT) (LOW SCALAR) (HIGH SCALAR)) => :VOID


    Parameters:	

        DEST - output array of random numbers; the array must be pre-allocated.

        LOW - inclusive lower boundary of the generated random numbers.

        HIGH - exclusive upper boundary of the generated random numbers.


This function fills the matrix dst with uniformly-distributed random numbers from the specified range.

See also:

(RNG), (RANDN), (THE-RNG)


(defun randu-example ()
  (let* ((data (alloc :float '(1.
0f0 2.0f0 3.0f0 4.0f0 5.0f0 
			       6.0f0 7.0f0 8.0f0 9.0f0)))
	 (m (mat 3 3 +32f+ data)))
    (format t "Print matrix M:~%~%")
    (dotimes (i (rows m))
      (dotimes (j (cols m))
	(format t "~a" (at m i j :float))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%")
    (format t "*Fill matrix M with random values with RANDU*~%~%")
    (randu m (scalar -100) (scalar 100))
    (format t "Print matrix M again:~%~%")
    (dotimes (i 3)
      (dotimes (j 3)
	(format t "~a" (at m i j :float))
	(princ #\Space))
      (princ #\Newline))
    (princ #\Space)
    (free data)))


RNG

The constructors

C++: RNG::RNG()

LISP-CV: (RNG) => (:POINTER RNG)

C++: RNG::RNG(uint64 state)

LISP-CV: (RNG (STATE :UINT64)) => (:POINTER RNG)

    Parameters:	

        STATE - 64-bit value used to initialize the RNG.


These are the RNG constructors. The first form sets the state to some pre-defined value, equal to 
2**32-1 in the current implementation. The second form sets the state to the specified value. If 
you passed STATE = 0 , the constructor uses the above default value instead to avoid the singular 
random number sequence, consisting of all zeros.


Example:


LISP-CV> (DEFPARAMETER RNG (RNG #XFFFFFFFF))

RNG

LISP-CV> (UNIFORM RNG 0 10)

6

LISP-CV> (UNIFORM RNG 0D0 10D0)

6.992592005760082d0

LISP-CV> (UNIFORM RNG 0F0 10F0)

3.1438508



SCALE-ADD

Calculates the sum of a scaled array and another array.

C++: void scaleAdd(InputArray src1, double alpha, InputArray src2, OutputArray dst)

LISP-CV: (SCALE-ADD (SRC1 MAT) (ALPHA :DOUBLE) (SRC2 MAT) (DEST MAT)) => :VOID


    Parameters:	

        SRC1 - First input matrix.

        ALPHA - Scale factor for the first matrix.

        SRC2 - Second input matrix of the same size and type as SRC1.

        DEST - Output matrix of the same size and type as SRC1.


The function SCALE-ADD is one of the classical primitive linear algebra operations, known as DAXPY 
or SAXPY in BLAS. It calculates the sum of a scaled matrix and another matrix.


See also:

(ADD), (ADD-WEIGHTED), (SUBTRACT), (DOT), (CONVERT-TO), Matrix Expressions(MAT-EXPR)


(defun scale-add-example (filename-1 filename-2)

  "Calculates the sum of a scaled IMAGE-1 and IMAGE-2 
   and shows the result in the third window. Moving t-
   he trackabar changes the scalar value used to scal-
   e IMAGE-1. 

   Using the Bear Drawing.jpg and Bear Painting.jpg i-
   n the LISP-CV images directory for IMAGE-1 and IMA-
   GE-2 respectively will give a nice effect in this 
   example."

  (let* ((image-1 (imread filename-1 1))
	 (image-2 (imread filename-2 1))
	 (dest (mat (rows image-1) (cols image-1) +8uc3+))
	 (alpha-val (alloc :int '(0))) 
	 (window-name-1 "IMAGE-1 - SCALE-ADD Example")
	 (window-name-2 "IMAGE-2 - SCALE-ADD Example")
	 (window-name-3 "DEST - SCALE-ADD Example"))
    (if (and (empty image-1)
             (empty image-2)) 
	(return-from scale-add-example 
	  (format t "Images were not loaded")))
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)
    (named-window window-name-3 +window-normal+)
    (move-window window-name-1 310 175)
    (move-window window-name-2 760 175)
    (move-window window-name-3 1210 175)
    (do* ((n 0))
	 ((plusp (wait-key *millis-per-frame*)) 
	  (format t "Key is pressed by user"))
      (create-trackbar "ALPHA-VAL" window-name-1 alpha-val 100)
      (format t "ALPHA-VAL = ~a~%~%" 
	      (setf n (coerce (mem-aref alpha-val :int) 
			      'double-float)))
      (scale-add image-1 n image-2 dest)
      (imshow window-name-1 image-1)
      (imshow window-name-2 image-2)
      (imshow window-name-3 dest))
    (free alpha-val)
    (destroy-all-windows)))



SUBTRACT

Calculates the per-element difference between two arrays or array and a scalar.

C++: void subtract(InputArray src1, InputArray src2, OutputArray dst, InputArray mask=noArray(), int dtype=-1)

LISP-CV: (SUBTRACT (SRC1 MAT) (SRC2 MAT) (DEST MAT) &OPTIONAL ((MASK MAT) (MAT)) 
         ((DTYPE :INT) -1) => :VOID


    Parameters:	

        SRC1 - First input array or a scalar.

        SRC2 - Second input array or a scalar.

        DST - Output array of the same size and the same number of channels as the input arrays.

        MASK - Optional operation mask; this is an 8-bit single channel array that specifies elements 
               of the output array to be changed.

        DTYPE - Optional depth of the output array (see the details below).


See OpenCV documentation:

http://docs.opencv.org/modules/core/doc/operations_on_arrays.html?highlight=log#subtract

for description and formulae.

See also:

(ADD), (ADD-WEIGHTED), (SCALE-ADD), (CONVERT-TO), Matrix Expressions(MAT-EXPR)


(defun subtract-example (&optional (camera-index 0) 
			   (width *default-width*)
			   (height *default-height*))

  (with-capture (cap (video-capture camera-index))
    (let* ((window-name "Frame Subtract - SUBTRACT Example")
	   (last-frame (mat height width +8uc3+))
	   (dest (mat height width +8uc3+)))
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-height+ height)
      (named-window window-name)
      (move-window window-name 610 225)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(subtract frame last-frame dest)
	(imshow window-name dest)
	(copy-to frame last-frame))
      (destroy-window window-name))))



SUM

Calculates the sum of array elements.

C++: Scalar sum(InputArray src)

LISP-CV: (SUM (SRC MAT)) => SCALAR


    Parameters:	SUM - Input array that must have from 1 to 4 channels.


The functions sum calculate and return the sum of array elements, independently for each channel.


See also:

(COUNT-NON-ZERO), (MEAN), (MEAN-STD-DEV), (NORM), (MIN-MAX-LOC), (REDUCE)


(defun sum-example ()
        ;Create matrix
  (let* ((mat (mat 4 4 +8u+))
         ;Initialize random number generator
         (rng (rng #xFFFFFFFF)))
    (format t "~%")
    ;Fill MAT with random values
    (dotimes (i (rows mat))
      (dotimes (j (cols mat))
	(setf (at mat i j :uchar) (uniform rng 1 8))))
     ;Print MAT
    (dotimes (i (rows mat))
      (dotimes (j (cols mat))
	(format t "~a" (at mat i j :uchar))
	(princ #\Space))
      (princ #\Newline))
      (format t "~%")
    ;Assign each each element 
    ;of MAT the sum of its ra-
    ;ndom elements
    (assgn-val mat (sum mat))
    ;Print MAT
    (dotimes (i (rows mat))
      (dotimes (j (cols mat))
	(format t "~a" (at mat i j :uchar))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%")))



UNIFORM

Returns the next random number sampled from the uniform distribution.

C++: int RNG::uniform(int a, int b)

LISP-CV: (UNIFORM (RNG (:POINTER RNG)) (A :DOUBLE) (B :DOUBLE)) => :DOUBLE

C++: float RNG::uniform(float a, float b)

LISP-CV: (UNIFORM (RNG (:POINTER RNG)) (A :FLOAT) (B :FLOAT)) => :FLOAT

C++: double RNG::uniform(double a, double b)

LISP-CV: (UNIFORM (RNG (:POINTER RNG)) (A :INT) (B :INT)) => :INT


    Parameters:	

        RNG - An RNG object

        A - lower inclusive boundary of the returned random numbers.

        B - upper non-inclusive boundary of the returned random numbers.


The methods transform the state using the MWC algorithm and return the next uniformly-distributed r-
andom number of the specified type, deduced from the input parameter type, from the range (a, b) . 
There is a nuance illustrated by the following example:


(defparameter rng (rng))

;; always produces 0
(defparameter a (uniform rng 0 1))

;; produces double from (0, 1)
(defparameter a1 (uniform rng 0d0 1d0))

;; produces float from (0, 1)
(defparameter b (uniform-f rng 0.0f0 1.0f0))

;; may cause compiler error because of ambiguity:
(defparameter d (uniform rng 0 .999999))



DRAWING FUNCTIONS:


GET-TEXT-SIZE


Calculates the width and height of a text string.


C++: Size getTextSize(const string& text, int fontFace, double fontScale, int thickness, int* baseLine)

LISP-CV:  (GET-TEXT-SIZE (TEXT *STRING) (FONT-FACE :INT) (FONT-SCALE :DOUBLE) (THICKNESS :INT) (BASE-LINE :POINTER)) 
           => SIZE


    Parameters:	

        TEXT - Input text string.

        FONT-FACE - Font to use. See (PUT-TEXT) for details.

        FONT-SCALE - Font scale. See (PUT-TEXT) for details.

        THICKNESS - Thickness of lines used to render the text. See (PUT-TEXT) for details.

        BASE-LINE - Output parame
ter - y-coordinate of the baseline relative to the bottom-most text 
                    point.


The function GET-TEXT-SIZE calculates and returns the size of a box that contains the specified text. 
That is, the following code renders some text, the tight box surrounding it, and the baseline:


(defun get-text-example ()
  (let* ((window-name "GET-TEXT Example")
	 (text "Funny text inside the box")
	 (font-face +font-hershey-script-simplex+)
	 (font-scale 2d0)
	 (thickness 3)
	 (img (mat 600 800 +8uc3+ (scalar-all 0)))
	 (base-line (alloc :int 0))
	 (text-size 0)
	 (text-org 0))
    ;; create a window
    (named-window window-name +window-normal+)
    ;; set window to fullscreen with stretched aspect ratio
    (set-window-property window-name +wnd-prop-fullscreen+ 
			 +window-fullscreen+)
    (if (equal 1.0d0 (get-window-property window-name +wnd-prop-fullscreen+)) 
	(set-window-property window-name +wnd-prop-aspectratio+ 
			     +window-freeratio+))
    ;; '?' is a macro for CFFI:MEM-AREF, basically 'base-line += thickness'
    (setf (? base-line :int) thickness)
    ;; calculates the width and height of TEXT.
    (setf text-size (get-text-size text font-face font-scale thickness base-line))
    ;; center the text
    (setf text-org (point (round (/ (- (cols img) (width text-size)) 2)) 
			  (round (/ (- (rows img) (height text-size)) 2))))
    ;; draw the box
    (rectangle img (point (point-x text-org) (round 
					      (+ (point-y text-org) 
						 (? base-line :int))))
	       (point (round 
		       (+ (point-x text-org) (width text-size))) 
		      (round 
		       (- (point-y text-org) (height text-size))))
	       (scalar 0 0 255))
    ;; ... and the baseline first
    (line img (point (point-x text-org) (round (+ (point-y text-org) thickness)))
	  (point (+ (point-x text-org) (round (width text-size))) 
		 (+ (point-y text-org) thickness))
	  (scalar 0 0 255))
    ;; then put the text itself
    (put-text img text text-org font-face font-scale (scalar-all 255) thickness 8)
    
    (imshow window-name img)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))



BGR


A macro for SCALAR organized as BGR(BLUE, GREEN, RED) color values.


LISP-CV: (BGR B G R) => SCALAR


    Parameters:	

        B - The blue color value

        G - The green color value

        R - The red color value


BGR is the default color space in LisP-CV


Example:

Here BGR supplies a blue color value. 

CIRCLE IMAGE POINT RADIUS (BRG 255 0 0) +FILLED+ +AA+ 0)





ELLIPSE

Draws a simple or thick elliptic arc or fills an ellipse sector.

C++: void ellipse(Mat& img, Point center, Size axes, double angle, double startAngle, double endAngle, const Scalar& color, 
     int thickness=1, int lineType=8, int shift=0)

LISP-CV: (ELLIPSE (IMG MAT) (CENTER POINT) (AXES SIZE) (ANGLE :DOUBLE) (START-ANGLE :DOUBLE)
             (END-ANGEL :DOUBLE) (COLOR SCALAR) &OPTIONAL ((THICKNESS :INT) 1) ((LINE-TYPE :INT) 8) ((SHIFT :INT) 0)) => :VOID

C++: void ellipse(Mat& img, const RotatedRect& box, const Scalar& color, int thickness=1, int lineType=8)


    Parameters:	

        IMG - Image.

        CENTER - Center of the ellipse.

        AXES - Half of the size of the ellipse main axes.

        AMGLE - Ellipse rotation angle in degrees.

        START-AMGLE - Starting angle of the elliptic arc in degrees.

        END-ANGLE - Ending angle of the elliptic arc in degrees.

        BOX - Alternative ellipse representation via ROTATED-RECT. This means that the function dra-
              ws an ellipse inscribed in the rotated rectangle.

        COLOR - Ellipse color.

        THICKNESS - Thickness of the ellipse arc outline, if positive. Otherwise, this -s th-
                    at a filled ellipse sector is to be drawn.

        LINE-TYPE - Type of the ellipse boundary. See the (LINE) description.

        SHIFT - Number of fractional bits in the coordinates of the center and values of axes.


The functions ELLIPSE with less parameters draws an ellipse outline, a filled ellipse, an elliptic 
arc, or a filled ellipse sector. A piecewise-linear curve is used to approximate the elliptic arc 
boundary. If you need more control of the ellipse rendering, you can retrieve the curve using the 
function (ELLIPSE-2-POLY) and then render it with the function (POLYLINES) or fill it with the 
function (FILL-POLY) . If you use the first variant of the function and you want to draw the whole 
ellipse, not an arc, pass (EQ START-ANGLE 0) and (END-ANGLE 360).


Example 1:



(defun random-color (rng &optional (icolor 0))
  (setf icolor rng)
  (return-from random-color (scalar (uniform rng 0 255) 
				    (uniform rng 0 255) (uniform rng 0 255))))

(defun ellipse-example-1 ()

  (let ((window-name "ELLIPSE Example 1")
	(angle 0d0))
    ;;Create a window
    (with-named-window (window-name +window-normal+)
      ;;Set window to fullscreen
      (set-window-property window-name +wnd-prop-fullscreen+ 
			   +window-fullscreen+)
      (if (equal 1.0d0 (get-window-property window-name +wnd-prop-fullscreen+)) 
	  (set-window-property window-name +wnd-prop-aspectratio+ 
			       +window-freeratio+))
      ;;Initialize random number generator
      (with-rng ((rng (rng #xFFFFFFFF)))
	(loop
	   ;; Set BOX location and size to random values
	   (with-point ((center (point (uniform rng 0 640) (uniform rng 0 480))))
	     (with-size ((axes (size (coerce (uniform rng 0 420) 'double-float) 
				     (coerce (uniform rng 0 420) 'double-float))))
	       ;;Create a black background, MAT
	       (with-mat ((mat (mat-zeros 640 480 +8uc3+)))
		 (dotimes (n 10)
		   ;;Draw multiple ellipses with varied parameters
		   (ellipse mat center axes angle 0.0d0 
			    (coerce (uniform rng 0 360) 'double-float)
			    (random-color rng) (uniform rng -1 9) +aa+)
		   (sleep .015)
		   ;;Show and then delete MAT
		   (imshow window-name mat)))
	       (let ((c (wait-key 33)))
		 (when (= c 27)
		   (return))))))))))



Example 2:


(defun random-color (rng &optional (icolor 0))
  (setf icolor rng)
  (return-from random-color (scalar (uniform rng 0 255) 
				    (uniform rng 0 255) (uniform rng 0 255))))

(defun ellipse-example-2 ()
  (let ((window-name "ELLIPSE Example 2")
	(box-angle 0f0))
    ;; Create a window
    (with-named-window (window-name +window-normal+)
      ;; Set window to fullscreen
      (set-window-property window-name +wnd-prop-fullscreen+ 
			   +window-fullscreen+)
      (if (equal 1.0d0 (get-window-property window-name +wnd-prop-fullscreen+)) 
	  (set-window-property window-name +wnd-prop-aspectratio+ 
			       +window-freeratio+))
      ;; Initialize random number generator
      (with-rng ((rng (rng #xFFFFFFFF)))
	(loop
	   ;; Set BOX location and size to random values
	   (with-point ((box-loc (point (uniform rng 0 640) (uniform rng 0 480))))
	     (with-size ((box-size (size (coerce (uniform rng 0 420) 'double-float) 
					 (coerce (uniform rng 0 420) 'double-float))))
	       (setf box-angle (coerce (uniform rng 0 360) 'float))
	       ;; Create BOX
	       (with-rotated-rect ((box (rotated-rect box-loc box-size box-angle)))
		 ;; Create a black background, MAT
		 (with-mat ((mat (mat-zeros 640 480 +8uc3+)))
		   (dotimes (n 10)
		     ;;Draw multiple ellipses with varied parameters
		     (ellipse mat box (random-color rng) (uniform rng -1 9) 
			      +aa+)
		     (sleep .01)
		     ;; Show and then delete MAT
		     (imshow window-name mat)))
		 (let ((c (wait-key 33)))
		   (when (= c 27)
		     (return)))))))))))


LINE

Draws a line segment connecting two points.

C++: void line(Mat& img, Point pt1, Point pt2, const Scalar& color, int thickness=1, int lineType=8, int shift=0)

LISP-CV: (LINE (IMG MAT) (PT1 POINT) (PT2 POINT) (COLOR SCALAR) &OPTIONAL ((THICKNESS :INT) 1) ((LINE-TYPE :INT) 8) 
         ((SHIFT :INT) 0)) => :VOID

    Parameters:	

              IMG - Image.

              PT1 - First point of the line segment.

              PT2 - Second point of the line segment.

              COLOR - Line color.

              THICKNESS - Line thickness.

              LINE-TYPE - Type of the line:

                       8 (or omitted) - 8-connected line.

                       4 - 4-connected line.

                       +AA+ - antialiased line.

              SHIFT - Number of fractional bits in the point coordinates.



The function line draws the line segment between pt1 and pt2 points in the image. The line is clipp-
ed by the image boundaries. For non-antialiased lines with integer coordinates, the 8-connected or 
4-connected Bresenham algorithm is used. Thick lines are drawn with rounding endings. Antialiased l-
ines are drawn using Gaussian filtering. To specify the color of the line, you may also use the mac-
ros RGB - (RGB R G B) and BGR - (BGR B G R).


Example:


(defun line-example ()

  (let ((window-name "LINE Example")
        ;; Initialize random number generator
	(rng (rng #xFFFFFFFF))
	;; Create a black background
	(mat (mat-zeros 640 480 +8uc3+)))
    ;; Create a window
    (named-window window-name +window-normal+)
    ;; Set window to fullscreen
    (set-window-property window-name +wnd-prop-fullscreen+ 
			 +window-fullscreen+)
    (if (equal 1.0d0 (get-window-property window-name +wnd-prop-fullscreen+)) 
	(set-window-property window-name +wnd-prop-aspectratio+ 
			     +window-freeratio+))
    ;; Set line type and thickness
    (do* ((line-type +aa+)
	  (thickness 1)
	  (pt1 0)
	  (pt2 0)
          (x-1 -450)
	  (x-2 1350)
	  (y-1 -450)
	  (y-2 1350))
	 ((plusp (wait-key 1)) 
	  (format t "Key is pressed by user"))
      ;; Set the lines points to random values
      (setf pt1 (point (uniform rng x-1 x-2) (uniform rng y-1 y-2)))    
      (setf pt2 (point (uniform rng x-1 x-2) (uniform rng y-1 y-2)))
      ;; Draw a line
      (line mat pt1 pt2 (bgr 255 0 0) thickness line-type)
      ;; Show output
      (imshow window-name mat))
    (destroy-window window-name)))



PUT-TEXT

Draws a text string.

C++: void putText(Mat& img, const string& text, Point org, int fontFace, double fontScale, Scalar color, int thickness=1, 
     int lineType=8, bool bottomLeftOrigin=false )

LISP-CV: (PUT-TEXT (IMG MAT) (TEXT *STRING) (ORG POINT) (FONT-FACE :INT) (FONT-SCALE :DOUBLE)  
             (COLOR SCALAR) &OPTIONAL (THICKNESS :INT) (LINE-TYPE :INT) (BOTTOM-LEFT-ORIGN :BOOLEAN)) => :VOID
 
    Parameters:	

        IMG - Image.

        TEXT - Text string to be drawn.

        ORG - Bottom-left corner of the text string in the image.

        FONT-FACE - Font type. One of: +FONT-HERSHEY-SIMPLEX+ 

                                       +FONT-HERSHEY-PLAIN+ 

                                       +FONT-HERSHEY-DUPLEX+ 

                                       +FONT-HERSHEY-COMPLEX+ 

                                        FONT_HERSHEY_ITALIC

                                       +FONT-HERSHEY-COMPLEX-SMALL+ 

                                       +FONT-HERSHEY-SCRIPT-SIMPLEX+ 
 
                                       +FONT-HERSHEY-SCRIPT-COMPLEX+ 


where each of the font id’s can be combined with +FONT-ITALIC+ to get the slanted letters.


        FONT-SCALE - Font scale factor that is multiplied by the font-specific base size.

        COLOR- Text color.

        THICKNESS - Thickness of the lines used to draw a text.

        LINE-TYPE - Line type. See the line for details.

        BOTTOM-LEFT-ORIGIN - When true, the image data origin is at the bottom-left corner. Otherwi-
                             se, it is at the top-left corner.


The function PUT-TEXT renders the specified text string in the image. Symbols that cannot be render
ed using the specified font are replaced by question marks. See (GET-TEXT-SIZE) for another text 
rendering code example.


Example:

(defun random-color (rng &optional (icolor 0))
  (setf icolor rng)
  (return-from random-color (scalar (uniform rng 0 255) 
				    (uniform rng 0 255) (uniform rng 0 255))))

(defun put-text-example ()

  (let* ((window-name "PUT-TEXT Example")
	 (window-width 1280)
	 (window-height 1024)
	 ;; Initialize random number generator
	 (rng (rng #xFFFFFFFF))
	 ;; Create a black background
	 (mat (mat-zeros window-width window-height +8uc3+)))
    ;; Create a window
    (named-window window-name +window-normal+)
    ;; Set window to fullscreen
    (set-window-property window-name +wnd-prop-fullscreen+ 
			 +window-fullscreen+)
    (if (equal 1.0d0 (get-window-property window-name +wnd-prop-fullscreen+)) 
	(set-window-property window-name +wnd-prop-aspectratio+ 
			     +window-freeratio+))
    ;; Set line type
    (do* ((line-type +aa+)
	  (i 0)
          (iterations 0)
          (scale 10.40d0)
	  ;; Initialize text position variables
	  (org 0)
          (x-1 0)
	  (x-2 0)
          (y 0))
	 ((plusp (wait-key 1)) 
	  (format t "Key is pressed by user"))
      ;; Were doing multiple prints of text to screen per loop here
      (dotimes (n iterations) 
	;; Set the texts location to random values
	(setf org (point (uniform rng x-1 x-2) y))
        ;; Print text
	(put-text mat "LisP-CV" org +font-hershey-complex-small+
		  scale  (random-color rng) (uniform rng 5 50) line-type))
      (incf i)
      (if (< y (+ window-height 358)) (incf y 2) (setf y 0))
      (if (< iterations 4) (incf iterations 1) (setf iterations 1))
      ;; Show result in window
      (imshow window-name mat))
    (destroy-window window-name)))



RECTANGLE


Draws a simple, thick, or filled up-right rectangle.


C++: void rectangle(Mat& img, Point pt1, Point pt2, const Scalar& color, int thickness=1, int lineType=8, int shift=0)

LISP-CV: (RECTANGLE (IMG MAT) (PT1 POINT) (PT2 POINT) (COLOR SCALAR) &OPTIONAL ((THICKNESS :INT) 1) ((LINE-TYPE :INT) 8) 
         ((SHIFT :INT) 0)) => :VOID


    Parameters:	

        Parameters:	

              IMG - Image.

              PT1 - First point of the line segment.

              PT2 - Second point of the line segment.

              COLOR - Rectangle color or brightness (grayscale image).

              THICKNESS - Thickness of lines that make up the rectangle. 
                          Negative values, like +FILLED+ (-1) , mean that 
                          the function has to draw a filled rectangle.

              LINE-TYPE - Type of the line:

                      8 (or omitted) - 8-connected line.

                      4 - 4-connected line.

                     +AA+ - antialiased line.

             SHIFT - Number of fractional bits in the point coordinates.

The function rectangle draws a rectangle outline or a filled rectangle whose two opposite corners a-
re PT1 and PT2.


Example:


(defun random-color (rng &optional (icolor 0))
  (setf icolor rng)
  (return-from random-color (scalar (uniform rng 0 255) 
				    (uniform rng 0 255) 
				    (uniform rng 0 255))))

(defun rectangle-example ()
  (with-rng ((rng (rng #xFFFFFFFF)))
    (let* ((window-name "RECTANGLE Example")
	   (window-width 640)
	   (window-height 480)
	   ;; Set line type and thickness
	   (line-type +aa+)
	   (thickness (uniform rng -3 10))
	   ;; Initialize rectangle position variables
	   (x-1 (/ (* window-width -1) 2))
	   (x-2 (/ (* window-width 3) 2))
	   (y-1 (/ (* window-width -1) 2))
	   (y-2 (/ (* window-width 3) 2)))
      ;; Create a window
      (with-named-window 
	  (window-name +window-normal+)
	;; Set window to fullscreen
	(set-window-property window-name +wnd-prop-fullscreen+ 
			     +window-fullscreen+)
	(set-window-property window-name +wnd-prop-aspectratio+ 
			     +window-freeratio+)
	;; Initialize random number generator
	;; Create a black background
	(with-mat 
	    ((mat (mat-zeros window-width window-height +8uc3+)))
	  (loop
	     ;; Print randomly colored rectangles to screen
	     (with-point 
		 ((pt1 (point (uniform rng x-1 x-2) (uniform rng y-1 y-2)))
		  
		  (pt2 (point (uniform rng x-1 x-2) (uniform rng y-1 y-2))))
	       (rectangle mat pt1 pt2 (random-color rng) thickness line-type)
	       ;; Show result in window
	       (imshow window-name mat)
	       (let ((c (wait-key 33)))
		 (when (= c 27)
		   (return))))))))))


RGB

A macro for SCALAR organized as RGB(RED, GREEN, BLUE) color values.

LISP-CV: (RGB R G B) => SCALAR

    Parameters:	


        R - The red color value

        G - The green color value

        B - The blue color value


A creative reversal of the default BGR color space in LisP-CV. Values are put into the RGB macro in
Red, Green, Blue, order, but are ultimately entered into the recieving function as BGR. This macro 
is designed for ease of use.


Usage:

Here RGB supplies a red color value. 

CIRCLE IMAGE POINT RADIUS (RGB 255 0 0) +FILLED+ +AA+ 0) 


UTILITY AND SYSTEM FUNCTIONS AND MACROS:



CHECK-HARDWARE-SUPPORT

Returns true if the specified feature is supported by the host hardware.

C++: bool checkHardwareSupport(int feature)

LISP-CV: (CHECK-HARDWARE-SUPPORT (FEATURE :INT)) => :BOOLEAN

    Parameters:	feature -

       The feature of interest, one of:

             +CPU_MMX+ - MMX
             +CPU_SSE+ - SSE
             +CPU_SSE2+ - SSE 2
             +CPU_SSE3+ - SSE 3
             +CPU_SSSE3+ - SSSE 3
             +CPU_SSE4_1+ - SSE 4.1
             +CPU_SSE4_2+ - SSE 4.2
             +CPU_POPCNT+ - POPCOUNT
             +CPU_AVX+ - AVX

The function returns true if the host hardware supports the specified feature. When user calls 
(SET-USE-OPTIMIZED 0), the subsequent calls to (CHECK-HARDWARE-SUPPORT) will return false until 
(SET-USE-OPTIMIZED 1) is called. This way user can dynamically switch on and off the optimized 
code.


(defun check-hardware-support-example ()

  "Returns true if the specified feature is supported 
   by the host hardware. Returns 0 otherwise"

  (let*  ((mmx (check-hardware-support +cpu-mmx+))
	  (sse (check-hardware-support +cpu-sse+))
          (sse2 (check-hardware-support +cpu-sse2+))
          (sse3 (check-hardware-support +cpu-sse3+))
          (ssse3 (check-hardware-support +cpu-ssse3+))
          (sse4-1 (check-hardware-support +cpu-sse4-1+))   
          (sse4-2 (check-hardware-support +cpu-sse4-2+))
          (popcnt (check-hardware-support +cpu-popcnt+))
	  (avx (check-hardware-support +cpu-avx+))
	  (hardware-max-feature (check-hardware-support 
                                 +hardware-max-feature+)))
    (format t "~%MMX Support = ~a~%" mmx)
    (format t "SSE Support = ~a~%" sse)
    (format t "SSE2 Support = ~a~%" sse2)
    (format t "SSE3 Support = ~a~%" sse3)
    (format t "SSSE3 Support = ~a~%" ssse3)
    (format t "SSE4-1 Support = ~a~%" sse4-1)
    (format t "SSE4-2 Support= ~a~%" sse4-2)
    (format t "POPCOUNT Support = ~a~%" popcnt)
    (format t "AVX Support = ~a~%" avx)
    (format t "HARDWARE-MAX-FEATURE = ~a~%~%" 
	    hardware-max-feature)))



CUBE-ROOT

Computes the cube root of an argument.

C++: float cubeRoot(float val)

LISP-CV:: (CUBE-ROOT (VAL :FLOAT)) => :FLOAT

    Parameters:	VAL - A function argument.

The function CUBE-ROOT computes the cubic root of VAL. Negative arguments are handled correctly. NAN 
and INFINITY are not handled. The accuracy approaches the maximum possible accuracy for single-precision 
data.


(defun cube-root-example (&optional (float-val 27.0))
  "Computes the cube root of FLOAT-VAL."
  (format t "Cube root of FLOAT-VAL = ~a~%~%" 
	  (cube-root float-val)))



FAST-ATAN2

Calculates the angle of a 2D vector in degrees.

C++: float fastAtan2(float y, float x)

LISP-CV: (FAST-ATAN2 (X :FLOAT) (Y :FLOAT)) => :FLOAT

    Parameters:	

        X - x-coordinate of the vector.

        Y - y-coordinate of the vector.


The function FAST-ATAN2 calculates the full-range angle of an input 2D vector. The angle is measured 
in degrees and varies from 0 to 360 degrees. The accuracy is about 0.3 degrees.


(defun fast-atan2-example (x y)

  "Calculates the angle of a 2D 
   float vector in degrees."

  (let*  ((float-y x)
          (float-x y))
    (format t "Angle of vector - float = ~a~%~%" 
	    (fast-atan2 float-y float-x))))



GET-TICK-COUNT

Returns the number of ticks.


C++: int64 getTickCount()

LISP-CV: (GET-TICK-COUNT) => :INT64


The function returns the number of ticks after the certain event (for example, when the machine was
turned on). It can be used to initialize (RNG) - Random Number Generator - or to measure a function 
execution time by reading the tick count before and after the function call. 

See also: (GET-TICK-FREQUENCY)



(defun get-tick-count-example ()

  "Calculates the amount CPU ticks that occur while 
   running the function used to create a matrix MAT 
   once and 1000 times in microseconds."

  (let* ((t1 0)
	 (t2 0)
	 (time-calc 0))
    (setf t1 (get-tick-count))
    (dotimes (i 1)
      (mat))
    (setf t2 (get-tick-count))
    (setf time-calc (- t2 t1))
    (format t "~a CPU ticks occurred while running MAT once ~%" 
	    time-calc)
    
    (setf t1 (get-tick-count))
    (dotimes (i 1000)
      (mat))
    (setf t2 (get-tick-count))
    (setf time-calc (- t2 t1))
    (format t "~a CPU ticks occurred while ruuning MAT 1000 times ~%"  
	    time-calc)))


GET-TICK-FREQUENCY


Returns the number of ticks per second.


C++: double getTickFrequency()

LISP-CV: (GET-TICK-FREQUENCY) => :DOUBLE


The function returns the number of ticks per second. That is, the following code computes the execu-
tion time in seconds:


(defun get-tick-frequency-example ()

  "Calculates the seconds that pass while the SLEEP function 
   is ran for 5 seconds and the seconds that pass while runn-
   ing the MAT-EYE function(used to create an identity matri-
   x) 1 million times for comparision.
   
   Note: This function takes about 6 seconds to complete on 
   a Quad-Core Processor."

  (let* ((t1 0)
	 (t2 0)
	 (time-calc 0))
    (setf t1 (coerce (get-tick-count) 'double-float))
    (sleep 5)
    (setf t2 (coerce (get-tick-count) 'double-float))
    (setf time-calc (/ (- t2 t1) (get-tick-frequency)))
    (format t "Sleeping for 5 seconds took ~a seconds~%~%" 
	    time-calc)
    (setf t1 (coerce (get-tick-count) 'double-float))
    (dotimes (i 1000000)
      (mat-eye 3 3 +8u+))
    (setf t2 (coerce (get-tick-count) 'double-float))
    (setf time-calc (/ (- t2 t1) (get-tick-frequency)))
    (format t "1 million runs of MAT-EYE took ~a seconds"  
	    time-calc)))



*SQRT

Calculates a square root of array elements.

C++: void sqrt(InputArray src, OutputArray dst)

LISP-CV: (SQRT (SRC MAT) (DEST MAT)) => :VOID

    Parameters:	

        SRC - Input floating-point array.

        DEST - Output array of the same size and type as SRC.


The functions *SQRT calculate a square root of each input array element. In case of multi-channel 
arrays, each channel is processed independently. The accuracy is approximately the same as of the 
Common Lisp function SQRT.

Note: This function is named *SQRT instead of SQRT because, SQRT is the name of a Common Lisp function.

See also:

(POW), (MAGNITUDE)


(defun *sqrt-example ()

  "Computes the square root of 
   each element of matrix M"

  (let* ((data (alloc :float '(53.0f0 62.0f0 85.0f0 64.0f0 23.0f0 
			       97.0f0 52.0f0 16.0f0 12.0f0)))
	 (m (mat 3 3 +32f+ data))
         (dest (mat 3 3 +32f+)))
    (format t "M = ~%~%")
    (dotimes (i (rows m))
      (dotimes (j (cols m))
	(format t "~a" (at m i j :float))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    (*sqrt m dest)
    (format t "DEST = ~%~%")
    (dotimes (i 3)
      (dotimes (j 3)
	(format t "~a" (at dest i j :float))
	(princ #\Space))
      (princ #\Newline))
    (free data)))



IMGPROC - IMAGE FILTERING:




BILATERAL-FILTER

Applies the bilateral filter to an image.

C++: void bilateralFilter(InputArray src, OutputArray dst, int d, double sigmaColor, double sigmaSpace, int borderType=BORDER_DEFAULT )

LISP-CV (BILATERAL-FILTER (SRC MAT) (DEST MAT) (D :INT) (SIGMA-COLOR :DOUBLE) (SIGMA-SPACE :DOUBLE) &OPTIONAL
        ((BORDER-TYPE :INT) +BORDER-DEFAULT+)) => :VOID

    Parameters:	

        SRC - Source 8-bit or floating-point, 1-channel or 3-channel image.

        DST - Destination image of the same size and type as SRC.

        D - Diameter of each pixel neighborhood that is used during filtering. If it is non-positive, 
            it is computed from sigma-Space.

        SIGMA-COLOR - Filter sigma in the color space. A larger value of the parameter means that 
                      farther colors within the pixel neighborhood (see SIGMA-SPACE) will be mixed 
                      together, resulting in larger areas of semi-equal color.

        SIGMA-SPACE - Filter sigma in the coordinate space. A larger value of the parameter means that 
                      farther pixels will influence each other as long as their colors are close enough 
                      (see SIGMA-COLOR). When (> d 0) , it specifies the neighborhood size regardless of 
                      SIGMA-SPACE . Otherwise, D is proportional to SIGMA-SPACE .

The function applies bilateral filtering to the input image, as described in:

http://www.dai.ed.ac.uk/CVonline/LOCAL_COPIES/MANDUCHI1/Bilateral_Filtering.html 

BILATERAL-FILTER can reduce unwanted noise very well while keeping edges fairly sharp. However, it is 
very slow compared to most filters.

Sigma values: For simplicity, you can set the 2 sigma values to be the same. If they are small (< 10), 
the filter will not have much effect, whereas if they are large (> 150), they will have a very strong 
effect, making the image look “cartoonish”.

Filter size: Large filters (> D 5) are very slow, so it is recommended to use (= D 5) for real-time 
applications, and perhaps (= D 9) for offline applications that need heavy noise filtering.

This filter does not work inplace.

Example:

See BLUR-EXAMPLE in this file.



BLUR

Blurs an image using the normalized box filter.


C++: void blur(InputArray src, OutputArray dst, Size ksize, Point anchor=Point(-1,-1), int borderType=BORDER_DEFAULT )

LISP-CV: (BLUR (SRC MAT) (DEST MAT) (KSIZE SIZE) &OPTIONAL ((ANCHOR POINT) (POINT -1 -1)) ((BORDER-TYPE :INT) +BORDER-DEFAULT+)) => :VOID


    Parameters:	

        SRC - Input image; it can have any number of channels, which are processed independently, but 
              the depth should be +8U+, +16U+, +16S+, +32F+ or +64F+.

        DST - Output image of the same size and type as SRC.

        KSIZE - Blurring kernel size.

        ANCHOR - Anchor point; default value (POINT -1,-1) means that the anchor is at the kernel center.

        BORDER-TYPE - Border mode used to extrapolate pixels outside of the image.


The function smoothes an image using the kernel:

See OpenCV documentation for the formula:

http://docs.opencv.org/trunk/modules/imgproc/doc/filtering.html?highlight=blur#blur


See also:

(BOX-FILTER), (BILATERAL-FILTER), (GAUSSIAN-BLUR), (MEDIAN-BLUR)


Example:


;;; Global Variables

(defparameter delay-caption 1500)
(defparameter delay-blur 100)
(defparameter max-kernel-length 31)

;;; Load the source image
(defparameter src (gc:imread "/d1" 1))

(defparameter dst (gc:mat))
(defparameter window-name "GAUSSIAN-BLUR Example")


(defun display-caption (caption &optional c)
  (setf dst (gc:mat-zeros (rows src) (cols src) (mat-type src)))
  (put-text dst caption (gc:point (round (/ (cols src) 4)) 
			       (round (/ (rows src) 2))) 
	    +font-hershey-complex+
	    1d0 (gc:scalar 255 255 255))
  (imshow window-name dst)
  (setf c (wait-key delay-caption))
  (if (>= c 0) (return-from display-caption -1)
      (return-from display-caption 0)))


(defun display-dst (delay &optional c)
  (imshow window-name dst)
  (setf c (wait-key delay))
  (if (>= c 0) (return-from display-dst -1)
      (return-from display-dst 0)))


(defun blur-example ()

  (with-named-window (window-name +window-autosize+)
    (move-window window-name (cols src) 0)
    
    (if (not (eq 0 (display-caption "Original Image"))) 
	(return-from blur-example 0))

    (setf dst (gc:clone src))

    (if (not (eq 0 (display-dst delay-caption))) 
	(return-from blur-example 0))

        ;;; Applying Homogeneous blur
    (if (not (eq 0 (display-caption "Homogeneous Blur"))) 
	(return-from blur-example 0))

    (do ((i 1 (+ i 2)))
	((> i max-kernel-length))
      (blur src dst (gc:size i i))

      (if (not (eq 0 (display-dst delay-blur))) 
	  (return-from blur-example 0)))

        ;;; Applying Gaussian blur
    (if (not (eq 0 (display-caption "Gaussian Blur"))) 
	(return-from blur-example 0))

    (do ((i 1 (+ i 2)))
	((> i max-kernel-length))
      (gaussian-blur src dst (gc:size i i) 0d0 0d0)

      (if (not (eq 0 (display-dst delay-blur))) 
	  (return-from blur-example 0)))

        ;;; Applying Median blur
    (if (not (eq 0 (display-caption "Median Blur"))) 
	(return-from blur-example 0))

    (do ((i 1 (+ i 2)))
	((> i max-kernel-length))
      (median-blur src dst i)

      (if (not (eq 0 (display-dst delay-blur))) 
	  (return-from blur-example 0)))

        ;;; Applying Bilateral Filter
    (if (not (eq 0 (display-caption "Bilateral Blur"))) 
	(return-from blur-example 0))

    (do ((i 1 (+ i 2)))
	((> i max-kernel-length))
      (bilateral-filter src dst i (coerce (* i 2) 'double-float) 
			(coerce (round (/ i 2)) 'double-float))

      (if (not (eq 0 (display-dst delay-blur))) 
	  (return-from blur-example 0)))

       ;;; Wait until user press a key
    (display-caption "End: Press a key!")
    (loop
       (let ((c (wait-key 27)))
	 (when (>= c 0)
	   (return))))))



COPY-MAKE-BORDER

Forms a border around an image.

C++: void copyMakeBorder(InputArray src, OutputArray dst, int top, int bottom, int left, int right, int borderType, 
     const Scalar& value=Scalar() )

LISP-CV: (COPY-MAKE-BORDER (SRC MAT) (DEST MAT) (TOP :INT) (BOTTOM :INT) (LEFT :INT) (RIGHT :INT) 
         (BORDER-TYPE :INT) &OPTIONAL ((VALUE SCALAR) (SCALAR))) => :VOID

    Parameters:	

        SRC - Source image.

        DEST - Destination image of the same type as SRC and the size:

                (+ (COLS SRC) LEFT RIGHT), (+ (ROWS SRC) TOP BOTTOM).

        TOP -

        BOTTOM -

        LEFT -

        RIGHT - 
   
                Parameters specifying how many pixels in each direction from the source image rectangle 
                to extrapolate. For example, (EQ TOP 1), (EQ BOTTOM 1), (EQ LEFT 1), (EQ RIGHT 1) means 
                that 1 pixel-wide border needs to be built.

        BORDER-TYPE - Border type, one of the +BORDER-*+ constants, except for +BORDER-TRANSPARENT+ 
                      and +BORDER-ISOLATED+. 

        VALUE - Border value if (EQ BORDER-TYPE +BORDER-CONSTANT+).


The function copies the source image into the middle of the destination image. The areas to the left, 
to the right, above and below the copied source image will be filled with extrapolated pixels. This 
is not what the OpenCV class FilterEngine or filtering functions based on it do (they extrapolate pi-
xels on-fly), but what other more complex functions, including your own, may do to simplify image bo-
undary handling.


Note

When the source image is a part (ROI) of a bigger image, the function will try to use the pixels outside 
of the ROI to form a border. To disable this feature and always do extrapolation, as if src was not a ROI, 
use (LOGIOR BORDER-TYPE +BORDER-ISOLATED+).

See also:

(BORDER-INTERPOLATE)


(defun copy-make-border-example (&optional (cam 0) 
				   (width *default-width*)
				   (height *default-height*))
  "Forms a border around FRAME"

  (with-captured-camera (cap cam :width width :height height)
    (let* ((window-name "COPY-MAKE-BORDER Example")
	   (border 100)
           (border-width (+ (* 2  border) width))
           (border-height (+ (* 2  border) height)))
      (with-named-window (window-name +window-normal+)
	(move-window window-name 759 175)
	(loop
	  ;Create a matrix big enough to accommodate 
	  ;a 100 pixel border on all sides
	   (with-mat ((rgb (mat border-height border-width +8uc3+))
		      (frame (mat)))
	     (cap-read cap frame)
	     ;Make a border around FRAME
	     (copy-make-border frame rgb border border border border  
			       +border-replicate+ (scalar-all 75))
	     (imshow window-name rgb)
	     (let ((c (wait-key 33)))
	       (when (= c 27)
		 (return)))))))))



DILATE

Dilates an image by using a specific structuring element.

C++: void dilate(InputArray src, OutputArray dst, InputArray kernel, Point anchor=Point(-1,-1), int iterations=1, 
     int borderType=BORDER_CONSTANT, const Scalar& borderValue=morphologyDefaultBorderValue() )

LISP-CV: (DILATE (SRC MAT) (DEST MAT) (KERNEL MAT) &OPTIONAL ((ANCHOR POINT) (POINT -1 -1)) ((ITERATIONS :INT) 1) 
         ((BORDER-TYPE :INT) +BORDER-CONSTANT+) ((BORDER-VALUE SCALAR) (MORPHOLOGY-DEFAULT-BORDER-VALUE+))) => :VOID

    Parameters:	

        SRC - Input image; the number of channels can be arbitrary, but the depth should be one of 
              +8U+, +16U+, +16S+, +32F+ or +64F+.

        DEST - Output image of the same size and type as SRC.

        KERNEL - Structuring element used for erosion; if (EQ ELEMENT (MAT)) , a 3 x 3 rectangular 
                 structuring element is used. Kernel can be created using (GET-STRUCTURING-ELEMENT).

        ANCHOR - Position of the anchor within the element; default value (-1, -1) means that the 
                  anchor is at the element center.

        ITERATIONS - Number of times dilation is applied.

        BORDER-TYPE - Border type, one of the +BORDER-*+ constants, except 
                      for +BORDER-TRANSPARENT+ and +BORDER-ISOLATED+.

        BORDER-VALUE - Border value in case of a constant border. 


The function dilates the source image using the specified structuring element that determines the 
shape of a pixel neighborhood over which the maximum is taken:

See OpenCv documentation:

http://docs.opencv.org/trunk/modules/imgproc/doc/filtering.html?highlight=erode#dilate

for a description and formula.

The function supports the in-place mode. Dilation can be applied several (ITERATIONS) times. In case 
of multi-channel images, each channel is processed independently.


See also:

(ERODE), (MORPHOLOGY-EX), (CREATE-MORPHOLOGY-FILTER), (GET-STRUCTURING-ELEMENT)


Example:


;; Global variables

(defparameter window-name "DILATION-DEST - DILATE Example")
;; Load an image
(defparameter src (imread "/my-pic.jpg" 1))
(defparameter dilation-dest (mat))
(defparameter dilation-elem (alloc :int 0))
(defparameter dilation-size (alloc :int 0))
(defparameter max-elem 2)
(defparameter max-kernel-size 21)


;; DILATION is a callback function called by the TRACKBAR in ERODE-EXAMPLE

(defcallback dilation :void ((dilation-type :int))
  ;; Adjust the GET-STRUCTURING-ELEMENT SHAPE 
  ;; parameter based on trackbar position
  (if (eq 0 (? dilation-elem :int)) (setf dilation-type +morph-rect+) 
      (if (eq 1 (? dilation-elem :int)) (setf dilation-type +morph-cross+)
	  (if (eq 2 (? dilation-elem :int)) (setf dilation-type +morph-ellipse+))))
  ;; Specify the shape of the kernel used 
  ;; to perform the dilation operation
  (with-mat ((element (%get-structuring-element 
		       dilation-type 
		       (size (+ (* (? dilation-size :int) 2) 1)
			     (+ (* (? dilation-size :int) 2) 1)) 
		       (point (? dilation-size :int) (? dilation-size :int)))))
    ;; Apply the dilation operation
    (dilate src dilation-dest element)
    (imshow window-name dilation-dest)))


(defun dilate-example ()

  (if (empty src) 
      (return-from dilate-example
	(format t "Image not loaded")))
  ;; Create window
  (with-named-window (window-name 1)
    (move-window window-name (cols src) 0)
    ;; Create trackbar that, adjusts the GET-STRUCTURING-ELEMENT 
    ;; SHAPE parameter and calls the DILATION callback function, 
    ;; every time it is moved
    (create-trackbar "SHAPE" window-name dilation-elem max-elem (callback dilation))
    ;; Create trackbar that, adjusts the GET-STRUCTURING-ELEMENT 
    ;; KSIZE and KERNEL parameters and calls the DILATION callback 
    ;; function, every time it is moved
    (create-trackbar  "KERNEL/KSIZE" window-name dilation-size max-kernel-size 
		      (callback dilation))
    (imshow window-name src)
    (loop
       (let ((c (wait-key 33)))
	 (when (= c 27)
 	   (del-mat src)
	   (del-mat dilation-dest)
	   (del dilation-elem)
	   (del dilation-size)
	   (return))))))



ERODE

Erodes an image by using a specific structuring element.

C++: void erode(InputArray src, OutputArray dst, InputArray kernel, Point anchor=Point(-1,-1), int iterations=1, 
     int borderType=BORDER_CONSTANT, const Scalar& borderValue=morphologyDefaultBorderValue() )

LISP-CV: (ERODE (SRC MAT) (DEST MAT) (KERNEL MAT) ((ANCHOR POINT) (POINT -1 -1)) 
         ((ITERATIONS :INT) 1) ((BORDER-TYPE :INT) +BORDER-CONSTANT+) 
         ((BORDER-VALUE SCALAR) (MORPHOLOGY-DEFAULT-BORDER-VALUE+))) => :VOID

    Parameters:	

        SRC - Input image; the number of channels can be arbitrary, but the depth should be one of 
              +8U+, +16U+, +16S+, +32F+ or +64F+.

        DEST - Output image of the same size and type as SRC.

        KERNEL - Structuring element used for erosion; if (EQ ELEMENT (MAT)) , a 3 x 3 rectangular 
                 structuring element is used. Kernel can be created using (GET-STRUCTURING-ELEMENT).

        ANCHOR - Position of the anchor within the element; default value (-1, -1) means that the 
                  anchor is at the element center.

        ITERATIONS - Number of times erosion is applied.

        BORDER-TYPE - Border type, one of the +BORDER-*+ constants, except 
                      for +BORDER-TRANSPARENT+ and +BORDER-ISOLATED+.

        BORDER-VALUE - Border value in case of a constant border. 


The function erodes the source image using the specified structuring element that determines the shape 
of a pixel neighborhood over which the minimum is taken:

See OpenCv documentation:

http://docs.opencv.org/trunk/modules/imgproc/doc/filtering.html?highlight=erode#erode

for a description and formula.

The function supports the in-place mode. Erosion can be applied several (ITERATIONS) times. In case 
of multi-channel images, each channel is processed independently.


See also:

(DILATE), (MORPHOLOGY-EX), (CREATE-MORPHOLOGY-FILTER), (GET-STRUCTURING-ELEMENT)


Example:


;; Global variables
(defparameter window-name "EROSION-DEST - ERODE Example")
;; Load an image
(defparameter src (imread "/d1" 1))
(defparameter erosion-dest (mat))
(defparameter erosion-elem (alloc :int 0))
(defparameter erosion-size (alloc :int 0))
(defparameter max-elem 2)
(defparameter max-kernel-size 21)


;; EROSION is a callback function called by the TRACKBAR in ERODE-EXAMPLE

(defcallback erosion :void ((erosion-type :int))
  ;; Adjust the GET-STRUCTURING-ELEMENT SHAPE 
  ;; parameter based on trackbar position
  (if (eq 0 (? erosion-elem :int)) (setf erosion-type +morph-rect+) 
      (if (eq 1 (? erosion-elem :int)) (setf erosion-type +morph-cross+)
	  (if (eq 2 (? erosion-elem :int)) (setf erosion-type +morph-ellipse+))))
  ;; Specify the shape of the kernel used 
  ;; to perform the erosion operation
  (with-mat ((element (get-structuring-element 
		       erosion-type 
		       (size (+ (* (? erosion-size :int) 2) 1)
			     (+ (* (? erosion-size :int) 2) 1)) 
		       (point (? erosion-size :int) (? erosion-size :int)))))
    ;; Apply the erosion operation
    (erode src erosion-dest element)
    (imshow window-name erosion-dest)))

(defun erode-example ()

  (if (empty src) 
      (return-from erode-example
	(format t "Image not loaded")))
  ;; Create window
  (with-named-window (window-name 1)
    (move-window window-name (cols src) 0)
    ;; Create trackbar that, adjusts the GET-STRUCTURING-ELEMENT 
    ;; SHAPE parameter and calls the EROSION callback function, 
    ;; every time it is moved
    (create-trackbar "SHAPE" window-name erosion-elem max-elem (callback erosion))
    ;; Create trackbar that, adjusts the GET-STRUCTURING-ELEMENT 
    ;; KSIZE and KERNEL parameters and calls the EROSION callback 
    ;; function, every time it is moved
    (create-trackbar  "KERNEL/KSIZE" window-name erosion-size max-kernel-size 
		      (callback erosion))
    (imshow window-name src)
    (loop
       (let ((c (wait-key 33)))
	 (when (= c 27)
	   (del-mat src)
	   (del-mat erosion-dest)
	   (del erosion-elem)
	   (del erosion-size)
	   (return))))))


FILTER-2D

Convolves an image with the kernel.

C++: void filter2D(InputArray src, OutputArray dst, int ddepth, InputArray kernel, Point anchor=Point(-1,-1), double delta=0, 
     int borderType=BORDER_DEFAULT )

LISP-CV: (FILTER-2D (SRC MAT) (DEST MAT) (DDEPTH :INT) (KERNEL MAT) &OPTIONAL ((ANCHOR POINT) (POINT -1 -1)) ((DELTA :DOUBLE) 0D0) 
         ((BORDER-TYPE :INT) +BORDER-DEFAULT+)) => :VOID

    Parameters:	

        SRC - Input image.

        DEST - Output image of the same size and the same number of channels as SRC.

        DDEPTH - Desired depth of the destination image; if it is negative, it will be the same as
                (DEPTH SRC); 

The following combinations of (DEPTH SRC) and ddepth are supported:

                (EQ (DEPTH SRC) +8U+): (EQ DDEPTH (OR -1 +16S+ +32F+ +64F+))

                (EQ (DEPTH SRC) (OR +16U+ +16S+)) -> (EQ DDEPTH (OR -1 +32F+ +64F+))

                (EQ (DEPTH SRC) +32F+) -> (EQ DDEPTH (OR -1 +32F+ +64F+))

                (EQ (DEPTH SRC) +64F+) -> (EQ DDEPTH (OR -1 +64F+))

        when (EQ DDEPTH -1), the output image will have the same depth as the source.
     
        KERNEL - Convolution kernel (or rather a correlation kernel), a single-channel floating point 
                 matrix; if you want to apply different kernels to different channels, split the image 
                 into separate color planes using (SPLIT) and process them individually.

        ANCHOR - Anchor of the kernel that indicates the relative position of a filtered point within 
                 the kernel; the anchor should lie within the kernel; default value (-1,-1) means that 
                 the anchor is at the kernel center.

        DELTA - Optional value added to the filtered pixels before storing them in DEST.

        BORDER-TYPE - Border type, one of the +BORDER-*+ constants, except 
                      for +BORDER-TRANSPARENT+ and +BORDER-ISOLATED+.


The function applies an arbitrary linear filter to an image. In-place operation is supported. When 
the aperture is partially outside the image, the function interpolates outlier pixel values according 
to the specified border mode.

See OpenCV documentation:

http://docs.opencv.org/trunk/modules/imgproc/doc/filtering.html?highlight=filter2#filter2d

for further description and formulae.

See also:

(SEP-FILTER-2D), (CREATE-LINEAR-FILTER), (DFT), (MATCH-TEMPLATE)


(defun filter-2d-example (filename)

  (let ((window-name "FILTER-2D Example")
	(ind 0)
       ;Initialize filter parameters
	(delta 0d0)
	(ddepth  -1)
	(kernel-size 0))
    (with-point ((anchor (point -1 -1)))
     ;Load an image
      (with-mat ((img (imread filename +load-image-color+)))
	(with-named-window (window-name +window-autosize+)
	  (move-window window-name (cols img) 175)
	  (with-mat ((dest (mat)))
	   ;The loop filters the image with 5 different 
	   ;kernel sizes, each lasting 0.5 seconds
	    (loop 
	      ;Update kernel size for a normalized box filter
	       (setf kernel-size (+ (* (mod ind 5) 2) 3))
	       (with-mat ((temp (mat-ones kernel-size kernel-size +32f+)))
		 (with-scalar ((scalar (scalar (* kernel-size kernel-size))))
		   (convert-to (assgn-val temp scalar) temp +32f+)
		   (with-mat ((kernel (mat-ones kernel-size kernel-size +32f+)))
		     (divide kernel temp kernel)
		    ;Apply filter
		     (filter-2d img dest ddepth kernel anchor delta +border-default+)
		     (imshow window-name dest)
		     (incf ind)
		     (let ((c (wait-key 500)))
		       (when (= c 27)
			 (return)))))))))))))



GAUSSIAN-BLUR

Blurs an image using a Gaussian filter.

C++: void GaussianBlur(InputArray src, OutputArray dst, Size ksize, double sigmaX, double sigmaY=0,
     int borderType=BORDER_DEFAULT )                   

Commom Lisp: (GAUSSIAN-BLUR (SRC MAT) (DEST MAT) (KSIZE SIZE) (SIGMA-X :DOUBLE) &OPTIONAL ((SIGMA-Y :DOUBLE) 0) 
             ((BORDER-TYPE :INT) +BORDER-DEFAULT+))

    Parameters:	

        SRC - input image; the image can have any number of channels, which are processed independently, 
              but the depth should be +8U+, +16U+, +16S+, +32F+ or +64F+.

        DST - output image of the same size and type as SRC.

        KSIZE - Gaussian kernel size KSIZE width and KSIZE height can differ but they both must be 
                positive and odd. Or, they can be zero’s and then they are computed from sigma.

        SIGMAX - Gaussian kernel standard deviation in X direction.

        SIGMAY - Gaussian kern
el standard deviation in Y direction; if SIGMAY is zero, it is set to 
                 be equal to SIGMAX, if both sigmas are zeros, they are computed from KSIZE width and 
                 KSIZE height , respectively (see (GET-GAUSSIAN-KERNEL) for details); to fully control 
                 the result regardless of possible future modifications of all this semantics, it is 
                 recommended To specify all of KSIZE, SIGMA-X, AND SIGMA-Y.

        BORDER-TYPE - Pixel extrapolation method, one of the +BORDER-*+ constants, except for 
                      +BORDER-TRANSPARENT+ and +BORDER-ISOLATED+.


The function convolves the source image with the specified Gaussian kernel. In-place filtering is s-
upported.

See also:

(SEP-FILTER-2D), FILTER-2D), (BLUR), (BOX-FILTER), (BILATERAL-FILTER), (MEDIAN-BLUR)


Example:

See BLUR-EXAMPLE in this file.



GET-STRUCTURING-ELEMENT

Returns a structuring element of the specified size and shape for morphological operations.

C++: Mat getStructuringElement(int shape, Size ksize, Point anchor=Point(-1,-1))

LISP-CV: (GET-STRUCTURING-ELEMENT (SHAPE :INT) (KSIZE SIZE) &OPTIONAL ((KERNEL POINT) (POINT -1 -1))) => MAT

    Parameters:	

        SHAPE - Element shape that could be one of the following:

            +MORPH-RECT+ - A rectangular structuring element:

            +MORPH-ELLIPSE+ - An elliptic structuring element, that is, a filled ellipse inscribed 
                              into the rectangle.

            +MORPH-CROSS+ - A cross-shaped structuring element:


        KSIZE - Size of the structuring element.


        ANCHOR - Anchor position within the element. The default value (-1 -1) means that the anchor 
                 is at the center. Note that only the shape of a cross-shaped element depends on the 
                 anchor position. In other cases the anchor just regulates how much the result of the 
                 morphological operation is shifted.

The function constructs and returns the structuring element that can be further passed to (CREATE-MORPHOLOGY-FILTER), 
(ERODE), (DILATE) or (MORPHOLOGY-EX) . But you can also construct an arbitrary binary mask yourself 
and use it as the structuring element.


;;; Global variables

(defparameter window-name "GET-STRUCTURING-ELEMENT Example")

;;; Load an image - The <lisp-cv-source-dir>/images/baboon.jpg works great with this example

(defparameter src (gc:imread "/home/w/quicklisp/dists/quicklisp/software/lisp-cv-master/images/baboon.jpg"))
(defparameter dest (gc:clone src))
(defparameter morph-elem (alloc :int 0))
(defparameter morph-size (alloc :int 0))
(defparameter morph-operator (alloc :int 0))


;;; Callback function MORPHOLOGY-OPERATIONS

(defcallback morphology-operations :void ((operation :int) (element mat))

  (setf operation (+ (? morph-operator :int) 2))

  ;;; ELEMENT is the kernel to be used. We use the function 
  ;;; GET-STRUCTURING-ELEMENT to define our own structure.

  (setf element 
	(gc:get-structuring-element (? morph-elem :int) 
				 (size 
				  (+ (* (? morph-size :int) 2) 1) 
				  (+ (* (? morph-size :int) 2) 1)) 
				 (point 
				  (? morph-size :int) 
				  (? morph-size :int))))

  ;;; Apply the specified morphology operation
  (morphology-ex src dest operation element)
  (imshow window-name dest))


(defun get-structuring-element-example ()

  (let ((window-name "GET-STRUCTURING-ELEMENT Example")
        (max-operator 4)
        (max-elem 2)
        (max-kernel-size 21))

    ;;; Create window
    (with-named-window (window-name +window-autosize+)
      (move-window window-name 759 175)

      ;;; Create Trackbar to select
      ;;; Morphology operation:

      ;;; 0: Opening 
      ;;; 1: Closing
      ;;; 2: Gradient 
      ;;; 3: Top Hat 
      ;;; 4: Black Hat

      (create-trackbar "Operator: " window-name morph-operator max-operator 
		       (callback morphology-operations))

       ;;; Create Trackbar to 
       ;;; select kernel type:

       ;;; 0: Rect 
       ;;; 1: Cross 
       ;;; 2: Ellipse

      (create-trackbar "Element: " window-name morph-elem max-elem 
		       (callback morphology-operations))

      ;;; Create Trackbar to choose kernel size

      (create-trackbar "Kernel size:"  window-name morph-size max-kernel-size 
		       (callback morphology-operations))
      (loop
         ;;; Default start
	 (imshow window-name dest)
	 (let ((c (wait-key 33)))
	   (when (= c 27)
	     (free morph-elem)
	     (free morph-size)
	     (free morph-operator)
	     (return)))))))



LAPLACIAN

Calculates the Laplacian of an image.

C++: void Laplacian(InputArray src, OutputArray dst, int ddepth, int ksize=1, double scale=1, double delta=0, 
     int borderType=BORDER_DEFAULT )

LISP-CV: (LAPLACIAN (SRC MAT) (DEST MAT) (DDEPTH :INT) &OPTIONAL ((KSIZE :INT) -1) ((SCALE :DOUBLE) 1D0) ((DELTA :DOUBLE) 0D0) 
         ((BORDER-TYPE :INT) +BORDER-DEFAULT+)) => :VOID

    Parameters:	

        SRC - Source image.

        DEST - Destination image of the same size and the same number of channels as SRC.

        DDEPTH - Desired depth of the destination image.

        KSIZE - Aperture size used to compute the second-derivative filters. See (GET-DERIV-KERNELS) 
                for details. The size must be positive and odd.

        SCALE - Optional scale factor for the computed Laplacian values. By default, no scaling is applied. 
                See (GET-DERIV-KERNELS) for details.

        DELTA - Optional delta value that is added to the results prior to storing them in DEST.

        BORDER-TYPE - Border type, one of the +BORDER-*+ constants, except 
                      for +BORDER-TRANSPARENT+ and +BORDER-ISOLATED+.


The function calculates the Laplacian of the source image by adding up the second x and y derivatives 
calculated using the Sobel operator:


See OpenCV documentation:

http://docs.opencv.org/trunk/modules/imgproc/doc/filtering.html?highlight=laplace#laplacian 

for further description and formulae.


See also:

(SOBEL), (SCHARR)


(defun laplacian-example (&optional (cam *camera-index*))

  (with-captured-camera (cap cam :width 640 :height 480)
    (let* ((window-name  "LAPLACIAN Example")) 
      (with-named-window (window-name +window-normal+)
	(set-window-property window-name +wnd-prop-fullscreen+ 
			     +window-fullscreen+)
	(set-window-property window-name +wnd-prop-aspectratio+ 
			     +window-freeratio+)
	(loop
	   (with-mat ((frame (mat)))
	     (cap-read cap frame)
	     (with-mat ((cvt (mat (rows frame) (cols frame) +8u+))
			(src (mat (rows frame) (cols frame) +8u+))
			(tmp (mat (rows frame) (cols frame) +8u+)))
	       (cvt-color frame cvt +bgr2gray+)
	       (imshow window-name (progn
				     (laplacian cvt tmp +64f+ 3)
				     (convert-scale-abs tmp src) src))))
	   (let ((c (wait-key 33)))
	     (when (= c 27)
	       (return))))))))



MEDIAN-BLUR


Blurs an image using the median filter.


C++: void medianBlur(InputArray src, OutputArray dst, int ksize)

LISP-CV: (MEDIAN-BLUR (SRC MAT) (DEST MAT) (KSIZE :INT)) => :VOID


    Parameters:	

        SRC - Input 1, 3, or 4 channel image; when KSIZE is 3 or 5, the image depth should be +8U+, 
              +16U+, or +32F+, for larger aperture sizes, it can only be +8U+.

        DEST - Destination array of the same size and type as SRC.

        KSIZE - Aperture linear size; it must be odd and greater than 1, for example: 3, 5, 7 ...


The function smoothes an image using the median filter with the (* KSIZE KSIZE) aperture. Each channel 
of a multi-channel image is processed independently. In-place operation is supported.

See also:

(BILATERAL-FILTER), (BLUR), (BOX-FILTER), (GAUSSIAN-BLUR)


Example:

See BLUR-EXAMPLE in this file.



MORPHOLOGY-EX

Performs advanced morphological transformations.

C++: void morphologyEx(InputArray src, OutputArray dst, int op, InputArray kernel, Point anchor=Point(-1,-1), int iterations=1, 
     int borderType=BORDER_CONSTANT, const Scalar& borderValue=morphologyDefaultBorderValue() )

LISP-CV: (MORPHOLOGY-EX (SRC MAT) (DEST MAT) (OP :INT) (KERNEL MAT) &OPTIONAL ((ANCHOR POINT) (POINT -1 -1)) ((ITERATIONS :INT) 1) 
        ((BORDER-TYPE :INT) +BORDER-CONSTANT+) ((BORDER-VALUE SCALAR) (MORPHOLOGY-DEFAULT-BORDER-VALUE))) => :VOID

    Parameters:	

        SRC - Source image. The number of channels can be arbitrary. The depth should be one of +8U+, 
              +16U+, +16S+, +32F+ or +64F+.

        DST - Destination image of the same size and type as SRC.

        KERNEL - Structuring element. It can be created using (GET-STRUCTURING-ELEMENT).

        ANCHOR - Anchor position with the kernel. Negative values mean that the anchor is at the kernel center.

        OP -

        Type of a morphological operation that can be one of the following:

            +MORPH-OPEN+ - An opening operation

            +MORPH-CLOSE+ - a closing operation

            +MORPH-GRADIENT+ - a morphological gradient

            +MORPH-TOPHAT+ - “top hat”

            +MORPH-BLACKHAT+ - “black hat”


        ITERATIONS - Number of times erosion and dilation are applied.

        BORDER-TYPE - Border type, one of the +BORDER-*+ constants, except 
                      for +BORDER-TRANSPARENT+ and +BORDER-ISOLATED+.

        BORDERVALUE - Border value in case of a constant border. The default value has a special 
                      meaning. See (CREATE-MORPHOLOGY-FILTER) for details.


The function can perform advanced morphological transformations using an erosion and dilation as basic operations.


See OpenCv documentation:

http://docs.opencv.org/trunk/modules/imgproc/doc/filtering.html?highlight=morphologyex#morphologyex

for a description and formulae.



(defun morphology-ex-example (filename)

   "This example basically converts text to a binary image. 
    The binary image can be used in further OCR operations. 
    A good image to use for this example is:

       <lisp-cv-source-dir>/images/webpage-1.png"

  (let ((window-name "MORPHOLOGY-EX Example"))
    (with-named-window (window-name +window-normal+)
      (set-window-property window-name +wnd-prop-fullscreen+ 
			   +window-fullscreen+) 
      (set-window-property window-name +wnd-prop-aspectratio+ 
			   +window-freeratio+)
      (with-mat ((image (imread filename 0)))
	(if (empty image) 
	    (return-from morphology-ex-example 
	      (format t "Image not loaded")))
	;Divide the image by its morphologically closed counterpart
	(with-size ((size (size 19 19)))
	  (with-mat ((kernel (get-structuring-element +morph-ellipse+ size)))
	    (with-mat ((closed (mat)))
	      (morphology-ex image closed +morph-close+ kernel)
	      ;Divide requires floating-point
	      (convert-to image image +32f+)
	      (divide image closed image 1d0 +32f+)
	      (normalize image image 0d0 255d0 +norm-minmax+)
	      ;Convert back to unsigned int
	      (convert-to image image +8u+)
	      ;Threshold each block (3x3 grid) of the image separately to
	      (dotimes (i 3)
		(dotimes (j 3)
		  (with-mat ((block (col-range 
				     (row-range image (* (round (/ (rows image) 3)) i) 
                                     (* (round (/ (rows image) 3)) (+ i 1))) 
				     (* (round (/ (cols image) 3)) j) 
				     (* (round (/ (cols image) 3)) (+ j 1)))))
		    (threshold block block -1d0 255d0(+ +thresh-binary-inv+ +thresh-otsu+)))
		  (imshow window-name image)
		  (loop
		     (let ((c (wait-key 33)))
		       (when (= c 27)
			 (return)))))))))))))



PYR-DOWN

Blurs an image and downsamples it.

C++: void pyrDown(InputArray src, OutputArray dst, const Size& dstsize=Size(), int borderType=BORDER_DEFAULT )

LISP-CV: (PYR-DOWN (SRC MAT) (DEST MAT) &OPTIONAL ((DSTSIZE SIZE) (SIZE)) 
         ((BORDER-TYPE :INT) +BORDER-DEFAULT+)) => :VOID

    Parameters:	

        SRC - Input image.

        DEST - Output image; it has the specified size and the same type as SRC.

        DSTSIZE -

        size of the output image; by default, it is computed as:

               (SIZE (/ (+ (COLS SRC) 1) 2) (/ (+ (ROW SRC) 1) 2)), 


        BORDER-TYPE - Border type, one of the +BORDER-*+ constants, except 
                      for +BORDER-TRANSPARENT+ and +BORDER-ISOLATED+.
     

The function performs the downsampling step of the Gaussian pyramid construction. First, it convolves 
the source image with the kernel, then, it downsamples the image by rejecting even rows and columns.


(defun pyr-down-example (filename)

  (defun do-pyr-down (in)
    (let* ((in-size (size in)) 
           (in-height (round (height in-size)))
	   (in-width (round (width in-size)))
	   (out (gc:mat (/ in-height 2) (/ in-width 2) +8uc3+)))

      ;;Make sure input image is divisible by two."
      (assert (and (equal (mod in-height 2) 0)
		   (equal (mod in-width 2) 0))
	      (in-height in-width)
	      "~S or ~S are not divisible by two" in-width in-height)

      ;;Blur and downsample image
      (pyr-down in out)
      out))

  (defun main (filename)
    
    (let* ((img-2 0)
	   (window-name-1 "Original image - PYR-DOWN Example")
	   (window-name-2 "Downsampled blurred Image - PYR-DOWN Example"))
      (with-mat ((img-1 (imread filename 1)))
	(if (empty img-1) 
	    (return-from main
	      (format t "Image not loaded")))
	(with-named-window (window-name-1 +window-autosize+)
	  (with-named-window (window-name-2 +window-autosize+)
	    (move-window window-name-1 533 175)
	    (move-window window-name-2 984 175)
	    (format t "~%Image size before downsampling = (~a, ~a)
       ~%~%"(rows img-1) (cols img-1))
	    ;;Show original image in window
	    (imshow window-name-1 img-1)
	    (setf img-2 (do-pyr-down img-1))
	    (format t "Image size after downsampling = (~a, ~a)
       ~%~%"(rows img-2)(cols img-2))
	    ;;Show blurred downsampled image in window
	    (imshow window-name-2 img-2)
	    (loop
	       (let ((c (wait-key 33)))
		 (when (= c 27)
		   (return)))))))))

  (main filename))



PYR-UP

Upsamples an image and then blurs it.

C++: void pyrUp(InputArray src, OutputArray dst, const Size& dstsize=Size(), int borderType=BORDER_DEFAULT )

LISP-CV: (PYR-UP (SRC MAT) (DEST MAT) &OPTIONAL ((DSTSIZE SIZE) (SIZE)) 
         ((BORDER-TYPE :INT) +BORDER-DEFAULT+)) => :VOID

    Parameters:	

        SRC - Input image.

        DEST - Output image. It has the specified size and the same type as SRC.

        DESTSIZE - Size of the output image; by default, it is computed as:

                   (SIZE (* (COLS SRC) 2) (* (ROWS SRC) 2)) 

        BORDER-TYPE - Border type, one of the +BORDER-*+ constants, except 
                      or +BORDER-TRANSPARENT+ and +BORDER-ISOLATED+.


The function performs the upsampling step of the Gaussian pyramid construction, though it can actually 
be used to construct the Laplacian pyramid. First, it upsamples the source image by injecting even 
zero rows and columns and then convolves the result with the same kernel as in (PYR-DOWN) multiplied by 4.



(defun pyr-up-example (filename)

  (defun do-pyr-up (in)
    (let* ((in-size (size in)) 
           (in-height (round (height in-size)))
	   (in-width (round (width in-size)))
	   (out (gc:mat (/ in-height 2) (/ in-width 2) +8uc3+)))

      ;;Make sure input image is divisible by two."
      (assert (and (equal (mod in-height 2) 0)
		   (equal (mod in-width 2) 0))
	      (in-height in-width)
	      "~S or ~S are not divisible by two" in-width in-height)

      ;;Blur and upsample image
      (pyr-up in out)
      out))

  (defun main (filename)
    
    (let* ((img-2 0)
	   (window-name-1 "Original image - PYR-UP Example")
	   (window-name-2 "Upsampled blurred Image - PYR-UP Example"))
      (with-mat ((img-1 (imread filename 1)))
	(if (empty img-1) 
	    (return-from main
	      (format t "Image not loaded")))
	(with-named-window (window-name-1 +window-autosize+)
	  (with-named-window (window-name-2 +window-autosize+)
	    (move-window window-name-1 533 175)
	    (move-window window-name-2 984 175)
	    (format t "~%Image size before upsampling = (~a, ~a)
       ~%~%"(rows img-1) (cols img-1))
	    ;;Show original image in window
	    (imshow window-name-1 img-1)
	    (setf img-2 (do-pyr-up img-1))
	    (format t "Image size after upsampling = (~a, ~a)
       ~%~%"(rows img-2)(cols img-2))
	    ;;Show blurred upsampled image in window
	    (imshow window-name-2 img-2)
	    (loop
	       (let ((c (wait-key 33)))
		 (when (= c 27)
		   (return)))))))))

  (main filename))



SCHARR

Calculates the first x- or y- image derivative.

C++: void Scharr(InputArray src, OutputArray dst, int ddepth, int dx, int dy, double scale=1, double delta=0, 
     int borderType=BORDER_DEFAULT )

LISP-CV: (SCHARR (SRC MAT) (DEST MAT) (DDEPTH :INT) (DX :INT) (DY :INT) &OPTIONAL ((SCALE :DOUBLE) 1) ((DELTA :DOUBLE) 1D0) 
         ((BORDER-TYPE :INT) +BORDER-DEFAULT+)) => :VOID


    Parameters:	

        SRC - Input image.

        DST - Output image of the same size and the same number of channels as SRC.

        DDEPTH - Output image depth (see SOBEL for the list of supported combination of (DEPTH SRC) and DDEPTH).

        DX - Order of the derivative x.

        DX - Order of the derivative y.

        SCALE - Optional scale factor for the computed derivative values; by default, no scaling is 
                applied (see (GET-DERIV-KERNELS) for details).

        DELTA - Optional delta value that is added to the results prior to storing them in DEST.

        BORDER-TYPE - Pixel extrapolation method, one of the +BORDER-*+ constants, except for 
                      +BORDER-TRANSPARENT+ and +BORDER-ISOLATED+. 


The function SCHARR computes the first x- or y- spatial image derivative.


See OpenCV documentation: 

http://docs.opencv.org/trunk/modules/imgproc/doc/filtering.html?highlight=scharr#scharr

for description and formulae.


See also:

(CART-TO-POLAR)


(defun scharr-example (&optional (cam 0))

  "In this example, we compare the code for the SOBEL function with the 
   same code for the SCHARR function. The SOBEL version is shown in the 
   left window and the SCHARR version in the right. This example should 
   give an idea of how this function works."

  (with-captured-camera (cap cam :width 640 :height 480)
    (let* ((window-name-1 "SOBEL - SCHARR Example")
	   (window-name-2 "SCHARR - SCHARR Example")
	   ;Declare variables 
           (scale 1d0)
	   (delta 0d0)
	   (ddepth +16s+))
      (with-named-window (window-name-1 +window-normal+)
	(with-named-window (window-name-2 +window-normal+)
	  (move-window window-name-1 533 175)
	  (move-window window-name-2 984 175)
	  (loop
	     ;Load camera feed
	     (with-mat ((frame (mat)))
	       (cap-read cap frame)
	       ;Declare more variables 
	       (with-size ((size (size 3 3)))
		 (with-mat ((src-gray (mat))
			    (grad (mat))
			    (grad-x (mat))
			    (grad-y (mat))
			    (abs-grad-x (mat))
			    (abs-grad-y (mat)))

		   ;SOBEL version:
		   
		   ;First, we apply a GAUSSIAN-BLUR to 
		   ;our image to reduce the noise 
		   (gaussian-blur frame frame  size 0d0 0d0)
		   ;Now we convert camera feed to grayscale
		   (cvt-color frame src-gray +bgr2gray+)

		   ;Then, we calculate the “derivatives” 
		   ;in x and y directions, using SOBEL

		   ;Gradient x  
		   (sobel src-gray grad-x ddepth 1 0 3 scale delta +border-default+)
		   ;Gradient y
		   (sobel src-gray grad-y ddepth 0 1 3 scale delta +border-default+)
		   ;We convert our partial 
		   ;results back to +8U+
                   (convert-scale-abs grad-x abs-grad-x)
		   (convert-scale-abs grad-y abs-grad-y)
		   ;Add both directional gradients
		   (add-weighted abs-grad-x 0.5d0 abs-grad-y 0.5d0 0d0 grad)
		   ;Then, show SOBEL version in a window
		   (imshow window-name-1 grad)

	           ;SCHARR version:

		   ;Apply a GAUSSIAN-BLUR
		   (gaussian-blur frame frame  size 0d0 0d0)
		   ;Convert camera feed to grayscale
		   (cvt-color frame src-gray +bgr2gray+)
		   ;Calculate the “derivatives" in the 
		   ;x and y directions, using SCHARR
		   (scharr src-gray grad-x ddepth 1 0 scale delta +border-default+)
                   (scharr src-gray grad-y ddepth 0 1 scale delta +border-default+)
		   ;Convert results back to +8U+      
		   (convert-scale-abs grad-x abs-grad-x)
		   (convert-scale-abs grad-y abs-grad-y)
		   ;Add both directional gradients
		   (add-weighted abs-grad-x 0.5d0 abs-grad-y 0.5d0 0d0 grad)
		   ;Show SCHARR version in a window
		   (imshow window-name-2 grad)
		   (let ((c (wait-key 33)))
		     (when (= c 27)
		       (return))))))))))))



SOBEL

Calculates the first, second, third, or mixed image derivatives using an extended Sobel operator.

C++: void Sobel(InputArray src, OutputArray dst, int ddepth, int dx, int dy, int ksize=3, double scale=1, 
     double delta=0, int borderType=BORDER_DEFAULT)

LISP-CV: (SOBEL (SRC MAT) (DEST MAT) (DDEPTH :INT) (DX :INT) (DY :INT) &OPTIONAL ((KSIZE :INT) 3) 
         ((SCALE :DOUBLE) 1D0) ((DELTA :DOUBLE) 0D0) ((BORDER-TYPE :INT) +BORDER-DEFAULT+))  => :VOID


    Parameters:	

        SRC - Input image.

        DEST - Output image of the same size and the same number of channels as SRC.

        DDEPTH -

        Output image depth; the following combinations of (DEPTH SRC) and DDEPTH are supported:

                (EQ (DEPTH SRC) +8U+): (EQ DDEPTH (OR -1 +16S+ +32F+ +64F+))

                (EQ (DEPTH SRC) (OR +16U+ +16S+)) -> (EQ DDEPTH (OR -1 +32F+ +64F+))

                (EQ (DEPTH SRC) +32F+) -> (EQ DDEPTH (OR -1 +32F+ +64F+))

                (EQ (DEPTH SRC) +64F+) -> (EQ DDEPTH (OR -1 +64F+))

        when (EQ DDEPTH -1), the destination image will have the same depth as the source; in the 
        case of 8-bit input images it will result in truncated derivatives.

        DX - Order of the derivative x.

        DY - Order of the derivative y.

        KSIZE - Size of the extended Sobel kernel; it must be 1, 3, 5, or 7.

        SCALE - Optional scale factor for the computed derivative values; by default, no scaling is 
                applied (see (GET-DERIV-KERNELS) for details).

        DELTA - Optional delta value that is added to the results prior to storing them in DEST.

        BORDER-TYPE - Pixel extrapolation method, one of the +BORDER-*+ constants, except for 
                      +BORDER-TRANSPARENT+ and +BORDER-ISOLATED+. 


Description: See OpenCv documentation:


http://docs.opencv.org/modules/imgproc/doc/filtering.html?highlight=sobel#sobel


for a description and formulae.


See also:

(SCHARR), (LAPLACIAN), (SEP-FILTER2D), FILTER-2D), (GAUSSIAN-BLUR), (CART-TO-POLAR)


Example 1:


(defun sobel-example-1 (&optional (cam *camera-index*))

  (with-captured-camera (cap cam :width 640 :height 480)
    (let* ((window-name  "SOBEL Example 1")
           (ddepth +32f+)) 
      (with-named-window (window-name +window-normal+)
       	(set-window-property window-name +wnd-prop-fullscreen+ 
			     +window-fullscreen+)
	(set-window-property window-name +wnd-prop-aspectratio+ 
			     +window-freeratio+)
	(with-mat ((gray (mat))
		   (sobelx (mat)))
	  (with-object ((minval (alloc :double 0d0))
			(maxval (alloc :double 0d0)))
	    (with-mat ((draw (mat)))
	      (loop
		 (with-mat ((frame (mat)))
		   (cap-read cap frame)
		   ;Show original camera 
		   ;output in a window
		   (imshow window-name frame)
		   ;Convert camera output, a 3 channel 
		   ;matrix, to 1 channel matrix
		   (cvt-color frame gray +bgr2gray+)
		   ;Compute Sobel x derivative and set 
		   ;to destination matrix SOBELX
		   (sobel gray sobelx ddepth 0 1 -7))
		 ;Find minimum and maximum intensities
		 (min-max-loc sobelx minval maxval)
		 (format t "MINVAL: ~a~%~%" (mem-aref minval :double 0))
		 (format t "MAXVAL: ~a~%~%" (mem-aref maxval :double 0))
		 ;+32F+ image needs to be converted to +8U+ type
		 (convert-to sobelx draw +8u+ (/ 255d0 (- (? maxval :double) 
							  (? minval :double))) 
			     (* (* (? minval :double) -1.283)  
				(/ 255.d0 (- (? maxval :double) 
					     
					     (? minval :double)))))
		 (imshow window-name draw)
		 (let ((c (wait-key 33)))
		   (when (= c 27)
		     (return)))))))))))



Example 2:


(defun sobel-example-2 (&optional (cam *camera-index*))

  (with-captured-camera (cap cam :width 640 :height 480)
    (let* ((window-name  "SOBEL Example")) 
      (with-named-window (window-name +window-normal+)
	(set-window-property window-name +wnd-prop-fullscreen+ 
			     +window-fullscreen+)
	(set-window-property window-name +wnd-prop-aspectratio+ 
			     +window-freeratio+)
	(loop
	   (with-mat ((frame (mat)))
	     (cap-read cap frame)
	     (with-mat ((cvt (mat (rows frame) (cols frame) +8u+))
			(src (mat (rows frame) (cols frame) +8u+))
			(tmp (mat (rows frame) (cols frame) +8u+)))
	       (cvt-color frame cvt +bgr2gray+)
	       (imshow window-name (progn
				     (sobel cvt tmp +32f+ 0 1 -1)
				     (convert-scale-abs tmp src) src))))
	(let ((c (wait-key 33)))
	  (when (= c 27)
	    (return))))))))



IMGPROC - GEOMETRIC IMAGE TRANSFORMATIONS:



REMAP

Applies a generic geometrical transformation to an image.

C++: void remap(InputArray src, OutputArray dst, InputArray map1, InputArray map2, int interpolation, int borderMode=BORDER_CONSTANT, const Scalar& borderValue=Scalar())

LISP-CV: (REMAP (SRC MAT) (DEST MAT) (MAP1 MAT) (MAP2 MAT) (INTERPOLATION :INT) &OPTIONAL ((BORDER-MODE :INT) +BORDER-CONSTANT+) 
         ((BORDER-VALUE SCALAR) (SCALAR))) => :VOID

    Parameters:	

        SRC - Source image.

        DST - Destination image. It has the same size as MAP1 and the same type as SRC.

        MAP1 - The first map of either (x,y) points or just x values having the type +16SC2+, +32FC1+, 
               or +32FC2+ . See (CONVERT-MAPS) for details on converting a floating point representation 
               to fixed-point for speed.

        MAP2 - The second map of y values having the type +16UC1+ , +32FC1+ , or none (empty map if 
               MAP1 is (x,y) points), respectively.

        INTERPOLATION - Interpolation method (see (RESIZE)). The method +INTER-AREA+ is not supported by this function.

        BORDER-MODE - Pixel extrapolation method (see (BORDER-INTERPOLATE)). 

           When (EQ BORDER-MODE +BORDER-TRANSPARENT+) , it means that the pixels in the destination 
           image that corresponds to the “outliers” in the source image are not modified by the 
           function.

        BORDER-VALUE - Value used in case of a constant border. By default, it is 0.


The function remap transforms the source image using the specified map:

see OpenCV documentation:

http://docs.opencv.org/trunk/modules/imgproc/doc/geometric_transformations.html?highlight=remap#remap

for description and formula:

This function cannot operate in-place.


Example:


;Global variables

(defparameter remap-window "REMAP Example")
(defparameter ind 0)

;Load the image
(defparameter src (imread "/home/user/my-pic.jpg" 1))

;Create DST, MAP-X and MAP-Y with the same size as SRC
(defparameter dst (gc:mat (rows src) (cols src) (mat-type src)))

(defparameter map-x (gc:mat (rows src) (cols src) +32f+))
(defparameter map-y (gc:mat (rows src) (cols src) +32f+))


;Fill the MAP-X and MAP-Y matrices 
;with 4 types of mappings
(defun update-map () 
  
  (setf ind (mod ind 4))

  (dotimes (j (rows src))
    (dotimes (i (cols src))

      (cond ((= 0 ind)

             (if (and (> i (* (cols src) 0.25)) 
                      (< i (* (cols src) 0.75)) 
		      (> j (* (rows src) 0.25)) 
                      (< j (* (rows src) 0.75)))
		 
                 (progn (setf (at map-x j i :float) 
			      (+ (* 2 (- i (* (cols src) 0.25))) 0.5)) 
			(setf (at map-y j i :float) 
			      (+ (* 2 (- j (* (rows src) 0.25))) 0.5))) 
		 
		 (progn (setf (at map-x j i :float) 0f0)
			(setf (at map-y j i :float) 0f0))))
	    
	    ((= 1 ind)

	     (progn (setf (at map-x j i :float) 
			  (coerce i 'float))
		    (setf (at map-y j i :float) 
			  (-  (coerce (rows src) 'float)  j))))

	    ((= 2 ind)

	     (progn (setf (at map-x j i :float) 
			  (- (coerce (cols src) 'float) i))
		    (setf (at map-y j i :float) 
			  (coerce j 'float))))

	    ((= 3 ind) 

	     (progn (setf (at map-x j i :float) 
			  (- (coerce (cols src) 'float) i))
		    (setf (at map-y j i :float) 
			  (- (coerce (rows src) 'float) j)))))))
  (incf ind))


(defun remap-example ()

  (with-named-window (remap-window +window-autosize+)
    (move-window remap-window (cols src) 175)

    (loop
       ;Update MAP-X and MAP-Y. Then apply REMAP
       (update-map)
       (remap src dst map-x map-y +inter-linear+ 
	      +border-constant+ (gc:scalar 0 0 0))
       ;Display results
       (imshow remap-window dst)
       ;Each 1 sec. Press ESC to exit the program
       (let ((c (wait-key 1000)))
	 (when (= c 27)
	   (return))))))



RESIZE


Resizes an image.

C++: void resize(InputArray src, OutputArray dst, Size dsize, double fx=0, double fy=0, int interpolation=INTER_LINEAR )

LISP-CV: (RESIZE (SRC MAT) (DEST MAT) (DSIZE SIZE) &OPTIONAL ((FX :DOUBLE) 0D0) ((FY :DOUBLE) 0D0) 
         ((INTERPOLATION :INT) +INTER-LINEAR+)) => :VOID


    Parameters:	

        SRC - Input image.

        DEST - Output image; it has the size DSIZE (when it is non-zero) or the size computed from 
              (SIZE SRC), FX, and FY; the type of DEST is the same as of SRC.

        DSIZE - Output image size.

        FX - Scale factor along the horizontal axis.

        FY - Scale factor along the vertical axis.

        INTERPOLATION -

        interpolation method:

            +INTER-NEAREST+ - A nearest-neighbor interpolation

            +INTER-LINEAR+ - A bilinear interpolation (used by default)

            +INTER-AREA+ - Resampling using pixel area relation. It may be a preferred method for image 
                           decimation, as it gives moire’-free results. But when the image is zoomed, it 
                           is similar to the +INTER-NEAREST+ method.

            +INTER-CUBIC+ - a bicubic interpolation over 4x4 pixel neighborhood

            +INTER-LANCZOS4+ - a Lanczos interpolation over 8x8 pixel neighborhood


The function RESIZE resizes the image SRC down to or up to the specified size. Note that the initial 
DEST type or size are not taken into account. Instead, the size and type are derived from the SRC, 
DSIZE, FX , and FY . If you want to resize SRC so that it fits the pre-created DEST , you may call 
the function as follows:


;; Explicitly specify (EQ DSIZE (SIZE DEST)) FX and FY will be computed from that.


(RESIZE SRC DEST (SIZE DEST) 0d0 0d0 INTERPOLATION)


If you want to decimate the image by factor of 2 in each direction, you can call the function this way:


;; Specify FX and FY and let the function compute the destination image size.


(RESIZE SRC DEST (SIZE) 0.5d0 0.5d0 INTERPOLATION)


To shrink an image, it will generally look best with +INTER-AREA+ interpolation, whereas to enlarge 
an image, it will generally look best with +INTER-CUBIC+ (slow) or +INTER-LINEAR+ (faster but still 
looks OK).


See also:

(WARP-AFFINE), (WARP-PERSPECTIVE), (REMAP)


(defun resize-example (&optional (camera-index *camera-index*) 
			 (width 640)
			 (height 480))

  "Uses RESIZE to enlarge the camera output and then shows
   both the resized FRAME(RESIZED) and the original FRAME 
   a window"

  (with-capture (cap (video-capture camera-index))
    (let* ((window-name-1 "Original FRAME - RESIZE Example")
	   (window-name-2 "RESIZED - RESIZE Example")
	   (resized (mat (round (* height 1.5)) (round (* width 1.35)) +8uc3+)))
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-height+ height)
      (named-window window-name-1 +window-autosize+)
      (named-window window-name-2 +window-autosize+)
      (move-window window-name-1 0 0)
      (move-window window-name-2 650 175)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
        (resize frame resized (size 1280 960) 0.0d0 
		0.0d0 +inter-lanczos4+)
	(imshow window-name-1 frame)
        (imshow window-name-2 resized))
      (destroy-all-windows))))




IMGPROC - MISCELLANEOUS IMAGE TRANSFORMATIONS:



ADAPTIVE-THRESHOLD


Applies an adaptive threshold to an array.


C++: void adaptiveThreshold(InputArray src, OutputArray dst, double maxValue, int adaptiveMethod, int thresholdType, 
     int blockSize, double C)

LISP-CV: (ADAPTIVE-THRESHOLD (SRC MAT) (DEST MAT) (MAX-VALUE :DOUBLE) (ADAPTIVE-METHOD :INT) (THRESHOLD-TYPE :INT) 
         (BLOCKSIZE :INT) (C :DOUBLE))


    Parameters:	

        SRC - Source 8-bit single-channel image.

        DEST - Destination image of the same size and the same type as SRC.

        MAX-VALUE - Non-zero value assigned to the pixels for which the condition is satisfied. 
                    See the details below.

        ADAPTIVE-METHOD - Adaptive thresholding algorithm to use, +ADAPTIVE-THRESH-MEAN-C+ or 
                          +ADAPTIVE-THRESH-GAUSSIAN-C+ . See the details below.

        THRESHOLD-TYPE - Thresholding type that must be either +THRESH-BINARY+ or +THRESH-BINARY-INV+.

        BLOCK-SIZE - Size of a pixel neighborhood that is used to calculate a threshold value for the 
                     pixel: 3, 5, 7, and so on.

        C - Constant subtracted from the mean or weighted mean (see the details below). Normally, it is 
            positive but may be zero or negative as well.


The function transforms a grayscale image to a binary image according to the formulae:

See OpenCV documentation for a description and formulae:

http://docs.opencv.org/trunk/modules/imgproc/doc/miscellaneous_transformations.html?highlight=adaptiveth#adaptivethreshold

The function can process the image in-place.

See also:

(THRESHOLD), (BLUR), (GAUSSIAN-BLUR)


;;Example in process




DISTANCE-TRANSFORM

Calculates the distance to the closest zero pixel for each pixel of the source image.

C++: void fscalardistanceTransform(InputArray src, OutputArray dst, int distanceType, int maskSize, int dstType=CV_32F )

LISP-CV: (DISTANCE-TRANSFORM (SRC MAT) (DEST MAT) (DISTANCE-TYPE :INT) (MASK-SIZE :INT)) => :VOID

C++: void distanceTransform(InputArray src, OutputArray dst, OutputArray labels, int distanceType, int maskSize, 
     int labelType=DIST_LABEL_CCOMP )

LISP-CV: (DISTANCE-TRANSFORM (SRC MAT) (DEST MAT) (*LABELS MAT) (DISTANCE-TYPE :INT) (MASK-SIZE :INT) &OPTIONAL 
         ((LABEL-TYPE :INT) +DIST-LABEL-CCOMP+))

    Parameters:	

        SRC - 8-bit, single-channel (binary) source image.

        DST - Output image with calculated distances. It is a 8-bit or 32-bit floating-point, 
              single-channel image of the same size as SRC.

        DISTANCE-TYPE - Type of distance. It can be +DIST-L1+, +DIST-L2+ , or +DIST-C+ .

        MASK-SIZE - Size of the distance transform mask. It can be 3, 5, or +DIST-MASK-PRECISE+ (the 
                    latter option is only supported by the first function). In case of the +DIST-L1+ 
                    or +DIST-C+ distance type, the parameter is forced to 3 because a 3 X 3 mask gives 
                    the same result as 5 X 5 or any larger aperture.

        DST-TYPE - Type of output image. It can be +8U+ or +32F+. Type +8U+ can be used only for the 
                   first variant of the function and (EQ DISTANCE-TYPE +DIST-L1+).

        LABELS - Optional output 2D array of labels (the discrete Voronoi diagram). It has the type 
                 +32SC1+ and the same size as SRC . See the details below.

        LABELTYPE - Type of the label array to build. If (EQ LABEL-TYPE +DIST-LABEL-CCOMP+) then each 
                    connected component of zeros in SRc (as well as all the non-zero pixels closest 
                    to the connected component) will be assigned the same label. If (EQ label-Type 
                    +DIST-LABEL-PIXEL+ then each zero pixel (and all the non-zero pixels closest to 
                    it) gets its own label.

The DISTANCE-TRANSFORM functions calculate the approximate or precise distance from every binary image 
pixel to the nearest zero pixel. For zero image pixels, the distance will obviously be zero.

When (EQ MASK-SIZE +DIST-MASK-PRECISE+) and (EQ DISTANCE-TYPE +DIST-L2+) , the function runs the algorithm 
described in [Felzenszwalb04]. This algorithm is parallelized with the TBB library.

In other cases, the algorithm [Borgefors86] is used. This means that for a pixel the function finds the 
shortest path to the nearest zero pixel consisting of basic shifts: horizontal, vertical, diagonal, or 
knight’s move (the latest is available for a 5 x 5 mask). The overall distance is calculated as a sum of 
these basic distances. Since the distance function should be symmetric, all of the horizontal and vertical 
shifts must have the same cost (denoted as a ), all the diagonal shifts must have the same cost (denoted as 
b ), and all knight’s moves must have the same cost (denoted as c ). For the +DIST-C+ and +DIST-L1+ types, 
the distance is calculated precisely, whereas for +DIST-L2+ (Euclidean distance) the distance can be calcu-
lated only with a relative error (a 5 x 5 mask gives more accurate results). For a,``b`` , and c , OpenCV 
uses the values suggested in the original paper:

+DIST-C+ 	(3 x 3) 	(EQ A 1), (EQ B 1)
+DIST-L1+ 	(3 x 3) 	(EQ A 1), (EQ B 1)
+DIST-L2+ 	(3 x 3) 	(EQ A 0.955), (EQ B 1.3693)
+DIST-L2+ 	(5 x 5) 	(EQ A 1), (EQ B 1.4), (EQ C 2.1969)

Typically, for a fast, coarse distance estimation +DIST-L2+, a 3 x 3 mask is used. For a more accurate 
distance estimation +DIST-L2+, a 5 x 5 mask or the precise algorithm is used. Note that both the precise 
and the approximate algorithms are linear on the number of pixels.

The second variant of the function does not only compute the minimum distance for each pixel (x, y) 
but also identifies the nearest connected component consisting of zero pixels (EQ LABEL-TYPE +DIST-
LABEL-CCOMP+) or the nearest zero pixel (EQ LABEL-TYPE +DIST-LABEL-PIXEL+). Index of the component/
pixel is stored (LABELS X Y) . When (EQ LABEL-TYPE +DIST-LABEL-CCOMP+), the function automatically 
finds connected components of zero pixels in the input image and marks them with distinct labels. 
When (EQ LABEL-TYPE +DIST-LABEL-CCOMP+), the function scans through the input image and marks all 
the zero pixels with distinct labels.

In this mode, the complexity is still linear, the function provides a very fast way to compute the 
Voronoi diagram for a binary image. The second variant can use only the approximate distance transform 
algorithm currently,  i.e. (EQ MASK-SIZE +DIST-MASK-PRECISE+) is not supported yet.



(defun distance-transform-example (&optional (cam 0) 
				     (width *default-width*)
				     (height *default-height*))

  "I came up with an amazing 3D visual effect that changes as you move 
   in front of the camera. I used the functions CANNY, THRESHOLD, DIST-
   ANCE-TRANSFORM and NORMALIZE to do this. I left the trackbar locati-
   ons at the position of my discovery. Try retracing my steps using t-
   he trackbars to get a better idea of how these functions are working 
   together to create this effect."

  (let ((window-name "DISTANCE-TRANSFORM Example"))
    (with-named-window (window-name +window-normal+)
      (set-window-property window-name +wnd-prop-fullscreen+ 
			   +window-fullscreen+)
      (set-window-property window-name +wnd-prop-aspectratio+ 
			   +window-freeratio+)
      (with-captured-camera (cap cam :width width :height height)
	(with-mat ((src (mat height width +8u+))
		   (dst (mat height width +8u+))
		   (final (mat height width +32f+)))
	  (with-object ((canny-1 (alloc :int 471))
			(canny-2 (alloc :int 128))
			(threshold (alloc :int +thresh-binary-inv+))
			(dist-trans (alloc :int +dist-c+)))
	    (loop
	       (with-mat ((frame (mat)))
		 (cap-read cap frame)
		 (cvt-color frame src +bgr2gray+)
		 (create-trackbar "canny-1" window-name canny-1 500)
		 (create-trackbar "canny-2" window-name canny-2 500)
		 (create-trackbar "threshold" window-name threshold 4)
		 (create-trackbar "dist-trans" window-name dist-trans 3)
		 (canny src dst (coerce (? canny-1 :int) 'double-float) 
			(coerce (? canny-2 :int) 'double-float))
		 (threshold dst dst 1d0 255d0 (? threshold :int))
		 (if (< (get-trackbar-pos "dist-trans" window-name) 1) 
		     (set-trackbar-pos "dist-trans" window-name 1) nil)
		 (distance-transform dst final (? dist-trans :int) 3)
		 (normalize final final 0.0d0 1.0d0 +norm-minmax+)
		 (imshow window-name final))
	       (let ((c (wait-key 33)))
		 (when (= c 27)
		   (return))))))))))



THRESHOLD

Applies a fixed-level threshold to each array element.

C++: double threshold(InputArray src, OutputArray dst, double thresh, double maxval, int type)

LISP-CV: (THRESHOLD (SRC MAT) (DEST MAT) (THRESH :DOUBLE) (MAX-VAL :DOUBLE) (TYPE :INT)) => :DOUBLE


    Parameters:	

        SRC - Input array (single-channel, 8-bit or 32-bit floating point).

        DEST - Output array of the same size and type as SRC.

        THRESH - Threshold value.

        MAX-VAL - Maximum value to use with the +THRESH-BINARY+ and +THRESH-BINARY-INV+ thresholding types.

        TYPE - Thresholding type (see the details below).


The function applies fixed-level thresholding to a single-channel array. The function is typically 
used to get a bi-level (binary) image out of a grayscale image ( (COMPARE) could be also used for 
this purpose) or for removing a noise, that is, filtering out pixels with too small or too large 
values. There are several types of thresholding supported by the function. 

They are determined by type:

                        +THRESH-BINARY+

                        +THRESH-BINARY-INV+

                        +THRESH-TRUNC+

                        +THRESH-TOZERO+

                        +THRESH-TOZERO-INV+


Also, the special value +THRESH-OTSU+ may be combined with one of the above values. In this case, 
the function determines the optimal threshold value using the Otsu’s algorithm and uses it instead 
of the specified thresh . The function returns the computed threshold value. Currently, the Otsu’s 
method is implemented only for 8-bit images. 

See also:

(ADAPTIVE-THRESHOLD), (FIND-CONTOURS), (COMPARE), (MIN), (MAX)


(defun threshold-example (&optional (cam 0) (width 640) (height 480))

  "Show the camera output and a thresholded version in a single window."

  (with-captured-camera (cap cam :width width :height height)
    (let* ((window-name "Camera/Threshold"))
      (with-named-window (window-name +window-autosize+)
	(move-window window-name 333 175)
	(loop
	   ;Set camera feed to FRAME
	   (with-mat ((frame (mat)))
	     (cap-read cap frame)
	     (with-mat ((grayscale (mat height width +8u+))
			(threshold (mat height width +8u+))
			(threshold3 (mat height width +8uc3+))
			;Create a double wide window to show the camera 
			;output and a thresholded camera output in
			(window (mat height (* width 2) +8uc3+)))
	       ;Convert FRAME to a 1 channel grayscale 
	       ;image and assign to GRAYSCALE
	       (cvt-color frame grayscale +bgr2gray+)
	       ;Apply a fixed-level threshold to 
	       ;each array element of GRAYSCALE
	       (threshold grayscale threshold 128d0 255d0 +thresh-binary+)
	       ;Convert threshold back to a 3 channel 
               ;BGR image and assign to THRESHOLD3
	       (cvt-color threshold threshold3 +gray2bgr+)
	       ;Set WINDOW roi to the left half
	       (with-mat ((a (adjust-roi window 0 0 0 (* (cols threshold3) -1))))
		 ;Copy original camera feed(FRAME) to WINDOW 
		 (copy-to frame window)
		 ;Set WINDOW roi to the right half
		 (with-mat ((b (adjust-roi window 0 0 (* (cols frame) -1) 
					   (cols threshold3))))
		   ;Copy thresholded camera feed to WINDOW
		   (copy-to threshold3 window)
		   ;Restore original roi
		   (with-mat ((c (adjust-roi window 0 0 (cols frame) 0)))
		     ;Show WINDOW in a window
		     (imshow window-name window)
		     (let ((c (wait-key 33)))
		       (when (= c 27)
			 (return)))))))))))))



IMGPROC - HISTOGRAMS:



EQUALIZE-HIST


Equalizes the histogram of a grayscale image.


C++: void equalizeHist(InputArray src, OutputArray dst)

LISP-CV: (EQUALIZE-HIST (SRC MAT) (DEST MAT)) => :VOID


    Parameters:	

        SRC - Source 8-bit single channel image.

        DST - Destination image of the same size and type as SRC.


The function equalizes the histogram of the input image using the following algorithm:

See OpenCV documentation for algorithm: 

http://docs.opencv.org/modules/imgproc/doc/histograms.html?highlight=equalizeh#equalizehist


(defun equalize-hist-example (&optional 
				(camera-index *camera-index*) 
				(width *default-width*)
				(height *default-height*))

  (with-capture (cap (video-capture camera-index))
    (let* ((window-name-1 "Original FRAME - EQUALIZE-HIST Example")
           (window-name-2 "1 channel FRAME - EQUALIZE-HIST Example")
           (window-name-3 "Equalized FRAME - EQUALIZE-HIST Example"))
      (if (not (cap-is-open cap)) 
	  (return-from equalize-hist-example 
	    (format t "Cannot open the video camera")))
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-height+ height)
      (format t "~%Frame Size : ~ax~a~%~%" 
	      (cap-get cap +cap-prop-frame-width+)
	      (cap-get cap +cap-prop-frame-height+))
      (with-named-window (window-name-1 +window-autosize+)
	(with-named-window (window-name-2 +window-autosize+)
	  (with-named-window (window-name-3 +window-autosize+)
	    (move-window window-name-1 184 175)
	    (move-window window-name-2 634 175)
	    (move-window window-name-3 1084 175)
	    (loop
	       (with-mat ((frame (mat)))
		 (cap-read cap frame)
		 (with-mat ((frame-gray (mat)))
		   ;Show FRAME in a window
		   (imshow window-name-1 frame)
		   ;Convert FRAME to 1 channel 
		   ;image, FRAME-GRAY
		   (cvt-color frame frame-gray +bgr2gray+)
		   ;Show FRAME-GRAY in a window
		   (imshow window-name-2 frame-gray)
		   ;Run EQUALIZE-HIST on FRAME-GRAY
		   (equalize-hist frame-gray frame-gray)
		   ;Show FRAME-GRAY in a window
		   (imshow window-name-3 frame-gray)
		   (let ((c (wait-key 33)))
		     (when (= c 27)
		       (return))))))))))))



IMGPROC - FEATURE DETECTION:


CANNY

Finds edges in an image using the [Canny86] algorithm.

C++: void Canny(InputArray image, OutputArray edges, double threshold1, double threshold2, int apertureSize=3, bool L2gradient=false)

LISP-CV: (CANNY (IMAGE MAT) (EDGES MAT) (THRESHOLD1 :DOUBLE) (THRESHOLD2 :DOUBLE) ((APERTURE-SIZE :INT) 3) 
               ((L2-GRADIENT :BOOLEAN) NIL)) => :VOID

    Parameters:	

        IMAGE - Single-channel 8-bit input image.

        EDGES - Output edge map; it has the same size and type as image.

        THRESHOLD1 - First threshold for the hysteresis procedure.

        THRESHOLD2 - Second threshold for the hysteresis procedure.

        APERTURE-SIZE - Aperture size for the (SOBEL) operator.

        L2-GRADIENT - A flag, indicating whether a more accurate L2 norm should be used to calculate 
                      the image gradient magnitude (EQ L2-GRADIENT T), or whether the default L1 norm 
                      is enough (EQ L2-GRADIENT NIL).

The function finds edges in the input image IMAGE and marks them in the output map edges using the 
Canny algorithm. The smallest value between THRESHOLD1 and THRESHOLD2 is used for edge linking. The 
largest value is used to find initial segments of strong edges. 

See: http://en.wikipedia.org/wiki/Canny_edge_detector


(defun canny-example (&optional (cam *camera-index*))

  (with-captured-camera (cap cam :width 640 :height 480)
    (let* ((window-name  "CANNY Example")
	   (aperture-size 3)
           (l2-gradient nil)) 
      (with-named-window (window-name +window-normal+)
	(set-window-property window-name +wnd-prop-fullscreen+ 
			     +window-fullscreen+)
	(set-window-property window-name +wnd-prop-aspectratio+ 
			     +window-freeratio+)
	;Allocate int pointers for trackbars to adjust
	(with-object ((low-thresh (alloc :int 50)) 
		      (high-thresh (alloc :int 60))
		      (l2-gradient-switch (alloc :int 0)))
	  (loop
	     (with-mat ((frame (mat)))
	       (cap-read cap frame)
	       ;Clone FRAME
	       (with-mat ((clone (clone frame))
		          ;Create destination matrix 
                          ;half the size of FRAME
			  (out (mat (/ (cols frame) 2) 
					  (/ (rows frame) 2) +8uc3+)))
		 (create-trackbar "LOW-THRESH" window-name low-thresh 500)                         
		 (create-trackbar "HIGH-THRESH" window-name high-thresh 500)
		 (create-trackbar "L2-GRADIENT" window-name l2-gradient-switch 1)
		 (if (eq (? l2-gradient-switch :int) 1) (setf l2-gradient t) 
		     (setf l2-gradient nil))
		 ;Convert CLONE to a 1 channel grayscale image.
		 (cvt-color clone clone +bgr2gray+)
		 ;Blur and downsample CLONE
		 (pyr-down clone out)
		 ;Detect edges in camera feed. The LOW-THRESH, 
		 ;HIGH-THRESH and L2-GRADIENT parameters can 
		 ;be changed by sliding the trackbars
		 (canny out out (coerce (? low-thresh :int) 'double-float) 
			(coerce (? high-thresh :int) 'double-float) 
			aperture-size l2-gradient)
		 ;Show result in window
		 (imshow window-name out)))	     
	     (let ((c (wait-key 33)))
	       (when (= c 27)
		 (return)))))))))


IMGPROC - OBJECT DETECTION:


MATCH-TEMPLATE:

Compares a template against overlapped image regions.

C++: void matchTemplate(InputArray image, InputArray templ, OutputArray result, int method)

LISP-CV: (MATCH-TEMPLATE (IMAGE MAT) (TEMPL MAT) (RESULT MAT) 
         (METHOD :INT))) => :VOID


    Parameters:	

        IMAGE - Image where the search is running. It must be 8-bit or 32-bit floating-point.

        TEMPL - Searched template. It must be not greater than the source image and have the 
                same data type.

        RESULT - Map of comparison results. It must be single-channel 32-bit floating-point. 
                 If image is (* W H) and templ is (* w h), then result is: (* (+ (- W w) 1) (+ (- H h) 1)).

        METHOD - Parameter specifying the comparison method (see below).


The function slides through image , compares the overlapped patches of size (* w h) against TEMPL 
using the specified method and stores the comparison results in RESULT. (See OpenCV documentation:
 
http://docs.opencv.org/modules/imgproc/doc/object_detection.html?highlight=matchtem#matchtemplate 

for the formulae for the available comparison methods). 

After the function finishes the comparison, the best matches can be found as global minimums (when 
+TM-SQDIFF+ was used) or maximums (when +TM-CCORR+ or +TM-CCOEFF+ was used) using the (MIN-MAX-LOC) 
function. In case of a color image, template summation in the numerator and each sum in the denominator 
is done over all of the channels and separate mean values are used for each channel. That is, the 
function can take a color template and a color image. The result will still be a single-channel image, 
which is easier to analyze.


Example 1:


(defun match-template-example-1 (&optional (camera-index *camera-index*) 
				   (width *default-width*)
				   (height *default-height*))

  "Here a template image extracted from a frame of the camera feed i-
   s compared to that frame to find the area most similiar to the te-
   mplate image in the camera feed. 
   
   The function: 

   (MATCH-TEMPLATE IMAGE TEMPL RESULT METHOD)
 
   is used for the matching. The last parameter chooses the method o-
   f template matching. We use all six methods, shown in six windows 
   starting with square difference matching (SQDIFF).

   Note the use of (NORMALIZE) in this code, which allows us to show 
   the results in a consistent way (recall that some of the matching 
   methods can return negative-valued results. We use the +NORM-MIN-
   MAX+ flag when normalizing; this tells the function to shift and 
   scale the floating-point images so that all returned values are b-
   etween 0 and 1.

   Matches are indicated by dark areas in the left column of black 
   and white images and by bright spots in the other two columns
  
   Position the rectangle in the top-leftmost window to select the 
   template MATCH-TEMPLATE uses to find objects in camera feed by 
   moving the trackbar sliders in the bottom-leftmost window"

  (with-capture (cap (video-capture camera-index))
    ;Create array of window names
    (let* ((window-name-arr (make-array 8 :initial-contents 
					(list "SRC - MATCH-TEMPLATE-EXAMPLE-1"
					      "FRAME - MATCH-TEMPLATE-EXAMPLE-1"
					      "SQDIFF - MATCH-TEMPLATE-EXAMPLE-1"
					      "SQDIFF-NORMED - MATCH-TEMPLATE-EXAMPLE-1"
					      "CCORR - MATCH-TEMPLATE-EXAMPLE-1"
					      "CCORR-NORMED - MATCH-TEMPLATE-EXAMPLE-1"
					      "COEFF - MATCH-TEMPLATE-EXAMPLE-1"
					      "COEFF-NORMED - MATCH-TEMPLATE-EXAMPLE-1")))
	   ;Initialize size parameters 
	   ;for the matches
	   (iwidth 0)
	   (iheight 0)
	   (arr (make-array '(6)))
           (n 10))      
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-width+ height)
      ;;Create array of windows
      (dotimes (i 8)
	(named-window (aref window-name-arr i) +window-normal+))
      ;Move windows to specified locations     
      (move-window (aref window-name-arr 0) 134 0)
      (move-window (aref window-name-arr 1) 134 400)
      (move-window (aref window-name-arr 2) 551 0)
      (move-window (aref window-name-arr 3) 551 400)
      (move-window (aref window-name-arr 4) 968 0)
      (move-window (aref window-name-arr 5) 968 400)
      (move-window (aref window-name-arr 6) 1385 0)
      (move-window (aref window-name-arr 7) 1385 400)
      ;Allocate int pointers for the trackbars to 
      ;adjust which will set the template image a-
      ;nd the guiding rectangle location and boun-
      ;daries
      (with-object ((rect-x (alloc :int '(0))))
	(with-object ((rect-y (alloc :int '(0))))
	  (with-object ((rect-width (alloc :int (list (round (/ width n))))))
	    (with-object ((rect-height (alloc :int (list (round (/ height n))))))
	     ;Create trackbars used to adjust template and rectangle position
	      (create-trackbar "RECT-X" (aref window-name-arr 1) rect-x width)
	      (create-trackbar "RECT-Y" (aref window-name-arr 1) rect-y height)
	      (create-trackbar "RECT-WIDTH" (aref window-name-arr 1) rect-width 
			       width)
	      (create-trackbar "RECT-HEIGHT" (aref window-name-arr 1) rect-height 
			       height)
	      ;Set rectangle color
	      (with-scalar ((color (scalar 0 255 0)))
		(loop
		   ;Set camera feed to FRAME
		   (with-mat ((frame (mat)))
		     (cap-read cap frame)
		     ;Print location and size of the 
		     ;template used to do the matchi-
		     ;ng and the rectangle
		     (format t "RECT-X: ~a~%~%" (mem-ref rect-x :int))
		     (format t "RECT-Y: ~a~%~%" (mem-ref rect-y :int))
		     (format t "RECT-WIDTH: ~a~%~%" (mem-ref rect-width :int))
		     (format t "RECT-HEIGHT: ~a~%~%" (mem-ref rect-height :int))
		     ;Instantiate logic used to move the 
		     ;template and the rectangle as one
		     (if (< (mem-ref rect-x :int) (round (/ (mem-ref rect-width :int) 128))) 
			 (setf (mem-ref rect-x :int) 1))
		     (if (> (mem-ref rect-x :int) 
			    (- (cols frame) (mem-ref rect-width :int))) 
			 (setf (mem-ref rect-x :int) 
			       (- (cols frame) (mem-ref rect-width :int))))
		     (if (< (mem-ref rect-y :int) (round (/ (mem-ref rect-height :int) 128))) 
			 (setf (mem-ref rect-y :int) 1))
		     (if (> (mem-ref rect-y :int) 
			    (- (rows frame) (mem-ref rect-height :int))) 
			 (setf (mem-ref rect-y :int) 
			       (- (rows frame) (mem-ref rect-height :int))))
		     (if (< (mem-ref rect-width :int) 1) 
			 (setf (mem-ref rect-width :int) 1))
		     (if (< (mem-ref rect-height :int) 1) 
			 (setf (mem-ref rect-height :int) 1))
		     (with-mat ((img (mat)))
		       (with-mat ((src (mat)))
		         ;Create 2 clones of FRAME, IMG will be where the 
			 ;rectangle is moved to choose the template. SRC 
		         ;is MATCH-TEMPLATE IMAGE parameter. FRAME will b-
		         ;e the template image 
			 (copy-to frame img)
			 (copy-to frame src)
			 ;Set template position and location parameters
			 (with-rect ((roi (rect (mem-ref rect-x :int)
						(mem-ref rect-y :int)
						(mem-ref rect-width :int)
						(mem-ref rect-height :int))))
			   ;Create template image from FRAME 
			   ;to use in MATCH-TEMPLATE. Set to 
			   ;FRAME
			   (copy-to (roi frame roi) frame))
			 ;Set rectangle location parameters
			 (with-point ((point-1 (point (mem-ref rect-x :int) 
						      (mem-ref rect-y :int)))
				      (point-2 (point (+ (mem-ref rect-x :int) 
							 (mem-ref rect-width :int)) 
						      (+ (mem-ref rect-y :int) 
							 (mem-ref rect-height :int))))) 
			   ;Create rectangle on IMG at same 
			   ;position as the template
			   (rectangle img point-1 point-2 color 5 4 0)
			   (imshow (aref window-name-arr 0) img))
			 ;Set width and height of matrices 
			 ;we will create in next step
			 (setf iwidth (+ (- (cols src) (cols frame)) 1))
			 (setf iheight (+ (- (rows src) (rows frame)) 1))
			 ;Create an array of finalized matrices 
                         ;to hold all of the matches. All of the
                         ;functions with automatic GC are in the
                         ;gc.lisp file and can be used by adding
                         ;the 'gc' prefix to the function name.    
			 (dotimes (i 6)
			   (setf (aref arr i) (gc:mat iheight iwidth +32f+)))
			 ;Run all versions of MATCH-TEMPLATE 
			 ;and run NORMALIZE on each match 
			 (dotimes (i 6)
			   (match-template src frame (aref arr i) i)
			   (normalize (aref arr i) (aref arr i) 1d0 0d0 +norm-minmax+)) 
			 ;Show template(FRAME) in a window
			 (imshow (aref window-name-arr 1) frame)
			 ;Show matches
			 (dotimes (i 6)
			   (imshow (aref window-name-arr (+ i 2)) (aref arr i)))))
		     ;Reset ROI
		     (with-rect ((roi (rect 0 0 (cols frame) (rows frame))))
		       (copy-to (roi frame roi) frame)))
		   (let ((c (wait-key 33)))
		     (when (= c 27)
		       (destroy-all-windows)
		       (return))))))))))))


Example 2:


(defun match-template-example-2 (&optional (camera-index *camera-index*) 
				   (width *default-width*)
				   (height *default-height*))

  "This is just like the MATCH-TEMPLATE-EXAMPLE-1 but just shows just one 
   MATCH-TEMPLATE method result in a window, +TM-CCOEFF-NORMED+. I wrote 
   this example, because in the MATCH-TEMPLATE-EXAMPLE-1 this block of co-
   de takes about half a second to run:

	(dotimes (i 6)
	  (match-template src frame (aref arr i) i)
	  (normalize (aref arr i) (aref arr i) 1d0 0d0 +norm-minmax+))

   The normal frame rate for videos is 0.033333333 second per frame. 
   This example should fill in any cracks left by the other one."

  (with-capture (cap (video-capture camera-index))
					;Create array of window names
    (let* ((window-name-arr (make-array 3 :initial-contents 
					(list "IMG - MATCH-TEMPLATE-EXAMPLE-2"
					      "FRAME - MATCH-TEMPLATE-EXAMPLE-2"
					      "SQDIFF - MATCH-TEMPLATE-EXAMPLE-2")))
           ;Initialize size parameters 
	   ;for the matches
           (iwidth 0)
	   (iheight 0)
           (n 10))      
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-width+ height)
      ;;Create windows
      (with-named-window ((aref window-name-arr 0) +window-normal+)
	(with-named-window ((aref window-name-arr 1) +window-normal+)
	  (with-named-window ((aref window-name-arr 2) +window-autosize+)   
	    ;Move windows to specified locations     
	    (move-window (aref window-name-arr 0) 253 0)
	    (move-window (aref window-name-arr 1) 253 400)
	    (move-window (aref window-name-arr 2) 670 150)
	    ;Allocate int pointers for the trackbars to 
	    ;adjust which will set the template image a-
	    ;nd the guiding rectangle location and boun-
	    ;daries
	    (with-object ((rect-x (alloc :int '(0))))
	      (with-object ((rect-y (alloc :int '(0))))
		(with-object ((rect-width (alloc :int (list (round (/ width n))))))
		  (with-object ((rect-height (alloc :int (list (round (/ height n))))))
		    ;Create trackbars used to adjust template and rectangle position
		    (create-trackbar "RECT-X" (aref window-name-arr 1) rect-x width)
		    (create-trackbar "RECT-Y" (aref window-name-arr 1) rect-y height)
		    (create-trackbar "RECT-WIDTH" (aref window-name-arr 1) rect-width 
				     width)
		    (create-trackbar "RECT-HEIGHT" (aref window-name-arr 1) rect-height 
				     height)
		    ;Set rectangle color
		    (with-scalar ((color (scalar 0 255 0)))
		      (loop
			 ;Set camera feed to FRAME
			 (with-mat ((frame (mat)))
			   (cap-read cap frame)
			  ;Print location and size of the 
			  ;template used to do the matchi-
			  ;ng and the rectangle
			  (format t "RECT-X: ~a~%~%" (mem-ref rect-x :int))
			  (format t "RECT-Y: ~a~%~%" (mem-ref rect-y :int))
			  (format t "RECT-WIDTH: ~a~%~%" (mem-ref rect-width :int))
			  (format t "RECT-HEIGHT: ~a~%~%" (mem-ref rect-height :int))
			  ;Instantiate logic used to move the 
			  ;template and the rectangle as one
			  (if (< (mem-ref rect-x :int) (round (/ (mem-ref rect-width :int) 128))) 
			      (setf (mem-ref rect-x :int) 1))
			  (if (> (mem-ref rect-x :int) 
				 (- (cols frame) (mem-ref rect-width :int))) 
			      (setf (mem-ref rect-x :int) 
				    (- (cols frame) (mem-ref rect-width :int))))
			  (if (< (mem-ref rect-y :int) (round (/ (mem-ref rect-height :int) 128))) 
			      (setf (mem-ref rect-y :int) 1))
			  (if (> (mem-ref rect-y :int) 
				 (- (rows frame) (mem-ref rect-height :int))) 
			      (setf (mem-ref rect-y :int) 
				    (- (rows frame) (mem-ref rect-height :int))))
			  (if (< (mem-ref rect-width :int) 1) 
			      (setf (mem-ref rect-width :int) 1))
			  (if (< (mem-ref rect-height :int) 1) 
			      (setf (mem-ref rect-height :int) 1))
			  (with-mat ((img (mat)))
			    (with-mat ((src (mat)))
			      ;Create 2 copies of FRAME, IMG will be where the 
			      ;rectangle is moved to choose the template. SRC 
			      ;is MATCH-TEMPLATE IMAGE parameter. FRAME will b-
			      ;e the template image 
			      (copy-to frame img)
			      (copy-to frame src)
			      ;Set template position and location parameters
			      (with-rect ((roi (rect (mem-ref rect-x :int)
						     (mem-ref rect-y :int)
						     (mem-ref rect-width :int)
						     (mem-ref rect-height :int))))
			       ;Create template image from FRAME 
			       ;to use in MATCH-TEMPLATE. Set to 
			       ;FRAME
				(copy-to (roi frame roi) frame))
			      ;Set rectangle location parameters
			      (with-point ((point-1 (point (mem-ref rect-x :int) 
							   (mem-ref rect-y :int)))
					   (point-2 (point (+ (mem-ref rect-x :int) 
							      (mem-ref rect-width :int)) 
							   (+ (mem-ref rect-y :int) 
							      (mem-ref rect-height :int))))) 
				;Create rectangle on IMG at same 
				;position as the template
				(rectangle img point-1 point-2 color 5 4 0)
				(imshow (aref window-name-arr 0) img))
			      ;Set width and height of matrices 
			      ;we will create in next step
			      (setf iwidth (+ (- (cols src) (cols frame)) 1))
			      (setf iheight (+ (- (rows src) (rows frame)) 1))
			      ;Create  matrix to hold all of the matches
			      (with-mat ((matches (mat iheight iwidth +32f+)))
				;Run MATCH-TEMPLATE on each frame of the camera 
                                ;feed and run NORMALIZE on each match 
				(match-template src frame matches +tm-ccoeff-normed+)
				(normalize matches matches 1d0 0d0 +norm-minmax+)
			        ;Show template(FRAME) in a window
				(imshow (aref window-name-arr 1) frame)
				;Show matches
				(imshow (aref window-name-arr 2) matches)
			        ;Reset ROI
				(with-rect ((roi (rect 0 0 (cols frame) (rows frame))))
				  (copy-to (roi frame roi) frame))
				(let ((c (wait-key 33)))
				  (when (= c 27)
				    (return)))))))))))))))))))



HIGHGUI - READING AND WRITING IMAGES AND VIDEO:



CAP-GET


Returns the specified VIDEO-CAPTURE property


C++: double VideoCapture::get(int propId)

LISP-CV: (CAP-GET (SELF VIDEO-CAPTURE) (PROP-ID :INT))


    Parameters:	SELF - The VIDEO-CAPTURE structure. 

                PROP-ID -

       Property identifier. It can be one of the following:

            +CAP-PROP-POS-MSEC+ Current position of the video file in milliseconds or video capture tim-
                                estamp.
            +CAP-PROP-POS-FRAMES+ 0-based index of the frame to be decoded/captured next.

            +CAP-PROP-POS-AVI-RATIO+ Relative position of the video file: 0 - start of the film, 1 - en-
                                     d of the film.
            +CAP-PROP-FRAME-WIDTH+ Width of the frames in the video stream.

            +CAP-PROP-FRAME-HEIGHT+ Height of the frames in the video stream.

            +CAP-PROP-FPS+ Frame rate.

            +CAP-PROP-FOURCC+ 4-character code of codec.

            +CAP-PROP-FRAME-COUNT+ Number of frames in the video file.

            +CAP-PROP-FORMAT+ Format of the Mat objects returned by retrieve() .

            +CAP-PROP-MODE+ Backend-specific value indicating the current capture mode.

            +CAP-PROP-BRIGHTNESS+ Brightness of the image (only for cameras).

            +CAP-PROP-CONTRAST+ Contrast of the image (only for cameras).

            +CAP-PROP-SATURATION+ Saturation of the image (only for cameras).

            +CAP-PROP-HUE+ Hue of the image (only for cameras).

            +CAP-PROP-GAIN+ Gain of the image (only for cameras).

            +CAP-PROP-EXPOSURE+ Exposure (only for cameras).

            +CAP-PROP-CONVERT-RGB+ Boolean flags indicating whether images should be converted to RGB.

            +CAP-PROP-WHITE-BALANCE+ Currently not supported

            +CAP-PROP-RECTIFICATION+ Rectification flag for stereo cameras (note: only supported by DC1394 
                                     v 2.x backend currently)

Note: When querying a property that is not supported by the backend used by the VideoCapture class <-todo,
value 0 is returned.


(defun cap-get-example (&optional 
                          (camera-index *camera-index*) 
			  (width *default-width*)
			  (height *default-height*))

  "Gets the width and height of the camera capture 
   with the function CAP-GET and prints it."

  (with-capture (cap (video-capture camera-index))
    (let ((window-name "CAP-GET Example"))
      (if (not (cap-is-open cap)) 
	  (return-from cap-get-example 
	    (format t "Cannot open the video camera")))
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-height+ height)
      (format t "~%Frame Size : ~ax~a~%~%" 
	      (cap-get cap +cap-prop-frame-width+)
	      (cap-get cap +cap-prop-frame-height+))
      (named-window window-name +window-normal+)
      (move-window window-name 759 175)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(imshow window-name frame)
        (del-mat frame))
      (destroy-window window-name))))



CAP-READ


Grabs, decodes and returns the next video frame.


C++: bool VideoCapture::read(Mat& image)

LISP-CV: (CAP-READ (SELF VIDEO-CAPTURE) (IMAGE MAT))


    Parameters:	

         SELF - The "grabbed" camera feed.

         IMAGE - The returned frame.


The methods/functions combine (CAP-GRAB) and (CAP-RETRIEVE) in one call. This is the most convenient 
method for reading video files or capturing data from decode and return the just grabbed frame. If no 
frames has been grabbed (camera has been disconnected, or there are no more frames in video file), the 
methods return false and the functions return NULL pointer.


(defun cap-read-example (&optional (camera-index 
				    *camera-index*))

  (with-capture (cap (video-capture camera-index))
    (let ((window-name "CAP-READ Example"))
      (if (not (cap-is-open cap)) 
	  (return-from cap-read-example 
	    (format t "Cannot open the video camera")))
      (named-window window-name +window-normal+)
      (move-window window-name 759 175)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(imshow window-name frame)
        (del-mat frame))
      (destroy-window window-name))))



CAP-RELEASE


Closes video file or capturing device.


C++: void VideoCapture::release()

LISP-CV: (CAP-RELEASE (SELF VIDEO-CAPTURE)) => :VOID


The methods are automatically called by subsequent (CAP-OPEN) and by VIDEO-CAPTURE destructor.


    Parameters:	

         SELF - The VIDEO-CAPTURE structure.


(defun cap-release-example (&optional 
                              (camera-index 
                               *camera-index*))

  "In order: First the function CAPTURE-FROM-CAM allocates and 
   initializes the structure for reading a video stream from t-
   he camera. Next a window is created with NAMED-WINDOW. Then 
   the camera stream is read with CAP-READ and then shown in t-
   he window with IMSHOW. Then, once the user invokes the WAIT-
   KEY function by pressing the Escape key while the window is 
   active the DO loop is exited and CAP-RELEASE is called clos-
   ing the structure used to read and show the video stream. 
   Note: If you use the macro WITH-CAPTURE, CAP-RELEASErot will b-
   e called automatically. See WITH-CAPTURE EXAMPLE for usage."
  
  (let ((cap (video-capture camera-index))
	(window-name "CAP-RELEASE Example"))
    (if (not (cap-is-open cap)) 
	(return-from cap-release-example 
	  (format t "Cannot open the video camera")))
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    (do* ((frame 0))
	 ((plusp (wait-key *millis-per-frame*)) 
	  (format t "Key is pressed by user"))
      (setf frame (mat))
      (cap-read cap frame)
      (imshow window-name frame))
    (cap-release cap)
    (destroy-window window-name)))



CAP-SET


Sets a property in the VIDEO-CAPTURE


C++: bool VideoCapture::set(int propId, double value)

LISP-CV: (CAP-SET (SELF VIDEO-CAPTURE) (PROP-ID :INT) (VALUE :DOUBLE)) => :BOOLEAN


    Parameters:	SELF - The VIDEO-CAPTURE structure.

                PROP-ID -

       Property identifier. It can be one of the following:

            +CAP-PROP-POS-MSEC+ Current position of the video file in milliseconds.

            +CAP-PROP-POS-FRAMES+ 0-based index of the frame to be decoded/captured next.

            +CAP-PROP-POS-AVI-RATIO+ Relative position of the video file: 0 - start of the file, 

                                                                          1 - end of the file.

            +CAP-PROP-FRAME-WIDTH+ Width of the frames in the video stream.

            +CAP-PROP-FRAME-HEIGHT+ Height of the frames in the video stream.

            +CAP-PROP-FPS+ Frame rate.

            +CAP-PROP-FOURCC+ 4-character code of codec.

            +CAP-PROP-FRAME-COUNT+ Number of frames in the video file.

            +CAP-PROP-FORMAT+ Format of the Mat objects returned by retrieve() .

            +CAP-PROP-MODE+ Backend-specific value indicating the current capture mode.

            +CAP-PROP-BRIGHTNESS+ Brightness of the image (only for cameras).

            +CAP-PROP-CONTRAST+ Contrast of the image (only for cameras).

            +CAP-PROP-SATURATION+ Saturation of the image (only for cameras).

            +CAP-PROP-HUE+ Hue of the image (only for cameras).

            +CAP-PROP-GAIN+ Gain of the image (only for cameras).

            +CAP-PROP-EXPOSURE+ Exposure (only for cameras).

            +CAP-PROP-CONVERT-RGB+ Boolean flags indicating whether images should be converted to RGB.

            +CAP-PROP-WHITE-BALANCE+ Currently unsupported

            +CAP-PROP-RECTIFICATION+ Rectification flag for stereo cameras (note: only supported by
                                     DC1394 v 2.x backend currently)
  
                 VALUE - Value of the property.


             
(defun cap-set-example (&optional 
                          (camera-index 
                           *camera-index*))

  "Changes the brightness level of the camera feed 
   with the function CAP-SET and then prints the b-
   rightness level."

  (with-capture (cap (video-capture camera-index))
    (let ((window-name "CAP-SET Example"))
      (if (not (cap-is-open cap)) 
	  (return-from cap-set-example 
	    (format t "Cannot open the video camera")))
(cap-set cap +cap-prop-brightness+ 0.7)
      (format t "Brightness level: ~a~%~%" 
	      (cap-get cap +cap-prop-brightness+))
      (named-window window-name +window-normal+)
      (move-window window-name 759 175)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(imshow window-name frame))
      (destroy-window window-name))))



IMREAD


Loads an image from a file.


C++: Mat imread(const string& filename, int flags=1)

LISP-CV: (IMREAD (FILENAME *STRING) &OPTIONAL ((FLAGS :INT) +LOAD-IMAGE-COLOR+)) => MAT


    Parameters:	

        FILENAME - Name of file to be loaded.

        FLAGS -

        Flags specifying the color type of a loaded image:

            +LOAD-IMAGE-ANYDEPTH+ - If set, return 16-bit/32-bit image when the input has the corresponding 
                                    depth, otherwise convert it to 8-bit.

            +LOAD-IMAGE-COLOR+ - If set, always convert image to the color one

            +LOAD-IMAGE-GRAYSCALE+ - If set, always convert image to the grayscale one

            >0 Return a 3-channel color image.

                Note

                In the current implementation the alpha channel, if any, is stripped from the output 
                image. Use negative value if you need the alpha channel.

            =0 Return a grayscale image.

            <0 Return the loaded image as is (with alpha channel).


The function IMREAD loads an image from the specified file and returns it. If the image cannot be r-
ead (because of missing file, improper permissions, unsupported or invalid format), the function re-
turns an empty matrix ( Mat::data==NULL )todo. Currently, the following file formats are supported:

        Windows bitmaps - *.bmp, *.dib (always supported)

        JPEG files - *.jpeg, *.jpg, *.jpe (see the Notes section)

        JPEG 2000 files - *.jp2 (see the Notes section)

        Portable Network Graphics - *.png (see the Notes section)

        Portable image format - *.pbm, *.pgm, *.ppm (always supported)

        Sun rasters - *.sr, *.ras (always supported)

        TIFF files - *.tiff, *.tif (see the Notes section)

Note:

    The function determines the type of an image by the content, not by the file extension.

    On Microsoft Windows* OS and MacOSX*, the codecs shipped with an LISP-CV image (libjpeg
    , libpng, libtiff, and libjasper) are used by default. So, LISP-CV can always read JPEG-
    s, PNGs, and TIFFs. On MacOSX, there is also an option to use native MacOSX image readers. But 
    beware that currently these native image loaders give images with different pixel values becaus-
    e of the color management embedded into MacOSX. 

    On Linux*, BSD flavors and other Unix-like open-source operating systems, LISP-CV looks
    for codecs supplied with an OS image. Install the relevant packages (do not forget the developm-
    ent files, for example, “libjpeg-dev”, in Debian* and Ubuntu*) to get the codec support or turn 
    on the OPENCV_BUILD_3RDPARTY_LIBS flag in CMake.

Note

In the case of color images, the decoded images will have the channels stored in B G R order.


Example using manual memory management:


(defun imread-example-1 (filename)

  "Open the image FILENAME with IMREAD 
   and show it in a window. This examp-
   le uses manual memory management"

  (let* ((image (imread filename 1))
	 (window-name "IMREAD Example 1"))
    (if (empty image) 
	(return-from imread-example-1 
	  (format t "Image not loaded")))
    (with-named-window (window-name +window-normal+)
      (move-window window-name 759 175)
      (imshow window-name image)
      (loop
	 (let ((c (wait-key 33)))
	   (when (= c 27)
	     (del-mat image)
	     (return)))))))


Example using a WITH-* macrs for memory/window managemant:


(defun imread-example-2 (filename)

  "Open the image FILENAME with IMREAD 
   and show it in a window. This examp-
   le uses a with-* macro for memory m-
   anagement."

  (let ((window-name "IMREAD Example 2"))
    (with-named-window (window-name +window-normal+)
      (move-window window-name 759 175)
      (with-mat ((image (imread filename 1)))
	(if (empty image) 
	    (return-from imread-example-2 
	      (format t "Image not loaded")))
	(imshow window-name image)
	(loop
	   (let ((c (wait-key 33)))
	     (when (= c 27)
	       (return))))))))



Example using TG finalizers for memory management:


(defun imread-example-3 (filename)

  "Open the image FILENAME with IMREAD 
   and show it in a window. This examp-
   le uses a TG finalizer for memory m-
   anagement"

  (let* ((image (gc:imread filename 1))
	 (window-name "IMREAD Example 3"))
    (if (empty image) 
	(return-from imread-example-3 
	  (format t "Image not loaded")))
    (with-named-window (window-name +window-normal+)
      (move-window window-name 759 175)
      (imshow window-name image)
      (loop
	 (let ((c (wait-key 33)))
	   (when (= c 27)
	     (return)))))))



IMWRITE

Saves an image to a specified file.

C++: bool imwrite(const string& filename, InputArray img, const vector<int>& params=vector<int>() )

LISP-CV: (IMWRITE (FILENAME :STRING) (IMG MAT) ((PARAMS VECTOR-INT) (VEC-INT))) => :BOOLEAN

    Parameters:	

        FILENAME - Name of the file.

        IMAGE - Image to be saved.

        PARAMS -

        Format-specific save parameters encoded as pairs PARAM-ID-1, PARAM-VALUE-1, PARAM-ID-2, 
        PARAM-VALUE-2, ... . The following parameters are currently supported:

            For JPEG, it can be a quality (+IMWRITE-JPEG-QUALITY+) from 0 to 100 (the higher is the 
            better). Default value is 95.

            For PNG, it can be the compression level (+IMWRITE-PNG-COMPRESSION+) from 0 to 9. A higher 
            value means a smaller size and longer compression time. Default value is 3.

            For PPM, PGM, or PBM, it can be a binary format flag (+IMWRITE-PXM-BINARY+ ), 0 or 1. 
            Default value is 1.

The function IMWRITE saves the image to the specified file. The image format is chosen based on the 
filename extension (see (IMREAD) for the list of extensions). Only 8-bit (or 16-bit unsigned (+16U+) 
in case of PNG, JPEG 2000, and TIFF) single-channel or 3-channel (with ‘BGR’ channel order) images 
can be saved using this function. If the format, depth or channel order is different, use (CONVERT-TO), 
and (CVT-COLOR) to convert it before saving. Or, use the universal XML I/O functions to save the image 
to XML or YAML format.

It is possible to store PNG images with an alpha channel using this function. To do this, create 8-bit 
(or 16-bit) 4-channel image BGRA, where the alpha channel goes last. Fully transparent pixels should 
have alpha set to 0, fully opaque pixels should have alpha set to 255/65535. 


(defun imwrite-example (filename out-file)
  
  (let* ((window-name-1 "Original image - IMWRITE Example")
         (window-name-2 "Flipped image - IMWRITE Example"))
    ;;Read in image
    (with-mat ((image (imread filename 1)))
      (if (empty image) 
	  (return-from imwrite-example 
	    (format t "Image not loaded")))
      (with-named-window (window-name-1 +window-normal+)
	(with-named-window (window-name-2 +window-normal+)
	  (move-window window-name-1 533 175)
	  (move-window window-name-2 984 175)
	  ;;Show original IMAGE in window
	  (imshow window-name-1 image)
	  ;;Flip IMAGE around the x-axis
	  (flip image image -1)
	  ;;Show flipped image in window
	  (imshow window-name-2 image)
	  ;;Write flipped image to filename specified 
	  ;;by the OUT-FILE parameter 
	  (imwrite out-file image 
		   (vec-int 
		    (list +imwrite-jpeg-quality+ 
			  99)))
	  (loop 
    	     (let ((c (wait-key 33)))
	       (when (= c 27)
		 (return)))))))))



VIDEO-CAPTURE

VideoCapture constructors.

C++: VideoCapture::VideoCapture()

LISP-CV: (VIDEO-CAPTURE) => VIDEO-CAPTURE

C++: VideoCapture::VideoCapture(int device)

LISP-CV: (VIDEO-CAPTURE &OPTIONAL (SRC :INT)) => VIDEO-CAPTURE

C++: VideoCapture::VideoCapture(const string& filename)

LISP-CV: (VIDEO-CAPTURE &OPTIONAL (SRC :POINTER STRING*)) => VIDEO-CAPTURE


  Parameters:	

        SRC - 

             A device: ID of the opened video capturing device (i.e. a camera index). If there is 
                       a single camera connected, just pass 0.     

             A file name: Name of the opened video file (eg. video.avi) or image sequence (eg. 
                          img_%02d.jpg, which will read samples like img_00.jpg, img_01.jpg, 
                          img_02.jpg, ...)

             NIL: Creates an uninitialized VIDEO-CAPTURE
        


(defun video-capture-example (filename &optional 
					 (camera-index *camera-index*))

  "This example use the function VIDEO-CAPTURE to open a video 
   capturing device, supplied by the *CAMERA-INDEX* parameter. 
   The setf-able *CAMERA-INDEX* parameter defaults to 0. Then 
   the function VIDEO-CAPTURE is used to open a video file sup-
   plied by the parameter FILENAME."
  
  (with-capture (video-capture (video-capture camera-index))
    (with-capture (cap-file (video-capture filename))
      (let ((window-name-1 "Camera - VIDEO-CAPTURE Example")
	    (window-name-2 "Video file - VIDEO-CAPTURE Example"))
	(if (not (cap-is-open video-capture)) 
	    (return-from video-capture-example 
	      (format t "Cannot open the video camera")))
	(if (not (cap-is-open cap-file)) 
	    (return-from video-capture-example 
	      (format t "Cannot open the video file")))
	(named-window window-name-1 +window-normal+)
	(named-window window-name-2 +window-normal+)
	(move-window window-name-1 533 175)
	(move-window window-name-2 984 175)
	(do* ((camera 0)
	      (video-file 0))
	     ((plusp (wait-key *millis-per-frame*)) 
	      (format t "Key is pressed by user"))
	  (setf camera (mat))
	  (cap-read video-capture camera)
	  (setf video-file (mat))
	  (cap-read cap-file video-file)
	  (imshow window-name-1 camera)
	(destroy-all-windows)))))



WITH-CAPTURE


Ensures CAP-RELEASE and DEL-VID-CAP gets called automatically when capture goes out of scope.


LISP-CV: (WITH-CAPTURE (CAPTURE-VAR CAP)) &BODY BODY)


Parameters:	

         CAPTURE-VAR - A variable representing the function used to open video file or a capturing 
                       device. Similar to the variable in a LET statement.

         CAP - The function used to open video file or a capturing device for video capturing, as in 
               (video-capture (DEVICE :INT)). See WITH-CAPTURE example.
    
         BODY - The body of the code to be executed once the video file or capturing device is open.


(defun with-capture-example (&optional 
			       (cam 0))

  (with-capture (cap (video-capture cam))
    (let ((window-name "WITH-CAPTURE Example"))
      (if (not (cap-is-open cap))
	  (return-from with-capture-example 
	    (format t "Cannot open the video camera")))
      (with-named-window( window-name +window-normal+)
	(move-window window-name 759 175)
	(loop 
	   (with-mat ((frame (mat)))
	     (cap-read cap frame)
	     (imshow window-name frame))
	   (let ((c (wait-key 33)))
	     (when (= c 27)
	       (return))))))))



WITH-CAPTURED-CAMERA


Ensures CAP-RELEASE and DEL-VID-CAP gets called on captures. Also sets capture width/height in function.


LISP-CV: (WITH-CAPTURED-CAMERA ((CAPTURE-VAR (DEV-INDEX :INT) &KEY (WIDTH :INT) (HEIGHT :INT)) &BODY BODY)) => VIDEO-CAPTURE


Parameters:	

         CAPTURE-VAR - A variable representing the function used to open video file or a capturing 
                       device. Similar to the variable in a LET statement.

         DEV-INDEX - ID of the opened video capturing device (e.g. a camera index). If there is a 
                     single camera connected, just pass 0.

         WIDTH - Width of the frames in the video stream.

         HEIGHT - Height of the frames in the video stream.
    
         BODY - The body of the code to be executed once the video file or capturing device is open.






WITH-CAPTURED-FILE


Ensures CAP-RELEASE and DEL-VID-CAP gets called on captures. Also sets capture width/height in function.


LISP-CV: (WITH-CAPTURED-FILE ((CAPTURE-VAR (FILE-PATH :INT) &KEY (WIDTH :INT) (HEIGHT :INT)) &BODY BODY))


Parameters:	

         CAPTURE-VAR - A variable representing the function used to open video file or a capturing 
                       device. Similar to the variable in a LET statement.

         FILENAME - Name of the opened video file (eg. video.avi) or image sequence (eg. img-%02d.jpg, 
                    which will read samples like img-00.jpg, img-01.jpg, img-02.jpg,...)

         WIDTH - Width of the frames in the video stream.

         HEIGHT - Height of the frames in the video stream.
    
         BODY - The body of the code to be executed once the video file or capturing device is open.



(defun with-captured-file-example (file-path)
  (let ((window-name "WITH-CAPTURED-FILE Example"))
    (with-named-window (window-name +window-normal+)
      (move-window window-name 759 175)
      (with-captured-file (cap file-path 
			       :width 640 
			       :height 480)
	(loop
	   (with-mat ((frame (mat)))
	     (cap-read cap frame)
	     (imshow window-name frame))
	   (let ((c (wait-key 33)))
	     (when (= c 27)
	       (return))))))))



HIGHGUI - USER INTERFACE



CREATE-TRACKBAR

Creates a trackbar and attaches it to the specified window.

C++: int createTrackbar(const string& trackbarname, const string& winname, int* value, int count, TrackbarCallback onChange=0, 
                        void* userdata=0)

LISP-CV:  (CREATE-TRACKBAR (TRACKBARNAME *STRING) (WINNAME *STRING) (VALUE :POINTER) (COUNT :INT) &OPTIONAL 
                          ((ON-CHANGE TRACKBAR-CALLBACK) (NULL-POINTER)) ((USERDATA :POINTER) (NULL-POINTER))) => :INT

    
    Parameters:	

        TRACKBARNAME - Name of the created trackbar

        WINNAME - Name of the window that will be used as a parent of the created trackbar.

        VALUE - Optional pointer to an integer variable whose value reflects the position of the slider. 
                Upon creation, the slider position is defined by this variable.

        COUNT - Maximal position of the slider. The minimal position is always 0.

        ON-CHANGE - Pointer to the function to be called every time the slider changes position. This 
                    function should be prototyped as in the below example, where the first parameter 
                    is the trackbar position and the second parameter is the user data (see the next 
                    parameter). If the callback is the NULL pointer, no callbacks are called, but only 
                    value is updated.

        USERDATA - User data that is passed as is to the callback. It can be used to handle trackbar 
                   events without using global variables.


The function CREATE-TRACKBAR creates a trackbar (a slider or range control) with the specified name
and range, assigns a variable value to be a position synchronized with the trackbar and specifies the 
callback function onChange to be called on the trackbar position change. The created trackbar is displayed 
in the specified window winname.

Note: (Qt Backend Only) WINNAME can be empty (or NIL) if the trackbar should be attached to the control panel.


Example:


;; a callback function called by the CREATE-TRACKBAR
;; ON-CHANGE parameter...a HELLO-WORLD function.

(defcallback hello-world-brightness :void ((pos :int) (ptr :pointer))
  (format t "Hello World!~%~%~a~a~%~%" (mem-aref ptr :string 0) pos))

;; another HELLO-WORLD callback function
(defcallback hello-world-contrast :void ((pos :int) (ptr :pointer))
  (format t "Hello World!~%~%~a~a~%~%" (mem-aref ptr :string 0) pos))


(defun create-trackbar-example (filename)
  (let ((window-name "Adjust brightness and contrast by moving the sliders.")
	(brightness 0)
	(contrast 0))
    (with-named-window (window-name +window-autosize+)
      (move-window window-name 759 175)
      ;; allocate two :int pointers that trackbar can adjust
      (with-object ((slider-1-value (alloc :int '(50)))
		    (slider-2-value (alloc :int '(50)))
		    ;; data to be passed to HELLO-WORLD-BRIGHTNESS callback function
		    (userdata-1 (alloc :string "Brightness =  "))
		    ;; data to be passed to HELLO-WORLD-CONTRAST callback function
		    (userdata-2 (alloc :string "Contrast = ")))
	(loop
	   ;; read in image supplied by filename parameter
	   (with-mat ((src (imread filename 1)))
	     (if (empty src) 
		 (return-from create-trackbar-example
		   (format t "Image not loaded")))
	     ;; Clone the source image to dest
	     (with-mat  ((dest (clone src)))
	       ;; create Trackbar with name, 'Brightness'
	       (create-trackbar "Brightness" window-name slider-1-value 100
				;; pointer to a callback function to be called every 
				;; time the trackbar slider changes position 
				(callback hello-world-brightness) 
				;; user data that is passed to 
				;; the callback function           
				userdata-1)
	       ;; create trackbar with name, 'Contrast'
	       (create-trackbar  "Contrast" window-name slider-2-value 100
				 ;; again,a callback function pointer 
				 (callback hello-world-contrast) 
				 ;; user data
				 userdata-2)
	       ;; when the top trackbar is moved, adjust brightness variable
	       (setf brightness (- (mem-ref slider-1-value :int) 50))
	       ;; when the bottom Trackbar is moved, adjust contrast variable
	       (setf contrast (/ (mem-ref slider-2-value :int) 50))
	       ;; apply brightness and contrast settings to the destination image
	       (convert-to src dest -1 (coerce contrast 'double-float)  
			   (coerce brightness 'double-float))
	       ;; show adjusted image in a window
	       (imshow window-name dest)
	       (let ((c (wait-key 33)))
		 (when (= c 27)
		   (return))))))))))



DESTROY-WINDOW


Destroys a window.


C++: void destroyWindow(const string& winname)

LISP-CV: (DESTROY-WINDOW (WINNAME *STRING)) => :VOID

    Parameters:	WINNAME - Name of the window to be destroyed.

The function DESTROY-WINDOW destroys the window with the given name.


(defun destroy-window-example ()

  "Creates a window. Window will be closed 
   by DESTROY-WINDOW when it is active and
   any key is pressed."

  (let* ((window-name "DESTROY-WINDOW Example"))
    (named-window window-name +window-normal+)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))



DESTROY-ALL-WINDOWS

Destroys all of the HighGUI windows.

C++: void destroyAllWindows()

LISP-CV: (DESTROY-ALL-WINDOWS) => :VOID


The function DESTROY-ALL-WINDOWS destroys all of the opened HighGUI windows.


(defun destroy-all-windows-example ()

  "In this example we create
 12 windows and DESTROY THEM!!!"

  (let* ((window-name-arr 
	  (make-array 12 :initial-contents 

		      (list
		       "WINDOW 1 - DESTROY-ALL-WINDOWS Example"
		       "WINDOW 2 - DESTROY-ALL-WINDOWS Example"
		       "WINDOW 3 - DESTROY-ALL-WINDOWS Example"
		       "WINDOW 4 - DESTROY-ALL-WINDOWS Example"
		       "WINDOW 5 - DESTROY-ALL-WINDOWS Example"
		       "WINDOW 6 - DESTROY-ALL-WINDOWS Example"
		       "WINDOW 7 - DESTROY-ALL-WINDOWS Example"
		       "WINDOW 8 - DESTROY-ALL-WINDOWS Example"
		       "WINDOW 9 - DESTROY-ALL-WINDOWS Example"
		       "WINDOW 10 - DESTROY-ALL-WINDOWS Example"
		       "WINDOW 11 - DESTROY-ALL-WINDOWS Example"
		       "WINDOW 12 - DESTROY-ALL-WINDOWS Example"))))

    ;; Create 12 windows to DESTROY!!!
    (dotimes (i 12)
      (named-window (aref window-name-arr i) +window-normal+))
    ;; Move the windows to specific coordinates.
    (move-window (aref window-name-arr 0) 88 0)
    (move-window (aref window-name-arr 1) 538 0)
    (move-window (aref window-name-arr 2) 988 0)
    (move-window (aref window-name-arr 3) 1438 0)
    (move-window (aref window-name-arr 4) 88 368)
    (move-window (aref window-name-arr 5) 538 368)
    (move-window (aref window-name-arr 6) 988 368)
    (move-window (aref window-name-arr 7) 1438 368)
    (move-window (aref window-name-arr 8) 88 708)
    (move-window (aref window-name-arr 9) 538 708)
    (move-window (aref window-name-arr 10) 988 708)
    (move-window (aref window-name-arr 11) 1438 708)
    ;; When you press the escape key, you will...
    ;; DESTROY 12 WINDOWS!!!
    (loop while (not (= (wait-key 0) 27)))
    (destroy-all-windows)))
    

GET-TRACKBAR-POS

Returns the trackbar position.

C++: int getTrackbarPos(const String& trackbarname, const String& winname)

LISP-CV: (GET-TRACKBAR-POS (TRACKBARNAME *STRING) (WINNAME *STRING)) => :INT


    Parameters:	

        TRACKBARNAME - Name of the trackbar.
        
        WINNAME - Name of the window that is the parent of the trackbar.
        

The function returns the current position of the specified trackbar.


(defun get-trackbar-pos-example (filename)
  (let ((window-name "See trackbars curre")
	(text 0)
	(pos 0)
        (scale 0.70d0)
	(thickness 1))
    ;Read in image 
    (with-mat ((img (imread filename 1)))
      (if (empty img) 
	  (return-from get-trackbar-pos-example
	    (format t "Image not loaded")))
      (with-named-window (window-name +window-autosize+)
	(move-window window-name (cols img) 0)
	(with-point ((org (point 0 25)))
	  (with-scalar ((color (scalar 255 255 255)))
	    (with-object ((unused-val (alloc :int 0)))
	      (loop
		 (with-mat ((clone (clone img)))
		   (create-trackbar "Position" window-name unused-val 100)
		   ;Get trackbar position and 
		   ;print it to window
		   (setf pos (get-trackbar-pos "Position" window-name))
		   (setf text (concatenate 'string "Current trackbar position: " 
					   (write-to-string pos)))
		   (put-text clone text org +font-hershey-triplex+
			     scale  color thickness +aa+)
		   (imshow window-name clone))
		 (let ((c (wait-key 33)))
		   (when (= c 27)
		     (return)))))))))))

    
    
IMSHOW

Displays an image in the specified window.

C++: void imshow(const string& winname, InputArray mat)

LISP-CV: (IMSHOW (WINNAME *STRING) (MAT MAT)) => :void

    Parameters:	

        WINNAME - Name of the window.

        MAT - Image to be shown.


The function IMSHOW displays an image in the specified window. If the window was created with the 
+WINDOW-AUTOSIZE+ flag, the image is shown with its original size. Otherwise, the image is scaled 
to fit the window. The function may scale the image, depending on its depth:

        If the image is 8-bit unsigned, it is displayed as is.

        If the image is 16-bit unsigned or 32-bit integer, the pixels are divided by 256. That is, 
        the value range [0,255*256] is mapped to [0,255].

        If the image is 32-bit floating-point, the pixel values are multiplied by 255. That is, the
        value range [0,1] is mapped to [0,255].



(defun imshow-example (filename)

  "Opens the image FILENAME and shows it 
   in a window with IMSHOW."

  (let* ((image (imread filename 1))
	 (window-name "IMSHOW Example"))
    (if (empty image) 
	(return-from imshow-example 
	  (format t "Image not loaded")))
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    (imshow window-name image)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))



MOVE-WINDOW

Moves window to the specified position

C++: void moveWindow(const string& winname, int x, int y)

LISP-CV: (MOVE-WINDOW (WINNAME *STRING) (X :INT) (Y :INT)) => VOID


    Parameters:	

        WINNAME - Window name

        X - The new x-coordinate of the window

        Y - The new y-coordinate of the window


(defun move-window-example ()

  "Creates a window then uses MOVE-WINDOW 
   to move the window to (x, y) position 
   (720, 175)."

  (let* ((window-name "MOVE-WINDOW Example")))
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name))



NAMED-WINDOW


Creates a window.


C++: void namedWindow(const string& winname, int flags=WINDOW_AUTOSIZE)

LISP-CV: (NAMED-WINDOW (WINNAME *STRING) &OPTIONAL ((FLAGS :INT) +WINDOW-AUTOSIZE+)) => :VOID


    Parameters:	

        NAME - Name of the window in the window caption that may be used as a window identifier.

        FLAGS -

        Flags of the window. The supported flags are:

            +WINDOW-NORMAL+ If this is set, the user can resize the window (no constraint).

            +WINDOW-AUTOSIZE+ If this is set, the window size is automatically adjusted to fit the 
                              displayed image (see (IMSHOW) ), and you cannot change the window size 
                              manually.

            +WINDOW-OPENGL+ If this is set, the window will be created with OpenGL support.


The function NAMED-WINDOW creates a window that can be used as a placeholder for images and trackba-
rs. Created windows are referred to by their names.

If a window with the same name already exists, the function does nothing.

You can call (DESTROY-WINDOW) or (DESTROY-ALL-WINDOWS) to close the window and de-allocate any asso-
ciated memory usage. For a simple program, you do not really have to call these functions because a-
ll the resources and windows of the application are closed automatically by the operating system up-
on exit.

Note:

Qt backend supports additional flags:

        +WINDOW-NORMAL+ or +WINDOW-AUTOSIZE+: +WINDOW-NORMAL+ enables you to resize the window, wher-
        eas +WINDOW-AUTOSIZE adjusts automatically the window size to fit the displayed image (see 
        (IMSHOW) ), and you cannot change the window size manually.

        +WINDOW-FREERATIO+ or +WINDOW-KEEPRATIO+: +WINDOW-FREERATIO+ adjusts the image with no resp-
        ect to its ratio, whereas +WINDOW-KEEPRATIO keeps the image ratio. 
        
        +GUI-NORMAL+ or +GUI-EXPANDED+: +GUI-NORMAL+ is the old way to draw the window without stat-
        usbar and toolbar, whereas +GUI-EXPANDED+ is a new enhanced GUI.

By default, (= FLAGS (LOGIOR +WINDOW-AUTOSIZE+  +WINDOW-KEEPRATIO+  +GUI-EXPANDED+))


(defun named-window-example ()

  "Creates a named window with NAMED-WINDOW. Window 
   will close when it is selected and any key is pr-
   essed."

  (let* ((window-name "NAMED-WINDOW Example"))
    (named-window window-name +window-normal+)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))



SET-MOUSE-CALLBACK


Sets mouse handler for the specified window


C++: void setMouseCallback(const string& winname, MouseCallback onMouse, void* userdata=0 )

LISP-CV: (SET-MOUSE-CALLBACK (WINNAME *STRING) (ON-MOUSE MOUSE-CALLBACK) (USERDATA :VOID)) => :VOID


    Parameters:	

        WINNAME - Window name

        ONMOUSE - Mouse callback. See example below for how to use the callback.

        USERDATA - The optional parameter passed to the callback.



(defcallback call-back-func :void ((event :int)(x :int)(y :int)
                                   (flags :int)(userdata :pointer))
  ;; This callback function is called by the SET-MOUSE CALLBACK function
  ;; in the example below. The mouse handler created by SET-MOUSE CALLBACK
  ;; captures movements made by the mouse along with 3 different keypresses.
  ;; They are then processed in this function.

  (format t "Recieved ~a~%~%" (mem-aref userdata :string))

  (if (= event +event-mousemove+)
      (format t "Mouse move over the window (~a, ~a)~%~%" x y))
  (if (= event +event-lbuttondown+)
      (format t "Left button of the mouse down (~a, ~a)~%~%" x y))
  (if (= event +event-rbuttondown+)
      (format t "Right button of the mouse down (~a, ~a)~%~%" x y))
  (if (= event +event-mbuttondown+)
      (format t "Middle button of the mouse down (~a, ~a)~%~%" x y))
  (if (= event +event-lbuttonup+)
      (format t "Left button of the mouse up (~a, ~a)~%~%" x y))
  (if (= event +event-rbuttonup+)
      (format t "Right button of the mouse up (~a, ~a)~%~%" x y))
  (if (= event +event-mbuttonup+)
      (format t "Middle button of the mouse up (~a, ~a)~%~%" x y))
  (if (= event +event-lbuttondblclk+)
      (format t "Left button double-click flag triggered (~a, ~a)~%~%" x y))
  (if (= event +event-rbuttondblclk+)
      (format t "Right button double-click flag triggered (~a, ~a)~%~%" x y))
  (if (= event +event-mbuttondblclk+)
      (format t "Middle button double-click flag triggered (~a, ~a)~%~%" x y))
  (if (= flags (+ +event-flag-ctrlkey+ +event-flag-lbutton+))
      (format t "Left mouse button is clicked while pressing CTRL key (~a, ~a)~%~%" x y))
  (if (= flags (+ +event-flag-shiftkey+ +event-flag-rbutton+))
      (format t "Right mouse button is clicked while pressing SHIFT key (~a, ~a)~%~%" x y))
  (if (= flags (+ +event-flag-altkey+ +event-mousemove+))
      (format t "Mouse is moved over the window while pressing ALT key  (~a, ~a)~%~%" x y )))


(defun set-mouse-callback-example (filename)
  ;; load image
  (let ((src (imread filename 1))
	(window-name "SET-MOUSE-CALLBACK Example")
	;; Declare a userdata parameter 
	(userdata (alloc :string "USERDATA output")))
    (if (empty src) 
	(return-from set-mouse-callback-example
	  (format t "Image not loaded")))
    (named-window window-name 1)
    (move-window window-name 759 175)
    ;; Set mouse handler for the window, which passes a constant 
    ;; stream of mouse and mouse button positional data to the f-
    ;; unction above.  Also passes the contents of USERDATA to t-
    ;; he above function CALL-BACK-FUNC.
    (set-mouse-callback window-name (callback call-back-func) userdata)
    (imshow window-name src)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)
    (del-mat src)
    (free userdata)))



SET-TRACKBAR-POS

Sets the trackbar position.

C++: void setTrackbarPos(const String& trackbarname, const String& winname, int pos)

LISP-CV: (SET-TRACKBAR-POS (trackbarname *string) (winname *string) (pos :int)) => :VOID


    Parameters:	

        TRACKBARNAME - Name of the trackbar.
        
        WINNAME - Name of the window that is the parent of trackbar.
        
        POS - New position.
        

The function sets the position of the specified trackbar in the specified window.


(defun set-trackbar-pos-example (filename)
  (let ((window-name "SET-TRACKBAR-POS Example")
	(text 0)
        (pos 0)
        (scale 0.70d0)
	(thickness 1))
    ;Read in image
    (with-mat ((img (imread filename 1)))
      (if (empty img) 
	  (return-from set-trackbar-pos-example
	    (format t "Image not loaded")))
      (with-named-window (window-name +window-autosize+)
	(move-window window-name (cols img) 0)
	(with-point ((org (point 0 25)))
	  (with-scalar ((color (scalar 255 255 255)))
	    (with-object ((unused-val (alloc :int 0)))
	      (loop
		 (with-mat ((clone (clone img)))
		   ;Create primary trackbar
		   (create-trackbar "Primary" window-name unused-val 100)
		   ;Set primary trackbar position to POS
		   (setf pos (get-trackbar-pos "Primary" window-name))
		   ;Print primary trackbar position on window
		   (setf text (concatenate 'string "Primary trackbar position: " 
					   (write-to-string pos)))
		   (put-text clone text org +font-hershey-triplex+
			     scale  color thickness +aa+)
		   ;Create 4 slave trackbars
		   (create-trackbar "Slave 1" window-name unused-val 100)
		   (create-trackbar "Slave 2" window-name unused-val 100)
		   (create-trackbar "Slave 3" window-name unused-val 100)
		   (create-trackbar "Slave 4" window-name unused-val 100)
		   ;Set slave trackbars position to POS
		   (set-trackbar-pos "Slave 1" window-name pos)
		   (set-trackbar-pos "Slave 2" window-name pos)
		   (set-trackbar-pos "Slave 3" window-name pos)
		   (set-trackbar-pos "Slave 4" window-name pos)
		   (imshow window-name clone))
		 (let ((c (wait-key 33)))
		   (when (= c 27)
		     (return)))))))))))



WAIT-KEY

Waits for a pressed key.

C++: int waitKey(int delay=0)

LISP-CV: (WAIT-KEY &OPTIONAL ((DELAY :INT) 0))

    Parameters:	DELAY - Delay in milliseconds. 0 is the special value that means “forever”.

The function WAIT-KEY waits for a key event infinitely when (<= DELAY 0), for DELAY milliseconds wh-
en it is positive. Since the OS has a minimum time between switching threads, the function will not 
wait exactly delay ms, it will wait at least delay ms, depending on what else is running on your co-
mputer at that time. It returns the code of the pressed key or -1 if no key was pressed before the 
specified time had elapsed.

Note

This function is the only method in HighGUI that can fetch and handle events, so it needs to be cal-
led periodically for normal event processing unless HighGUI is used within an environment that take-
s care of event processing.

Note

The function only works if there is at least one HighGUI window created and the window is active. I-
f there are several HighGUI windows, any of them can be active.


(defun wait-key-example ()

  "After window is created with NAMED-WINDOW 
   and moved with MOVE-WINDOW this function 
   waits until a keypress(ESC) is detected w-
   ith the function WAIT-KEY until it runs t-
   he function DESTROY-WINDOW. Note: window 
   must be active before the key press will 
   be detected."

  (let* ((window-name "WAIT-KEY Example"))
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))



HIGHGUI - QT NEW FUNCTIONS



DISPLAY-OVERLAY

Displays a text on a window image as an overlay for a specified duration.

C++: void displayOverlay(const String& winname, const String& text, int delayms=0 )

LISP-CV: (DISPLAY-OVERLAY (WINNAME *STRING) (TEXT *STRING) &OPTIONAL ((DELAYMS :INT) 0)) => :VOID


    Parameters:	

        NAME - Name of the window.

        TEXT - Overlay text to write on a window image.

        DELAYMS - The period (in milliseconds), during which the overlay text is displayed. If this 
                  function is called before the previous overlay text timed out, the timer is restarted 
                  and the text is updated. If this value is zero, the text never disappears.

The function DISPLAY-OVERLAY displays useful information/tips on top of the window for a certain amount 
of time delayms. The function does not modify the image, displayed in the window, that is, after the 
specified delay the original content of the window is restored.


(defun display-overlay-example (filename &optional (cap 0))

  (with-capture (cap (video-capture cap))
    (let ((window-name "DISPLAY-OVERLAY Example"))
      (with-named-window (window-name +window-normal+)
	(move-window window-name 759 175)
	(with-mat ((image (imread filename 1)))
	  (if (empty image) 
	      (return-from display-overlay-example
		(format t "Image not loaded")))
	  (loop
	     (with-mat ((frame (mat)))
	       (cap-read cap frame)
	       (display-overlay window-name "This is a test" 1)
	       (imshow window-name frame)
	       (let ((c (wait-key 33)))
		 (when (= c 27)
		   (return))))))))))





GET-WINDOW-PROPERTY


Provides parameters of a window.


C++: double getWindowProperty(const string& winname, int prop_id)

LISP-CV: (GET-WINDOW-PROPERTY (WINNAME (:pOINTER STRING*)) (PROP-ID) :INT) => :DOUBLE


          Parameters:	

              WINNAME - Name of the window.

              PROP-ID -

              Window property to retrieve. The following operation flags are available:


                     +WND-PROP-FULLSCREEN+ Change if the window is fullscreen (+WINDOW-NORMAL+ or 
                                           +WINDOW-FULLSCREEN+).

                     +WND-PROP-AUTOSIZE+ Change if the window is resizable (+WINDOW-NORMAL+ or 
                                         +WINDOW-AUTOSIZE+).

                     +WND-PROP-ASPECTRATIO+ Change if the aspect ratio of the image is preserved,
                                            (+WINDOW-FREERATIO+ or +WINDOW-KEEPRATIO+).



Note: See (SET-WINDOW-PROPERTY) to know the meaning of the returned values.


The function GET-WINDOW-PROPERTY returns properties of a window.


(defun get-window-property-example (filename)

  ;; Read in image
  (let* ((image (imread filename 1))
	 (window-name "GET-WINDOW-PROPERTY Example"))
    (if (empty image) 
	(return-from get-window-property-example 
	  (format t "Image not loaded")))
    ;; Create window
    (named-window window-name +window-normal+)
    ;; Set window to fullscreen
    (set-window-property window-name +wnd-prop-fullscreen+ 
			 +window-fullscreen+)
    ;; If aspect ratio is not stretched, set to stretched
    (if (equal 1.0d0 (get-window-property window-name +wnd-prop-fullscreen+)) 
	(set-window-property window-name +wnd-prop-aspectratio+ 
			     +window-freeratio+))
    ;; Show image
    (imshow window-name image)
    (loop while (not (= (wait-key 0) 27)))
    (del-mat image)
    (destroy-window window-name)))



SET-WINDOW-PROPERTY


Changes parameters of a window dynamically.


C++: void setWindowProperty(const string& winname, int prop_id, double prop_value)

LISP-CV: (SET-WINDOW-PROPERTY (WINNAME *STRING) (PROP-ID :INT) (PROP-VALUE :DOUBLE)) => :VOID


    Parameters:	

        WINNAME - Name of the window.

        PROP-ID -

           Window property to edit. The following operation flags are available:


               +WND-PROP-FULLSCREEN+ Change if the window is fullscreen (+WINDOW-NORMAL+ or 
                                     +WINDOW-FULLSCREEN+).

               +WND-PROP-AUTOSIZE+ Change if the window is resizable (+WINDOW-NORMAL+ or 
                                   +WINDOW-AUTOSIZE+).

               +WND-PROP-ASPECTRATIO+ Change if the aspect ratio of the image is preserved,
                                      (+WINDOW-FREERATIO+ or +WINDOW-KEEPRATIO+).

        PROP-VALUE -

           New value of the window property. the following operation flags are available:


               +WINDOW-NORMAL+ Change the window to normal size or make the window resizable.

               +WINDOW-AUTOSIZE+ Constrain the size by the displayed image. the window is not resizable.

               +WINDOW-FULLSCREEN+ Change the window to fullscreen.

               +WINDOW-FREERATIO+ Make the window resizable without any ratio constraints.

               +WINDOW-KEEPRATIO+ Make the window resizable, but preserve the proportions of the di-
                                  splayed image.



The function SET-WINDOW-PROPERTY enables changing properties of a window.


(defun set-window-property-example (filename)
  ;; Read in image
  (let* ((image (imread filename 1))
	 (window-name "SET-WINDOW-PROPERTY Example"))
    (if (empty image) 
	(return-from set-window-property-example 
	  (format t "Image not loaded")))
    ;; Create window
    (named-window window-name +window-normal+)
    ;; Set window to fullscreen
    (set-window-property window-name +wnd-prop-fullscreen+ 
			 +window-fullscreen+)
    ;; Show image
    (imshow window-name image)
    (loop while (not (= (wait-key 0) 27)))
    (del-mat image)
    (destroy-window window-name)))



FEATURES2D - COMMON INTERFACES OF DESCRIPTOR MATCHERS



DESCRIP-MATCHER-MATCH

Finds the best match for each descriptor from a query set.

C++: void DescriptorMatcher::match(InputArray queryDescriptors, InputArray trainDescriptors, vector<DMatch>& matches, 
     InputArray mask=noArray() ) const

LISP-CV: (DESCRIP-MATCHER-MATCH (SELF FEATURE-2D) (QUERY-DESCRIPTORS MAT) (TRAIN-DESCRIPTORS MAT) 
         (MATCHES VECTOR-DMATCH) (MASK MAT)) => :VOID

    Parameters:	

        SELF - A FEATURE-2D contruct e.g. BF-MATCHER

        QUERY-DESCRIPTORS - Query set of descriptors.

        TRAIN-DESCRIPTORS - Train set of descriptors. This set is not added to the train descriptors 
                            collection stored in the class object.

        MATCHES - Matches. If a query descriptor is masked out in mask , no match is added for this 
                  descriptor. So, matches size may be smaller than the query descriptors count.

        MASK - Mask specifying permissible matches between an input query and train matrices of descriptors.


In this function the train descriptors are passed as an input argument. An optional mask can be passed 
to specify which query and training descriptors can be matched. Namely, (QUERY-DESCRIPTORS I) can be 
matched with (TRAIN-DESCRIPTORS J) only if (AT MASK I J :UCHAR) is non-zero.


Example:

See BRISK-EXAMPLE



OBJDETECT - CASCADE CLASSIFICATION:



CASCADE-CLASSIFIER


Creates a CASCADE-CLASSIFIER object or loads a classifier from a file.


C++: CascadeClassifier::CascadeClassifier()

C++: CascadeClassifier::CascadeClassifier(const string& filename)

LISP-CV: (CASCADE-CLASSIFIER) => CASCADE-CLASSIFIER

LISP-CV: (CASCADE-CLASSIFIER (FILENAME *STRING)) => CASCADE-CLASSIFIER


    Parameters:	

        SELF - A CASCADE-CLASSIFIER object

        FILENAME - Name of the file from which the classifier is loaded.


Example:


;Create an uninitialized CASCADE-CLASSIFIER object

LISP-CV> (DEFPARAMETER FACE-CASCADE (CASCADE-CLASSIFIER))

FACE-CASCADE


;Create a CASCADE-CLASSIFIER object initialized with an XML classifier 

LISP-CV> (DEFPARAMETER FACE-CASCADE-NAME "<opencv_source_directory>/data/haarcascades/haarcascade_frontalface_alt.xml")

FACE-CASCADE-NAME

LISP-CV> (DEFPARAMETER FACE-CASCADE (CASCADE-CLASSIFIER FACE-CASCADE-NAME))

FACE-CASCADE




CASCADE-CLASSIFIER-LOAD

Loads a classifier from a file.

C++: bool CascadeClassifier::load(const string& filename)

LISP-CV: (CASCADE-CLASSIFIER-LOAD (SELF CASCADE-CLASSIFIER) (FILENAME *STRING)) => :BOOLEAN


    Parameters:	

        SELF - A CASCADE-CLASSIFIER object

        FILENAME - Name of the file from which the classifier is loaded. The file may contain an old 
                   HAAR classifier trained by the haartraining application or a new cascade classifier 
                   trained by the traincascade application.


Example:


LISP-CV> (DEFPARAMETER FACE-CASCADE-NAME "<opencv_source_directory>/data/haarcascades/haarcascade_frontalface_alt.xml")

FACE-CASCADE-NAME

LISP-CV> (DEFPARAMETER FACE-CASCADE (CASCADE-CLASSIFIER)) ;Create CASCADE-CLASSIFIER object 

FACE-CASCADE

LISP-CV> (CASCADE-CLASSIFIER-LOAD FACE-CASCADE FACE-CASCADE-NAME)  ;Load the Classifier

T ;<--- Operation successful




DETECT-MULTI-SCALE

Detects objects of different sizes in the input image. The detected objects are returned as a list of rectangles.

C++: void CascadeClassifier::detectMultiScale(InputArray image, vector<Rect>& objects, double scaleFactor=1.1, 
                                              int minNeighbors=3, int flags=0, Size minSize=Size(), 
                                              Size maxSize=Size())

LISP-CV: (DETECT-MULTI-SCALE (SELF CASCADE-CLASSIFIER) (IMAGE MAT) (OBJECTS VECTOR-RECT) &OPTIONAL 
                            ((SCALE-FACTOR :DOUBLE) 1.1D0) ((MIN-NEIGHBORS :INT) 3) ((FLAGS :INT) 0) 
                           ((MIN-SIZE SIZE) (SIZE)) ((MAX-SIZE SIZE) (SIZE))) => :VOID

C++: void CascadeClassifier::detectMultiScale(InputArray image, vector<Rect>& objects, vector<int>& numDetections, 
                                              double scaleFactor=1.1, int minNeighbors=3, int flags=0, 
                                              Size minSize=Size(), Size maxSize=Size())

LISP-CV: (DETECT-MULTI-SCALE (SELF CASCADE-CLASSIFIER) (IMAGE MAT) (OBJECTS VECTOR-RECT) (NUM-DETECTIONS VECTOR-INT) 
                              &OPTIONAL (SCALE-FACTOR :DOUBLE) 1.1D0) ((MIN-NEIGHBORS :INT) 3) ((FLAGS :INT) 0)
                            ((MIN-SIZE SIZE) (SIZE)) ((MAX-SIZE SIZE) (SIZE))) => :VOID

C++: void CascadeClassifier::detectMultiScale(const Mat& image, vector<Rect>& objects, std::vector<int>& rejectLevels, 
                                              vector<double>& levelWeights, double scaleFactor = 1.1, int minNeighbors = 3, 
                                              int flags = 0, Size minSize = Size(), Size maxSize = Size(), 
                                              bool outputRejectLevels = false )

LISP-CV: (DETECT-MULTI-SCALE (SELF CASCADE-CLASSIFIER) (IMAGE MAT) (OBJECTS VECTOR-RECT) (NUM-DETECTIONS VECTOR-INT) &OPTIONAL 
         (SCALE-FACTOR :DOUBLE) 1.1D0) ((MIN-NEIGHBORS :INT) 3) ((FLAGS :INT) 0) ((MIN-SIZE SIZE) (SIZE)) 
         ((MAX-SIZE SIZE) (SIZE))) => :VOID


    Parameters:	

        SELF - A CASCADE-CLASSIFIER object

        IMAGE - Matrix of the type +8U+ containing an image where objects are detected.

        OBJECTS - Vector of rectangles where each rectangle contains the detected object, the rectangles
                  may be partially outside the original image.

        NUM-DETECTIONS - Vector of detection numbers for the corresponding objects. An object’s number 
                         of detections is the number of neighboring positively classified rectangles 
                         that were joined together to form the object.

        SCALE-FACTOR - Parameter specifying how much the image size is reduced at each image scale.

        MINN-EIGHBORS - Parameter specifying how many neighbors each candidate rectangle should have to retain it.

        FLAGS - Can be one of the following:

                              +CASCADE-DO-CANNY-PRUNING+ 

                              +CASCADE-SCALE-IMAGE+ 

                              +CASCADE-FIND-BIGGEST-OBJECT+ 

                              +CASCADE-DO-ROUGH-SEARCH+  

        MIN-SIZE - Minimum possible object size. Objects smaller than that are ignored.

        MAX-SIZE - Maximum possible object size. Objects larger than that are ignored.

This function is parallelized with the TBB library.


;Global variables

(defparameter face-cascade-name "<open-cv-src-dir>/data/haarcascades/haarcascade_frontalface_alt.xml")

;Create CASCADE-CLASSIFIER object
(defparameter face-cascade (cascade-classifier))
;Number of file to be saved
(defparameter filenumber 0)
;Name of file to be saved 
(defparameter filename 0) 
(defparameter crop (mat))



(defun detect-and-display (frame)

  (let ((text 0)
        (num-buffers 2)
        (size-factor 1.1d0)
	(area-current 0) 
	(index-biggest 0)
	(area-biggest 0) 
	(faces-list 0))
    (with-vector-rect ((faces (vec-rect)))
      (with-mat ((frame-gray (mat))
		 (res (mat))
		 (gray (mat)))
	(cvt-color frame frame-gray +bgr2gray+)
	(equalize-hist frame-gray frame-gray)
	;Detect faces
	(with-size ((face-size (size 30 30)))
	  (detect-multi-scale face-cascade frame-gray faces size-factor 
			      num-buffers +cascade-do-canny-pruning+ face-size) 1)
	;Convert VECTOR-RECT to a Lisp list for speed
	(setf faces-list (vec-rect :to-lisp-list faces))
	;Set Region of Interest...
	(with-rect ((roi (rect)))
	  ;Iterate through all current elements (detected faces)
	  (dotimes (index-current (length faces-list))
	    ;Get the area of current element (detected face)
            ;AREA-CURRENT is area of current element
	    (setf area-current (* (rect-width (nth index-current faces-list))  ;AREA-CURRENT is area 
   				  (rect-height (nth index-current faces-list)))) ;of current element
            ;INDEX-BIGGEST is index of the biggest element
	    (setf roi (rect-clone (nth index-biggest faces-list))) 

	    ;Get the area of biggest element, at the 
            ;beginning it is same as current element
        
            ;AREA-BIGGEST is area of the biggest element
	    (setf area-biggest (* (rect-width (nth index-biggest faces-list)) 
                                  (rect-height (nth index-biggest faces-list)))) 
            (if (> area-current area-biggest)
		(progn 
		  (setf index-biggest index-current)
		  (setf roi (rect-clone (nth index-biggest faces-list)))) nil)

	    (setf crop (gc:roi frame roi))
	    ;This will be needed later while saving images
	    (resize crop res (size 128 128) 0d0 0d0 +inter-linear+)
	    ;Convert cropped image to Grayscale
	    (cvt-color crop gray +bgr2gray+)
	    ;Form a filename
	    (setf filename (concatenate 'string "/home/user/Pictures/my-face/my-face-" 
					(write-to-string filenumber) ".png"))
	    (incf filenumber)

	    (imwrite filename gray)
	    ;Display detected faces on main window - live stream from camera
	    (with-point ((pt1 (point (rect-x (nth index-current faces-list)) 
				     (rect-y (nth index-current faces-list))))
			 (pt2 (point (+ (rect-x (nth index-current faces-list)) 
					(rect-height (nth index-current faces-list)))
				     (+ (rect-y (nth index-current faces-list)) 
					(rect-width (nth index-current faces-list))))))
	      (with-scalar ((color1 (scalar 0 255 0)))
		(rectangle frame pt1 pt2 color1 2 8 0))))

	(setf text (concatenate 'string "Crop area size: " 
				(write-to-string (rect-width roi)) "x" 
				(write-to-string (rect-height roi)) " Filename: " 
				(write-to-string filename))))
	(with-point ((org (point 30 30)))
	  (with-scalar ((color2 (scalar 0 0 255)))	
	    (put-text frame text org +font-hershey-complex-small+ 
		      0.8d0 color2 1 +aa+)))))))



(defun detect-multi-scale-example (&optional 
				     (cam *camera-index*) 
				     (width *default-width*)
				     (height *default-height*))

  ;Setting width/height to 2 less than default width-height vastly
  ;improves the speed of DETECT-MULTI-SCALE in this example
  (with-captured-camera (cap cam :width (- width 2) :height (- height 2))
    (let ((window-name "Original - DETECT-MULTI-SCALE Example"))
      ;Check if camera is opened
      (if (not (cap-is-open cap)) 
	  (return-from detect-multi-scale-example 
	    (format t "Cannot open the video camera")))
      ;Load the cascade
      (if (not (cascade-classifier-load face-cascade face-cascade-name))
	  (return-from detect-multi-scale-example 
	    (format t "Error Loading")))
      (with-named-window (window-name +window-autosize+)
	  (move-window window-name 0 300)
	  (loop
             ;Read the video stream
	     (with-mat ((frame (mat)))
	       (cap-read cap frame)
	       (if (not (empty frame)) 
                   ;Apply the classifier to the frame
		   (detect-and-display frame)
		   (format t "No captured frame: Break!"))
	       ;Show camera feed
	       (imshow window-name frame)
	       ;Show image
	       (if (not (empty crop)) 
		   (progn (imshow "Detected" crop)
			  (setf crop (gc:mat)))
		   (destroy-window "Detected"))
	       (let ((c (wait-key 33)))
		 (when (= c 27)
		   (destroy-window "Detected")
		   (return)))))))))



ML - NORMAL BAYES CLASSIFIER



NORMAL-BAYES-CLASSIFIER

Default and training constructors.

C++: CvNormalBayesClassifier::CvNormalBayesClassifier()

C++: CvNormalBayesClassifier::CvNormalBayesClassifier(const Mat& trainData, const Mat& responses, const Mat& varIdx=Mat(), 
                                                      const Mat& sampleIdx=Mat() )

LISP-CV: (NORMAL-BAYES-CLASSIFIER &OPTIONAL (TRAIN-DATA MAT) (RESPONSES MAT) ((VAR-IDX MAT) (MAT) GIVEN-VAR-IDX) 
                                ((SAMPLE-IDX MAT) (MAT) GIVEN-SAMPLE-IDX)) => NORMAL-BAYES-CLASSIFIER


The constructors follow conventions of (STAT-MODEL). See (STAT-MODEL-TRAIN) for parameters descriptions.



Example:


;; This example implements a feed-forward Artificial Neural Network or 
;; more particularly, Multi-Layer Perceptrons (MLP), the most commonly 
;; used type of Neural Networks. MLP consists of the input layer, outp-
;; ut layer, and one or more hidden layers. Each layer of MLP includes 
;; one or more neurons directionally linked with the neurons from the 
;; previous and the next layer. Download the PDF at this link for more 
;; details.

;; https://github.com/bytefish/opencv/blob/master/machinelearning/doc/machinelearning.pdf 

;; Note: This example is similar to, but is a more advanced 
;; and informative version of the ANN-MLP-EAXMPLE in this file.

;; Declare global variables.

(defparameter *plot-support-vectors* t)
(defparameter *num-training-points* 200)
(defparameter *num-test-points* 2000)
(defparameter *size* 200)

;; Move the trackbar on the middle window to decide 
;; which equation in the (F) function to use.

(defparameter *equation* (alloc :int 2))

;; Move the trackbars on the middle window to decide 
;; what to multiply X by in the (F) function equatio-
;; ns. Each trackbar in this list corresponds to a s-
;; etting of the *EQUATION* variable and affects the 
;; equations in the (F) function.

;; Note: When changing any of the five below values 
;; with the trackbar, make sure not to move the trac-
;; kbar to less than 1, or the program will freeze. 
;; Leaving this possibility allows for more precise 
;; adjustments.

(defparameter *equation-0* (alloc :int 10))
(defparameter *equation-1* (alloc :int 10))
(defparameter *equation-2* (alloc :int 2))
(defparameter *equation-3* (alloc :int 10))
(defparameter *other* (alloc :int 10))

;; You can change the number of Neurons per layer wi-
;; the trackbars, though it is reccomended that you 
;; read the notes below before you do so. See the co-
;; mments in the (MLP) function under the LAYERS hea-
;; ding for more information.

;; The number of Neurons in some of the layers are n-
;; ot adjustable, or, they are not easily adjustable. 
;; I left the oppurtunity for you to try because the 
;; C++ errors in *INFERIOR-LISP*(or your implementat-
;; ions version) offer good information and are usef-
;; ul debug tools.

;; Note 1: *LAYER-0* needs to stay on 2. Take a note 
;; of the C++ error in *INFERIOR-LISP* if you do dec-
;; ide to change it.

;; Note 2: If the value of *LAYER-1* goes below two 
;; you will get an error in *INFERIOR-LISP*. I could 
;; have added two to the value, when *LAYER-1* is cal-
;; led, to subvert the error, but I didn't for the sa-
;; ke of precision. Nou you will be able to see exac-
;; tly how many Neurons you're creating with the tra-
;; ckbar in Layer 1(and all layers).

;; Note 3: *LAYER-2* operates the same as *LAYER-1*.
;; See Note 2 for more details.

;; Note 4: *LAYER-3* operates the same as *LAYER-0*, 
;; but needs to stay on 1. See Note 1 for details.

(defparameter *layer-0* (alloc :int 2)) 
(defparameter *layer-1* (alloc :int 10))
(defparameter *layer-2* (alloc :int 15))
(defparameter *layer-3* (alloc :int 1))

;; Calculates the accuracy of the Normal Bayes Classifier and the Neural Network. 
;; The accuracy is affected by the equations and equation's parameters you choose 
;; for the (F) function and also by the number of Neurons per layer you choose in 
;; the (MLP) function.

(defun evaluate (predicted actual &optional p a (*t 0) (f 0)) 
  (if (eq (rows predicted) (rows actual))
      (dotimes (i (rows actual))
	(setf p (at predicted i 0 :float))
	(setf a (at actual i 0 :float))
	(if (or (and (>= p 0.0f0) (>= a 0.0f0)) (and (<= p 0.0f0) (<= a 0.0f0))) 
	    (incf *t)
	    (incf f))) nil)
  (float (/ (* *t 1) (+ *t f))))

;; Function to learn. The trackbars on the middle window change the values held in 
;; the EQUATION-* variables. The (? *EQUATION-* :INT) statements dereference those 
;; variables and supply their values to the equations in this function. The '?' is 
;; a macro for CFFI's dereferencing function, MEM-AREF. Again, make sure not to mo-
;; ve the trackbar to less than 1 when adjusting these variables.

(defun f (x y equation) 
  (case equation (0 (return-from f (if (> y (sin (* x (+ (? *equation-0* :int) 1)))) -1 1)))
	(1 (return-from f (if (> y (cos (* x (+ (? *equation-1* :int) 1)))) -1 1)))
	(2 (return-from f (if (> y (* x (+ (? *equation-2* :int) 1))) -1 1)))
	(3 (return-from f (if (> y (tan (* x (? *equation-3* :int)))) -1 1)))
	(otherwise (return-from f (if (> y (cos (* x (? *other* :int)))) -1 1)))))

;; NORMAL BAYES CLASSIFIER
(defun bayes (training-data training-classes test-data test-classes) 
  (with-normal-bayes-classifier ((bayes (normal-bayes-classifier training-data training-classes)))
    (with-mat ((predicted (mat (rows test-classes) 1 +32f+)))
      (dotimes (i (rows test-data))
	(with-mat ((sample (row test-data i)))
	  (setf (at predicted i 0 :float) (normal-bayes-classifier-predict bayes sample))))
      ;; Calculate the accuracy of the Normal Bayes Classifier
      (let ((evaluate (evaluate predicted test-classes)))
	(format t "~%Accuracy_{BAYES} = ~a~%" evaluate))
      ;; Plot the predictions
      (plot-binary test-data predicted "Predictions Bayes"))))

;; NEURAL NETWORK

(defun mlp (training-Data training-Classes test-Data test-Classes) 

  ;; LAYERS

  ;; The purpose of a neural network is to generalize, which is the ability 
  ;; to approximate outputs for inputs not available in the training set. W-
  ;; hile small networks may not be able to approximate a function, large n-
  ;; etworks tend to overfit and not find any relationship in data. It has 
  ;; been shown that, given enough data, a multi layer perceptron with one 
  ;; hidden layer can approximate any continuous function to any degree of 
  ;; accuracy. Here the number of neurons per layer is stored in the row-o-
  ;; rdered MAT below, LAYERS.

  (with-mat ((layers (mat 4 1 +32SC1+)))
    (setf (at layers 0 0 :int) (? *layer-0* :int)) 
    (setf (at layers 1 0 :int) (? *layer-1* :int))
    (setf (at layers 2 0 :int) (? *layer-2* :int))
    (setf (at layers 3 0 :int) (? *layer-3* :int))
    (with-ann-mlp ((mlp (ann-mlp)))
      (with-term-criteria ((criteria (term-criteria (logior +termcrit-iter+ +termcrit-eps+) 
						    100 0.00001d0)))
	(with-ann-mlp-train-params ((params (ann-mlp-train-params criteria 
								  +ann-mlp-train-params-backprop+ 
								  0.05d0 
	 							  0.05d0)))
	  (ann-mlp-create mlp layers)
	  ;; Train the Neural Net.
	  (with-mat ((predicted (mat (rows test-classes) 1 +32f+))
                     (sample-weights (mat))
                     (sample-idx (mat)))
	    (ann-mlp-train mlp training-data training-classes sample-weights sample-idx params)
	    (dotimes (i (rows test-data))
	      (with-mat ((response (mat 1 1 +32fc1+)))
		(with-mat ((sample (row test-data i)))
		  (ann-mlp-predict mlp sample response))
		(setf (at predicted i 0 :float) (at response 0 0 :float))))
            ;; Print the values of all adjustable variables.
            (format t "~%*EQUATION* = ~a~%" (? *equation* :int))
            (format t "~%*EQUATION-0* = ~a~%" (? *equation-0* :int))
            (format t "~%*EQUATION-1* = ~a~%" (? *equation-1* :int))
            (format t "~%*EQUATION-2* = ~a~%" (? *equation-2* :int))
            (format t "~%*EQUATION-3* = ~a~%" (? *equation-3* :int))
            (format t "~%*OTHER* = ~a~%" (? *other* :int))
            (format t "~%*LAYER-0* = ~a~%" (? *layer-0* :int))
            (format t "~%*LAYER-1* = ~a~%" (? *layer-1* :int))
            (format t "~%*LAYER-2* = ~a~%" (? *layer-2* :int))
            (format t "~%*LAYER-3* = ~a~%" (? *layer-3* :int))
            ;; Calculate the accuracy of the Neural Net
            (let ((evaluate (evaluate predicted test-classes)))
	      (format t "~%Accuracy_{MLP} = ~a~%" evaluate))
            ;; Plot the predictions
	    (plot-binary test-data predicted "Predictions Backpropagation")
	    nil ))))))

;; Plot Data and Class function
(defun plot-binary(data classes name &optional x y) 
  (with-mat ((plot (mat *size* *size* +8uc3+)))
    (with-scalar ((scalar (scalar 255 255 255)))
      (assgn-val plot scalar))
    (dotimes (i (rows data))
      (setf x (* (at data i 0 :float) *size*))
      (setf y (* (at data i 1 :float) *size*))
      (with-scalar ((color1 (rgb 255 0 0))
		    (color2 (rgb 0 255 0)))
	(if (> (at classes i 0 :float) 0f0)
            ;; Plot the points with the CIRCLE function
	    (with-point ((center (point (round x) (round y))))
	      (circle plot center 1 color1 1))
	    (with-point ((center (point (round x) (round y))))
	      (circle plot center 1 color2 1)))))
    (imshow name plot)))


(defun normal-bayes-classifier-example ()

  "In this example, in the 2 right-most windows, a Normal Bayes 
   Classifier is compared side by side with a Neural Network."
  ;; Window names available to all of the functions
  (let ((window-name-1 "Training data")
	(window-name-2 "Test data")
	(window-name-3 "Predictions Backpropagation")
        (window-name-4 "Predictions Bayes")
        ;; Declare other variables
        (training-classes 0)
        (test-classes 0)
        (x 0)
        (y 0))
    ;; Create windows
    (with-named-window (window-name-1 +window-normal+)
      (with-named-window (window-name-2 +window-autosize+)
	(with-named-window (window-name-3 +window-normal+)
	  (with-named-window (window-name-4 +window-normal+)
	    ;; Move windows to specified locations
	    (move-window window-name-1 85 175)
	    (move-window window-name-2 549 175)
	    (move-window window-name-3 971 175)
	    (move-window window-name-4 1435 175)
	    ;; Create trackbars used to adjust the values in the (F) 
            ;; and (MLP) functions. The trackbar names are hints at 
            ;; how not to adjust them e.g. This 'Eq 0: > 1' means do
            ;; not let the trackbar go below 1 and this 'Layr 0: 2'
            ;; means the trackbar must stay at 2. Not abiding by the
            ;; guidelines will cause the program to freeze.
	    (create-trackbar "Equation" window-name-2 *equation* 4)
	    (create-trackbar "Eq 0: > 1" window-name-2 *equation-0* 150)
	    (create-trackbar "Eq 1: > 1" window-name-2 *equation-1* 150)
	    (create-trackbar "Eq 2: > 1" window-name-2 *equation-2* 10)
	    (create-trackbar "Eq 3: > 1" window-name-2 *equation-3* 150)
	    (create-trackbar "Other: > 1" window-name-2 *other* 150)
	    (create-trackbar "Lyr 0: 2" window-name-2 *layer-0* 10)
	    (create-trackbar "Lyr 1: > 2" window-name-2 *layer-1* 500)
	    (create-trackbar "Lyr 2: > 2 " window-name-2 *layer-2* 500)
	    (create-trackbar "Lyr 3: 1" window-name-2 *layer-3* 5)
	    (with-mat ((training-data (mat *num-training-points* 2 +32fc1+))
		       (test-data (mat *num-test-points* 2 +32fc1+)))

	      (loop;; Fill training and test data matrices 
		   ;; with random numbers from zero to one
		 (with-scalar ((zero (scalar 0))
			       (one (scalar 1)))
		   (randu training-data zero one)
		   (randu test-data zero one))

		 ;; Label data with equation
		 (with-mat ((labels1 (mat (rows training-data) 1 +32fc1+)))
		   (dotimes (i (rows training-data))
		     (setf x (at training-data i 0 :float))
		     (setf y (at training-data i 1 :float))
		     (setf (at labels1 i 0 :float) (coerce (f x y (? *equation* :int)) 'float)))

		   (with-mat ((labels2 (mat (rows test-data) 1 +32fc1+)))
		     (dotimes (i (rows test-data))
		       (setf x (at test-data i 0 :float))
		       (setf y (at test-data i 1 :float))
		       (setf (at labels2 i 0 :float) (coerce (f x y (? *equation* :int)) 'float)))

		     (setf training-classes labels1)
		     (setf test-classes labels2)

		     ;; Plot training data
		     (plot-binary training-data training-classes window-name-1)
		     ;; Plot test data
		     (plot-binary test-data test-classes window-name-2)
		     ;; Plot predictions
		     (mlp training-data training-classes test-data test-classes)
		     (bayes training-data training-classes test-data test-classes)))
		 (let ((c (wait-key 33)))
		   (when (= c 27)
		     (return)))))))))))



NORMAL-BAYES-CLASSIFIER-PREDICT

Predicts the response for sample(s).

C++: float CvNormalBayesClassifier::predict(const Mat& samples, Mat* results=0, Mat* results_prob=0 ) const

LISP-CV: (NORMAL-BAYES-CLASSIFIER-PREDICT (SELF NORMAL-BAYES-CLASSIFIER) (SAMPLES MAT) ((RESULTS MAT) (NULL-POINTER)) 
                                          ((RESULTS-PROB MAT) (NULL-POINTER))) => :FLOAT

The method estimates the most probable classes for input vectors. Input vectors (one or more) are 
stored as rows of the matrix samples. In case of multiple input vectors, there should be one output 
vector RESULTS. The predicted class for a single input vector is returned by the method. The vector 
RESULTS-PROB contains the output probabilities coresponding to each element of RESULTS.

The function is parallelized with the TBB library.


Example:

See NORMAL-BAYES-CLASSIFIER-EXAMPLE in this file.



ML - NEURAL NETWORKS


ANN-MLP

The constructors.

C++: CvANN_MLP::CvANN_MLP()

C++: CvANN_MLP::CvANN_MLP(const CvMat* layerSizes, int activateFunc=CvANN_MLP::SIGMOID_SYM, double fparam1=0, double fparam2=0 )

LISP-CV: (ANN-MLP &OPTIONAL (LAYER-SIZES MAT) ((ACTIVATE-FUNC :INT) +ANN-MLP-SIGMOID-SYM+) ((FPARAM1 :DOUBLE) 0D0) 
                            ((FPARAM2 :DOUBLE) 0D0)) => ANN-MLP

The advanced constructor allows to create MLP with the specified topology. See (ANN-MLP-CREATE) for 
details.


Example:

;; This example implements a feed-forward Artificial Neural Network or 
;; more particularly, Multi-Layer Perceptrons (MLP), the most commonly 
;; used type of Neural Networks. MLP consists of the input layer, outp-
;; ut layer, and one or more hidden layers. Each layer of MLP includes 
;; one or more neurons directionally linked with the neurons from the 
;; previous and the next layer. 


;; Declare global variables.

(defparameter *plot-support-vectors* t)
(defparameter *num-training-points* 200)
(defparameter *num-test-points* 2000)
(defparameter *size* 200)

;; Move the trackbar on the middle window to decide 
;; which equation in the (F) function to use.

(defparameter *equation* (alloc :int 2))

;; Move the trackbars on the middle window to decide 
;; what to multiply X by in the (F) function equatio-
;; ns. Each trackbar in this list corresponds to a s-
;; etting of the *EQUATION* variable and affects the 
;; equations in the (F) function.

(defparameter *equation-0* (alloc :int 10))
(defparameter *equation-1* (alloc :int 10))
(defparameter *equation-2* (alloc :int 2))
(defparameter *equation-3* (alloc :int 10))
(defparameter *equation-other* (alloc :int 10))


;; Calculates the accuracy of the Neural Network. the NN 
;; accuracy is affected the equations and equation's par-
;; ameters you choose for the (F) function.

(defun evaluate (predicted actual &optional p a (*t 0) (f 0)) 
  (if (eq (rows predicted) (rows actual))
      (dotimes (i (rows actual))
	(setf p (at predicted i 0 :float))
	(setf a (at actual i 0 :float))
	(if (or (and (>= p 0.0f0) (>= a 0.0f0)) (and (<= p 0.0f0) (<= a 0.0f0))) 
	    (incf *t)
	    (incf f))) nil)
  (float (/ (* *t 1) (+ *t f))))


;; Function to learn. The trackbars on the middle window change the values held in 
;; the EQUATION-* variables. The (? *EQUATION-* :INT) statements dereference those 
;; variables and supply their values to the equations in this function. The ? func-
;; tion, is a macro for CFFI::MEM-AREF.

(defun f (x y equation) 
  (case equation (0 (return-from f (if (> y (sin (* x (? *equation-0* :int)))) -1 1)))
	(1 (return-from f (if (> y (cos (* x (? *equation-1* :int)))) -1 1)))
	(2 (return-from f (if (> y (* x (? *equation-2* :int))) -1 1)))
	(3 (return-from f (if (> y (tan (* x (? *equation-3* :int)))) -1 1)))
	(otherwise (return-from f (if (> y (cos (* x (? *equation-other* :int)))) -1 1)))))



(defun mlp (training-Data training-Classes test-Data test-Classes) 

  (with-mat ((layers (mat 4 1 +32SC1+)))
    (setf (at layers 0 0 :int) 2)
    (setf (at layers 1 0 :int) 10)
    (setf (at layers 2 0 :int) 15)
    (setf (at layers 3 0 :int) 1)
    (with-ann-mlp ((mlp (ann-mlp)))
      (with-term-criteria ((criteria (term-criteria (logior +termcrit-iter+ +termcrit-eps+) 
						    100 0.00001d0)))
	(with-ann-mlp-train-params ((params (ann-mlp-train-params criteria 
								  +ann-mlp-train-params-backprop+ 
								  0.05d0 
								  0.05d0)))
	  (ann-mlp-create mlp layers)
	  ;; Train the NN.
	  (with-mat ((predicted (mat (rows test-classes) 1 +32f+))
                     (sample-weights (mat))
                     (sample-idx (mat)))
	    (ann-mlp-train mlp training-data training-classes sample-weights sample-idx params)
	    (dotimes (i (rows test-data))
	      (with-mat ((response (mat 1 1 +32fc1+)))
		(with-mat ((sample (row test-data i)))
		  (ann-mlp-predict mlp sample response))
		(setf (at predicted i 0 :float) (at response 0 0 :float))))
            (format t "~%*EQUATION* = ~a~%" (? *equation* :int))
            (format t "~%*EQUATION-0* = ~a~%" (? *equation-0* :int))
            (format t "~%*EQUATION-1* = ~a~%" (? *equation-1* :int))
            (format t "~%*EQUATION-2* = ~a~%" (? *equation-2* :int))
            (format t "~%*EQUATION-3* = ~a~%" (? *equation-3* :int))
            (format t "~%*EQUATION-OTHER* = ~a~%" (? *equation-other* :int))
            (let ((evaluate (evaluate predicted test-classes)))
	      (format t "~%Accuracy_{MLP} = ~a~%" evaluate) 
	      (plot-binary test-data predicted "Predictions Backpropagation - ANN-MLP Example"))
	    nil ))))))


;; Plot data and class
(defun plot-binary(data classes name &optional x y) 
  (with-mat ((plot (mat *size* *size* +8uc3+)))
    (with-scalar ((scalar (scalar 255 255 255)))
      (assgn-val plot scalar))
    (dotimes (i (rows data))
      (setf x (* (at data i 0 :float) *size*))
      (setf y (* (at data i 1 :float) *size*))
      (with-scalar ((color1 (rgb 255 0 0))
		    (color2 (rgb 0 255 0)))
	(if (> (at classes i 0 :float) 0f0)
	    (with-point ((center (point (round x) (round y))))
	      (circle plot center 1 color1 1))
	    (with-point ((center (point (round x) (round y))))
	      (circle plot center 1 color2 1)))))
    (imshow name plot)))


(defun ann-mlp-example ()

  (let ((window-name-1 "Training data - ANN-MLP Example")
	(window-name-2 "Test data - ANN-MLP Example")
	(window-name-3 "Predictions Backpropagation - ANN-MLP Example")
        ;; Declare other variables
        (training-classes 0)
        (test-classes 0)
        (x 0)
        (y 0))
    ;; Create windows
    (with-named-window (window-name-1 +window-normal+)
      (with-named-window (window-name-2 +window-autosize+)
	(with-named-window (window-name-3 +window-normal+)
          ;; Move windows to specified locations
	  (move-window window-name-1 310 175)
	  (move-window window-name-2 760 175)
	  (move-window window-name-3 1210 175)
          ;; Create trackbars used to adjust 
          ;; the (F) function equations
	  (create-trackbar "Equation" window-name-2 *equation* 4)
	  (create-trackbar "Equation 0" window-name-2 *equation-0* 150)
	  (create-trackbar "Equation 1" window-name-2 *equation-1* 150)
	  (create-trackbar "Equation 2" window-name-2 *equation-2* 10)
	  (create-trackbar "Equation 3" window-name-2 *equation-3* 150)
	  (create-trackbar "Eq. Other" window-name-2 *equation-other* 150)
	  (with-mat ((training-data (mat *num-training-points* 2 +32fc1+))
		     (test-data (mat *num-test-points* 2 +32fc1+)))

	    (loop;; Fill training and test data matrices 
	       ;; with random numbers from zero to one
	       (with-scalar ((zero (scalar 0))
			     (one (scalar 1)))
		 (randu training-data zero one)
		 (randu test-data zero one))

               ;; Label data with equation
	       (with-mat ((labels1 (mat (rows training-data) 1 +32fc1+)))
		 (dotimes (i (rows training-data))
		   (setf x (at training-data i 0 :float))
		   (setf y (at training-data i 1 :float))
		   (setf (at labels1 i 0 :float) (coerce (f x y (? *equation* :int)) 'float)))

		 (with-mat ((labels2 (mat (rows test-data) 1 +32fc1+)))
		   (dotimes (i (rows test-data))
		     (setf x (at test-data i 0 :float))
		     (setf y (at test-data i 1 :float))
		     (setf (at labels2 i 0 :float) (coerce (f x y (? *equation* :int)) 'float)))

		   (setf training-classes labels1)
		   (setf test-classes labels2)

                   ;; Plot training data
		   (plot-binary training-data training-classes window-name-1)
                   ;; Plot test data
		   (plot-binary test-data test-classes window-name-2)
                   ;; Plot predictions
		   (mlp training-data training-classes test-data test-classes)))
	       (let ((c (wait-key 33)))
		 (when (= c 27)
		   (return))))))))))



ANN-MLP-CREATE

Constructs MLP with the specified topology.

C++: void CvANN_MLP::create(const Mat& layerSizes, int activateFunc=CvANN_MLP::SIGMOID_SYM, double fparam1=0, double fparam2=0 )

LISP-CV: (ANN-MLP-CREATE (SELF ANN-MLP) (LAYER-SIZES MAT) &OPTIONAL ((ACTIVATE-FUNC :INT) +ANN-MLP-SIGMOID-SYM+) 
                         ((FPARAM1 :DOUBLE) 0D0) ((FPARAM2 :DOUBLE) 0D0)) => :VOID


    Parameters:	

        LAYER-SIZES - Integer vector specifying the number of neurons in each layer including the 
                      input and output layers.

        ACTIVATE-FUNC - Parameter specifying the activation function for each neuron: one of;
                        +ANN-MLP-IDENTITY+, +ANN-MLP-SIGMOID-SYM+, +ANN-MLP-GAUSSIAN+ 

        FPARAM1 - Free parameter of the ALPHA activation function. See the formulas in the 
                  introduction section.

        FPARAM2 - Free parameter of the BETA activation function. See the formulas in the 
                  introduction section.


Note: Introduction section of the OpenCV Neural Network documentation is at the top of this link:

http://docs.opencv.org/trunk/modules/ml/doc/neural_networks.html

There you will find a more complete explanation of the activation functions as well as the formulae.


The method creates an MLP network with the specified topology and assigns the same activation function 
to all the neurons.


Example:

See ANN-MLP-EXAMPLE in this file.



ANN-MLP-PREDICT

Predicts responses for input samples.

C++: float CvANN_MLP::predict(const Mat& inputs, Mat& outputs) const

LISP-CV: (ANN-MLP-PREDICT (SELF ANN-MLP) (INPUTS MAT) (OUTPUTS MAT)) => :FLOAT


    Parameters:	

        SELF - An ANN-MLP construct

        INPUT - Input samples.

        OUTPUTS - Predicted responses for corresponding samples.


The method returns a dummy value which should be ignored.

If you are using the default +ANN-MLP-SIGMOID-SYM+ activation function with the default parameter 
values (EQ FPARAM1 0) and (EQ FPARAM2 0) then the function used is (EQ Y (* 1.7159 (TANH (* 2/3 X)))),
so the output will range from (-1.7159, 1.7159), instead of (0,1).


Example:

See ANN-MLP-EXAMPLE in this file.




ANN-MLP-TRAIN

Trains/updates MLP.

C++: int CvANN_MLP::train(const Mat& inputs, const Mat& outputs, const Mat& sampleWeights, const Mat& sampleIdx=Mat(), CvANN_MLP_TrainParams params=CvANN_MLP_TrainParams(), int flags=0 )

LISP-CV: (ANN-MLP-TRAIN (SELF ANN-MLP) (INPUTS MAT) (OUTPUTS MAT) (SAMPLE-WEIGHTS MAT) &OPTIONAL 
                        ((SAMPLE-IDX MAT) (MAT) GIVEN-SAMPLE-IDX) ((PARAMS ANN-MLP-TRAIN-PARAMS) (ANN-MLP-TRAIN-PARAMS) GIVEN-PARAMS) 
                        ((FLAGS :INT) 0)) => :INT

    Parameters:	

        INPUTS - Floating-point matrix of input vectors, one vector per row.

        OUTPUTS - Floating-point matrix of the corresponding output vectors, one vector per row.

        SAMPLE-WEIGHTS - (RPROP only) Optional floating-point vector of weights for each sample. 
                         Some samples may be more important than others for training. You may want 
                         to raise the weight of certain classes to find the right balance between 
                         hit-rate and false-alarm rate, and so on.

        SAMPLE-IDX - Optional integer vector indicating the samples (rows of inputs and outputs) that 
                     are taken into account.

        PARAMS - Training parameters. See the ANN-MLP-TRAIN-PARAMS description.

        FLAGS -

          Various parameters to control the training algorithm.
          A combination of the following parameters is possible:

            +UPDATE-WEIGHTS+ Algorithm updates the network weights, rather than computes them from scratch. 
                             In the latter case the weights are initialized using the Nguyen-Widrow algorithm.

            +NO-INPUT-SCALE+ Algorithm does not normalize the input vectors. If this flag is not set, 
                             the training algorithm normalizes each input feature independently, shifting 
                             its mean value to 0 and making the standard deviation equal to 1. If the network 
                             is assumed to be updated frequently, the new training data could be much different 
                             from original one. In this case, you should take care of proper normalization.

            +NO-OUTPUT-SCALE+ Algorithm does not normalize the output vectors. If the flag is not set, 
                              the training algorithm normalizes each output feature independently, by 
                              transforming it to the certain range depending on the used activation function.


This method applies the specified training algorithm to computing/adjusting the network weights. It 
returns the number of done iterations.

The RPROP training algorithm is parallelized with the TBB library.

If you are using the default ANN-MLP-SIGMOID-SYM+ activation function then the output should be in 
the range [-1,1], instead of [0,1], for optimal results.


Example:

See ANN-MLP-EXAMPLE in this file.



ANN-MLP-TRAIN-PARAMS

The constructors.

C++: CvANN_MLP_TrainParams::CvANN_MLP_TrainParams()

C++: CvANN_MLP_TrainParams::CvANN_MLP_TrainParams(CvTermCriteria term_crit, int train_method, double param1, double param2=0 )

LISP-CV: (ANN-MLP-TRAIN-PARAMS (TERM-CRIT TERM-CRITERIA) (TRAIN-METHOD :INT) (PARAM1 :DOUBLE) ((PARAM2 :DOUBLE) 0.0D0)) 
                                => ANN-MLP-TRAIN-PARAMS

    Parameters:	

        TERM-CRIT - Termination criteria of the training algorithm. You can specify the maximum number 
                    of iterations (MAX-COUNT) and/or how much the error could change between the iterations 
                    to make the algorithm continue (EPSILON).

        TRAIN-METHOD -

        Training method of the MLP. Possible values are:

            +ANN-MLP-TRAIN-PARAMS-BACKPROP+ - The back-propagation algorithm.

            +ANN-MLP-TRAIN-PARAMS-RPROP+ - The RPROP algorithm.

        PARAM1 - Parameter of the training method. It is rp_dw0 for RPROP and bp_dw_scale for BACKPROP.

        PARAM2 - Parameter of the training method. It is rp_dw_min for RPROP and bp_moment_scale for 
                 BACKPROP.


Note: In OpenCV, rp_dw0, bp_dw_scale, rp_dw_min and bp_moment_scale are names of CvANN_MLP_TrainParams
struct members. The CvANN_MLP_TrainParams class is represented in Lisp as a pointer to the class named
ANN-MLP-TRAIN-PARAMS so the specific struct members are not referenced e.g. you will need to remember: 


If you are using the back-propagation algorithm:

   PARAM1 supplies the bp_dw_scale member value.

   PARAM2 supplies the bp_moment_scale member value.


If you are using the RPROP algorithm:

   PARAM1 supplies the rp_dw0 member value.

   PARAM2 supplies the rp_dw_min member value.



By default the RPROP algorithm is used. The following code from:

<Open-Cv-Source-Directory>/modules/ml/src/ann_mlp.cpp 

is how this function is called(in C++) when no values are supplied.


CvANN_MLP_TrainParams::CvANN_MLP_TrainParams()
{
    term_crit = cvTermCriteria( CV_TERMCRIT_ITER + CV_TERMCRIT_EPS, 1000, 0.01 );
    train_method = RPROP;
    bp_dw_scale = bp_moment_scale = 0.1;
    rp_dw0 = 0.1; rp_dw_plus = 1.2; rp_dw_minus = 0.5;
    rp_dw_min = FLT_EPSILON; rp_dw_max = 50.;
}


Example:

See ANN-MLP-EXAMPLE in this file.



PHOTO - INPAINTING


IN-PAINT


Restores the selected region in an image using the region neighborhood.


C++: void inpaint(InputArray src, InputArray inpaintMask, OutputArray dst, double inpaintRadius, int flags)

LISP-CV:

    Parameters:	

        SRC - Input 8-bit 1-channel or 3-channel image.

        IN-PAINT-MASK - Inpainting mask, 8-bit 1-channel image. Non-zero pixels indicate the area 
                        that needs to be inpainted.

        DEST - Output image with the same size and type as SRC.

        IN-PAINT-RADIUS - Radius of a circular neighborhood of each point inpainted that is considered 
                          by the algorithm.

        FLAGS -

        Inpainting method that could be one of the following:

            +INPAINT-NS+ Navier-Stokes based method [Navier01]

            +INPAINT-TELEA+ Method by Alexandru Telea [Telea04].

The function reconstructs the selected image area from the pixel near the area boundary. The function 
may be used to remove dust and scratches from a scanned photo, or to remove undesirable objects from 
still images or video. See http://en.wikipedia.org/wiki/Inpainting for more details.


Example:

(defparameter pt 0)
(defparameter prev-pt 0)
(defparameter color 0)
(defparameter img 0)
(defparameter in-paint-mask 0)

(defun help ()

  (format t "~%Cool inpainting demonstration.")
  (format t "~%~%Inpainting repairs damage to images by floodfilling")
  (format t "~%the damage with surrounding image areas.")
  (format t "~%~%Hot keys:")
  (format t "~%~%ESC - quit the program")
  (format t "~%~%r - restore the original image")
  (format t "~%~%i or SPACE - run inpainting algorithm")
  (format t "~%~%(before running it, paint something on the image)~%~%"))


(defcallback on-mouse :void ((event :int) (x :int) (y :int) (flags :int))

  (if (or (= event +event-lbuttonup+) (zerop (logand flags +event-flag-lbutton+)))
      (setf prev-pt (gc:point -1 -1))
      (if (eq event +event-lbuttondown+)
	  (setf prev-pt (gc:point x y))
	  (if (and (eq event +event-mousemove+) (logand flags +event-flag-lbutton+))
	      (progn
		(setf pt (gc:point x y))
		(setf color (gc:scalar-all 255))
		(if (< (point-x prev-pt) 0)
		    (setf prev-pt pt))
		(line in-paint-mask prev-pt pt color 5 8 0)
		(line img prev-pt pt color 5 8 0)
		(setf prev-pt pt)
		(imshow "Image - IN-PAINT Example" img))))))


(defun in-paint-example (&optional filename)
  (if (not filename)
      (return-from in-paint-example 
	(progn (format t "~%~%Usage:")
	       (format t "~%~%in-paint-example <image-name> ")
	       (format t "~%~%Try <lisp-cv-dir>/images/damaged-img-by-glen-luchford.jpeg~%~%"))))
  (let ((window-name-1 "Image - IN-PAINT Example")
	(window-name-2 "Inpainted image - IN-PAINT Example"))
    ;; load image
    (with-mat ((img (imread filename -1))
	       (img0 (clone img))    
	       (in-paint-mask (mat-zeros (rows img) (cols img) +8u+)))
      (if (empty img) 
	  (return-from in-paint-example
	    (format t "Couldn't open the image.")))
      (help)
      (with-named-window (window-name-1 +window-normal+)
	(with-named-window (window-name-2 +window-normal+)
	  (move-window window-name-1 533 175)
	  (move-window window-name-2 984 175)
	  (imshow window-name-1 img)
	  (imshow window-name-2 img)
	  (set-mouse-callback window-name-1 (callback on-mouse))
	  (loop 
	     (let ((c (wait-key)))
	       (if (eq c 27)
		   (return))
	       (if (eq c 114)
		   (with-scalar ((zeros (scalar-all 0)))
		     (assgn-val in-paint-mask zeros)
		     (copy-to img0 img)
		     (imshow window-name-1 img)))
	       (if (or (eq c 105) (eq c 32))
		   (with-mat ((in-painted (mat)))
		     (in-paint img in-paint-mask in-painted 3d0 +inpaint-telea+)  
		     (imshow window-name-2 in-painted))))
	     (let ((c (wait-key 33)))
	       (when (= c 27)
		 (return)))))))))


PHOTO - DECOLORIZATION


DECOLOR

Transforms a color image to a grayscale image. It is a basic tool in digital printing, stylized 
black-and-white photograph rendering, and in many single channel image processing applications.

C++: void decolor(InputArray src, OutputArray grayscale, OutputArray color_boost)

LISP-CV: (DECOLOR (SRC MAT) (GRAYSCALE MAT) (COLOR-BOOST MAT)) => :VOID

    Parameters:	

        SRC - Input 8-bit 3-channel image.

        GRAYSCALE - Output 8-bit 1-channel image.

        COLOR-BOOST - Output 8-bit 3-channel image.


This function is to be applied on color images.


(defun decolor-example (filename)

  (let ((window-name-1 "SOURCE - DECOLOR Example")
	(window-name-2 "GRAYSCALE - DECOLOR Example")
	(window-name-3 "COLOR-BOOST - DECOLOR Example"))
    (with-named-window (window-name-1 +window-normal+)
      (with-named-window (window-name-2 +window-normal+)
	(with-named-window (window-name-3 +window-normal+)
	  (move-window window-name-1 310 175)
	  (move-window window-name-2 760 175)
	  (move-window window-name-3 1210 175)
	  (with-mat ((source (imread filename 1))
		     (grayscale (mat))
		     (color-boost (mat)))
	    (if (empty source)
		(return-from decolor-example))
	    (decolor source grayscale color-boost)
	    (imshow window-name-1 source)
	    (imshow window-name-2 grayscale)
	    (imshow window-name-3 color-boost)
	    (loop
	       (let ((c (wait-key 33)))
		 (when (= c 27)
		   (return))))))))))



PHOTO - SEAMLESS CLONING



COLOR-CHANGE

Given an original color image, two differently colored versions of this image can be mixed seamlessly.

C++: void colorChange(InputArray src, InputArray mask, OutputArray dst, float red_mul=1.0f, float green_mul=1.0f, float blue_mul=1.0f)

LISP-CV: (COLOR-CHANGE (SRC MAT) (MASK MAT) (DEST MAT) &OPTIONAL ((RED-MUL :FLOAT) 1.0F0) ((GREEN-MUL :FLOAT) 1.0F0) 
                      ((BLUE-MUL :FLOAT) 1.0F0)) => :VOID


    Parameters:	

        SRC - Input 8-bit 3-channel image.

        MASK - Input 8-bit 1 or 3-channel image.

        DEST - Output image with the same size and type as SRC.

        RED-MUL - R-channel multiply factor.

        GREEN-MUL - G-channel multiply factor.

        BLUE-MUL - B-channel multiply factor.


Multiplication factor is between 0.5f0 to 2.5f0.


(defparameter *cloning-dir*
  (cat *lisp-cv-src-dir* "images/cloning"))

(defun color-change-example ()

  (let ((window-name-1 "SOURCE - COLOR-CHANGE Example")
	(window-name-2 "MASK - COLOR-CHANGE Example")
	(window-name-3 "RESULT - COLOR-CHANGE Example"))
    (with-named-window (window-name-1 +window-normal+)
      (with-named-window (window-name-2 +window-normal+)
	(with-named-window (window-name-3 +window-normal+)
	  (move-window window-name-1 310 175)
	  (move-window window-name-2 760 175)
	  (move-window window-name-3 1210 175)
	  (with-mat ((source (imread (cat *cloning-dir* "/color-change/source.png") 1))
		     (mask (imread (cat *cloning-dir* "/color-change/mask.png") 1))
		     (result (clone source)))
	    (if (empty source)
		(return-from color-change-example 
		  (format t "Could not load source image")))
	    (if (empty mask)
		(return-from color-change-example 
		  (format t "Could not load mask image")))
	    (color-change source mask result 1.5f0 0.5f0 0.5f0)
	    (imwrite (cat *cloning-dir* "/mixed-cloning/cloned.png") result)
	    (imshow window-name-1 source)
	    (imshow window-name-2 mask)
	    (imshow window-name-3 result)
	    (loop
	       (let ((c (wait-key 33)))
		 (when (= c 27)
		   (return))))))))))



ILLUMINATION-CHANGE

Applying an appropriate non-linear transformation to the gradient field inside the selection and 
then integrating back with a Poisson solver, modifies locally the apparent illumination of an 
image.

C++: void illuminationChange(InputArray src, InputArray mask, OutputArray dst, float alpha=0.2f, float beta=0.4f)

LISP-CV: (ILLUMINATION-CHANGE (SRC MAT) (MASK MAT) (DEST MAT) &OPTIONAL ((ALPHA :FLOAT) 0.2F0) ((BETA :FLOAT) 0.4F0)) => :VOID

    Parameters:	

        SRC - Input 8-bit 3-channel image.

        MASK - Input 8-bit 1 or 3-channel image.

        DEST - Output image with the same size and type as SRC.

        ALPHA - Value ranges between 0-2.

        BETA - Value ranges between 0-2.


This is useful to highlight under-exposed foreground objects or to reduce specular reflections.



(defparameter *cloning-dir*
  (cat *lisp-cv-src-dir* "images/cloning"))

(defun illumination-change-example ()

  (let ((window-name-1 "SOURCE - ILLUMINATION-CHANGE Example")
	(window-name-2 "MASK - ILLUMINATION-CHANGE Example")
	(window-name-3 "RESULT - ILLUMINATION-CHANGE Example"))
    (with-named-window (window-name-1 +window-normal+)
      (with-named-window (window-name-2 +window-normal+)
	(with-named-window (window-name-3 +window-normal+)
	  (move-window window-name-1 310 175)
	  (move-window window-name-2 760 175)
	  (move-window window-name-3 1210 175)
	  (with-mat ((source (imread (cat *cloning-dir* "/illumination-change/source.png") 1))
		     (mask (imread (cat *cloning-dir* "/illumination-change/mask.png") 1))
		     (result (clone source)))
	    (if (empty source)
		(return-from illumination-change-example
		  (format t "Could not load source image")))
	    (if (empty mask)
		(return-from illumination-change-example 
		  (format t "Could not load mask image")))
	    (illumination-change source mask result 0.2f0 0.4f0)
	    (imwrite (cat *cloning-dir* "/illumination-change/cloned.png") result)
	    (imshow window-name-1 source)
	    (imshow window-name-2 mask)
	    (imshow window-name-3 result)
	    (loop
	       (let ((c (wait-key 33)))
		 (when (= c 27)
		   (return))))))))))



SEAMLESS-CLONE

Image editing tasks concern either global changes (color/intensity corrections, filters, deformations) 
or local changes concerned to a selection. Here we are interested in achieving local changes, ones that 
are restricted to a region manually selected (ROI), in a seamless and effortless manner. The extent of 
the changes ranges from slight distortions to complete replacement by novel content.

C++: void seamlessClone(InputArray src, InputArray dst, InputArray mask, Point p, OutputArray blend, int flags)

LISP-CV:  (SEAMLESS-CLONING (SRC MAT) (DEST MAT) (MASK MAT) (P POINT) (BLEND MAT) (FLAGS :INT))  => :VOID

    Parameters:	

        SRC - Input 8-bit 3-channel image.

        DST - Input 8-bit 3-channel image.

        MASK - Input 8-bit 1 or 3-channel image.

        P - Point in DST image where object is placed.

        RESULT - Output image with the same size and type as DST.

        FLAGS -

        Cloning method that could be one of the following:

            +NORMAL-CLONE+ The power of the method is fully expressed when inserting objects with 
                           complex outlines into a new background.

            +MIXED-CLONE+ The classic method, color-based selection and alpha

                masking might be time consuming and often leaves an undesirable halo. Seamless cloning, 
                even averaged with the original image, is not effective. Mixed seamless cloning based 
                on a loose selection proves effective.

            +FEATURE-EXCHANGE+ Feature exchange allows the user to replace easily certain
                features of one object by alternative features.

Example 1:

(defparameter *cloning-dir*
  (cat *lisp-cv-src-dir* "images/cloning"))

(defun seamless-cloning-example-1 ()

  (let ((window-name-1 "SOURCE - SEAMLESS-CLONING Example 1")
        (window-name-2 "DESTINATION - SEAMLESS-CLONING Example 1")
	(window-name-3 "MASK - SEAMLESS-CLONING Example 1")
	(window-name-4 "RESULT - SEAMLESS-CLONING Example 1"))
    (with-named-window (window-name-1 +window-normal+)
      (with-named-window (window-name-2 +window-normal+)
	(with-named-window (window-name-3 +window-normal+)
	  (with-named-window (window-name-4 +window-normal+)
	    (move-window window-name-1 485 98)
	    (move-window window-name-2 894 98)
	    (move-window window-name-3 485 444)
	    (move-window window-name-4 894 444)
	    (with-mat ((source (imread (cat *cloning-dir* "/normal-cloning/source.png") 1))
		       (destination (imread (cat *cloning-dir* "/normal-cloning/destination.png") 1))
		       (mask (imread (cat *cloning-dir* "/normal-cloning/mask.png") 1))
		       (result (clone source)))
	      (if (empty source)
		  (return-from seamless-cloning-example-1 
		    (format t "Could not load source image")))
	      (if (empty destination)
		  (return-from seamless-cloning-example-1 
		    (format t "Could not load destination image")))
	      (if (empty mask)
		  (return-from seamless-cloning-example-1 
		    (format t "Could not load mask image")))
	      (with-point ((p (point 400 100)))
		(seamless-clone source destination mask p result +normal-clone+)
		(imwrite (cat *cloning-dir* "/normal-cloning/cloned.png") result)
		(imshow window-name-1 source)
		(imshow window-name-2 destination)
		(imshow window-name-3 mask)
		(imshow window-name-4 result)
		(loop
		   (let ((c (wait-key 33)))
		     (when (= c 27)
		       (return))))))))))))



Example 2:


(defparameter *cloning-dir*
  (cat *lisp-cv-src-dir* "images/cloning"))

(defun seamless-cloning-example-2 ()

  (let ((window-name-1 "SOURCE - SEAMLESS-CLONING Example 2")
	(window-name-2 "DESTINATION - SEAMLESS-CLONING Example 2")
	(window-name-3 "MASK - SEAMLESS-CLONING Example 2")
	(window-name-4 "RESULT - SEAMLESS-CLONING Example 2"))
    (with-named-window (window-name-1 +window-normal+)
      (with-named-window (window-name-2 +window-normal+)
	(with-named-window (window-name-3 +window-normal+)
	  (with-named-window (window-name-4 +window-normal+)
	    (move-window window-name-1 485 98)
	    (move-window window-name-2 894 98)
	    (move-window window-name-3 485 444)
	    (move-window window-name-4 894 444)
	    (with-mat ((source (imread (cat *cloning-dir* "/monochrome-transfer/source.png") 1))
		       (destination (imread (cat *cloning-dir* "/monochrome-transfer/destination.png") 1))
		       (mask (imread (cat *cloning-dir* "/monochrome-transfer/mask.png") 1))
		       (result (clone source)))
	      (if (empty source)
		  (return-from seamless-cloning-example-2 
		    (format t "Could not load source image")))
	      (if (empty destination)
		  (return-from seamless-cloning-example-2 
		    (format t "Could not load destination image")))
	      (if (empty mask)
		  (return-from seamless-cloning-example-2 
		    (format t "Could not load mask image")))
	      (with-point ((p (point (- (round (/ (width (size destination)) 2)) 10)
				     (round (/ (height (size destination)) 2)))))
		(seamless-clone source destination mask p result +feature-exchange+)
		(imwrite (cat *cloning-dir* "/mixed-cloning/cloned.png") result)
		(imshow window-name-1 source)
		(imshow window-name-2 destination)
		(imshow window-name-3 mask)
		(imshow window-name-4 result)
		(loop
		   (let ((c (wait-key 33)))
		     (when (= c 27)
		       (return))))))))))))




TEXTURE-FLATTENING

By retaining only the gradients at edge locations, before integrating with the Poisson solver, one 
washes out the texture of the selected region, giving its contents a flat aspect. Here Canny Edge 
Detector is used.

C++: void textureFlattening(InputArray src, InputArray mask, OutputArray dst, double low_threshold=30 , double high_threshold=45, 
                            int kernel_size=3)

LISP-CV: (TEXTURE-FLATTENING (SRC MAT) (MASK MAT) (DEST MAT) &OPTIONAL ((LOW-THRESHOLD :DOUBLE) 30D0) 
                            ((HIGH-THRESHOLD :DOUBLE) 45D0) ((KERNAL-SIZE :INT) 3)) => :VOID

    Parameters:	

        SRC - Input 8-bit 3-channel image.

        MASK - Input 8-bit 1 or 3-channel image.

        DEST - Output image with the same size and type as SRC.

        LOW-THRESHOLD - Range from 0 to 100.

        HIGH-THRESHOLD - Value > 100.

        KERNEL-SIZE - The size of the Sobel kernel to be used.


NOTE:

The algorithm assumes that the color of the source image is close to that of the destination. This 
assumption means that when the colors don’t match, the source image color gets tinted toward the 
color of the destination image.


(defparameter *cloning-dir*
  (cat *lisp-cv-src-dir* "images/cloning"))

(defun texture-flattening-example ()

  (let ((window-name-1 "SOURCE - TEXTURE-FLATTENING Example")
	(window-name-2 "MASK - TEXTURE-FLATTENING Example")
	(window-name-3 "RESULT - TEXTURE-FLATTENING Example"))
    (with-named-window (window-name-1 +window-normal+)
      (with-named-window (window-name-2 +window-normal+)
	(with-named-window (window-name-3 +window-normal+)
	  (move-window window-name-1 310 175)
	  (move-window window-name-2 760 175)
	  (move-window window-name-3 1210 175)
	  (with-mat ((source (imread (cat *cloning-dir* "/texture-flattening/source.png") 1))
		     (mask (imread (cat *cloning-dir* "/texture-flattening/mask.png") 1))
		     (result (clone source)))
	    (if (empty source)
		(return-from texture-flattening-example
		  (format t "Could not load source image")))
	    (if (empty mask)
		(return-from texture-flattening-example 
		  (format t "Could not load mask image")))
	    (texture-flattening source mask result 30d0 45d0 3)
	    (imwrite (cat *cloning-dir* "/texture-flattening/cloned.png") result)
	    (imshow window-name-1 source)
	    (imshow window-name-2 mask)
	    (imshow window-name-3 result)
	    (loop
	       (let ((c (wait-key 33)))
		 (when (= c 27)
		   (return))))))))))



PHOTO - NON-PHOTOREALISTIC RENDERING



DETAIL-ENHANCE


This filter enhances the details of a particular image.


C++: void detailEnhance(InputArray src, OutputArray dst, float sigma_s=10, float sigma_r=0.15f)

LISP-CV: (DETAIL-ENHANCE (SRC MAT) (DEST MAT) &OPTIONAL ((SIGMA-S :FLOAT) 60F0) ((SIGMA-R :FLOAT) 0.45F0)) => :VOID


    Parameters:	

        SRC - Input 8-bit 3-channel image.

        DEST - Output image with the same size and type as SRC.

        SIGMA-S - Range between 0 to 200.

        SIGMA-R - Range between 0 to 1.


Example:

See STYLIZATION-EXAMPLE in this file.



EDGE-PRESERVING-FILTER


Filtering is the fundamental operation in image and video processing. Edge-preserving smoothing filters 
are used in many different applications.

C++: void edgePreservingFilter(InputArray src, OutputArray dst, int flags=1, float sigma_s=60, float sigma_r=0.4f)

LISP-CV: (EDGE-PRESERVING-FILTER (SRC MAT) (DEST MAT) &OPTIONAL ((FLAGS :INT) 1) ((SIGMA-S :FLOAT) 60F0) 
                                ((SIGMA-R :FLOAT) 0.4F0)) => :VOID

    Parameters:	

        SRC - Input 8-bit 3-channel image.

        DEST - Output 8-bit 3-channel image.

        FLAGS -

        Edge preserving filters:

              +RECURS-FILTER+

              +NORMCONV-FILTER+

        SIGMA-S - Range between 0 to 200.

        SIGMA-R - Range between 0 to 1.


Example:

See STYLIZATION-EXAMPLE in this file.



PENCIL-SKETCH


Pencil-like non-photorealistic line drawing

C++: void pencilSketch(InputArray src, OutputArray dst1, OutputArray dst2, float sigma_s=60, float sigma_r=0.07f, 
                       float shade_factor=0.02f)

LISP-CV: (PENCIL-SKETCH (SRC MAT) (DEST1 MAT) (DEST2 MAT) &OPTIONAL ((SIGMA-S :FLOAT) 60F0) ((SIGMA-R :FLOAT) 0.07F0) 
                       ((SHADE-FACTOR :FLOAT) 0.02F0)) => :VOID

    Parameters:	

        SRC - Input 8-bit 3-channel image.

        DEST1 - Output 8-bit 1-channel image.

        DEST2 - Output image with the same size and type as SRC.

        SIGMA-S - Range between 0 to 200.

        SIGMA-R - Range between 0 to 1.

        SHADE-FACTOR - Range between 0 to 0.1.


Example:

See STYLIZATION-EXAMPLE in this file.



STYLIZATION

Stylization aims to produce digital imagery with a wide variety of effects not focused on photorealism.
Edge-aware filters are ideal for stylization, as they can abstract regions of low contrast while preserving, 
or enhancing, high-contrast features.

C++: void stylization(InputArray src, OutputArray dst, float sigma_s=60, float sigma_r=0.45f)

LISP-CV: (STYLIZATION (SRC MAT) (DEST MAT) &OPTIONAL ((SIGMA-S :FLOAT) 60F0) ((SIGMA-R :FLOAT) 0.45F0)) => :VOID

    Parameters:	

        SRC - Input 8-bit 3-channel image.

        DEST - Output image with the same size and type as src.

        SIGMA-S - Range between 0 to 200.

        SIGMA-R - Range between 0 to 1.



(defun stylization-example (filename)

  "Note: If you don't get good results, try changing the 
   parameters or using different picture. Some pictures 
   don't work well with this example but most look great"

    (with-mat ((i (imread filename 1))
	       (img (mat))
	       (img1 (mat)))
      (if (empty i) 
	  (return-from stylization-example 
	    (format t "Image not found")))

      (format t "~%~% Edge Preserve Filter")
      (format t "~%----------------------")

      (format t "~%Options: ~%~%")

      (format t "~%~%1) Edge Preserve Smoothing")
      (format t "~%~%  a) Using Normalized convolution Filter")
      (format t "~%~%  b) Using Recursive Filter")
      (format t "~%~%2) Detail Enhancement")
      (format t "~%~%3) Pencil sketch/Color Pencil Drawing")
      (format t "~%~%4) Stylization~%~%")
      (format t "~%~%Press number 1-4 to choose from above techniques: ")
      (format t "~%~%Enter a number: ")
      (let ((num (read)))
	(if (eq num 1)
	    (progn (format t "~%~%Press 1 for Normalized Convolution Filter: ")
		   (format t "~%~%Press 2 for Recursive Filter: ")
                   (format t "~%~%Enter a number: ")
		   (let ((type (read)))
		     (edge-preserving-filter i img type))
		   (imshow "Edge Preserve Smoothing" img)
		   (move-window "Edge Preserve Smoothing" 759 175))
	    (if (eq num 2)
		(progn (detail-enhance i img)
		       (imshow "Detail Enhanced" img)
		       (move-window "Detail Enhanced" 759 175))
		(if (eq num 3)
		    (progn (pencil-sketch i img1 img 10f0 0.1f0 0.03f0)
			   (imshow "Pencil Sketch" img1)
			   (imshow "Color Pencil Sketch" img)
			   (move-window "Pencil Sketch" 533 175)
			   (move-window "Color Pencil Sketch" 984 175))
		    (if (eq num 4)
			(progn (stylization i img)
			       (imshow "Stylization" img)
			       (move-window "Stylization" 759 175))))))
	(loop
	   (let ((c (wait-key 33)))
	     (when (= c 27)
	       (destroy-all-windows)
	       (return)))))))



CONTRIB - COLORMAPS IN OPENCV



APPLY-COLOR-MAP

Applies a GNU Octave/MATLAB equivalent colormap on a given image.

C++: void applyColorMap(InputArray src, OutputArray dst, int colormap)

LISP-CV: (APPLY-COLOR-MAP (SRC MAT) (DEST MAT) (COLORMAP :INT)) => :VOID

    Parameters:	

        SRC - The source image, grayscale or colored does not matter.

        DEST - The result is the colormapped source image. Note: In the OpenCV code Mat::create() 
               is called on DEST.

        COLORMAP - The colormap to apply, see the list of available colormaps below.


Currently the following GNU Octave/MATLAB equivalent colormaps are implemented:


(defanonenum 

  (+colormap-autumn+ 0)
  (+colormap-bone+ 1)
  (+colormap-jet+ 2)
  (+colormap-winter+ 3)
  (+colormap-rainbow+ 4)
  (+colormap-ocean+ 5)
  (+colormap-summer+ 6)
  (+colormap-spring+ 7)
  (+colormap-cool+ 8)
  (+colormap-hsv+ 9)
  (+colormap-pink+ 10)
  (+colormap-hot+ 11))


Description:

The human perception isn’t built for observing fine changes in grayscale images. Human eyes are more 
sensitive to observing changes between colors, so you often need to recolor your grayscale images to 
get a clue about them. OpenCV now comes with various colormaps to enhance the visualization in your 
computer vision application.

In Lisp-cv you only need (APPLY-COLOR-MAP) to apply a colormap on a given image. The following example 
code applies all 11 color map types to an image based on the position of a trackbar.


(defun apply-color-map-example (filename)

  (let ((window-name "APPLY-COLOR-MAP Example")
        (i (alloc :int 0)))
    (with-named-window (window-name +window-normal+)
      (move-window window-name 759 175)
      ;Create trackbar
      (create-trackbar "Color Map" window-name i 11)
      ;Load image as grayscale - Color is okay too
      (with-mat ((img0 (imread filename 0)))
	(if (empty img0) 
	    (return-from apply-color-map-example
	      (format t "Image not loaded")))
        (with-mat ((cm-img0 (mat)))
	  (loop
	     ;In a loop apply one of 11 color map 
	     ;types based on trackbar position
	     (apply-color-map img0 cm-img0 (? i :int))
	     (imshow window-name cm-img0)
	     (let ((c (wait-key 33)))
	       (when (= c 27)
		 (return)))))))))



See OpenCv documentation for applyColorMap:

http://docs.opencv.org/trunk/modules/contrib/doc/facerec/colormaps.html?highlight=colormap#applycolormap

for the color scales for each of the available colormaps.




CAP-IS-OPEN

Returns true if video capturing has been initialized already.

C++: bool VideoCapture::isOpened()

LISP-CV: (CAP-IS-OPEN (SELF VIDEO-CAPTURE))

Parameters:	

         SELF - The VIDEO-CAPTURE structure.

If the previous call to VIDEO-CAPTURE constructor or CAP-IS-OPEN succeeded, the method returns true.


(defun cap-is-open-example (&optional 
                              (camera-index 
                               *camera-index*))

  "If the previous call to VIDEO-CAPTURE constructor (i/e, 
   (video-capture CAMERA-INDEX) in the below example) or the fu-
   nction CAP-IS-OPEN succeeded, the method returns true. 
   The boolean output of the PRINC function in the IF sta-
   tement in this example reflects a good or bad capture. 
   Output will likely be '1' or 'True' unless your camera 
   is unplugged....Try unplugging your camera to test it 
   out."

  (with-capture (cap (video-capture camera-index)) 
    (let ((window-name "CAP-IS-OPEN Example"))
      (if (not (princ (cap-is-open cap))) 
	  (return-from cap-is-open-example 
	    (format t "Cannot open the video camera")))
      (named-window window-name +window-normal+)
      (move-window window-name 759 175)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "~%~%Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(imshow window-name frame))
      (cap-release cap)
      (destroy-window window-name))))



SCALAR


SCALAR constructor.


C++: Scalar_<_Tp>::Scalar_(_Tp v0, _Tp v1, _Tp v2, _Tp v3)

LISP-CV:  (SCALAR ((VAL0 :DOUBLE) &OPTIONAL ((VAL1 :DOUBLE) 0) ((VAL2 :DOUBLE) 0) ((VAL3 :DOUBLE) 0)))

C++: Scalar_<_Tp> Scalar_<_Tp>::all(_Tp v0)

LISP-CV:  (SCALAR-ALL (VAL0123 :DOUBLE))


    Parameters:	

        VAl0 - First scalar element - Must exist.

        VAl1 - Second scalar element - Can be empty.
       
        VAl2 - Third scalar element - Can be empty.
    
        VAl3 - Fourth scalar element - Can be empty.

        VAL0123 - Value of all scalar elements.


The function SCALAR is a SCALAR constructor. It returns a pointer to an up to 4 element scalar. Only 
VAL0 is required, the rest are optional. The function SCALAR-ALL returns a pointer to 4 element scalar 
with all elements having the same value.


;; Still gets errors mem-reffing SCALAR-1 and SCALAR-2, willl be fixed soon

(defun scalar-example ()

(with-scalar ((scalar-1 (scalar 0 255 0))
	     (scalar-2 (scalar-all 255)))
		(format t "~%SCALAR-1 = (~a, ~a, ~a)~%~%" 
			(? scalar-1 :double 0)
			(? scalar-1 :double 1)
			(? scalar-1 :double 2))
		(format t "~%SCALAR-2 = (~a, ~a, ~a, ~a)~%~%" 
			(? scalar-2 :double 0)
			(? scalar-2 :double 1)
			(? scalar-2 :double 2)
			(? scalar-2 :double 3))))



MAT-SIZE

Returns pointer to a matrix size.

C++: Size Mat::size() const

LISP-CV: (MAT-SIZE (SELF MAT)) => SIZE

The function MAT-SIZE returns SIZE, a matrix size pointer in which the columns are listed first an-
d the rows second. When the matrix is more than 2-dimensional, the returned size is (-1 -1).


Note: This example uses TG finalizers for memory management.

(defun mat-size-example ()
       
  "In the code below the (COLS, ROWS) values of MAT are
   accessed and stored in a SIZE object. Their values a-
   re accessed with the WIDTH and HEIGHT functions."
       
       (let* ((mat (gc:mat 5 5 +8u+ (gc:scalar 100 100 100)))
	      (mat-size (gc:size mat)))
	 (format t "~%MAT (COLS,ROWS) = (~a ~a)~%~%" 
		 ;;The '?' is a macro for CFFI:MEM-AREF
		 (width mat-size)
		 (height mat-size))))


MAT-TYPE

Returns the type of a matrix element.

C++: int Mat::type() const

LISP-CV: (MAT-TYPE (SELF MAT))

    Parameters:	

        SELF - A matrix(MAT)

The method returns a matrix element type. This is an identifier compatible with OpenCV's CvMat type
system, like CV_16SC3(+16SC3+ in LISP-CV) or 16-bit signed 3-channel array, and so on.


Note: This example uses TG finalizers for memory management


(defun mat-type-example ()

  "This function uses MAT-TYPE to find 
   the type of MAT-ONE and MAT-TWO."

  (let* ((mat-one (gc:mat-zeros 1 2 +32f+))
	 (mat-two (gc:mat-zeros 2 4 +64f+)))
    (format t "~%MAT-ONE type is ~a(+32f+). It is a Single Precision Floating Point Matrix.~%" 
	    (mat-type mat-one))
    (format t "~%MAT-TWO type is ~a(+64f+). It is a Double Precision Floating Point Matrix.~%~%" 
	    (mat-type mat-two))))



CIRCLE

Draws a circle.

C++: void circle(Mat& img, Point center, int radius, const Scalar& color, int thickness=1, int lineType=8, int shift=0)

LISP-CV: (CIRCLE (IMG MAT) (CENTER POINT) (RADIUS :INT) (COLOR SCALAR) &OPTIONAL ((THICKNESS :INT) 1) ((LINE-TYPE :INT) 8) ((SHIFT :INT) 0))

    Parameters:	

        IMG - Image where the circle is drawn.

        CENTER - Center of the circle.

        RADIUS - Radius of the circle.

        COLOR - Circle color.

        THICKNESS - Thickness of the circle outline, if positive. Negative thickness means that a F-
                    illed circle is to be drawn.

        LINE-TYPE - Type of the circle boundary. See the (LINE) description.

        SHIFT - Number of fractional bits in the coordinates of the center and in the radius value.

The function circle draws a simple or filled circle with a given center and radius.

CIRCLE-EXAMPLE:

(defparameter x 40)
(defparameter y 40)
(defparameter point 0)
(defparameter the-right-wall 600)
(defparameter the-left-wall 40)
(defparameter the-ceiling 40)
(defparameter the-floor 440)
(defparameter rate 10)
(defparameter right-wall-switch 0)
(defparameter left-wall-switch 0)
(defparameter ceiling-switch 0)
(defparameter floor-switch 0)

(defun report ()
  (format t "x = ~a~%" x)
  (format t "y = ~a~%" y)
  (format t "right-wall-switch = ~a~%" right-wall-switch)
  (format t "left-wall-switch = ~a~%" left-wall-switch)
  (format t "ceiling-switch = ~a~%" ceiling-switch)
  (format t "floor-switch = ~a~%" floor-switch))



(defparameter x 40)
(defparameter y 40)
(defparameter point 0)
(defparameter the-right-wall 600)
(defparameter the-left-wall 40)
(defparameter the-ceiling 40)
(defparameter the-floor 440)
(defparameter rate 10)
(defparameter right-wall-switch 0)
(defparameter left-wall-switch 0)
(defparameter ceiling-switch 0)
(defparameter floor-switch 0)

(defun report ()
  (format t "x = ~a~%" x)
  (format t "y = ~a~%" y)
  (format t "right-wall-switch = ~a~%" right-wall-switch)
  (format t "left-wall-switch = ~a~%" left-wall-switch)
  (format t "ceiling-switch = ~a~%" ceiling-switch)
  (format t "floor-switch = ~a~%" floor-switch))


(defun circle-example (&optional (camera-index *camera-index*))

  "This example uses the function CIRCLE to create a little 
   red ball. Then it uses a bit of logic to make the ball b-
   ounce around the room."

  (with-capture (cap (video-capture camera-index))
    (let* ((window-name "CICRLE Example")
	   (color (scalar 0 0 255)))
      (if (not (cap-is-open cap)) 
	  (return-from circle-example 
	    (format t "Cannot open the video camera")))
      (format t "~%Frame Size : ~ax~a~%~%" 
	      (cap-get cap +cap-prop-frame-width+)
	      (cap-get cap +cap-prop-frame-height+))
      (named-window window-name +window-normal+)
      (move-window window-name 759 175)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(setf point (point x y))
        (circle frame point 40 color +filled+ +aa+ 0)
	(imshow window-name frame)
	(if (= x the-right-wall) (progn 
				   (format t "right wall has been touched~%") 
				   (setf right-wall-switch 1)))    
	(if (= x the-left-wall) (progn
				  (format t "left wall has been touched~%") 
				  (setf left-wall-switch 1)))	
	(if (= y the-floor) (progn 
			      (format t "floor has been touched~%") 
			      (setf floor-switch 1))) 
	(if (= y the-ceiling) (progn 
				(format t"ceiling has been touched~%") 
				(setf ceiling-switch 1))) 
	(if (and (< x the-right-wall) (= right-wall-switch 0)) (incf x rate) (decf x rate))
	(if (and (< y the-floor) (= floor-switch 0)) (incf y rate) (decf y rate))
	(if (< x (+ 40 rate)) (setf right-wall-switch 0))
	(if (< y (+ 40 rate)) (setf floor-switch 0))
	(report))
      (destroy-window window-name))))


MAT-ZEROS

Returns a zero array of the specified size and type.
todo figure out if I do all of these
C++: static MatExpr Mat::zeros(int rows, int cols, int type)

LISP-CV: (MAT-ZEROS (ROWS :INT) (COLS :INT) (TYPE :INT))

C++: static MatExpr Mat::zeros(Size size, int type)

C++: static MatExpr Mat::zeros(int ndims, const int* sz, int type)


    Parameters:	

        NDIMS - Array dimensionality.

        ROWS - Number of rows.

        COLS - Number of columns.

        SIZE - Alternative to the matrix size specification (SIZE COLS ROWS).

        SZ - Array of integers specifying the array shape.

        TYPE - Created matrix type.

The method returns a Matlab-style zero array initializer. It can be used to quickly form a constant
array as a function parameter, part of a matrix expression, or as a matrix initializer.

(DEFPARAMETER A (MAT))
(SETF A (MAT-ZEROS 3 3 +32F+))

In the example above, a new matrix is allocated only if A is not a 3x3 floating-point matrix. Other-
wise, the existing matrix A is filled with zeros.


(defun mat-zeros-example ()

  "In this example the function MAT-ZEROS 
   is used to create a matrix MAT filled 
   with zeros. MAT is then shown in a win-
   dow for verification."

  (let* ((mat (mat-zeros 3 3 +8u+))
	 (window-name "MAT - MAT-ZEROS Example"))
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    (imshow window-name mat)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))


Mat::ones
todo figure out if I do all of these
Returns an array of all 1’s of the specified size and type.

C++: static MatExpr Mat::ones(int rows, int cols, int type)

LISP-CV: (MAT-ONES (ROWS :INT) (COLS :INT) (TYPE :INT))

C++: static MatExpr Mat::ones(Size size, int type)

C++: static MatExpr Mat::ones(int ndims, const int* sz, int type)


    Parameters:	

        NDIMS - Array dimensionality.

        ROWS - Number of rows.

        COLS - Number of columns.

        SIZE - Alternative to the matrix size specification Size(cols, rows).

        SZ - Array of integers specifying the array shape.

        TYPE - Created matrix type.


The method returns a Matlab-style 1’s array initializer, similarly to MAT-ZEROS. Note that using th-
is method you can initialize an array with an arbitrary value, using the following Matlab idiom:

Mat A = Mat::ones(100, 100, CV_8U)*3; // make 100x100 matrix filled with 3. todo

The above operation does not form a 100x100 matrix of 1’s and then multiply it by 3. Instead, it ju-
st remembers the scale factor (3 in this case) and use it when actually invoking the matrix initial-
izer.


(defun mat-ones-example ()

  "In this example the function MAT-ONES 
   is used to create a matrix MAT filled 
   with ones. MAT is then shown in a win-
   dow."

  (let* ((mat (mat-ones 4 4 +8u+))
	 (window-name "MAT - MAT-ONES Example"))
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    (imshow window-name mat)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))


MAT-EYE

Returns an identity matrix of the specified size and type.

C++: static MatExpr Mat::eye(int rows, int cols, int type)

LISP-CV: (MAT-EYE (ROWS :INT) (COLS :INT) (TYPE :INT)) => MAT


    Parameters:	

        ROWS - Number of rows.

        COLS - Number of columns.

        TYPE - Created matrix type.


The method returns a Matlab-style identity matrix initializer, similarly to (MAT-ZEROS). Similarly 
to (MAT-ONES), you can use a scale operation to create a scaled identity matrix efficiently:

;; Make a 4x4 diagonal matrix with 0.1's on the diagonal.

(DEFPARAMETER A (SCALE (<< (MAT-EYE 4 4 +32F+)) 0.1D0))


(defun mat-eye-example ()

  "Here we use the function MAT-EYE to create a 3x3 
   identity matrix IDENTITY-MAT-1, which is shown i-
   n the left-most window. 

   Next an identity matrix, IDENTITY-MAT-2 is creat-
   ed and using the function SCALE is scaled by 0.1. 
   Then both the pre-scaled version and the scaled 
   version are shown in the 2 right-most windows. T-
   his allows you to see the fact that, because the-
   y are single-float matrices, their elements are 
   shown as colors, not numbers, unlike the matrix 
   in the left-most window. So an identity matrix w-
   hich has a diagonal that is all ones, it's diago-
   nal would be represented as pure white. An ident-
   ity matrix whose diagonal is all 0.1, it's diago-
   nal would be represented as a dark, dark grey Ve-
   ry close to black, a colored boolean.

   Note: The PROMOTE(<<) function was needed in the 
   SCALE function to coerce IDENTITY-MAT-2 to MAT-E-
   XPR from MAT type for the scaling operation. The 
   FORCE(>>) function is then used in IMSHOW to coe-
   rce SCALED-IDENTITY-MAT-3 back to MAT."

  (let* ((identity-mat-1 (mat-eye 3 3 +8u+))
         (identity-mat-2 (mat-eye 4 4 +32f+))
	 (scaled-identity-mat (scale (<< identity-mat-2) 0.1d0))
	 (window-name-1 "IDENTITY-MAT-1 - MAT-EYE Example")
	 (window-name-2 "IDENTITY-MAT-3 - MAT-EYE Example")
         (window-name-3 "SCALED-IDENTITY-MAT-3 - MAT-EYE Example"))
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)
    (named-window window-name-3 +window-normal+)
    (move-window window-name-1 310 175)
    (move-window window-name-2 760 175)
    (move-window window-name-3 1210 175)
    (imshow window-name-1 identity-mat-1)
    (imshow window-name-2 identity-mat-2)
    (imshow window-name-3  (>> scaled-identity-mat))
    (loop while (not (= (wait-key 0) 27)))
    (destroy-all-windows)))




MAT-EXPR-T

Transposes a matrix.

C++: MatExpr Mat::t() const

LISP-CV: (MAT-EXPR-T (SELF MAT)) => MAT-EXPR

The method performs matrix transposition by means of matrix expressions. It does not perform the ac-
tual transposition but returns a temporary matrix transposition object that can be further used as 
a part of more complex matrix expressions or can be assigned to a matrix.

  Parameters:	

        SELF - Input matrix


; todo - finish example


FORCE

Coverts a MAT-EXPR to MAT

LISP-CV: (FORCE (SELF MAT-EXPR)) => MAT

LISP-CV: (>> (SELF MAT-EXPR)) => MAT

The function FORCE converts a functions output from MAT-EXPR to MAT.  This is useful if you have just 
done mathematical computation with a Matrix Expressions(MAT-EXPR) function and would like to use the 
result in a function that only accepts a MAT as input i.e. IMSHOW. The function >> is an identical 
shorthand version of the FORCE function supplied for ease of use. 

  Parameters:	

        SELF - A Matrix Expressions pointer.


(defun force-example ()

  "In this example a matrix filled with ones(MAT) is 
   created. MAT is then added to itself and the resu-
   lt is shown in a window. The function >> which is 
   a shorthand version of the FORCE function, is nec-
   essary here is because the return of the matrix a-
   ddition function ADD is MAT-EXPR and that return 
   must be coerced to MAT before it can be shown in 
   a window with IMSHOW."

  (let* ((mat (mat-ones 3 3 +8u+))
         (out (add mat mat))
	 (window-name "FORCE Example"))
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    (imshow window-name  (>> out))
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))



         
DIAG

Extracts a diagonal from a matrix, or creates a diagonal matrix.

C++: Mat Mat::diag(int d=0 ) const

LISP-CV: (MAT-DIAG (SELF MAT) (D :INT)) => MAT

C++: static Mat Mat::diag(const Mat& d)


    Parameters:	

        D -

        Single-column matrix that forms a diagonal matrix or index of the diagonal, with the follow-
        ing values:

            (= D 0) is the main diagonal.

            (> D 0) is a diagonal from the lower half. For example, (= D 1) means the diagonal is s-
                    et immediately below the main one.

            (< D 0) is a diagonal from the upper half. For example, (= D -1) means the diagonal is 
                    set immediately above the main one.


The method makes a new header for the specified matrix diagonal. The new matrix is represented as a
single-column matrix. Similarly to (ROW) and (COL) , this is an O(1) operation.


(defun diag-example ()

  "In this exmple, the main diagonal of the 
   matrix MAT is retrieved with the functio-
   n DIAG and then it it is printed. The fu-
   nction DIAG makes a new header for the s-
   pecified matrix diagonal. The new matrix 
   is represented as a single-column matrix."

  (let* ((data (alloc :int '(1 2 3 4 5 6 7 8 9)))
	 (mat (mat 3 3 +32s+ data))
	 (diag (diag mat 0)))
    (dotimes (i 3)
      (format t "~a" (at-int diag i 0))
      (princ #\Newline))
    (free data)))



SUB

Subtracts matrix M1 from matrix M2

C++: MatExpr - operator

LISP-CV: (SUB (M1 MAT) (M2 MAT)) => MAT-EXPR


    Parameters:	

        M1 - A matrix.

        M2 - A matrix.


The function SUB subtracts the elements of matrix M2 from the elements of matrix M1 in order. Both 
matrices must be the same size.  You may need to coerce the result of SUB, the return value, back 
to type MAT with the function (FORCE), (or the shorthand version (>>)) to use in other functions. 


(defun sub-example ()

  "Matrix M2 is subtracted from matrix M1 with the 
   function SUB. Matrix M1, matrix M2 and the resu-
   lt(RESULT) are then printed."

  (let* ((m1-data (alloc :uint '(53 62 85 64 23 97 52 16 12)))
	 (m2-data (alloc :uint '(64 22 64 15 11 17 42 16 88)))
	 (m1 (mat 3 3 +32s+ m1-data))
         (m2 (mat 3 3 +32s+ m2-data))
         (result (sub m1 m2)))
    (dotimes (i (rows m1))
      (dotimes (j (cols m1))
	(format t "~a" (at m1 i j :int))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    (dotimes (i (rows m2))
      (dotimes (j (cols m2))
	(format t "~a" (at m2 i j :int))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    (dotimes (i 3)
      (dotimes (j 3)
	       (format t "~a" (at (>> result) i j :int))
	       (princ #\Space))
      (princ #\Newline))
    (free m1-data)
    (free m2-data)))


DIV

Divides matrix M1 by matrix M2.

C++: MatExpr / operator

LISP-CV: (DIV (M1 MAT) (M2 MAT)) => MAT-EXPR


    Parameters:	

        M1 - A matrix.

        M2 - A matrix.


The function DIV divides the elements of matrix M1 by the elements of matrix M2 in order. Both matrices 
must be the same size. You may need to coerce the result of DIV, the return value, back to type MAT with 
the function (FORCE), (or the shorthand version (>>)) to use in other functions. 


(defun div-example ()

  "Matrix M1 is divided by M2 with the function DIV.
   Matrix M1, M2 and the result(RESULT) are then pri-
   nted."

  (let* ((m1-data (alloc :float '(53.0f0 62.0f0 85.0f0 64.0f0 23.0f0 
				  97.0f0 52.0f0 16.0f0 12.0f0)))
	 (m2-data (alloc :float '(64.0f0 22.0f0 64.0f0 15.0f0 11.0f0 
				  17.0f0 42.0f0 16.0f0 88.0f0)))
	 (m1 (mat 3 3 +32f+ m1-data))
         (m2 (mat 3 3 +32f+ m2-data))
         (result (div m1 m2)))
    (dotimes (i (rows m1))
      (dotimes (j (cols m1))
	(format t "~a" (at m1 i j :float))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    (dotimes (i (rows m2))
      (dotimes (j (cols m2))
	(format t "~a" (at m2 i j :float))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    (dotimes (i 3)
      (dotimes (j 3)
	(format t "~a" (at (>> result) i j :float))
	(princ #\Space))
      (princ #\Newline))
    (free m1-data)
    (free m2-data)))



VIDEO-WRITER

VIDEO-WRITER constructors

C++: VideoWriter::VideoWriter()

LISP-CV: (VIDEO-WRITER) => VIDEO-WRITER

C++: VideoWriter::VideoWriter(const string& filename, int fourcc, double fps, Size frameSize, bool isColor=true)

LISP-CV: (VIDEO-WRITER (FILENAME *STRING) (FOURCC :INT) (FPS :DOUBLE) (FRAME-SIZE SIZE) ((IS-COLOR :INT) T)) => VIDEO-WRITER


    Parameters:	

        FILENAME - Name of the output video file.

todo        FOURCC - 4-character code of codec used to compress the frames. For example, CV_FOURCC('P','I','M,'1') is a MPEG-1 codec, CV_FOURCC('M','J','P','G') is a motion-jpeg codec etc. List of codes can be obtained at Video Codecs by FOURCC page.

        FPS - Framerate of the created video stream.

        FRAME-SIZE - Size of the video frames.

        IS-COLOR - If it is not zero, the encoder will expect and encode color frames, otherwise it 
                   will work with grayscale frames (the flag is currently supported on Windows only)


The constructors/functions initialize video writers. On Linux FFMPEG is used to write videos; on Wi-
ndows FFMPEG or VFW is used; on MacOSX QTKit is used.


(defun video-writer-example (filename &optional	
					(cam 0) 
					(width *default-width*)
					(height *default-height*))
  (with-captured-camera (cap cam :width width :height height)
    (let* ((filename filename)
	   (window-name "VIDEO-WRITER Example")
	   (dheight (rational (cap-get cap +cap-prop-frame-height+)))
	   (dwidth (rational (cap-get cap +cap-prop-frame-width+))))
      ;; Initialize the VIDEO-WRITER object 
      (with-video-writer ((o-video-writer (video-writer filename 1196444237 ; todo
						       20.0d0 (size width height) 1))) 
	(if (not (cap-is-open cap))
	    (return-from video-writer-example 
	      (format t "ERROR: Cannot open the video file")))
	(if (not (video-writer-is-open o-video-writer)) 
	    (return-from video-writer-example 
	      (format t "ERROR: Failed to write the video"))) 
	(format t "~%Frame Size : ~ax~a~%~%" dwidth dheight)     
	(with-named-window (window-name +window-normal+)
	  (move-window window-name 759 175)
	  (loop
	     (with-mat ((frame (mat)))	
	       (cap-read cap frame)
	       (if (not frame) 
		   (return-from video-writer-example 
		     (format t "ERROR: Cannot read video file")))
	       ;; Write a frame into the file
	       (video-writer-write o-video-writer frame)
	       (imshow window-name frame)
	       (let ((c (wait-key 33)))
		 (when (= c 27)
		   (return))))))))))



VIDEO-WRITER-IS-OPEN


Returns true if video writer has been successfully initialized.


C++: bool VideoWriter::isOpened()

LISP-CV: (VIDEO-WRITER-IS-OPEN (SELF VIDEO-WRITER)) :BOOLEAN


(defun video-writer-is-open-example (filename &optional (camera-index *camera-index*))

  (with-capture (cap (video-capture camera-index)) ; Open the video camera no. 0
    (let* (; Initialize the VideoWriter object 
	   (o-video-writer (video-writer filename 1196444237 ; todo
					 20.0d0 (size 640 480) 1)))
      (format t "If VIDEO-WRITER is open a T will be displayed, else NIL: ~a"
	      (video-writer-is-open o-video-writer)))))



VIDEO-WRITER-WRITE


Writes the next video frame


C++: VideoWriter& VideoWriter::operator<<(const Mat& image)

LISP-CV: (VIDEO-WRITER-WRITE (SELF VIDEO-WRITER) (IMAGE MAT)) => VIDEO-WRITER

    Parameters:	

        SELF - Pointer to VIDEO-WRITER

        IMAGE - The written frame


The function VIDEO-WRITER-WRITE writes the specified image to video file. It must have the same siz-
e as has been specified when opening the video writer.


(defun video-writer-write-example (filename &optional 
					      (camera-index *camera-index*))

  (with-capture (cap (video-capture camera-index)) 
    (let* ((window-name "VIDEO-WRITER-WRITE Example")
	   (o-video-writer (video-writer filename 1196444237 
					 20.0d0 (size 640 480) 1)))
      (if (not (cap-is-open cap)) 
	  (return-from video-writer-write-example 
	    (format t "ERROR: Cannot open the video file")))
      (if (not (princ (video-writer-is-open o-video-writer))) 
	  (return-from video-writer-write-example 
	    (format t "ERROR: Failed to write the video"))) 
      (named-window window-name +window-normal+)
      (move-window window-name 759 175)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame) 
	(video-writer-write o-video-writer frame) 
	(imshow window-name frame))
      (destroy-window window-name))))


CLONE

Creates a full copy of the array and the underlying data.

C++: Mat Mat::clone() const

LISP-CV: (MAT-CLONE (SELF MAT)) => MAT


    Parameters:	

        SELF - Pointer to a matrix


The method creates a full copy of the array. The original TODO step[] is not taken into account. So
, the array copy is a continuous array occupying (* (TOTAL) (ELEM-SIZE)) bytes.


(defun clone-example ()

        ; Create data
  (let* ((m1-data (alloc :float '(53.0f0 62.0f0 85.0f0 64.0f0 23.0f0 
				   97.0f0 52.0f0 16.0f0 12.0f0)))
         ; Create matrix M1 and fill it with data
	 (m1 (mat 3 3 +32f+ m1-data))
         ; Create a clone of matrix M1 called M2
         (m2 (clone m1)))
    ; Print the elements of natrix M2 in a loop
    (dotimes (i (rows m2))
      (dotimes (j (cols m2))
        ; AT retrieves elements of M2, FORMAT 
        ; prints the elements to the screen 
	(format t "~a" (at m2 i j :float))
	(princ #\Space))
      (princ #\Newline))
      (free m1-data)))


TOTAL

Returns the total number of array elements.

C++: size_t Mat::total() const

LISP-CV: (TOTAL (SELF MAT)) => :UNSIGNED-INT


    Parameters:	

        SELF - Pointer to a matrix


The method returns the number of array elements (a number of pixels if the array represents an imag-
e).


(defun total-example ()

   "In this function, TOTAL returns the total 
   number of array elements in MAT1 and MAT2"

   (let* ((data (alloc :int '(1 2 3 4)))
	  (mat1 (mat 2 2 +32s+ data))
	  (mat2 (mat 100 100 +32s+))
	  (total1 (total mat1))
	  (total2 (total mat2)))
     (format t "Total mumber of elements in MAT1 = ~a~%~%" total1)
     (format t "Total mumber of elements in MAT2 = ~a" total2)
     (free data)))


ROI

Returns matrix header corresponding to the rectangular sub-array of an input matrix.

C++: Mat::Mat(const Mat& m, const Rect& roi)

LISP-CV: (ROI (SELF MAT) (ROI RECT)) => MAT


    Parameters:	

        SELF - Pointer to a matrix

        ROI -  Zero-based coordinates of the rectangle of interest


The function returns a header, corresponding to a specified rectangle of the input array. In other 
words, it allows the user to treat a rectangular part of input array as a stand-alone array. ROI is
taken into account by the function so the sub-array of ROI is actually extracted. The function ROI 
returns a pointer to the resultant sub-array header.


(defparameter x 100)
(defparameter y 100)
(defparameter region-of-interest 0)
(defparameter the-right-wall 540)
(defparameter the-left-wall 100)
(defparameter the-ceiling 100)
(defparameter the-floor 380)
(defparameter rate 10)
(defparameter right-wall-switch 0)
(defparameter left-wall-switch 0)
(defparameter ceiling-switch 0)
(defparameter floor-switch 0)

(defun report ()
  (format t "x = ~a~%" x)
  (format t "y = ~a~%" y)
  (format t "right-wall-switch = ~a~%" right-wall-switch)
  (format t "left-wall-switch = ~a~%" left-wall-switch)
  (format t "ceiling-switch = ~a~%" ceiling-switch)
  (format t "floor-switch = ~a~%" floor-switch))


(defun roi-example (&optional (camera-index *camera-index*))

  "A slight variation on my circle-example. here I use a bit of 
   logic to make the camera region of interest bounce around th-
   room. Have a look through the code, I named the variables in 
   a way to make it easy to understand."

  (with-capture (cap (video-capture camera-index))
    (let* ((window-name "ROI Example"))
      (if (not (cap-is-open cap)) 
	  (return-from roi-example 
	    (format t "Cannot open the video camera")))
      (format t "~%Frame Size : ~ax~a~%~%" 
	      (cap-get cap +cap-prop-frame-width+)
	      (cap-get cap +cap-prop-frame-height+))
      (named-window window-name +window-normal+)
      (move-window window-name 759 175)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(setf region-of-interest (rect x y 40 40))
        (setf frame (roi frame region-of-interest))
	(imshow window-name frame)
	(if (= x the-right-wall) (progn 
				   (format t "right wall has been touc-hed~%") 
				   (setf right-wall-switch 1)))    
	(if (= x the-left-wall) (progn
				  (format t "left wall has been touched~%") 
				  (setf left-wall-switch 1)))	
	(if (= y the-floor) (progn 
			      (format t "floor has been touched~%") 
			      (setf floor-switch 1))) 
	(if (= y the-ceiling) (progn 
				(format t "ceiling has been touched~%") 
				(setf ceiling-switch 1))) 
	(if (and (< x the-right-wall) (= right-wall-switch 0)) (incf x rate) (decf x rate))
	(if (and (< y the-floor) (= floor-switch 0)) (incf y rate) (decf y rate))
	(if (< x (+ 100 rate)) (setf right-wall-switch 0))
	(if (< y (+ 100 rate)) (setf floor-switch 0))
	(report))
      (destroy-window window-name))))


RECT

RECT constructor.

C++: Rect_(_Tp _x, _Tp _y, _Tp _width, _Tp _height);

LISP-CV:  (RECT (X :INT) (Y :INT) (:WIDTH :INT) (HEIGHT :INT)) => RECT

C++: _Tp x, y, width, height;

LISP-CV: (RECT-X (SELF RECT)) => :INT

LISP-CV: (RECT-Y (SELF RECT)) => :INT

LISP-CV: (RECT-WIDTH (SELF RECT)) => :INT

LISP-CV: (RECT-HEIGHT (SELF RECT)) => :INT

C++: Size_<_Tp> size() const;

LISP-CV: (RECT-SIZE (SELF RECT)) => SIZE

C++: Point_<_Tp> tl() const;

LISP-CV: (RECT-TL (SELF RECT)) => POINT

C++: Point_<_Tp> br() const;

LISP-CV: (RECT-BR (SELF RECT)) => POINT

C++ See cv_Rect_clone in LISP-CV-MASTER/SRC/RECT.CPP

LISP-CV: (RECT-CLONE (SELF RECT)) => RECT


    Parameters:	

        SELF - A rectangle.

        X - X-coordinate of the rectangle.

        Y -	Y-coordinate of the rectangle.

        WIDTH - Width of the rectangle.

        HEIGHT - Height of the rectangle.


The function RECT stores coordinates of a rectangle.

The function RECT-X retrieves the x coordinate of the rectangle.

The function RECT-Y retrieves the y coordinate of the rectangle.

The function RECT-WIDTH retrieves the WIDTH of the rectangle.

The function RECT-HEIGHT retrieves the HEIGHT of the rectangle.

The function RECT-SIZE retrieves the size (width, height) of the rectangle.

The function RECT-TL retrieves the top-left corner of the rectangle.

The function RECT-BR retrieves the bottom-right corner of the rectangle.



(defun rect-example (x y width height)

  ;Create a rectangle and find its size(width, height), 
  ;location and size(x, y, width, height) and its top-
  ;left and bottom-right corner

  ;WITH-RECT calls DEL-RECT automatically when RECTAN-
  ;GLE goes out of scope. DEL-RECT frees the memory a-
  ;llocated by RECT
  
  (format t "~%~%RECTANGLE:~%~%")
  (with-rect (rectangle (rect x y width height))
    (let* ((x (rect-x rectangle))
	   (y (rect-y rectangle))
	   (width (rect-width rectangle))
	   (height (rect-height rectangle))
           (size (rect-size rectangle))
	   (tl-corner (rect-tl rectangle))
	   (br-corner (rect-br rectangle)))
      (format t "~%The (x, y, width, height) of RECTANGLE = (~a, ~a, ~a, ~a)~%" 
	      x y width height)
      (format t "~%The size(width, height) of RECTANGLE = (~a, ~a)~%" 
	      (width size)
	      (height size))
      (format t "~%The top-left corner of RECTANGLE = (~a, ~a)~%" 
	      (point-x tl-corner)
	      (point-y tl-corner))
      (format t "~%The bottom-right corner of RECTANGLE = (~a, ~a)~%" 
	      (point-x br-corner)
	      (point-y br-corner)))

   ;Create a clone of RECTANGLE and find its size(width, 
   ;height), location and size(x, y, width, height) and 
   ;its top-left and bottom-right corner

    (format t "~%~%RECTANGLE-CLONE:~%")
    (with-rect (rectangle-clone (rect-clone rectangle)) 
      (let* ((clone-x (rect-x rectangle-clone))
	     (clone-y (rect-y rectangle-clone))
	     (clone-width (rect-width rectangle-clone))
	     (clone-height (rect-height rectangle-clone))
             (clone-size (rect-size rectangle-clone))
	     (clone-tl-corner (rect-tl rectangle-clone))
	     (clone-br-corner (rect-br rectangle-clone)))
	(format t "~%~%The (x, y, width, height) of RECTANGLE-CLONE = (~a, ~a, ~a, ~a)~%" 
		clone-x clone-y clone-width clone-height)
	(format t "~%The size(width, height) of RECTANGLE-CLONE = (~a, ~a)~%" 
		(width clone-size)
		(height clone-size))
	(format t "~%The top-left corner of RECTANGLE-CLONE = (~a, ~a)~%" 
		(point-x clone-tl-corner)
		(point-y clone-tl-corner))
	(format t "~%The bottom-right corner of RECTANGLE-CLONE = (~a, ~a)~%~%" 
		(point-x clone-br-corner)
		(point-y clone-br-corner))))))


DOT

Dot product computed in double-precision arithmetics.

C++:  _Tp dot(const Point_& pt) const;

LISP-CV: (DOT (SELF POINT) (OTHER POINT)) => :INT

LISP-CV: (DOT2F (SELF POINT-2F) (OTHER POINT-2F)) => :FLOAT

LISP-CV: (DOT2D (SELF POINT-2D) (OTHER POINT-2D)) => :DOUBLE

C++"  _Tp dot(const Point3_& pt) const;

LISP-CV: (DOT3I (SELF POINT-3I) (OTHER POINT-3I)) => :INT

LISP-CV: (DOT3F (SELF (:POINTER POINT-3F)) (OTHER POINT-3F)) => :FLOAT

LISP-CV: (DOT3D (SELF POINT) (OTHER POINT)) => :INT


    Parameters:	

        SELF - A POINT construct.
         
        OTHER - A POINT construct.


(defun dot-example ()

  "This example uses the function DOT to 
   find the dot product of points P1 and 
   P2."

  (let* ((p1 (point 1 2))
	 (p2 (point 3 4)))
    (format t "The dot product of P1 and P2 = ~a~%~%"  
	    (dot p1 p2) )))


(defun dot2f-example ()

  "This example uses the function DOT to 
   find the dot product of 2 point-2f con-
   structs P1 and P2."

  (let* ((p1 (point-2f 1.0f0 2.0f0))
	 (p2 (point-2f 3.0f0 4.0f0)))
    (format t "The dot product of P1 and P2 = ~a~%~%"  
	    (dot2f p1 p2) )))


(defun dot2d-example ()

  "This example uses the function DOT to 
   find the dot product of 2 point-2d con-
   structs P1 and P2."

  (let* ((p1 (point-2d 1.0d0 2.0d0))
	 (p2 (point-2d 3.0d0 4.0d0)))
    (format t "The dot product of P1 and P2 = ~a~%~%"  
	    (dot2d p1 p2) )))


(defun dot3i-example ()

  "This example uses the function DOT to 
   find the dot product of 2 point-3i con-
   structs P1 and P2."

  (let* ((p1 (point-3i 1 2 3))
	 (p2 (point-3i 4 5 6)))
    (format t "The dot product of P1 and P2 = ~a~%~%"  
	    (dot3i p1 p2) )))


(defun dot3f-example ()

  "This example uses the function DOT to 
   find the dot product of 2 point-3f con-
   structs P1 and P2."

  (let* ((p1 (point-3f 7.0f0 8.0f0 9.0f0))
	 (p2 (point-3f 10.0f0 11.0f0 12.0f0)))
    (format t "The dot product of P1 and P2 = ~a~%~%"  
	    (dot3f p1 p2) )))

(defun dot3d-example ()

  "This example uses the function DOT to 
   find the dot product of 2 point-3d con-
   structs P1 and P2."

  (let* ((p1 (point-3d 13.0d0 14.0d0 15.0d0))
	 (p2 (point-3d 16.0d0 17.0d0 18.0d0)))
    (format t "The dot product of P1 and P2 = ~a~%~%"  
	    (dot3d p1 p2))))



CVT-COLOR

Converts an image from one color space to another.

C++: void cvtColor(InputArray src, OutputArray dst, int code, int dstCn=0 )

LISP-CV: (CVT-COLOR (SRC MAT) (DEST MAT) (CODE :INT) ((DEST-CN :INT) 0))


    Parameters:	

        SRC - input image: 8-bit unsigned, 16-bit unsigned ( +16UC...+ ), or single-precision float-
              ing-point.

        DST - output image of the same size and depth as src.

        CODE - color space conversion code (see the description below).

        DEST-CN - number of channels in the destination image; if the parameter is 0, the number of
                  the channels is derived automatically from src and code .


The function converts an input image from one color space to another. In case of a transformation t-
o-from RGB color space, the order of the channels should be specified explicitly (RGB or BGR). Note
that the default color format in OpenCV is often referred to as RGB but it is actually BGR (the byt-
es are reversed). So the first byte in a standard (24-bit) color image will be an 8-bit Blue compon-
ent, the second byte will be Green, and the third byte will be Red. The fourth, fifth, and sixth by-
tes would then be the second pixel (Blue, then Green, then Red), and so on.


The conventional ranges for R, G, and B channel values are:

    0 to 255 for +8U+ images

    0 to 65535 for +16U+ images

    0 to 1 for +32F+ images

In case of linear transformations, the range does not matter. But in case of a non-linear transform-
ation, an input RGB image should be normalized to the proper value range to get the correct results, 
for example, for RGB -> L*u*v* transformation. For example, if you have a 32-bit floating-point ima-
ge directly converted from an 8-bit image without any scaling, then it will have the 0..255 value r-
ange instead of 0..1 assumed by the function. So, before calling CVT-COLOR , you need first to scal-
e the image down:

(LET ((img (/ 1 255)))todo
  (CVT-COLOR IMG IMG +BGR2LUV))

If you use CVT-COLOR with 8-bit images, the conversion will have some information lost. For many ap-
plications, this will not be noticeable but it is recommended to use 32-bit images in applications 
that need the full range of colors or that convert an image before an operation and then convert back.

The function can do the following transformations:


        ***RGB <-> GRAY (+BGR2GRAY+, +RGB2GRAY+, +GRAY2BGR+, +GRAY2RGB+)*** 

Transformations within RGB space like adding/removing the alpha channel, reversing the channel orde-
r, conversion to/from 16-bit RGB color (R5:G6:B5 or R5:G5:B5), as well as conversion to/from graysc-
ale. The conversion from a RGB image to gray is done with:


(CVT-COLOR SRC BWSRC +RGB2GRAY+)

More advanced channel reordering can also be done with (MIX-CHANNELS).


For more info on the CVT-COLOR types below. See OpenCV's cvtColor Documentation at: 


http://docs.opencv.org/modules/imgproc/doc/miscellaneous_transformations.html?highlight=cvtcolor#cv.CvtColor


Thank you to Wikipedia for the information on Color Spaces provided below:


        ***RGB <-> CIE XYZ.REC 709 WITH D65 WHITE POINT (+BGR2XYZ+, +RGB2XYZ+, +XYZ2BGR+, +XYZ2RGB+)***


   Meaning of X, Y, and Z:

 A comparison between a typical normalised M cone's spectral sensitivity and the CIE 1931 luminosity
function for a standard observer in photopic vision

 When judging the relative luminance (brightness) of different colours in well-lit situations, human-
s tend to perceive light within the green parts of the spectrum as brighter than red or blue light 
of equal power. The luminosity function that describes the perceived brightnesses of different wave-
lengths is thus roughly analogous to the spectral sensitivity of M cones.

 The CIE model capitalises on this fact by defining Y as luminance. Z is quasi-equal to blue stimul-
ation, or the S cone response, and X is a mix (a linear combination) of cone response curves chosen 
to be nonnegative. The XYZ tristimulus values are thus analogous to, but not equal to, the LMS cone 
responses of the human eye. Defining Y as luminance has the useful result that for any given Y valu-
e, the XZ plane will contain all possible chromaticities at that luminance.


        ***RGB <-> YCRCB JPEG (OR YCC) (+BGR2YCRCB+, +RGB2YCRCB+, +YCRCB2BGR+, +YCRCB2RGB+)***


 YCbCr, Y′CbCr, or Y Pb/Cb Pr/Cr, also written as YCBCR or Y′CBCR, is a family of color spaces used 
as a part of the color image pipeline in video and digital photography systems. Y′ is the luma comp-
onent and CB and CR are the blue-difference and red-difference chroma components. Y′ (with prime) i-
s distinguished from Y, which is luminance, meaning that light intensity is nonlinearly encoded bas-
ed on gamma corrected RGB primaries.

 Y′CbCr is not an absolute color space; rather, it is a way of encoding RGB information. The actual 
color displayed depends on the actual RGB primaries used to display the signal. Therefore a value e-
xpressed as Y′CbCr is predictable only if standard RGB primary chromaticities are used.


        ***RGB <-> HSV (+BGR2HSV+, +RGB2HSV+, +HSV2BGR+, +HSV2RGB+)***

        ***RGB <-> HLS (+BGR2HLS+, +RGB2HLS+, +HLS2BGR+, +HLS2RGB+)***


 HSL and HSV are the two most common cylindrical-coordinate representations of points in an RGB colo-
r model. The two representations rearrange the geometry of RGB in an attempt to be more intuitive a-
nd perceptually relevant than the cartesian (cube) representation. Developed in the 1970s for compu-
ter graphics applications, HSL and HSV are used today in color pickers, in image editing software, 
and less commonly in image analysis and computer vision.


        ***RGB <-> CIE L*A*B* (+BGR2LAB+, +RGB2LAB+, +LAB2BGR+, +LAB2RGB+)***


 A Lab color space is a color-opponent space with dimension L for lightness and a and b for the colo-
r-opponent dimensions, based on nonlinearly compressed CIE XYZ color space coordinates.

 The dimensions of the Hunter 1948 L, a, b color space are L, a, and b.[1][2] However, Lab is now mo-
re often used as an informal abbreviation for the CIE 1976 (L*, a*, b*) color space (or CIELAB). Th-
e difference between Hunter and CIE color coordinates is that the CIE coordinates are based on a cu-
be root transformation of the color data, while the Hunter coordinates are based on a square root t-
ransformation.


        ***RGB <-> CIE L*U*V* (+BGR2LUV+, +RGB2LUV+, +LUV2BGR+, +LUV2RGB+)***


 In colorimetry, the CIE 1976 (L*, Readu*, v*) color space, commonly known by its abbreviation CIELUV, 
is a color space adopted by the International Commission on Illumination (CIE) in 1976, as a simple-
to-compute transformation of the 1931 CIE XYZ color space, but which attempted perceptual uniformit-
y. It is extensively used for applications such as computer graphics which deal with colored lights-
. Although additive mixtures of different colored lights will fall on a line in CIELUV's uniform ch-
romaticity diagram (dubbed the CIE 1976 UCS), such additive mixtures will not, contrary to popular 
belief, fall along a line in the CIELUV color space unless the mixtures are constant in lightness.


        ***BAYER <-> RGB (+BAYERBG2BGR+, +BAYERGB2BGR+, +BAYERRG2BGR+, +BAYERGR2BGR+
                         +BAYERBG2RGB+, +BAYERGB2RGB+, +BAYERRG2RGB+, +BAYERGR2RGB+)***


 A Bayer filter mosaic is a color filter array (CFA) for arranging RGB color filters on a square gr-
id of photosensors. Its particular arrangement of color filters is used in most single-chip digital 
image sensors used in digital cameras, camcorders, and scanners to create a color image. The filter 
pattern is 50% green, 25% red and 25% blue, hence is also called RGBG,[1][2] GRGB,[3] or RGGB.[4]


(defun cvt-color-example (&optional (camera-index *camera-index*) 
			    (width 640)
			    (height 480))

  "In this example, the function CVT-COLOR converts 
   the camera output to 4 different color spaces an-
   d shows the results in four windows. See the CVT-
   COLOR documentation:

   LISP-CV-MASTER/EXAMPLES/EXAMPLES.LISP 

   for more information on these color spaces."

  (with-capture (cap (video-capture camera-index))
    (let ((window-name-1 "+BGR2HSV+ - CVT-COLOR Example")
	  (window-name-2 "+BGR2XYZ+ - CVT-COLOR Example")
	  (window-name-3 "+BGR2GRAY+ - CVT-COLOR Example")
	  (window-name-4 "+BGR2HLS+ - CVT-COLOR Example")
	  (src1 0)
	  (src2 0)
	  (src3 0))
      (if (not (cap-is-open cap)) 
	  (return-from cvt-color-example 
	    (format t "Cannot open the video camera")))
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-height+ height)
      (format t "~%Frame Size : ~ax~a~%~%" 
	      (cap-get cap +cap-prop-frame-width+)
	      (cap-get cap +cap-prop-frame-height+))
      (named-window window-name-1 +window-normal+)
      (named-window window-name-2 +window-normal+)
      (named-window window-name-3 +window-normal+)
      (named-window window-name-4 +window-normal+)
      (move-window window-name-1 485 98)
      (move-window window-name-2 894 98)
      (move-window window-name-3 485 444)
      (move-window window-name-4 894 444)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(setf src1 (clone frame))
	(setf src2 (clone frame))
	(setf src3 (clone frame))
	(cvt-color frame frame +BGR2HSV+)
	(cvt-color src1 src1 +BGR2XYZ+)
	(cvt-color src2 src2 +BGR2GRAY+)
	(cvt-color src3 src3 +BGR2HLS+)
	(imshow window-name-1 frame)
	(imshow window-name-2 src1)
	(imshow window-name-3 src2)
	(imshow window-name-4  src3)
	(del-mat frame) (del-mat src1) 
	(del-mat src2) (del-mat src3))
      (destroy-window window-name-1)
      (destroy-window window-name-2)
      (destroy-window window-name-3)
      (destroy-window window-name-4))))


SCALE

Finds the product a matrix and a scalar..

C++: MatExpr * operator

LISP-CV: (SCALE (SELF MAT-EXPR) (ALPHA :DOUBLE)) => MAT-EXPR


    Parameters:	

        SELF - A single float or double float matrix.

        ALPHA - A scalar of type double-float. 


This is the primary function used in this library for multiplication by and division by scalar. See 
SCALE-EXAMPLE for an example of division by scalar. You may need to coerce the rerturn value of SCALE, 
a scaled matrix, back to type MAT with the function (FORCE), (or the shorthand version (>>)) to use in 
other functions. Also matrices of MAT type must be coerced to MAT-EXPR, with the function PROMOTE(<<), 
before passing to SCALE.


(defun scale-example ()

  "In this example a +32F+(float) matrix is 
   created and filled with data. Then, usin-
   g SCALE, each element of the matrix 
   is divided by the scalar 10. Finally the 
   matrix is printed."

  (let* ((data (alloc :float '(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0 
                               6.0f0 7.0f0 8.0f0 9.0f0)))
	 (mat (mat 3 3 +32f+ data))
	 (scaled-mat (scale (<< mat) (/ 1d0 10d0))))
    (dotimes (i 3)
      (dotimes (j 3)
	(format t "~a" (at (>> scaled-mat) i j :float))
	(princ #\Space))
      (princ #\Newline))
    (free data)))



DRAW-MATCHES

Draws the found matches of keypoints from two images.

C++: void drawMatches(const Mat& img1, const vector<KeyPoint>& keypoints1, const Mat& img2, const vector<KeyPoint>& keypoints2, const vector<DMatch>& matches1to2, Mat& outImg, const Scalar& matchColor=Scalar::all(-1), const Scalar& singlePointColor=Scalar::all(-1), const vector<char>& matchesMask=vector<char>(), int flags=DrawMatchesFlags::DEFAULT )

LISP-CV: (DRAW-MATCHES (IMG1 MAT) (KEYPOINTS1 KEYPOINT) (IMG2 MAT) (KEYPOINTS2 KEYPOINT) (MATCHES1TO2 VECTOR-DMATCH) (OUTIMG MAT) (MATCH-COLOR SCALAR) (SINGLE-POINT-COLOR SCALAR) &OPTIONAL ((MATCHES-MASK VECTOR-CHAR) (VEC-CHAR)) ((FLAGS :INT) +DEFAULT+))
-
C++: void drawMatches(const Mat& img1, const vector<KeyPoint>& keypoints1, const Mat& img2, const vector<KeyPoint>& keypoints2, const vector<vector<DMatch>>& matches1to2, Mat& outImg, const Scalar& matchColor=Scalar::all(-1), const Scalar& singlePointColor=Scalar::all(-1), const vector<vector<char>>& matchesMask=vector<vector<char> >(), int flags=DrawMatchesFlags::DEFAULT )

    Parameters:	

        IMG1 - First source image.

        KEYPOINTS1 - Keypoints from the first source image.

        IMG2 - Second source image.

        KEYPOINTS2 - Keypoints from the second source image.

        MATCHES1TO2 - Matches from the first image to the second one, which means that (KEYPOINTS1 I)
                      has a corresponding point in (KEYPOINTS2 (MATCHES I)).

        OUT-IMG - Output image. Its content depends on the flags value defining what is drawn in the 
                  output image. See possible flags bit values below.

        MATCH-COLOR - Color of matches (lines and connected keypoints). If (EQ MATCH-COLOR (SCALAR-ALL -1)) 
                      the color is generated randomly.

        SINGLE-POINT-COLOR - Color of single keypoints (circles), which means that keypoints do not 
                             have the matches. If (EQ SINGLE-POINT-COLOR (SCALAR-ALL -1)) , the color 
                             is generated randomly.

        MATCHES-MASK - Mask determining which matches are drawn. If the mask is empty, all matches are drawn.

        FLAGS - Flags setting drawing features. Possible flags bit values are defined below.

This function draws matches of keypoints from two images in the output image. Match is a line connecting 
two keypoints (circles). The FLAGS parameters are defined as follows:

        +DEFAULT+ = 0 - Output image matrix will be created (MAT-CREATE), i.e. existing memory of 
                        output image may be reused. Two source images, matches, and single keypoints 
                        will be drawn. For each keypoint, only the center point will be drawn (without 
                        a circle around the keypoint with the keypoint size and orientation).

        +DRAW-OVER-OUTIMG+ = 1 - Output image matrix will not be created (using MAT-CREATE). Matches 
                                 will be drawn on existing content of output image.

        +NOT-DRAW-SINGLE-POINTS = 2 - Single keypoints will not be drawn.

        +DRAW-RICH-KEYPOINTS+ = 4 - For each keypoint, the circle around keypoint with keypoint siz-
                                    e and orientation wilL be drawn.



(defun draw-matches-example (filename-1 filename-2) 

  "I use this example to show examples of the parameters of DRAW-MATCHES.
   See the documentation in EXAMPLES.LISP for more details on these para-
   meters. Each window is labeled, first, with the color used to define g-
   ood matches between images, the MATCH-COLOR parameter. Secondly the co-
   lor used to mark empty keypoints or non-matches, the SINGLE-POINT-COLOR 
   parameter. Finally, each window is labeled with the name of the flag u-
   sed to set  drawing features for that particular window, for example:
 
      RED * WHITE * +NOT-DRAW-SINGLE-POINTS+
    
    Try using the box.png and box_in_scene.png images located inside the
    LISP-CV-MASTER/IMAGES directory to get a clearer understanding of th-
    is example the first time you run it."

  ;; the object you want to track 
  (let* ((object (imread filename-1 +load-image-grayscale+))
	 ;; the image the object is a part of
	 (image (imread filename-2 +load-image-grayscale+)) 
	 (keypoints-a (vec-key-point))
	 (keypoints-b (vec-key-point))
	 (descriptors-a (mat))
	 (descriptors-b (mat))
         ;; Set brisk parameters
	 (thresh 60)
	 (octaves 4)
	 (pattern-scale 2.0f0)
         ;; declare feature detector
	 (briskd (brisk thresh octaves pattern-scale))
         ;; declare matcher
	 (matcher (bf-matcher))
	 (matches (vec-dmatch))
	 (all-matches (mat))
	 (window-name-1 "RANDOM * RANDOM * +DEFAULT+")
	 (window-name-2 "BLACK * WHITE * +DRAW-RICH-KEYPOINTS+")
	 (window-name-3 "RED * WHITE * +NOT-DRAW-SINGLE-POINTS+")
	 (window-name-4 "WHITE * RANDOM * +DRAW-RICH-KEYPOINTS+"))
    (if (empty (or object image)) 
	(return-from draw-matches-example 
	  (format t "Both images were not loaded")))
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)
    (named-window window-name-3 +window-normal+)
    (named-window window-name-4 +window-normal+)
    (move-window window-name-1 485 98)
    (move-window window-name-2 894 98)
    (move-window window-name-3 485 444)
    (move-window window-name-4 894 444)
    ;; create a feature detector
    (feat-detector-create briskd "SimpleBlob")
    ;; detect keypoints in OBJECT
    (feat-detector-detect briskd object keypoints-a)
    ;; Compute the descriptors for a set of keypoints detected in object
    (feat-2d-compute briskd object keypoints-a descriptors-a)
    ;; detect keypoints in IMAGE
    (feat-detector-detect briskd image keypoints-b)
    ;; Compute the descriptors for a set of keypoints detected in IMAGE
    (feat-2d-compute briskd image keypoints-b descriptors-b)
    ;; find the best match for each descriptor
    (descrip-matcher-match matcher descriptors-a descriptors-b matches)
    ;; draw the found matches
    (draw-matches object keypoints-a image keypoints-b matches all-matches 
		  (scalar-all -1) (scalar-all -1) (vec-char) 
		  +default+)
    ;; show the matches in a window 
    (imshow window-name-1 all-matches)
    ;; draw the found matches
    (draw-matches object keypoints-a image keypoints-b matches all-matches 
		  (scalar 0 0 0) (scalar 255 255 255) (vec-char) 
		  +draw-rich-keypoints+)
    ;; show the matches in a window 
    (imshow window-name-2 all-matches)
    ;; draw the found matches
    (draw-matches object keypoints-a image keypoints-b matches all-matches 
		  (scalar 0 0 255) (scalar 255 255 2555) (vec-char) 
		  +not-draw-single-points+)
    ;; show the matches in a window 
    (imshow window-name-3 all-matches)
    ;; draw the found matches
    (draw-matches object keypoints-a image keypoints-b matches all-matches 
		  (scalar-all 255) (scalar-all -1) (vec-char) 
		  +draw-rich-keypoints+)
    ;; show the matches in a window 
    (imshow window-name-4 all-matches)
    (del-mat object)
    (del-mat image)
    (del-vec-kp keypoints-a)
    (del-vec-kp keypoints-b)
    (del-mat descriptors-a)
    (del-mat descriptors-b)
    (del-feature-2d briskd)
    (del-feature-2d matcher)
    (del-vec-dm matches)
    (del-mat all-matches)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name-1)
    (destroy-window window-name-2)
    (destroy-window window-name-3)
    (destroy-window window-name-4)))



BRISK

The BRISK constructor

C++: BRISK::BRISK(int thresh=30, int octaves=3, float patternScale=1.0f)

LISP-CV: (BRISK &OPTIONAL ((THRESH :INT) 30) ((OCTAVES :INT) 3) ((PATTERN-SCALE :FLOAT) 1.0F0) => FEATURE-2D

    Parameters:	

        THRESH - FAST/AGAST detection threshold score.

        OCTAVES - detection octaves. Use 0 to do single scale.

        PATTERN-SCALE - apply this scale to the pattern used for sampling the neighbourhood of a keypoint.

BRISK is a object implementing the BRISK keypoint detector and descriptor extractor, described in [LCS11]:

http://docs.opencv.org/modules/features2d/doc/feature_detection_and_description.html?highlight=brisk#lcs11



(defun brisk-example (filename-1 filename-2)

  "Warning: Creating 12 BRISK objects uses a lot of RAM. It
   will get deleted though, within 30 seconds. It's best to 
   start this program with a good amount of available RAM.

   Don't let this example make you nervous it's basically 12 
   FEAT-DETECTOR-CREATE-EXAMPLES, stacked, in one. Here I am 
   basically just showing, in a quick easy to see fashion, h-
   ow the THRESH, OUTPUT and PATTERN-SCALE parameters of the 
   function BRISK affect it's output. Each of the 12 windows 
   has the function call used to set those parameters printe-
   d on the titlebar, so you don't have to look through the 
   code to get the effect of this example. For example, if y-
   ou see this on the windows titlebar: (BRISK 0 0 0.0f0), t-
   hen you know the BRISK parameter set for that window is: 

       THRESH = 0, OCTAVES  = 0, PATTERN-SCALE = 0.0f0.

   Note: Try using the box.png and the box_in_scene.png from
   the LISP-CV-MASTER/IMAGES directory to get a better under-
   standing of this example the first time you run it. And, 
   just be aware, this example takes a few seconds to start."

  ;; read two images in grayscale, first the object you want to track,
  (let* ((gray-a (gc:imread filename-1 +load-image-grayscale+))
	 ;; second the image the object is a part of
	 (gray-b (gc:imread filename-2 +load-image-grayscale+))
         ;; make arrays to hold the keypoints and descriptors
         (keypoints-a-arr (make-array '(12))) 
         (keypoints-b-arr (make-array '(12)))
	 (descriptors-a-arr (make-array '(12)))
	 (descriptors-b-arr (make-array '(12)))  
	 (matcher-arr (make-array '(12))) 
	 (matches-arr (make-array '(12)))
	 (all-matches-arr (make-array '(12)))
	 ;; declare an array of BRISK objects
	 (brisk-arr 
	  (make-array 12 :initial-contents 
		      (list
		       (brisk 0 0 0.0f0)
		       (brisk 60 0 0.0f0)
		       (brisk 120 0 0.0f0)
		       (brisk 180 0 0.0f0)
		       (brisk 0 4 1.0f0)
		       (brisk 60 4 1.0f0)
		       (brisk 120 4 1.0f0)
		       (brisk 180 4 1.0f0)
		       (brisk 0 8 2.0f0)
		       (brisk 60 8 2.0f0)
		       (brisk 120 8 2.0f0)
		       (brisk 180 8 2.0f0))))
         ;; declare an array of 12 window names which will be used 
         ;; by MOVE-WINDOW, NAMED WINDOW, IMSHOW and  DESTROY-WINDOW
	 (window-name-arr 
	  (make-array 12 :initial-contents 

		      (list
		       "(BRISK 0 0 0.0f0) - BRISK Example"
		       "(BRISK 60 0 0.0f0) - BRISK Example"
		       "(BRISK 120 0 0.0f0) - BRISK Example"
		       "(BRISK 180 0 0.0f0) - BRISK Example"
		       "(BRISK 0 4 1.0f0) - BRISK Example"
		       "(BRISK 60 4 1.0f0) - BRISK Example"
		       "(BRISK 120 4 1.0f0) - BRISK Example"
		       "(BRISK 180 4 1.0f0) - BRISK Example"
		       "(BRISK 0 8 2.0f0) - BRISK Example"
		       "(BRISK 60 8 2.0f0) - BRISK Example"
		       "(BRISK 120 8 2.0f0) - BRISK Example"
		       "(BRISK 180 8 2.0f0) - BRISK Example"))))
    ;; if images not loaded, break
    (if (empty (or gray-a gray-b)) 
	(return-from brisk-example 
	  (format t "Both images were not loaded")))
    ;; create 12 windows to show the output images in
    (dotimes (i 12)
      (named-window (aref window-name-arr i) +window-normal+))
    ;; move the windows to specific coordinates
    (move-window (aref window-name-arr 0) 88 0)
    (move-window (aref window-name-arr 1) 538 0)
    (move-window (aref window-name-arr 2) 988 0)
    (move-window (aref window-name-arr 3) 1438 0)
    (move-window (aref window-name-arr 4) 88 368)
    (move-window (aref window-name-arr 5) 538 368)
    (move-window (aref window-name-arr 6) 988 368)-
    (move-window (aref window-name-arr 7) 1438 368)
    (move-window (aref window-name-arr 8) 88 708)
    (move-window (aref window-name-arr 9) 538 708)
    (move-window (aref window-name-arr 10) 988 708)
    (move-window (aref window-name-arr 11) 1438 708)
    ;; declare 2 arrays of 12 keypoints each
    (dotimes (i 12)
      (setf (aref keypoints-a-arr i) (gc:vec-key-point))
      (setf (aref keypoints-b-arr i) (gc:vec-key-point))
      ;; declare an array of 12 query descriptors 
      (setf (aref descriptors-a-arr i) (gc:mat))
      ;; declare an array of 12 train descriptors 
      (setf (aref descriptors-b-arr i) (gc:mat))
      ;; declare an array of 12 matchers
      (setf (aref matcher-arr i) (gc:bf-matcher))
      ;; declare an array of 12 MAT constructs to hold the 
      ;; matches from the first image to the second one
      (setf (aref matches-arr i) (gc:vec-dmatch))
      ;; declare an array of 12 MAT constructs to hold the final output images
      (setf (aref all-matches-arr i) (gc:mat))
      ;; find matches, between the two images, 12 times,
      ;; each using a different set of BRISK parameters
      (feat-detector-create (aref brisk-arr i) "BRISK")
      ;; detect keypoints in the image GRAY-A
      (feat-detector-detect (aref brisk-arr i) gray-a (aref keypoints-a-arr i))
      ;; Compute the descriptors for a set of keypoints detected in GRAY-A
      (feat-2d-compute (aref brisk-arr i) gray-a (aref keypoints-a-arr i) (aref descriptors-a-arr i))
      ;; detect keypoints in the image GRAY-B
      (feat-detector-detect (aref brisk-arr i) gray-b (aref keypoints-b-arr i))
      ;; compute the descriptors for a set of keypoints detected in GRAY-B
      (feat-2d-compute (aref brisk-arr i) gray-b (aref keypoints-b-arr i) (aref descriptors-b-arr i))
      ;; find the best match for each descriptor
      (descrip-matcher-match (aref matcher-arr i) (aref descriptors-a-arr i) 
			     (aref descriptors-b-arr i) (aref matches-arr i))
      ;; draw the found matches
      (draw-matches gray-a (aref keypoints-a-arr i) gray-b (aref keypoints-b-arr i) 
		    (aref matches-arr i) (aref all-matches-arr i) 
		    (gc:scalar-all -1) (gc:scalar-all -1) (gc:vec-char) 
		    +draw-rich-keypoints+)
      ;; show the 12 different matches in 12 windows
      (imshow (aref window-name-arr i) (aref all-matches-arr i))
      (del-feature-2d (aref brisk-arr i)))
    ;; after 'esc' key is pressed destroy all 12 windows
    (loop while (not (= (wait-key 0) 27)))
    (dotimes (i 12)
      (destroy-window (aref window-name-arr i)))))




FEAT-DETECT-CREATE

Creates a feature detector by its name.

C++: Ptr<FeatureDetector> FeatureDetector::create(const string& detectorType)

LISP-CV: (FEAT-DETECTOR-CREATE (SELF FEATURE-2D) (DETECTOR-TYPE :STRING)) => FEATURE-2D 


    Parameters:	

        SELF - A pointer to a BRISK construct

        DETECTOR-TYPE - Feature detector type:


The following detector types are supported:


    "FAST" - FAST-FEATURE-DETECTOR
    "STAR" - STAR-FEATURE-DETECTOR
    "SIFT" - SIFT (nonfree module)
    "SURF" - SURF (nonfree module)
    "ORB" - ORB
    "BRISK" - BRISK
    "MSER" - MSER
    "GFTT" - GOOD-FEATURES-TO-TRACK-DETECTOR
    "HARRIS" - GOOD-FEATURES-TO-TRACK-DETECTOR with Harris detector enabled
    "Dense" - DENSE-FEATURE-DETECTOR
    "SimpleBlob" - SIMPLE-BLOB-DETECTOR


Also a combined format is supported: feature detector adapter name:

           ("GRID" - GRID-ADAPTED-FEATURE-DETECTOR, "PYRAMID" - PYRAMID-ADAPTED-FEATURE-DETECTOR) + 

                                     feature detector name: (see above), for example: "GRIDFAST", "PYRAMIDSTAR".



(defun feat-detect-create-example (filename-1 filename-2) 

  "Try using the box.png and the box_in_scene.png from
   the LISP-CV-MASTER/IMAGES directory to get a better 
   understanding of this example the first time you ru-
   n it."

  ;; set brisk parameters
  (let* ((thresh 60)
	 (octaves 4)
	 (pattern-scale 2.0f0)
	 (window-name "All Matches - FEAT-DETECT-CREATE Example"))
    ;; read some images in grayscale -> The object you want to track
    (with-mat ((gray-a (imread filename-1 +load-image-grayscale+))
	       ;; The image the object is a part of
	       (gray-b (imread filename-2 +load-image-grayscale+)) 
	       (descriptors-a (mat))
	       (descriptors-b (mat))
	       (all-matches (mat)))
      (if (empty (or gray-a gray-b)) 
	  (return-from feat-detect-create-example 
	    (format t "Both images were not loaded")))
      (with-vector-key-point ((keypoints-a (vec-key-point))
			      (keypoints-b (vec-key-point)))
	;; declare a variable BRISKD of the type FEATURE-2D
	(with-feature-2d ((briskd (brisk thresh octaves pattern-scale))
			  ;; declare matcher
			  (matcher (bf-matcher)))
	  (with-vector-dmatch ((matches (vec-dmatch)))
	    ;; create a feature detector
	    (feat-detector-create briskd "STAR")
	    ;; detect keypoints in the image GRAY-A
	    (feat-detector-detect briskd gray-a keypoints-a)
	    ;; Compute the descriptors for a set of keypoints detected in GRAY-A
	    (feat-2d-compute briskd gray-a keypoints-a descriptors-a)
	    ;; detect keypoints in the image GRAY-B
	    (feat-detector-detect briskd gray-b keypoints-b)
	    ;; Compute the descriptors for a set of keypoints detected in GRAY-B
	    (feat-2d-compute briskd gray-b keypoints-b descriptors-b)
	    ;; find the best match for each descriptor
	    (descrip-matcher-match matcher descriptors-a descriptors-b matches)
	    (with-named-window (window-name +window-normal+)
	      (move-window window-name 759 175)
	      (with-scalar ((scalar (scalar-all -1)))
		;; draw the found matches
		(with-vector-char ((matches-mask (vec-char)))
		  (draw-matches gray-a keypoints-a gray-b keypoints-b matches all-matches 
				scalar scalar matches-mask
				+not-draw-single-points+)
		  ;; show the matches in a window 
		  (imshow window-name all-matches)
		  (loop 
		     (let ((c (wait-key 33)))
		       (when (= c 27)
			 (return)))))))))))))



SURF

The SURF extractor constructors.

C++: SURF::SURF()

LISP-CV: (SURF) => FEATURE-2D

C++: SURF::SURF(double hessianThreshold, int nOctaves=4, int nOctaveLayers=2, bool extended=true, bool upright=false )

LISP-CV: (SURF (HESSIAN-THRESHOLD :DOUBLE) &OPTIONAL ((N-OCTAVES :INT) 4) 
                 ((EXTENDED :BOOLEAN) T) ((UPRIGHT :BOOLEAN) NIL)) => FEATURE-2D

    Parameters:	

        HESSIAN-THRESHOLD - Threshold for hessian keypoint detector used in SURF.

        N-OCTAVES - Number of pyramid octaves the keypoint detector will use.

        N-OCTAVE-LAYERS - Number of octave layers within each octave.

        EXTENDED - Extended descriptor flag (t - use extended 128-element descriptors; nil - u-
                   se 64-element descriptors).

        UPRIGHT - Up-right or rotated features flag (t - do not compute orientation of features; 
                  nil - compute orientation).



(defun surf-example (filename-1 filename-2) 

  "Try using the box.png and the box_in_scene.png from
   the LISP-CV-MASTER/IMAGES directory to get a better 
   understanding of this example the first time you ru-
   n it."

  ;; Read in image in grayscale -> The object you want to track
  (let* ((img-1 (gc:imread filename-1 +load-image-grayscale+))
	 ;; The image the object is a part of
	 (img-2 (gc:imread filename-2 +load-image-grayscale+))
         (min-hessian 400d0) 
         (detector (gc:surf min-hessian))
	 (keypoints-1 (gc:vec-key-point))
	 (keypoints-2 (gc:vec-key-point))
         (extractor (gc:surf))
	 (descriptors-1 (gc:mat))
	 (descriptors-2 (gc:mat))
	 (matcher (gc:bf-matcher +norm-l2+))
	 (matches (gc:vec-dmatch))
	 (img-matches (gc:mat))
	 (window-name "Image Matches - SURF Example"))
    (if (empty (or img-1 img-2)) 
	(return-from surf-example 
	  (format t "Both images were not loaded")))
    (with-named-window (window-name +window-normal+)
      (move-window window-name 759 175)
      ;;-- Step 1: Detect the keypoints using SURF Detector
      (feat-detector-detect detector img-1 keypoints-1)
      (feat-detector-detect detector img-2 keypoints-2)
      ;;-- Step 2: Calculate descriptors (feature vectors)
      (feat-2d-compute extractor img-1 keypoints-1 descriptors-1)
      (feat-2d-compute extractor img-2 keypoints-2 descriptors-2)
      ;-- Step 3: Matching descriptor vectors with a brute force matcher
      (descrip-matcher-match matcher descriptors-1 descriptors-2 matches)
      ;;-- Draw matches
      (draw-matches img-1 keypoints-1 img-2 keypoints-2 matches img-matches)
      ;;-- Show detected matches
      (imshow window-name img-matches)
      (loop
	 (let ((c (wait-key 33)))
	   (when (= c 27)
	     (return)))))))



$

Time how long a function takes to complete n iterations.


LISP-CV: ($ FORM &optional (COUNT-FORM 1000000) ) => RESULT

    Parameters:	

        FORM - From the Common Lisp HyperSpec:
               1. any object meant to be evaluated. 2. a symbol, a compound form, or a self-evaluat-
               ing object. 3. (for an operator, as in ``<<operator>> form'') a compound form having 
               that operator as its first element. ``A quote form is a constant form.''

        COUNT-FORM - The number of iterations of FORM you would like to calculate.

This is useful, if you have just written a function and would like to time the seconds it takes to 
complete n iterations, because all you have to do is go back one in the REPL history and add a $ and 

Example:

LISP-CV> ($  (sleep 1) 5)

Evaluation took:
  5.0000 seconds of real time
  0.004951 seconds of total run time (0.003775 user, 0.001176 system)
  0.10% CPU
  12,501,013,695 processor cycles
  33,008 bytes consed
  
NIL



VECTOR

Bindings for the C++ VECTOR class.

C++: template < class T, class Alloc = allocator<T> > class vector; // generic template

LISP-CV: See description.


    Parameters:	

       See description.


The bindings for the C++ vector class, so far, are:


LISP-CV:         
             
-----------------------        
VEC-CHAR -> vector<char>

VEC-DMATCH -> vector<DMatch>      

VEC-DOUBLE -> vector<double> 

VEC-FLOAT -> vector<float>

VEC-INT -> vector<int>

VEC-KEY-POINT -> vector<KeyPoint>

VEC-POINT -> vector<Point>

VEC-POINT-2F -> vector<Point2f>

VEC-RECT -> vector<Rect>

VEC-UCHAR -> vector<uchar>
-------------------------



Description:



Vectors with numbers as elements, VEC-CHAR, VEC-DOUBLE, VEC-FLOAT and VEC-INT VEC-UCHAR operate as 
follows:(I use VEC-FLOAT as an example of the 5 aforementioned vector types). 

The idea behind the vector macros in Lisp-CV is you use them primarily to pass data to and from the 
underlying OpenCV code only. The reason being, due to the nature of wrapping the C++ vector class in 
Lisp, some of the C++ speed is lost. Since Lisp vectors are extremely fast, the best workaround is to 
use Lisp's own vector and list processing functions to fill in the gaps. The functions are designed so 
first you would create, for instance, a Lisp vector or a list of floats e.g.


CV> (DEFPARAMETER A '(1F0 2F0 3F0))

A

CV> A

(1.0 2.0 3.0)


CV> (DEFPARAMETER A (LIST 1F0 2F0 3F0))

A

CV> A

(1.0 2.0 3.0)


CV> (DEFPARAMETER A (VECTOR 1F0 2F0 3F0))

A

CV> A

#(1.0 2.0 3.0)


Next, and this is where it gets fun, you can use any of the myriad of functions in Lisp designed 
to operate on lists and vectors and then pass the results to the underlying OpenCV code by way of 
the Lisp-CV vector functions. Since Lisp's main strength is list processing, hence it's name, this 
is a great feature and adds a lot of power to OpenCV and a lot of speed to the Lisp-CV bindings. 

Now, once you have created your vector as above and performed whatever operations you needed to on 
it, you can convert that vector to a C++ vector as so:


CV> (DEFPARAMETER B (VEC-FLOAT A))

B

CV> B


#<STD-VECTOR-FLOAT {1006BA6983}> <--- A CLOS object, STD-VECTOR-FLOAT type, pointing to a vector<float>


That's all there is to it. Now it can be passed to any Lisp-CV binding for OpenCV or C++ that will 
accept it.


Now, if an OpenCV or C++ binding in this library outputs a STD-VECTOR-FLOAT(vector<float>) type object
(again, using vector<float> as an example of all number based C++ vector types). You can convert that 
back to a Lisp list or a Lisp vector like so:(variable B being a #<STD-VECTOR-FLOAT {1006BA6983}>:)

CV> (VEC-FLOAT :TO-LISP-LIST B)

(1.0 2.0 3.0)


CV> (VEC-FLOAT :TO-LISP-VEC B)

#(1.0 2.0 3.0)


and then complete any necessary operations on the data.


The ability to retrieve the length of a C++ vector is built into the Lisp-CV vector functions, it is 
quite fast as well. You just evaluate as follows(using variable B from above):


CV> (VEC-FLOAT :LENGTH B)

3  <--- Vector B length


If you would like to created an unititialized pointer to a vector<float> to pass to a function, you 
evaluate:


LISP-CV> (VEC-FLOAT)


#<STD-VECTOR-FLOAT {100352FEA3}> <--- Output is an object pointing to an uninitialized vector<float>.



If you would like to created an initialized C++ vector to pass to a function, you evaluate either of 
the below:



LISP-CV> (VEC-FLOAT '(1f0 2f0 3f0)) 


LISP-CV> (VEC-FLOAT (LIST 1f0 2f0 3f0)) 


LISP-CV> (VEC-FLOAT (VECTOR 1f0 2f0 3f0))  


#<STD-VECTOR-FLOAT {100352FEA3}> <--- Output is a CLOS object pointing to an initialized vector<float>.



Again these functions are a little bit slow for extremely large operations. However, if you assign 
the vector to a variable first like this:


CV> (DEFPARAMETER A (VECTOR 1F0 2F0 3F0 4F0 5F0))

A


CV> (VEC-FLOAT A)  <--- this only takes about 0.699 seconds for a million iterations



The functionality to retrieve data from an initialized vector is built into the vector functions. So 
to retrieve data from a vector you evaluate as follows(vector elements are zero-based):


CV> (DEFPARAMETER A (VEC-FLOAT '(1F0 2F0 3F0)))

A

LISP-CV> (VEC-FLOAT A)  <--- Access the 0th element of A.

1.0

LISP-CV> (VEC-FLOAT A 1)   <---Access the 1st element of A.

2.0

LISP-CV> (VEC-FLOAT A 2)  <---Access the 2nd element of A.

3.0



Vectors with objects as their elements, VEC-DMATCH, VEC-KEY-POINT, VEC-POINT, VEC-POINT-2F 
and VEC-RECT operate as follows:(I use VEC-POINT as an example of the four vectors.)


If you would like to created an uninitialized vector, you evaluate:


CV> (VEC-POINT)

#<STD-VECTOR-POINT {1007B187B3}> <--- Output is a object pointing to an uninitialized POINT vector.


If you would like to created an initialized vector, you evaluate:


CV> (VEC-POINT (LIST (POINT 1 2) (POINT 3 4)))

#<STD-VECTOR-POINT {1002DBE013}> <--- Output is a object pointing to an initialized POINT vector.


Again, you'll get the most efficiency if you, first, assign the list or vector to a variable.


The functionality to retrieve data from an initialized vector of objects is built into the vector 
function. You just evaluate as follows(vector elements are zero-based):


CV> (DEFPARAMETER A (VEC-POINT (LIST (POINT 1 2) (POINT 3 4)))) <--- Create an initialized vector A.

A 


CV> (VEC-POINT A 0)  <--- Access the 0th POINT in vector A

#<CV-POINT {1003D4FA73}> 


CV> (VEC-POINT A 1)  <--- Access the 1st POINT in vector A

#<CV-POINT {10042AB633}> 



LISP-CV> (VEC-POINT A 0 0) <--- Access the 0th element of the 0th POINT in vector A.

1

LISP-CV> (VECT-POINT A 0 1) <--- Access the 1st element of the 0th POINT in vector A.

2

LISP-CV> (VEC-POINT A 1 0) <--- Access the 0th element of the 1st POINT in vector A.

3

LISP-CV> (VEC-POINT A 1 1) <--- Access the 1st element of the 1st POINT in vector A.

4


And, also, as above, you can convert the POINT vector back to a Lisp list or vector as follows:


CV> (VEC-POINT :TO-LISP-LIST A)


(#<CV-POINT {1006C49DA3}> #<CV-POINT {1006C49E03}>)  <--- Lisp list


CV> (VEC-POINT :TO-LISP-VEC A)


#(#<CV-POINT {1006C49DA3}> #<CV-POINT {1006C49E03}>)  <--- Lisp vector


And also retrieve the length of the vector:


(VEC-POINT :LENGTH A)

2  <--- Vector a length,



Note: For VEC-DMATCH. When accessing the numerical content of of the objects in a VECTOR-DMATCH, 
you must supply the type of the element you are attempting to access as the last parameter e.g.


CV> (DEFPARAMETER A (VEC-DMATCH (VECTOR (DMATCH 1 2 3F0) (DMATCH 1 2 3F0))))

A

CV> (VEC-DMATCH A 0 0 :INT)

1

CV> (VEC-DMATCH A 0 1 :INT)

2

CV> (VEC-DMATCH A 0 2 :FLOAT)  
    
#<SINGLE-FLOAT quiet NaN>                                

CV> (VEC-DMATCH A 0 2 :INT)

-1

CV> (VEC-DMATCH A 0 3 :FLOAT)  <--- If a VECTOR-DMATCH contains a 3 element DMATCH, the value you entered 
                                    as the 3rd parameter will be accessible at the 4th index. 
3.0



LISP-CV - MACROS


Macro for CFFI::FOREIGN-ALLOC

CFFI: - foreign-alloc type &key initial-element initial-contents (count 1) null-terminated-p ⇒ pointer

LISP-CV: (ALLOC TYPE VALUE) => :POINTER


    Parameters:	

        TYPE - A CFFI type

        VALUE - A number or a sequence - Stand-in for the INITIAL-ELEMENT 
                and INITIAL-CONTENTS parameter of FOREIGN-ALLOC



Example:

LISP-CV> (DEFPARAMETER A (ALLOC :DOUBLE 8.0D0))

A

LISP-CV> (MEM-AREF A :DOUBLE)

8.0d0

LISP-CV> (DEFPARAMETER B (ALLOC :INT '(1 2 3)))

B

LISP-CV> (MEM-AREF B :INT)

1

LISP-CV> (MEM-AREF B :INT 1)

2

LISP-CV> (MEM-AREF B :INT 2)

3



FREE

Macro for CFFI::FOREIGN-FREE

CFFI: - foreign-free ptr ⇒ undefined

LISP-CV: (FREE PTR) => undefined


    Parameters:	

        PTR - A foreign pointer.


Example:

LISP-CV> (DEFPARAMETER A (ALLOC :INT 55))

A

LISP-CV> (MEM-REF A :INT)

55

LISP-CV> (FREE A)

NIL

LISP-CV> (MEM-REF A :INT)

0



?

Macro for CFFI::MEM-AREF

CFFI: mem-aref ptr type &optional (index 0)

CFFI: (setf (mem-aref ptr type &optional (index 0)) new-value) 

LISP-CV: (? (PTR TYPE &OPTIONAL (INDEX 0))) => RESULT


    Parameters:	

        PTR - A foreign pointer.

        TYPE - A foreign-type, See below.

        INDEX - Index of the element you want retrieve, defaults to 0


This function is a macro for CFFI's MEM-AREF. It retrieves the value of a METAOBJECT at (INDEX N)


The typenames associated with ? include:

:char     :int16    :short    :uint32   :string
:double   :int32    :uchar    :uint64   :pointer
:float    :int64    :uint     :ullong
:int      :llong    :uint8    :ulong
:int8     :long     :uint16   :ushort


Example:


LCV> (DEFPARAMETER A (C-STRING-TO-STRING "12545" 5))

A

LCV> A

#<STD-STRING {1003C23F63}>

LCV> (? A :STRING)

"12545"


CONTINUABLE

Catches any error and gives the option to ignore it and continue.

LISP-CV: (CONTINUABLE &BODY BODY)

    Parameters: 
          
         body - Give all expressions in the macro


Macro included for reference:


(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue"  )))


UPDATE-SWANK

Grabs SWANK connections and tells it to handle requests. Call this every loop in the main loop of your 
program.

LISP-CV: (UPDATE-SWANK)

     Parameters: None


Function included for reference:


(defun update-swank ()
  "Grabs SWANK connections and tells it to handle requests. 
   Call this every loop in the main loop of your program"
  (continuable
    (let ((connection (or swank::*emacs-connection*
			  (swank::default-connection))))
      (when connection
	(swank::handle-requests connection t)))))


Example:


(defvar n (/ 1 1))

(defun live-code-editing-example (filename)

  "You can update this program as it runs! Just run 
   M-x SLIME-COMPILE-AND-LOAD-FILE then at the REPL 
   run (LIVE-CODE-EDITING-EXAMPLE FILENAME). Switch 
   to the REPL, even though its not active, you can 
   use (SETF N (/ 1 <NEW VALUE>)) to update N which 
   will change the strobe effect. After you running 
   SETF once you will get the REPL back"

  (let ((image (imread filename 1))
	(x 0)
	(window-name "test"))
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    (do* ((value (scalar 0 0 0)))
	 ((plusp (wait-key *millis-per-frame*)) 
	  (format t "Key is pressed by user"))
      (incf x 1)
      (if (= x 2) (progn (assgn-val image value)  (decf x 2)))
      (update-swank)
      (continuable (imshow window-name image))
      (del-mat image)
      (setf image (imread filename 1))
      (sleep n))
    (destroy-window window-name)))



EXTRA FUNCTIONS:


DEL

Deletes allocated memory

C++: void operator delete  ( void* ptr )

LISP-CV: (DEL (SELF :POINTER)) :VOID


  Parameters:	

        SELF - A pointer to allocated memory


Some of the OpenCV C bindings for its C++ interface that this library binds to, allocate memory for 
their return value, with a new operator. A call to the C++ delete operator must be made for every call 
to new to avoid a memory leak. The DEL function is a wrapper for the C++ delete operator. See the specific 
functions example to see if it is necessary to call DEL or use the macro WITH-ALLOC to free its memory when 
the function goes out of scope.


Example:


LISP-CV> (DEFPARAMETER A (POINT 1 2)) ;A POINT is created

A

LISP-CV> (POINT-X A) ;The x coordinate of A is retrieved

1

LISP-CV> (POINT-Y A) ;The y coordinate of A is retrieved

2

LISP-CV> (DEL A) ; A is deleted with DEL

; No value


LISP-CV> (POINT-X A) ; The memory has been deallocated

0

LISP-CV> (POINT-Y A)

0



EXTRA FUNCTIONS:



DEL-*


Deletes allocated memory


LISP-CV: (DEL (SELF :POINTER)) => :VOID

LISP-CV: (DEL-ANN-MLP (SELF ANN-MLP)) => :VOID

LISP-CV: (DEL-ANN-MLP-TRAIN-PARAMS (SELF ANN-MLP-TRAIN-PARAMS)) => :VOID

LISP-CV: (DEL-CASC-CLASS (SELF CASCADE-CLASSIFIER)) => :VOID

LISP-CV: (DEL-DMATCH (SELF DMATCH)) => :VOID

LISP-CV: (DEL-FEATURE-2D (SELF FEATURE-2D)) => :VOID

LISP-CV: (DEL-KP (SELF KEY-POINT)) => :VOID

LISP-CV: (DEL-MAT (SELF MAT)) => :VOID

LISP-CV: (DEL-MAT-EXPR (SELF MAT-EXPR)) => :VOID

LISP-CV: (DEL-POINT (SELF POINT)) => :VOID

LISP-CV: (DEL-POINT-2D (SELF POINT-2D)) => :VOID

LISP-CV: (DEL-POINT-2F (SELF POINT-2F)) => :VOID

LISP-CV: (DEL-POINT-3D (SELF POINT-3D)) => :VOID

LISP-CV: (DEL-POINT-3F (SELF POINT-3F)) => :VOID

LISP-CV: (DEL-POINT-3I (SELF POINT-3I)) => :VOID

LISP-CV: (DEL-RECT (SELF RECT)) => :VOID

LISP-CV: (DEL-RNG (SELF RNG)) => :VOID

LISP-CV: (DEL-ROT-RECT (SELF ROT-RECT)) => :VOID

LISP-CV: (DEL-SCALAR (SELF SCALAR)) => :VOID

LISP-CV: (DEL-SIZE (SELF SIZE)) => :VOID

LISP-CV: (DEL-SIZE2F (SELF SIZE2F)) => :VOID

LISP-CV: (DEL-STD-STRING (SELF *STRING)) => :VOID

LISP-CV: (DEL-TERM-CRIT (SELF TERM-CRITERIA)) => :VOID

LISP-CV: (DEL-VEC-CHAR (SELF VECTOR-CHAR)) => :VOID

LISP-CV: (DEL-VEC-DBL (SELF VECTOR-DOUBLE)) => :VOID

LISP-CV: (DEL-VEC-DM (SELF VECTOR-DMATCH)) => :VOID

LISP-CV: (DEL-VEC-FLT (SELF VECTOR-FLOAT)) => :VOID

LISP-CV: (DEL-VEC-INT (SELF VECTOR-INT)) => :VOID

LISP-CV: (DEL-VEC-KP (SELF VECTOR-KEY-POINT)) => :VOID

LISP-CV: (DEL-VEC-MAT (SELF VECTOR-MAT)) => :VOID

LISP-CV: (DEL-VEC-POINT (SELF VECTOR-POINT)) => :VOID

LISP-CV: (DEL-VEC-POINT-2F (SELF VECTOR-POINT-2F)) => :VOID

LISP-CV: (DEL-VEC-RECT (SELF VECTOR-RECT)) => :VOID

LISP-CV: (DEL-VEC-UCHAR (SELF VECTOR-UCHAR)) => :VOID

LISP-CV: (DEL-VID-CAP (SELF VIDEO-CAPTURE)) => :VOID

LISP-CV: (DEL-VID-WRITER (SELF VIDEO-WRITER)) => :VOID


  Parameters:	

        SELF - A pointer to a <type> construct


Some of the OpenCV C bindings for its C++ interface that this library binds to, allocate memory for
their return value, with a new operator. The return value of the C functions are a pointer to an OpenCv
class specified by the return value. e.g. Mat* (represented in LISP-CV as MAT) is a pointer to the OpenCV 
Mat class. A call to the C++ delete operator must be made for every call to new to avoid a memory leak. 
The DEL-* functions are wrappers for the C++ delete operator and pass the type of * to the delete operator 
when the DEL-* function is called. The DEL-* function types are below.


Note: Each DEL-* function has a companion WITH-* macro that calls the associated DEL-* function, when
the * goes out of scope, automatically. See <lisp-cv-source directory>/with-macros.lisp for the associated 
WITH-* macro.

The function DEL deletes anything(may not be safe on all implementations)

The function DEL-ANN-MLP deletes a ANN-MLP object.

The function DEL-ANN-MLP-TRAIN-PARAMS deletes a ANN-MLP-TRAIN-PARAMS object.

The function DEL-CASC-CLASS deletes a CASCADE-CLASSIFIER object.

The function DEL-DMATCH deletes a DMATCH object.

The function DEL-FEATURE-2D deletes a FEATURE-2D object.

The function DEL-KP deletes a KEY-POINT object.

The function DEL-MAT deletes a MAT object.

The function DEL-MAT-EXPR deletes a MAT-EXPR object.

The function DEL-POINT deletes a POINT object.

The function DEL-POINT-2D deletes a POINT-2D object.

The function DEL-POINT-2F deletes a POINT-2F object.

The function DEL-POINT-3D deletes a POINT-3D object.

The function DEL-POINT-3F deletes a POINT-3F object.

The function DEL-POINT-3I deletes a POINT-3I object.

The function DEL-RECT deletes a RECT object.

The function DEL-ROT-RECT deletes a ROTATED-RECT object.

The function DEL-RNG deletes a RNG object.

The function DEL-SCALAR deletes a SCALAR object.

The function DEL-SIZE deletes a SIZE object.

The function DEL-SIZE2F deletes a SIZE2F object.

The function DEL-STD-STRING deletes a *STRING object.

The function DEL-TERM-CRIT deletes a TERM-CRITERIA object.

The function DEL-VEC-CHAR deletes a VECTOR-CHAR object.

The function DEL-VEC-DBL deletes a VECTOR-DOUBLE object.

The function DEL-VEC-DM deletes a VECTOR-DMATCH object.

The function DEL-VEC-FLT deletes a VECTOR-FLOAT object.

The function DEL-VEC-INT deletes a VECTOR-INT object.

The function DEL-VEC-KP deletes a VECTOR-KEY-POINT object.

The function DEL-VEC-MAT deletes a VECTOR-MAT object.

The function DEL-VEC-POINT deletes a VECTOR-POINT object.

The function DEL-VEC-POINT-2F deletes a VECTOR-POINT-2F object.

The function DEL-VEC-RECT deletes a VECTOR-RECT object.

The function DEL-VEC-UCHAR deletes a VECTOR-UCHAR object.

The function DEL-VID-CAP deletes a VIDEO-CAPTURE object.

The function DEL-VID-WRITER deletes a VIDEO-WRITER object.


Example:


LISP-CV> (DEFPARAMETER A (POINT 1 2)) ;A POINT is created

A

LISP-CV> (POINT-X A) ;The x coordinate of A is retrieved

1

LISP-CV> (POINT-Y A) ;The y coordinate of A is retrieved

2

LISP-CV> (DEL-POINT A) ; A is deleted with DEL-POINT

; No value


LISP-CV> (POINT-X A) ; The memory has been deallocated

0

LISP-CV> (POINT-Y A)

0

