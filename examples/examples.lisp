;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; examples.lisp
;;;; Documentation and Examples(In process):





BASIC STRUCTURES:



ADD

Adds two matrices.

C++: MatExpr + operator

LISP-CV: (ADD (M1 (:POINTER MAT)) (M2 (:POINTER MAT))) => (:POINTER MAT-EXPR)


    Parameters:	

        M1 - A matrix.

        M2 - A matrix.


The function ADD adds the elements of matrix M1 to the elements of matrix M2 in order. Both 
matrices must have the same number of rows and columns. You may need to coerce the result o-
f ADD, the return value, back to type (:POINTER MAT) with the function (FORCE), (or the 
shorthand version (>>)) to use in other functions. 



(defun add-example ()

  "Matrix M1 and M2 are added together with the 
   function ADD. Matrix M1, M2 and RESULT are t-
   hen printed."

  (let* ((m1-data (alloc :uint '(1 2 3 4 5 6 7 8 9)))
	 (m2-data (alloc :uint '(1 2 3 4 5 6 7 8 9)))
	 (m1 (mat-data 3 3 +32s+ m1-data))
         (m2 (mat-data 3 3 +32s+ m2-data))
         (result (add m1 m2)))
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



ADJUST-ROI

Adjusts a submatrix size and position within the parent matrix.

C++: Mat& Mat::adjustROI(int dtop, int dbottom, int dleft, int dright)

LISP-CV: (ADJUST-ROI (SELF (:POINTER MAT)) (DTOP :INT) (DBOTTOM :INT) (DLEFT :INT) (DRIGHT :INT)) => (:POINTER MAT)

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
      (format t "Frame Size : ~ax~a~%~%" 
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

LISP-CV: (ASSGN-VAL (SELF (:POINTER MAT)) (S (:POINTER SCALAR))) => (:POINTER MAT)

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
	 (image (imread filename 1))
         ;; Create a matrix to fill with a scalar 
         ;; value defined below
         (mat (mat-ones 640 480 +8uc3+))
         (scalar (scalar 255 0 0))
         (result 0))
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)	
    (move-window window-name-1 533 175)
    (move-window window-name-2 984 175)
    ;; Set all elements of MAT to the value defined 
    ;; by SCALAR.
    (assgn-val mat scalar)
    ;; Print IMAGE type. It is important to know this
    ;; Before subtracting a matrix from an Image. You 
    ;; must set the matrix size and type to be the sa-
    ;; me as the image
    (format t "IMAGE type = ~a(+8UC3+)" (mat-type image))
    ;; Subtract MAT from IMAGE
    (setf result (sub image mat))
    ;; Show results
    (imshow window-name-1 image)
    (imshow window-name-2 (>> result))
    (loop while (not (= (wait-key 0) 27)))
    (destroy-all-windows)))


AT

Returns a reference to the specified array element.

C++: uchar* Mat::ptr(int i0=0)

CFFI: mem-aref ptr type &optional (index 0)

CFFI: (setf (mem-aref ptr type &optional (index 0)) new-value) 

LISP-CV: (AT (SELF (:POINTER MAT)) (I :INT) (J :INT) (TYPE :KEYWORD)) 

LISP-CV: (AT (SELF (:POINTER MAT)) (I :INT) (J :INT) (TYPE :KEYWORD)) 


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
  (let ((h (mat-typed 5 5 +64f+)))
    (dotimes (i (rows h))
      (dotimes (j (cols h))
	(setf (at h i j :double) (/ 1.0d0 (+ i j 1)))
	(princ (at h i j :double))
	(princ #\Space))
      (princ #\Newline)))) 



CHANNELS

Returns the number of matrix channels.

C++: int Mat::channels() const

LISP-CV: (CHANNELS (SELF (:POINTER MAT))) => :INT

    Parameters:	

        SELF - A matrix.  

The method returns the number of matrix channels.


(defun channels-example (filename)

  "All BRG images are 3 channel,(BLUE, GREEN, RED). When 
   you convert an image to grayscale you also convert it 
   to a 1 channel image. This example shows that with th-
   e function CVT-COLOR and the function CHANNELS."

  ;; Read image
  (let* ((img (imread filename 1))
	 (window-name-1 "Original image - CHANNELS Example")
         (window-name-2 "Grayscale image - CHANNELS Example"))
    (if (eq 1 (channels img)) 
	(return-from channels-example 
	  (format t "Must load full color image")))
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)	
    (move-window window-name-1 514 175)
    (move-window window-name-2 966 175)
    ;;Print the original number of channels of IMG
    (format t "The number of channels of IMG as is = ~a~%~%" 
	    (channels img))
    (imshow window-name-1 img)
    ;;Convert IMG to a grayscale image
    (cvt-color img img +bgr2gray+)
    ;;Print the number of channels of a grayscale IMG
    (format t "After converting to grayscale the number of channels = ~a~%~%" 
	    (channels img)) 
    (imshow window-name-2 img)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-all-windows)))





COPY-TO

Copies the matrix to another one.


C++: void Mat::copyTo(OutputArray m) const

LISP-CV: (COPY-TO (SELF (:POINTER MAT)) (M (:POINTER MAT))) => :VOID

C++: void Mat::copyTo(OutputArray m, InputArray mask) const

LISP-CV: (COPY-TO (SELF (:POINTER MAT)) (M (:POINTER MAT)) (MASK (:POINTER MAT))) => :VOID


    Parameters:	

        SELF - A matrix.         

        M - Destination matrix. If it does not have a proper size or type before the operation, it 
            is reallocated.

        MASK - Operation mask. Its non-zero elements - which matrix elements need to be copi-
               ed.


The method copies the matrix data to another matrix. Before copying the data, the method invokes:

(CREATE (MAT-SIZE THIS) (MAT-TYPE THIS))

So that the destination matrix is reallocated if needed. While (COPY-TO M M) works flawlessly, the 
function does not handle the case of a partial overlap between the source and the destination matri-
ces. When the operation mask is specified, and the (CREATE) call shown above reallocated the matrix
, the newly allocated matrix is initialized with all zeros before copying the data.


(defun copy-to-example ()
  ;; initialize data for matrices
  (let* ((data (alloc :int '(10 20 30 40)))
         ;; initialize MAT-1 with DATA.
         (mat-1 (mat-data 2 2 +32s+ data))
         ;; initialize MAT-2, a second, 
         ;; identical matrix
         (mat-2 (mat-data 2 2 +32s+ data))
         ;; create empty matrices M-1 and M-2
         (m-1 (mat))
	 (m-2 (mat))
         ;; create a mask for MAT-2 copy operation,
         ;; an identity matrix. its non-zero eleme-
         ;; nts - which matrix elements nee-
         ;; d to be copied.
         (mask (mat-eye 2 2 +8u+)))
    ;; copy data from MAT-1 to M.
    (copy-to mat-1 m-1)
    ;; print contents of MAT-1.
    (format t "MAT-1 =~%~%")
    (dotimes (i (rows mat-1))
      (dotimes (j (cols mat-1))
	(format t "~a" (at mat-1 i j :int))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%")
    ;; print contents of of M-1.
    (format t "M-1 =~%~%")
    (dotimes (i (rows m-1))
      (dotimes (j (cols m-1))
	(format t "~a" (at m-1 i j :int))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%")
    ;; copy data from MAT-2 to M using mask.
    (copy-to mat-2 m-2 mask)
    ;; print contents of MAT-2.
    (format t "MAT-2 =~%~%")
    (dotimes (i (rows mat-2))
      (dotimes (j (cols mat-2))
	(format t "~a" (at mat-2 i j :int))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%")
    ;; print contents of MASK.
    (format t "MASK =~%~%")
    (dotimes (i (rows mask))
      (dotimes (j (cols mask))
	(format t "~a" (at mask i j :uchar))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%")
    ;; print final contents of M-2.
    (format t "M-2 =~%~%")
    (dotimes (i (rows m-2))
      (dotimes (j (cols m-2))
	(format t "~a" (at m-2 i j :int))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")))



DEPTH

Returns the depth of a matrix element.

C++: int Mat::depth() const

LISP-CV: (DEPTH (SELF (:POINTER MAT))) => :INT

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

LCV> (DEFPARAMETER A (MAT-TYPED 3 3 +8SC1+)) ;Initialize 1 channel matrix of 8-bit signed integer type

A

LCV> (MAT-TYPE A) 

1  ;The type of the matrix is 1(+8SC1+) - 1 channel matrix with 8-bit signed integer elements

LCV> (DEPTH A)

1  ;The type of the matrix elements are 1(+8S+) - 8-bit signed integer


LCV> (DEFPARAMETER A (MAT-TYPED 3 3 +8SC3+)) ;Initialize 3 channel matrix of 8-bit signed integer type

A

LCV> (MAT-TYPE A)  

17  ;The type of the matrix is 17(+8SC1+) - 3 channel matrix with 8-bit signed integer elements

LCV> (DEPTH A)

1  ;The type of the matrix elements are 1(+8S+) - 8-bit signed integer



INV

Inverses a matrix.

C++: MatExpr Mat::inv(int method=DECOMP_LU) const

LISP-CV: (INV (SELF (:POINTER MAT)) (METHOD :INT)) => (:POINTER MAT-EXPR) 

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
back to type (:POINTER MAT) with the function (FORCE), (or the shorthand version (>>)) to use in 
other functions.


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
         (numerator (mat-data 3 3 +32f+ data-1))
	 ;Create divisor matrix
         (divisor (mat-data 3 3 +32f+ data-2)) 
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
	 (mat (mat-data 3 3 +8u+ data))
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



LOCATE-ROI

Locates the matrix header within a parent matrix.

C++: void Mat::locateROI(Size& wholeSize, Point& ofs) const

LISP-CV: (LOCATE-ROI (SELF (:POINTER MAT)) (S (:POINTER SIZE)) (P (:POINTER POINT))) => :VOID


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
      (format t "Frame Size : ~ax~a~%~%" 
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



MUL

Finds the product of two matrices.

C++: MatExpr * operator

LISP-CV: (MUL (M1 (:POINTER MAT)) (M2 (:POINTER MAT))) => (:POINTER MAT-EXPR)


    Parameters:	

        M1 - A single float or double float matrix.

        M2 - A single float or double float matrix.


To perform matrix multiplication on two Matrices, the number of columns in the first matrix must be 
the same as the number of rows in the second Matrix. The matrices to be multiplied must also be of 
type Single Float(+32F+) or Double Float(+64f+). You may need to coerce the result of MUL, the retu-
rn value, back to type (:POINTER MAT) with the function (FORCE), (or the shorthand version (>>)) to 
use in other functions. 



(defun mul-example ()

  "In this example, MUL is used to find 
   the product of two matrices. The con-
   tent of the first matrix(M1), the se-
   cond matrix(M2) and result(RESULT) a-
   re printed."

  (let* ((data (alloc :float '(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0 
			       6.0f0 7.0f0 8.0f0 9.0f0)))
	 (m1 (mat-data 3 3 +32f+ data))
         (m2 (mat-data 3 3 +32f+ data))
         (result (mul m1 m2)))
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
    (free data)))



PROMOTE 

Coverts a MAT to MAT-EXPR

LISP-CV: (PROMOTE (SELF (:POINTER MAT))) => (:POINTER MAT-EXPR)

LISP-CV: (<< (SELF (:POINTER MAT))) => (:POINTER MAT-EXPR)

The function PROMOTE converts a functions return from (:POINTER MAT) to (:POINTER MAT-EXPR). This i-
s useful if you would like to do math computation on a matrix with a (:POINTER MAT) type using a su-
per fast Matrix Expressions(MAT-EXPR) function. Some Matrix Expressions functions will only accept 
a (:POINTER MAT-EXPR) type as input and that is what makes this function necessary.  You can then c-
onvert back to (:POINTER MAT) with the function FORCE to use the result in a function that only acc-
epts a MAT as input i.e. IMSHOW. The function << is an identical shorthand version of the PROMOTE f-
unction supplied for ease of use. 

   Parameters:	

        SELF - A MAT pointer.


Example:


(defun promote-example ()

  "In this example a matrix filled with ones(MAT) is 
   created. PROMOTE or actually the shorthand versio-
   n of the function PROMOTE << is then used to coer-
   ce MAT to a (:POINTER MAT-EXPR) type so it can th-
   en be multiplied by the scalar S with the functio-
   n SCALE. This is necessary since SCALE only accep-
   ts (:POINTER MAT-EXPR) types as it's input. The o-
   utput of SCALE(OUT) is then coerced back to (:POI-
   NTER MAT) with the function FORCE for the opposit-
   e reason so it can then be shown in a window with 
   IMSHOW."

  (let* ((mat (mat-ones 3 3 +8u+))
         (s 5.0d0)
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

LISP-CV: (PTR (SELF (:POINTER MAT)) &OPTIONAL ((I0 :INT) 0)) => :POINTER

    Parameters:	

        SELF - A matrix.

        i0 - A 0-based row index.


This function returns a pointer to the specified matrix row.


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
    (destroy-all-windows)))


RESHAPE

Changes the shape and/or the number of channels of a 2D matrix without copying the data.

C++: Mat Mat::reshape(int cn, int rows=0) const

LISP-CV: (RESHAPE (SELF (:POINTER MAT)) (CN :INT) (ROWS :INT)) => (:POINTER MAT)


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
         (mat (mat-data 3 3 +8u+ data))
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



SIZE

SIZE constructor

C++: Size_(_Tp _width, _Tp _height);
     typedef Size_<int> Size2i;
     typedef Size2i Size;

LISP-CV: (SIZE (WIDTH :INT) (HEIGHT :INT)) => (:POINTER SIZE)

C++: _Tp width, height

LISP-CV: (WIDTH (SELF (:POINTER SIZE))) => :INT

C++: _Tp width, height




    Parameters:	

        SELF - A SIZE construct.

        WIDTH - The width of SIZE.
        
        HEIGHT - The height of SIZE.


The function SIZE creates and also retrieves size values and stores the values in SIZE construct.

The function WIDTH Finds the width of a SIZE construct.

The function HEIGHT Finds the height of a SIZE construct.


The function SIZE contains the functionality of both the OpenCV class Size_ and the OpenCV MAT class 
method size. It can return a pointer to an uninitialized SIZE construct, an initialized SIZE construct 
holding (WIDTH, HEIGHT) values and also determines the SIZE value of any MAT construct passed to it, When 
returning a MAT size the columns are listed first and  the rows are listed second(COLS, ROWS). For a tiny
bit faster matrix size accessor choose the MAT-SIZE Dfunction.


(defun size-example ()
       
       "In the code below the (COLS, ROWS) values of MAT are 
   accessed and stored in a SIZE construct. Their value-
   s are accessed with the WIDTH and HEIGHT functions. 
   Then an uninitialized and an initialized SIZE constr-
   uct are created. Their values are also printed."
   
   (let* ((mat (mat-value 5 5 +8u+ (scalar 100 100 100)))
	  (mat-size (size mat))
          (size-un-init (size))
          (size (size 640d0 480d0)))
     ;;The '?' is a macro for CFFI:MEM-AREF
     (format t "MAT (COLS,ROWS) = (~a ~a)~%" 
	     (? mat-size :int)
	     (? mat-size :int 1))
	     (format t "Pointer to an uninitialized SIZE construct:~a
               ~%" size-un-init) 
	       (format t "Width of SIZE = ~a~%" (width size))
	       (format t "Height of SIZE = ~a" (height size))))



OPERATIONS ON ARRAYS:


%ABS

Calculates an absolute value of each matrix element.

C++: MatExpr abs(const Mat& m)

LISP-CV: (%ABS (M (:POINTER MAT))) => (:POINTER MAT-EXPR)

    Parameters:	

        M - matrix.

%ABS is a meta-function that is expanded to one of (ABS-DIFF) or (CONVERT-SCALE-ABS) forms:

        (DEFPARAMETER C (%ABS (>> (SUB A B)))) is equivalent to (ABSDIFF A B C)

        (DEFPARAMETER C (%ABS A)) is equivalent to (ABSDIFF A (SCALAR-ALL 0) C)


The output matrix has the same size and the same type as the input one except for the last case, 
where C is (EQ DEPTH +8U+). 


Note: The function is named %ABS instead of ABS because, ABS is the name of a Common Lisp function.


See also:

Matrix Expressions(MAT-EXPR), (ABS-DIFF), (CONVERT-SCALE-ABS)



(defun %abs-example (&optional 
		      (camera-index *camera-index*))
  ;;Set Camera feed to CAP.
  (with-capture (cap (video-capture camera-index))
    (let* ((window-name "~ABS Example")
           ;;Allocate data and create a 2x2 matrix.
	   (data (alloc :float '(4f0 -7f0 2f0 -3f0)))
	   (mat (mat-data 2 2 +32f+ data))
           ;;Find absolute value of all MAT elements.
           (abs-val (>> (%abs mat))))
      (if (not (cap-is-open cap)) 
	  (return-from %abs-example 
	    (format t "Cannot open the video camera")))
      ;;Print MAT's absolute value.
      (dotimes (i (cols abs-val))
	(dotimes (j (rows abs-val))
	  (princ (at abs-val i j :float))
	  (princ #\Space))
	(princ #\Newline))
      (named-window window-name +window-normal+)
      (move-window window-name 759 175)
      (do* ((frame 0)
	    (clone 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
        ;;Read camera feed, set to FRAME.
	(setf frame (mat))
	(cap-read cap frame)
	;;Clone FRAME(CLONE).
	(setf clone (clone frame))
        ;;Assign each element of CLONE 
        ;;a 3 channel scalar value.
	(assgn-val clone (scalar 255 0 255 ))
	;;Subtract CLONE from frame to remove all color 
	;;values but green. Then calculate the absolute 
	;;value of FRAME before showing output in a win-
	;;dow. Negative matrix elements in FRAME would 
	;;cause an unhandled memory fault error.
	(setf abs-val (%abs (>> (sub frame clone))))
	(imshow window-name (>> abs-val))
        ;;Clean up used memory.
	(del-mat-expr abs-val)
	(del-mat clone))
      (destroy-window window-name))))



ABSDIFF

Calculates the per-element absolute difference between two arrays or between an array and a scalar.

C++: void absdiff(InputArray src1, InputArray src2, OutputArray dst)

LISP-CV: (ABSDIFF (SRC1 (:POINTER MAT)) (SRC2 (:POINTER MAT)) (DEST (:POINTER MAT)))

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
    (let ((scalar (mat-value 1 1 +64f+ (scalar 128 128 128)))
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


BITWISE-OR

Calculates the per-element bit-wise disjunction of two arrays.

C++: void bitwise_or(InputArray src1, InputArray src2, OutputArray dst, InputArray mask=noArray())

LISP-CV: (BITWISE-OR (SRC1 (:POINTER MAT)) (SRC2 (:POINTER MAT)) (DEST (:POINTER MAT)) (MASK (:POINTER MAT))) => :VOID


    Parameters:	

        SRC1 - A matrix.

        SRC2 - A matrix.

        DEST - Output matrix that has the same size and type as the input matrices.

        MASK - Optional operation mask, 8-bit single channel matrix, that 
               specifies elements of the output matrix to be changed.


The function calculates the per-element bit-wise logical disjunction for two matrices when SRC1 and SRC2 
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
	 (dest (mat-typed (rows image-1) (cols image-1) +8uc3+ ))
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
    (destroy-all-windows)))



BITWISE-XOR

Calculates the per-element bit-wise “exclusive or” operation on two arrays.

C++: void bitwise_xor(InputArray src1, InputArray src2, OutputArray dst, InputArray mask=noArray())

LISP-CV: (BITWISE-XOR (SRC1 (:POINTER MAT)) (SRC2 (:POINTER MAT)) (DEST (:POINTER MAT)) (MASK (:POINTER MAT))) => :VOID

    Parameters:	

        SRC1 - A matrix.

        SRC2 - A matrix.

        DEST - Output matrix that has the same size and type as the input matrices.

        MASK - Optional operation mask, 8-bit single channel matrix, that 
               specifies elements of the output matrix to be changed.


The function calculates the per-element bit-wise logical “exclusive-or” operation for two matrices 
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
	 (dest (mat-typed (rows image-1) (cols image-1) +8uc3+ ))
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
    (destroy-all-windows)))


CONVERT-SCALE-ABS

Scales, calculates absolute values, and converts the result to 8-bit.

C++: void convertScaleAbs(InputArray src, OutputArray dst, double alpha=1, double beta=0)

LISP-CV: (CONVERT-SCALE-ABS  (SRC (:POINTER MAT)) (DEST (:POINTER MAT)) &OPTIONAL ((ALPHA :INT) 1.0D0) ((BETA :INT) 0.0D0)) => :VOID

    Parameters:	

        SRC - input array.

        DEST - output array.

        ALPHA - optional scale factor.

        BETA - optional delta added to the scaled values.

On each element of the input array, the function CONVERT-SCALE-ABS performs three operations sequentially: 
scaling, taking an absolute value, conversion to an unsigned 8-bit type. In case of multi-channel arrays, 
the function processes each channel independently. 


(defun convert-scale-abs-example (&optional (camera-index *camera-index*) 
				    (width *default-width*)
				    (height *default-height*))

  (with-capture (cap (video-capture camera-index))    
    (let ((window-name "CONVERT-SCALE-ABS Example")
	  ;;Create matrix so we can scale it, caclculate its 
	  ;;absolute value and convert it to 8-bit with fun-
	  ;;ction CONVERT-SCALE-ABS
	  (mat (mat-ones 6 4 +32f+)))
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-height+ height)
      (named-window window-name +window-normal+)
       (move-window window-name 759 175)
      ;;Print original type of MAT
      (format t "MAT type before conversion = ~a(or +32f+)~%~%" (mat-type mat))
      ;;Print original MAT before conversion
      (format t "Printing MAT before the conversion~%~%")
      (dotimes (i (rows mat))
	(dotimes (j (cols mat))
	  (format t "~a" (at mat i j :float))
	  (princ #\Space))
	(princ #\Newline))
      ;;Run CONVERT-SCALE-ABS
      (convert-scale-abs mat mat 2d0 5d0)
      ;;Print converted MAT type which is 8-bit
      (format t "~%~%MAT type after conversion = ~a(or +8u+)~%~%" (mat-type mat))
      ;;Print converted MAT
      (format t "~%~%Printing MAT after the conversion~%~%")
      (dotimes (i (rows mat))
	(dotimes (j (cols mat))
	  (format t "~a" (at mat i j :float))
	  (princ #\Space))
	(princ #\Newline))
      (do ((frame 0))
	  ((plusp (wait-key *millis-per-frame*)) 
	   (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
        ;;Run CONVERT-SCALE-ABS on each frame of 
        ;;camera output just to see what happens
	(convert-scale-abs frame frame 2d0 5d0)
        (imshow window-name frame))
      (destroy-window window-name))))


DET

Returns the determinant of a square floating-point matrix.

C++: double determinant(InputArray mtx)

LISP-CV: (DET (MTX (:POINTER MAT))) => :DOUBLE

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
         (zero-det-mat (mat-data 3 3 +32f+ data-1))
	;Create matrix with non-zero determinant.
         (mat (mat-data 3 3 +32f+ data-2))      
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


FLIP

Flips a 2D array around vertical, horizontal, or both axes.

C++: void flip(InputArray src, OutputArray dst, int flipCode)

LISP-CV: (FLIP (SRC (:POINTER MAT)) (DEST (:POINTER MAT)) (FLIP-CODE :INT)) => :VOID

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
	   (mat (mat-data 3 3 +8u+ data))
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



INVERT

Finds the inverse or pseudo-inverse of a matrix.

C++: double invert(InputArray src, OutputArray dst, int flags=DECOMP_LU)

LISP-CV: (INVERT (SRC (:POINTER MAT)) (DEST (:POINTER MAT)) &OPTIONAL ((FLAGS :INT) +DECOMP-LU+))) => :DOUBLE

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
	   (mat (mat-data 2 2 +32f+ data))
           ;;Create a destination matrix 
           ;;the same size/type as MAT
	   (dest-1 (mat-typed 2 2 +32f+))
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
      (imshow (aref window-name-arr 2) identity-mat)
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

LISP-CV: (MEAN (SRC (:POINTER MAT)) &OPTIONAL ((MASK (:POINTER (MAT)) (MAT)))) => (:POINTER SCALAR)


    Parameters:	

        SRC - Input array that should have from 1 to 4 channels so that the result can be stored in SCALAR.

        MASK - Optional operation mask.

The function mean calculates the mean value M of array elements, independently for each channel, and 
returns it. When all the mask elements are 0’s, the function returns (SCALAR-ALL 0) .

See also:

(COUNT-NON-ZERO), (MEAN-STD-DEV), (NORM), (MIN-MAX-LOC)


Example:


(defun mean-example (&optional (camera-index *camera-index*) 
		       (width *default-width*)
		       (height *default-height*))

  "Position the rectangle in the window by moving the trackbar 
   sliders. The rectangle gets its color from the averaging of 
   the pixels in the region of the rectangle. This averaging is
   calculated by the function MEAN. For example if you position 
   the rectangle over some thing red, the rectangle will turn a 
   shade of red. The rectangle starts at 0,0 X,Y coordinates."

  (with-capture (cap (video-capture camera-index))
    (let* ((window-name "IMG - MEAN Example")
           (n 10)
           ;;Initialize the rectangle location/
           ;;dimension variables
	   (rect-x (alloc :int '(0)))
	   (rect-y (alloc :int '(0)))
	   (rect-width (alloc :int (list (round (/ width n)))))
	   (rect-height (alloc :int (list (round (/ height n))))))      
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-height+ height) 
      ;;Create fullscreen window
      (named-window window-name +window-normal+)
      (set-window-property window-name +wnd-prop-fullscreen+ 
			   +window-fullscreen+)
      (move-window window-name 624 100)
      ;;Initialize other variables
      (do* ((frame 0)
            (color 0)
            (roi 0)
	    (img 0)
	    (point-1 0)
	    (point-2 0))
	   ((plusp (wait-key *millis-per-frame*)) nil)
        (setf frame (mat))
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
	    (setf (mem-ref rect-x :int) 1))
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
	;;Set region of interest of FRAME to the rectangle 
	;;location/dimensions we specified
	(setf roi (rect (mem-ref rect-x :int) (mem-ref rect-y :int)
			(mem-ref rect-width :int) (mem-ref rect-height :int)))
        ;;Create an empty matrix
	(setf img (mat))
        ;;Make a copy of FRAME, IMG, to use 
        ;;for the fullscreen camera output
        (copy-to frame img)
        ;;Set region of interest of FRAME to ROI. This region of 
        ;;interest is the where we find the mean of the pixels. 
	(setf frame (roi frame roi))
        ;;Set position parameters of the RECTANGLE we will create, 
        ;;that will be the color of the mean of the pixels in FRAME, 
        ;;to that of the position of the region of interest of FRAME
	(setf point-1 (point (mem-ref rect-x :int) 
                             (mem-ref rect-y :int)))
	(setf point-2 (point (+ (mem-ref rect-x :int) 
                                (mem-ref rect-width :int)) 
			     (+ (mem-ref rect-y :int) 
                                (mem-ref rect-height :int))))
        ;;Find mean of FRAME and set to 
        ;;COLOR parameter of RECTANGLE
        (setf color (mean frame)) 
        ;;Create a rectangle the color of 
        ;;the mean of the pixels it covers
        (rectangle img point-1 point-2 color +filled+ 4 0)
    	(imshow window-name img)
        ;;Clean up used matrices
	(del-mat img)
        (del-mat frame))
      ;;Free memory as program ends
      (free rect-x)
      (free rect-y)
      (free rect-width)
      (free rect-height)
      (destroy-all-windows))))


%MAX

Calculates per-element maximum of two arrays.

C++: void max(InputArray src1, InputArray src2, OutputArray dst)

LISP-CV: (%MAX (SRC1 (:POINTER MAT)) (SRC2 (:POINTER MAT)) (DEST (:POINTER MAT))) => :VOID


    Parameters:	

        SRC1 - First input array.

        SRC2 - Second input array of the same size and type as SRC1.

        DEST - Output array of the same size and type as SRC1.


The function %MAX calculates the per-element maximum of two arrays. When the input array is multi-channel, 
each channel is compared with value independently.

Note: The function is named %MAX instead of MAX because, MAX is the name of a Common Lisp function.


See also:

(%MIN), (COMPARE), (INRANGE), (MIN-MAX-LOC), Matrix Expressions(MAT-EXPR)


(defun %max-example (&optional (camera-index 0)
		       (width *default-width*)
		       (height *default-height*))

  "Look at the first window and notice that whatever is black
   in the first window has a beautiful glow in the third wind-
   ow. You can change the effect by altering the color of the 
   matrix MAT-3 in the middle window with the trackbar .The t-
   rackbar changes the scalar value the ASSGN-VAL function us-
   es to decide what to set each element of MAT-3 to."

  (with-capture (cap (video-capture camera-index))   
       ;Create two matrices: MAT-1 and MAT-2(used to show how %MAX works)
    (let* ((mat-1 (mat-data 3 3 +32s+ (alloc :int '(1 2 3 4 5 6 7 8 9))))
	   (mat-2 (mat-data 3 3 +32s+ (alloc :int '(9 8 7 6 5 4 3 2 1))))
           ;Create destination matrix of same size and type: DEST
           (dest (mat-typed 3 3 +32s+))
           ;Create 3 matrices used to hold 
           ;data we use later in the example
           (mat-3 (mat-typed height width +8u+))
           (mat-4 (mat-typed height width +8u+))
           (mat-5 (mat-typed height width +8u+))
           ;Allocate :int pointer for trackbar to change
           (val (alloc :int '(128)))
	   (window-name-1 "MAT-3 after THRESHOLD - %MAX-Example")
	   (window-name-2 "MAT-5 after ASSGN-VAL - %MAX-Example")
	   (window-name-3 "MAT-4 after %MAX - %MAX-Example")) 
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
      ;Find per element maximum of 
      ;MAT-1 and MAT-2, set to DEST
      (%max mat-1 mat-2 dest)
      ;Print DEST
      (format t "Per element maximum of MAT-1 and  MAT-2:~%~%")
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
        ;Find the maximum of each element 
        ;of mat-4 AND mat-5, set to mat-4
        (%max mat-4 mat-5 mat-4)
        ;Show mat-3, mat-5 and mat-4 in windows
	(imshow window-name-1 mat-3)
	(imshow window-name-2 mat-5)
	(imshow window-name-3 mat-4)) 
      (destroy-all-windows))))



%MIN

Calculates per-element minimum of two arrays.

C++: void min(InputArray src1, InputArray src2, OutputArray dst)

LISP-CV: (%MIN (SRC1 (:POINTER MAT)) (SRC2 (:POINTER MAT)) (DEST (:POINTER MAT))) => :VOID


    Parameters:	

        SRC1 - First input array.

        SRC2 - Second input array of the same size and type as SRC1.

        DEST - Output array of the same size and type as SRC1.


The function %MIN calculates the per-element minimum of two arrays. When the input array is multi-channel, 
each channel is compared with value independently.

Note: The function is named %MIN instead of MIN because, MIN is the name of a Common Lisp function.


See also:

(%MAX), (COMPARE), (INRANGE), (MIN-MAX-LOC), Matrix Expressions(MAT-EXPR)


(defun %min-example (&optional (camera-index 0)
		       (width *default-width*)
		       (height *default-height*))

  "Look at the first window and notice that whatever is black
   in the first window has a beautiful glow in the third wind-
   ow. You can change the effect by altering the color of the 
   matrix MAT-3 in the middle window with the trackbar .The t-
   rackbar changes the scalar value the ASSGN-VAL function us-
   es to decide what to set each element of MAT-3 to."

  (with-capture (cap (video-capture camera-index))   
       ;Create two matrices: MAT-1 and MAT-2(used to show how %MIN works)
    (let* ((mat-1 (mat-data 3 3 +32s+ (alloc :int '(1 2 3 4 5 6 7 8 9))))
	   (mat-2 (mat-data 3 3 +32s+ (alloc :int '(9 8 7 6 5 4 3 2 1))))
           ;Create destination matrix of same size and type: DEST
           (dest (mat-typed 3 3 +32s+))
           ;Create 3 matrices used to hold 
           ;data we use later in the example
           (mat-3 (mat-typed height width +8u+))
           (mat-4 (mat-typed height width +8u+))
           (mat-5 (mat-typed height width +8u+))
           ;Allocate :int pointer for trackbar to change
           (val (alloc :int '(128)))
	   (window-name-1 "MAT-3 after THRESHOLD - %MIN-Example")
	   (window-name-2 "MAT-5 after ASSGN-VAL - %MIN-Example")
	   (window-name-3 "MAT-4 after %MIN - %MIN-Example")) 
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
      (%min mat-1 mat-2 dest)
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
        (%min mat-4 mat-5 mat-4)
        ;Show mat-3, mat-5 and mat-4 in windows
	(imshow window-name-1 mat-3)
	(imshow window-name-2 mat-5)
	(imshow window-name-3 mat-4)) 
      (destroy-all-windows))))



MULTIPLY

Calculates the per-element scaled product of two arrays.

C++: void multiply(InputArray src1, InputArray src2, OutputArray dst, double scale=1, int dtype=-1 )

LISP-CV: (MULTIPLY (SRC1 (:POINTER MAT)) (SRC2 (:POINTER MAT)) (DEST (:POINTER MAT)) (SCALE :DOUBLE) (DTYPE :INT)) => :VOID


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
	 (int-mat-1 (mat-data 3 3 +32s+ int-data))
         (int-mat-2 (mat-data 3 3 +32s+ int-data))
         (dest1 (mat))
	 (double-mat-1 (mat-data 3 3 +64f+ double-data))
         (double-mat-2 (mat-data 3 3 +64f+ double-data))
         (dest2 (mat))
	 (uchar-mat-1 (mat-data 3 3 +8u+ uchar-data))
         (uchar-mat-2 (mat-data 3 3 +8u+ uchar-data))
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
      (dotimes (j (cols dest1))
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


RANDU

Generates a single uniformly-distributed random number or an array of random numbers.

C++: void randu(InputOutputArray dst, InputArray low, InputArray high)

LISP-CV: (RANDU (DEST (:POINTER MAT)) (LOW (:POINTER SCALAR)) (HIGH (:POINTER SCALAR))) => :VOID


    Parameters:	

        DEST - output array of random numbers; the array must be pre-allocated.

        LOW - inclusive lower boundary of the generated random numbers.

        HIGH - exclusive upper boundary of the generated random numbers.


This function fills the matrix dst with uniformly-distributed random numbers from the specified range.

See also:

(RNG), (RANDN), (THE-RNG)


(defun randu-example ()
  (let* ((data (alloc :float '(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0 
			       6.0f0 7.0f0 8.0f0 9.0f0)))
	 (m (mat-data 3 3 +32f+ data)))
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


SCALE-ADD

Calculates the sum of a scaled array and another array.

C++: void scaleAdd(InputArray src1, double alpha, InputArray src2, OutputArray dst)

LISP-CV: (SCALE-ADD (SRC1 (:POINTER MAT)) (ALPHA :DOUBLE) (SRC2 (:POINTER MAT)) (DEST (:POINTER MAT))) => :VOID


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
	 (dest (mat-typed (rows image-1) (cols image-1) +8uc3+))
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


DRAWING FUNCTIONS:


GET-TEXT-SIZE


Calculates the width and height of a text string.


C++: Size getTextSize(const string& text, int fontFace, double fontScale, int thickness, int* baseLine)

LISP-CV:  (GET-TEXT-SIZE (TEXT (:POINTER STRING*)) (FONT-FACE :INT) (FONT-SCALE :DOUBLE) (THICKNESS :INT) (BASE-LINE :POINTER)) 
           => (:POINTER SIZE)


    Parameters:	

        TEXT - Input text string.

        FONT-FACE - Font to use. See (PUT-TEXT) for details.

        FONT-SCALE - Font scale. See (PUT-TEXT) for details.

        THICKNESS - Thickness of lines used to render the text. See (PUT-TEXT) for details.

        BASE-LINE - Output parameter - y-coordinate of the baseline relative to the bottom-most text 
                    point.


The function GET-TEXT-SIZE calculates and returns the size of a box that contains the specified text. 
That is, the following code renders some text, the tight box surrounding it, and the baseline:


(defun get-text-example ()
  (let* ((window-name "GET-TEXT Example")
	 (text "Funny text inside the box")
	 (font-face +font-hershey-script-simplex+)
	 (font-scale 2d0)
	 (thickness 3)
	 (img (mat-value 600 800 +8uc3+ (scalar-all 0)))
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


LISP-CV: (BGR B G R) => (:POINTER SCALAR)


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

LISP-CV: (ELLIPSE (IMG (:POINTER MAT)) (CENTER (:POINTER POINT)) (AXES (:POINTER SIZE)) (ANGLE :DOUBLE) (START-ANGLE :DOUBLE)
             (END-ANGEL :DOUBLE) (COLOR (:POINTER SCALAR)) (THICKNESS :INT) (LINE-TYPE :INT) (SHIFT :INT)) => :VOID

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
arc, or a filled ellipse sector. A piecewise-linear curve is used to approximate the elliptic arc b-
oundary. If you need more control of the ellipse rendering, you can retrieve the curve using the fu-
nction (ELLIPSE-2-POLY) and then render it with the function (POLYLINES) or fill it with the functi-
on (FILL-POLY) . If you use the first variant of the function and want to draw the whole ellipse, n-
ot an arc, pass (EQ START-ANGLE 0) and (END-ANGLE 360).


Example:

(defun random-color (rng &optional (icolor 0))
  (setf icolor rng)
  (return-from random-color (scalar (uniform rng 0 255) 
				    (uniform rng 0 255) (uniform rng 0 255))))



(defun ellipse-example-1 ()

  (let ((window-name "ELLIPSE Example 1")
					;Initialize random number generator
	(rng (rng #xFFFFFFFF))
	;;Declare MAT
	(mat 0))
    ;;Create a window
    (named-window window-name +window-normal+)
    ;; Set window to fullscreen
    (set-window-property window-name +wnd-prop-fullscreen+ 
			 +window-fullscreen+)
    (if (equal 1.0d0 (get-window-property window-name +wnd-prop-fullscreen+)) 
	(set-window-property window-name +wnd-prop-aspectratio+ 
			     +window-freeratio+))
    ;;Initialize line type and thickness variables
    (do* ((line-type +aa+)
	  (thickness -1)
	  ;;Initialize value for the ellipse center
	  (center (point 240 300))
	  ;;Initialize value equaling to half of the ellipse main 
          ;;axes
	  (axes 0)
	  ;;Initialize value for the ellipse rotation angle
          (angle 0))
	 ((plusp (wait-key 1)) 
	  (format t "Key is pressed by user"))
      ;;Create a black background using MAT-ZEROS
      (setf mat (mat-zeros 640 480 +8uc3+))
      (dotimes (n 1)
	;;Set the ellipse axes to random values
	(setf axes (size (coerce (uniform rng 0 240) 'double-float) 
			 (coerce (uniform rng 0 240) 'double-float)))
        ;;Set the ellipse angle to random values
	(setf angle (coerce (uniform rng 0 180) 'double-float))
	;;Draw multiple ellipses with varied parameters
	(ellipse10 mat center axes angle 0.0d0 (coerce (uniform rng 0 360) 'double-float)
		   (random-color rng) (uniform rng thickness 9) line-type))
      (sleep .14)
      ;; Show output
      (imshow window-name mat)
      (del-mat mat))
    (destroy-window window-name)))





LINE

Draws a line segment connecting two points.

C++: void line(Mat& img, Point pt1, Point pt2, const Scalar& color, int thickness=1, int lineType=8, int shift=0)

LISP-CV: (LINE (IMG (:POINTER MAT)) (PT1 (:POINTER POINT)) (PT2 (:POINTER POINT)) (COLOR (:POINTER SCALAR)) &OPTIONAL 
((THICKNESS :INT) 1) ((LINE-TYPE :INT) 8) ((SHIFT :INT) 0)) => :VOID

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

LISP-CV: (PUT-TEXT (IMG (:POINTER MAT)) (TEXT (:POINTER STRING*)) (ORG (:POINTER POINT)) (FONT-FACE :INT) (FONT-SCALE :DOUBLE)  
             (COLOR (:POINTER SCALAR)) (THICKNESS :INT) (LINE-TYPE :INT) (BOTTOM-LEFT-ORIGN :BOOLEAN)) => :VOID
 
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

LISP-CV: (RECTANGLE (IMG (:POINTER MAT)) (PT1 (:POINTER POINT)) (PT2 (:POINTER POINT)) (COLOR (:POINTER SCALAR)) &OPTIONAL 
             ((THICKNESS :INT) 1) ((LINE-TYPE :INT) 8) ((SHIFT :INT) 0)) => :VOID


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
				    (uniform rng 0 255) (uniform rng 0 255))))

(defun rectangle-example ()

  (let* ((window-name "RECTANGLE Example")
	 (window-width 640)
	 (window-height 480)
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
    ;; Set line type and thickness
    (do* ((line-type +aa+)
          (thickness (uniform rng -3 10))
	  ;; Initialize rectangle position variables
          (x-1 (/ (* window-width -1) 2))
	  (x-2 (/ (* window-width 3) 2))
          (y-1 (/ (* window-width -1) 2))
          (y-2 (/ (* window-width 3) 2))
          (pt1 0)
	  (pt2 0))
	 ((plusp (wait-key 1)) 
	  (format t "Key is pressed by user"))
      ;; Print randomly colored rectangles to screen
      (setf pt1 (point (uniform rng x-1 x-2) (uniform rng y-1 y-2)))
      (setf pt2 (point (uniform rng x-1 x-2) (uniform rng y-1 y-2)))
      (rectangle mat pt1 pt2 (random-color rng) thickness line-type)
      ;; Show result in window
      (imshow window-name mat))
    (destroy-window window-name)))




RGB

A macro for SCALAR organized as RGB(RED, GREEN, BLUE) color values.

LISP-CV: (RGB R G B) => (:POINTER SCALAR)

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


READING AND WRITING IMAGES AND VIDEO:




IMWRITE

Saves an image to a specified file.

C++: bool imwrite(const string& filename, InputArray img, const vector<int>& params=vector<int>() )

LISP-CV: (IMWRITE (FILENAME :STRING) (IMG (:POINTER MAT)) ((PARAMS (:POINTER VECTOR-INT)) (VECTOR-INT))) => :BOOLEAN

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


(defun imwrite-example (filename-1 out-file)
       ;;Read in image
  (let* ((image (imread filename-1 1))
	 (window-name-1 "Original image - IMWRITE Example")
         (window-name-2 "Flipped image - IMWRITE Example"))
    (if (empty image) 
	(return-from imwrite-example 
	  (format t "Image not loaded")))
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)
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
    (imwrite out-file image (vector-int 
			     (list +imwrite-jpeg-quality+ 99)))
    (loop while (not (= (wait-key 0) 27)))
    (destroy-all-windows)))



VIDEO-CAPTURE

VideoCapture constructors.

C++: VideoCapture::VideoCapture()

LISP-CV: (VIDEO-CAPTURE) => (:POINTER VIDEO-CAPTURE)

C++: VideoCapture::VideoCapture(int device)

LISP-CV: (VIDEO-CAPTURE &OPTIONAL (SRC :INT)) => (:POINTER VIDEO-CAPTURE)

C++: VideoCapture::VideoCapture(const string& filename)

LISP-CV: (VIDEO-CAPTURE &OPTIONAL (SRC :POINTER STRING*)) => (:POINTER VIDEO-CAPTURE)


  Parameters:	

        SRC - 

             A device: ID of the opened video capturing device (i.e. a camera index). If there is 
                       a single camera connected, just pass 0.     

             A file name: Name of the opened video file (eg. video.avi) or image sequence (eg. 
                          img_%02d.jpg, which will read samples like img_00.jpg, img_01.jpg, 
                          img_02.jpg, ...)

             NIL: Creates an uninitialized (:POINTER VIDEO-CAPTURE)
        


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
	  (imshow window-name-2 video-file))
	(destroy-all-windows)))))



IMAGE FILTERING:


COPY-MAKE-BORDER

Forms a border around an image.

C++: void copyMakeBorder(InputArray src, OutputArray dst, int top, int bottom, int left, int right, int borderType, 
     const Scalar& value=Scalar() )

LISP-CV: (COPY-MAKE-BORDER (SRC (:POINTER MAT)) (DEST (:POINTER MAT)) (TOP :INT) (BOTTOM :INT) (LEFT :INT) (RIGHT :INT) 
         (BORDER-TYPE :INT) (VALUE (:POINTER SCALAR))) => :VOID

    Parameters:	

        SRC - Source image.

        DEST - Destination image of the same type as SRC and the size:

                (+ (COLS SRC) LEFT RIGHT), (+ (ROWS SRC) TOP BOTTOM).

        TOP -

        BOTTOM -

        LEFT -

        RIGHT - Parameter specifying how many pixels in each direction from the source image rectangle 
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


(defun copy-make-border-example (&optional (camera-index *camera-index*)
				   (width *default-width*)
				   (height *default-height*))
  "Forms a border around FRAME"

  (with-capture (cap (video-capture camera-index))
    (let* ((window-name-1 "FRAME - COPY-MAKE-BORDER Example")
	   (window-name-2 "BORDERED - COPY-MAKE-BORDER Example")
	   (border 100)
           ;Create a matrix big enough to accommodate 
           ;a 100 pixel border on all sides
           (border-width (+ (* 2  border) width))
           (border-height (+ (* 2  border) height))
	   (rgb (mat-typed border-height border-width +8uc3+)))
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-height+ height)
      (named-window window-name-1 +window-normal+)
      (named-window window-name-2 +window-normal+)
      (move-window window-name-1 533 175)
      (move-window window-name-2 984 175)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
        ;Make a border around FRAME set to BORDERED
	(copy-make-border frame rgb border border border border  
			  +border-replicate+ (scalar-all 75))
	(imshow window-name-1 gray)
	(imshow window-name-2 rgb)) 
      (destroy-all-windows))))



PYR-DOWN

Blurs an image and downsamples it.

C++: void pyrDown(InputArray src, OutputArray dst, const Size& dstsize=Size(), int borderType=BORDER_DEFAULT )

LISP-CV: (PYR-DOWN (SRC (:POINTER MAT)) (DEST (:POINTER MAT)) &OPTIONAL ((DSTSIZE (:POINTER SIZE)) (SIZE)) 
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
	   (out (mat-typed (/ in-height 2) (/ in-width 2) +8uc3+)))
      ;;Make sure input image is divisible by two."
      (assert (and (equal (mod in-height 2) 0)
		   (equal (mod in-width 2) 0))
	      (in-height in-width)
	      "~S or ~S are not divisible by two" in-width in-height)
      ;;Blur and downsample image
      (pyr-down in out)
      out))

  (defun main (filename)
    
    (let* ((img-1 (imread filename 1))
	   (img-2 0)
	   (window-name-1 "Original image - PYR-DOWN Example")
	   (window-name-2 "Downsampled blurred Image - PYR-DOWN Example"))
      (named-window window-name-1 +window-autosize+)
      (named-window window-name-2 +window-autosize+)
      (move-window window-name-1 533 175)
      (move-window window-name-2 984 175)
      (format t "Image size before downsampling = (~a, ~a)
       ~%~%"(rows img-1) (cols img-1))
      ;;Show original image in window
      (imshow window-name-1 img-1)
      (setf img-2 (do-pyr-down img-1))
      (format t "Image size after downsampling = (~a, ~a)
       ~%~%"(rows img-2)(cols img-2))
      ;;Show blurred downsampled image in window
      (imshow window-name-2 img-2)
      (loop while (not (= (wait-key 0) 27)))
      (destroy-all-windows)))

  (main filename))



PYR-UP

Upsamples an image and then blurs it.

C++: void pyrUp(InputArray src, OutputArray dst, const Size& dstsize=Size(), int borderType=BORDER_DEFAULT )

LISP-CV: (PYR-UP (SRC (:POINTER MAT)) (DEST (:POINTER MAT)) &OPTIONAL ((DSTSIZE (:POINTER SIZE)) (SIZE)) 
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
	   (in-width (round (width in-size)))
	   (in-height (round (height in-size)))
	   (out (mat-typed (/ in-width 2) (/ in-height 2) +8uc3+)))
      ;;Make sure input image is divisible by two."
      (assert (and (equal (mod in-width 2) 0)
		   (equal (mod in-height 2) 0))
	      (in-width in-height)
	      "~S or ~S are not divisible by two" in-width in-height)
      ;;Blur and downsample image
      (pyr-up in out)
      out))

  (defun main (filename)
    
    (let* ((img-1 (imread filename 1))
	   (img-2 0)
	   (window-name-1 "Original image - PYR-UP Example")
	   (window-name-2 "Upsampled blurred image - PYR-UP Example"))
      (named-window window-name-1 +window-autosize+)
      (named-window window-name-2 +window-autosize+)
      (move-window window-name-1 450 0)
      (move-window window-name-2 985 0)
      (format t "Image size before upsampling = (~a, ~a)
       ~%~%"(cols img-1) (rows img-1))
      ;;Show original image in window
      (imshow window-name-1 img-1)
      (setf img-2 (do-pyr-up img-1))
      (format t "Image size after upsampling = (~a, ~a)
       ~%~%"(cols img-2) (rows img-2))
      ;;Show blurred upsampled image in window
      (imshow window-name-2 img-2)
      (loop while (not (= (wait-key 0) 27)))
      (destroy-all-windows)))

  (main filename))


MISCELLANEOUS IMAGE TRANSFORMATIONS:



THRESHOLD

Applies a fixed-level threshold to each array element.

C++: double threshold(InputArray src, OutputArray dst, double thresh, double maxval, int type)

LISP-CV: (THRESHOLD (SRC (:POINTER MAT)) (DEST (:POINTER MAT)) (THRESH :DOUBLE) (MAX-VAL :DOUBLE) (TYPE :INT)) => :DOUBLE


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

See also

(ADAPTIVE-THRESHOLD), (FIND-CONTOURS), (COMPARE), (MIN), (MAX)


(defun threshold-example (&optional (camera-index 0)
			    (width 640) (height 480))

  "Show the camera output and a thresholded 
   version in a single window."

  (with-capture (cap (video-capture camera-index))
    (let* ((window-name "Camera/Threshold")
           (grayscale (mat-typed height width +8u+))
           (threshold (mat-typed height width +8u+))
           (threshold3 (mat-typed height width +8uc3+))
           ;Create a double wide window to show the camera 
           ;output and a thresholded camera output in
           (window (mat-typed height (* width 2) +8uc3+)))
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-height+ height)
      (named-window window-name)
      (move-window window-name 333 175)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
        ;Set camera feed to FRAME
	(setf frame (mat))
	(cap-read cap frame)
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
        (adjust-roi window 0 0 0 (* (cols threshold3) -1))
        ;Copy original camera feed(FRAME) to WINDOW 
        (copy-to frame window)
        ;Set WINDOW roi to the right half
        (adjust-roi window 0 0 (* (cols frame) -1) (cols threshold3))
        ;Copy thresholded camera feed to WINDOW
        (copy-to threshold3 window)
        ;Restore original roi
        (adjust-roi window 0 0 (cols frame) 0)
        ;Show WINDOW in a window
	(imshow window-name window))
      (destroy-window window-name))))


FEATURE DETECTION:


CANNY

Finds edges in an image using the [Canny86] algorithm.

C++: void Canny(InputArray image, OutputArray edges, double threshold1, double threshold2, int apertureSize=3, bool L2gradient=false)

LISP-CV: (CANNY (IMAGE (:POINTER MAT)) (EDGES (:POINTER MAT)) (THRESHOLD1 :DOUBLE) (THRESHOLD2 :DOUBLE) ((APERTURE-SIZE :INT) 3) 
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


;;Define global parameters
(defparameter n 0)
(defparameter i 5)
(defparameter low-thresh 10d0) 
(defparameter high-thresh 100d0)
(defparameter aperture-size 3)
(defparameter l2-gradient nil)



(defcallback call-back-func :void ((event :int) (x :int) (y :int) (flags :int))
  ;This callback function is called by the SET-MOUSE CALLBACK function in 
  ;the MAIN function below. It captures button clicks and keypresses.

  ;Mouse position
  (format t "Mouse position = (~a, ~a)~%~%" x y)
  
  ;If left mouse button and shift pressed, 
  ;increment the CANNY, LOW-THRESH parameter.
  (if (= flags (+ +event-flag-shiftkey+ +event-flag-lbutton+))
      (progn (incf low-thresh i) (format t "low-thresh = ~a~%~%" low-thresh)))
  ;If left mouse button and ctrl pressed, deccrement 
  ;the CANNY, LOW-THRESH parameter.
  (if (= flags (+ +event-flag-ctrlkey+ +event-flag-lbutton+))
      (progn (decf low-thresh i) (format t "low-thresh = ~a~%~%" low-thresh)))
  ;If middle mouse button and shift pressed, 
  ;increment the CANNY, HIGH-THRESH parameter.
  (if (= flags (+ +event-flag-shiftkey+ +event-flag-mbutton+))
      (progn (incf high-thresh i) (format t "high-thresh = ~a~%~%" high-thresh)))
  ;If middle mouse button and ctrl pressed, 
  ;deccrement the CANNY, HIGH-THRESH parameter.
  (if (= flags (+ +event-flag-ctrlkey+ +event-flag-mbutton+))
      (progn (decf high-thresh i) (format t "high-thresh = ~a~%~%" high-thresh)))
  ;If right mouse button double clicked, toggle L2-GRADIENT.
  (if (= event +event-rbuttondblclk+)
      (progn (if (eq n 0) (progn 
			    (setf l2-gradient t) (setf n 1) 
			    (format t "L2-GRADIENT = ~a~%~%" l2-gradient)) 
		 (progn 
		   (setf l2-gradient nil) (setf n 0) 
		   (format t "L2-GRADIENT = ~a~%~%" l2-gradient))))))


(defun canny-example (&optional (camera-index *camera-index*))
  "Finds edges in an image using the [Canny86] algorithm.
   Canny only handles gray scale images"
  (with-capture (cap (video-capture camera-index))
    (let* ((window-name "OUT - CANNY Example"))
      (named-window window-name +window-normal+)
      (move-window window-name 305 300)
      ;Set window to fullscreen with stretched aspect ratio.
      (set-window-property window-name +wnd-prop-fullscreen+ 
			   +window-fullscreen+)
      (if (equal 1.0d0 (get-window-property window-name +wnd-prop-fullscreen+)) 
	  (set-window-property window-name +wnd-prop-aspectratio+ 
			       +window-freeratio+))
      (do* ((frame 0)
	    (clone 0)
	    (out 0))
	   ((eq 27 (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	;Set camera feed to FRAME.
	(setf frame (mat))
	(cap-read cap frame)
	;Clone FRAME
	(setf clone (clone frame))
	;Create destination matrix, half the size of FRAME.
	(setf out (mat-typed (/ (cols frame) 2) (/ (rows frame) 2) +8uc3+))
	;Convert CLONE to a 1 channel grayscale image.
	(cvt-color clone clone +bgr2gray+)
	;Blur and downsample CLONE.
	(pyr-down clone out)
	;Detect edges in camera feed, The parameters can be changed 
	;by clicking the mouse button in the window and pressing a 
	;key. See the callback function above for how to use. 
	(canny out out low-thresh high-thresh aperture-size l2-gradient)
	(set-mouse-callback window-name (callback call-back-func))
	(imshow window-name out)
	(del-mat clone)
	(del-mat out))
      (destroy-all-windows))))



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
    (destroy-window window-name)))



SET-WINDOW-PROPERTY


Changes parameters of a window dynamically.


C++: void setWindowProperty(const string& winname, int prop_id, double prop_value)

LISP-CV: (SET-WINDOW-PROPERTY (WINNAME (:POINTER STRING*)) (PROP-ID :INT) (PROP-VALUE :DOUBLE)) => :VOID


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
  "In the code below the (COLS, ROWS) values of MAT are 
   accessed and stored in a SIZE construct. Their values 
   are accessed with the WIDTH and HEIGHT functions. The-
   n an uninitialized and an initialized SIZE construct  
   are created. Their values are also printed."

  (let* ((mat (mat-value 5 5 +64f+ (scalar 100 100 100)))
         (mat-size (mat-size mat))
          (size-un-init)
          (size (size 640 480)))
    (format t " The (COLS ROWS) of MAT = (~a ~a)~%"  
	    (width mat-size)
	    (height mat-size)
            (format t "Pointer to an uninitialized SIZE construct:~a~%" 
size-un-init) 
(format t "Width of SIZE = ~a~%" (width size))
    (format t "Height of SIZE = ~a" (height size)))))


    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))


IMSHOW

Displays an image in the specified window.

C++: void imshow(const string& winname, InputArray mat)

LISP-CV: (IMSHOW (WINNAME (:POINTER STRING*)) (MAT (:POINTER MAT))) => :void

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

If window was created with OpenGL support, IMSHOW also support ogl::Buffer , ogl::Texture2D and gpu::GpuMat as input. todo


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


IMREAD

Loads an image from a file.

C++: Mat imread(const string& filename, int flags=1)

LISP-CV: (IMREAD (FILENAME (:POINTER STRING*)) &OPTIONAL ((FLAGS :INT) +LOAD-IMAGE-COLOR+)) => (:POINTER MAT)

    Parameters:	

        FILENAME - Name of file to be loaded.

        FLAGS -

        Flags specifying the color type of a loaded image:

            +LOAD-IMAGE-ANYDEPTH+ - If set, return 16-bit/32-bit image when the input has the corre-
                                    sponding depth, otherwise convert it to 8-bit.

            +LOAD-IMAGE-COLOR+ - If set, always convert image to the color one

            +LOAD-IMAGE-GRAYSCALE+ - If set, always convert image to the grayscale one

            >0 Return a 3-channel color image.

                Note

                In the current implementation the alpha channel, if any, is stripped from the outpu-
                t image. Use negative value if you need the alpha channel.

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


(defun imread-example (filename)

  "Open the image FILENAME with IMREAD 
   and show it in a window."

  (let* ((image (imread filename 1))
	 (window-name "IMREAD Example"))
    (if (empty image) 
	(return-from imread-example 
	  (format t "Image not loaded")))
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    (imshow window-name image)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))


NAMED-WINDOW

Creates a window.

C++: void namedWindow(const string& winname, int flags=WINDOW_AUTOSIZE)

LISP-CV: (NAMED-WINDOW (WINNAME (:POINTER STRING*)) &OPTIONAL ((FLAGS :INT) +WINDOW-AUTOSIZE+)) => :VOID

    Parameters:	

        NAME - Name of the window in the window caption that may be used as a window identifier.

        FLAGS -

        Flags of the window. The supported flags are:

            +WINDOW-NORMAL+ If this is set, the user can resize the window (no constraint).

            +WINDOW-AUTOSIZE+ If this is set, the window size is automatically adjusted to fit the 
                              displayed image (see (IMSHOW) ), and you cannot change Mat::copyTo

Copies the matrix to another one.

C++: void Mat::copyTo(OutputArray m) const

C++: void Mat::copyTo(OutputArray m, InputArray mask) const
    Parameters:	

        m - Destination matrix. If it does not have a proper size or type before the operation, it is reallocated.
        mask - Operation mask. Its non-zero elements indicate which matrix elements need to be copied.

The method copies the matrix data to another matrix. Before copying the data, the method invokes

m.create(this->size(), this->type);

so that the destination matrix is reallocated if needed. While m.copyTo(m); works flawlessly, the function does not handle the case of a partial overlap between the source and the destination matrices.

When the operation mask is specified, and the Mat::create call shown above reallocated the matrix, the newly allocated matrix is initialized with all zeros before copying the data.
the window siz-
                              e manually.

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


DESTROY-WINDOW

Destroys a window.

C++: void destroyWindow(const string& winname)

LISP-CV: (DESTROY-WINDOW (WINNAME (:POINTER STRING*)))

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


MOVE-WINDOW

Moves window to the specified position

C++: void moveWindow(const string& winname, int x, int y)

LISP-CV: (MOVE-WINDOW (WINNAME (:POINTER STRING*)) (X :INT) (Y :INT))

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


CAP-GET

Returns the specified VIDEO-CAPTURE property

C++: double VideoCapture::get(int propId)

LISP-CV: (CAP-GET (SELF (:POINTER VIDEO-CAPTURE)) (PROP-ID :INT))

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
      (format t "Frame Size : ~ax~a~%~%" 
	      (cap-get cap +cap-prop-frame-width+)
	      (cap-get cap +cap-prop-frame-height+))
      (named-window window-name +window-normal+)
      (move-window window-name 759 175)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(imshow window-name frame))
      (destroy-window window-name))))


CAP-SET

Sets a property in the VIDEO-CAPTURE

C++: bool VideoCapture::set(int propId, double value)

LISP-CV: (CAP-SET (SELF (:POINTER VIDEO-CAPTURE)) (PROP-ID :INT) (VALUE :DOUBLE)) => :BOOLEAN

    Parameters:	SELF - The VIDEO-CAPTURE structure.

                PROP-ID -

       Property identifier. It can be one of the following:

            +CAP-PROP-POS-MSEC+ Current position of the video file in milliseconds.

            +CAP-PROP-POS-FRAMES+ 0-based index of the frame to be decoded/captured next.

            +CAP-PROP-POS-AVI-RATIO+ Relative position of the video file: 0 - start of the film, 1 -
                                     end of the film.

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


CAP-READ

Grabs, decodes and returns the next video frame.

C++: bool VideoCapture::read(Mat& image)

LISP-CV: (CAP-READ (SELF (:POINTER VIDEO-CAPTURE)) (IMAGE (:POINTER MAT)))

Parameters:	

         SELF - The "grabbed" camera feed.

         IMAGE - The returned frame.


The methods/functions combine (CAP-GRAB) and (CAP-RETRIEVE) in one call. This is the most convenien-
t method for reading video files or capturing data from decode and return the just grabbed frame. I-
f no frames has been grabbed (camera has been disconnected, or there are no more frames in video fi-
le), the methods return false and the functions return NULL pointer.


(defun cap-read-example (&optional 
  "In the code below the (COLS, ROWS) values of MAT are 
   accessed and stored in a SIZE construct. Their values 
   are accessed with the WIDTH and HEIGHT functions. The-
   n an uninitialized and an initialized SIZE construct  
   are created. Their values are also printed."

  (let* ((mat (mat-value 5 5 +64f+ (scalar 100 100 100)))
         (mat-size (mat-size mat))
          (size-un-init)
          (size (size 640 480)))
    (format t " The (COLS ROWS) of MAT = (~a ~a)~%"  
	    (width mat-size)
	    (height mat-size)
            (format t "Pointer to an uninitialized SIZE construct:~a~%" 
size-un-init) 
(format t "Width of SIZE = ~a~%" (width size))
    (format t "Height of SIZE = ~a" (height size)))))


                           (camera-index 
                            *camera-index*))

  "Grabs, decodes and returns the next video frame 
   with the function CAP-READ and then shows it in 
   a window with the function IMSHOW."

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
	(imshow window-name frame))
      (destroy-window window-name))))


CAP-RELEASE

Closes video file or capturing device.

C++: void VideoCapture::release()

LISP-CV: (CAP-RELEASE (SELF (:POINTER VIDEO-CAPTURE))) => :VOID

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
   Note: If you use the macro WITH-CAPTURE, CAP-RELEASE will b-
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


WITH-CAPTURE

Ensures CAP-RELEASE gets called on captures.

LISP-CV: (WITH-CAPTURE (CAPTURE-VAR CAP)) &BODY BODY)

Parameters:	

         CAPTURE-VAR - A variable representing the function used to open video file or a capturing 
                       device. Similar to the variable in a LET statement.

         CAP - The function used to open video file or a capturing device for video capturing, as i-
               n (video-capture (DEVICE :INT)). See WITH-CAPTURE example.
    
         BODY - The body of the code to be executed once the video file or capturing device is open.


(defun with-capture-example (&optional 
			       (camera-index *camera-index*))

  "WITH-CAPTURE is a macro that basically ensures 
   CAP-RELEASE gets called on all captures. CAP-R-
   ELEASE is a function that releases the capture 
   structure created by video-capture, autom-
   atically, when the code exits. Using it is not 
   neccesary, but it does make the code a bit mor-
   eelegant. See CAP-RELEASE example to see how t-
   o release the capture structure without callin-
   g WITH-CAPTURE."

  (with-capture (cap (video-capture camera-index))
    (let ((window-name "WITH-CAPTURE Example"))
      (if (not (cap-is-open cap)) 
	  (return-from with-capture-example 
	    (format t "Cannot open the video camera")))
      (named-window window-name +window-normal+)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(imshow window-name frame))
      (destroy-window window-name))))


CAP-IS-OPEN

Returns true if video capturing has been initialized already.

C++: bool VideoCapture::isOpened()

LISP-CV: (CAP-IS-OPEN (SELF (:POINTER VIDEO-CAPTURE)))

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

LISP-CV:  (SCALAR ((VAL0 :DOUBLE) &OPTIONAL ((VAL1 :DOUBLE) 0) ((VAL2 :DOUBLE) 0) ((VAL3 :DOUBLE) 0)))

    Parameters:	

        VAl0 - First scalar element - Must exist.

        VAl1 - Second scalar element - Can be empty.
       
        VAl2 - Third scalar element - Can be empty.
    
        VAl3 - Fourth scalar element - Can be empty.


The function SCALAR is a SCALAR constructor. It returns a pointer to an up to 4 element scalar. On-
ly VAL1 is required, the rest are optional. 

(defun scalar-example ()

  "The function SCALAR is a SCALAR constructor. It 
   returns a pointer to an up to 4 element scalar."

  (let* ((scalar-1 (scalar 1))
	 (scalar-2 (scalar 1 2))
	 (scalar-3 (scalar 1 2 3))
	 (scalar-4 (scalar 1 2 3 4)))
    (format t "SCALAR-1 element 0 = ~a~%" (mem-aref scalar-1 :double 0))
    (dotimes (n 2)
      (format t "~%SCALAR-2
element ~a = ~a~%" n (mem-aref scalar-2 :double n)))
    (dotimes (n 3)
      (format t "~%SCALAR-3 element ~a = ~a~%" n (mem-aref scalar-3 :double n)))
    (dotimes (n 4)
      (format t "~%SCALAR-4 element ~a = ~a~%" n (mem-aref scalar-4 :double n)))))


SCALAR-ALL

SCALAR constructor.

LISP-CV:  (SCALAR-ALL (VAL0123 :DOUBLE))

    Parameters:	

        VAl0123 -  Scalar element


SCALAR-ALL is a SCALAR constructor. Unlike the function SCALAR, All of the 4 scalar values, 0 thro-
ugh 3, are initialized with one value(VAL0123).


(defun scalar-all-example ()

  "The function SCALAR-ALL, is a SCALAR constructor. 
   Unlike the function SCALAR, it initializes all of
   the 4 scalar values 0...3 with one value. SCALAR-
   ALL returns a pointer to this scalar."

  (let* ((scalar (scalar-all 255)))
    (dotimes (n 4)
      (format t "~%SCALAR element ~a = ~a~%" n 
	      (mem-aref scalar :double n)))))


;;; Types and structures



;;; User Interface


MAT-SIZE

Returns pointer to a matrix size.

C++: Size Mat::size() const

LISP-CV: (MAT-SIZE (SELF (:POINTER MAT)))

The function MAT-SIZE returns Size*, a matrix size pointer in which the columns are listed first an-
d the rows second. When the matrix is more than 2-dimensional, the returned size is (-1 -1).


(defun mat-size-example ()
  
  "In the code below the (COLS, ROWS) values of MAT are 
   accessed and stored in a SIZE construct. Their value-
   s are accessed with the WIDTH and HEIGHT functions."
  
  (let* ((mat (mat-value 5 5 +8u+ (scalar 100 100 100)))
	 (mat-size (size mat)))
    (format t "MAT (COLS,ROWS) = (~a ~a)~%" 
	    ;;The '?' is a macro for CFFI:MEM-AREF
	    (? mat-size :int)
	    (? mat-size :int 1))))



ROWS

Returns number or rows in MAT.

C++: int rows, cols

LISP-CV: (ROWS (SELF (:POINTER MAT))) => :INT

    Parameters:	

        SELF - A MAT construct.


The function ROWS finds the number of rows in a matrix or -1 when the array has more than 2 dimensi-
ons. 


(defun rows-example ()

  "Uses ROWS to find the number of rows in the matrix MAT"

  (let* ((mat (mat-value 3 4 +64f+ (scalar 100)) ))
          (format t "The number of rows in MAT = ~a" (rows mat))))


COLS

Returns number or cols in MAT.

C++: int rows, cols

LISP-CV:  (COLS (SELF (:POINTER MAT))) => :INT

    Parameters:	

        SELF - A matrix(MAT).


The function COLS finds the number of columns in a matrix or -1 when the array has more than 2 dime-
nsions. 


(defun cols-example ()

  "Uses COLS to find the number of columns in the matrix MAT"

  (let* ((mat (mat-value 3 4 +64f+ (scalar 5)) ))
          (format t "The number of columns in MAT = ~a" (cols mat))))



POINT

POINT constructor.

C++: Point_()

LISP-CV: (POINT) => (:POINTER POINT)

C++: Point_(_Tp _x, _Tp _y)

LISP-CV:  (POINT (X :INT) (Y :INT)) => (:POINTER POINT)

C++: _Tp x, y;

LISP-CV: (POINT-X (SELF (:POINTER POINT))) => :INT

C++: _Tp x, y;

LISP-CV: (POINT-Y (SELF (:POINTER POINT))) => :INT


    Parameters:	

        SELF - A POINT construct.

        X - X-coordinate of the point.

        Y -	Y-coordinate of the point.


POINT creates a 2D point with integer coordinates (usually zero-based). Functions POINT-X and  POINT-Y are used to extract the x,y coordinates of a point.



(defun point-example (x y)

  "In this example we create an unitialized 
   POINT with the function POINT. Then crea-
   tes a point with the function POINT. Fin-
   ally, lists the x,y coordinates with the 
   POINT functions POINT-X and POINT-Y."

  (let* ((initialized-point (point))
	 (point (point x y)))
    (format t "Pointer to initialized point: ~a~%~%" 
	    initialized-point)
    (format t "POINT (x, y) = (~a, ~a)~%" 
	    (point-x point)
	    (point-y point))))


POINT2D


POINT2D constructor.


C++: typedef Point_<double> Point2d

LISP-CV: (POINT2D (X :INT) (Y :INT)) => (:POINTER POINT2D)

C++: _Tp x, y

LISP-CV: (POINT2D-X (SELF (:POINTER POINT2D))) => :DOUBLE

C++: _Tp x, y

LISP-CV: (POINT2D-Y (SELF (:POINTER POINT2D))) => :DOUBLE


    Parameters:	

        SELF - A POINT2D construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.


POINT2D creates a 2D point with double-float coordinates (usually zero-based). Functions POINT2D-X 
and  POINT2D-Y are used to extract the x,y coordinates of the point.


(defun point2d-example (x y)

  "In this example we create an uninitialized 
   point2d with the function POINT2D. Then, w-
   e create an initialized point2d and list t-
   he x,y,z coordinates with the functions PO-
   INT2D-X and POINT2D-Y."

  (let* ((point2d-un-init (point2d))
	 (point2d (point2d x y)))
    (format t "Pointer to POINT2D: ~a~%~%" 
	    point2d-un-init)
    (format t "POINT2D (x, y) = (~a, ~a)~%" 
	    (point2d-x point2d)
	    (point2d-y point2d))))


POINT2F

POINT2F constructor.


C++: typedef Point_<float> Point2f

LISP-CV:  (POINT2F (X :INT) (Y :INT)) => (:POINTER POINT2F)

C++: _Tp x, y

LISP-CV: (POINT2F-X (SELF (:POINTER POINT2F))) => :INT

C++: _Tp x, y

LISP-CV: (POINT2F-Y (SELF (:POINTER POINT2F))) => :INT


    Parameters:	

        SELF - A POINT2F construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.


POINT2F creates a 2D point with float coordinates (usually zero-based). Functions POINT2F-X and POI-
NT2F-Y are used to extract the x,y coordinates the point.


(defun point2f-example (x y)

  "In this example we create an uninitialized 
   point2f with the function POINT2F. Then, w-
   e create an initialized point2f and list t-
   he x,y coordinates with the functions POIN-
   T2F-X and POINT2F-Y."

  (let* ((point2f-un-init (point2f))
	 (point2f (point2f x y)))
    (format t "Pointer to POINT2F: ~a~%~%" 
	    point2f-un-init)
    (format t "POINT2F (x, y) = (~a, ~a)~%" 
	    (point2f-x point2f)
	    (point2f-y point2f))))



POINT3I

POINT3I constructor.


C++: typedef Point3_<int> Point3i;

LISP-CV:  (POINT3I (X :INT) (Y :INT) (Z :INT)) => (:POINTER POINT3I)

C++: _Tp x, y, z

LISP-CV: (POINT3I-X (SELF (:POINTER POINT3I))) => :INT

C++: _Tp x, y, z

LISP-CV: (POINT3I-Y (SELF (:POINTER POINT3I))) => :INT


    Parameters:	

        SELF - A POINT3I construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.
        
        Z - Z-coordinate of the point.


POINT3I creates a 3D point with integer coordinates (usually zero-based). Functions POINT3I-X, POIN-
T3I-Y and POINT3I-Z are used to extract the x,y,Z coordinates of the point.


(defun point3i-example (x y z)

  "In this example we create an uninitialized 
   point3i with the function POINT3I. Then, w-
   e create an initialized point3i and list t-
   he x,y,z coordinates with the functions PO-
   INT3I-X, POINT3I-Y and POINT3I-Z."

  (let* ((point3i-un-init (point3i))
        (point3i (point3i x y z)))
    (format t "Pointer to POINT3I: ~a~%~%" 
	    point3i-un-init)
    (format t "POINT3I (x, y, z) = (~a, ~a, ~a)~%" 
	    (point3i-x point3i)
	    (point3i-y point3i)
            (point3i-z point3i))))



POINT3D

POINT3D constructor.


C++: typedef Point3_<double> Point3d

LISP-CV:  (POINT3D (X :INT) (Y :INT) (Z :INT)) => (:POINTER POINT3D)

C++: _Tp x, y, z

LISP-CV: (POINT3D-X (SELF (:POINTER POINT3D))) => :DOUBLE

C++: _Tp x, y, z

LISP-CV: (POINT3D-Y (SELF (:POINTER POINT3D))) => :DOUBLE


    Parameters:	

        SELF - A POINT3D construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.
        
        Z - Z-coordinate of the point.


POINT3D creates a 3D point with double-float coordinates (usually zero-based). Functions POINT3D-X, 
POINT3D-Y AND POINT3D-Z are used to extract the x,y,Z coordinates the point.


(defun point3d-example (x y z)

  "In this example we create an uninitialized 
   point3d with the function POINT3D. Then, w-
   e create an initialized point3d and list t-
   he x,y,z coordinates with the functions PO-
   INT3D-X, POINT3D-Y and POINT3D-Z."

  (let* ((point3d-un-init (point3d))
        (point3d (point3d x y z)))
    (format t "Pointer to POINT3D: ~a~%~%" 
	    point3d-un-init)
    (format t "POINT3D (x, y, z) = (~a, ~a, ~a)~%" 
	    (point3d-x point3d)
	    (point3d-y point3d)
            (point3d-z point3d))))



POINT3F

POINT3F constructor.


C++: typedef Point3_<float> Point3f

LISP-CV:  (POINT3F (X :INT) (Y :INT) (Z :INT)) => (:POINTER POINT3F)

C++: _Tp x, y, z

LISP-CV: (POINT3F-X (SELF (:POINTER POINT3F))) => :FLOAT

C++: _Tp x, y, z

LISP-CV: (POINT3F-Y (SELF (:POINTER POINT))) => :FLOAT


    Parameters:	

        SELF - A POINT3F construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.
        
        Z - Z-coordinate of the point.


POINT3F creates a 3D point with float coordinates (usually zero-based). Functions POINT3F-X, POINT3-
F-Y AND POINT3F-Z are used to extract the x,y,Z coordinates the point.


(defun point3f-example (x y z)

  "In this example we create an uninitialized 
   point3f with the function POINT3F. Then, w-
   e create an initialized point3f and list t-
   he x,y,z coordinates with the functions PO-
   INT3F-X, POINT3F-Y and POINT3F-Z."

  (let* ((point3f-un-init (point3f))
        (point3f (point3f x y z)))
    (format t "Pointer to POINT3F: ~a~%~%" 
	    point3f-un-init)
    (format t "POINT3F (x, y, z) = (~a, ~a, ~a)~%" 
	    (point3f-x point3f)
	    (point3f-y point3f)
            (point3f-z point3f))))



EMPTY

Returns true if the array has no elements.

C++: bool Mat::empty() const

LISP-CV: (EMPTY (SELF (:POINTER MAT)))


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
    (named-window window-name +window-no
  "In the code below the (COLS, ROWS) values of MAT are 
   accessed and stored in a SIZE construct. Their values 
   are accessed with the WIDTH and HEIGHT functions. The-
   n an uninitialized and an initialized SIZE construct  
   are created. Their values are also printed."

  (let* ((mat (mat-value 5 5 +64f+ (scalar 100 100 100)))
         (mat-size (mat-size mat))
          (size-un-init)
          (size (size 640 480)))
    (format t " The (COLS ROWS) of MAT = (~a ~a)~%"  
	    (width mat-size)
	    (height mat-size)
            (format t "Pointer to an uninitialized SIZE construct:~a~%" 
size-un-init) 
(format t "Width of SIZE = ~a~%" (width size))
    (format t "Height of SIZE = ~a" (height size)))))

rmal+)
    (move-window window-name 759 175)
    (imshow window-name image)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))


CV-TYPE

Returns the type of a matrix element.

C++: int Mat::type() const

LISP-CV: (CV-TYPE (SELF (:POINTER MAT)))

    Parameters:	

        SELF - A matrix(MAT)

The method returns a matrix element type. This is an identifier compatible with OpenCV's CvMat type
system, like CV_16SC3(+16SC3+ in LISP-CV) or 16-bit signed 3-channel array, and so on.


(defun mat-type-example ()

  "This function uses MAT-TYPE to find 
   the type of MAT-ONE and MAT-TWO."

  (let* ((mat-one (mat-zeros 1 2 +32f+))
	 (mat-two (mat-zeros 2 4 +64f+)))
    (format t "MAT-ONE type is ~a(+32f+). It is a Single Precision Floating Point Matrix.~%" 
	    (mat-type mat-one))
    (format t "~%MAT-TWO type is ~a(+64f+). It is a Double Precision Floating Point Matrix." 
	    (mat-type mat-two))))



CIRCLE

Draws a circle.

C++: void circle(Mat& img, Point center, int radius, const Scalar& color, int thickness=1, int lineType=8, int shift=0)

LISP-CV: (CIRCLE (IMG (:POINTER MAT)) (CENTER (:POINTER POINT)) (RADIUS :INT) (COLOR (:POINTER SCALAR)) &OPTIONAL ((THICKNESS :INT) 1) ((LINE-TYPE :INT) 8) ((SHIFT :INT) 0))

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
      (format t "Frame Size : ~ax~a~%~%" 
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

LISP-CV: (MAT-EYE (ROWS :INT) (COLS :INT) (TYPE :INT)) => (:POINTER MAT)


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

LISP-CV: (MAT-EXPR-T (SELF (:POINTER MAT))) => (:POINTER MAT-EXPR)

The method performs matrix transposition by means of matrix expressions. It does not perform the ac-
tual transposition but returns a temporary matrix transposition object that can be further used as 
a part of more complex matrix expressions or can be assigned to a matrix.

  Parameters:	

        SELF - Input matrix


; todo - finish example


FORCE

Coverts a MAT-EXPR to MAT

LISP-CV: (FORCE (SELF (:POINTER MAT-EXPR))) => (:POINTER MAT)

LISP-CV: (>> (SELF (:POINTER MAT-EXPR))) => (:POINTER MAT)

The function FORCE converts a functions output from (:POINTER MAT-EXPR) to (:POINTER MAT).  This is 
useful if you have just done mathematical computation with a Matrix Expressions(MAT-EXPR) function 
and would like to use the result in a function that only accepts a MAT as input i.e. IMSHOW. The fu-
nction >> is an identical shorthand version of the FORCE function supplied for ease of use. 

  Parameters:	

        SELF - A Matrix Expressions pointer.


(defun force-example ()

  "In this example a matrix filled with ones(MAT) is 
   created. MAT is then added to itself and the resu-
   lt is shown in a window. The function >> which is 
   a shorthand version of the FORCE function, is nec-
   essary here is because the return of the matrix a-
   ddition function MAT-EXPR+ is (:POINTER MAT-EXPR) 
   and that return must be coerced to (:POINTER MAT) 
   before it can be shown in a window with IMSHOW."

  (let* ((mat (mat-ones 3 3 +8u+))
         (out (add mat mat))
	 (window-name "FORCE Example"))
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    (imshow window-name  (>> out))
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))



mat-typed

Creates an empty matrix of type TYPE.

LISP-CV: (mat-typed) (ROWS :INT) (COLS :INT) (TYPE :INT)) => (:POINTER MAT)

    Parameters:	

        ROWS - The number of rows.
    
        COLS - The number of colounns

        TYPE - The type of the matrix


(defun mat-typed-example ()

  "In this example an 8-bit unsigned matrix is 
   created and the empty matrix is shown in a 
   window."

  (let* ((mat (mat-typed 4 4 +32s+))
	 (window-name "mat-typed Example"))
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    (imshow window-name mat)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))

         
DIAG

Extracts a diagonal from a matrix, or creates a diagonal matrix.

C++: Mat Mat::diag(int d=0 ) const

LISP-CV: (MAT-DIAG (SELF (:POINTER MAT)) (D :INT)) => (:POINTER MAT)

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
	 (mat (mat-data 3 3 +32s+ data))
	 (diag (diag mat 0)))
    (dotimes (i 3)
      (format t "~a" (at-int diag i 0))
      (princ #\Newline))
    (free data)))



SUB

Subtracts matrix M1 from matrix M2

C++: MatExpr - operator

LISP-CV: (SUB (M1 (:POINTER MAT)) (M2 (:POINTER MAT))) => (:POINTER MAT-EXPR)


    Parameters:	

        M1 - A matrix.

        M2 - A matrix.


The function SUB subtracts the elements of matrix M2 from the elements of matrix M1 in order. Both 
matrices must be the same size.  You may need to coerce the result of SUB, the return value, back 
to type (:POINTER MAT) with the function (FORCE), (or the shorthand version (>>)) to use in other 
functions. 


(defun sub-example ()

  "Matrix M2 is subtracted from matrix M1 with the 
   function SUB. Matrix M1, matrix M2 and the resu-
   lt(RESULT) are then printed."

  (let* ((m1-data (alloc :uint '(53 62 85 64 23 97 52 16 12)))
	 (m2-data (alloc :uint '(64 22 64 15 11 17 42 16 88)))
	 (m1 (mat-data 3 3 +32s+ m1-data))
         (m2 (mat-data 3 3 +32s+ m2-data))
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

LISP-CV: (DIV (M1 (:POINTER MAT)) (M2 (:POINTER MAT))) => (:POINTER MAT-EXPR)


    Parameters:	

        M1 - A matrix.

        M2 - A matrix.


The function DIV divides the elements of matrix M1 by the elements of matrix M2 in order. Both matr-
ices must be the same size. You may need to coerce the result of DIV, the return value, back to typ-
e (:POINTER MAT) with the function (FORCE), (or the shorthand version (>>)) to use in other functio-
ns. 


(defun div-example ()

  "Matrix M1 is divided by M2 with the function DIV.
   Matrix M1, M2 and the result(RESULT) are then pri-
   nted."

  (let* ((m1-data (alloc :float '(53.0f0 62.0f0 85.0f0 64.0f0 23.0f0 
				  97.0f0 52.0f0 16.0f0 12.0f0)))
	 (m2-data (alloc :float '(64.0f0 22.0f0 64.0f0 15.0f0 11.0f0 
				  17.0f0 42.0f0 16.0f0 88.0f0)))
	 (m1 (mat-data 3 3 +32f+ m1-data))
         (m2 (mat-data 3 3 +32f+ m2-data))
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

LISP-CV: (VIDEO-WRITER)

C++: VideoWriter::VideoWriter(const string& filename, int fourcc, double fps, Size frameSize, bool isColor=true)

LISP-CV: (VIDEO-WRITER (FILENAME (:POINTER STRING*)) (FOURCC :INT) (FPS :DOUBLE) 
              (FRAME-SIZE (:POINTER SIZE)) ((IS-COLOR :INT) T)) => (:POINTER VIDEO-WRITER)


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
					(camera-index *camera-index*))

  (with-capture (cap (video-capture camera-index)) ; Open the video camera no. 0
    (let* ((filename filename)
	   (window-name "VIDEO-WRITER Example")

			    ; Get the width of frames of the video
	   (dwidth (rational (cap-get cap +cap-prop-frame-width+)))

			     ; Get the width of frames of the video
	   (dheight (rational (cap-get cap +cap-prop-frame-height+)))

			  ; Initialize the VideoWriter object 
	   (o-video-writer (video-writer filename 1196444237 ; todo
					 20.0d0 (size 640 480) 1))) 
      
      (if (not (cap-is-open cap)) ; If not success, exit program
	  (return-from video-writer-example 
	    (format t "ERROR: Cannot open the video file")))

     ; If VideoWriter isn't initialized successfully, exit the program
      (if (not (video-writer-is-open o-video-writer)) 
	  (return-from video-writer-example 
	    (format t "ERROR: Failed to write the video"))) 

    ;; Print video width and height
      (format t "Frame Size : ~ax~a~%~%" dwidth dheight)

      (named-window window-name +window-normal+) ; Create a window
      (move-window window-name 759 175)
      (do* ((frame 0))

	          ; Wait for 'esc' key press for 30ms. 
                  ; If 'esc' key is pressed, break loop
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))

	(cap-read cap frame) ; read a new frame from video

	(if (not frame) ; If not success, break loop
	    (return-from video-writer-example 
	      (format t "ERROR: Cannot read a frame from video file")))

	(video-writer-write o-video-writer frame) ; Write the frame into the file

	(imshow window-name frame)) ; Show the frame in window
      (destroy-window window-name))))



VIDEO-WRITER-IS-OPEN

Returns true if video writer has been successfully initialized.

C++: bool VideoWriter::isOpened()

LISP-CV: (VIDEO-WRITER-IS-OPEN (SELF (:POINTER VIDEO-WRITER)))


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

LISP-CV: (VIDEO-WRITER-WRITE (SELF (:POINTER VIDEO-WRITER)) (IMAGE (:POINTER MAT)))

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

LISP-CV: (MAT-CLONE (SELF (:POINTER MAT))) => (:POINTER MAT)


    Parameters:	

        SELF - Pointer to a matrix


The method creates a full copy of the array. The original TODO step[] is not taken into account. So
, the array copy is a continuous array occupying (* (TOTAL) (ELEM-SIZE)) bytes.


(defun clone-example ()

        ; Create data
  (let* ((m1-data (alloc :float '(53.0f0 62.0f0 85.0f0 64.0f0 23.0f0 
				   97.0f0 52.0f0 16.0f0 12.0f0)))
         ; Create matrix M1 and fill it with data
	 (m1 (mat-data 3 3 +32f+ m1-data))
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


MAT-STEP1

Returns a normalized step.

C++: size_t Mat::step1(int i=0 ) const

LISP-CV: (STEP1 (SELF (:POINTER MAT))) => :UNSIGNED-INT

The method returns a matrix step divided by (ELEM-SIZE1) . It can be useful to quickly access an ar
bitrary matrix element.



TOTAL

Returns the total number of array elements.

C++: size_t Mat::total() const

LISP-CV: (TOTAL (SELF (:POINTER MAT))) => :UNSIGNED-INT


    Parameters:	

        SELF - Pointer to a matrix


The method returns the number of array elements (a number of pixels if the array represents an imag-
e).


(defun total-example ()

   "In this function, TOTAL returns the total 
   number of array elements in MAT1 and MAT2"

   (let* ((data (alloc :int '(1 2 3 4)))
	  (mat1 (mat-data 2 2 +32s+ data))
	  (mat2 (mat-typed 100 100 +32s+))
	  (total1 (total mat1))
	  (total2 (total mat2)))
     (format t "Total mumber of elements in MAT1 = ~a~%~%" total1)
     (format t "Total mumber of elements in MAT2 = ~a" total2)
     (free data)))


ROI

Returns matrix header corresponding to the rectangular sub-array of an input matrix.

C++: Mat::Mat(const Mat& m, const Rect& roi)

LISP-CV: (ROI (SELF (:POINTER MAT)) (ROI (:POINTER RECT))) => (:POINTER MAT)


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
      (format t "Frame Size : ~ax~a~%~%" 
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

LISP-CV:  (RECT (X :INT) (Y :INT) (:WIDTH :INT) (HEIGHT :INT)) => (:POINTER RECT)

C++: Point_<_Tp> tl() const;

LISP-CV: (TL (SELF (:POINTER RECT))) => (:POINTER POINT)

C++: Point_<_Tp> br() const;

LISP-CV: (BR (SELF (:POINTER RECT))) => (:POINTER POINT)

C++: Size_<_Tp> size() const;

LISP-CV: (SZ (SELF (:POINTER RECT))) => (:POINTER SIZE)


    Parameters:	

        SELF - A rectangle.

        X - x-coordinate of the rectangle.

        Y -	y-coordinate of the rectangle.

        WIDTH - Width of the rectangle.

        HEIGHT - Height of the rectangle.


The function RECT stores coordinates of a rectangle.

The function TL retrieves the top-left corner of the rectangle.

The function BR retrieves t
)he bottom-right corner of the rectangle.

The function SZ retrieves the size (width, height) of the rectangle.

The function AREA retrieves the area (width*height) of the rectangle.


(defun rect-example ()

  "The function RECT is a RECT constructor. It returns a 
   pointer to a rectangle. In this function a rectangle 
   is created. The size of the rectangle(width, height) 
   is retrieved with the function SZ. The top-left corne-
   r is retrieved with the function TL. The bottom-right 
   corner is retrieved with the function BR. All three a-
   re printed to the screen."

  (let* ((rectangle (rect 0 0 640 480))
	 (rect-size (sz rectangle))
	 (rect-tl-corner (tl rectangle))
	 (rect-br-corner (br rectangle)))
    (format t "~%The size(width, height) of RECTANGLE = (~a, ~a)~%" 
	    (mem-aref rect-size :int 0)
	    (mem-aref rect-size :int 1))
    (format t "~%The top-left corner of RECTANGLE = (~a, ~a)~%" 
	    (mem-aref rect-tl-corner :int 0)
	    (mem-aref rect-tl-corner :int 1))
    (format t "~%The bottom-right corner of RECTANGLE = (~a, ~a)~%" 
	    (mem-aref rect-br-corner :int 0)
	    (mem-aref rect-br-corner :int 1))))


DOT

Dot product computed in double-precision arithmetics.

C++:  _Tp dot(const Point_& pt) const;

LISP-CV: (DOT (SELF (:POINTER POINT)) (OTHER (:POINTER POINT))) => :INT

LISP-CV: (DOT2F (SELF (:POINTER POINT2F)) (OTHER (:POINTER POINT2F))) => :FLOAT

LISP-CV: (DOT2D (SELF (:POINTER POINT2D)) (OTHER (:POINTER POINT2D))) => :DOUBLE

C++"  _Tp dot(const Point3_& pt) const;

LISP-CV: (DOT3I (SELF (:POINTER POINT3I)) (OTHER (:POINTER POINT3I))) => :INT

LISP-CV: (DOT3F (SELF (:POINTERRead POINT3F)) (OTHER (:POINTER POINT3F))) => :FLOAT

LISP-CV: (DOT3D (SELF (:POINTER POINT)) (OTHER (:POINTER POINT))) => :INT


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
   find the dot product of 2 point2f con-
   structs P1 and P2."

  (let* ((p1 (point2f 1.0f0 2.0f0))
	 (p2 (point2f 3.0f0 4.0f0)))
    (format t "The dot product of P1 and P2 = ~a~%~%"  
	    (dot2f p1 p2) )))


(defun dot2d-example ()

  "This example uses the function DOT to 
   find the dot product of 2 point2d con-
   structs P1 and P2."

  (let* ((p1 (point2d 1.0d0 2.0d0))
	 (p2 (point2d 3.0d0 4.0d0)))
    (format t "The dot product of P1 and P2 = ~a~%~%"  
	    (dot2d p1 p2) )))


(defun dot3i-example ()

  "This example uses the function DOT to 
   find the dot product of 2 point3i con-
   structs P1 and P2."

  (let* ((p1 (point3i 1 2 3))
	 (p2 (point3i 4 5 6)))
    (format t "The dot product of P1 and P2 = ~a~%~%"  
	    (dot3i p1 p2) )))


(defun dot3f-example ()

  "This example uses the function DOT to 
   find the dot product of 2 point3f con-
   structs P1 and P2."

  (let* ((p1 (point3f 7.0f0 8.0f0 9.0f0))
	 (p2 (point3f 10.0f0 11.0f0 12.0f0)))
    (format t "The dot product of P1 and P2 = ~a~%~%"  
	    (dot3f p1 p2) )))

(defun dot3d-example ()

  "This example uses the function DOT to 
   find the dot product of 2 point3d con-
   structs P1 and P2."

  (let* ((p1 (point3d 13.0d0 14.0d0 15.0d0))
	 (p2 (point3d 16.0d0 17.0d0 18.0d0)))
    (format t "The dot product of P1 and P2 = ~a~%~%"  
	    (dot3d p1 p2))))


IN-RANGE

Checks if array elements lie between the elements of two other arrays.

C++: void inRange(InputArray src, InputArray lowerb, InputArray upperb, OutputArray dst)

LISP-CV: (IN-RANGE-S (SRC (:POINTER MAT)) (LOWERB (:POINTER SCALAR)) (UPPERB (:POINTER SCALAR)) (DEST (:POINTER MAT)) => :VOID


    Parameters:	

        SRC - first input array.

        LOWERB - A scalar.

        UPPERB - A scalar.

        DEST - output array of the same size as SRC and +8U+ type.


All the arrays must have the same type, except the destination, and the same size (or ROI size).



(defun in-range-s-example (&optional (camera-index *camera-index*) 
			     (width 640)
			     (height 480))

  (with-capture (cap (video-capture camera-index))
    (let ((window-name-1 "Original camera feed - IN-RANGE-S Example")
	  (window-name-2 "Only red objects - IN-RANGE-S Example")
	  (img-hsv 0)
	  (img-thresh 0)
	  (src 0))
      (if (not (cap-is-open cap)) 
	  (return-from in-range-s-example 
	    (format t "Cannot open the video camera")))
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-height+ height)
      (format t "Frame Size : ~ax~a~%~%" 
	      (cap-get cap +cap-prop-frame-width+)
	      (cap-get cap +cap-prop-frame-height+))
      (named-window window-name-1 +window-normal+)
      (named-window window-name-2 +window-normal+)
      (move-window window-name-1 533 175)
      (move-window window-name-2 984 175)
      ;; Iterate through each frames of the video
      (do* ((frame 0)
	    (lower-hsv (scalar 170 160 60))
	    (upper-hsv (scalar 180 2556 256)))
           ;; Wait 33mS - If 'esc' is pressed, break the loop
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(setf src (clone frame))
	(setf img-hsv (mat))
	(setf img-thresh (mat))
	;; Smooth the original image using Gaussian kernel
	(gaussian-blur src src (size 5 5) 0.0d0 0.0d0)
	;; Change the color format from BGR to HSV
	(cvt-color src img-hsv +bgr2hsv+)
	;; Threshold the HSV image and create a binary image
	(in-range-s img-hsv lower-hsv upper-hsv img-thresh)
	;; Smooth the binary image using Gaussian kernel
	(gaussian-blur img-thresh img-thresh (size 5 5) 0.0d0 0.0d0)
	(imshow window-name-1 src)
	(imshow window-name-2 img-thresh)
	;; Clean up used images
	(del-mat img-hsv) (del-mat img-thresh) 
	(del-mat frame) (del-mat src))
      (destroy-window window-name-1)
      (destroy-window window-name-2))))


GAUSSIAN-BLUR

Blurs an image using a Gaussian filter.

C++: void GaussianBlur(InputArray src, OutputArray dst, Size ksize, double sigmaX, double sigmaY=0,
     int borderType=BORDER_DEFAULT )                   

Commom Lisp: (GAUSSIAN-BLUR (SRC (:POINTER MAT)) (DEST (:POINTER MAT)) (KSIZE (:POINTER SIZE)) (SIG
              MA-X :DOUBLE) &OPTIONAL ((SIGMA-Y :DOUBLE) 0) ((BORDER-TYPE :INT) +BORDER-DEFAULT+))

    Parameters:	

        SRC - input image; the image can have any number of channels, which are processed independe-
              ntly, but the depth should be +8U+, +16U+, +16S+, +32F+ or +64F+.

        DST - output image of the same size and type as SRC.

        KSIZE - Gaussian kernel size KSIZE width and KSIZE height can differ but they both must be 
                positive and odd. Or, they can be zero’s and then they are computed from sigma.

        SIGMAX - Gaussian kernel standard deviation in X direction.

        SIGMAY - Gaussian kernel standard deviation in Y direction; if SIGMAY is zero, it is set to 
                 be equal to SIGMAX, if both sigmas are zeros, they are computed from KSIZE width a-
                 nd KSIZE height , respectively (see (GET-GAUSSIAN-KERNEL) for details); to fully c-
                 ontrol the result regardless of possible future modifications of all this semantics, 
                 it is recommended To specify all of KSIZE, SIGMA-X, AND SIGMA-Y.

        BORDER-TYPE - pixel extrapolation method (see (BORDER-INTERPOLATE) for details).


The function convolves the source image with the specified Gaussian kernel. In-place filtering is s-
upported.

See also:

(SEP-FILTER-2D), FILTER-2D), (BLUR), (BOX-FILTER), (BILATERAL-FILTER), (MEDIAN-BLUR)


(defun gaussian-blur-example (&optional (camera-index *camera-index*) 
				(width 640)
				(height 480))
  "In this example the function GAUSSIAN-BLUR is used 
   to blur an image using a Gaussian filter. The orig-
   inal image FRAME and the blurred image SRC are sho-
   wn in separate windows."
  (with-capture (cap (video-capture camera-index))
    (let ((window-name-1 "Original - GAUSSIAN-BLUR Example")
	  (window-name-2 "Blurred output - GAUSSIAN-BLUR Example")
	  (src 0))
      (if (not (cap-is-open cap)) 
	  (return-from gaussian-blur-example 
	    (format t "Cannot open the video camera")))
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-height+ height)
      (format t "Frame Size : ~ax~a~%~%" 
	      (cap-get cap +cap-prop-frame-width+)
	      (cap-get cap +cap-prop-frame-height+))
      (named-window window-name-1 +window-normal+)
      (named-window window-name-2 +window-normal+)
      (move-window window-name-1 533 175)
      (move-window window-name-2 984 175)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(setf src (clone frame))
	(gaussian-blur src src (size 19 19) 0.0d0 0.0d0)
	(imshow window-name-1 frame)
	(imshow window-name-2 src)
	(del-mat frame) (del-mat src))
      (destroy-window window-name-1)
      (destroy-window window-name-2))))


CVT-COLOR

Converts an image from one color space to another.

C++: void cvtColor(InputArray src, OutputArray dst, int code, int dstCn=0 )

LISP-CV: (CVT-COLOR (SRC (:POINTER MAT)) (DEST (:POINTER MAT)) (CODE :INT) ((DEST-CN :INT) 0))


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
      (format t "Frame Size : ~ax~a~%~%" 
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

LISP-CV: (SCALE (SELF (:POINTER MAT-EXPR)) (ALPHA :DOUBLE)) => (:POINTER MAT-EXPR)


    Parameters:	

        SELF - A single float or double float matrix.

        ALPHA - A scalar of type double-float. 


This is the primary function used in this library for multiplication by and division by scalar. See 
SCALE-EXAMPLE for an example of division by scalar. You may need to coerce the rerturn value of 
SCALE, a scaled matrix, back to type (:POINTER MAT) with the function (FORCE), (or the shorthan-
d version (>>)) to use in other functions. Also matrices of (:POINTER MAT) type must be coerced to 
(:POINTER MAT-EXPR) type, with the function PROMOTE(<<), before passing to SCALE.


(defun scale-example ()

  "In this example a +32F+(float) matrix is 
   created and filled with data. Then, usin-
   g SCALE, each element of the matrix 
   is divided by the scalar 10. Finally the 
   matrix is printed."

  (let* ((data (alloc :float '(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0 
                               6.0f0 7.0f0 8.0f0 9.0f0)))
	 (mat (mat-data 3 3 +32f+ data))
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

LISP-CV: (DRAW-MATCHES (IMG1 (:POINTER MAT)) (KEYPOINTS1 (:POINTER KEYPOINT)) (IMG2 (:POINTER MAT)) (KEYPOINTS2 (:POINTER KEYPOINT)) (MATCHES1TO2 (:POINTER VECTOR-DMATCH)) (OUTIMG (:POINTER MAT)) (MATCH-COLOR (:POINTER SCALAR)) (SINGLE-POINT-COLOR (:POINTER SCALAR)) &OPTIONAL ((MATCHES-MASK (:POINTER VECTOR-CHAR)) (VECTOR-CHAR)) ((FLAGS :INT) +DEFAULT+))
-
C++: void drawMatches(const Mat& img1, const vector<KeyPoint>& keypoints1, const Mat& img2, const vector<KeyPoint>& keypoints2, const vector<vector<DMatch>>& matches1to2, Mat& outImg, const Scalar& matchColor=Scalar::all(-1), const Scalar& singlePointColor=Scalar::all(-1), const vector<vector<char>>& matchesMask=vector<vector<char> >(), int flags=DrawMatchesFlags::DEFAULT )

    Parameters:	

        IMG1 - First source image.

        KEYPOINTS1 - Keypoints from the first source image.

        IMG2 - Second source image.

        KEYPOINTS2 - Keypoints from the second source image.

        MATCHES1TO2 - Matches from the first image to the second one, which means that keypoints1[i]
                      has a corresponding point in keypoints2[matches[i]].

        OUT-IMG - Output image. Its content depends on the flags value defining what is drawn in th-
                  e output image. See possible flags bit values below.

        MATCH-COLOR - Color of matches (lines and connected keypoints). If (EQUAL MATCH-COLOR (SCALAR-ALL -1)) 
                      the color is generated randomly.

        SINGLE-POINT-COLOR - Color of single keypoints (circles), which means that keypoints do not 
                             have the matches. If (EQUAL SINGLE-POINT-COLOR (SCALAR-ALL -1)) , the 
                             color is generated randomly.

        MATCHES-MASK - Mask determining which matches are drawn. If the mask is empty, all matches are drawn.

        FLAGS - Flags setting drawing features. Possible flags bit values are defined below.

This function draws matches of keypoints from two images in the output image. Match is a line conne-
cting two keypoints (circles). The FLAGS parameters are defined as follows:

        +DEFAULT+ = 0 - Output image matrix will be created (MAT-CREATE), i.e. existing memory of o-
                        utput image may be reused. Two source images, matches, and single keypoints 
                        will be drawn. For each keypoint, only the center point will be drawn (with-
                        out a circle around the keypoint with the keypoint size and orientation).

        +DRAW-OVER-OUTIMG+ = 1 - Output image matrix will not be created (using MAT-CREATE). Matche-
                                 s will be drawn on existing content of output image.

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
 
      RED * WHITE * +NOT-;;; Types and structures



;;; User InterfaceDRAW-SINGLE-POINTS+
    
    Try using the box.png and box_in_scene.png images located inside the
    LISP-CV-MASTER/IMAGES directory to get a clearer understanding of th-
    is example the first time you run it."

  ;; the object you want to track - 
  (let* ((object (imread filename-1 +load-image-grayscale+))
	 ;; the image the object is a part of
	 (image (imread filename-2 +load-image-grayscale+)) 
	 (keypoints-a (vector-keypoint))
	 (keypoints-b (vector-keypoint))
	 (descriptors-a (mat))
	 (descriptors-b (mat))
         ;; Set brisk parameters
	 (thresh 60)
	 (octaves 4)
	 (pattern-scale 2.0f0)
         ;; declare a variable BRISKD of the type (:POINTER BRISK)
	 (briskd (brisk thresh octaves pattern-scale))
         ;; declare matcher
	 (matcher (bf-matcher))

	 (matches (vector-dmatch))
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
		  (scalar-all -1) (scalar-all -1) (vector-char) 
		  +default+)
    ;; show the matches in a window 
    (imshow window-name-1 all-matches)
    ;; draw the found matches
    (draw-matches object keypoints-a image keypoints-b matches all-matches 
		  (scalar 0 0 0) (scalar 255 255 255) (vector-char) 
		  +draw-rich-keypoints+)
    ;; show the matches in a window 
    (imshow window-name-2 all-matches)
    ;; draw the found matches
    (draw-matches object keypoints-a image keypoints-b matches all-matches 
		  (scalar 0 0 255) (scalar 255 255 2555) (vector-char) 
		  +not-draw-single-points+)
    ;; show the matches in a window 
    (imshow window-name-3 all-matches)
    ;; draw the found matches
    (draw-matches object keypoints-a image keypoints-b matches all-matches 
		  (scalar-all 255) (scalar-all -1) (vector-char) 
		  +draw-rich-keypoints+)
    ;; show the matches in a window 
    (imshow window-name-4 all-matches)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name-1)
    (destroy-window window-name-2)
    (destroy-window window-name-3)
    (destroy-window window-name-4)))



BRISK

The BRISK constructor

C++: BRISK::BRISK(int thresh=30, int octaves=3, float patternScale=1.0f)

LISP-CV: (BRISK &OPTIONAL ((THRESH :INT) 30) ((OCTAVES :INT) 3) ((PATTERN-SCALE :FLOAT) 1.0F0) => (:POINTER BRISK)

    Parameters:	

        THRESH - FAST/AGAST detection threshold score.

        OCTAVES - detection octaves. Use 0 to do single scale.

        PATTERN-SCALE - apply this scale to the pattern used for sampling the neighbourhood of a keypoint.

BRISK is a construct implementing the BRISK keypoint detector and descriptor extractor, described in [LCS11]:

http://docs.opencv.org/modules/features2d/doc/feature_detection_and_description.html?highlight=brisk#lcs11



(defun brisk-example (filename-1 filename-2)

  "Don't let this example make you nervous it's basically 12 
   FEAT-DETECT-CREATE-EXAMPLES, stacked, in one. Here I'm ba-
   sically just showing, in a quick easy to see fashion, how 
   the THRESH, OUTPUT and PATTERN-SCALE parameters of the fu-
   nction BRISK affect it's output. Each of the 12 windows h-
   as the function call used to set those parameters printed 
   on the titlebar, so you don't have to look through the co-
   de to get the effect of this example. For example, if you 
   see this on the titlebar of the window: (BRISK 0 0 0.0f0) 
   Then you know the BRISK parameter set for that window is: 

       THRESH = 0, OCTAVES  = 0, PATTERN-SCALE = 0.0f0.

   Note: Try using the box.png and the box_in_scene.png from
   the LISP-CV-MASTER/IMAGES directory to get a better under-
   standing of this example the first time you run it. And, 
   just be aware, this example takes a few seconds to start."

  ;; read two images in grayscale, first the object you want to track,
  (let* ((gray-a (imread filename-1 +load-image-grayscale+))
	 ;; second the image the object is a part of
	 (gray-b (imread filename-2 +load-image-grayscale+))
         ;; make arrays to hold the keypoints and descriptors
         (keypoints-a-arr (make-array '(12))) 
         (keypoints-b-arr (make-array '(12)))
	 (descriptors-a-arr (make-array '(12)))
	 (descriptors-b-arr (make-array '(12)))  
	 (matcher-arr (make-array '(12))) 
	 (matches-arr (make-array '(12)))
	 (all-matches-arr (make-array '(12)))
	 ;; declare an array of BRISK constructs of the 
         ;; type (:POINTER BRISK) and set their parameters
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
      (setf (aref keypoints-a-arr i) (vector-keypoint)))
    (dotimes (i 12)
      (setf (aref keypoints-b-arr i) (vector-keypoint)))
    ;; declare an array of 12 query descriptors 
    (dotimes (i 12)
      (setf (aref descriptors-a-arr i) (mat)))
    ;; declare an array of 12 train descriptors 
    (dotimes (i 12)
      (setf (aref descriptors-b-arr i) (mat)))
    ;; declare an array of 12 matchers with type (:POINTER BF-MATCHER)
    (dotimes (i 12)
      (setf (aref matcher-arr i) (bf-matcher)))
    ;; declare an array of 12 MAT constructs to hold the 
    ;; matches from the first image to the second one
    (dotimes (i 12)
      (setf (aref matches-arr i) (vector-dmatch)))
    ;; declare an array of 12 MAT constructs to hold the final output images
    (dotimes (i 12)
      (setf (aref all-matches-arr i) (mat)))
    ;; find matches, between the two images, 12 times,
    ;; each using a different set of BRISK parameters
    (dotimes (i 12)
      ;; create a feature detector
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
      (descrip-matcher-match (aref matcher-arr i) (aref descriptors-a-arr i) (aref descriptors-b-arr i) (aref matches-arr i))
      ;; draw the found matches
      (draw-matches gray-a (aref keypoints-a-arr i) gray-b (aref keypoints-b-arr i) (aref matches-arr i) (aref all-matches-arr i) 
		    (scalar-all -1) (scalar-all -1) (vector-char) 
		    +draw-rich-keypoints+))
    ;; show the 12 different matches in 12 windows
    (dotimes (i 12)
      (imshow (aref window-name-arr i) (aref all-matches-arr i)))
    ;; cleanup used memory
    (dotimes (i 12)
      (del-bf-matcher (aref matcher-arr i))
      (del-brisk (aref brisk-arr i)))
    ;; after 'esc' key is pressed destroy all 12 windows
    (loop while (not (= (wait-key 0) 27)))
    (dotimes (i 12)
      (destroy-window (aref window-name-arr i)))))


FEAT-DETECT-CREATE

Creates a feature detector by its name.

C++: Ptr<FeatureDetector> FeatureDetector::create(const string& detectorType)

LISP-CV: (FEAT-DETECTOR-CREATE (SELF (:POINTER FEATURE-DETECTOR)) (DETECTOR-TYPE :STRING)) => (:POINTER FEATURE-DETECTOR) 


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

  ;; read some images in grayscale -> The object you want to track
  (let* ((gray-a (imread filename-1 +load-image-grayscale+))
	 ;; The image the object is a part of
	 (gray-b (imread filename-2 +load-image-grayscale+)) 
	 (keypoints-a (vector-keypoint))
	 (keypoints-b (vector-keypoint))
	 (descriptors-a (mat))
	 (descriptors-b (mat))
         ;; set brisk parameters
	 (thresh 60)
	 (octaves 4)
	 (pattern-scale 2.0f0)
         ;; declare a variable BRISKD of the type (:POINTER BRISK)
	 (briskd (brisk thresh octaves pattern-scale))
         ;; declare matcher
	 (matcher (bf-matcher))
	 (matches (vector-dmatch))
	 (all-matches (mat))
	 (window-name "All Matches - FEAT-DETECT-CREATE Example"))
    (if (empty (or gray-a gray-b)) 
	(return-from feat-detect-create-example 
	  (format t "Both images were not loaded")))
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
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    ;; draw the found matches
    (draw-matches gray-a keypoints-a gray-b keypoints-b matches all-matches 
		  (scalar-all -1) (scalar-all -1) (vector-char) 
		  +not-draw-single-points+)
    ;; show the matches in a window 
    (imshow window-name all-matches)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))


SURF

The SURF extractor constructors.

C++: SURF::SURF()

LISP-CV: (SURF0) => (:POINTER SURF)

C++: SURF::SURF(double hessianThreshold, int nOctaves=4, int nOctaveLayers=2, bool extended=true, bool upright=false )

LISP-CV: (SURF5 (HESSIAN-THRESHOLD :DOUBLE) &OPTIONAL ((N-OCTAVES :INT) 4) 
                 ((EXTENDED :BOOLEAN) T) ((UPRIGHT :BOOLEAN) NIL)) => (:POINTER SURF)

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

  ;; read some images in grayscale -> The object you want to track
  (let* ((img-1 (imread filename-1 +load-image-grayscale+))
	 ;; The image the object is a part of
	 (img-2 (imread filename-2 +load-image-grayscale+))
         (min-hessian 400d0) 
         (detector (surf5 min-hessian))
	 (keypoints-1 (vector-keypoint))
	 (keypoints-2 (vector-keypoint))
         (extractor (surf0))
	 (descriptors-1 (mat))
	 (descriptors-2 (mat))
	 (matcher (bf-matcher +norm-l2+))
	 (matches (vector-dmatch))
	 (img-matches (mat))
	 (window-name "Image Matches - SURF Example"))
    (if (empty (or img-1 img-2)) 
	(return-from surf-example 
	  (format t "Both images were not loaded")))
    ;;-- Step 1: Detect the keypoints using SURF Detector
    (feat-detector-detect detector img-1 keypoints-1)
    (feat-detector-detect detector img-2 keypoints-2)
    ;;-- Step 2: Calculate descriptors (feature vectors)
    (feat-2d-compute extractor img-1 keypoints-1 descriptors-1)
    (feat-2d-compute extractor img-2 keypoints-2 descriptors-2)
    ;-- Step 3: Matching descriptor vectors with a brute force matcher
    (descrip-matcher-match matcher descriptors-1 descriptors-2 matches)
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    ;;-- Draw matches
    (draw-matches img-1 keypoints-1 img-2 keypoints-2 matches img-matches)
    ;;-- Show detected matches
    (imshow window-name img-matches)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))


CREATE-TRACKBAR

Creates a trackbar and attaches it to the specified window.

C++: int createTrackbar(const string& trackbarname, const string& winname, int* value, int count, TrackbarCallback onChange=0, void* userdata=0)

LISP-CV:  (CREATE-TRACKBAR (TRACKBARNAME :STRING) (WINNAME :STRING) (VALUE :POINTER) (COUNT :INT) &OPTIONAL ((ON-CHANGE (:POINTER TRACKBAR-CALLBACK)) (NULL-POINTER)) ((USERDATA :POINTER) (NULL-POINTER))) => :INT

    
    Parameters:	

        TRACKBARNAME - Name of the created trackb

        WINNAME - Name of the window that will be used as a parent of the created trackbar.

        VALUE - Optional pointer to an integer variable whose value reflects the position of the sl-
                ider. Upon creation, the slider position is defined by this variable.

        COUNT - Maximal position of the slider. The minimal position is always 0.

        ON-CHANGE - Pointer to the function to be called every time the slider changes position. Th-
                    is function should be prototyped as void Foo(int,void*); , where the first para-
                    meter is the trackbar position and the second parameter is the user data (see t-
                    he next parameter). If the callback is the NULL pointer, no callbacks are calle-
                    d, but only value is updated.

        userdata - User data that is passed as is to the callback. It can be used to handle trackba-
                   r events without using global variables.


The function CREATE-TRACKBAR creates a trackbar (a slider or range control) with the specified name
and range, assigns a variable value to be a position synchronized with the trackbar and specifies t-
he callback function onChange to be called on the trackbar position change. The created trackbar is 
displayed in the specified window winname.

Note: [Qt Backend Only] winname can be empty (or NULL) if the trackbar should be attached to the control panel.


Example:

;; a callback function called by the CREATE-TRACKBAR
;; ON-CHANGE parameter...a HELLO-WORLD function.
;; '?' is a macro for CFFI:MEM-AREF
(defcallback hello-world-brightness :void ((pos :int) (ptr :pointer))
  (format t "Hello World!~%~%~a~a~%~%" (? ptr :string 0) pos))

;; another HELLO-WORLD callback function
(defcallback hello-world-contrast :void ((pos :int) (ptr :pointer))
  (format t "Hello World!~%~%~a~a~%~%" (? ptr :string 0) pos))


(defun create-trackbar-example (filename)
  ;; read in image supplied by filename parameter
  (let ((src (imread filename 1))
	(window-name "Adjust brightness and contrast by moving the sliders.")
        ;; allocate two :int pointers that trackbar can adjust
	(slider-1-value (alloc :int '(50)))
	(slider-2-value (alloc :int '(50)))
	;; data to be passed to HELLO-WORLD-BRIGHTNESS callback function
	(userdata-1 (alloc :string "Brightness =  "))
	;; data to be passed to HELLO-WORLD-CONTRAST callback function
	(userdata-2 (alloc :string "Contrast = ")))
    (if (empty src) 
	(return-from create-trackbar-example
	  (format t "Image not loaded")))
    (named-window window-name 1)
    (move-window window-name 759 175)
    (do* ((dest 0)
	  (brightness 0)
	  (contrast 0))
	 ((plusp (wait-key *millis-per-frame*)) 
	  (format t "Key is pressed by user"))
      ;; Clone the source image to dest
      (setf dest (clone src))
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
      ;; clen up used memory
      (del-mat dest))
    (destroy-window window-name)
    (free slider-1-value)
    (free slider-2-value)
    (free userdata-1)
    (free userdata-2)))



SET-MOUSE-CALLBACK

Sets mouse handler for the specified window

C++: void setMouseCallback(const string& winname, MouseCallback onMouse, void* userdata=0 )

LISP-CV: (SET-MOUSE-CALLBACK (WINNAME :STRING) (ON-MOUSE (:POINTER MOUSE-CALLBACK)) (USERDATA :VOID)) => :VOID


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
    (free userdata)))



DATA

Pointer to MAT data.

C++: uchar* data

LISP-CV: (DATA (SELF (:POINTER MAT)) ) => :POINTER

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


;;Must supply a filename parameter for the image you 
;;will be using in this example and one for the file 
;;the image's pixel value will be written it.
(defun data-example (filename-1 filename-2)
  ;;read image
  (let* ((img (imread filename-1 1)) 
         ;;INPUT is a pointer to IMG data
	 (input (data img))
         ;;variables used to hold the BGR image pixel values
	 (b 0)
	 (g 0)
	 (r 0)
	 (window-name "DATA Example"))
    (if (empty img) 
	(return-from data-example 
	  (format t "Image not loaded")))
    (named-window window-name +window-normal+)
    (move-window window-name 759 175)
    ;;In a loop access IMG pixel data using the STEP* 
    ;;function and print to a file instead of the co-
    ;;nsole so your implimentation doesn't freeze.
    (with-open-file (str filename-2
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (dotimes (i (rows img))
	(dotimes (j (cols img))
	  (setf b (mem-aref input :uchar 
			    (+  (* (step* img) i) (* 3 j) )))
	  (setf g (mem-aref input :uchar 
			    (+ (+  (* (step* img) i) (* 3 j) ) 1)))
	  (setf r (mem-aref input :uchar 
			    (+ (+  (* (step* img) i) (* 3 j) ) 2)))
	  (format str "(~a,~a,~a)~%" b g r))))
    (imshow window-name img)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))


STEP*

Used to compute address of a matrix element

C++: MStep step

LISP-CV: (STEP (SELF (:POINTER MAT))) => :UNSIGNED-INT

    Parameters:	

        SELF  a pointer to matrix(MAT construc


This function is used to compute the address of a matrix element. The image step gives you the dist-
ance in bytes between the first element of one row and the first element of the next row. This func-
tion is named STEP*, because the name STEP conflicts with a Lisp Macro.


(defun step*-example (filename)
  ;; load image
  (let* ((img (imread filename 1))
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



$

Time how long a function takes to complete n iterations.


LISP-CV: ($ FORM &optional (COUNT-FORM 1000000) ) => RESULT*

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


UNIFORM

Returns the next random number sampled from the uniform distribution.

C++: int RNG::uniform(int a, int b)

LISP-CV: (UNIFORM (RNG (:POINTER RNG)) (A :DOUBLE) (B :DOUBLE)) => :DOUBLE

C++: float RNG::uniform(float a, float b)

LISP-CV: (UNIFORM (RNG (:POINTER RNG)) (A :FLOAT) (B :FLOAT)) => :FLOAT

C++: double RNG::uniform(double a, double b)

LISP-CV: (UNIFORM (RNG (:POINTER RNG)) (A :INT) (B :INT)) => :INT


    Parameters:	

        RNG - An RNG construct

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


RNG

The constructors

C++: RNG::RNG()

LISP-CV: (RNG)

C++: RNG::RNG(uint64 state)


    Parameters:	

        STATE - 64-bit value used to initialize the RNG.


These are the RNG constructors. The first form sets the state to some pre-defined value, equal to 
2**32-1 in the current implementation. The second form sets the state to the specified value. If 
you passed STATE = 0 , the constructor uses the above default value instead to avoid the singular 
random number sequence, consisting of all zeros.



VECTOR

Bindings for the C++ VECTOR class.

C++: template < class T, class Alloc = allocator<T> > class vector; // generic template

LISP-CV: See description.


    Parameters:	

       See description.


The bindings for the C++ vector class, so far, are:


LISP-CV:                      
-------------        

VECTOR-CHAR(vector<char>)

VECTOR-DMATCH(vector<DMatch>)      

VECTOR-DOUBLE(vector<double>) 

VECTOR-FLOAT(vector<float>)

VECTOR-INT(vector<int>)

VECTOR-KEYPOINT(vector<KeyPoint>)

VECTOR-POINT(vector<Point>)

VECTOR-POINT2F(vector<Point2f>)

VECTOR-UCHAR(vector<uchar>)



Description:


Vectors with numbers as elements, VECTOR-CHAR, VECTOR-DOUBLE, VECTOR-FLOAT and VECTOR-INT 
VECTOR-UCHAR operate as follows: (I use VECTOR-FLOAT as an example of the 5 vectors).


If you would like to created an unititialized vector, you evaluate:



LISP-CV> (VECTOR-FLOAT)


#.(SB-SYS:INT-SAP #X7FFFDC000F40) <--- Output is an uninitialized pointer to a float vector.



If you would like to created an initialized vector, you evaluate:



LISP-CV> (VECTOR-FLOAT '(1f0 2f0 3f0)) 


#.(SB-SYS:INT-SAP #X7FFFDC000F80) <--- Output is an initialized pointer to a float vector.



The functionality to retrieve data from an initialized vector is built into the vector function. So
to retrieve data from a vector you evaluate as follows(vector elements are zero-based):



LISP-CV> (DEFPARAMETER A (VECTOR-FLOAT '(1f0 2f0 3f0))) <--- Create an initialized vector A.

A

LISP-CV> (VECTOR-FLOAT A) <--- Access the 0th element of A.

1.0

LISP-CV> (VECTOR-FLOAT A 1) <---Access the 1st element of A.

2.0

LISP-CV> (VECTOR-FLOAT A 2) <---Access the 2nd element of A.

3.0



Vectors with, pointers to vectors with numbers, as their elements, VECTOR-DMATCH, VECTOR-KEYPOINT,
VECTOR-POINT, and VECTOR-POINT2F operate as follows:(I use VECTOR-POINT2F as an example of the 
four vectors.)



If you would like to created an uninitialized vector, you evaluate:


LISP-CV> (VECTOR-POINT2F)


#.(SB-SYS:INT-SAP #X7FFFDC001120) <--- Output is an uninitialized pointer to a POINT2F vector.



If you would like to created an initialized vector, you evaluate:


LISP-CV> (VECTOR-POINT2F (LIST (POINT2F 1F0 2F0) (POINT2F 3F0 4F0)))


#.(SB-SYS:INT-SAP #X7FFFDC0011C0) <--- Output is an initialized pointer to a POINT2F vector.



The functionality to retrieve data from an initialized vector with pointers to vectors with numbers, 
as their elements is built into the vector function. So to retrieve data from this type of vector 
you evaluate as follows(vector elements are zero-based):


LISP-CV> (DEFPARAMETER A (VECTOR-POINT2F 
			  (LIST (POINT2F 1F0 2F0) (POINT2F 3F0 4F0)))) <--- Create an initialized vector A.

A

LISP-CV> (VECTOR-POINT2F A 0) ---> Access the 0th POINT2F in vector A

#.(SB-SYS:INT-SAP #X7FFFD80008C0)

LISP-CV> (VECTOR-POINT2F A 0) ---> Access the 1st POINT2F in vector A

#.(SB-SYS:INT-SAP #X7FFFD80008C0)

LISP-CV> (VECTOR-POINT2F A 0 0) <--- Access the 0th element of the 0th POINT2F in vector A.

1.0

LISP-CV> (VECTOR-POINT2F A 0 1) <--- Access the 1st element of the 0th POINT2F in vector A.

2.0

LISP-CV> (VECTOR-POINT2F A 1 0) <--- Access the 0th element of the 1st POINT2F in vector A.

3.0

LISP-CV> (VECTOR-POINT2F A 1 1) <--- Access the 1st element of the 1st POINT2F in vector A.

4.0








MACROS





Macro for CFFI::FOREIGN-ALLOC

CFFI: - Function: foreign-alloc type &key initial-element initial-contents (count 1) null-terminated-p ⇒ pointer

LISP-CV: (ALLOC TYPE VALUE) => :POINTER


    Parameters:	

        TYPE - A CFFI type

        VALUE - A number or a sequence - Stand-in for the INITIAL-ELEMENT and INITIAL-CONTENTS para-
                meter of FOREIGN-ALLOC



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



LIVE-CODE-EDITING:


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



