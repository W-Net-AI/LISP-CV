;;;; -*- mode: lisp; indent-tabs: nil -*-
;;;; examples.lisp
;;;; Documentation and Examples:


;====================================================CORE==========================================
;
;
;BASIC STRUCTURES
;
;
;====================================================CORE==========================================
;
;COPY-TO
;
;Copies the matrix to another one.
;
;
;C++: void Mat::copyTo(OutputArray m) const
;
;Common Lisp: (COPY-TO (SELF (:POINTER MAT)) (M (:POINTER MAT))) => :VOID
;
;C++: void Mat::copyTo(OutputArray m, InputArray mask) const
;
;Common Lisp: (COPY-TO (SELF (:POINTER MAT)) (M (:POINTER MAT)) (MASK (:POINTER MAT))) => :VOID
;
;
;    Parameters:	
;
;        SELF - A matrix.         
;
;        M - Destination matrix. If it does not have a proper size or type before the operation, it 
;            is reallocated.
;
;        MASK - Operation mask. Its non-zero elements indicate which matrix elements need to be copi-
;               ed.
;
;
;The method copies the matrix data to another matrix. Before copying the data, the method invokes:
;
;(CREATE (MAT-SIZE THIS) (MAT-TYPE THIS))
;
;So that the destination matrix is reallocated if needed. While (COPY-TO M M) works flawlessly, the 
;function does not handle the case of a partial overlap between the source and the destination matri-
;ces. When the operation mask is specified, and the (CREATE) call shown above reallocated the matrix
;, the newly allocated matrix is initialized with all zeros before copying the data.


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
         ;; nts indicate which matrix elements nee-
         ;; d to be copied.
         (mask (mat-eye 2 2 +8u+)))
    ;; copy data from MAT-1 to M.
    (copy-to mat-1 m-1)
    ;; print contents of MAT-1.
    (format t "MAT-1 =~%~%")
    (dotimes (i (rows mat-1))
      (dotimes (j (cols mat-1))
	(format t "~a" (at-int mat-1 i j))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%")
    ;; print contents of of M-1.
    (format t "M-1 =~%~%")
    (dotimes (i (rows m-1))
      (dotimes (j (cols m-1))
	(format t "~a" (at-int m-1 i j))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%")
    ;; copy data from MAT-2 to M using mask.
    (copy-to mat-2 m-2 mask)
    ;; print contents of MAT-2.
    (format t "MAT-2 =~%~%")
    (dotimes (i (rows mat-2))
      (dotimes (j (cols mat-2))
	(format t "~a" (at-int mat-2 i j))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%")
    ;; print contents of MASK.
    (format t "MASK =~%~%")
    (dotimes (i (rows mask))
      (dotimes (j (cols mask))
	(format t "~a" (at-uchar mask i j))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%")
    ;; print contents of of M-2.
    (format t "M-2 =~%~%")
    (dotimes (i (rows m-2))
      (dotimes (j (cols m-2))
	(format t "~a" (at-int m-2 i j))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")))


;====================================================CORE=============================================
;
;
;PROMOTE 
;
;Coverts a MAT to MAT-EXPR
;
;Common Lisp: (PROMOTE (SELF (:POINTER MAT))) => (:POINTER MAT-EXPR)
;
;Common Lisp: (<< (SELF (:POINTER MAT))) => (:POINTER MAT-EXPR)
;
;The function PROMOTE converts a functions return from (:POINTER MAT) to (:POINTER MAT-EXPR). This i-
;s useful if you would like to do math computation on a matrix with a (:POINTER MAT) type using a su-
;per fast Matrix Expressions(MAT-EXPR) function. Some Matrix Expressions functions will only accept 
;a (:POINTER MAT-EXPR) type as input and that is what makes this function necessary.  You can then c-
;onvert back to (:POINTER MAT) with the function FORCE to use the result in a function that only acc-
;epts a MAT as input i.e. IMSHOW. The function << is an identical shorthand version of the PROMOTE f-
;unction supplied for ease of use. 
;
;   Parameters:	
;
;        SELF - A MAT pointer.
;
;
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
    (move-window window-name 720 175)
    (imshow window-name  (>> out))
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))




;====================================================CORE===========================================
;
;Drawing Functions
;
;====================================================CORE===========================================
;
;BGR
;
;
;A macro for SCALAR organized as BGR(BLUE, GREEN, RED) color values.
;
;
;Common Lisp: (BGR B G R) => (:POINTER SCALAR)
;
;
;    Parameters:	
;
;        B - The blue color value
;
;        G - The green color value
;
;        R - The red color value
;
;
;BGR is the default color space in LisP-CV
;
;
;Usage:
;
;
; Here BGR supplies a blue color value. 
;
(CIRCLE IMAGE POINT RADIUS (BRG 255 0 0) +FILLED+ +AA+ 0)
;
;
;====================================================CORE===========================================
;
;
;ELLIPSE
;
;Draws a simple or thick elliptic arc or fills an ellipse sector.
;
;C++: void ellipse(Mat& img, Point center, Size axes, double angle, double startAngle, double endAngle, const Scalar& color, 
;     int thickness=1, int lineType=8, int shift=0)
;
;Common Lisp: (ELLIPSE (IMG (:POINTER MAT)) (CENTER (:POINTER POINT)) (AXES (:POINTER SIZE)) (ANGLE :DOUBLE) (START-ANGLE :DOUBLE)
;             (END-ANGEL :DOUBLE) (COLOR (:POINTER SCALAR)) (THICKNESS :INT) (LINE-TYPE :INT) (SHIFT :INT)) => :VOID
;
;C++: void ellipse(Mat& img, const RotatedRect& box, const Scalar& color, int thickness=1, int lineType=8)
;
;
;
;    Parameters:	
;
;        IMG - Image.
;
;        CENTER - Center of the ellipse.
;
;        AXES - Half of the size of the ellipse main axes.
;
;        AMGLE - Ellipse rotation angle in degrees.
;
;        START-AMGLE - Starting angle of the elliptic arc in degrees.
;
;        END-ANGLE - Ending angle of the elliptic arc in degrees.
;
;        BOX - Alternative ellipse representation via ROTATED-RECT. This means that the function dra-
;              ws an ellipse inscribed in the rotated rectangle.
;
;        COLOR - Ellipse color.
;
;        THICKNESS - Thickness of the ellipse arc outline, if positive. Otherwise, this indicates th-
;                    at a filled ellipse sector is to be drawn.
;
;        LINE-TYPE - Type of the ellipse boundary. See the (LINE) description.
;
;        SHIFT - Number of fractional bits in the coordinates of the center and values of axes.
;
;
;The functions ELLIPSE with less parameters draws an ellipse outline, a filled ellipse, an elliptic 
;arc, or a filled ellipse sector. A piecewise-linear curve is used to approximate the elliptic arc b-
;oundary. If you need more control of the ellipse rendering, you can retrieve the curve using the fu-
;nction (ELLIPSE-2-POLY) and then render it with the function (POLYLINES) or fill it with the functi-
;on (FILL-POLY) . If you use the first variant of the function and want to draw the whole ellipse, n-
;ot an arc, pass (EQ START-ANGLE 0) and (END-ANGLE 360).
;
;
;====================================================CORE============================================
;
;LINE
;
;Draws a line segment connecting two points.
;
;C++: void line(Mat& img, Point pt1, Point pt2, const Scalar& color, int thickness=1, int lineType=8, int shift=0)
;
;Common Lisp: (LINE (IMG (:POINTER MAT)) (PT1 (:POINTER POINT)) (PT2 (:POINTER POINT)) (COLOR (:POINTER SCALAR)) &OPTIONAL 
;((THICKNESS :INT) 1) ((LINE-TYPE :INT) 8) ((SHIFT :INT) 0)) => :VOID
;
;    Parameters:	
;
;              IMG - Image.
;
;              PT1 - First point of the line segment.
;
;              PT2 - Second point of the line segment.
;
;              COLOR - Line color.
;
;              THICKNESS - Line thickness.
;
;              LINE-TYPE - Type of the line:
;
;                       8 (or omitted) - 8-connected line.
;
;                       4 - 4-connected line.
;
;                       +AA+ - antialiased line.
;
;              SHIFT - Number of fractional bits in the point coordinates.
;
;
;
;The function line draws the line segment between pt1 and pt2 points in the image. The line is clipp-
;ed by the image boundaries. For non-antialiased lines with integer coordinates, the 8-connected or 
;4-connected Bresenham algorithm is used. Thick lines are drawn with rounding endings. Antialiased l-
;ines are drawn using Gaussian filtering. To specify the color of the line, you may also use the mac-
;ros RGB - (RGB R G B) and BGR - (BGR B G R).


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


;====================================================CORE============================================
;
;PUT-TEXT
;
;Draws a text string.
;
;C++: void putText(Mat& img, const string& text, Point org, int fontFace, double fontScale, Scalar color, int thickness=1, 
;     int lineType=8, bool bottomLeftOrigin=false )
;
;Common Lisp: (PUT-TEXT (IMG (:POINTER MAT)) (TEXT (:POINTER STRING*)) (ORG (:POINTER POINT)) (FONT-FACE :INT) (FONT-SCALE :DOUBLE)  
;             (COLOR (:POINTER SCALAR)) (THICKNESS :INT) (LINE-TYPE :INT) (BOTTOM-LEFT-ORIGN :BOOLEAN)) => :VOID
; 
;    Parameters:	
;
;        IMG - Image.
;
;        TEXT - Text string to be drawn.
;
;        ORG - Bottom-left corner of the text string in the image.
;
;        FONT-FACE - Font type. One of: +FONT-HERSHEY-SIMPLEX+ 
;
;                                       +FONT-HERSHEY-PLAIN+ 
;
;                                       +FONT-HERSHEY-DUPLEX+ 
;
;                                       +FONT-HERSHEY-COMPLEX+ 
;
;                                        FONT_HERSHEY_ITALIC
;
;                                       +FONT-HERSHEY-COMPLEX-SMALL+ 
;
;                                       +FONT-HERSHEY-SCRIPT-SIMPLEX+ 
; 
;                                       +FONT-HERSHEY-SCRIPT-COMPLEX+ 
;
;
;where each of the font id’s can be combined with +FONT-ITALIC+ to get the slanted letters.
;
;
;        FONT-SCALE - Font scale factor that is multiplied by the font-specific base size.
;
;        COLOR- Text color.
;
;        THICKNESS - Thickness of the lines used to draw a text.
;
;        LINE-TYPE - Line type. See the line for details.
;
;        BOTTOM-LEFT-ORIGIN - When true, the image data origin is at the bottom-left corner. Otherwi-
;                             se, it is at the top-left corner.
;
;
;The function PUT-TEXT renders the specified text string in the image. Symbols that cannot be render-
;ed using the specified font are replaced by question marks. See (GET-TEXT-SIZE) for a text renderin-
;g code example.



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


;====================================================CORE=============================================
;
;RECTANGLE
;
;
;Draws a simple, thick, or filled up-right rectangle.
;
;
;C++: void rectangle(Mat& img, Point pt1, Point pt2, const Scalar& color, int thickness=1, int lineType=8, int shift=0)
;
;Common Lisp: (RECTANGLE (IMG (:POINTER MAT)) (PT1 (:POINTER POINT)) (PT2 (:POINTER POINT)) (COLOR (:POINTER SCALAR)) &OPTIONAL 
;             ((THICKNESS :INT) 1) ((LINE-TYPE :INT) 8) ((SHIFT :INT) 0)) => :VOID
;
;
;    Parameters:	
;
;        Parameters:	
;
;              IMG - Image.
;
;              PT1 - First point of the line segment.
;
;              PT2 - Second point of the line segment.
;
;              COLOR - Rectangle color or brightness (grayscale image).
;
;              THICKNESS - Thickness of lines that make up the rectangle. 
;                          Negative values, like +FILLED+ (-1) , mean that 
;                          the function has to draw a filled rectangle.
;
;              LINE-TYPE - Type of the line:
;
;                      8 (or omitted) - 8-connected line.
;
;                      4 - 4-connected line.
;
;                      +AA+ - antialiased line.
;
;              SHIFT - Number of fractional bits in the point coordinates.
;
;
;The function rectangle draws a rectangle outline or a filled rectangle whose two opposite corners a-
;re PT1 and PT2.


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


;====================================================CORE=============================================
;
;RGB
;
;A macro for SCALAR organized as RGB(RED, GREEN, BLUE) color values.
;
;Common Lisp: (RGB R G B) => (:POINTER SCALAR)
;
;    Parameters:	
;
;
;        R - The red color value
;
;        G - The green color value
;
;        B - The blue color value
;
;
;A creative reversal of the default BGR color space in LisP-CV. Values are put into the RGB macro in
;Red, Green, Blue, order, but are ultimately entered into the recieving function as BGR. This macro 
;is designed for ease of use.

;
;Usage:
;
;Here RGB supplies a red color value. 
;
(CIRCLE IMAGE POINT RADIUS (RGB 255 0 0) +FILLED+ +AA+ 0) 
;
;
;
;TYPES AND STRUCTURES
;--------------------
;
;
;
;USER INTERFACE
;--------------
;
;
;
;QT NEW FUNCTIONS
;----------------


GET-WINDOW-PROPERTY

Provides parameters of a window.

C++: double getWindowProperty(const string& winname, int prop_id)

Common Lisp: (GET-WINDOW-PROPERTY (WINNAME (:pOINTER STRING*)) (PROP-ID) :INT) => :DOUBLE


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

Common Lisp: (SET-WINDOW-PROPERTY (WINNAME (:POINTER STRING*)) (PROP-ID :INT) (PROP-VALUE :DOUBLE)) => :VOID


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
    (destroy-window window-name)))














IMSHOW

Displays an image in the specified window.

C++: void imshow(const string& winname, InputArray mat)

Common Lisp: (IMSHOW (WINNAME (:POINTER STRING*)) (MAT (:POINTER MAT))) => :void

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
    (move-window window-name 720 175)
    (imshow window-name image)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))


IMREAD

Loads an image from a file.

C++: Mat imread(const string& filename, int flags=1)

Common Lisp: (IMREAD (FILENAME (:POINTER STRING*)) &OPTIONAL ((FLAGS :INT) +LOAD-IMAGE-COLOR+)) => (:POINTER MAT)

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

    On Microsoft Windows* OS and MacOSX*, the codecs shipped with an W-NET-CL-OPENCV image (libjpeg
    , libpng, libtiff, and libjasper) are used by default. So, W-NET-CL-OPENCV can always read JPEG-
    s, PNGs, and TIFFs. On MacOSX, there is also an option to use native MacOSX image readers. But 
    beware that currently these native image loaders give images with different pixel values becaus-
    e of the color management embedded into MacOSX. 

    On Linux*, BSD flavors and other Unix-like open-source operating systems, W-NET-CL-OPENCV looks
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
    (move-window window-name 720 175)
    (imshow window-name image)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))


NAMED-WINDOW

Creates a window.

C++: void namedWindow(const string& winname, int flags=WINDOW_AUTOSIZE)

Common Lisp: (NAMED-WINDOW (WINNAME (:POINTER STRING*)) &OPTIONAL ((FLAGS :INT) +WINDOW-AUTOSIZE+)) => :VOID

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

Common Lisp: (DESTROY-WINDOW (WINNAME (:POINTER STRING*)))

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

Common Lisp: (DESTROY-ALL-WINDOWS) => :VOID


The function DESTROY-ALL-WINDOWS destroys all of the opened HighGUI windows.


(defun destroy-all-windows-example ()

  "In this example we create
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
    (destroy-window window-name))) 12 windows and DESTROY THEM!!!"

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

Common Lisp: (MOVE-WINDOW (WINNAME (:POINTER STRING*)) (X :INT) (Y :INT))

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
    (move-window window-name 720 175)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name))


WAIT-KEY

Waits for a pressed key.

C++: int waitKey(int delay=0)

Ccommon Lisp: (WAIT-KEY &OPTIONAL ((DELAY :INT) 0))

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
    (move-window window-name 720 175)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))


CAP-GET

Returns the specified VIDEO-CAPTURE property

C++: double VideoCapture::get(int propId)

Common Lisp: (CAP-GET (SELF (:POINTER VIDEO-CAPTURE)) (PROP-ID :INT))

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

  (with-capture (cap (cap-cam camera-index))
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
      (move-window window-name 720 175)
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

Common Lisp: (CAP-SET (SELF (:POINTER VIDEO-CAPTURE)) (PROP-ID :INT) (VALUE :DOUBLE)) => :BOOLEAN

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

  (with-capture (cap (cap-cam camera-index))
    (let ((window-name "CAP-SET Example"))
      (if (not (cap-is-open cap)) 
	  (return-from cap-set-example 
	    (format t "Cannot open the video camera")))
(cap-set cap +cap-prop-brightness+ 0.7)
      (format t "Brightness level: ~a~%~%" 
	      (cap-get cap +cap-prop-brightness+))
      (named-window window-name +window-normal+)
      (move-window window-name 720 175)
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

Common Lisp: (CAP-READ (SELF (:POINTER VIDEO-CAPTURE)) (IMAGE (:POINTER MAT)))

Parameters:	

         SELF - The "grabbed" camera feed.

         IMAGE - The returned frame.


The methods/functions combine (CAP-GRAB) and (CAP-RETRIEVE) in one call. This is the most convenien-
t method for reading video files or capturing data from decode and return the just grabbed frame. I-
f no frames has been grabbed (camera has been disconnected, or there are no more frames in video fi-
le), the methods return false and the functions return NULL pointer.


(defun cap-read-example (&optional 
                           (camera-index 
                            *camera-index*))

  "Grabs, decodes and returns the next video frame 
   with the function CAP-READ and then shows it in 
   a window with the function IMSHOW."

  (with-capture (cap (cap-cam camera-index))
    (let ((window-name "CAP-READ Example"))
      (if (not (cap-is-open cap)) 
	  (return-from cap-read-example 
	    (format t "Cannot open the video camera")))
      (named-window window-name +window-normal+)
      (move-window window-name 720 175)
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

Common Lisp: (CAP-RELEASE (SELF (:POINTER VIDEO-CAPTURE))) => :VOID

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
  
  (let ((cap (cap-cam camera-index))
	(window-name "CAP-RELEASE Example"))
    (if (not (cap-is-open cap)) 
	(return-from cap-release-example 
	  (format t "Cannot open the video camera")))
    (named-window window-name +window-normal+)
    (move-window window-name 720 175)
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

Common Lisp: (WITH-CAPTURE (CAPTURE-VAR CAP)) &BODY BODY)

Parameters:	

         CAPTURE-VAR - A variable representing the function used to open video file or a capturing 
                       device. Similar to the variable in a LET statement.

         CAP - The function used to open video file or a capturing device for video capturing, as i-
               n (CAP-CAM (DEVICE :INT)). See WITH-CAPTURE example.
    
         BODY - The body of the code to be executed once the video file or capturing device is open.


(defun with-capture-example (&optional 
			       (camera-index *camera-index*))

  "WITH-CAPTURE is a macro that basically ensures 
   CAP-RELEASE gets called on all captures. CAP-R-
   ELEASE is a function that releases the capture 
   structure created by CAP-CAM, autom-
   atically, when the code exits. Using it is not 
   neccesary, but it does make the code a bit mor-
   eelegant. See CAP-RELEASE example to see how t-
   o release the capture structure without callin-
   g WITH-CAPTURE."

  (with-capture (cap (cap-cam camera-index))
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

Common Lisp: (CAP-IS-OPEN (SELF (:POINTER VIDEO-CAPTURE)))

Parameters:	

         SELF - The VIDEO-CAPTURE structure.

If the previous call to VIDEO-CAPTURE constructor or CAP-IS-OPEN succeeded, the method returns true.


(defun cap-is-open-example (&optional 
                              (camera-index 
                               *camera-index*))

  "If the previous call to VIDEO-CAPTURE constructor (i/e, 
   (CAP-CAM CAMERA-INDEX) in the below example) or the fu-
   nction CAP-IS-OPEN succeeded, the method returns true. 
   The boolean output of the PRINC function in the IF sta-
   tement in this example reflects a good or bad capture. 
   Output will likely be '1' or 'True' unless your camera 
   is unplugged....Try unplugging your camera to test it 
   out."

  (with-capture (cap (cap-cam camera-index)) 
    (let ((window-name "CAP-IS-OPEN Example"))
      (if (not (princ (cap-is-open cap))) 
	  (return-from cap-is-open-example 
	    (format t "Cannot open the video camera")))
      (named-window window-name +window-normal+)
      (move-window window-name 720 175)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "~%~%Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(imshow window-name frame))
      (cap-release cap)
      (destroy-window window-name))))indicate


ABSDIFF

Calculates the per-element absolute difference between two arrays or between an array and a scalar.

C++: void absdiff(InputArray src1, InputArray src2, OutputArray dst)

Common Lisp: (ABSDIFF (SRC1 (:POINTER MAT)) (SRC2 (:POINTER MAT)) (DEST (:POINTER MAT)))

    Parameters:	

        SRC1 - first input array or a scalar.

        SRC2 - second input array or a scalar.

        DEST - output array that has the same size and type as input arrays.

The function absdiff calculates:

        Absolute difference between two arrays when they have the same size and type:

        \texttt{dst}(I) = \texttt{saturate} (| \texttt{src1}(I) - \texttt{src2}(I)|) todo

        Absolute difference between an array and a scalar when the second array is constructed from
        Scalar or has as many elements as the number of channels in SRC1:

        \texttt{dst}(I) = \texttt{saturate} (| \texttt{src1}(I) - \texttt{src2} |) todo

        Absolute difference between a scalar and an array when the first array is constructed from 
        Scalar or has as many elements as the number of channels in SRC2:

        \texttt{dst}(I) = \texttt{saturate} (| \texttt{src1} - \texttt{src2}(I) |) todo

        where I is a multi-dimensional index of array elements. In case of multi-channel arrays, ea-
        ch channel is processed independently.

Note:

Saturation is not applied when the arrays have the depth +32S+. You may even get a negative value i-
n the case of overflow.

See also:

(ABS) todo add this function


(defun absdiff-example (&optional 
			  (camera-index 
			   *camera-index*) 
			  (width *default-width*)
			  (height *default-height*))

  "The function ABSDIFF calculates the per-element absolute 
   difference between FRAME(the camera stream) and SCALAR a-
   nd outputs the result to a window...Makes for quite an i-
   nteresting effect."

  (with-capture (cap (cap-cam camera-index))
    (let ((scalar (mat-value 1 1 +64f+ (scalar 128 128 128)))
	  (window-name "ABSDIFF Example"))
      (if (not (cap-is-open cap)) 
	  (return-from absdiff-example 
	    (format t "Cannot open the video camera")))
      (cap-set cap +cap-prop-frame-width+ width)
      (cap-set cap +cap-prop-frame-height+ height)
      (named-window window-name +window-normal+)
      (move-window window-name 720 175)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(absdiff frame scalar frame)
	(imshow window-name frame))
      (destroy-window window-name))))


SCALAR

SCALAR constructor.

Common Lisp:  (SCALAR ((VAL0 :DOUBLE) &OPTIONAL ((VAL1 :DOUBLE) 0) ((VAL2 :DOUBLE) 0) ((VAL3 :DOUBLE) 0)))

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
    (destroy-window window-name))) element ~a = ~a~%" n (mem-aref scalar-2 :double n)))
    (dotimes (n 3)
      (format t "~%SCALAR-3 element ~a = ~a~%" n (mem-aref scalar-3 :double n)))
    (dotimes (n 4)
      (format t "~%SCALAR-4 element ~a = ~a~%" n (mem-aref scalar-4 :double n)))))


SCALAR-ALL

SCALAR constructor.

Common Lisp:  (SCALAR-ALL (VAL0123 :DOUBLE))

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

Common Lisp: (MAT-SIZE (SELF (:POINTER MAT)))

The function MAT-SIZE returns Size*, a matrix size pointer in which the columns are listed first an-
d the rows second. When the matrix is more than 2-dimensional, the returned size is (-1 -1).


(defun mat-size-example ()

  "The function MAT-SIZE returns Size*, a matrix size 
   pointer in which the columns are listed first and 
   the rows are listed second(COLS ROWS). In the cod-
   e below the (COLS ROWS) values of MAT which are s-
   tored in SIZE are accessed with the CFFI function 
   MEM-AREF."

  (let* ((mat (mat-value 3 7 +64f+ (scalar 100 100 100)))
         (size (mat-size mat)))
    (format t " The (COLS ROWS) of MAT = (~a ~a)~%"  
	    (mem-aref size :int 0)
	    (mem-aref size :int 1))))


ROWS

Returns number or rows in MAT.

C++: int rows, cols

Common Lisp: (ROWS (SELF (:POINTER MAT))) => :INT

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

Common Lisp:  (COLS (SELF (:POINTER MAT))) => :INT

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

Common Lisp: (POINT) => (:POINTER POINT)

C++: Point_(_Tp _x, _Tp _y)

Common Lisp:  (POINT (X :INT) (Y :INT)) => (:POINTER POINT)

C++: _Tp x, y;

Common Lisp: (POINT-X (SELF (:POINTER POINT))) => :INT

C++: _Tp x, y;

Common Lisp: (POINT-Y (SELF (:POINTER POINT))) => :INT


    Parameters:	

        SELF - A POINT construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.


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

Common Lisp:  (POINT2D (X :INT) (Y :INT)) => (:POINTER POINT2D)

C++: _Tp x, y

Common Lisp: (POINT2D-X (SELF (:POINTER POINT2D))) => :DOUBLE

C++: _Tp x, y

Common Lisp: (POINT2D-Y (SELF (:POINTER POINT2D))) => :DOUBLE


    Parameters:	

        SELF - A POINT2D construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.


POINT2D creates a 2D point with double-float coordinates (usually zero-based). Functions POINT2D-X 
and  POINT2D-Y are used to extract the x,y coordinates of the point.


(defun point2d-example (x y)
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
    (move-window (aref window-name-arr 6) 988 368)
    (move-window (aref window-name-arr 7) 1438 368)
    (move-window (aref window-name-arr 8) 88 708)
    (move-window (aref window-name-arr 9) 538 708)
    (move-window (aref window-name-arr 10) 988 708)
    (move-window (aref window-name-arr 11) 1438 708)
  "Creates a point2d with the function 
   POINT2D. Then, lists the x,y coordi-
   nates with the POINT2D functions PO-
   INT2D-X and POINT2D-Y."

  (let* ((point2d (point2d x y)))
    (format t "Pointer to POINT2D: ~a~%~%" 
	    point2d)
    (format t "POINT2D (x, y) = (~a, ~a)~%" 
	    (point2d-x point2d)
	    (point2d-y point2d))))


POINT2F

POINT2F constructor.


C++: typedef Point_<float> Point2f

Common Lisp:  (POINT2F (X :INT) (Y :INT)) => (:POINTER POINT2F)

C++: _Tp x, y

Common Lisp: (POINT2F-X (SELF (:POINTER POINT2F))) => :INT

C++: _Tp x, y

Common Lisp: (POINT2F-Y (SELF (:POINTER POINT2F))) => :INT


    Parameters:	

        SELF - A POINT2F construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.


POINT2F creates a 2D point with float coordinates (usually zero-based). Functions POINT2F-X and POI-
NT2F-Y are used to extract the x,y coordinates the point.


(defun point2f-example (x y)

  "Creates a point2f with the function 
   POINT2f. Then, lists the x,y coordi-
   nates with the POINT2F functions PO-
   INT2F-X and POINT2F-Y."

  (let* ((point2f (point2f x y)))
    (format t "Pointer to POINT2F: ~a~%~%" 
	    point2f)
    (format t "POINT2F (x, y) = (~a, ~a)~%" 
	    (point2f-x point2f)
	    (point2f-y point2f))))


POINT3I

POINT3I constructor.


C++: typedef Point3_<int> Point3i;

Common Lisp:  (POINT3I (X :INT) (Y :INT) (Z :INT)) => (:POINTER POINT3I)

C++: _Tp x, y, z

Common Lisp: (POINT3I-X (SELF (:POINTER POINT3I))) => :INT

C++: _Tp x, y, z

Common Lisp: (POINT3I-Y (SELF (:POINTER POINT3I))) => :INT


    Parameters:	

        SELF - A POINT3I construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.
        
        Z - Z-coordinate of the point.


POINT3I creates a 3D point with integer coordinates (usually zero-based). Functions POINT3I-X, POIN-
T3I-Y and POINT3I-Z are used to extract the x,y,Z coordinates of the point.


(defun point3i-example (x y z)

  "Creates a point3i with the function 
   POINT3I. Then, lists the x,y,z coor-
   dinates with the POINT3I functions 
   POINT3I-X, POINT3I-Y and POINT3I-Z."

  (let* ((point3i (point3i x y z)))
    (format t "Pointer to POINT3I: ~a~%~%" 
	    point3d)
    (format t "POINT3I (x, y, z) = (~a, ~a, ~a)~%" 
	    (point3i-x point3i)
	    (point3i-y point3i)
            (point3i-z point3i))))


POINT3D

POINT3D constructor.


C++: typedef Point3_<double> Point3d

Common Lisp:  (POINT3D (X :INT) (Y :INT) (Z :INT)) => (:POINTER POINT3D)

C++: _Tp x, y, z

Common Lisp: (POINT3D-X (SELF (:POINTER POINT3D))) => :DOUBLE

C++: _Tp x, y, z

Common Lisp: (POINT3D-Y (SELF (:POINTER POINT3D))) => :DOUBLE


    Parameters:	

        SELF - A POINT3D construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.
        
        Z - Z-coordinate of the point.


POINT3D creates a 3D point with double-float coordinates (usually zero-based). Functions POINT3D-X, 
POINT3D-Y AND POINT3D-Z are used to extract the x,y,Z coordinates the point.


(defun point3d-example (x y z)

  "Creates a point3d with the function 
   POINT3D. Then, lists the x,y,z coor-
   dinates with the POINT3D functions 
   POINT3D-X, POINT3D-Y and POINT3D-Z."

  (let* ((point3d (point3d x y z)))
    (format t "Pointer to POINT3D: ~a~%~%" 
	    point3d)
    (format t "POINT3D (x, y, z) = (~a, ~a, ~a)~%" 
	    (point3d-x point3d)
	    (point3d-y point3d)
            (point3d-z point3d))))


POINT3F

POINT3F constructor.


C++: typedef Point3_<float> Point3f

Common Lisp:  (POINT3F (X :INT) (Y :INT) (Z :INT)) => (:POINTER POINT3F)

C++: _Tp x, y, z

Common Lisp: (POINT3F-X (SELF (:POINTER POINT3F))) => :FLOAT

C++: _Tp x, y, z

Common Lisp: (POINT3F-Y (SELF (:POINTER POINT))) => :FLOAT


    Parameters:	

        SELF - A POINT3F construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.
        
        Z - Z-coordinate of the point.


POINT3F creates a 3D point with float coordinates (usually zero-based). Functions POINT3F-X, POINT3-
F-Y AND POINT3F-Z are used to extract the x,y,Z coordinates the point.


(defun point3f-example (x y z)

  "Creates a point3f with the function 
   POINT3F. Then, lists the x,y,z coor-
   dinates with the POINT3F functions 
   POINT3F-X, POINT3F-Y and POINT3F-Z."

  (let* ((point3f (point3f x y z)))
    (format t "Pointer to POINT3F: ~a~%~%" 
	    point3f)
    (format t "POINT3F (x, y, z) = (~a, ~a, ~a)~%" 
	    (point3f-x point3f)
	    (point3f-y point3f)
            (point3f-z point3f))))


EMPTY

Returns true if the array has no elements.

C++: bool Mat::empty() const

Common Lisp: (EMPTY (SELF (:POINTER MAT)))


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
    (move-window window-name 720 175)
    (imshow window-name image)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))


CV-TYPE

Returns the type of a matrix element.

C++: int Mat::type() const

Common Lisp: (CV-TYPE (SELF (:POINTER MAT)))

    Parameters:	

        SELF - A matrix(MAT)

The method returns a matrix element type. This is an identifier compatible with OpenCV's CvMat type
system, like CV_16SC3(+16SC3+ in Common Lisp) or 16-bit signed 3-channel array, and so on.


((defun mat-type-example ()

  "This function uses MAT-TYPE to find 
   the type of MAT-ONE and MAT-TWO. Sh-
   ows MAT-ONE and MAT-TWO in a window 
   so you can see what they look like."

  (let* ((mat-one (mat-zeros 1 2 +8u+))
	 (mat-two (mat-zeros 2 4 +8u+))
	 (window-name-1 "MAT-ONE - MAT-TYPE Example")
	 (window-name-2 "MAT-TWO - MAT-TYPE Example"))
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)
    (move-window window-name-1 464 175)
    (move-window window-name-2 915 175)
    (format t "MAT-ONE type
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
    (destroy-window window-name))) is ~a(+32f+). It is a Single Precision Floating Point Matrix.~%" 
	    (mat-type mat-one))
    (format t "~%MAT-TWO type is ~a(+64f+). It is a Double Precision Floating Point Matrix." 
	    (mat-type mat-two))
    (imshow window-name-1 mat-one)
    (imshow window-name-2 mat-two)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name-1)
    (destroy-window window-name-2)))


CAP-CAM

C++: VideoCapture::VideoCapture(int device)

Common Lisp: (CAP-CAM (DEVICE :INT))

    Parameters:	

        DEVICE - id of the opened video capturing device (i.e. a camera index). If there is a singl-
                 e camera connected, just pass 0.


(defun cap-cam-example (&optional 
			  (camera-index 
			   *camera-index*))

  "This function use CAP-CAM to open a video 
   capturing device (i.e. a camera index). If th-
   ere is a single camera connected, just pass 0
   (the default value of *camera-index*)."

  (with-capture (cap (ca;;; Types and structures



;;; User Interfacep-cam camera-index))
    (let ((window-name "CAP-CAM Example"))
      (if (not (cap-is-open cap)) 
	  (return-from cap-cam-example 
	    (format t "Cannot open the video camera")))
      (named-window window-name +window-autosize+)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(imshow window-name frame))
      (destroy-window window-name))))


CAP-FILE

C++: VideoCapture::VideoCapture(const string& filename)

Common Lisp: (CAP-FILE (FILENAME (:POINTER STRING*)))

    Parameters:	

        FILENAME - Name of the opened video file (eg. video.avi) or image sequence (eg. img_%02d.j-
                    pg, which will read samples like img_00.jpg, img_01.jpg, img_02.jpg, ...)


(defun cap-file-example (filename)

  "This function use CAP-FILE to open a video 
   file supplied by the parameter FILENAME."

  (with-capture (cap (cap-file filename))
    (let ((window-name "CAP-FILE Example"))
      (if (not (cap-is-open cap)) 
	  (return-from cap-file-example 
	    (format t "Cannot open the video camera")))
      (named-window window-name +window-autosize+)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(imshow window-name frame))
      (destroy-window window-name))))


AT

Returns a reference to the specified array element.

C++: template<typename T> T& Mat::at(int i) const

C++: template<typename T> const T& Mat::at(int i) const

C++: template<typename T> T& Mat::at(int i, int j)

Common Lisp: (AT-<TYPE-MAME> (SELF (:POINTER MAT)) (I :INT) (J :INT))

C++: template<typename T> const T& Mat::at(int i, int j) const

C++: template<typename T> T& Mat::at(Point pt)

C++: template<typename T> const T& Mat::at(Point pt) const

C++: template<typename T> T& Mat::at(int i, int j, int k)

C++: template<typename T> const T& Mat::at(int i, int j, int k) const

C++: template<typename T> T& Mat::at(const int* idx)

C++: template<typename T> const T& Mat::at(const int* idx) const

    Parameters:	

        I - Index along the dimension 0

        J - Index along the dimension 1

        K - Index along the dimension 2

        PT - Element position specified as Point(j,i) .

        IDX - Array of Mat::dims indices.

The template methods return a reference to the specified array element. For the sake of higher perf-
ormance, the index range checks are only performed in the Debug configuration.

Note that the variants with a single index (I) can be used to access elements of single-row or sing-
le-column 2-dimensional arrays. That is, if, for example, A is a 1 x N floating-point matrix and B 
is an M x 1 integer matrix, you can simply write (AT-FLOAT A (+ K 4)) and (AT-INT B (+ 1 (* 2 4))) instead of 
(AT-FLOAT A 0 (+ K 4)) and (AT-INT B (+ 1 (* 2 I)) 0) , respectively.


The example below initializes a Hilbert matrix:

(defun at-example ()
  (let ((h (mat-typed-0 5 5 +64f+)))
    (dotimes (i (rows h))indicate
      (dotimes (j (cols h))
	(at-double+ h i j (/ 1.0d0 (+ i j 1)))
	(princ (at-double h i j))
	(princ #\Space))
      (princ #\Newline)))) 


   The typenames associated with AT include:


        char       point       vec2b     scalar
        double     point2d     vec2d
        float      point2f     vec2f
        int        point3d     vec2i
        short      point3f     vec2s
        uchar                  vec3b
        ushort                 vec3d
                               vec3f
                               vec3i
                               vec3s
                               vec4b
                               vec4d
                               vec4f
                               vec4i
                               vec4s 




CIRCLE

Draws a circle.

C++: void circle(Mat& img, Point center, int radius, const Scalar& color, int thickness=1, int lineType=8, int shift=0)

Common Lisp: (CIRCLE (IMG (:POINTER MAT)) (CENTER (:POINTER POINT)) (RADIUS :INT) (COLOR (:POINTER SCALAR)) &OPTIONAL ((THICKNESS :INT) 1) ((LINE-TYPE :INT) 8) ((SHIFT :INT) 0))

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

  (with-capture (cap (cap-cam camera-index))
    (let* ((window-name "CICRLE Example")
	   (color (scalar 0 0 255)))
      (if (not (cap-is-open cap)) 
	  (return-from circle-example 
	    (format t "Cannot open the video camera")))
      (format t "Frame Size : ~ax~a~%~%" 
	      (cap-get cap +cap-prop-frame-width+)
	      (cap-get cap +cap-prop-frame-height+))
      (named-window window-name +window-normal+)
      (move-window window-name 720 175)
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
				(format t;;; Types and structures



;;; User Interface "ceiling has been touched~%") 
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

Common Lisp: (MAT-ZEROS (ROWS :INT) (COLS :INT) (TYPE :INT))

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
    (move-window window-name 720 175)
    (imshow window-name mat)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))


Mat::ones
todo figure out if I do all of these
Returns an array of all 1’s of the specified size and type.

C++: static MatExpr Mat::ones(int rows, int cols, int type)

Common Lisp: (MAT-ONES (ROWS :INT) (COLS :INT) (TYPE :INT))

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
    (move-window window-name 720 175)
    (imshow window-name mat)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))


MAT-EYE

Returns an identity matrix of the specified size and type.

C++: static MatExpr Mat::eye(int rows, int cols, int type)

Common Lisp: (MAT-EYE (ROWS :INT) (COLS :INT) (TYPE :INT)) => (:POINTER MAT)

C++: static MatExpr Mat::eye(Size size, int type)

Common Lisp: (MAT-EYE (S (:POINTER SIZE)) (TYPE :INT)) => (:POINTER MAT)


    Parameters:	

        ROWS - Number of rows.

        COLS - Number of columns.

        SIZE - Alternative matrix size specification as (SIZE ROWS COLS).

        TYPE - Created matrix type.


The method returns a Matlab-style identity matrix initializer, similarly to (MAT-ZEROS). Similarly 
to (MAT-ONES), you can use a scale operation to create a scaled identity matrix efficiently:

;; Make a 4x4 diagonal matrix with 0.1's on the diagonal.

(DEFPARAMETER A (SCALE (<< (MAT-EYE 4 4 +32F+)) 0.1D0))


(defun mat-eye-example ()

  "This example introduces the functions MAT-EYE a-
   nd MAT-EYE. Both identity matrix functions are 
   the same, except that MAT-EYE has row,column pa-
   rameters, and MAT-EYE has a size parameters.  B-
   oth are used to create identical 3x3 identity m-
   atrices of unsigned-char type, IDENTITY-MAT-1 a-
   nd IDENTITY-MAT-2, which are shown in a window. 

   Next an identity matrix, IDENTITY-MAT-3 is creat-
   ed and using the function SCALE is scaled by 
   0.1 . Then both the pre-scaled version and the s-
   caled version are shown in window. This allows y-
   ou to see the fact that, because they are of typ-
   e +32F+(single-float) matrices, their elements a-
   re shown as colors, not numbers, unlike the matr-
   ices in the top 2 windows. So an identity matrix 
   which has a diagonal that is all ones, it's diag-
   onal would be represented as pure white. An iden-
   tity matrix whose diagonal is all 0.1, it's diag-
   onal would be represented as a dark, dark grey V-
   ery close to black...A colored boolean.

   Note: The PROMOTE(<<) function was needed in the 
   SCALE function to cooerce IDENTITY-MAT-3 to 
   MAT-EXPR from MAT type for the scaling operation. 
   The FORCE(>>) function is used in IMSHOW to coer-
   ce SCALED-IDENTITY-MAT-3 back to MAT."

  (let* ((identity-mat-1 (mat-eye 3 3 +8u+))
         (identity-mat-2 (mat-eye (size 3 3) +8u+))
         (identity-mat-3 (mat-eye 4 4 +32f+))
	 (scaled-identity-mat-3 (scale (<< identity-mat-3) 0.1d0))
	 (window-name-1 "IDENTITY-MAT-1 - MAT-EYE Example")
         (window-name-2 "IDENTITY-MAT-2 - MAT-EYE Example")
	 (window-name-3 "IDENTITY-MAT-3 - MAT-EYE Example")
         (window-name-4 "SCALED-IDENTITY-MAT-3 - MAT-EYE Example"))
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)
    (named-window window-name-3 +window-normal+)
    (named-window window-name-4 +window-normal+)

    (move-window window-name-1 485 98)
    (move-window window-name-2 894 98)
    (move-window window-name-3 485 444)
    (move-window window-name-4 894 444)
    (imshow window-name-1 identity-mat-1)
    (imshow window-name-2 identity-mat-2)
    (imshow window-name-3 identity-mat-3)
    (imshow window-name-4  (>> scaled-identity-mat-3))
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name-1)
    (destroy-window window-name-2)
    (destroy-window window-name-3)
    (destroy-window window-name-4)))


GET-TICK-COUNT

Returns the number of ticks.

C++: int64 getTickCount()

Common Lisp: (GET-TICK-COUNT) => :INT64

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

Common Lisp: (GET-TICK-FREQUENCY) => :DOUBLE

The function returns the nu
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
    (destroy-window window-name)))mber of ticks per second. That is, the following code computes the execu-
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


MAT-EXPR-T

Transposes a matrix.

C++: MatExpr Mat::t() const

Common Lisp: (MAT-EXPR-T (SELF (:POINTER MAT))) => (:POINTER MAT-EXPR)

The method performs matrix transposition by means of matrix expressions. It does not perform the ac-
tual transposition but returns a temporary matrix transposition object that can be further used as 
a part of more complex matrix expressions or can be assigned to a matrix.

  Parameters:	

        SELF - Input matrix


; todo - finish example


FORCE

Coverts a MAT-EXPR to MAT

Common Lisp: (FORCE (SELF (:POINTER MAT-EXPR))) => (:POINTER MAT)

Common Lisp: (>> (SELF (:POINTER MAT-EXPR))) => (:POINTER MAT)

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
    (move-window window-name 720 175)
    (imshow window-name  (>> out))
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))



MAT-TYPED-0

Creates an empty matrix of type TYPE.

Common Lisp: (MAT-TYPED-0) (ROWS :INT) (COLS :INT) (TYPE :INT)) => (:POINTER MAT)

    Parameters:	

        ROWS - The number of rows.
    
        COLS - The number of colounns

        TYPE - The type of the matrix


(defun mat-typed-0-example ()

  "In this example an 8-bit unsigned matrix is 
   created and the empty matrix is shown in a 
   window."

  (let* ((mat (mat-typed-0 4 4 +32s+))
	 (window-name "MAT-TYPED-0 Example"))
    (named-window window-name +window-normal+)
    (move-window window-name 720 175)
    (imshow window-name mat)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))

         
DIAG

Extracts a diagonal from a matrix, or creates a diagonal matrix.

C++: Mat Mat::diag(int d=0 ) const

Common Lisp: (MAT-DIAG (SELF (:POINTER MAT)) (D :INT)) => (:POINTER MAT)

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

  (let* ((data (foreign-alloc :int :initial-contents 
			      '(1 2 3 4 5 6 7 8 9)))
	 (mat (mat-data 3 3 +32s+ data))
	 (diag (diag mat 0)))
    (dotimes (i 3)
      (format t "~a" (at-int diag i 0))
      (princ #\Newline))))


MUL

Finds the product of two matrices.

C++: MatExpr * operator

Common Lisp: (MUL (M1 (:POINTER MAT)) (M2 (:POINTER MAT))) => (:POINTER MAT-EXPR)


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

  (let* ((m1-data (foreign-alloc :float :initial-contents 
				 '(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0 
				   6.0f0 7.0f0 8.0f0 9.0f0)))
	 (m2-data (foreign-alloc :float :initial-contents 
				 '(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0 
				   6.0f0 7.0f0 8.0f0 9.0f0)))
	 (m1 (mat-data 3 3 +32f+ m1-data))
         (m2 (mat-data 3 3 +32f+ m2-data))
         (result (mul m1 m2)))
    (dotimes (i (rows m1))
      (dotimes (j (cols m1))
	(format t "~a" (at-float m1 i j))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    (dotimes (i (rows m2))
      (dotimes (j (cols m2))
	(format t "~a" (at-float m2 i j))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    (dotimes (i 3)
      (dotimes (j 3)
	(format t "~a" (at-float (>> result) i j))
	(princ #\Space))
      (princ #\Newline))))



ADD

Adds two matrices.

C++: MatExpr + operator

Common Lisp: (ADD (M1 (:POINTER MAT)) (M2 (:POINTER MAT))) => (:POINTER MAT-EXPR)


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

  (let* ((m1-data (foreign-alloc :uint :initial-contents 
				 '(1 2 3 4 5 6 7 8 9)))
	 (m2-data (foreign-alloc :uint :initial-contents 
				 '(1 2 3 4 5 6 7 8 9)))
	 (m1 (mat-data 3 3 +32s+ m1-data))
         (m2 (mat-data 3 3 +32s+ m2-data))
         (result (add m1 m2)))
    (dotimes (i (rows m1))
      (dotimes (j (cols m1))
	(format t "~a" (at-int m1 i j))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    (dotimes (i (rows m2))
      (dotimes (j (cols m2))
	(format t "~a" (at-int m2 i j))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    (dotimes (i 3)
      (dotimes (j 3)
	(format t "~a" (at-int (>> result) i j))
	(princ #\Space))
      (princ #\Newline))))


SUB

Subtracts matrix M1 from matrix M2

C++: MatExpr - operator

Common Lisp: (SUB (M1 (:POINTER MAT)) (M2 (:POINTER MAT))) => (:POINTER MAT-EXPR)


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

  (let* ((m1-data (foreign-alloc :uint :initial-contents 
				 '(53 62 85 64 23 97 52 16 12)))
	 (m2-data (foreign-alloc :uint :initial-contents 
				 '(64 22 64
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
    (destroy-window window-name))) 15 11 17 42 16 88)))
	 (m1 (mat-data 3 3 +32s+ m1-data))
         (m2 (mat-data 3 3 +32s+ m2-data))
         (result (sub m1 m2)))
    (dotimes (i (rows m1))
      (dotimes (j (cols m1))
	(format t "~a" (at-int m1 i j))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    (dotimes (i (rows m2))
      (dotimes (j (cols m2))
	(format t "~a" (at-int m2 i j))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    (dotimes (i 3)
      (dotimes (j 3)indicate
	(format t "~a" (at-int (>> result) i j))
	(princ #\Space))
      (princ #\Newline))))


DIV

Divides matrix M1 by matrix M2.

C++: MatExpr / operator

Common Lisp: (DIV (M1 (:POINTER MAT)) (M2 (:POINTER MAT))) => (:POINTER MAT-EXPR)


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

  (let* ((m1-data (foreign-alloc :float :initial-contents 
				 '(53.0f0 62.0f0 85.0f0 64.0f0 23.0f0 
				   97.0f0 52.0f0 16.0f0 12.0f0)))
	 (m2-data (foreign-alloc :float :initial-contents 
				 '(64.0f0 22.0f0 64.0f0 15.0f0 11.0f0 
				   17.0f0 42.0f0 16.0f0 88.0f0)))
	 (m1 (mat-data 3 3 +32f+ m1-data))
         (m2 (mat-data 3 3 +32f+ m2-data))
         (result (div m1 m2)))
    (dotimes (i (rows m1))
      (dotimes (j (cols m1))
	(format t "~a" (at-float m1 i j))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    (dotimes (i (rows m2))
      (dotimes (j (cols m2))
	(format t "~a" (at-float m2 i j))
	(princ #\Space))
      (princ #\Newline))
    (format t "~%~%")
    (dotimes (i 3)
      (dotimes (j 3)
	(format t "~a" (at-float (>> result) i j))
	(princ #\Space))
      (princ #\Newline))))


VIDEO-WRITER

VIDEO-WRITER constructors

C++: VideoWriter::VideoWriter()

Common Lisp: (VIDEO-WRITER0)

C++: VideoWriter::VideoWriter(const string& filename, int fourcc, double fps, Size frameSize, bool isColor=true)

Common Lisp: (VIDEO-WRITER (FILENAME (:POINTER STRING*)) (FOURCC :INT) (FPS :DOUBLE) 
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

  (with-capture (cap (cap-cam camera-index)) ; Open the video camera no. 0
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
      (move-window window-name 720 175)
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


SIZE

SIZE constructor

C++: Size_(_Tp _width, _Tp _height);
     typedef Size_<int> Size2i;
     typedef Size2i Size;

Common Lisp: (SIZE (WIDTH :INT) (HEIGHT :INT)) => (:POINTER SIZE)

C++: _Tp width, height

Common Lisp: (WIDTH (SELF (:POINTER SIZE))) => :INT

C++: _Tp width, height

Common Lisp: (HEIGHT (SELF (:POINTER SIZE))) => :INT


    Parameters:	

        SELF - A SIZE construct.

        WIDTH - The width of SIZE.
        
        HEIGHT - The height of SIZE.


The function SIZE stores size values.

The function WIDTH Finds the width of a SIZE construct.

The function HEIGHT Finds the height of a SIZE construct.


(defun size-example ()

  "Initializes SIZE constructor with values 
   and prints them"

  (let* ((size (size 640 480)))
    (format t "Width = ~a~%" (width size))
    (format t "Height = ~a" (height size))))


VIDEO-WRITER-IS-OPEN

Returns true if video writer has been successfully initialized.

C++: bool VideoWriter::isOpened()

Common Lisp: (VIDEO-WRITER-IS-OPEN (SELF (:POINTER VIDEO-WRITER)))


(defun video-writer-is-open-example (filename &optional (camera-index *camera-index*))

  (with-capture (cap (cap-cam camera-index)) ; Open the video camera no. 0
    (let* (; Initialize the VideoWriter object 
	   (o-video-writer (video-writer filename 1196444237 ; todo
					 20.0d0 (size 640 480) 1)))
      (format t "If VIDEO-WRITER is open a T will be displayed, else NIL: ~a"
	      (video-writer-is-open o-video-writer)))))


VIDEO-WRITER-WRITE

Writes the next video frame

C++: VideoWriter& VideoWriter::operator<<(const Mat& indicateimage)

Common Lisp: (VIDEO-WRITER-WRITE (SELF (:POINTER VIDEO-WRITER)) (IMAGE (:POINTER MAT)))

    Parameters:	

        SELF - Pointer to VIDEO-WRITER

        IMAGE - The written frame

The function VIDEO-WRITER-WRITE writes the specified image to video file. It must have the same siz-
e as has been specified when opening the video writer.


(defun video-writer-write-example (filename &optional 
					      (camera-index *camera-index*))

  (with-capture (cap (cap-cam camera-index)) 
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
      (move-window window-name 720 175)
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

Common Lisp: (MAT-CLONE (SELF (:POINTER MAT))) => (:POINTER MAT)


    Parameters:	

        SELF - Pointer to a matrix


The method creates a full copy of the array. The original TODO step[] is not taken into account. So
, the array copy is a continuous array occupying (* (TOTAL) (ELEM-SIZE)) bytes.


(defun clone-example ()

        ; Create data
  (let* ((m1-data (foreign-alloc :float :initial-contents 
				 '(53.0f0 62.0f0 85.0f0 64.0f0 23.0f0 
				   97.0f0 52.0f0 16.0f0 12.0f0)))
         ; Create matrix M1 and fill it with data
	 (m1 (mat-data 3 3 +32f+ m1-data))
         ; Create a clone of matrix M1 called M2
         (m2 (clone m1)))
    (format t "~%~%")
    ; Print the elements of natrix M2 in a loop
    (dotimes (i (rows m2))
      (dotimes (j (cols m2))
        ; AT-FLOAT retrieves elements of M2,  MEM-AREF 
        ; dereferences AT-FLOAT's return, FORMAT print-
        ; s the elements to the screen 
	(format t "~a" (mem-aref (at-float m2 i j) :float 0))
	(princ #\Space))
      (princ #\Newline))))


MAT-STEP1

Returns a normalized step.

C++: size_t Mat::step1(int i=0 ) const

Common Lisp: (STEP1 (SELF (:POINTER MAT))) => :UNSIGNED-INT

The method returns a matrix step divided by (ELEM-SIZE1) . It can be useful to quickly access an ar
bitrary matrix element.



TOTAL

Returns the total number of array elements.

C++: size_t Mat::total() const

Common Lisp: (TOTAL (SELF (:POINTER MAT))) => :UNSIGNED-INT


    Parameters:	

        SELF - Pointer to a matrix


The method returns the number of array elements (a number of pixels if the array represents an imag-
e).


(defun total-example ()

  "In this function, TOTAL returns the total 
   number of array elements in MAT1 and MAT2"

  (let* ((data (foreign-alloc :int :initial-contents 
			      '(1 2 3 4)))
	 (mat1 (mat-data 2 2 +32s+ data))
	 (mat2 (mat-typed-0 100 100 +32s+))
	 (total1 (total mat1))
	 (total2 (total mat2)))
    (format t "Total mumber of elements in MAT1 = ~a~%~%" total1)
    (format t "Total mumber of elements in MAT2 = ~a" total2)))



ROI

Returns matrix header corresponding to the rectangular sub-array of an input matrix.

C++: Mat::Mat(const Mat& m, const Rect& roi)

Common Lisp: (ROI (SELF (:POINTER MAT)) (ROI (:POINTER RECT))) => (:POINTER MAT)


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

  (with-capture (cap (cap-cam camera-index))
    (let* ((window-name "ROI Example"))
      (if (not (cap-is-open cap)) 
	  (return-from roi-example 
	    (format t "Cannot open the video camera")))
      (format t "Frame Size : ~ax~a~%~%" 
	      (cap-get cap +cap-prop-frame-width+)
	      (cap-get cap +cap-prop-frame-height+))
      (named-window window-name +window-normal+)
      (move-window window-name 720 175)
      (do* ((frame 0))
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))
	(cap-read cap frame)
	(setf region-of-interest (rect x y 40 40))
        (setf frame (roi frame region-of-interest))
	(imshow window-name frame)
	(if (= x the-right-wall) (progn 
				   (format t "right wall has been toucindicatehed~%") 
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

Common Lisp:  (RECT (X :INT) (Y :INT) (:WIDTH :INT) (HEIGHT :INT)) => (:POINTER RECT)

C++: Point_<_Tp> tl() const;

Common Lisp: (TL (SELF (:POINTER RECT))) => (:POINTER POINT)

C++: Point_<_Tp> br() const;

Common Lisp: (BR (SELF (:POINTER RECT))) => (:POINTER POINT)

C++: Size_<_Tp> size() const;

Common Lisp: (SZ (SELF (:POINTER RECT))) => (:POINTER SIZE)


    Parameters:	

        SELF - A rectangle.

        X - x-coordinate of the rectangle.

        Y -	y-coordinate of the rectangle.

        WIDTH - Width of the rectangle.

        HEIGHT - Height of the rectangle.


The function RECT stores coordinates of a rectangle.

The function TL retrieves the top-left corner of the rectangle.

The function BR retrieves t
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
    (destroy-window window-name)))he bottom-right corner of the rectangle.

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

Common Lisp: (DOT (SELF (:POINTER POINT)) (OTHER (:POINTER POINT))) => :INT

Common Lisp: (DOT2F (SELF (:POINTER POINT2F)) (OTHER (:POINTER POINT2F))) => :FLOAT

Common Lisp: (DOT2D (SELF (:POINTER POINT2D)) (OTHER (:POINTER POINT2D))) => :DOUBLE

C++"  _Tp dot(const Point3_& pt) const;

Common Lisp: (DOT3I (SELF (:POINTER POINT3I)) (OTHER (:POINTER POINT3I))) => :INT

Common Lisp: (DOT3F (SELF (:POINTERRead POINT3F)) (OTHER (:POINTER POINT3F))) => :FLOAT

Common Lisp: (DOT3D (SELF (:POINTER POINT)) (OTHER (:POINTER POINT))) => :INT


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
   find the dot product of 2 po;;; Basic Structuresint3d con-
   structs P1 and P2."

  (let* ((p1 (point3d 13.0d0 14.0d0 15.0d0))
	 (p2 (point3d 16.0d0 17.0d0 18.0d0)))
    (format t "The dot product of P1 and P2 = ~a~%~%"  
	    (dot3d p1 p2))))


IN-RANGE

Checks if array elements lie between the elements of two other arrays.

C++: void inRange(InputArray src, InputArray lowerb, InputArray upperb, OutputArray dst)

Common Lisp: (IN-RANGE-S (SRC (:POINTER MAT)) (LOWERB (:POINTER SCALAR)) (UPPERB (:POINTER SCALAR)) (DEST (:POINTER MAT)) => :VOID


    Parameters:	

        SRC - first input array.

        LOWERB - A scalar.

        UPPERB - A scalar.

        DEST - output array of the same size as SRC and +8U+ type.
MAT-SIZE

All the arrays must have the same type, except the destination, and the same size (or ROI size).



(defun in-range-s-example (&optional (camera-index *camera-index*) 
			     (width 640)
			     (height 480))

  (with-capture (cap (cap-cam camera-index))
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
      (move-window window-name-1 464 175)
      (move-window window-name-2 915 175)
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
  (with-capture (cap (cap-cam camera-index))
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
      (move-window window-name-1 464 175)
      (move-window window-name-2 915 175)
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

Common Lisp: (CVT-COLOR (SRC (:POINTER MAT)) (DEST (:POINTER MAT)) (CODE :INT) ((DEST-CN :INT) 0))


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

  (with-capture (cap (cap-cam camera-index))
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

Common Lisp: (SCALE (SELF (:POINTER MAT-EXPR)) (ALPHA :DOUBLE)) => (:POINTER MAT-EXPR)


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

  (let* ((data (foreign-alloc :float :initial-contents 
			      '(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0 
                                6.0f0 7.0f0 8.0f0 9.0f0)))
	 (mat (mat-data 3 3 +32f+ data))
	 (scaled-mat (scale (<< mat) (/ 1d0 10d0))))
    (dotimes (i 3)
      (dotimes (j 3)
	(format t "~a" (at-float (>> scaled-mat) i j))
	(princ #\Space))
      (princ #\Newline))))


DRAW-MATCHES

Draws the found matches of keypoints from two images.

C++: void drawMatches(const Mat& img1, const vector<KeyPoint>& keypoints1, const Mat& img2, const vector<KeyPoint>& keypoints2, const vector<DMatch>& matches1to2, Mat& outImg, const Scalar& matchColor=Scalar::all(-1), const Scalar& singlePointColor=Scalar::all(-1), const vector<char>& matchesMask=vector<char>(), int flags=DrawMatchesFlags::DEFAULT )

Common Lisp: (DRAW-MATCHES (IMG1 (:POINTER MAT)) (KEYPOINTS1 (:POINTER KEYPOINT)) (IMG2 (:POINTER MAT)) (KEYPOINTS2 (:POINTER KEYPOINT)) (MATCHES1TO2 (:POINTER VECTOR-DMATCH)) (OUTIMG (:POINTER MAT)) (MATCH-COLOR (:POINTER SCALAR)) (SINGLE-POINT-COLOR (:POINTER SCALAR)) &OPTIONAL ((MATCHES-MASK (:POINTER VECTOR-CHAR)) (VECTOR-CHAR)) ((FLAGS :INT) +DEFAULT+))
indicate
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

Common Lisp: (BRISK &OPTIONAL ((THRESH :INT) 30) ((OCTAVES :INT) 3) ((PATTERN-SCALE :FLOAT) 1.0F0) => (:POINTER BRISK)

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
    (move-window (aref window-name-arr 6) 988 368)indicate
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

Common Lisp: (FEAT-DETECTOR-CREATE (SELF (:POINTER FEATURE-DETECTOR)) (DETECTOR-TYPE :STRING)) => (:POINTER FEATURE-DETECTOR) 


    Parameters:	

        SELF - A pointer to a BRISK construct

        DETECTOR-TYPE - Feature detector type.

The following detector types are supported:

    "FAST" - FastFeatureDetector
    "STAR" - StarFeatureDetector
    "SIFT" - SIFT (nonfree module)
    "SURF" - SURF (nonfree module)
    "ORB" - ORB
    "BRISK" - BRISK
    "MSER" - MSER
    "GFTT" - GoodFeaturesToTrackDetector
    "HARRIS" - GoodFeaturesToTrackDetector with Harris detector enabled
    "Dense" - DenseFeatureDetector
    "SimpleBlob" - SimpleBlobDetector

Also a combined format is supported: feature detector adapter name ( "Grid" - GridAdaptedFeatureDetector, "Pyramid" - PyramidAdaptedFeatureDetector ) + feature detector name (see above), for example: "GridFAST", "PyramidSTAR" .


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
    (feat-detector-detect
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
    (destroy-window window-name))) briskd gray-a keypoints-a)
    ;; Compute the descriptors for a set of keypoints detected in GRAY-A
    (feat-2d-compute briskd gray-a keypoints-a descriptors-a)
    ;; detect keypoints in the image GRAY-B
    (feat-detector-detect briskd gray-b keypoints-b)
    ;; Compute the descriptors for a set of keypoints detected in GRAY-B
    (feat-2d-compute briskd gray-b keypoints-b descriptors-b)
    ;; find the best match for each descriptor
    (descrip-matcher-match matcher descriptors-a descriptors-b matches)
    (named-window window-name +window-normal+)
    (move-window window-name 720 175)
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

Common Lisp: (SURF0) => (:POINTER SURF)

C++: SURF::SURF(double hessianThreshold, int nOctaves=4, int nOctaveLayers=2, bool extended=true, bool upright=false )

Common Lisp: (SURF5 (HESSIAN-THRESHOLD :DOUBLE) &OPTIONAL ((N-OCTAVES :INT) 4) 
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
    (move-window window-name 720 175)
    ;;-- Draw matches
    (draw-matches img-1 keypoints-1 img-2 keypoints-2 matches img-matches)
    ;;-- Show detected matches
    (imshow window-name img-matches)
    (loop while (not (= ;;; Types and structures



;;; User Interface(wait-key 0) 27)))
    (destroy-window window-name)))


CREATE-TRACKBAR

Creates a trackbar and attaches it to the specified window.

C++: int createTrackbar(const string& trackbarname, const string& winname, int* value, int count, TrackbarCallback onChange=0, void* userdata=0)

Common Lisp:  (CREATE-TRACKBAR (TRACKBARNAME :STRING) (WINNAME :STRING) (VALUE :POINTER) (COUNT :INT) &OPTIONAL ((ON-CHANGE (:POINTER TRACKBAR-CALLBACK)) (NULL-POINTER)) ((USERDATA :POINTER) (NULL-POINTER))) => :INT

    
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




;; a callback function called by the CREATE-TRACKBAR
;; ON-CHANGE parameter...a HELLO-WORLD function.
(defcallback hello-world-brightness :void ((pos :int) (ptr :pointer))
  (format t "Hello World!~%~%~a~a~%~%" (mem-aref ptr :string 0) pos))

;; another HELLO-WORLD callback function
(defcallback hello-world-contrast :void ((pos :int) (ptr :pointer))
  (format t "Hello World!~%~%~a~a~%~%" (mem-aref ptr :string 0) pos))



(defun create-trackbar-example (filename)
  ;; read in image supplied by filename parameter
  (let ((src (imread filename 1))

	(window-name "Adjust brightness and contrast by moving the sliders.")

        ;; allocate two :int pointers that trackbar can adjust
	(slider-1-value (foreign-alloc :int 
				       :initial-contents '(50)))
	(slider-2-value (foreign-alloc :int 
				       :initial-contents '(50))))
    (if (empty src) 
	(return-from create-trackbar-example
	  (format t "Image not loaded")))
    (named-window window-name 1)
    (move-window window-name 720 175)
    (do* ((dest 0)
	  (brightness 0)
	  (contrast 0)
	  ;; data to be passed to HELLO-WORLD-BRIGHTNESS callback function
	  (userdata-1 (foreign-alloc :string :initial-element "Brightness =  "))
	  ;; data to be passed to HELLO-WORLD-CONTRAST callback function
	  (userdata-2 (foreign-alloc :string :initial-element "Contrast = ")))
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
    (destroy-window window-name)))


SET-MOUSE-CALLBACK

Sets mouse handler for the specified window

C++: void setMouseCallback(const string& winname, MouseCallback onMouse, void* userdata=0 )

Common Lisp: (SET-MOUSE-CALLBACK (WINNAME :STRING) (ON-MOUSE (:POINTER MOUSE-CALLBACK)) (USERDATA :VOID)) => :VOID

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
  (if (= event +event-lbuttonup+)Read
      (format t "Left button of the mouse up (~a, ~a)~%~%" x y))
  (if (= event +event-rbuttonup+)
      (format t "Right button of the mouse up (~a, ~a)~%~%" x y))
  (if (= event +event-mbuttonup+)
      (format t "Middle button of the mouse up (~a, ~a)~%~%" x y))
  (if (= flags +event-lbuttondblclk+)
      (format t "Left button double-click flag triggered (~a, ~a)~%~%" x y))
  (if (= flags +event-rbuttondblclk+)
      (format t "Right button double-click flag triggered (~a, ~a)~%~%" x y))
  (if (= flags +event-mbuttondblclk+)
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
	(userdata (foreign-alloc :string 
				 :initial-element "USERDATA output")))
    (if (empty src) 
	(return-from set-mouse-callback-example
	  (format t "Image not loaded")))
    (named-window window-name 1)
    (move-window window-name 720 175)
    ;; Set mouse handler for the window, which passes a constant 
    ;; stream of mouse and mouse button positional data to the f-
    ;; unction above.  Also passes the contents of USERDATA to t-
    ;; he above function CALL-BACK-FUNC.
    (set-mouse-callback window-name (callback call-back-func) userdata)
    (imshow window-name src)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))



DATA

Pointer to MAT data.

C++: uchar* data

Common Lisp: (DATA (SELF (:POINTER MAT)) ) => :POINTER

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
  ;; read image
  (let* ((img (imread filename 1))
	 ;; variables used to access the data/pixel color values
         ;; INPUT is a pointer to IMG data
	 (input (data img))
         ;; variables used to hold the BGR image pixel values
	 (b 0)
	 (g 0)
	 (r 0)
	 (window-name "DATA Example"))
    (if (empty img) 
	(return-from data-example 
	  (format t "Image not loaded")))
    (named-window window-name +window-normal+)
    (move-window window-name 720 175)
   ;; in a loop access IMG pixel data using the STEP* function
    (dotimes (i (rows img))
      (dotimes (j (cols img))
	(setf b (mem-aref input :uchar 
			  (+  (* (step* img) j) i )))
	(setf g (mem-aref input :uchar 
			  (+ (+  (* (step* img) j) i ) 1)))
	(setf r (mem-aref input :uchar 
			  (+ (+  (* (step* img) j) i ) 2)))
        ;; print the data so you can see this example works.
        ;; warning: may print alot of data, you may need to 
        ;; restart your Lisp implementation. Save all your 
        ;; work befor you run this example.
	(format t "(~a,~a,~a) " b g r)))
    (imshow window-name img)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)))


STEP*

Used to compute address of a matrix element

C++: MStep step

Common Lisp: (STEP (SELF (:POINTER MAT))) => :UNSIGNED-INT

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
    (move-window window-name 720 175)
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


Common Lisp: ($ FORM &optional (COUNT-FORM 1000000) ) => RESULT*

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

Common Lisp: (UNIFORM (RNG (:POINTER RNG)) (A :DOUBLE) (B :DOUBLE)) => :DOUBLE

C++: float RNG::uniform(float a, float b)

Common Lisp: (UNIFORM (RNG (:POINTER RNG)) (A :FLOAT) (B :FLOAT)) => :FLOAT

C++: double RNG::uniform(double a, double b)

Common Lisp: (UNIFORM (RNG (:POINTER RNG)) (A :INT) (B :INT)) => :INT


    Parameters:	

        RNG - An RNG construct

        A - lower inclusive boundary of the returned random numbers.

        B - upper non-inclusive boundary of the returned random numbers.

The methods transform the state using the MWC algorithm and return the next uniformly-distributed r-
andom number of the specified type, deduced from the input parameter type, from the range (a, b) . 
There is a nuance illustrated by the following example:


(defparameter rng (rng))

;; always produce;;; Basic Structuress 0
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

Common Lisp: (RNG)

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

Common Lisp: See description.


    Parameters:	

       See description.


The bindings for the C++ vector class, so far, are:


Common Lisp:                      
-------------        

VECTOR-CHAR(vector<char>)

VECTOR-DMATCH(vector<DMatch>)      

VECTOR-FLOAT(vector<float>)

VECTOR-INT(vector<int>)

VECTOR-KEYPOINT(vector<KeyPoint>)

VECTOR-POINT2F(vector<Point2f>)



Description:


Vectors with numbers as elements, VECTOR-CHAR, VECTOR-FLOAT and VECTOR-INT operate as follows: 
(I use VECTOR-FLOAT as an example of the 3 vectors).



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



Vectors with, pointers to vectors with numbers, as their elements, VECTOR-DMATCH, VECTOR-KEYPOINT a-
nd VECTOR-POINT2F operate as follows:(I use VECTOR-POINT2F as an example of the three vectors.)



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

LISP-CV> (VECTOR-POINT2F A 0 0) <--- Access the 0th element of the 0th POINT2F in vector A.

1.0

LISP-CV> (VECTOR-POINT2F A 0 1) <--- Access the 1st element of the 0th POINT2F in vector A.

2.0

LISP-CV> (VECTOR-POINT2F A 1 0) <--- Access the 0th element of the 1st POINT2F in vector A.

3.0

LISP-CV> (VECTOR-POINT2F A 1 1) <--- Access the 1st element of the 1st POINT2F in vector A.

4.0



ASSGN-VAL

Assign a scalar value to a matrix.

C++: MatExpr = operator

Common Lisp: (ASSGN-VAL (SELF (:POINTER MAT)) (S (:POINTER SCALAR))) => (:POINTER MAT)

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
    (move-window window-name-1 464 175)
    (move-window window-name-2 915 175)
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

;==============================================MACROS===============================================

ALLOC

Macro for CFFI::FOREIGN-ALLOC

CFFI: - Function: foreign-alloc type &key initial-element initial-contents (count 1) null-terminated-p ⇒ pointer

Common Lisp: (ALLOC TYPE VALUE) => :POINTER


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

Common Lisp: (FREE PTR) => undefined


    Parameters:	

        PTR - A foreign pointer.


Example:


LCV> (DEFPARAMETER A (ALLOC :INT 55))

A

LCV> (MEM-REF A :INT)

55

LCV> (FREE A)

NIL

LCV> (MEM-REF A :INT)

0
