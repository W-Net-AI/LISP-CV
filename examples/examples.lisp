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

  "Opens the image FILENAME and shows it in a Extracts a diagonal from a matrix, or creates a diagonal matrix.
   window with IMSHOW."

  (let* ((filename (foreign-alloc 
		    :string :initial-element filename))
	 (image (imread filename 1))
	 (window-name (foreign-alloc 
		       :string :initial-element 
		       "IMSHOW Example")))
    (if (cffi:null-pointer-p image) 
	(return-from imshow-example 
	  (format t "Image not loaded")))
    (named-window window-name +window-normal+)
    (move-window window-name 720 175)
    (imshow window-name image)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)
    (foreign-free filename)
    (foreign-free window-name)))


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

  (let* ((filename (foreign-alloc 
		    :string :initial-element filename))
	 (image (imread filename 1))
	 (window-name (foreign-alloc 
		       :string :initial-element 
		       "IMREAD Example")))
    (if (cffi:null-pointer-p image) 
	(return-from imread-example 
	  (format t "Image not loaded")))
    (named-window window-name +window-normal+)
    (move-window window-name 720 175)
    (imshow window-name image)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)
    (foreign-free filename)
    (foreign-free window-name)))


NAMED-WINDOW

Creates a window.

C++: void namedWindow(const string& winname, int flags=WINDOW_AUTOSIZE )

Common Lisp: (NAMED-WINDOW (WINNAME (:POINTER STRING*)) &OPTIONAL ((FLAGS :INT) +WINDOW-AUTOSIZE+))

    Parameters:	

        NAME - Name of the window in the window caption that may be used as a window identifier.

        FLAGS -

        Flags of the window. The supported flags are:

            +WINDOW-NORMAL+ If this is set, the user can resize the window (no constraint).

            +WINDOW-AUTOSIZE+ If this is set, the window size is automatically adjusted to fit the 
                              displayed image (see (IMSHOW) ), and you cannot change the window siz-
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

  (let* ((window-name (foreign-alloc 
		       :string :initial-element 
		       "NAMED-WINDOW Example")))
    (named-window window-name +window-normal+)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)
    (foreign-free window-name)))


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

  (let* ((window-name (foreign-alloc 
		       :string :initial-element 
		       "DESTROY-WINDOW Example")))
    (named-window window-name +window-normal+)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)
    (foreign-free window-name)))


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

  (let* ((window-name (foreign-alloc 
		       :string :initial-element 
		       "MOVE-WINDOW Example")))
    (named-window window-name +window-normal+)
    (move-window window-name 720 175)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)
    (foreign-free window-name)))


WAIT-KEY

Waits for a pressed key.

C++: int waitKey(int delay=0)

Ccommon Lisp: (WAIT-KEY &OPTIONAL ((DELAY :INT) 0))

    Parameters:	DELAY - Delay in milliseconds. 0 is the special value that means “forever”.

The function WAIT-KEY waits for a key event infinitely (when \texttt{delay}\leq 0 TODO GP) or for d-
elay milliseconds, when it is positive. Since the OS has a minimum time between switching threads, 
the function will not wait exactly delay ms, it will wait at least delay ms, depending on what else
is running on your computer at that time. It returns the code of the pressed key or -1 if no key wa-
s pressed before the specified time had elapsed.

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

  (let* ((window-name (foreign-alloc 
		       :string :initial-element 
		       "WAIT-KEY Example")))
    (named-window window-name +window-normal+)
    (move-window window-name 720 175)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)
    (foreign-free window-name)))


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


(defun cap-get-example (&optional (camera-index *camera-index*) 
			  (width *default-width*)
			  (height *default-height*))

  "Gets the width and height of the camera capture 
   with the function CAP-GET and prints it."

  (with-capture (cap (cap-cam camera-index))
    (let ((window-name (foreign-alloc 
			:string :initial-element 
			"CAP-GET Example")))
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
      (destroy-window window-name)
      (foreign-free window-name))))


CAP-SET

Sets a property in the VIDEO-CAPTURE

C++: bool VideoCapture::set(int propId, double value)

Common Lisp: (CAP-SET (SELF (:POINTER VIDEO-CAPTURE)) (PROP-ID :INT) (VALUE :DOUBLE))

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


(defun cap-set-example (&optional (camera-index *camera-index*))

  "Changes the brightness level of the camera feed 
   with the function CAP-SET and then prints the b-
   rightness level."

  (with-capture (cap (cap-cam camera-index))
    (let ((window-name (foreign-alloc 
			:string :initial-element 
			"CAP-SET Example")))
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
      (destroy-window window-name)
      (foreign-free window-name))))


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


(defun cap-read-example (&optional (camera-index *camera-index*))

  "Grabs, decodes and returns the next video frame 
   with the function CAP-READ and then shows it in 
   a window with the function IMSHOW."

  (with-capture (cap (cap-cam camera-index))
    (let ((window-name (foreign-alloc 
			:string :initial-element 
			"CAP-READ Example")))
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
      (destroy-window window-name)
      (foreign-free window-name))))


CAP-RELEASE

Closes video file or capturing device.

C++: void VideoCapture::release()

Common Lisp: (CAP-RELEASE (SELF (:POINTER VIDEO-CAPTURE)))

The methods are automatically called by subsequent (CAP-OPEN) and by VIDEO-CAPTURE destructor.

Parameters:	

         SELF - The VIDEO-CAPTURE structure.


(defun cap-release-example (&optional (camera-index *camera-index*))

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
	(window-name (foreign-alloc 
		      :string :initial-element 
		      "CAP-RELEASE Example")))
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
    (destroy-window window-name)
    (foreign-free window-name)))


WITH-CAPTURE

Ensures CAP-RELEASE gets called on captures.

Common Lisp: (WITH-CAPTURE (CAPTURE-VAR CAP)) &BODY BODY)

Parameters:	

         CAPTURE-VAR - A variable representing the function used to open video file or a capturing 
                       device. Similar to the variable in a LET statement.

         CAP - The function used to open video file or a capturing device for video capturing, as i-
               n (CAP-CAM (DEVICE :INT)). See WITH-CAPTURE example.
    
         BODY - The body of the code to be executed once the video file or capturing device is open.



(defun with-capture-example (&optional (camera-index *camera-index*))

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
    (let ((window-name (foreign-alloc 
			:string :initial-element 
			"WITH-CAPTURE Example")))
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
      (destroy-window window-name)
      (foreign-free window-name))))



CAP-IS-OPEN

Returns true if video capturing has been initialized already.

C++: bool VideoCapture::isOpened()

Common Lisp: (CAP-IS-OPEN (SELF (:POINTER VIDEO-CAPTURE)))

Parameters:	

         SELF - The VIDEO-CAPTURE structure.

If the previous call to VIDEO-CAPTURE constructor or CAP-IS-OPEN succeeded, the method returns true.


(defun cap-is-open-example (&optional (camera-index *camera-index*))

  "If the previous call to VIDEO-CAPTURE constructor (i/e, 
   (CAP-CAM CAMERA-INDEX) in the below example) or the fu-
   nction CAP-IS-OPEN succeeded, the method returns true. 
   The boolean output of the PRINC function in the IF sta-
   tement in this example reflects a good or bad capture. 
   Output will likely be '1' or 'True' unless your camera 
   is unplugged....Try unplugging your camera to test it 
   out."

  (with-capture (cap (cap-cam camera-index)) 
    (let ((window-name (foreign-alloc 
			:string :initial-element 
			"CAP-IS-OPEN Example")))
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
      (destroy-window window-name)
      (foreign-free window-name))))



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

(ABS) todo maybe add this function


(defun absdiff-example (&optional (camera-index *camera-index*) 
			  (width *default-width*)
			  (height *default-height*))

  "The function ABSDIFF calculates the per-element absolute 
   difference between FRAME(the camera stream) and SCALAR a-
   nd outputs the result to a window...Makes for quite an i-
   nteresting effect."

  (with-capture (cap (cap-cam camera-index))
    (let ((scalar (mat-value 1 1 +64f+ (scalar 128 128 128)))
	  (window-name (foreign-alloc 
			:string :initial-element 
			"ABSDIFF Example")))
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
      (destroy-window window-name)
      (foreign-free window-name))))


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
      (format t "~%SCALAR-2 element ~a = ~a~%" n (mem-aref scalar-2 :double n)))
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


SIZE-MAT

Returns pointer to a matrix size.

C++: Size Mat::size() const

Common Lisp: (SIZE-MAT (SELF (:POINTER MAT)))

The function SIZE-MAT returns Size*, a matrix size pointer in which the columns are listed first an-
d the rows second. When the matrix is more than 2-dimensional, the returned size is (-1 -1).


(defun SIZE-MAT-example ()

  "The function SIZE-MAT returns Size*, a matrix size 
   pointer in which the columns are listed first and 
   the rows are listed second(COLS ROWS). In the cod-
   e below the (COLS ROWS) values of MAT which are s-
   tored in SIZE are accessed with the CFFI function 
   MEM-AREF."

  (let* ((mat (mat-value 3 7 +64f+ (scalar 100 100 100)))
         (size (size-mat mat)))
    (format t " The (COLS ROWS) of MAT = (~a ~a)~%"  
	    (mem-aref size :int 0)
	    (mem-aref size :int 1))))


ROWS

Returns number or rows in MAT.

Common Lisp:  (ROWS (SELF (:POINTER MAT)))

    Parameters:	

        SELF - A matrix(MAT).


The function ROWS finds the number of rows in a matrix or -1 when the array has more than 2 dimensi-
ons. 


(defun rows-example ()

  "Uses ROWS to find the number of rows in the matrix MAT"

  (let* ((mat (mat-value 3 4 +64f+ (scalar 100)) ))
          (format t "The number of rows in MAT = ~a" (rows mat))))


COLS

Returns number or cols in MAT.

Common Lisp:  (COLS (SELF (:POINTER MAT)))

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

C++: Point_();

Common Lisp: (POINT-INIT) => (:POINTER POINT)

C++: Point_(_Tp _x, _Tp _y)

Common Lisp:  (POINT (X :INT) (Y :INT)) => (:POINTER POINT)

C++: _Tp x, y;

Common Lisp: (X (SELF (:POINTER POINT))) => :INT

C++: _Tp x, y;

Common Lisp: (Y (SELF (:POINTER POINT))) => :INT


    Parameters:	

        SELF - A POINT construct.

        X - x-coordinate of the point.

        Y -	y-coordinate of the point.


2D point with integer coordinates (usually zero-based).


(defun point-example (x y)

  "Initializes a POINT construct with the 
   function POINT-INIT. Then creates a po-
   int with the function POINT. Finally, 
   lists the x,y coordinates with the POI-
   NT functions X and Y."

  (let* ((initialized-point (point-init))
	 (point (point x y)))
    (format t "Pointer to initialized point: ~a~%~%" 
	    initialized-point)
    (format t "POINT (x, y) = (~a, ~a)~%" 
	    (x point)
	    (y point))))


EMPTY

Returns true if the array has no elements.

C++: bool Mat::empty() const

Common Lisp: (EMPTY (SELF (:POINTER MAT)))

The method returns true if (TOTAL) is 0 or if Mat::data todo is NIL. Because of pop_back() todo an-
d resize()todo methods (equal (TOTAL M) 0) does not imply that M.data == NULL todo.


CV-TYPE

Returns the type of a matrix element.

C++: int Mat::type() const

Common Lisp: (CV-TYPE (SELF (:POINTER MAT)))

    Parameters:	

        SELF - A matrix(MAT)

The method returns a matrix element type. This is an identifier compatible with OpenCV's CvMat type
system, like CV_16SC3(+16SC3+ in Common Lisp) or 16-bit signed 3-channel array, and so on.


(defun cv-type-example ()

  "This function uses CV-TYPE to find 
   the type of MAT-ONE and MAT-TWO. S-
   hows MAT-ONE and MAT-TWO in a wind-
   ow so you can see what they look l-
   ike."

  (let* ((mat-one (mat-zeros 1 2 +8u+))
	 (mat-two (mat-zeros 2 4 +8u+))
	 (window-name-1 (foreign-alloc 
			 :string :initial-element 
			 "MAT-ONE - CV-TYPE Example"))
         (window-name-2 (foreign-alloc 
			 :string :initial-element 
			 "MAT-TWO - CV-TYPE Example")))
    (named-window window-name-1 +window-normal+)
    (named-window window-name-2 +window-normal+)
    (move-window window-name-1 464 175)
    (move-window window-name-2 915 175)
    (format t "MAT-ONE type is ~a(+32f+). It is a Single Precision Floating Point Matrix.~%" 
	    (cv-type mat-one))
    (format t "~%MAT-TWO type is ~a(+64f+). It is a Double Precision Floating Point Matrix." 
	    (cv-type mat-two))
    (imshow window-name-1 mat-one)
    (imshow window-name-2 mat-two)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name-1)
    (destroy-window window-name-2)
    (foreign-free window-name-1)
    (foreign-free window-name-2)))


CAP-CAM

C++: VideoCapture::VideoCapture(int device)

Common Lisp: (CAP-CAM (DEVICE :INT))

    Parameters:	

        DEVICE - id of the opened video capturing device (i.e. a camera index). If there is a singl-
                 e camera connected, just pass 0.


(defun cap-cam-example (&optional (camera-index *camera-index*))

  "This function use CAP-CAM to open a video 
   capturing device (i.e. a camera index). If th-
   ere is a single camera connected, just pass 0
   (the default value of *camera-index*)."

  (with-capture (cap (cap-cam camera-index))
    (let ((window-name (foreign-alloc 
			:string :initial-element 
			"CAP-CAM Example")))
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
      (destroy-window window-name)
      (foreign-free window-name))))


CAP-FILE

C++: VideoCapture::VideoCapture(const string& filename)

Common Lisp: (CAP-FILE (FILENAME (:POINTER STRING*)))

    Parameters:	

        FILENAME - Name of the opened video file (eg. video.avi) or image sequence (eg. img_%02d.j-
                    pg, which will read samples like img_00.jpg, img_01.jpg, img_02.jpg, ...)


(defun cap-file-example (filename)

  "This function use CAP-FILE to open a video 
   file supplied by the parameter FILENAME."

  (setf filename (foreign-alloc :string :initial-element 
				filename))
  (with-capture (cap (cap-file filename))
    (let ((window-name (foreign-alloc 
			:string :initial-element 
			"CAP-FILE Example")))
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
      (destroy-window window-name)
      (foreign-free filename)
      (foreign-free window-name))))



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
    (dotimes (i (rows h))
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
    (let* ((window-name (foreign-alloc 
			 :string :initial-element 
			 "CICRLE Example"))
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
				(format t "ceiling has been touched~%") 
				(setf ceiling-switch 1))) 
	(if (and (< x the-right-wall) (= right-wall-switch 0)) (incf x rate) (decf x rate))
	(if (and (< y the-floor) (= floor-switch 0)) (incf y rate) (decf y rate))
	(if (< x (+ 40 rate)) (setf right-wall-switch 0))
	(if (< y (+ 40 rate)) (setf floor-switch 0))
	(report))
      (destroy-window window-name)
      (foreign-free window-name))))


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
	 (window-name (foreign-alloc 
		       :string :initial-element 
		       "MAT - MAT-ZEROS Example")))
    (named-window window-name +window-normal+)
    (move-window window-name 720 175)
    (imshow window-name mat)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)
    (foreign-free window-name)))



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
	 (window-name (foreign-alloc 
		       :string :initial-element 
		       "MAT - MAT-ONES Example")))
    (named-window window-name +window-normal+)
    (move-window window-name 720 175)
    (imshow window-name mat)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)
    (foreign-free window-name)))


MAT-EYE
todo figure out if I do all of these
Returns an identity matrix of the specified size and type.

C++: static MatExpr Mat::eye(int rows, int cols, int type)

Common Lisp: (MAT-EYE-0 (ROWS :INT) (COLS :INT) (TYPE :INT)) => (:POINTER MAT)

C++: static MatExpr Mat::eye(Size size, int type)

Common Lisp: (MAT-EYE-1 (S (:POINTER SIZE)) (TYPE :INT)) => (:POINTER MAT)


    Parameters:	

        ROWS - Number of rows.

        COLS - Number of columns.

        SIZE - Alternative matrix size specification as (SIZE ROWS COLS).

        TYPE - Created matrix type.


The method returns a Matlab-style identity matrix initializer, similarly to (MAT-ZEROS). Similarly 
to (MAT-ONES), you can use a scale operation to create a scaled identity matrix efficiently:

// make a 4x4 diagonal matrix with 0.1's on the diagonal.
Mat A = Mat::eye(4, 4, CV_32F)*0.1;


(defun mat-eye-example ()

  "In this example the function MAT-EYE 
   is used to create a 3x3 identity mat-
   ri(MAT). MAT is then shown in a wind-
   ow."

  (let* ((mat (mat-eye 3 3 +8u+))
	 (window-name (foreign-alloc 
		       :string :initial-element 
		       "MAT - MAT-EYE Example")))
    (named-window window-name +window-normal+)
    (move-window window-name 720 175)
    (imshow window-name mat)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)
    (foreign-free window-name)))


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


MAT-EXPR-T

Transposes a matrix.

C++: MatExpr Mat::t() const

Common Lisp: (MAT-EXPR-T (SELF (:POINTER MAT))) => (:POINTER MAT-EXPR)

The method performs matrix transposition by means of matrix expressions. It does not perform the ac-
tual transposition but returns a temporary matrix transposition object that can be further used as 
a part of more complex matrix expressions or can be assigned to a matrix.

  Parameters:	

        SELF - Input matrix


(defun mat-expr-t-example ()

  "Computes (A + lambda*I)^t * (A + lamda*I)
   and shows the result in a window."

  (let* ((a (mat-typed-0 3 3 +32f+))
	 (l 3.0d0)
	 (a1 (mat-expr-s (mat-expr+ a (mat-eye-1 
				       (size-mat a) (mat-type a))) l))
	 (c (mat-expr* (>> (mat-expr-t (>> a1))) (>> a1)))
	 (window-name (foreign-alloc 
		       :string :initial-element 
		       "MAT-EXPR-T Example")))
    (named-window window-name +window-normal+)
    (move-window window-name 720 175)
    (imshow window-name (>> c))
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)
    (foreign-free window-name)))


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
         (out (mat-expr+ mat mat))
	 (window-name (foreign-alloc 
		       :string :initial-element 
		       "FORCE Example")))
    (named-window window-name +window-normal+)
    (move-window window-name 720 175)
    (imshow window-name  (>> out))
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)
    (foreign-free window-name)))


PROMOTE

Coverts a MAT to MAT-EXPR

Common Lisp: (PROMOTE (SELF (:POINTER MAT))) => (:POINTER MAT-EXPR)

Common Lisp: (<< (SELF (:POINTER MAT))) => (:POINTER MAT-EXPR)

The function PROMOTE converts a functions return from (:POINTER MAT) to (:POINTER MAT-EXPR).  This 
is useful if you would like to do math computation on a matrix with a (:POINTER MAT) type using a 
super fast Matrix Expressions(MAT-EXPR) function. Matrix Expressions functions will only accept a
(:POINTER MAT-EXPR) type as input.  You can then convert back to (:POINTER MAT) with the function
FORCE to use the result in a function that only accepts a MAT as input i.e. IMSHOW. The function 
<< is an identical shorthand version of the PROMOTE function supplied for ease of use. 

   Parameters:	

        SELF - A MAT pointer.


(defun promote-example ()

  "In this example a matrix filled with ones(MAT) is 
   created. PROMOTE or actually the shorthand versio-
   n of the function PROMOTE << is then used to coer-
   ce MAT to a (:POINTER MAT-EXPR) type so it can th-
   en be multiplied by the scalar S with the functio-
   n MAT-EXPR-S. This is necessary since MAT-EXPR-S 
   only accepts (:POINTER MAT-EXPR) types as it's in-
   put. The output of MAT-EXPR-S(OUT) is then coerce-
   d back to (:POINTER MAT) with the function FORCE 
   for the opposite reason so it can then be shown i-
   n a window with IMSHOW."

  (let* ((mat (mat-ones 3 3 +8u+))
         (s 5.0d0)
         (out (mat-expr-s (<< mat) s))
	 (window-name (foreign-alloc 
		       :string :initial-element 
		       "PROMOTE Example")))
    (named-window window-name +window-normal+)
    (move-window window-name 720 175)
    (imshow window-name  (>> out))
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)
    (foreign-free window-name)))



MAT-EXPR-S

Multiplies a matrix by a scalar.

Common Lisp: (MAT-EXPR-S (SELF (:POINTER MAT-EXPR)) (ALPHA :DOUBLE)) => (:POINTER MAT-EXPR)

The function MAT-EXPR-S multiplies a matrix by a scalar. Keep in mind MAT-EXPR-S will only accept a
(:POINTER MAT-EXPR) type as input so you if your attempting to multiply a matrix with (:POINTER MAT)
as its type you will first need to convert it to a (:POINTER MAT) with the PROMOTE function(<<) bef-
ore doing the computation. You can then convert back to (:POINTER MAT) with the function FORCE. 

    Parameters:	

        SELF - The input matrix.

        ALPHA - A scalar of type double-float.


(defun mat-expr-s-example ()

  "In this example a matrix filled with ones(MAT) is 
   multiplied by a scalar S with the function MAT-EX-
   PR. When using MAT-EXPR-S to multiply a matrix wi-
   th a (:POINTER MAT) type by a scalar, you need to 
   first use the PROMOTE function or << to convert t-
   o a (:POINTER-MAT-EXPR) type. Then use the functi-
   on FORCE or >> to convert back to (:POINTER MAT) 
   if needed. The resultant matrix RESULT is then sh-
   own in a window"

  (let* ((mat (mat-ones 4 4 +8u+))
         (s 7.0d0)
         (result (mat-expr-s (<< mat) s))
	 (window-name (foreign-alloc 
		       :string :initial-element 
		       "MAT-EXPR-S Example")))
    (named-window window-name +window-normal+)
    (move-window window-name 720 175)
    (imshow window-name  (>> result))
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)
    (foreign-free window-name)))


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

  (let* ((mat (mat-typed-0 4 4 +32f+))
	 (window-name (foreign-alloc 
		       :string :initial-element 
		       "MAT-TYPED-0 Example")))
    (named-window window-name +window-normal+)
    (move-window window-name 720 175)
    (imshow window-name mat)
    (loop while (not (= (wait-key 0) 27)))
    (destroy-window window-name)
    (foreign-free window-name)))

         
DIAG

Extracts a diagonal from a matrix, or creates a diagonal matrix.

C++: Mat Mat::diag(int d=0 ) const

Common Lisp: (MAT-DIAG (SELF (:POINTER MAT)) (D :INT)) => (:POINTER MAT)

C++: static Mat Mat::diag(const Mat& d)


    Parameters:	

        D –

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


MAT-EXPR*

Finds the product of two matrices.

C++: MatExpr *

Common Lisp: (MAT-EXPR* (M1 (:POINTER MAT)) (M2 (:POINTER MAT))) => (:POINTER MAT-EXPR)


    Parameters:	

        M1 – A single float or double float matrix.

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

C++: MatExpr +

Common Lisp: (ADD (M1 (:POINTER MAT)) (M2 (:POINTER MAT))) => (:POINTER MAT-EXPR)


    Parameters:	

        M1 – A matrix.

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

C++: MatExpr -

Common Lisp: (SUB (M1 (:POINTER MAT)) (M2 (:POINTER MAT))) => (:POINTER MAT-EXPR)


    Parameters:	

        M1 – A matrix.

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
				 '(64 22 64 15 11 17 42 16 88)))
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
      (dotimes (j 3)
	(format t "~a" (at-int (>> result) i j))
	(princ #\Space))
      (princ #\Newline))))



DIV

Divides matrix M1 by matrix M2.

C++: MatExpr /

Common Lisp: (DIV (M1 (:POINTER MAT)) (M2 (:POINTER MAT))) => (:POINTER MAT-EXPR)


    Parameters:	

        M1 – A matrix.

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

Common Lisp: (VIDEO-WRITER-INIT)

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


(defun video-writer-example (&optional (camera-index *camera-index*))

  (with-capture (cap (cap-cam camera-index)) ; Open the video camera no. 0
    (let* ((filename (foreign-alloc 
		      :string :initial-element 
		      "/home/w/my-video.avi"))
	   (window-name (foreign-alloc 
			 :string :initial-element 
			 "VIDEO-WRITER Example"))

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

           ; Wait for 'esc' key press for 30ms. If 'esc' key is pressed, break loop
	   ((plusp (wait-key *millis-per-frame*)) 
	    (format t "Key is pressed by user"))
	(setf frame (mat))

	(cap-read cap frame) ; read a new frame from video

	(if (not frame) ; If not success, break loop
	    (return-from video-writer-example 
	      (format t "ERROR: Cannot read a frame from video file")))

	(video-writer-write o-video-writer frame) ; Write the frame into the file

	(imshow window-name frame)) ; Show the frame in window
      (destroy-window window-name)
      (foreign-free window-name)
      (foreign-free filename))))


SIZE

SIZE constructor

C++: Size_(_Tp _width, _Tp _height);
     typedef Size_<int> Size2i;
     typedef Size2i Size;

Common Lisp: (SIZE (WIDTH :INT) (HEIGHT :INT)) => (:POINTER SIZE)

C++: _Tp width, height

Common Lisp: (SIZE-WIDTH (SELF (:POINTER SIZE))) => :INT

C++: _Tp width, height

Common Lisp: (SIZE-HEIGHT (SELF (:POINTER SIZE))) => :INT


    Parameters:	

        SELF - A SIZE construct.

        WIDTH - The width of SIZE.
        
        HEIGHT - The height of SIZE.


The function SIZE stores size values.

The function SIZE-WIDTH Finds the width of a SIZE construct.

The function SIZE-HEIGHT Finds the height of a SIZE construct.


(defun size-example ()

  "Initializes SIZE constructor with values 
   and prints them"

  (let* ((size (size 640 480)))
    (format t "Width = ~a~%" (size-width size))
    (format t "Height = ~a" (size-height size))))


VIDEO-WRITER-IS-OPEN

Returns true if video writer has been successfully initialized.

C++: bool VideoWriter::isOpened()

Common Lisp: (VIDEO-WRITER-IS-OPEN (SELF (:POINTER VIDEO-WRITER)))


(defun video-writer-is-open-example (filename &optional (camera-index *camera-index*))

  (with-capture (cap (cap-cam camera-index)) ; Open the video camera no. 0
    (let* ((filename (foreign-alloc 
		      :string :initial-element 
		      filename))
			 ; Initialize the VideoWriter object 
	   (o-video-writer (video-writer filename 1196444237 ; todo
					 20.0d0 (size 640 480) 1)))
      (format t "If VIDEO-WRITER is open a T will be displayed, else NIL: ~a"
	      (video-writer-is-open o-video-writer))
      (foreign-free filename))))



VIDEO-WRITER-WRITE

Writes the next video frame

C++: VideoWriter& VideoWriter::operator<<(const Mat& image)

Common Lisp: (VIDEO-WRITER-WRITE (SELF (:POINTER VIDEO-WRITER)) (IMAGE (:POINTER MAT)))

    Parameters:	

        SELF - Pointer to VIDEO-WRITER

        IMAGE - The written frame

The function VIDEO-WRITER-WRITE writes the specified image to video file. It must have the same siz-
e as has been specified when opening the video writer.


(defun video-writer-write-example (filename &optional 
					      (camera-index *camera-index*))

  (with-capture (cap (cap-cam camera-index)) 
    (let* ((filename (foreign-alloc 
		      :string :initial-element filename))
	   (window-name (foreign-alloc 
			 :string :initial-element 
			 "VIDEO-WRITER-WRITE Example"))
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
      (destroy-window window-name)
      (foreign-free window-name)
      (foreign-free filename))))


MAT-CLONE

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
    (let* ((window-name (foreign-alloc 
			 :string :initial-element 
			 "ROI Example")))
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
				   (format t "right wall has been touched~%") 
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
      (destroy-window window-name)
      (foreign-free window-name))))


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

The function BR retrieves the bottom-right corner of the rectangle.

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

;; _Tp dot(const Point_& pt) const;

Common Lisp: (DOT (SELF (:POINTER POINT)) (OTHER (:POINTER POINT))) => :INT


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


IN-RANGE

Checks if array elements lie between the elements of two other arrays.

C++: void inRange(InputArray src, InputArray lowerb, InputArray upperb, OutputArray dst)

Common Lisp: (IN-RANGE-S (SRC (:POINTER MAT)) (LOWERB (:POINTER SCALAR)) (UPPERB (:POINTER SCALAR)) (DEST (:POINTER MAT)) => :VOID


    Parameters:	

        SRC – first input array.

        LOWERB – A scalar.

        UPPERB – A scalar.

        DEST – output array of the same size as SRC and +8U+ type.


All the arrays must have the same type, except the destination, and the same size (or ROI size).


(defun in-range-s-example (&optional (camera-index *camera-index*) 
			     (width 640)
			     (height 480))

  (with-capture (cap (cap-cam camera-index))
    (let ((window-name-1 (foreign-alloc 
			  :string :initial-element 
			  "Original camera feed - IN-RANGE-S Example"))
	  (window-name-2 (foreign-alloc 
			  :string :initial-element 
			  "Only red objects - IN-RANGE-S Example"))
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
	(del img-hsv) (del img-thresh) (del frame) (del src))
      (destroy-window window-name-1)
      (destroy-window window-name-2)
      (foreign-free window-name-1)
      (foreign-free window-name-2))))


GAUSSIAN-BLUR

Blurs an image using a Gaussian filter.

C++: void GaussianBlur(InputArray src, OutputArray dst, Size ksize, double sigmaX, double sigmaY=0,
     int borderType=BORDER_DEFAULT )                   

Commom Lisp: (GAUSSIAN-BLUR (SRC (:POINTER MAT)) (DEST (:POINTER MAT)) (KSIZE (:POINTER SIZE)) (SIG
              MA-X :DOUBLE) &OPTIONAL ((SIGMA-Y :DOUBLE) 0) ((BORDER-TYPE :INT) +BORDER-DEFAULT+))

    Parameters:	

        SRC – input image; the image can have any number of channels, which are processed independe-
              ntly, but the depth should be +8U+, +16U+, +16S+, +32F+ or +64F+.

        DST – output image of the same size and type as SRC.

        KSIZE – Gaussian kernel size KSIZE width and KSIZE height can differ but they both must be 
                positive and odd. Or, they can be zero’s and then they are computed from sigma.

        SIGMAX – Gaussian kernel standard deviation in X direction.

        SIGMAY – Gaussian kernel standard deviation in Y direction; if SIGMAY is zero, it is set to 
                 be equal to SIGMAX, if both sigmas are zeros, they are computed from KSIZE width a-
                 nd KSIZE height , respectively (see (GET-GAUSSIAN-KERNEL) for details); to fully c-
                 ontrol the result regardless of possible future modifications of all this semantics, 
                 it is recommended To specify all of KSIZE, SIGMA-X, AND SIGMA-Y.

        BORDER-TYPE – pixel extrapolation method (see (BORDER-INTERPOLATE) for details).


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
    (let ((window-name-1 (foreign-alloc 
			  :string :initial-element 
			  "Original - GAUSSIAN-BLUR Example"))
	  (window-name-2 (foreign-alloc 
			  :string :initial-element 
			  "Blurred output - GAUSSIAN-BLUR Example"))
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
	(del frame) (del src))
      (destroy-window window-name-1)
      (destroy-window window-name-2)
      (foreign-free window-name-1)
      (foreign-free window-name-2))))



