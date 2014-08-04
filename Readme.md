Welcome to my Lisp-CV repository, My name is Joe BiMedina.


Tested on Ubuntu - all versions 12.04 and up - using SBCL and CLISP and ALLEGRO CL

Warning: Work on this library is still in process. Some functions may not work as
expected. I test the functions all the time so you have a pretty good shot that
almost all of them would work.

--------------------------------------------------------------------------------

#INTRODUCTION

  Lisp-CV is a Lisp wrapper for OpenCV's C++ interface, not the C interface. The
Lisp wrapper wraps around C wrappers which wrap around the OpenCV C++ functions.
Due to the speediness of C and C++, the speed of the C++ functions compared to
the C wrappers for them is almost identical.

  This install tutorial is made for people wanting to help work on the project.
Thus no build file is included so you'll have to install everything manually.
Don't worry it's easy. This is intended so you can familiarize yourself with how
everything is put together. As stated previously, Lisp-CV is known to work on
Ubuntu versions 12.04 through 14.04. It should work on any system that OpenCV,
SBCL, CLisp, Allegro Lisp, CFFI, ASDF and QuickLisp can be compiled on. There is
nothing but Lisp, and C++, C and Python in the whole project.

  If you have gotten Lisp-CV to work on another system and would like to add the
install instructions to this tutorial just send me an e-mail at wnetai@gmail.com
with the install instructions included. They will be added to this file and you
will be given credit for your addition unless you decline.

  If you would like to contribute a function to this project, you will need to
first find out if the C wrapper for it is available. Don't worry 550+ C bindings
are available and they can be found in the below files.(See the INSTALLATION section
for the location of these files.)

```
opencv_generated.cpp
excluded_functions.cpp
extra_functions.cpp
interop.cpp
mat.cpp
point.cpp
rect.cpp
scalar.cpp
size.cpp
```

 If you run into a Lisp function that doesn't work suddenly after compiling the latest
.cpp files, and it worked before, a change in the C bindings might be the reason. Check
the above .cpp files to make sure the C bindings weren't changed in anyway. Also, I make
a lot of changes every day, so a non-working function could be the result of something I
missed. I am very good at discovering these errors so it shouldn't be long before I fix
said issue. You can hasten the fix by sending a bug report.

 I write a code example and documentation for each function I add to the library, so it
is very well documented. In the lisp-cv-master/examples/examples.lisp file there are over
12,000 lines of code examples and documentation. If you would like to contribute a Lisp
function, I ask that you also write an example of how to use it. It doesn't have to be a
large example it only has to show what the function does and give an idea how it is used
in the most basic way. See lisp-cv-master/examples/examples.lisp for an example of the
examples I already included.

To add contributions just Fork my repository at:

https://github.com/W-Net-AI/LISP-CV and send me a Pull Request.

or send them to me in an e-mail at wnetai@gmail.com.



--------------------------------------------------------------------------------

##INSTALLATION

 To use Lisp-CV you will need to first install the development build of OpenCV here
https://github.com/Itseez/opencv and GitHub member arjuncomar's fork of opencv_contrib
here https://github.com/arjuncomar/opencv_contrib. Instructions to build both are in
the Readme.md at the former link. You will also need Lisp environment, of course.

Note #1: I will make a Makefile or script to install Lisp-CV as library progresses but for
now this install tutorial is made people who want to work on the project.  So all steps to
install must be done by hand so you get familiar how the library works...don't worry though
it's easy. If you do end up helping on the project you might want to make a script to speed
up some of the process.

Clone sources from official repository (https://github.com/W-Net-AI/LISP-CV) first and them
extract them to location reachable by ASDF. Discussing ways to make ASDF look for systems
definitions in custom directories is out of scope of this instruction, please refer to ASDF
manual.

Note: You may want to change the name of the extracted LISP-CV source folder from "LISP-CV-master"
to "lisp-cv-master", to prevent any possible unforeseen issues. I don't think there would be, but, 
better to be safe. The caps were just used to make the LISP-CV repo look nicer and do not appear 
anywhere in the source code.

If you built OpenCV with the extra modules correctly per this link:

https://github.com/arjuncomar/opencv_contrib.

You should have these files present on your machine:
```
some-random-path/opencv/build/modules/c/src/opencv_generated.cpp
some-random-path/opencv_contrib/modules/c/include/opencv2/c/mat.hpp
some-random-path/opencv_contrib/modules/c/include/opencv2/c/point.hpp
some-random-path/opencv_contrib/modules/c/include/opencv2/c/rect.hpp
some-random-path/opencv_contrib/modules/c/include/opencv2/c/scalar.hpp
some-random-path/opencv_contrib/modules/c/include/opencv2/c/size.hpp
some-random-path/opencv_contrib/modules/c/include/opencv2/c/excluded_functions.hpp
/usr/local/include/opencv2/c/opencv_generated.hpp
some-random-path/opencv_contrib/modules/c/src/mat.cpp
some-random-path/opencv_contrib/modules/c/src/point.cpp
some-random-path/opencv_contrib/modules/c/src/rect.cpp
some-random-path/opencv_contrib/modules/c/src/scalar.cpp
some-random-path/opencv_contrib/modules/c/src/size.cpp
some-random-path/opencv_contrib/modules/c/src/excluded_functions.cpp
```
```
"Note: for the time being, until an error in the opencv_contrib module is ironed
out, the opencv_generated.cpp and opencv_generated.hpp will be provided in the: 

lisp-cv-src-dir/src

lisp-cv-src-dir/include 

directories. You will still need to build OpenCv with the instructions at the above 
link, but when your done, will need to place these files at the appropriate paths below. 
This should work fine but, due to the error, I am unable to test the install at this time 
to verify it."
```

You will need to copy these files. They can be found in the Lisp-CV src and include directories:
```
path-to-LISP-CV/include/extra_functions.hpp
path-to-LISP-CV/src/extra_functions.cpp
path-to-LISP-CV/include/interop.hpp
path-to-LISP-CV/src/interop.cpp
```

Use any C++ compiler to compile provided files. In example provided I will use GCC:
```
g++ -Wall -shared -fPIC -o <filename>.so <filename>.cpp
```

Then install resulting so-files using your distro's recommended way to install custom libraries.

Now you can start your Lisp and load LISP-CV:
```
(asdf:load-system :lisp-cv)

(asdf:load-system :gc)
```

You're all done! Now you can switch your package to LISP-CV's one for extra comfort:
```
(in-package #:lisp-cv)
```

Let's do some tests just to be sure:

```
(defun imread-example-2 (filename)
  "Open the image FILENAME with IMREAD and show it in a window. This example uses with-*
macros for memory management"
  (let ((window-name "IMREAD Example 2"))
    (with-named-window (window-name +window-normal+)
      (move-window window-name 759 175)
      (with-mat ((image (imread filename 1)))
        (if (empty image)
          (return-from imread-example-2 (format t "Image not loaded")))
        (imshow window-name image)
        (loop
          (let ((c (wait-key 33)))
            (when (= c 27)
              (return))))))))

(imread-example-2  <path-to-any-image>)

```


If all went as planned, you should see your picture in a window.

--------------------------------------------------------------------------------

###TUTORIAL

You can learn to use this library by looking at the examples in:
```
lisp-cv-master/examples/examples.lisp.
```
If you want to know if a function has been wrapped and is available, do a search
for the C++ function name in the examples.lisp folder. All functions are documented
and all have the C++ function declarations above the Lisp one. Some function names
aren't well defined but you can do a search for the C++ class name i.e To find the
Lisp equivalent of the Matrix Expressions "*" , "/", "+" or "-" operators just do a
search for MatExpr in the examples.lisp file. Learning how to search this file will
help answer a lot of questions about this Library and put incredible amounts of code
snippets right at your fingertips. Every function provided in this library has an
example program associated with it and all of the names of the examples are formatted
as <function-name>-example.


You can also learn how to use the library by comparing the OpenCV C++ tutorials
at this link:

http://opencv-srf.blogspot.com/2011/09/capturing-images-videos.html

to the VIDEO-CAPTURE-EXAMPLE example in examples.lisp.

By comparing the tutorials at this link:

http://opencv-srf.blogspot.com/2011/11/track-bars.html

to the CREATE-TRACKBAR-EXAMPLE.

and by comparing the tutorial at this link:

http://opencv-srf.blogspot.com/2011/11/mouse-events.html

to the SET-MOUSE-CALLBACK-EXAMPLE. Comparing the C++ code to the Lisp code will give
you an idea of how to convert C++ OpenCv code you find online to Lisp.


All the constants are defined as so:

If a constant in OpenCV's C++ interface is WINDOW_AUTOSIZE

The Lisp version is +window-autosize+

If a constant in OpenCV's C++ interface is CV_8UC3, it's Lisp version is +8UC3+

I always add the plus signs to both ends and I always change the underscores to dashes
and I always remove the "CV_".


--------------------------------------------------------------------------------

####FINALLY

Feel free to e-mail at me wnetai@gmail.com for any of the following reasons:

*Bug reports*

*If you would like to help develop this project(I intend to make it complete)*

*Request for features*

*If you have gotten Lisp-CV to work on a system other than specified at the top 
of this page* (Remember to include how you got it done!)

*If you have a function and example that you would like to add*

*If you would like add a code sample to the examples folder(something you made
with Lisp-CV)*

*If you would like to add some other unspecified addition to the library*

*Comments*


