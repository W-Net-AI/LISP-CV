Welcome to my Lisp-CV repository, My name is Joe BiMedina.


Tested on Ubuntu - all versions 12.04 and up - using SBCL and CLISP and ALLEGRO CL

Warning: This library still in process. Some functions may not work as expected. 
I test the functions all the time so you have a pretty good shot that almost all 
of them would work.

--------------------------------------------------------------------------------

#INTRODUCTION
 
 Lisp-CV is a Lisp wrapper for OpenCV's C++ interface, not the C interface. The
Lisp wrapper wraps around C wrappers which wrap around the OpenCV C++ functions. 
Due to the speediness of C and C++, the speed of the C++ functions compared to 
the C wappers for them is almost identical.

  This install tutorial is made for people wanting to help work on the project. 
Thus no build file is included so you'll have to install everything manually. 
Don't worry it's easy. This is intended so you can familiarize yourself with how 
everything is put together. As stated previously, Lisp-CV is known to work on 
Ubuntu versions 12.04 through 14.04. It should work on any system that OpenCV, 
SBCL CLISP, ALLEGRO, CFFI, ASDF and QuickLisp can be compiled on.  There is 
nothing but Lisp, and OpenCV C++, C and Python in the whole project. 

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
the above .cpp files to make sure the C bindings were'nt changed in anyway. Also, I make 
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
the Readme.md at the former link. You will need to install Emacs, SBCL (or ACL, CLISP), 
SLIME and QuickLisp, and then finally Lisp-CV and all of the required .so files.

First, Install Emacs, (SBCL,CLISP,ACL), SLIME, QuickLisp. You should be able to easily find 
a tutorial online.


Now in Emacs, at the REPL, enter the below command to test out your QuickLisp installation 
and also create the ~/quicklisp/dists/quicklisp/software/ directory we will use in a later 
step:

(ql:quickload "ieee-floats")

If QuickLisp is installed correctly you should have this folder now.

~/quicklisp/dists/quicklisp/software/ieee-floats-20140211-git

You can remove the ieee-floats-20140211-git directory now if you like.


Now to install Lisp-CV!!!!


Note #1: I will make a Makefile or script to install Lisp-CV as library progresses but for 
now this install tutorial is made people who want to work on the project.  So all steps to 
install must be done by hand so you get familiar how the library works...don't worry though 
it's easy.  If you do end up helping on the project you might want to make a script to speed 
up some of the process.


Let's get started.

From this link download Lisp-CV:

https://github.com/W-Net-AI/LISP-CV

Extract the:

~/Downloads/lisp-cv-master.zip file 

to:

 ~/quicklisp/dists/quicklisp/software


You might want to change the name of the LISP-CV-master folder to lisp-cv-master after extracting
to avoid confusion. However the LISP-CV-master name shouldn't affect the compilation of Lisp-CV.


If you built OpenCV with the extra modules correctly per this link: 

https://github.com/arjuncomar/opencv_contrib. 

You should have these files present on your machine:
```
<where-you-placed->>/opencv/build/modules/c/src/opencv_generated.cpp
<where-you-placed->>/opencv_contrib/modules/c/include/opencv2/c/mat.hpp
<where-you-placed->>/opencv_contrib/modules/c/include/opencv2/c/point.hpp
<where-you-placed->>/opencv_contrib/modules/c/include/opencv2/c/rect.hpp
<where-you-placed->>/opencv_contrib/modules/c/include/opencv2/c/scalar.hpp
<where-you-placed->>/opencv_contrib/modules/c/include/opencv2/c/size.hpp
<where-you-placed->>/opencv_contrib/modules/c/include/opencv2/c/excluded_functions.hpp


<where-you-placed->>/opencv/build/modules/c/include/opencv2/c/opencv_generated.hpp
<where-you-placed->>/opencv_contrib/modules/c/src/mat.cpp
<where-you-placed->>/opencv_contrib/modules/c/src/point.cpp
<where-you-placed->>/opencv_contrib/modules/c/src/rect.cpp
<where-you-placed->>/opencv_contrib/modules/c/src/scalar.cpp
<where-you-placed->>/opencv_contrib/modules/c/src/size.cpp
<where-you-placed->>/opencv_contrib/modules/c/src/excluded_functions.cpp
```
You will need to add these files. They can be found in the Lisp-CV src and include directories:
```
<lisp-cv-src-dir>/include/extra_functions.hpp
<lisp-cv-src-dir>/src/extra_functions.cpp

<lisp-cv-src-dir>/include/interop.hpp
<lisp-cv-src-dir>/src/interop.cpp
```
Build the above files with the below command:
```
g++ -Wall -shared -fPIC -o <filename>.so <filename>.cpp
```
Then take each one of those .so files you created and place in /usr/local/lib.


Now start Emacs and run:

```
(ql:quickload "cffi")
```

To install CFFI, which Lisp-CV depends on, plus it's dependencies.


Then run:

```
(ql:quickload "asdf")

```
To install ASDF, a Lisp-CV dependency.


Run:

```
gksu gedit ~/.sbclrc

```
or
```
gksu gedit ~/.clisprc
```

If you don't have it already, add this to the top of your ~/.sbclrc or ~/.clisprc file.
```
(require :asdf)
;put all subdirectories of quicklisp\software into asdf:*central-registry*
 (dolist (dir (directory "/home/w/quicklisp/dists/quicklisp/software/*/"))
 (pushnew dir asdf:*central-registry* :test #'equal))
```

Open Emacs and run this at the REPL:
```
(asdf:operate 'asdf:load-op :lisp-cv)

(asdf:operate 'asdf:load-op :gc)
```

Lisp-CV will load. Then run this at the REPL:

```
(in-package #:lisp-cv)
```

Your REPL will change to look like this:

```
CV>
```
Now you are all finished!


Run this at the REPL to test:

```
(defun imread-example-2 (filename)

  "Open the image FILENAME with IMREAD 
   and show it in a window. This examp-
   le uses with-* macros for memory ma-
   nagement"

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

```
and then this:

```
(imread-example-2  <path-to-an-image>)
```

If all went as planned you should see your picture in a window.


Note: You can place:

```
(asdf:operate 'asdf:load-op :lisp-cv)

(asdf:operate 'asdf:load-op :gc)

(in-package #:lisp-cv)
```
In your Lisp implementations initialization file(e.g. .sbclrc or .clisprc) to start LISP-CV
automatically.

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
Lisp equivelant of the Matrix Expressions "*" , "/", "+" or "-" operators just do a 
search for MatExpr in the examples.lisp file. Learning how to search this file will 
help answer a lot of questions about this Library and put incrediple amounts of code 
snippets right at your fingertips. Every function provided in this library has an 
example program associated with it and all of the names of the examples are formatted 
as <function-name>-example.


You can also learn how to use the library by comparing the OpenCV C++ tutorials 
at this link:

http://opencv-srf.blogspot.com/2011/09/capturing-images-videos.html

to the CAP-FILE and CAP-CAM examples in examples.lisp.

By comparing the tutorials at this link:

http://opencv-srf.blogspot.com/2011/11/track-bars.html

to the CREATE-TRACKBAR example.

and by comparing the tutorial at this link:

http://opencv-srf.blogspot.com/2011/11/mouse-events.html

to the SET-MOUSE-CALLBACK example. Comparing the C++ code to the Lisp code will give 
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























