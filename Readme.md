Welcome to my LisP-CV repository, My name is Joe BiMedina.


Tested on Ubuntu - all versions 12.04 and up - using SBCL and CLISP and ALLEGRO CL

Warning: This library still in process. Some functions may not work as expected 
but most functions are fully tested and well documented.

--------------------------------------------------------------------------------

INTRODUCTION:
 
 LisP-CV is a Lisp wrapper for OpenCV's C++ interface, not the C interface. The
Lisp wrapper wraps around C wrappers which wrap around the OpenCV C++ functions. 
Due to the speediness of C and C++, the speed of the C++ functions compared to 
the C wappers for them is almost identical.

  This install tutorial is made for people wanting to help work on the project. 
Thus no build file is included so you'll have to install everything manually. 
Don't worry it's easy. This is intended so you can familiarize yourself with how 
everything is put together. As stated previously, LisP-CV is known to work on 
Ubuntu versions 12.04 through 14.04. It should work on any system that OpenCV, 
SBCL CLISP, ALLEGRO, CFFI, ASDF and QuickLisp can be compiled on.  There is 
nothing but pure Lisp, and OpenCV C++ and C in the whole project. 

  If you have gotten LisP-CV to work on another system and would like to add the
install instructions to this tutorial just send me an e-mail at wnetai@gmail.com 
with the install instructions included. They will be added to this file and you 
will be given credit for your addition unless you decline.

  If you would like to contribute a function to this project, you will need to 
first find out if the C wrapper for it is available. Don't worry over 550 C wrappers 
are available and they can be found in the opencv_generated.cpp, excluded_functions.cpp, 
mat.cpp, point.cpp ,rect.cpp, scalar.cpp, and size.cpp files. The location of those files 
and their corresponding .hpp files can be found in the installation section of this file. 
If there is no C wrapper available and you are familiar with C and C++ feel free to write 
one, but, if you do, try to do it similarly to the C wrappers already in the aforementioned 
.cpp files. They C wrappers are going to be included in the main OpenCV repository soon and 
that is the way they work best with the OpenCV's current development build. If you have a 
valid point as to how a C wrapper you write, that doesn't fit into the style of what's 
already there, would be better (more functionality, more speed) feel free to 
email me wnetai@gmail.com with a writeup of your conclusions. I'm assisting in developing 
the C wrappers so I can present your case to the others involved. Keep in mind that the 
C wrappers are still being developed so always be checking the 
LisP-CV repository for the latest versions. I will be keeping those wrappers up 
to date. Keep in mind though my version of the opencv_generated.cpp, excluded_functions.cpp, 
mat.cpp, point.cpp, rect.cpp, scalar.cpp, and size.cpp files have all the same functions as 
the main C wrapper repository at: 

https://github.com/arjuncomar/opencv_contrib 

there are a few more as well. This is primarily for testing purposes, as the C wrapper 
module matures those differences will be ironed out.  If you run into a Lisp function 
that doesn't work suddenly after compiling the latest .cpp files in lisp-cv-master/src, 
and it worked before, a change in the C wrappers might be the reason. To circumvent 
that situation always install my full build (all of the .lisp, .cpp and .hpp files) 
because I will always make sure it's in good working 
order. 

  I write a code example and documentation for each function I add to the library, so 
it is very well documented. In the lisp-cv-master/examples/examples.lisp folder there 
are over 5400 lines of code examples and documentation. So, unlike some other Lisp libraries, 
you WILL be able to EASILY figure out how to use it. If you would like to contribute a Lisp 
function I ask that you also write an example of how to use it. It doesn't have to be a large 
example it only has to show what the function does and give an idea how it is used in the most 
basic way. The code should be commented good enough so that anyone with basic Lisp knowledge 
could get a basic idea of the usage of it. You do not need to write any documentation for it, however, 
I will do that and as far as adding constants, I will do that as well. You will be given credit for 
all your contributions in the contributions.lisp file in the root directory unless you decline. 
Just, let me know how you would like to be referenced in that file i.e name/username/email/phone# or 
if you decline reference.

See lisp-cv-master/examples/examples.lisp for an example of the examples I already included.

To add contributions just Fork my repository at:

https://github.com/W-Net-AI/lisp-cv and send me a Pull Request.

or send them to me in an e-mail at wnetai@gmail.com. 

--------------------------------------------------------------------------------

INSTALLATION:

  To use LisP-CV you will need to first install GitHub member arjuncomar's  Open-CV 3.0.0 dev. 
pull of Itseez/OpenCV(the original OpenCV). Then you will need to install Emacs, SBCL, SLIME and 
QuickLisp, and then finally LisP-CV and all of the required .so files.

To install arjuncomrs OpenCV go to this link:

https://github.com/arjuncomar/opencv

and download it by clicking the "Download ZIP" button on right side of page.

Then go to this link and follow the instructions, verbatim, to install OpenCV.

http://miloq.blogspot.com/2012/12/install-opencv-ubuntu-linux.html

When you are following the instructions at the above link to install, if you get
this error when trying to test OpenCV:

g++: error: rt: No such file or directory
g++: error: pthread: No such file or directory
g++: error: m: No such file or directory
g++: error: dl: No such file or directory
g++: error: tbb: No such file or directory

To fix it run:

gksu gedit /usr/local/lib/pkgconfig/opencv.pc

In the file that opens, put a "-l" without quotes in front of the single occurence of each of these 5 entries:

rt, pthread, m , dl, tbb

Now to install Emacs, SBCL, SLIME, QuickLisp:

To install Emacs run:

sudo apt-get install emacs24

To install SBCL run:  

sudo apt-get install sbcl

To install SLIME run:

sudo apt-get install slime

To install QuickLisp:

go to http://beta.quicklisp.org/quicklisp.lisp to download the quicklisp.lisp file

Place it in ~/.emacs.d/

Next, run sbcl, in Terminal, and type in the following:

(load "~/.emacs.d/quicklisp.lisp")

After it loads, run:

(quicklisp-quickstart:install)

That’ll download the rest of the system and get it set up for you. Quicklisp will
install by default in ~/quicklisp.

Finally, run:

(ql:add-to-init-file) 

Press Enter.

That will add Quicklisp to your SBCL init file so that anytime you run SBCL Quicklisp 
will be loaded and ready to go. 

Now, go to terminal and type emacs, to start Emacs. When Emacs starts, if everything 
went as planned, you should see the word "Polling" at the bottom of the window for a 
few seconds and the a cool little text animation and then a prompt that looks like this:

CL-USER>

If Emacs stays stuck on "Polling", first verify you have a .sbclrc file in your 
home directory(~/) and that it contains the following text:

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

If it doesn't or there is no file there, repeat the steps starting from "To inst-
all QuickLisp:" and review any error messages to debug.

Now in Emacs, at the CL-USER> prompt (it's correct name is the REPL(Read Eval Pr-
int Loop), enter the below command to test out your QuickLisp installation and a-
lso create the ~/quicklisp/dists/quicklisp/software/ directory we will use in a 
later step:

(ql:quickload "ieee-floats")

If QuickLisp is installed correctly you should have this folder in your 

~/quicklisp/dists/quicklisp/software/

directory.

ieee-floats-20140211-git

You can remove the ieee-floats-20140211-git directory now if you like and it wil-
l be effectively uninstalled.

Now to install LisP-CV!!!

Note #1: I will make a Makefile or script to install LisP-CV as library progress-
es but for now this install tutorial is made people who want to work on the proj-
ect.  So all steps to install must be done by hand so you get familiar how the l-
ibrary works...don't worry though it's easy.  If you do end up helping on the pr-
oject you might want to make a script to speed up some of the processes defined 
below.

Note #2: "LisP-CV" is a decorative name made to signify that is a Common Lisp wr-
apper for OpenCV and to pay hommage to the fact the name Lisp was derived from i-
ts method of operation, "List Processing". The name with the capitalized 'L', 'P',
'C' and 'V' appears nowhere in the uncommented source code.

Let's get started.

From this link download Lisp-CV:

https://github.com/W-Net-AI/LisP-CV

Extract the:

~/Downloads/lisp-cv-master.zip file 

to:

 ~/quicklisp/dists/quicklisp/software

Place the opencv_geenerated.cpp file in the /src folder of the lisp-cv-master directory 
in the following directory on your machine:

<directory-where-you-installed-opencv>/opencv-master/build/modules/c/src/

backing up first, then overwriting the original opencv_generate.cpp.

Place all of the other the .cpp files in the /src folder of the lisp-cv-master directory 
in the following directory:

<directory-where-you-installed-opencv>/opencv-master/modules/c/src/

backing up first, then overwriting the originals.

Place all of .hpp files in the /include folder of the lisp-cv-master directory in the following directory:

/usr/local/include/opencv2/c/

backing up first, then overwriting the originals.

Go to the two directores you placed the .cpp files from the src/ folder and run:

g++ -Wall -shared -fPIC -o <filename>.so <filename>.cpp

(where <filename> stands in for one of each of the .cpp filenames) on each of the .cpp files from the src/ folder, 
that you place in the directory.

Then take each one of those .so files you created and place in /usr/local/lib.

Now start Emacs and run:

(ql:quickload "cffi")

To install CFFI, which LisP-CV depends on, plus it's dependencies.

Then run:

(ql:quickload "asdf")

To install ASDF, a LisP-CV dependency.

Run:

gksu gedit ~/.sbclrc

and add this to the top of your ~/.sbclrc file.

(require :asdf)
;put all subdirectories of quicklisp\software into asdf:*central-registry*
 (dolist (dir (directory "/home/w/quicklisp/dists/quicklisp/software/*/"))
 (pushnew dir asdf:*central-registry* :test #'equal))

Open Emacs and run this at the REPL:

(asdf:operate 'asdf:load-op :lisp-cv)

LisP-CV will load.

Then run this at the REPL

(in-package #:lisp-cv)

Your REPL will change to look like this:

LISP-CV>

Now you are all finished!

Run this at the REPL to test:


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


and then this:


(move-window-example)


If all went as planned an empty window should open up.

--------------------------------------------------------------------------------

TUTORIAL:

You can learn to use this library by looking at the examples in:

lisp-cv-master/examples/examples.lisp.

If you want to know if a function has been wrapped and is available, do a search 
for the C++ function name in the examples.lisp folder. All functions are documented 
and all have the C++ function declaration above the C function declaration above the 
Lisp one. Some function names aren't well defined but you can do a search for the class 
names i.e To find the Lisp equivelant of the Matrix Expressions "*" , "/", "+" or "-" 
operators just do a search for MatExpr in the examples.lisp file. I'll be making a 
function to simplify this very soon.


You can also learn how to use the library by comparing the OpenCV C++ tutorials 
at this link:

http://opencv-srf.blogspot.com/2011/09/capturing-images-videos.html

to the CAP-FILE and CAP-CAM examples in examples.lisp.



All the constants are as so:

If a constant in OpenCV's C++ interface is WINDOW_AUTOSIZE

The Lisp version is +window-autosize+

If a constant in OpenCV's C++ interface is CV_8UC3, it's Lisp version is +8UC3+

I always add the plus signs to both ends,  I always change the underscores to da-
shes and I always remove the "CV_", no exceptions.


--------------------------------------------------------------------------------

FINALLY:

Feel free to e-mail at me wnetai@gmail.com for any of the following reasons:

*Bug reports*

*If you would like to help develop this project(I intend to make it complete)*

*Request for features*

*If you have gotten LisP-CV to work on a system other than specified at the top 
of this page* (Remember to include how you got it done!)

*If you have a function and example that you would like to add*

*If you would like add a code sample in the examples(something you made with LisP-CV)*

*If you would like to add some other unspecified addition to the library*

*Comments*























