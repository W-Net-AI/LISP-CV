Welcome to my LisP-CV repository, My name is Joe BiMedina.

LisP-CV is a Lisp wrapper for OpenCV's C++ interface, not the C interface. 
The Lisp wrapper wraps around C wrappers which wrap around the OpenCV C++ functions.
Due to the speediness of C and C++,  the speed of the C++ functions compared to the 
C wappers for them is almost identical.


To use LisP-CV you will need to First install GitHub member arjuncomar's  OpenCV 3.0.0 dev. pull of 
Itseez/OpenCV(the original OpenCV). Then you will need to install Emacs, SBCL, SLIME and QuickLisp,
and then finally LisP-CV and all of the required .so files.



To install arjuncomrs OpenCV go to this link:

https://github.com/arjuncomar/opencv

and download it by clicking the "Download ZIP" button on right side of page.



Then go to this link and follow the instructions, verbatim, to install it.

http://miloq.blogspot.com/2012/12/install-opencv-ubuntu-linux.html



When you are following the instructions at the above link to install, if you get this 
error when trying to test OpenCV:


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

go to http://beta.quicklisp.org/quicklisp.lisp to download quicklisp.lisp

Place in ~/.emacds.d/



Next, run sbcl, in Terminal, and type in the following:


(load "~/.emacs.d/quicklisp.lisp")


After it loads, run:

	
(quicklisp-quickstart:install)


That’ll download the rest of the system and get it set up for you. Quicklisp will install by default in 
~/quicklisp.


Finally, run:


(ql:add-to-init-file)


That’ll add Quicklisp to your SBCL init file so that anytime you run SBCL Quicklisp will be loaded and 
ready to go. 



Now, go to terminal and type emacs, to start Emacs. Wen Emacs starts, if everything went as planned, you 
should see the word "Polling" at the bottom of the window for a few seconds and the a cool little 
text animation and then a prompt that looks like this:


CL-USER>


If Emacs stays stuck on "Polling", first verify you hace a .sbclrc file in your home directory(~/)
and that it contains the following text:



;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))



If it doesn't or there is no file there, repeat the steps starting from "To install QuickLisp:" and review 
any error messages to debug.


Now in Emacs at the REPL(Read Eval Print Loop) or the CL-USER> prompt enter the below command to test out 
your QuickLisp installation:

(ql:quickload "ieee-floats")

If QuickLisp is installed correctly you should have this folder in your ~/quicklisp/dists/quicklisp/software/
directory.

ieee-floats-20140211-git




Now to install LisP-CV

Note: will make a Makefile or script for LisP-CV as library progresses.


From this link download Lisp-CV:

https://github.com/joe-w-bimedina/LisP-CV


Extract the ~/Downloads/lisp-cv-master.zip file to ~/quicklisp/dists/quicklisp/software



Place the opencv_geenerated.cpp file in the /src folder of the lisp-cv-master directory in the following directory 
on your machine:

<directory-where-you-installed-opencv>/opencv-master/build/modules/c/src/

backing up first, then overwriting the original opencv_generate.cpp.



Place all of the other the .cpp files in the /src folder of the lisp-cv-master directory in the following directory:

<directory-where-you-installed-opencv>/opencv-master/modules/c/src/

backing up first, then overwriting the originals.




Place all of .hpp files in the /include folder of the lisp-cv-master directory in the following directory:

/usr/local/include/opencv2/c/

backing up first, then overwriting the originals.




Go to the directores you placed the two .cpp files from the src/ folder and run:

g++ -Wall -shared -fPIC -o <filename>.so <filename>.cpp

(where <filename> stands in for one of the .cpp filenames) on each of the .cpp files from the src/ folder, that 
you place in the directory.


Then take each one of those .so files you created and place in /usr/local/lib.


Now start Emacs and run:


(ql:quickload "cffi")

To install CFFI, which LisP-CV depends on, plus it's dependencies.

Then run 

(ql:quickload "asdf")


Add this to the top of your ~/.sbclrc file.

(require :asdf)
;put all subdirectories of quicklisp\software into asdf:*central-registry*
 (dolist (dir (directory "/home/w/quicklisp/dists/quicklisp/software/*/"))
 (pushnew dir asdf:*central-registry* :test #'equal))


(asdf:operate 'asdf:load-op :lisp-cv)

 
(in-package #:lisp-cv)





















