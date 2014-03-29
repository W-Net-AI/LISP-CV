#ifndef __OPENCV_GENERATED_HPP
#define __OPENCV_GENERATED_HPP
#include <opencv2/opencv.hpp>
#include <opencv2/nonfree.hpp>
#include <vector>
using namespace cv;
using namespace std;
using namespace flann;
using namespace cvflann;
extern "C" {
FeatureDetector* cv_FeatureDetector_create2(FeatureDetector* self, const char* detectorType);
Mat* cv_imread2(const char* filename, int flags);
void cv_inRangeS(Mat* src, Scalar* lowerb, Scalar* upperb, Mat* dst);
int cv_createTrackbar(String* trackbarname, String* winname, int* value, int count, TrackbarCallback onChange, void* userdata);
void cv_setMouseCallback(String* winname, MouseCallback onMouse, void* userdata);
void cv_delete_Mat(void* ptr);
void cv_delete_BFMatcher(void* ptr);
void cv_delete_BRISK(void* ptr);
)
