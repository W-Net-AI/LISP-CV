#include <opencv2/c/opencv_generated.hpp>
using namespace cv;
using namespace std;
using namespace flann;
using namespace cvflann;

typedef vector<Rect> vector_Rect;

#include <opencv2/c/extra_functions.hpp>

extern "C" {

CascadeClassifier* cv_create_CascadeClassifier1_2(const char* filename) {
	return new CascadeClassifier(filename);
}

bool cv_CascadeClassifier_load1_2(CascadeClassifier* self, const char* filename) {
	return self->load(filename);
}

void cv_delete_Point(Point* self) {
     delete self;
}

void cv_destruct_Point(Point* self) {
     self->~Point();
}

void cv_delete_Rect(Rect* self) {
     delete self;
}

void cv_destruct_Rect(Rect* self) {
     self->~Rect();
}

int &cv_Rect_x(Rect* self) {
    return self->x;
}

int &cv_Rect_y(Rect* self) {
    return self->y;
}

int &cv_Rect_width(Rect* self) {
    return self->width;
}

int &cv_Rect_height(Rect* self) {
    return self->height;
}

bool cv_findChessboardCorners2(Mat* image, Size* patternSize, vector_Point2f* corners, int flags) {
	return cv::findChessboardCorners(*image, *patternSize, *corners, flags);
}

void cv_cornerSubPix2(Mat* image, vector_Point2f* corners, Size* winSize, Size* zeroZone, TermCriteria* criteria) {
	cv::cornerSubPix(*image, *corners, *winSize, *zeroZone, *criteria);
}

void cv_drawChessboardCorners2(Mat* image, Size* patternSize, vector_Point2f* corners, bool patternWasFound) {
	cv::drawChessboardCorners(*image, *patternSize, *corners, patternWasFound);
}

TermCriteria* cv_create_TermCriteria(int type, int maxCount, double epsilon) {
    return new TermCriteria(type, maxCount, epsilon);
}

MatExpr* cv_Mat_sub(Mat* m1, Mat* m2) {
    return new MatExpr(*m1 - *m2);
}

RNG* cv_create_RNG() {
    return new RNG();
}

RNG* cv_create_RNG_state(uint64 state) {
    return new RNG(state);
}

double cv_RNG_uniform_double(RNG* self, double a, double b) {
    return self->uniform(a, b);
}

float cv_RNG_uniform_float(RNG* self, float a, float b) {
    return self->uniform(a, b);
}

int cv_RNG_uniform_int(RNG* self, int a, int b) {
    return self->uniform(a, b);
}

uchar* cv_Mat_get_Data(Mat* self) {
    return self->data;
}

Mat* cv_Mat_get_ROI(Mat* self, Rect* roi) {
    return new Mat(*self, *roi);
}

bool cv_imwrite2(const char* filename, Mat* img, vector_int* params) {
	return cv::imwrite(filename, *img, *params);
}

Mat* cv_imread2(const char* filename, int flags) {
	return new Mat(imread(filename, flags));
}

FeatureDetector* cv_FeatureDetector_create2(FeatureDetector* self, const char* detectorType) {
	return &*self->create(detectorType);
}

Size2f* cv_create_Size2f() {
    return new Size2f;
}

Size2f* cv_create_Size2f2(float width, float height) {
    return new Size2f(width, height);
}

Size2f* cv_Size2f_assignTo(Size2f* self, Size2f* other) {
    *self = *other;
    return self;
}

Size2f* cv_Size2f_fromPoint2f(Point2f* p) {
    return new Size2f(*p);
}

float cv_Size2f_area(Size2f* self) {
    return self->area();
}

float cv_Size2f_width(Size2f* self) {
    return self->width;
}

float cv_Size2f_height(Size2f* self) {
    return self->height;
}

void cv_delete_BRISK(void* ptr) {
    delete (BRISK*)ptr;
}

void cv_destruct_BRISK(BRISK* self) {
     self->~BRISK();
}

void cv_delete_BFMatcher(void* ptr) {
    delete (BFMatcher*)ptr;
}

void cv_destruct_BFMatcher(BFMatcher* self) {
     self->~BFMatcher();
}


}
