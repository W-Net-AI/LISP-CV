#include <opencv2/c/opencv_generated.hpp>
using namespace cv;
using namespace std;
using namespace flann;
using namespace cvflann;

typedef vector<Rect> vector_Rect;

#include <opencv2/c/extra_functions.hpp>


CvPoint cvPoint_glue(int x, int y)

{

    return cvPoint(x, y);

}

extern "C" {

void cv_displayOverlay(String* winname, String* text, int delayms) {
	cv::displayOverlay(*winname, *text, delayms);
}

MatExpr* cv_abs(Mat* m) {
	return new MatExpr(cv::abs(*m));
}

Rect* cv_RotatedRect_boundingRect(RotatedRect* self) {
    return new Rect(self->boundingRect());
}

MatExpr* cv_Mat_div(Mat* m1, Mat* m2) {
    return new MatExpr(*m1 / *m2);
}

Scalar* cv_create_morphologyDefaultBorderValue() { 
    return new Scalar(Scalar::all(DBL_MAX)); 
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

TermCriteria* cv_create_TermCriteria() {
    return new TermCriteria();
}

TermCriteria* cv_create_TermCriteria3(int type, int maxCount, double epsilon) {
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

void cv_delete_BRISK(BRISK* self) {
    delete self;
}

void cv_delete_BFMatcher(BFMatcher* self) {
    delete self;
}

void cv_delete_CscadeClassifier(CascadeClassifier* self) {
     delete self;
}

void cv_delete_DMatch(DMatch* self) {
    delete self;
}

void cv_delete_KeyPoint(KeyPoint* self) {
     delete self;
}

void cv_delete_Point(Point* self) {
     delete self;
}

void cv_delete_Point2d(Point2d* self) {
     delete self;
}

void cv_delete_Point2f(Point2f* self) {
     delete self;
}

void cv_delete_Point3d(Point3d* self) {
     delete self;
}

void cv_delete_Point3f(Point3f* self) {
     delete self;
}

void cv_delete_Point3i(Point3i* self) {
     delete self;
}

void cv_delete_Rect(Rect* self) {
     delete self;
}

void cv_delete_RNG(RNG* self) {
     delete self;
}

void cv_delete_RotatedRect(RotatedRect* self) {
     delete self;
}

void cv_delete_Scalar(Scalar* self) {
     delete self;
}

void cv_delete_Size(Size* self) {
     delete self;
}

void cv_delete_Size2f(Size2f* self) {
     delete self;
}

void cv_delete_SURF(SURF* self) {
     delete self;
}

void cv_delete_TermCriteria(TermCriteria* self) {
     delete self;
}

void cv_delete_vector_char(vector_char* self) {
     delete self;
}

void cv_delete_VideoCapture(VideoCapture* self) {
     delete self;
}

void cv_delete_VideoWriter(VideoWriter* self) {
     delete self;
}


}
