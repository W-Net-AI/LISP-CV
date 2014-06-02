#include <opencv2/c/opencv_generated.hpp>
using namespace cv;
using namespace std;
using namespace flann;
using namespace cvflann;


#include "/home/w/quicklisp/dists/quicklisp/software/lisp-cv-master/include/extra_functions.hpp"


extern "C" {

void cv_LineSegmentDetector_detect2(LineSegmentDetector* self, Mat* _image, vector_Vec4i* _lines, Mat* width, Mat* prec, Mat* nfa) {
	self->detect(*_image, *_lines, *width, *prec, *nfa);
}

void cv_LineSegmentDetector_drawSegments2(LineSegmentDetector* self, Mat* _image, vector_Vec4i* lines) {
	self->drawSegments(*_image, *lines);
}


Vec4i* cv_create_Vec4i() {
    return new Vec4i();
}

CvANN_MLP_TrainParams* cv_create_CvANN_MLP_TrainParams() {
    return new CvANN_MLP_TrainParams();
}

CvANN_MLP_TrainParams* cv_create_CvANN_MLP_TrainParams4(TermCriteria* term_crit, int train_method, double param1, double param2) {
    return new CvANN_MLP_TrainParams(*term_crit, train_method, param1, param2);
}

CvDTreeParams* cv_create_CvDTreeParams() {
    return new CvDTreeParams();
}

CvDTreeParams* cv_create_CvDTreeParams9(int max_depth, int min_sample_count, float regression_accuracy, bool use_surrogates, int max_categories, int cv_folds, bool use_1se_rule, bool truncate_pruned_tree, const float* priors) {
    return new CvDTreeParams(max_depth, min_sample_count, regression_accuracy, use_surrogates, max_categories, cv_folds, use_1se_rule, truncate_pruned_tree, priors);
}

Mat* cv_Mat_with_Range(Mat* self, Range* rowRange, Range* colRange) {
    return new Mat(*self, *rowRange, *colRange);
}

Range* cv_create_Range(int _start, int _end) {
    return new Range(_start, _end);
}

Range* cv_create_RangeAll() {
    return new Range(Range::all());
}

bool cv_Range_empty(Range* self) {
    return self->empty();
}

int cv_Range_size(Range* self) {
    return self->size();
}

int cv_Range_getstart(Range* self) {
    return self->start;
}

int cv_Range_getend(Range* self) {
    return self->end;
}

void cv_HOGDescriptor_detectMultiScale9_2(HOGDescriptor* self, Mat* img, vector_Rect* foundLocations, double hitThreshold, Size* winStride, Size* padding, double scale, double finalThreshold, bool useMeanshiftGrouping) {
	self->detectMultiScale(*img, *foundLocations,  hitThreshold, *winStride, *padding, scale, finalThreshold, useMeanshiftGrouping);
}


void cv_HOGDescriptor_setSVMDetector2(HOGDescriptor* self, vector_float* _svmdetector) {
	self->setSVMDetector(*_svmdetector);
}

CvSVMParams* cv_create_CvSVMParams() {
    return new CvSVMParams();
}

//CvSVMParams* cv_create_CvSVMParams10(int svm_type, int kernel_type, double degree, double gamma, double coef0, double Cvalue, double nu, //double p, CvMat* class_weights, TermCriteria term_crit) {
//   return new CvSVMParams(svm_type, kernel_type, degree, gamma, coef0, Cvalue, nu, p, Mat(class_weights), term_crit);
//}

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

void cv_delete(char* self) {
    delete self;
}

void cv_delete_CvANN_MLP(CvANN_MLP* self) {
    delete self;
}

void cv_delete_CvANN_MLP_TrainParams(CvANN_MLP_TrainParams* self) {
    delete self;
}

void cv_delete_CascadeClassifier(CascadeClassifier* self) {
     delete self;
}

void cv_delete_DMatch(DMatch* self) {
    delete self;
}

void cv_delete_CvDTree(CvDTree* self) {
    delete self;
}

void cv_delete_CvDTreeParams(CvDTreeParams* self) {
    delete self;
}

void cv_delete_Feature2D(Feature2D* self) {
    delete self;
}

void cv_delete_HOGDescriptor(HOGDescriptor* self) {
    delete self;
}

void cv_delete_KeyPoint(KeyPoint* self) {
     delete self;
}

void cv_delete_CvKNearest(CvKNearest* self) {
     delete self;
}

void cv_delete_CvNormalBayesClassifier(CvNormalBayesClassifier* self) {
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

void cv_delete_Range(Range* self) {
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

void cv_delete_TermCriteria(TermCriteria* self) {
     delete self;
}

void cv_delete_Vec4i(Vec4i* self) {
     delete self;
}

void cv_delete_VideoCapture(VideoCapture* self) {
     delete self;
}

void cv_delete_VideoWriter(VideoWriter* self) {
     delete self;
}


}
