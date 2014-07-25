/*
* =====================================================================================
*
* Filename: excluded_functions.cpp
*
* Description: Functions that the generator outputs incorrectly, either by them entirely
* or by outputting them with incorrectly specified types.
*
* Version: 1.0
* Created: 04/13/2014 12:06:39 AM
* Revision: none
* Compiler: g++
*
* Author: Arjun Comar
*
* =====================================================================================
*/
#include <opencv2/c/excluded_functions.hpp>


#define ADD_VEC_FUNC_IMPL_0_0(t) \
    Vec2##t * cv_create_0_Vec2##t() { \
        return new Vec2##t ();\
    } \
    Vec3##t * cv_create_0_Vec3##t() { \
        return new Vec3##t ();\
    } \
    Vec4##t * cv_create_0_Vec4##t() { \
        return new Vec4##t ();\
    } \

#define ADD_VEC_FUNC_IMPL_0_1(t) \
    Vec6##t * cv_create_0_Vec6##t() { \
        return new Vec6##t ();\
    } \

#define ADD_VEC_FUNC_IMPL_0_2(t) \
    Vec8##t * cv_create_0_Vec8##t() { \
        return new Vec8##t ();\
    } \

#define ADD_VEC_FUNC_IMPL_1(t, tn) \
    Vec2##t * cv_create_Vec2##t(tn v0, tn v1) { \
        return new Vec2##t (v0, v1);\
    } \
    Vec3##t * cv_create_Vec3##t(tn v0, tn v1, tn v2) { \
        return new Vec3##t (v0, v1, v2);\
    } \
    Vec4##t * cv_create_Vec4##t(tn v0, tn v1, tn v2, tn v3) { \
        return new Vec4##t (v0, v1, v2, v3);\
    } \

#define ADD_VEC_FUNC_IMPL_2(t, tn) \
    Vec6##t * cv_create_Vec6##t(tn v0, tn v1, tn v2, tn v3, tn v4, tn v5) { \
        return new Vec6##t (v0, v1, v2, v3, v4, v5);\
    } \

#define ADD_VEC_FUNC_IMPL_3(t, tn) \
    Vec8##t * cv_create_Vec8##t(tn v0, tn v1, tn v2, tn v3, tn v4, tn v5, tn v6, tn v7) { \
        return new Vec8##t (v0, v1, v2, v3, v4, v5, v6, v7);\
    } \

#define ADD_WRITE_FUNC_IMPL_0(t, tn) \
    void cv_FileNode_write_number_##t(FileStorage* fs, String* name, tn value) { \
        write(*fs, *name, value);\
} \

#define ADD_WRITE_FUNC_IMPL_1(t, tn) \
    void cv_FileNode_write_pointer_##t(FileStorage* fs, String* name, tn* value) { \
        write(*fs, *name, *value);\
} \

#define ADD_READ_FUNC_IMPL_0(t, tn) \
    void cv_FileNode_read_number_##t(FileNode* node, tn value, tn default_value) { \
        read(*node, value, default_value);\
} \

#define ADD_READ_FUNC_IMPL_1(t, tn) \
    void cv_FileNode_read_pointer_##t(FileNode* node, tn* value, tn* default_value) { \
        read(*node, *value, *default_value);\
} \

#define ADD_READ_FUNC_IMPL_2(t, tn) \
void cv_FileNode_read_pointer_##t(FileNode* node, vector_KeyPoint* keypoints) { \
    read(*node, *keypoints);\
} \

#define CV_FOURCC_MACRO(c1, c2, c3, c4) (((c1) & 255) + (((c2) & 255) << 8) + (((c3) & 255) << 16) + (((c4) & 255) << 24)) 

extern "C" {

ADD_VEC_FUNC_IMPL_0_0(b);
ADD_VEC_FUNC_IMPL_0_0(d);
ADD_VEC_FUNC_IMPL_0_0(f);
ADD_VEC_FUNC_IMPL_0_0(i);
ADD_VEC_FUNC_IMPL_0_0(s);
ADD_VEC_FUNC_IMPL_0_0(w);

ADD_VEC_FUNC_IMPL_0_1(d);
ADD_VEC_FUNC_IMPL_0_1(f);
ADD_VEC_FUNC_IMPL_0_1(i);

ADD_VEC_FUNC_IMPL_0_2(i);

ADD_VEC_FUNC_IMPL_1(b, uchar);
ADD_VEC_FUNC_IMPL_1(d, double);
ADD_VEC_FUNC_IMPL_1(f, float);
ADD_VEC_FUNC_IMPL_1(i, int);
ADD_VEC_FUNC_IMPL_1(s, short);
ADD_VEC_FUNC_IMPL_1(w, ushort);

ADD_VEC_FUNC_IMPL_2(d, double);
ADD_VEC_FUNC_IMPL_2(f, float);
ADD_VEC_FUNC_IMPL_2(i, int);

ADD_VEC_FUNC_IMPL_3(i, int);

ADD_WRITE_FUNC_IMPL_0(d, double);
ADD_WRITE_FUNC_IMPL_0(f, float);
ADD_WRITE_FUNC_IMPL_0(i, int);

ADD_WRITE_FUNC_IMPL_1(s, String);
ADD_WRITE_FUNC_IMPL_1(m, Mat);
ADD_WRITE_FUNC_IMPL_1(vkp, vector_KeyPoint);

ADD_READ_FUNC_IMPL_0(d, double);
ADD_READ_FUNC_IMPL_0(f, float);
ADD_READ_FUNC_IMPL_0(i, int);

ADD_READ_FUNC_IMPL_1(s, String);
ADD_READ_FUNC_IMPL_1(m, Mat);

ADD_READ_FUNC_IMPL_2(vkp, vector_KeyPoint);

Vec3b* cv_Mat_at_Vec3b0(Mat* self, int i, int j) {
  return &self->at<Vec3b>(i, j);
}

uchar &cv_Mat_at_uchar1(Mat* self, int i, int j) {
  return self->at<uchar>(i, j);
}

const float* cv_CvSVM_get_support_vector(SVM* self, int i) {
    return self->get_support_vector(i);
}

int cv_vector_int_get_value (vector_int* self, int idx) {
    return (*self)[idx];
}

CvMat* cv_Mat_to_CvMat(Mat* self) {

    CvMat m = *self;
    return new CvMat(m);
}

CvTermCriteria cv_TermCriteria_to_CvTermCriteria(TermCriteria* self) {

    CvTermCriteria t = *self;
    return CvTermCriteria(t);
}

Mat* cv_imdecode_2(vector_uchar* buf, int flags) {
	return new Mat(cv::imdecode(*buf, flags));
}

bool cv_imencode_2(const char* ext, Mat* img, vector_uchar* buf, vector_int* params) {
	return cv::imencode(ext, *img, *buf, *params);
}

int CV_FOURCC(char c1, char c2, char c3, char c4)
{
    return CV_FOURCC_MACRO(c1, c2, c3, c4); 
}

void cv_groupRectangles_3(vector_Rect* rectList, int groupThreshold, double eps) {
	cv::groupRectangles(*rectList, groupThreshold, eps);
}

FileNode * cv_FileNode_assignVal(FileStorage * fs, string * nodename) {

    return new cv::FileNode(fs->operator[](*nodename));

}

FileStorage* cv_FileStorage_write_String(FileStorage* fs, String* value)
{  
  return new FileStorage(*fs << String(*value));
}

Scalar* cv_create_Scalar0()
{
    return new Scalar();
}

Scalar* cv_create_Scalar4(double val0, double val1, double val2, double val3)
{
    return new Scalar(val0, val1, val2, val3);
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

CvSVMParams* cv_create_CvSVMParams10(int svm_type, int kernel_type, double degree, double gamma, double coef0, double Cvalue, double nu, double p, CvMat* class_weights, CvTermCriteria term_crit) {
   return new CvSVMParams(svm_type, kernel_type, degree, gamma, coef0, Cvalue, nu, p, class_weights, term_crit);
}

void cv_displayOverlay(String* winname, String* text, int delayms) {
	cv::displayOverlay(*winname, *text, delayms);
}

MatExpr* cv_abs(Mat* m) {
	return new MatExpr(cv::abs(*m));
}

float cv_RotatedRect_angle(RotatedRect* self) {
    return self->angle;
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

void cv_delete_BFMatcher(BFMatcher* self) {
    delete self;
}

void cv_delete_BRISK(BRISK* self) {
    delete self;
}

void cv_delete_CascadeClassifier(CascadeClassifier* self) {
     delete self;
}

void cv_delete_CvANN_MLP(CvANN_MLP* self) {
    delete self;
}

void cv_delete_CvANN_MLP_TrainParams(CvANN_MLP_TrainParams* self) {
    delete self;
}

void cv_delete_CvMat(CvMat* self) {
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

void cv_delete_FileNode(FileNode* self) {
    delete self;
}

void cv_delete_FileStorage(FileStorage* self) {
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

void cv_delete_SURF(SURF* self) {
    delete self;
}

void cv_delete_CvSVM(CvSVM* self) {
    delete self;
}

void cv_delete_CvSVMParams(CvSVMParams* self) {
    delete self;
}

void cv_delete_TermCriteria(TermCriteria* self) {
     delete self;
}

void cv_delete_Vec2b(Vec2b* self) {
     delete self;
}

void cv_delete_Vec3b(Vec3b* self) {
     delete self;
}

void cv_delete_Vec4b(Vec4b* self) {
     delete self;
}

void cv_delete_Vec2d(Vec2d* self) {
     delete self;
}

void cv_delete_Vec3d(Vec3d* self) {
     delete self;
}

void cv_delete_Vec4d(Vec4d* self) {
     delete self;
}

void cv_delete_Vec6d(Vec6d* self) {
     delete self;
}

void cv_delete_Vec2f(Vec2f* self) {
     delete self;
}

void cv_delete_Vec3f(Vec3f* self) {
     delete self;
}

void cv_delete_Vec4f(Vec4f* self) {
     delete self;
}

void cv_delete_Vec6f(Vec6f* self) {
     delete self;
}

void cv_delete_Vec2i(Vec2i* self) {
     delete self;
}

void cv_delete_Vec3i(Vec3i* self) {
     delete self;
}

void cv_delete_Vec4i(Vec4i* self) {
     delete self;
}

void cv_delete_Vec6i(Vec6i* self) {
     delete self;
}

void cv_delete_Vec8i(Vec8i* self) {
     delete self;
}

void cv_delete_Vec2s(Vec2s* self) {
     delete self;
}

void cv_delete_Vec3s(Vec3s* self) {
     delete self;
}

void cv_delete_Vec4s(Vec4s* self) {
     delete self;
}

void cv_delete_Vec2w(Vec2w* self) {
     delete self;
}

void cv_delete_Vec3w(Vec3w* self) {
     delete self;
}

void cv_delete_Vec4w(Vec4w* self) {
     delete self;
}

void cv_delete_VideoCapture(VideoCapture* self) {
     delete self;
}

void cv_delete_VideoWriter(VideoWriter* self) {
     delete self;
}


}
