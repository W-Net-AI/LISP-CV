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

int cv_Mat_set_rows(Mat* self, int val) {
    return self->rows = val;
}

int cv_Mat_set_cols(Mat* self, int val) {
    return self->cols = val;
}

typedef vector<Vec2i> vector_Vec2i;

Vec2i* cv_vector_Vec2i_at_1(vector_Vec2i* self, int i) {
  return & (self->at(i));
}

int cv_vector_Vec2i_at_2(vector_Vec2i* self, int i, int j) {
  return (self->at(i)(j));
}

Vec2i* cv_vector_Vec2i_at_set_Val_1(vector_Vec2i* self, int idx, Vec2i* val) {
  return & (self->at(idx) = *val);
}

int cv_vector_Vec2i_at_set_Val_2(vector_Vec2i* self, int i, int j, int val) {
  return (self->at(i)(j) = val );
}

FlannBasedMatcher* cv_create_FlannBasedMatcher0() {
    return new FlannBasedMatcher();
}

Size* cv_Rect_set_size(Rect* self, Size* val) {
    return new Size(self->size() = *val);
}

void cv_Mat_set_Type(Mat* self, int val) {
    self->convertTo(*self, val);
}

Mat* cv_PCA_get_eigenvectors(PCA* self) {
    return new Mat(self->eigenvectors);
}

Mat* cv_PCA_set_Eigenvectors(PCA* self, Mat* val) {
   return new Mat(self->eigenvectors = *val);
}

Mat* cv_PCA_get_eigenvalues(PCA* self) {
    return new Mat(self->eigenvalues);
}

Mat* cv_PCA_set_Eigenvalues(PCA* self, Mat* val) {
   return new Mat(self->eigenvalues = *val);
}

Mat* cv_PCA_get_Mean(PCA* self) {
    return new Mat(self->mean);
}

Mat* cv_PCA_set_Mean(PCA* self, Mat* val) {
   return new Mat(self->mean = *val);
}

Mat* cv_PCA_BackProject1(PCA* self, Mat* vec) {
    return new Mat (self->backProject(*vec));
}

void cv_PCA_BackProject2(PCA* self, Mat* vec, Mat* result) {
    self->backProject(*vec, *result);
}

Mat* cv_PCA_Project1(PCA* self, Mat* vec) {
    return new Mat (self->project(*vec));
}

void cv_PCA_Project2(PCA* self, Mat* vec, Mat* result) {
    self->project(*vec, *result);
}

PCA* cv_create_PCA() {
    return new PCA();
}

PCA* cv_create_PCA4(Mat* data, Mat* mean, int flags, double retainedVariance) {
    return new PCA(*data, *mean, flags, retainedVariance);
}

Mat* test(uchar* a, size_t len ) {
Mat* m = new Mat;
for(size_t i = 0; i < len; i++) 
m->ptr(a[i]);
return m;
}

int cv_TermCriteria_getType(TermCriteria* self) {
  return self->type;
}

int cv_TermCriteria_set_Type(TermCriteria* self, int val) {
   return self->type = val;
}

int cv_TermCriteria_getMaxCount(TermCriteria* self) {
  return self->maxCount;
}

int cv_TermCriteria_set_MaxCount(TermCriteria* self, int val) {
   return self->maxCount = val;
}

double cv_TermCriteria_getEpsilon(TermCriteria* self) {
  return self->epsilon;
}

double cv_TermCriteria_set_Epsilon(TermCriteria* self, double val) {
   return self->epsilon = val;
}

void cv_circle_2(Mat* img, Point2f* center, int radius, Scalar* color, int thickness, int lineType, int shift) {
	cv::circle(*img, *center, radius, *color, thickness, lineType, shift);
}

void cv_RNG_fill(RNG* self, Mat* mat, int distType, Scalar* a, Scalar* b, bool saturateRange) {
    self->fill(*mat, distType, *a, *b, saturateRange);
}

vector_Mat* std_carrayTovectorm2(Mat** a, size_t len) {
    vector_Mat* v = new vector_Mat();
    for(size_t i = 0; i < len; i++) 
        v->push_back(*a[i]);
    return v;
}

typedef vector<Mat*> vector_Mat1;

vector_Mat1* std_carrayTovectorm1(Mat** a, size_t len) {
    vector_Mat1* v = new vector_Mat1();
    for(size_t i = 0; i < len; i++) 
        v->push_back(a[i]);
    return v;
}

Mat** std_vectorm_to_carray1( vector_Mat1* v ) {
return v->data();
}

void cv_findContours1(Mat* image, vector_Mat1* contours, Mat* hierarchy, int mode, int method, Point* offset) {
	cv::findContours(*image, *contours, *hierarchy, mode, method, *offset);
}

int cv_Mat_checkVector(Mat* self, int elemChannels, int depth, bool requireContinuous) {
    return self->checkVector(elemChannels, depth, requireContinuous);
}

double cv_Size_set_width(Size* self, double val) {
   return self->width = val;
}
    
double cv_Size_set_height(Size* self, double val) {
   return self->height = val;
}

void cv_BackgroundSubtractorMOG2_getBackgroundImage(BackgroundSubtractorMOG2* self, Mat* backgroundImage) {
    self->getBackgroundImage(*backgroundImage);
}

void cv_Mat_create_typed(Mat* self, int rows, int cols, int type) {
    self->create(rows, cols, type);
}

void cv_Mat_push_back(Mat* self, Mat* m) {
    self->push_back(*m);
}

void cv_RotatedRect_setCenter(RotatedRect* self, Point* val) {
  self->center = *val;
}

void cv_RotatedRect_setSize(RotatedRect* self, Size* val) {
  self->size = *val;
}

float cv_RotatedRect_setAngle(RotatedRect* self, float val) {
  return self->angle = val;
}

int cv_Rect_setWidth(Rect* self, int val) {
  return self->width = val;
}

int cv_Rect_setHeight(Rect* self, int val) {
  return self->height = val;
}

int cv_Rect_setX(Rect* self, int val) {
  return self->x = val;
}

int cv_Rect_setY(Rect* self, int val) {
  return self->y = val;
}

int cv_Point2i_setX(Point* self, int val) {
  return self->x = val;
}

int cv_Point2i_setY(Point* self, int val) {
  return self->y = val;
}

double cv_Point2d_setX(Point2d* self, double val) {
  return self->x = val;
}

double cv_Point2d_setY(Point2d* self, double val) {
  return self->y = val;
}

float cv_Point2f_setX(Point2f* self, float val) {
  return self->x = val;
}

float cv_Point2f_setY(Point2f* self, float val) {
  return self->y = val;
}

int cv_Point3i_setX(Point3i* self, int val) {
  return self->x = val;
}

int cv_Point3i_setY(Point3i* self, int val) {
  return self->y = val;
}

int cv_Point3i_setZ(Point3i* self, int val) {
  return self->z = val;
}

double cv_Point3d_setX(Point3d* self, double val) {
  return self->x = val;
}

double cv_Point3d_setY(Point3d* self, double val) {
  return self->y = val;
}

double cv_Point3d_setZ(Point3d* self, double val) {
  return self->z = val;
}

float cv_Point3f_setX(Point3f* self, float val) {
  return self->x = val;
}

float cv_Point3f_setY(Point3f* self, float val) {
  return self->y = val;
}

float cv_Point3f_setZ(Point3f* self, float val) {
  return self->z = val;
}

int cv_DMatch_getQueryIdx(DMatch* self) {
  return self->queryIdx;
}

int cv_DMatch_setQueryIdx(DMatch* self, int val) {
  return self->queryIdx = val;
}

int cv_DMatch_getTrainIdx(DMatch* self) {
  return self->trainIdx;
}

int cv_DMatch_setTrainIdx(DMatch* self, int val) {
  return self->trainIdx = val;
}

int cv_DMatch_getImgIdx(DMatch* self) {
  return self->imgIdx;
}

float cv_DMatch_setImgIdx(DMatch* self, float val) {
  return self->imgIdx = val;
}

float cv_DMatch_getDistance(DMatch* self) {
  return self->distance;
}

float cv_DMatch_setDistance(DMatch* self, float val) {
  return self->distance = val;
}

Point2f* cv_KeyPoint_getPt(KeyPoint* self) {
  return &self->pt;
}

void cv_KeyPoint_setPt(KeyPoint* self, Point2f* val) {
  self->pt = *val;
}

float cv_KeyPoint_getSize(KeyPoint* self) {
  return self->size;
}

float cv_KeyPoint_setSize(KeyPoint* self, float val) {
  return self->size = val;
}

float cv_KeyPoint_getAngle(KeyPoint* self) {
  return self->angle;
}

float cv_KeyPoint_setAngle(KeyPoint* self, float val) {
  return self->angle = val;
}

float cv_KeyPoint_getResponse(KeyPoint* self) {
  return self->response;
}

float cv_KeyPoint_setResponse(KeyPoint* self, float val) {
  return self->response = val;
}

int cv_KeyPoint_getOctave(KeyPoint* self) {
  return self->octave;
}

int cv_KeyPoint_setOctave(KeyPoint* self, int val) {
  return self->octave = val;
}

int cv_KeyPoint_getClass_id(KeyPoint* self) {
  return self->class_id;
}

int cv_KeyPoint_setClass_id(KeyPoint* self, int val) {
  return self->class_id = val;
}

Point* cv_Mat_at_Point_1(Mat* self, int i) {
  return &self->at<Point>(i);
}

Point* cv_Mat_at_Point_2(Mat* self, int i, int j) {
  return &self->at<Point>(i, j);
}

void cv_Mat_at_Point_set_Val_1(Mat* self, int i, Point* val) {
  self->at<Point>(i) = *val;
}

void cv_Mat_at_Point_set_Val_2(Mat* self, int i, int j, Point* val) {
  self->at<Point>(i, j) = *val;
}

Point2d* cv_Mat_at_Point2d_1(Mat* self, int i) {
  return &self->at<Point2d>(i);
}

Point2d* cv_Mat_at_Point2d_2(Mat* self, int i, int j) {
  return &self->at<Point2d>(i, j);
}

void cv_Mat_at_Point2d_set_Val_1(Mat* self, int i, Point2d* val) {
  self->at<Point2d>(i) = *val;
}

void cv_Mat_at_Point2d_set_Val_2(Mat* self, int i, int j, Point2d* val) {
  self->at<Point2d>(i, j) = *val;
}

Point2f* cv_Mat_at_Point2f_1(Mat* self, int i) {
  return &self->at<Point2f>(i);
}

Point2f* cv_Mat_at_Point2f_2(Mat* self, int i, int j) {
  return &self->at<Point2f>(i, j);
}

void cv_Mat_at_Point2f_set_Val_1(Mat* self, int i, Point2f* val) {
  self->at<Point2f>(i) = *val;
}

void cv_Mat_at_Point2f_set_Val_2(Mat* self, int i, int j, Point2f* val) {
  self->at<Point2f>(i, j) = *val;
}

Point3d* cv_Mat_at_Point3d_1(Mat* self, int i) {
  return &self->at<Point3d>(i);
}

Point3d* cv_Mat_at_Point3d_2(Mat* self, int i, int j) {
  return &self->at<Point3d>(i, j);
}

void cv_Mat_at_Point3d_set_Val_1(Mat* self, int i, Point3d* val) {
  self->at<Point3d>(i) = *val;
}

void cv_Mat_at_Point3d_set_Val_2(Mat* self, int i, int j, Point3d* val) {
  self->at<Point3d>(i, j) = *val;
}

Point3f* cv_Mat_at_Point3f_1(Mat* self, int i) {
  return &self->at<Point3f>(i);
}

Point3f* cv_Mat_at_Point3f_2(Mat* self, int i, int j) {
  return &self->at<Point3f>(i, j);
}

void cv_Mat_at_Point3f_set_Val_1(Mat* self, int i, Point3f* val) {
  self->at<Point3f>(i) = *val;
}

void cv_Mat_at_Point3f_set_Val_2(Mat* self, int i, int j, Point3f* val) {
  self->at<Point3f>(i, j) = *val;
}

Point3i* cv_Mat_at_Point3i_1(Mat* self, int i) {
  return &self->at<Point3i>(i);
}

Point3i* cv_Mat_at_Point3i_2(Mat* self, int i, int j) {
  return &self->at<Point3i>(i, j);
}

void cv_Mat_at_Point3i_set_Val_1(Mat* self, int i, Point3i* val) {
  self->at<Point3i>(i) = *val;
}

void cv_Mat_at_Point3i_set_Val_2(Mat* self, int i, int j, Point3i* val) {
  self->at<Point3i>(i, j) = *val;
}

Scalar* cv_Mat_at_Scalar(Mat* self, int i, int j) {
  return &self->at<Scalar>(i, j);
}

void cv_Mat_at_Scalar_set_Val(Mat* self, int i, int j, Scalar* val) {
  self->at<Scalar>(i, j) = *val;
}

char &cv_Mat_at_char_1(Mat* self, int i) {
  return self->at<char>(i);
}

double &cv_Mat_at_double_1(Mat* self, int i) {
  return self->at<double>(i);
}

float &cv_Mat_at_float_1(Mat* self, int i) {
  return self->at<float>(i);
}

int &cv_Mat_at_int_1(Mat* self, int i) {
  return self->at<int>(i);
}

short &cv_Mat_at_short_1(Mat* self, int i) {
  return self->at<short>(i);
}

uchar &cv_Mat_at_uchar_1(Mat* self, int i) {
  return self->at<uchar>(i);
}

ushort &cv_Mat_at_ushort_1(Mat* self, int i) {
  return self->at<ushort>(i);
}

char &cv_Mat_at_char_2(Mat* self, int i, int j) {
  return self->at<char>(i, j);
}

double &cv_Mat_at_double_2(Mat* self, int i, int j) {
   return self->at<double>(i,j);
}

float &cv_Mat_at_float_2(Mat* self, int i, int j) {
  return self->at<float>(i, j);
}

int &cv_Mat_at_int_2(Mat* self, int i, int j) {
  return self->at<int>(i, j);
}

short &cv_Mat_at_short_2(Mat* self, int i, int j) {
  return self->at<short>(i, j);
}

uchar &cv_Mat_at_uchar_2(Mat* self, int i, int j) {
  return self->at<uchar>(i, j);
}

ushort &cv_Mat_at_ushort_2(Mat* self, int i, int j) {
  return self->at<ushort>(i, j);
}

char &cv_Mat_at_char_3(Mat* self, int i, int j, int k) {
  return self->at<char>(i, j, k);
}

double &cv_Mat_at_double_3(Mat* self, int i, int j, int k) {
   return self->at<double>(i, j, k);
}

float &cv_Mat_at_float_3(Mat* self, int i, int j, int k) {
  return self->at<float>(i, j ,k);
}

int &cv_Mat_at_int_3(Mat* self, int i, int j, int k) {
  return self->at<int>(i, j, k);
}

short &cv_Mat_at_short_3(Mat* self, int i, int j, int k) {
  return self->at<short>(i, j, k);
}

uchar &cv_Mat_at_uchar_3(Mat* self, int i, int j, int k) {
  return self->at<uchar>(i, j, k);
}

ushort &cv_Mat_at_ushort_3(Mat* self, int i, int j, int k) {
  return self->at<ushort>(i, j, k);
}

Vec2b* cv_Mat_at_Vec2b_1(Mat* self, int i) {
  return &self->at<Vec2b>(i);
}

void cv_Mat_at_Vec2b_set_Val_1(Mat* self, int i, Vec2b* val) {
  self->at<Vec2b>(i) = *val;
}

Vec2b* cv_Mat_at_Vec2b_2(Mat* self, int i, int j) {
  return &self->at<Vec2b>(i, j);
}

void cv_Mat_at_Vec2b_set_Val_2(Mat* self, int i, int j, Vec2b* val) {
  self->at<Vec2b>(i, j) = *val;
}

Vec2d* cv_Mat_at_Vec2d_1(Mat* self, int i) {
  return &self->at<Vec2d>(i);
}

void cv_Mat_at_Vec2d_set_Val_1(Mat* self, int i, Vec2d* val) {
  self->at<Vec2d>(i) = *val;
}

Vec2d* cv_Mat_at_Vec2d_2(Mat* self, int i, int j) {
  return &self->at<Vec2d>(i, j);
}

void cv_Mat_at_Vec2d_set_Val_2(Mat* self, int i, int j, Vec2d* val) {
  self->at<Vec2d>(i, j) = *val;
}

Vec2f* cv_Mat_at_Vec2f_1(Mat* self, int i) {
  return &self->at<Vec2f>(i);
}

void cv_Mat_at_Vec2f_set_Val_1(Mat* self, int i, Vec2f* val) {
  self->at<Vec2f>(i) = *val;
}

Vec2f* cv_Mat_at_Vec2f_2(Mat* self, int i, int j) {
  return &self->at<Vec2f>(i, j);
}

void cv_Mat_at_Vec2f_set_Val_2(Mat* self, int i, int j, Vec2f* val) {
  self->at<Vec2f>(i, j) = *val;
}

Vec2i* cv_Mat_at_Vec2i_1(Mat* self, int i) {
  return &self->at<Vec2i>(i);
}

void cv_Mat_at_Vec2i_set_Val_1(Mat* self, int i, Vec2i* val) {
  self->at<Vec2i>(i) = *val;
}

Vec2i* cv_Mat_at_Vec2i_2(Mat* self, int i, int j) {
  return &self->at<Vec2i>(i, j);
}

void cv_Mat_at_Vec2i_set_Val_2(Mat* self, int i, int j, Vec2i* val) {
  self->at<Vec2i>(i, j) = *val;
}

Vec2s* cv_Mat_at_Vec2s_1(Mat* self, int i) {
  return &self->at<Vec2s>(i);
}

void cv_Mat_at_Vec2s_set_Val_1(Mat* self, int i, Vec2s* val) {
  self->at<Vec2s>(i) = *val;
}

Vec2s* cv_Mat_at_Vec2s_2(Mat* self, int i, int j) {
  return &self->at<Vec2s>(i, j);
}

void cv_Mat_at_Vec2s_set_Val_2(Mat* self, int i, int j, Vec2s* val) {
  self->at<Vec2s>(i, j) = *val;
}

Vec2w* cv_Mat_at_Vec2w_1(Mat* self, int i) {
  return &self->at<Vec2w>(i);
}

void cv_Mat_at_Vec2w_set_Val_1(Mat* self, int i, Vec2w* val) {
  self->at<Vec2w>(i) = *val;
}

Vec2w* cv_Mat_at_Vec2w_2(Mat* self, int i, int j) {
  return &self->at<Vec2w>(i, j);
}

void cv_Mat_at_Vec2w_set_Val_2(Mat* self, int i, int j, Vec2w* val) {
  self->at<Vec2w>(i, j) = *val;
}

Vec3b* cv_Mat_at_Vec3b_1(Mat* self, int i) {
  return &self->at<Vec3b>(i);
}

void cv_Mat_at_Vec3b_set_Val_1(Mat* self, int i, Vec3b* val) {
  self->at<Vec3b>(i) = *val;
}

Vec3b* cv_Mat_at_Vec3b_2(Mat* self, int i, int j) {
  return &self->at<Vec3b>(i, j);
}

void cv_Mat_at_Vec3b_set_Val_2(Mat* self, int i, int j, Vec3b* val) {
  self->at<Vec3b>(i, j) = *val;
}

Vec3d* cv_Mat_at_Vec3d_1(Mat* self, int i) {
  return &self->at<Vec3d>(i);
}

void cv_Mat_at_Vec3d_set_Val_1(Mat* self, int i, Vec3d* val) {
  self->at<Vec3d>(i) = *val;
}

Vec3d* cv_Mat_at_Vec3d_2(Mat* self, int i, int j) {
  return &self->at<Vec3d>(i, j);
}

void cv_Mat_at_Vec3d_set_Val_2(Mat* self, int i, int j, Vec3d* val) {
  self->at<Vec3d>(i, j) = *val;
}

Vec3f* cv_Mat_at_Vec3f_1(Mat* self, int i) {
  return &self->at<Vec3f>(i);
}

void cv_Mat_at_Vec3f_set_Val_1(Mat* self, int i, Vec3f* val) {
  self->at<Vec3f>(i) = *val;
}

Vec3f* cv_Mat_at_Vec3f_2(Mat* self, int i, int j) {
  return &self->at<Vec3f>(i, j);
}

void cv_Mat_at_Vec3f_set_Val_2(Mat* self, int i, int j, Vec3f* val) {
  self->at<Vec3f>(i, j) = *val;
}

Vec3i* cv_Mat_at_Vec3i_1(Mat* self, int i) {
  return &self->at<Vec3i>(i);
}

void cv_Mat_at_Vec3i_set_Val_1(Mat* self, int i, Vec3i* val) {
  self->at<Vec3i>(i) = *val;
}

Vec3i* cv_Mat_at_Vec3i_2(Mat* self, int i, int j) {
  return &self->at<Vec3i>(i, j);
}

void cv_Mat_at_Vec3i_set_Val_2(Mat* self, int i, int j, Vec3i* val) {
  self->at<Vec3i>(i, j) = *val;
}

Vec3s* cv_Mat_at_Vec3s_1(Mat* self, int i) {
  return &self->at<Vec3s>(i);
}

void cv_Mat_at_Vec3s_set_Val_1(Mat* self, int i, Vec3s* val) {
  self->at<Vec3s>(i) = *val;
}

Vec3s* cv_Mat_at_Vec3s_2(Mat* self, int i, int j) {
  return &self->at<Vec3s>(i, j);
}

void cv_Mat_at_Vec3s_set_Val_2(Mat* self, int i, int j, Vec3s* val) {
  self->at<Vec3s>(i, j) = *val;
}

Vec3w* cv_Mat_at_Vec3w_1(Mat* self, int i) {
  return &self->at<Vec3w>(i);
}

void cv_Mat_at_Vec3w_set_Val_1(Mat* self, int i, Vec3w* val) {
  self->at<Vec3w>(i) = *val;
}

Vec3w* cv_Mat_at_Vec3w_2(Mat* self, int i, int j) {
  return &self->at<Vec3w>(i, j);
} 

void cv_Mat_at_Vec3w_set_Val_2(Mat* self, int i, int j, Vec3w* val) {
  self->at<Vec3w>(i, j) = *val;
}

Vec4b* cv_Mat_at_Vec4b_1(Mat* self, int i) {
  return &self->at<Vec4b>(i);
}

void cv_Mat_at_Vec4b_set_Val_1(Mat* self, int i, Vec4b* val) {
  self->at<Vec4b>(i) = *val;
}

Vec4b* cv_Mat_at_Vec4b_2(Mat* self, int i, int j) {
  return &self->at<Vec4b>(i, j);
}

void cv_Mat_at_Vec4b_set_Val_2(Mat* self, int i, int j, Vec4b* val) {
  self->at<Vec4b>(i, j) = *val;
}

Vec4d* cv_Mat_at_Vec4d_1(Mat* self, int i) {
  return &self->at<Vec4d>(i);
}

void cv_Mat_at_Vec4d_set_Val_1(Mat* self, int i, Vec4d* val) {
  self->at<Vec4d>(i) = *val;
}

Vec4d* cv_Mat_at_Vec4d_2(Mat* self, int i, int j) {
  return &self->at<Vec4d>(i, j);
}

void cv_Mat_at_Vec4d_set_Val_2(Mat* self, int i, int j, Vec4d* val) {
  self->at<Vec4d>(i, j) = *val;
}

Vec4f* cv_Mat_at_Vec4f_1(Mat* self, int i) {
  return &self->at<Vec4f>(i);
}

void cv_Mat_at_Vec4f_set_Val_1(Mat* self, int i, Vec4f* val) {
  self->at<Vec4f>(i) = *val;
}

Vec4f* cv_Mat_at_Vec4f_2(Mat* self, int i, int j) {
  return &self->at<Vec4f>(i, j);
}

void cv_Mat_at_Vec4f_set_Val_2(Mat* self, int i, int j, Vec4f* val) {
  self->at<Vec4f>(i, j) = *val;
}

Vec4i* cv_Mat_at_Vec4i_1(Mat* self, int i) {
  return &self->at<Vec4i>(i);
}

void cv_Mat_at_Vec4i_set_Val_1(Mat* self, int i, Vec4i* val) {
  self->at<Vec4i>(i) = *val;
}

Vec4i* cv_Mat_at_Vec4i_2(Mat* self, int i, int j) {
  return &self->at<Vec4i>(i, j);
}

void cv_Mat_at_Vec4i_set_Val_2(Mat* self, int i, int j, Vec4i* val) {
  self->at<Vec4i>(i, j) = *val;
}

Vec4s* cv_Mat_at_Vec4s_1(Mat* self, int i) {
  return &self->at<Vec4s>(i);
}

void cv_Mat_at_Vec4s_set_Val_1(Mat* self, int i, Vec4s* val) {
  self->at<Vec4s>(i) = *val;
}

Vec4s* cv_Mat_at_Vec4s_2(Mat* self, int i, int j) {
  return &self->at<Vec4s>(i, j);
}

void cv_Mat_at_Vec4s_set_Val_2(Mat* self, int i, int j, Vec4s* val) {
  self->at<Vec4s>(i, j) = *val;
}

Vec4w* cv_Mat_at_Vec4w_1(Mat* self, int i) {
  return &self->at<Vec4w>(i);
}

void cv_Mat_at_Vec4w_set_Val_1(Mat* self, int i, Vec4w* val) {
  self->at<Vec4w>(i) = *val;
}

Vec4w* cv_Mat_at_Vec4w_2(Mat* self, int i, int j) {
  return &self->at<Vec4w>(i, j);
}

void cv_Mat_at_Vec4w_set_Val_2(Mat* self, int i, int j, Vec4w* val) {
  self->at<Vec4w>(i, j) = *val;
}

const float* cv_CvSVM_get_support_vector(SVM* self, int i) {
    return self->get_support_vector(i);
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

void cv_delete_FastFeatureDetector(FastFeatureDetector* self) {
    delete self;
}

void cv_delete_FileNode(FileNode* self) {
    delete self;
}

void cv_delete_FileStorage(FileStorage* self) {
    delete self;
}

void cv_delete_FlannBasedMatcher(FlannBasedMatcher* self) {
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

void cv_delete_PCA(PCA* self) {
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
