/*
* =====================================================================================
*
* Filename: excluded_functions.hpp
*
* Description: Functions that the generator outputs incorrectly, either by them entirely
* or by outputting them with incorrectly specified types.
*
* Version: 1.0
* Created: 04/13/2014 12:00:46 AM
* Revision: none
* Compiler: g++
*
* Author: Arjun Comar
*
* =====================================================================================
*/

#include <opencv2/c/opencv_generated.hpp>

#define ADD_VEC_FUNC_HEADERS_0_0(t) \
    Vec2##t * cv_create_0_Vec2##t(); \
    Vec3##t * cv_create_0_Vec3##t(); \
    Vec4##t * cv_create_0_Vec4##t(); \

#define ADD_VEC_FUNC_HEADERS_0_1(t) \
    Vec6##t * cv_create_0_Vec6##t(); \

#define ADD_VEC_FUNC_HEADERS_0_2(t) \
    Vec8##t * cv_create_0_Vec8##t(); \

#define ADD_VEC_FUNC_HEADERS_1(t, tn) \
    Vec2##t * cv_create_Vec2##t(tn v0, tn v1); \
    Vec3##t * cv_create_Vec3##t(tn v0, tn v1, tn v2); \
    Vec4##t * cv_create_Vec4##t(tn v0, tn v1, tn v2, tn v3); \

#define ADD_VEC_FUNC_HEADERS_2(t, tn) \
    Vec6##t * cv_create_Vec6##t(tn v0,  tn v1, tn v2, tn v3. tn v4, tn v5); \

#define ADD_VEC_FUNC_HEADERS_3(t, tn) \
    Vec8##t * cv_create_Vec8##t(tn v0, tn v1, tn v2, tn v3, tn v4, tn v5, tn v6, tn v7); \

#define ADD_WRITE_FUNC_HEADERS_0(t, tn) \
typedef vector< tn > vector_##tn; \
    void cv_FileNode_write_number_##t(FileStorage* fs, String* name,  tn value); \

#define ADD_WRITE_FUNC_HEADERS_1(t, tn) \
    void cv_FileNode_write_pointer_##t(FileStorage* fs, String* name,  tn* value); \

#define ADD_READ_FUNC_HEADERS_0(t, tn) \
    void cv_FileNode_read_number_##t (FileNode* fs, tn value,  tn default_value); \

#define ADD_READ_FUNC_HEADERS_1(t, tn) \
    void cv_FileNode_read_pointer_##t(FileNode* fs, tn* value,  tn* default_value); \

#define ADD_READ_FUNC_HEADERS_2(t, tn) \
    void cv_FileNode_read_pointer_vkp(FileNode* node, tn* keypoints); \

extern "C" {
ADD_VEC_FUNC_HEADER_0_0(b);
ADD_VEC_FUNC_HEADER_0_0(d);
ADD_VEC_FUNC_HEADER_0_0(f);
ADD_VEC_FUNC_HEADER_0_0(i);
ADD_VEC_FUNC_HEADER_0_0(s);
ADD_VEC_FUNC_HEADER_0_0(w);
ADD_VEC_FUNC_HEADER_0_1(d);
ADD_VEC_FUNC_HEADER_0_1(f);
ADD_VEC_FUNC_HEADER_0_1(i);
ADD_VEC_FUNC_HEADER_0_2(i);
ADD_VEC_FUNC_HEADER_1(b, uchar);
ADD_VEC_FUNC_HEADER_1(d, double);
ADD_VEC_FUNC_HEADER_1(f, float);
ADD_VEC_FUNC_HEADER_1(i, int);
ADD_VEC_FUNC_HEADER_1(s, short);
ADD_VEC_FUNC_HEADER_1(w, ushort);
ADD_VEC_FUNC_HEADER_2(d, double);
ADD_VEC_FUNC_HEADER_2(f, float);
ADD_VEC_FUNC_HEADER_2(i, int);
ADD_VEC_FUNC_HEADER_3(i, int);
ADD_WRITE_FUNC_HEADERS_0(i, int);
ADD_WRITE_FUNC_HEADERS_0(f, float);
ADD_WRITE_FUNC_HEADERS_0(d, double);
ADD_WRITE_FUNC_HEADERS_1(s, String);
ADD_WRITE_FUNC_HEADERS_1(m, Mat);
ADD_WRITE_FUNC_HEADERS_1(vkp, vector_KeyPoint);
ADD_READ_FUNC_HEADERS_0(i, int);
ADD_READ_FUNC_HEADERS_0(f, float);
ADD_READ_FUNC_HEADERS_0(d, double);
ADD_READ_FUNC_HEADERS_1(s, String);
ADD_READ_FUNC_HEADERS_1(m, Mat);
int CV_FOURCC(char c1, char c2, char c3, char c4);
void cv_groupRectangles_3(vector_Rect* rectList, int groupThreshold, double eps);
void cv_FileNode_read_pointer_vkp (FileNode* fs, vector_KeyPoint* keypoints):
FileNode * cv_FileNode_assignVal(FileStorage * fs, string * nodename);
FileStorage* cv_FileStorage_write_String(FileStorage* fs, String* value);
Scalar* cv_create_Scalar0();
Scalar* cv_create_Scalar4(double val0, double val1, double val2, double val3);
CvDTreeParams* cv_create_CvDTreeParams();
CvDTreeParams* cv_create_CvDTreeParams9(int max_depth, int min_sample_count, float regression_accuracy, bool use_surrogates, int max_categories, int cv_folds, bool use_1se_rule, bool truncate_pruned_tree, const float* priors);
Mat* cv_Mat_with_Range(Mat* self, Range* rowRange, Range* colRange);
Range* cv_create_Range(int _start, int _end);
Range* cv_create_RangeAll();
bool cv_Range_empty(Range* self);
int cv_Range_size(Range* self);
int cv_Range_getstart(Range* self);
int cv_Range_getend(Range* self);
void cv_HOGDescriptor_detectMultiScale9_2(HOGDescriptor* self, Mat* img, vector_Rect* foundLocations, double hitThreshold, Size* winStride, Size* padding, double scale, double finalThreshold, bool useMeanshiftGrouping);
void cv_HOGDescriptor_setSVMDetector2(HOGDescriptor* self, vector_float* _svmdetector);
CvSVMParams* cv_create_CvSVMParams(); 
//CvSVMParams* cv_create_CvSVMParams10(int svm_type, int kernel_type, double degree, double gamma, double coef0, double Cvalue, double nu, double p, CvMat class_weights, TermCriteria term_crit);
void cv_displayOverlay(String* winname, String* text, int delayms);
MatExpr* cv_abs(Mat* m);
float cv_RotatedRect_angle(RotatedRect* self);
Rect* cv_RotatedRect_boundingRect(RotatedRect* self);
MatExpr* cv_Mat_div(Mat* m1, Mat* m2);
FeatureDetector* cv_FeatureDetector_create2(FeatureDetector* self, const char* detectorType);
Scalar* cv_create_morphologyDefaultBorderValue();
bool cv_findChessboardCorners2(Mat* image, Size* patternSize, vector_Point2f* corners, int flags);
void cv_cornerSubPix2(Mat* image, vector_Point2f* corners, Size* winSize, Size* zeroZone, TermCriteria* criteria);
void cv_drawChessboardCorners2(Mat* image, Size* patternSize, vector_Point2f* corners, bool patternWasFound);
TermCriteria* cv_create_TermCriteria();
TermCriteria* cv_create_TermCriteria3(int type, int maxCount, double epsilon);
MatExpr* cv_Mat_sub(Mat* m1, Mat* m2);
RNG* cv_create_RNG();
RNG* cv_create_RNG_state(uint64 state);
double cv_RNG_uniform_double(RNG* self, double a, double b);
float cv_RNG_uniform_float(RNG* self, float a, float b);
int cv_RNG_uniform_int(RNG* self, int a, int b);
uchar* cv_Mat_get_Data(Mat* self);
Mat* cv_Mat_get_ROI(Mat* self, Rect* roi);
Mat* cv_imread2(const char* filename, int flags);
bool cv_imwrite2(const char* filename, Mat* img, vector_int* params);
FeatureDetector* cv_FeatureDetector_create2(FeatureDetector* self, const char* detectorType);
Size2f* cv_create_Size2f();
Size2f* cv_create_Size2f2(float width, float height);
Size2f* cv_Size_assignTo(Size2f* self, Size2f* other);
Size2f* cv_Size2f_fromPoint2f(Point2f* p);
float cv_Size2f_area(Size2f* self);
float cv_Size2f_width(Size2f* self);
float cv_Size2f_height(Size2f* self);
void cv_delete_CvANN_MLP(CvANN_MLP* self);
void cv_delete_CvANN_MLP_TrainParams(CvANN_MLP_TrainParams* self);
void cv_delete_BFMatcher(BFMatcher* self);
void cv_delete_BRISK(BRISK* self);
void cv_delete_CascadeClassifier(CascadeClassifier* self);
void cv_delete_DescriptorExtractor(DescriptorExtractor* self);
void cv_delete_DescriptorMatcher(DescriptorMatcher* self);
void cv_delete_DMatch(DMatch* ptr);
void cv_delete_CvDTree(CvDTree* self);
void cv_delete_CvDTreeParams(CvDTreeParams* self);
void cv_delete_Feature2D(Feature2D* self);
void cv_delete_FeatureDetector(FeatureDetector* self);
void cv_delete_FileNode(FileNode* self);
void cv_delete_FileStorage(FileStorage* self);
void cv_delete_HOGDescriptor(HOGDescriptor* self);
void cv_delete_KeyPoint(KeyPoint* self);
void cv_delete_CvKNearest(CvKNearest* self);
void cv_delete_CvNormalBayesClassifier(CvNormalBayesClassifier* self);
void cv_delete_Point(Point* self);
void cv_delete_Point2d(Point2d* self);
void cv_delete_Point2f(Point2f* self);
void cv_delete_Point3d(Point3d* self);
void cv_delete_Point3f(Point3f* self);
void cv_delete_Point3i(Point3i* self);
void cv_delete_Range(Range* self);
void cv_delete_Rect(Rect* self);
void cv_delete_RNG(RNG* self);
void cv_delete_RotatedRect(RotatedRect* self);
void cv_delete_Scalar(Scalar* self);
void cv_delete_Size(Size* self);
void cv_delete_Size2f(Size2f* self);
void cv_delete_SURF(SURF* self);
void cv_delete_TermCriteria(TermCriteria* self);
void cv_delete_Vec2b(Vec2b* self); 
void cv_delete_Vec3b(Vec3b* self);
void cv_delete_Vec4b(Vec4b* self); 
void cv_delete_Vec2d(Vec2d* self);
void cv_delete_Vec3d(Vec3d* self);
void cv_delete_Vec4d(Vec4d* self);
void cv_delete_Vec6d(Vec6d* self);
void cv_delete_Vec2f(Vec2f* self);
void cv_delete_Vec3f(Vec3f* self);
void cv_delete_Vec4f(Vec4f* self); 
void cv_delete_Vec6f(Vec6f* self);
void cv_delete_Vec2i(Vec2i* self);
void cv_delete_Vec3i(Vec3i* self);
void cv_delete_Vec4i(Vec4i* self);
void cv_delete_Vec6i(Vec6i* self);
void cv_delete_Vec8i(Vec8i* self); 
void cv_delete_Vec2s(Vec2s* self);
void cv_delete_Vec3s(Vec3s* self);
void cv_delete_Vec4s(Vec4s* self);
void cv_delete_Vec2w(Vec2w* self); 
void cv_delete_Vec3w(Vec3w* self);
void cv_delete_Vec4w(Vec4w* self);
void cv_delete_VideoCapture(VideoCapture* self);
void cv_delete_VideoWriter(VideoWriter* self);
}
