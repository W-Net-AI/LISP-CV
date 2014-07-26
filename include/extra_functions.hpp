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
Point* cv_Mat_at_Point(Mat* self, int i, int j);
void cv_Mat_at_Point_set_Val(Mat* self, int i, int j, Point* val);
Point2d* cv_Mat_at_Point2d(Mat* self, int i, int j);
void cv_Mat_at_Point2d_set_Val(Mat* self, int i, int j, Point2d* val);
Point2f* cv_Mat_at_Point2f(Mat* self, int i, int j);
void cv_Mat_at_Point2f_set_Val(Mat* self, int i, int j, Point2f* val);
Point3d* cv_Mat_at_Point3d(Mat* self, int i, int j);
void cv_Mat_at_Point3d_set_Val(Mat* self, int i, int j, Point3d* val);
Point3f* cv_Mat_at_Point3f(Mat* self, int i, int j);
void cv_Mat_at_Point3f_set_Val(Mat* self, int i, int j, Point3f* val);
Point3i* cv_Mat_at_Point3i(Mat* self, int i, int j);
void cv_Mat_at_Point3i_set_Val(Mat* self, int i, int j, Point3i* val);
Scalar* cv_Mat_at_Scalar(Mat* self, int i, int j);
void cv_Mat_at_Scalar_set_Val(Mat* self, int i, int j, Scalar* val);
char &cv_Mat_at_char_2(Mat* self, int i, int j);
double &cv_Mat_at_double_2(Mat* self, int i, int j);
float &cv_Mat_at_float_2(Mat* self, int i, int j);
int &cv_Mat_at_int_2(Mat* self, int i, int j);
short &cv_Mat_at_short_2(Mat* self, int i, int j);
uchar &cv_Mat_at_uchar_2(Mat* self, int i, int j);
ushort &cv_Mat_at_ushort_2(Mat* self, int i, int j);
char &cv_Mat_at_char_3(Mat* self, int i, int j, int k);
double &cv_Mat_at_double_3(Mat* self, int i, int j, int k);
float &cv_Mat_at_float_3(Mat* self, int i, int j, int k);
int &cv_Mat_at_int_3(Mat* self, int i, int j, int k);
short &cv_Mat_at_short_3(Mat* self, int i, int j, int k);
uchar &cv_Mat_at_uchar_3(Mat* self, int i, int j, int k);
ushort &cv_Mat_at_ushort_3(Mat* self, int i, int j, int k);

Vec2b* cv_Mat_at_Vec2b(Mat* self, int i, int j);
void cv_Mat_at_Vec2b_set_Val(Mat* self, int i, int j, Vec2b* val);

Vec2d* cv_Mat_at_Vec2d(Mat* self, int i, int j);
void cv_Mat_at_Vec2d_set_Val(Mat* self, int i, int j, Vec2d* val);

Vec2f* cv_Mat_at_Vec2f(Mat* self, int i, int j);
void cv_Mat_at_Vec2f_set_Val(Mat* self, int i, int j, Vec2f* val);

Vec2i* cv_Mat_at_Vec2i(Mat* self, int i, int j);
void cv_Mat_at_Vec2i_set_Val(Mat* self, int i, int j, Vec2i* val);

Vec2s* cv_Mat_at_Vec2s(Mat* self, int i, int j);
void cv_Mat_at_Vec2s_set_Val(Mat* self, int i, int j, Vec2s* val);

Vec2w* cv_Mat_at_Vec2w(Mat* self, int i, int j);
void cv_Mat_at_Vec2w_set_Val(Mat* self, int i, int j, Vec2w* val);

Vec3b* cv_Mat_at_Vec3b(Mat* self, int i, int j);
void cv_Mat_at_Vec3b_set_Val(Mat* self, int i, int j, Vec3b* val);

Vec3d* cv_Mat_at_Vec3d(Mat* self, int i, int j);
void cv_Mat_at_Vec3d_set_Val(Mat* self, int i, int j, Vec3d* val);

Vec3f* cv_Mat_at_Vec3f(Mat* self, int i, int j);
void cv_Mat_at_Vec3f_set_Val(Mat* self, int i, int j, Vec3f* val);

Vec3i* cv_Mat_at_Vec3i(Mat* self, int i, int j);
void cv_Mat_at_Vec3i_set_Val(Mat* self, int i, int j, Vec3i* val);

Vec3s* cv_Mat_at_Vec3s(Mat* self, int i, int j);
void cv_Mat_at_Vec3s_set_Val(Mat* self, int i, int j, Vec3s* val);

Vec3w* cv_Mat_at_Vec3w(Mat* self, int i, int j);
void cv_Mat_at_Vec3w_set_Val(Mat* self, int i, int j, Vec3w* val);

Vec4b* cv_Mat_at_Vec4b(Mat* self, int i, int j);
void cv_Mat_at_Vec4b_set_Val(Mat* self, int i, int j, Vec4b* val);

Vec4d* cv_Mat_at_Vec4d(Mat* self, int i, int j);
void cv_Mat_at_Vec4d_set_Val(Mat* self, int i, int j, Vec4d* val);

Vec4f* cv_Mat_at_Vec4f(Mat* self, int i, int j);
void cv_Mat_at_Vec4f_set_Val(Mat* self, int i, int j, Vec4f* val);

Vec4i* cv_Mat_at_Vec4i(Mat* self, int i, int j);
void cv_Mat_at_Vec4i_set_Val(Mat* self, int i, int j, Vec4i* val);

Vec4s* cv_Mat_at_Vec4s(Mat* self, int i, int j);
void cv_Mat_at_Vec4s_set_Val(Mat* self, int i, int j, Vec4s* val);

Vec4w* cv_Mat_at_Vec4w(Mat* self, int i, int j); 
void cv_Mat_at_Vec4w_set_Val(Mat* self, int i, int j, Vec4w* val);

int cv_vector_int_get_value (vector_int* self, int idx);
const float* cv_CvSVM_get_support_vector(SVM* self, int i);
CvMat* cv_Mat_to_CvMat(Mat* self);
CvTermCriteria cv_TermCriteria_to_CvTermCriteria(TermCriteria* self);
Mat* cv_imdecode_2(vector_uchar* buf, int flags);
bool cv_imencode_2(const char* ext, Mat* img, vector_uchar* buf, vector_int* params);
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
CvSVMParams* cv_create_CvSVMParams10(int svm_type, int kernel_type, double degree, double gamma, double coef0, double Cvalue, double nu, double p, CvMat* class_weights, CvTermCriteria term_crit);
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
void cv_delete_CvMat(CvMat* self);
void cv_delete_BFMatcher(BFMatcher* self);
void cv_delete_BRISK(BRISK* self);
void cv_delete_CascadeClassifier(CascadeClassifier* self);
void cv_delete_CvANN_MLP(CvANN_MLP* self);
void cv_delete_CvANN_MLP_TrainParams(CvANN_MLP_TrainParams* self);
void cv_delete_CvNat(CvMat* self);
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
void cv_delete_CvSVM(CvSVM* self);
void cv_delete_CvSVMParams(CvSVMParams* self);
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
