/*
 * =====================================================================================
 *
 *       Filename:  mat.hpp
 *
 *    Description:  Wrappers for the OpenCV Matrix class
 *
 *        Version:  1.0
 *        Created:  09/24/13 20:01:07
 *       Revision:  none
 *       Compiler:  g++
 *
 *         Author:  Arjun Comar and Joe W. BiMedina
 *
 * =====================================================================================
 */

#include <opencv2/c/opencv_generated.hpp>

extern "C" {
Mat* cv_Mat_get_ROI(Mat* self, Rect* roi);
MatExpr* cv_Mat_div(Mat* m1, Mat* m2);
Mat* cv_create_sized_identity (Size* s, int type);
Mat* cv_create_Mat_with_value(int rows, int cols, int type, Scalar* s);
MatExpr* cv_Mat_sub(Mat* m1, Mat* m2);
Mat* cv_create_Mat_typed(int rows, int cols, int type);
Mat* cv_create_Mat_with_data(int rows, int cols, int type, void* data );
Point* cv_Mat_at_Point0(Mat* self, int i, int j);
Point2d* cv_Mat_at_Point2d0(Mat* self, int i, int j);
Point2f* cv_Mat_at_Point2f0(Mat* self, int i, int j);
Point3d* cv_Mat_at_Point3d0(Mat* self, int i, int j);
Point3f* cv_Mat_at_Point3f0(Mat* self, int i, int j);
Point3i* cv_Mat_at_Point3i0(Mat* self, int i, int j);
Scalar* cv_Mat_at_Scalar0(Mat* self, int i, int j);
short cv_Mat_at_short0(Mat* self, int i, int j);
char cv_Mat_at_char0(Mat* self, int i, int j);
uchar cv_Mat_at_uchar0(Mat* self, int i, int j);
uint cv_Mat_at_uint0(Mat* self, int i, int j);
ushort cv_Mat_at_ushort0(Mat* self, int i, int j);
float cv_Mat_at_float0(Mat* self, int i, int j);
int cv_Mat_at_int0(Mat* self, int i, int j);
double cv_Mat_at_double0(cv::Mat* M, int i, int j);
Vec2b* cv_Mat_at_Vec2b0(Mat* self, int i, int j);
Vec2d* cv_Mat_at_Vec2d0(Mat* self, int i, int j);
Vec2f* cv_Mat_at_Vec2f0(Mat* self, int i, int j);
Vec2i* cv_Mat_at_Vec2i0(Mat* self, int i, int j);
Vec2s* cv_Mat_at_Vec2s0(Mat* self, int i, int j);
Vec3b* cv_Mat_at_Vec3b0(Mat* self, int i, int j);
Vec3d* cv_Mat_at_Vec3d0(Mat* self, int i, int j);
Vec3f* cv_Mat_at_Vec3f0(Mat* self, int i, int j);
Vec3i* cv_Mat_at_Vec3i0(Mat* self, int i, int j);
Vec3s* cv_Mat_at_Vec3s0(Mat* self, int i, int j);
Vec4b* cv_Mat_at_Vec4b0(Mat* self, int i, int j);
Vec4d* cv_Mat_at_Vec4d0(Mat* self, int i, int j);
Vec4f* cv_Mat_at_Vec4f0(Mat* self, int i, int j);
Vec4i* cv_Mat_at_Vec4i0(Mat* self, int i, int j);
Vec4s* cv_Mat_at_Vec4s0(Mat* self, int i, int j);
Point* cv_Mat_at_Point1(Mat* self, int i, int j);
Point2d* cv_Mat_at_Point2d1(Mat* self, int i, int j);
Point2f* cv_Mat_at_Point2f1(Mat* self, int i, int j);
Point3d* cv_Mat_at_Point3d1(Mat* self, int i, int j);
Point3f* cv_Mat_at_Point3f1(Mat* self, int i, int j);
Point3i* cv_Mat_at_Point3i1(Mat* self, int i, int j);
Scalar* cv_Mat_at_Scalar1(Mat* self, int i, int j);
short &cv_Mat_at_short1(Mat* self, int i, int j);
char &cv_Mat_at_char1(Mat* self, int i, int j);
uchar &cv_Mat_at_uchar1(Mat* self, int i, int j);
uint &cv_Mat_at_uint1(Mat* self, int i, int j);
ushort &cv_Mat_at_ushort1(Mat* self, int i, int j);
float &cv_Mat_at_float1(Mat* self, int i, int j);
int &cv_Mat_at_int1(Mat* self, int i, int j);
double &cv_Mat_at_double1(cv::Mat* M, int i, int j);
Vec2b* cv_Mat_at_Vec2b1(Mat* self, int i, int j);
Vec2d* cv_Mat_at_Vec2d1(Mat* self, int i, int j);
Vec2f* cv_Mat_at_Vec2f1(Mat* self, int i, int j);
Vec2i* cv_Mat_at_Vec2i1(Mat* self, int i, int j);
Vec2s* cv_Mat_at_Vec2s1(Mat* self, int i, int j);
Vec3b* cv_Mat_at_Vec3b1(Mat* self, int i, int j);
Vec3d* cv_Mat_at_Vec3d1(Mat* self, int i, int j);
Vec3f* cv_Mat_at_Vec3f1(Mat* self, int i, int j);
Vec3i* cv_Mat_at_Vec3i1(Mat* self, int i, int j);
Vec3s* cv_Mat_at_Vec3s1(Mat* self, int i, int j);
Vec4b* cv_Mat_at_Vec4b1(Mat* self, int i, int j);
Vec4d* cv_Mat_at_Vec4d1(Mat* self, int i, int j);
Vec4f* cv_Mat_at_Vec4f1(Mat* self, int i, int j);
Vec4i* cv_Mat_at_Vec4i1(Mat* self, int i, int j);
Vec4s* cv_Mat_at_Vec4s1(Mat* self, int i, int j);
MatExpr* cv_abs(Mat* m);
Mat* cv_create_Mat();
Mat* cv_Mat_assign(Mat* self, Mat* m);
Mat* cv_Mat_assignVal(Mat* self, Scalar* s);
Mat* cv_Mat_getRow(Mat* self, int y);
Mat* cv_Mat_getCol(Mat* self, int x);
Mat* cv_Mat_getRowRange(Mat* self, int startrow, int endrow);
Mat* cv_Mat_getColRange(Mat* self, int startrow, int endrow);
Mat* cv_Mat_diag(Mat* self);
Mat* cv_Mat_diag_d(Mat* self, int d);
Mat* cv_create_diagMat(Mat* d);
Mat* cv_Mat_clone(Mat* self);
void cv_Mat_copyTo(Mat* self, Mat* m);
void cv_Mat_copyTo_masked(Mat* self, Mat* m, Mat* mask);
void cv_Mat_assignTo(Mat* self, Mat* m);
void cv_Mat_assignTo_t(Mat*self, Mat* m, int t);
Mat* cv_Mat_setTo(Mat*self, Scalar* value);
Mat* cv_Mat_setTo_masked(Mat* self, Scalar* value, Mat* mask);
Mat* cv_Mat_reshape(Mat* self, int cn);
Mat* cv_Mat_reshape_rows(Mat* self, int cn, int rows);
size_t cv_Mat_elemSize(Mat* self);
size_t cv_Mat_elemSize1(Mat* self);
int cv_Mat_type(Mat* self);
int cv_Mat_depth(Mat* self);
size_t cv_Mat_total(Mat* self);
bool cv_Mat_isContinuous(Mat* self);
int cv_Mat_channels(Mat* self);
int cv_Mat_rows(Mat* self);
int cv_Mat_cols(Mat* self);
int cv_Mat_empty(Mat* self);
Size* cv_Mat_size(Mat* self);
size_t cv_Mat_step1(Mat* self);
uchar* cv_Mat_ptr(Mat* self);
uchar* cv_Mat_ptr_index(Mat* self, int i);
Mat* cv_create_identity0(int rows, int cols, int type);
Mat* cv_create_ones(int rows, int cols, int type);
Mat* cv_create_zeros(int rows, int cols, int type);
MatExpr* cv_Mat_transpose_mat(Mat* self);
MatExpr* cv_Mat_inv_mat(Mat* self, int method);
MatExpr* cv_Mat_add(Mat* m1, Mat* m2);
MatExpr* cv_Mat_mult(Mat* m1, Mat* m2);
Mat* force(MatExpr* expr);
MatExpr* promote(Mat* m);
MatExpr* cv_Mat_scale(MatExpr* m, double alpha);
double cv_Mat_dot(Mat* self, Mat* m);
Mat* cv_Mat_cross(Mat* self, Mat* m);
void cv_Mat_locateROI(Mat* self, Size* s, Point* p);
Mat* cv_Mat_adjustROI(Mat* self, int dtop, int dbottom, int dleft, int dright);
}

