/*
 * =====================================================================================
 *
 *       Filename:  mat.cpp
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  09/24/13 20:12:17
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  YOUR NAME (), 
 *   Organization:  
 *
 * =====================================================================================
 */
#include <opencv2/c/mat.hpp>

extern "C" {
Mat* cv_create_Mat_as_vectort(vector_Point2f* vec, bool copyData) {
    return new Mat(*vec, copyData);
}

uchar* cv_Mat_get_Data(Mat* self) {
    return self->data;
}

size_t cv_Mat_get_Step(Mat* self) {
    return self->step;
}

void cv_Mat_convertTo(Mat* self,Mat* m, int rtype, double alpha, double beta) {
    self->convertTo(*m, rtype, alpha, beta);
}

MatExpr* cv_Mat_div_S(Mat* m1, int m2) {
    return new MatExpr(*m1 / m2);
}

Mat* cv_Mat_get_ROI(Mat* self, Rect* roi) {
    return new Mat(*self, *roi);
}

MatExpr* cv_Mat_div(Mat* m1, Mat* m2) {
    return new MatExpr(*m1 / *m2);
}

Mat* cv_create_sized_identity(Size* s, int type) {
    return new Mat(Mat::eye(*s, type));
}

Mat* cv_create_Mat_with_value(int rows, int cols, int type, Scalar* s) {
    return new Mat(rows, cols, type, *s);
}

MatExpr* cv_Mat_sub(Mat* m1, Mat* m2) {
    return new MatExpr(*m1 - *m2);
}

Mat* cv_create_Mat_typed(int rows, int cols, int type) {
    return new Mat(rows, cols, type);
}

Mat* cv_create_Mat_with_data(int rows, int cols, int type, void* data ) {
    return new Mat(rows, cols, type, data);
}

Point* cv_Mat_at_Point0(Mat* self, int i, int j) {
  return &self->at<Point>(i, j);
}

Point2d* cv_Mat_at_Point2d0(Mat* self, int i, int j) {
  return &self->at<Point2d>(i, j);
}

Point2f* cv_Mat_at_Point2f0(Mat* self, int i, int j) {
  return &self->at<Point2f>(i, j);
}

Point3d* cv_Mat_at_Point3d0(Mat* self, int i, int j) {
  return &self->at<Point3d>(i, j);
}

Point3f* cv_Mat_at_Point3f0(Mat* self, int i, int j) {
  return &self->at<Point3f>(i, j);
}

Point3i* cv_Mat_at_Point3i0(Mat* self, int i, int j) {
  return &self->at<Point3i>(i, j);
}

Scalar* cv_Mat_at_Scalar0(Mat* self, int i, int j) {
  return &self->at<Scalar>(i, j);
}

int cv_Mat_at_int0(Mat* self, int i, int j) {
  return self->at<int>(i, j);
}

double cv_Mat_at_double0(Mat* self, int i, int j) { 
   return self->at<double>(i,j); 
}

short cv_Mat_at_short0(Mat* self, int i, int j) {
  return self->at<short>(i, j);
}

char cv_Mat_at_char0(Mat* self, int i, int j) {
  return self->at<char>(i, j);
}

uint cv_Mat_at_uint0(Mat* self, int i, int j) {
  return self->at<uint>(i, j);
}

ushort cv_Mat_at_ushort0(Mat* self, int i, int j) {
  return self->at<ushort>(i, j);
}

uchar cv_Mat_at_uchar0(Mat* self, int i, int j) {
  return self->at<uchar>(i, j);
}

float cv_Mat_at_float0(Mat* self, int i, int j) {
  return self->at<float>(i, j);
}

Vec2b* cv_Mat_at_Vec2b0(Mat* self, int i, int j) {
  return &self->at<Vec2b>(i, j);
}

Vec2d* cv_Mat_at_Vec2d0(Mat* self, int i, int j) {
  return &self->at<Vec2d>(i, j);
}

Vec2f* cv_Mat_at_Vec2f0(Mat* self, int i, int j) {
  return &self->at<Vec2f>(i, j);
}

Vec2i* cv_Mat_at_Vec2i0(Mat* self, int i, int j) {
  return &self->at<Vec2i>(i, j);
}

Vec2s* cv_Mat_at_Vec2s0(Mat* self, int i, int j) {
  return &self->at<Vec2s>(i, j);
}

Vec3b* cv_Mat_at_Vec3b0(Mat* self, int i, int j) {
  return &self->at<Vec3b>(i, j);
}

Vec3d* cv_Mat_at_Vec3d0(Mat* self, int i, int j) {
  return &self->at<Vec3d>(i, j);
}

Vec3f* cv_Mat_at_Vec3f0(Mat* self, int i, int j) {
  return &self->at<Vec3f>(i, j);
}

Vec3i* cv_Mat_at_Vec3i0(Mat* self, int i, int j) {
  return &self->at<Vec3i>(i, j);
}

Vec3s* cv_Mat_at_Vec3s0(Mat* self, int i, int j) {
  return &self->at<Vec3s>(i, j);
}

Vec4b* cv_Mat_at_Vec4b0(Mat* self, int i, int j) {
  return &self->at<Vec4b>(i, j);
}

Vec4d* cv_Mat_at_Vec4d0(Mat* self, int i, int j) {
  return &self->at<Vec4d>(i, j);
}

Vec4f* cv_Mat_at_Vec4f0(Mat* self, int i, int j) {
  return &self->at<Vec4f>(i, j);
}

Vec4i* cv_Mat_at_Vec4i0(Mat* self, int i, int j) {
  return &self->at<Vec4i>(i, j);
}

Vec4s* cv_Mat_at_Vec4s0(Mat* self, int i, int j) {
  return &self->at<Vec4s>(i, j);
}

Point* cv_Mat_at_Point1(Mat* self, int i, int j) {
  return &self->at<Point>(i, j);
}

Point2d* cv_Mat_at_Point2d1(Mat* self, int i, int j) {
  return &self->at<Point2d>(i, j);
}

Point2f* cv_Mat_at_Point2f1(Mat* self, int i, int j) {
  return &self->at<Point2f>(i, j);
}

Point3d* cv_Mat_at_Point3d1(Mat* self, int i, int j) {
  return &self->at<Point3d>(i, j);
}

Point3f* cv_Mat_at_Point3f1(Mat* self, int i, int j) {
  return &self->at<Point3f>(i, j);
}

Point3i* cv_Mat_at_Point3i1(Mat* self, int i, int j) {
  return &self->at<Point3i>(i, j);
}

Scalar* cv_Mat_at_Scalar1(Mat* self, int i, int j) {
  return &self->at<Scalar>(i, j);
}

double &cv_Mat_at_double1(Mat* self, int i, int j) { 
   return self->at<double>(i,j); 
}

short &cv_Mat_at_short1(Mat* self, int i, int j) {
  return self->at<short>(i, j);
}

char &cv_Mat_at_char1(Mat* self, int i, int j) {
  return self->at<char>(i, j);
}

uchar &cv_Mat_at_uchar1(Mat* self, int i, int j) {
  return self->at<uchar>(i, j);
}

uint &cv_Mat_at_uint1(Mat* self, int i, int j) {
  return self->at<uint>(i, j);
}

ushort &cv_Mat_at_ushort1(Mat* self, int i, int j) {
  return self->at<ushort>(i, j);
}

float &cv_Mat_at_float1(Mat* self, int i, int j) {
  return self->at<float>(i, j);
}

int &cv_Mat_at_int1(Mat* self, int i, int j) {
  return self->at<int>(i, j);
}

Vec2b* cv_Mat_at_Vec2b1(Mat* self, int i, int j) {
  return &self->at<Vec2b>(i, j);
}

Vec2d* cv_Mat_at_Vec2d1(Mat* self, int i, int j) {
  return &self->at<Vec2d>(i, j);
}

Vec2f* cv_Mat_at_Vec2f1(Mat* self, int i, int j) {
  return &self->at<Vec2f>(i, j);
}

Vec2i* cv_Mat_at_Vec2i1(Mat* self, int i, int j) {
  return &self->at<Vec2i>(i, j);
}

Vec2s* cv_Mat_at_Vec2s1(Mat* self, int i, int j) {
  return &self->at<Vec2s>(i, j);
}

Vec3b* cv_Mat_at_Vec3b1(Mat* self, int i, int j) {
  return &self->at<Vec3b>(i, j);
}

Vec3d* cv_Mat_at_Vec3d1(Mat* self, int i, int j) {
  return &self->at<Vec3d>(i, j);
}

Vec3f* cv_Mat_at_Vec3f1(Mat* self, int i, int j) {
  return &self->at<Vec3f>(i, j);
}

Vec3i* cv_Mat_at_Vec3i1(Mat* self, int i, int j) {
  return &self->at<Vec3i>(i, j);
}

Vec3s* cv_Mat_at_Vec3s1(Mat* self, int i, int j) {
  return &self->at<Vec3s>(i, j);
}

Vec4b* cv_Mat_at_Vec4b1(Mat* self, int i, int j) {
  return &self->at<Vec4b>(i, j);
}

Vec4d* cv_Mat_at_Vec4d1(Mat* self, int i, int j) {
  return &self->at<Vec4d>(i, j);
}

Vec4f* cv_Mat_at_Vec4f1(Mat* self, int i, int j) {
  return &self->at<Vec4f>(i, j);
}

Vec4i* cv_Mat_at_Vec4i1(Mat* self, int i, int j) {
  return &self->at<Vec4i>(i, j);
}

Vec4s* cv_Mat_at_Vec4s1(Mat* self, int i, int j) {
  return &self->at<Vec4s>(i, j);
}

Mat* cv_create_Mat() {
    return new Mat();
}

Mat* cv_Mat_assign(Mat* self, Mat* m) {
    *self = *m;
    return self;
}

Mat* cv_Mat_assignVal(Mat* self, Scalar* s) {
    *self = *s;
    return self;
}

Mat* cv_Mat_getRow(Mat* self, int y) {
    return new Mat(self->row(y));
}

Mat* cv_Mat_getCol(Mat* self, int x) {
    return new Mat(self->col(x));
}
Mat* cv_Mat_getRowRange(Mat* self, int startrow, int endrow) {
    return new Mat(self->rowRange(startrow, endrow));
}

Mat* cv_Mat_getColRange(Mat* self, int startcol, int endrow) {
    return new Mat(self->colRange(startcol, endrow));
}

Mat* cv_Mat_diag(Mat* self) {
    return new Mat(self->diag());
}

Mat* cv_Mat_diag_d(Mat* self, int d) {
    return new Mat(self->diag(d));
}

Mat* cv_create_diagMat(Mat* d) {
    return new Mat(Mat::diag(*d));
}

Mat* cv_Mat_clone(Mat* self) {
    return new Mat(self->clone());
}

void cv_Mat_copyTo(Mat* self, Mat* m) {
    self->copyTo(*m);
}

void cv_Mat_copyTo_masked(Mat* self, Mat* m, Mat* mask) {
    self->copyTo(*m, *mask);
}

void cv_Mat_assignTo(Mat* self, Mat* m) {
    self->assignTo(*m);
}

void cv_Mat_assignTo_t(Mat*self, Mat* m, int t) {
    self->assignTo(*m, t);
}

Mat* cv_Mat_setTo(Mat* self, Scalar* value) {
    Mat* m = new Mat;
    *m = *value;
    return new Mat(self->setTo(*m));
}

Mat* cv_Mat_setTo_masked(Mat* self, Scalar* value, Mat* mask) {
    Mat* m = new Mat;
    *m = *value;
    return new Mat(self->setTo(*m, *mask));
}

Mat* cv_Mat_reshape(Mat* self, int cn) {
   return new Mat(self->reshape(cn)); 
}

Mat* cv_Mat_reshape_rows(Mat* self, int cn, int rows) {
   return new Mat(self->reshape(cn, rows)); 
}

size_t cv_Mat_elemSize(Mat* self) {
    return self->elemSize();
}

size_t cv_Mat_elemSize1(Mat* self) {
    return self->elemSize1();
}

int cv_Mat_type(Mat* self) {
    return self->type();
}

int cv_Mat_depth(Mat* self) {
    return self->depth();
}

size_t cv_Mat_total(Mat* self) {
    return self->total();
}

bool cv_Mat_isContinuous(Mat* self) {
    return self->isContinuous();
}

int cv_Mat_channels(Mat* self) {
    return self->channels();
}

int cv_Mat_rows(Mat* self) {
    return self->rows;
}

int cv_Mat_cols(Mat* self) {
    return self->cols;
}

int cv_Mat_empty(Mat* self) {
    return self->empty();
}

Size* cv_Mat_size(Mat* self) {
    return new Size(self->size());
}

size_t cv_Mat_step1(Mat* self) {
    return self->step1();
}

uchar* cv_Mat_ptr(Mat* self) {
    return self->ptr();
}

uchar* cv_Mat_ptr_index(Mat* self, int i) {
    return self->ptr(i);
}

Mat* cv_create_identity(int rows, int cols, int type) {
    return new Mat(Mat::eye(rows, cols, type));
}

Mat* cv_create_ones(int rows, int cols, int type) {
    return new Mat(Mat::ones(rows, cols, type));
}

Mat* cv_create_zeros(int rows, int cols, int type) {
    return new Mat(Mat::zeros(rows, cols, type));
}

MatExpr* cv_abs(Mat* m) {
	return new MatExpr(abs(*m));
}

MatExpr* cv_Mat_transpose_mat(Mat* self) {
    return new MatExpr(self->t());
}

MatExpr* cv_Mat_inv_mat(Mat* self, int method) {
    return new MatExpr(self->inv(method));
}

MatExpr* cv_Mat_add(Mat* m1, Mat* m2) {
    return new MatExpr(*m1 + *m2);
}

MatExpr* cv_Mat_mult(Mat* m1, Mat* m2) {
    return new MatExpr(*m1 * *m2);
}

Mat* force(MatExpr* expr) {
    Mat* p = new Mat;
    *p = *expr;
    return p;
}

MatExpr* promote(Mat* m) {
    return new MatExpr(*m);
}

MatExpr* cv_Mat_scale(MatExpr* m, double alpha) {
    return new MatExpr(*m * alpha);
}

double cv_Mat_dot(Mat* self, Mat* m) {
    return self->dot(*m);
}

Mat* cv_Mat_cross(Mat* self, Mat* m) {
    return new Mat(self->cross(*m));
}

void cv_Mat_locateROI(Mat* self, Size* s, Point* p) {
    self->locateROI(*s, *p);
}

Mat* cv_Mat_adjustROI(Mat* self, int dtop, int dbottom, int dleft, int dright) {
    return new Mat(self->adjustROI(dtop, dbottom, dleft, dright));
}

}
