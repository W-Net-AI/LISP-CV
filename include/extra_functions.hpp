#include <opencv2/opencv.hpp>
#include <opencv2/nonfree.hpp>
#include <vector>
using namespace cv;
using namespace std;
using namespace flann;
using namespace cvflann;

extern "C" {
CvANN_MLP_TrainParams* cv_create_CvANN_MLP_TrainParams();
CvANN_MLP_TrainParams* cv_create_CvANN_MLP_TrainParams4(TermCriteria* term_crit, int train_method, double param1, double param2);
CvSVMParams* cv_create_CvSVMParams(); 
//CvSVMParams* cv_create_CvSVMParams10(int svm_type, int kernel_type, double degree, double gamma, double coef0, double Cvalue, double nu, double p, Mat* class_weights, TermCriteria* term_crit);
void cv_displayOverlay(String* winname, String* text, int delayms);
MatExpr* cv_abs(Mat* m);
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
void cv_delete_CascadeClassifier(CascadeClassifier* self);
void cv_delete_DMatch(DMatch* ptr);
void cv_delete_Feature2D(Feature2D* self);
void cv_delete_KeyPoint(KeyPoint* self);
void cv_delete_CvNormalBayesClassifier(CvNormalBayesClassifier* self);
void cv_delete_Point(Point* self);
void cv_delete_Point2d(Point2d* self);
void cv_delete_Point2f(Point2f* self);
void cv_delete_Point3d(Point3d* self);
void cv_delete_Point3f(Point3f* self);
void cv_delete_Point3i(Point3i* self);
void cv_delete_Rect(Rect* self);
void cv_delete_RNG(RNG* self);
void cv_delete_RotatedRect(RotatedRect* self);
void cv_delete_Scalar(Scalar* self);
void cv_delete_Size(Size* self);
void cv_delete_Size2f(Size2f* self);
void cv_delete_TermCriteria(TermCriteria* self);
void cv_delete_VideoCapture(VideoCapture* self);
void cv_delete_VideoWriter(VideoWriter* self);

}



