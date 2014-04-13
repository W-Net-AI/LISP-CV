#include <opencv2/opencv.hpp>
#include <opencv2/nonfree.hpp>
#include <vector>
using namespace cv;
using namespace std;
using namespace flann;
using namespace cvflann;
extern "C" {
bool cv_imwrite2(const char* filename, Mat* img, vector_int* params);
void cv_randu2(Mat* dst, Scalar* low, Scalar* high);
void cv_goodFeaturesToTrack2(Mat* image, vector_Point2f* corners, int maxCorners, double qualityLevel, double minDistance, Mat* mask, int blockSize, bool useHarrisDetector, double k);
Point* cv_Mat_at_Point0(Mat* self, int i, int j);
Mat* cv_create_Mat_as_vectort(vector_Point2f* vec, bool copyData);
uchar* cv_Mat_get_Data(Mat* self); 
size_t cv_Mat_get_Step(Mat* self); 
void cv_Mat_convertTo(Mat* self,Mat* m, int rtype, double alpha, double beta);
MatExpr* cv_Mat_div_S(Mat* m1, int m2); 
Mat* cv_Mat_get_ROI(Mat* self, Rect* roi);
MatExpr* cv_Mat_div(Mat* m1, Mat* m2); 
Mat* cv_create_sized_Mat(Size* s, int type); 
Mat* cv_create_sized_identity(Size* s, int type); 
Mat* cv_create_Mat_with_value(int rows, int cols, int type, Scalar* s);
MatExpr* cv_Mat_sub(Mat* m1, Mat* m2); 
Size2f* cv_create_Size2f();
Size2f* cv_create_Size2f2(float width, float height);
Size2f* cv_Size_assignTo(Size2f* self, Size2f* other);
Size2f* cv_Size2f_fromPoint2f(Point2f* p);
float cv_Size2f_area(Size2f* self);
float cv_Size2f_width(Size2f* self);
float cv_Size2f_height(Size2f* self);
Size* cv_RotatedRect_size(RotatedRect* self);
Point* cv_RotatedRect_center(RotatedRect* self);
RotatedRect* cv_create_RotatedRect(Point* center, Size* size, float angle);
FeatureDetector* cv_FeatureDetector_create2(FeatureDetector* self, const char* detectorType);
Mat* cv_imread2(const char* filename, int flags);
void cv_inRangeS(Mat* src, Scalar* lowerb, Scalar* upperb, Mat* dst);
int cv_createTrackbar(String* trackbarname, String* winname, int* value, int count, TrackbarCallback onChange, void* userdata);
void cv_setMouseCallback(String* winname, MouseCallback onMouse, void* userdata);
void cv_delete_Mat(void* ptr);
void cv_delete_MatExpr(void* ptr);
void cv_delete_BFMatcher(void* ptr);
void cv_delete_BRISK(void* ptr);
)

