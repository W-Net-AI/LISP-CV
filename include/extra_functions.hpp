#include <opencv2/opencv.hpp>
#include <opencv2/nonfree.hpp>
#include <vector>
using namespace cv;
using namespace std;
using namespace flann;
using namespace cvflann;

extern "C" {
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
void cv_delete_BFMatcher(void* ptr);
void cv_delete_BRISK(void* ptr);
}



