#include <opencv2/c/opencv_generated.hpp>
using namespace cv;
using namespace std;
using namespace flann;
using namespace cvflann;
extern "C" {
void cv_inRange(Mat* src, Mat* lowerb, Mat* upperb, Mat* dst) {
	cv::inRange(*src, *lowerb, *upperb, *dst);
}
}
