#include <string>
#include <vector>
#include <opencv2/c/opencv_generated.hpp>

using namespace std;
typedef vector<char> vector_char;
typedef vector<double> vector_double;
typedef vector<int> vector_int;
typedef vector<float> vector_float;
typedef vector<DMatch> vector_DMatch;
typedef vector<KeyPoint> vector_KeyPoint;
typedef vector<Point2f> vector_Point2f;
typedef vector<Mat> vector_Mat;




extern "C" {
vector_Mat* std_create_vectorm();
vector_Mat* std_carrayTovectorm(Mat* a, size_t len);
Mat* std_vectormToCArray(vector_Mat* s);
size_t std_vectorm_length(vector_Mat* self);

Point2f* std_vectorp2fToCArray(vector_Point2f* s);
vector_Point2f* std_carrayTovectorp2f(Point2f* a, size_t len);
vector_Point2f* std_create_vectorp2f();
size_t std_vectorp2f_length(vector_Point2f* self);

vector_KeyPoint* std_create_vectorkp();
vector_KeyPoint* std_carrayTovectorkp(KeyPoint* a, size_t len);
KeyPoint* std_vectorkpToCArray(vector_KeyPoint* s);
size_t std_vectorkp_length(vector_KeyPoint* self);

vector_DMatch* std_create_vectordm();
vector_DMatch* std_carrayTovectordm(DMatch* a, size_t len);
DMatch* std_vectordmToCArray(vector_DMatch* s);
size_t std_vectordm_length(vector_DMatch* self);

string* std_create_string();
string* std_cstringToString(char* s, size_t len);
const char*  std_stringToCString(string* s);
size_t std_vectorc_length(vector_char* self);

vector_char* *std_create_vectorc();
vector_char* std_carrayTovectorc(char* a, size_t len);
char* std_vectorcToCArray(vector_char* s);
size_t std_vectorc_length(vector_char* self);

vector_int* std_create_vector();
vector_int* std_carrayTovector(int* a, size_t len);
int*   std_vectorToCArray(vector_int* s);
size_t std_vector_length(vector_int* self);

vector_float* std_create_vectorf();
vector_float* std_carrayTovectorf(float* a, size_t len);
float*   std_vectorfToCArray(vector_float* s);
size_t std_vectorf_length(vector_float* self);

vector_double* std_create_vectord();
vector_double* std_carrayTovectord(double* a, size_t len);
double*   std_vectordToCArray(vector_double* s);
size_t std_vectord_length(vector_double* self);
}

