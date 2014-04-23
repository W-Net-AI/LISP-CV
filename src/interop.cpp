#include <opencv2/c/interop.hpp>

typedef string String;


extern "C" {


vector_Rect* std_create_vectorr() {
    return new vector<Rect>;
}

vector_Rect* std_carrayTovectorr(Rect* a, size_t len) {
    vector<Rect>* v = new vector<Rect>;
    for(size_t i = 0; i < len; i++)
        v->push_back(a[i]);
     return v;
}

Rect* std_vectorrToCArray(vector_Rect* s) {
    return s->data();
}

size_t std_vectorr_length(vector_Rect* self) {
    return self->size();
}

vector_uchar* std_create_vectoru() {
    return new vector<uchar>;
}

vector_uchar* std_carrayTovectoru(uchar* a, size_t len) {
    vector<uchar>* v = new vector<uchar>;
    for(size_t i = 0; i < len; i++) 
        v->push_back(a[i]);
    return v;
}

uchar* std_vectoruToCArray(vector_uchar* s) {
    return s->data();
}

size_t std_vectoru_length(vector_uchar* self) {
    return self->size();
}

vector_Mat* std_create_vectorm() {
    return new vector<Mat>;
}

vector_Mat* std_carrayTovectorm(Mat* a, size_t len) {
vector<Mat>* v = new vector<Mat>;
for(size_t i = 0; i < len; i++)
v->push_back(a[i]);
return v;
}

Mat* std_vectormToCArray(vector_Mat* s) {
    return s->data();
}

size_t std_vectorm_length(vector_Mat* self) {
    return self->size();
}

vector_Point* std_create_vectorp() {
    return new vector<Point>;
}

vector_Point* std_carrayTovectorp(Point* a, size_t len) {
    vector<Point>* v = new vector<Point>;
    for(size_t i = 0; i < len; i++) 
        v->push_back(a[i]);
    return v;
}

Point* std_vectorpToCArray(vector_Point* s) {
    return s->data();
}

size_t std_vectorp_length(vector_Point* self) {
    return self->size();
}

vector_Point2f* std_create_vectorp2f() {
    return new vector<Point2f>;
}

vector_Point2f* std_carrayTovectorp2f(Point2f* a, size_t len) {
    vector<Point2f>* v = new vector<Point2f>;
    for(size_t i = 0; i < len; i++) 
        v->push_back(a[i]);
    return v;
}

Point2f* std_vectorp2fToCArray(vector_Point2f* s) {
    return s->data();
}

size_t std_vectorp2f_length(vector_Point2f* self) {
    return self->size();
}

vector_char* std_create_vectorc() {
    return new vector<char>;
}

vector_char* std_carrayTovectorc(char* a, size_t len) {
    vector<char>* v = new vector<char>;
    for(size_t i = 0; i < len; i++) 
        v->push_back(a[i]);
    return v;
}

char* std_vectorcToCArray(vector_char* s) {
    return s->data();
}

size_t std_vectorc_length(vector_char* self) {
    return self->size();
}

vector_KeyPoint* std_create_vectorkp() {
    return new vector<KeyPoint>;
}

vector_KeyPoint* std_carrayTovectorkp(KeyPoint* a, size_t len) {
    vector<KeyPoint>* v = new vector<KeyPoint>;
    for(size_t i = 0; i < len; i++) 
        v->push_back(a[i]);
    return v;
}

KeyPoint* std_vectorkpToCArray(vector_KeyPoint* s) {
    return s->data();
}

size_t std_vectorkp_length(vector_KeyPoint* self) {
    return self->size();
}

vector_DMatch* std_create_vectordm() {
    return new vector<DMatch>;
}

vector_DMatch* std_carrayTovectordm(DMatch* a, size_t len) {
    vector<DMatch>* v = new vector<DMatch>;
    for(size_t i = 0; i < len; i++) 
        v->push_back(a[i]);
    return v;
}

DMatch* std_vectordmToCArray(vector_DMatch* s) {
    return s->data();
}
size_t std_vectordm_length(vector_DMatch* self) {
    return self->size();
}

string* std_create_string() {
    return new string;
}

string* std_cstringToString(char* s, size_t len) {
    return new string(s, len);
}

const char*   std_stringToCString(string* s) {
    return s->c_str();
}

vector_int* std_create_vector() {
    return new vector<int>;
}

vector_int* std_carrayTovector(int* a, size_t len) {
    vector<int>* v = new vector<int>;
    for(size_t i = 0; i < len; i++) 
        v->push_back(a[i]);
    return v;
}

int* std_vectorToCArray(vector_int* s) {
    return s->data();
}

size_t std_vector_length(vector_int* self) {
    return self->size();
}

vector_float* std_create_vectorf() {
    return new vector<float>;
}

vector_float* std_carrayTovectorf(float* a, size_t len) {
    vector<float>* v = new vector<float>;
    for(size_t i = 0; i < len; i++) 
        v->push_back(a[i]);
    return v;
}

float* std_vectorfToCArray(vector_float* s) {
    return s->data();
}

size_t std_vectorf_length(vector_float* self) {
    return self->size();
}

vector_double* std_create_vectord() {
    return new vector<double>;
}

vector_double* std_carrayTovectord(double* a, size_t len) {
    vector<double>* v = new vector<double>;
    for(size_t i = 0; i < len; i++) 
        v->push_back(a[i]);
    return v;
}

double* std_vectordToCArray(vector_double* s) {
    return s->data();
}

size_t std_vectord_length(vector_double* self) {
    return self->size();
}

}
