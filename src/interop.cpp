#include <opencv2/c/interop.hpp>

typedef string String;

extern "C" {

vector_char* std_create_vectorc() {
    return new vector<char>;
}

vector_KeyPoint* std_create_vectork() {
    return new vector<KeyPoint>;
}

vector_DMatch* std_create_vectordm() {
    return new vector<DMatch>;
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
