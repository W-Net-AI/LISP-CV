#include "/home/w/quicklisp/dists/quicklisp/software/lisp-cv-master/include/interop.hpp"

typedef string stdstring;

extern "C" {
stdstring* create_std_string() {
    return new string;
}

stdstring* cstring_to_std_string(char* s, size_t len) {
    return new string(s, len);
}

const char* std_string_to_cstring(stdstring* s) {
    return s->c_str();
}


void cv_delete_std_string(string* self) {
     delete self;
}

ADD_VECTOR_IMPL(char, c);
ADD_VECTOR_IMPL(double, d);
ADD_VECTOR_IMPL(float, f);
ADD_VECTOR_IMPL(int, i);
ADD_VECTOR_IMPL(uchar, u);
ADD_VECTOR_IMPL(DMatch, dm);
ADD_VECTOR_IMPL(KeyPoint, kp);
ADD_VECTOR_IMPL(Mat, m);
ADD_VECTOR_IMPL(Point, p);
ADD_VECTOR_IMPL(Point2f, p2f);
ADD_VECTOR_IMPL(Rect, r);
ADD_VECTOR_IMPL(Vec4i, v4i);

}