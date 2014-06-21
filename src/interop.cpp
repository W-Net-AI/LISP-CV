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
ADD_VECTOR_IMPL(Vec2b, v2b);
ADD_VECTOR_IMPL(Vec3b, v3b);
ADD_VECTOR_IMPL(Vec4b, v4b);
ADD_VECTOR_IMPL(Vec2d, v2d);
ADD_VECTOR_IMPL(Vec3d, v3d);
ADD_VECTOR_IMPL(Vec4d, v4d);
ADD_VECTOR_IMPL(Vec6d, v6d);
ADD_VECTOR_IMPL(Vec2f, v2f);
ADD_VECTOR_IMPL(Vec3f, v3f);
ADD_VECTOR_IMPL(Vec4f, v4f);
ADD_VECTOR_IMPL(Vec6f, v6f);
ADD_VECTOR_IMPL(Vec2i, v2i);
ADD_VECTOR_IMPL(Vec3i, v3i);
ADD_VECTOR_IMPL(Vec4i, v4i);
ADD_VECTOR_IMPL(Vec6i, v6i);
ADD_VECTOR_IMPL(Vec8i, v8i);
ADD_VECTOR_IMPL(Vec2s, v2s);
ADD_VECTOR_IMPL(Vec3s, v3s);
ADD_VECTOR_IMPL(Vec4s, v4s);
ADD_VECTOR_IMPL(Vec2w, v2w);
ADD_VECTOR_IMPL(Vec3w, v3w);
ADD_VECTOR_IMPL(Vec4w, v4w);

}
//
