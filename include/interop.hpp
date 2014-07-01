#include <string>
#include <vector>
#include <opencv2/opencv.h>

using namespace std;


#define ADD_VECTOR_HEADERS(t, tn) \
typedef vector< t > vector_##t; \
vector_##t * create_std_vector##tn(); \
vector_##t * carray_to_std_vector##tn( t * a, size_t len ); \
t * std_vector##tn##_to_carray( vector_##t * v ); \
size_t std_vector##tn##_length( vector_##t * v); \
void destroy_std_vector##tn( vector_##t * v); \
void delete_std_vector##tn( vector_##t * v);

#define ADD_VECTOR_IMPL(t, tn) \
vector_##t * create_std_vector##tn() { \
return new vector_##t;\
}\
\
vector_##t * carray_to_std_vector##tn( t * a, size_t len ) {\
vector_##t * v = new vector_##t;\
for(size_t i = 0; i < len; i++) \
v->push_back(a[i]);\
return v;\
}\
\
t * std_vector##tn##_to_carray( vector_##t * v ) {\
return v->data();\
}\
\
size_t std_vector##tn##_length( vector_##t * v) { \
return v->size();\
} \
void destroy_std_vector##tn( vector_##t * v) { \
v->~vector_##t();\
}\
void delete_std_vector##tn( vector_##t * v) { \
delete v;\
}\

extern "C" {
string* create_std_string();
string* cstring_to_std_string(char* s, size_t len);
const char* std_string_to_cstring(string* s);
void cv_delete_std_string(string* s);
ADD_VECTOR_HEADERS(char, c);
ADD_VECTOR_HEADERS(double, d);
ADD_VECTOR_HEADERS(float, f);
ADD_VECTOR_HEADERS(int, i);
ADD_VECTOR_HEADERS(uchar, u);
ADD_VECTOR_HEADERS(DMatch, dm);
ADD_VECTOR_HEADERS(KeyPoint, kp);
ADD_VECTOR_HEADERS(Mat, m);
ADD_VECTOR_HEADERS(Point, p);
ADD_VECTOR_HEADERS(Point2f, p2f);
ADD_VECTOR_HEADERS(Rect, r);
ADD_VECTOR_HEADERS(Vec2b, v2b);
ADD_VECTOR_HEADERS(Vec3b, v3b);
ADD_VECTOR_HEADERS(Vec4b, v4b);
ADD_VECTOR_HEADERS(Vec2d, v2d);
ADD_VECTOR_HEADERS(Vec3d, v3d);
ADD_VECTOR_HEADERS(Vec4d, v4d);
ADD_VECTOR_HEADERS(Vec6d, v6d);
ADD_VECTOR_HEADERS(Vec2f, v2f);
ADD_VECTOR_HEADERS(Vec3f, v3f);
ADD_VECTOR_HEADERS(Vec4f, v4f);
ADD_VECTOR_HEADERS(Vec6f, v6f);
ADD_VECTOR_HEADERS(Vec2i, v2i);
ADD_VECTOR_HEADERS(Vec3i, v3i);
ADD_VECTOR_HEADERS(Vec4i, v4i);
ADD_VECTOR_HEADERS(Vec6i, v6i);
ADD_VECTOR_HEADERS(Vec8i, v8i);
ADD_VECTOR_HEADERS(Vec2s, v2s);
ADD_VECTOR_HEADERS(Vec3s, v3s);
ADD_VECTOR_HEADERS(Vec4s, v4s);
ADD_VECTOR_HEADERS(Vec2w, v2w);
ADD_VECTOR_HEADERS(Vec3w, v3w);
ADD_VECTOR_HEADERS(Vec4w, v4w);


}
