#include <opencv2/c/extra_functions.hpp>


#define ADD_POINT_FUNC_IMPL(t, tn) \
    Point2##t * cv_create_Point2##t ( tn x,  tn y) { \
        return new Point2##t (x, y);\
    } \
    Point3##t * cv_create_Point3##t ( tn x,  tn y,  tn z) { \
        return new Point3##t (x, y, z);\
    } \
    tn cv_Point2##t##_getX( Point2##t * self) { \
        return self->x;\
    }\
    tn cv_Point2##t##_getY( Point2##t * self) { \
        return self->y;\
    }\

    
extern "C" {
    ADD_POINT_FUNC_IMPL(i, int);
    ADD_POINT_FUNC_IMPL(f, float);
    ADD_POINT_FUNC_IMPL(d, double);
}
