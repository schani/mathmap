#ifndef __TUPLES_H__
#define __TUPLES_H__

#define MAX_TUPLE_LENGTH     9

typedef struct
{
    int number;
    int length;
} tuple_info_t;

typedef struct
{
    float data[MAX_TUPLE_LENGTH];
    int number;
    int length;
} tuple_t;

tuple_info_t make_tuple_info (int number, int length);

void tuple_to_color (tuple_t *tuple, float *red, float *green, float *blue, float *alpha);
tuple_t color_to_tuple (float red, float green, float blue, float alpha);

#endif
