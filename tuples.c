/* -*- c -*- */

#include <assert.h>

#include "tuples.h"
#include "tags.h"

tuple_info_t
make_tuple_info (int number, int length)
{
    tuple_info_t info = { number, length };

    return info;
}

void
tuple_to_color (tuple_t *tuple, float *red, float *green, float *blue, float *alpha)
{
    assert(tuple->length == 4);

    if (tuple->data[0] <= 0.0)
	*red = 0.0;
    else if (tuple->data[0] >= 1.0)
	*red = 1.0;
    else
	*red = tuple->data[0];

    if (tuple->data[1] <= 0.0)
	*green = 0.0;
    else if (tuple->data[1] >= 1.0)
	*green = 1.0;
    else
	*green = tuple->data[1];

    if (tuple->data[2] <= 0.0)
	*blue = 0.0;
    else if (tuple->data[2] >= 1.0)
	*blue = 1.0;
    else
	*blue = tuple->data[2];

    if (tuple->data[3] <= 0.0)
	*alpha = 0.0;
    else if (tuple->data[3] >= 1.0)
	*alpha = 1.0;
    else
	*alpha = tuple->data[3];
}

tuple_t
color_to_tuple (float red, float green, float blue, float alpha)
{
    tuple_t tuple;

    tuple.number = nil_tag_number;
    tuple.length = 4;
    tuple.data[0] = red;
    tuple.data[1] = green;
    tuple.data[2] = blue;
    tuple.data[3] = alpha;

    return tuple;
}
