/* -*- c -*- */

/*
 * tuples.c
 *
 * MathMap
 *
 * Copyright (C) 1997-2007 Mark Probst
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "tuples.h"
#include "tags.h"

tuple_info_t
make_tuple_info (int number, int length)
{
    tuple_info_t info = { number, length };

    return info;
}

tuple_t*
make_tuple (int number, int length)
{
    tuple_t *tuple = (tuple_t*)malloc(sizeof(tuple_t) + sizeof(float) * length);

    assert(tuple != 0);

    tuple->number = number;
    tuple->length = length;

    return tuple;
}

void
free_tuple (tuple_t *tuple)
{
    free(tuple);
}

tuple_t*
copy_tuple (tuple_t *src)
{
    tuple_t *dst = make_tuple(src->number, src->length);

    memcpy(dst->data, src->data, sizeof(float) * dst->length);

    return dst;
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

tuple_t*
color_to_tuple (float red, float green, float blue, float alpha)
{
    tuple_t *tuple = make_tuple(nil_tag_number, 4); /* FIXME: shouldn't this be rgba? */

    tuple->data[0] = red;
    tuple->data[1] = green;
    tuple->data[2] = blue;
    tuple->data[3] = alpha;

    return tuple;
}

