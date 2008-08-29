/*
 * tuples.h
 *
 * MathMap
 *
 * Copyright (C) 1997-2002 Mark Probst
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

#ifndef __TUPLES_H__
#define __TUPLES_H__

typedef struct
{
    int number;
    int length;
} tuple_info_t;

/* TEMPLATE tuple */
typedef struct
{
    int number;
    int length;
    float data[];
} tuple_t;
/* END */

tuple_info_t make_tuple_info (int number, int length);

tuple_t* make_tuple (int number, int length);
void free_tuple (tuple_t *tuple);
tuple_t* copy_tuple (tuple_t *src);

void tuple_to_color (tuple_t *tuple, float *red, float *green, float *blue, float *alpha);
tuple_t* color_to_tuple (float red, float green, float blue, float alpha);

#endif
