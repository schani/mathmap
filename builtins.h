/* -*- c -*- */

/*
 * builtins.h
 *
 * MathMap
 *
 * Copyright (C) 1997-2000 Mark Probst
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

#ifndef __BUILTINS_H__
#define __BUILTINS_H__

#include <stdio.h>

#include "tuples.h"
#include "postfix.h"

#define MAX_BUILTIN_LENGTH     63

typedef void (*builtin_function_t) (postfix_arg*);
typedef void (*generator_function_t) (FILE*, int*, int*, int);

typedef struct _builtin
{
    char name[MAX_BUILTIN_LENGTH + 1];
    builtin_function_t function;
    generator_function_t generator;
    int numParams;
    tuple_info_t tuple_info;
    struct _builtin *next;
} builtin;

double color_to_double (unsigned int red, unsigned int green,
			unsigned int blue, unsigned int alpha);
void double_to_color (double val, unsigned int *red, unsigned int *green,
		      unsigned int *blue, unsigned int *alpha);

void convert_rgb_to_hsv (float *rgb, float *hsv);
void convert_hsv_to_rgb (float *hsv, float *rgb);

builtin_function_t builtin_with_name (const char *name);

void init_builtins (void);

#endif
