/* -*- c -*- */

/*
 * template.c
 *
 * MathMap
 *
 * Copyright (C) 2002 Mark Probst
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

/*
 * $$l -> MAX_TUPLE_LENGTH
 * $$c -> HAVE_COMPLEX ? 1 : 0
 * $$g -> GIMP ? 1 : 0
 * $$m -> mathmap code
 */

#include <math.h>
#if $c
#include <complex.h>
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
double __complex__ cgamma (double __complex__ z);
#endif

typedef struct
{
    float data[$l];
    int number;
    int length;
} tuple_t;

typedef struct _userval_t
{
    union
    {
	int int_const;
	float float_const;
	float bool_const;

	struct
	{
	    unsigned char button_value[4];
	    tuple_t value;
	} color;

	struct
	{
	    float *values;
	} curve;

	struct
	{
	    float (*values)[4];
	} gradient;

	struct
	{
	    int index;
	} image;
    } v;

#if $g
    void *widget;
#endif
} userval_t;

typedef struct _mathmap_t
{
    void *userval_infos;
    void *variables;
    void *internals;

    void *exprtree;

    void *initfunc;
    void *module_info;

    void *expression;
    int exprlen;
} mathmap_t;

typedef struct _mathmap_invocation_t
{
    mathmap_t *mathmap;

    userval_t *uservals;
    tuple_t *variables;
    tuple_t *internals;

    int antialiasing;
    int supersampling;

    int output_bpp;

    int edge_behaviour;
    unsigned char edge_color[4];

    int current_frame;
    int origin_x, origin_y;
    int img_width, img_height;
    double middle_x, middle_y;
    double image_R, image_X, image_Y, image_W, image_H;

    double current_x, current_y, current_r, current_a, current_t;

    tuple_t *stack;
    int stackp;

    int exprp;

    void *mathfunc;
} mathmap_invocation_t;

typedef tuple_t* (*mathfunc_t) (void);

extern void get_orig_val_pixel (mathmap_invocation_t *invocation, float x, float y, unsigned char *pixel, int drawable_index, int frame);
extern void get_orig_val_intersample_pixel (mathmap_invocation_t *invocation, float x, float y, unsigned char *pixel, int drawable_index, int frame);
extern void convert_rgb_to_hsv (float *rgb, float *hsv);
extern void convert_hsv_to_rgb (float *hsv, float *rgb);
extern float noise (float, float, float);

static void (*get_orig_val_pixel_func) (mathmap_invocation_t*, float, float, unsigned char*, int, int);

static mathmap_invocation_t *invocation;

static tuple_t*
mathmapfunc (void)
{
    int dummy;

    $m;
}

mathfunc_t
mathmapinit (mathmap_invocation_t *_invocation)
{
    invocation = _invocation;

    if (invocation->antialiasing)
	get_orig_val_pixel_func = get_orig_val_intersample_pixel;
    else
	get_orig_val_pixel_func = get_orig_val_pixel;

    return &mathmapfunc;
}
