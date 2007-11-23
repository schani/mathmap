/*
 * builtins.c
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

#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>

#include "builtins.h"
#include "tags.h"
#include "overload.h"
#include "mathmap.h"

void
apply_edge_behaviour (mathmap_invocation_t *invocation, int *_x, int *_y, int width, int height)
{
    int x = *_x, y = *_y;

    switch (invocation->edge_behaviour_x)
    {
	case EDGE_BEHAVIOUR_WRAP :
	    if (x < 0)
		x = x % width + width;
	    else if (x >= width)
		x %= width;
	    break;

	case EDGE_BEHAVIOUR_REFLECT :
	    if (x < 0)
		x = -x % width;
	    else if (x >= width)
		x = (width - 1) - (x % width);
	    break;

	case EDGE_BEHAVIOUR_ROTATE :
	    if (x < 0)
	    {
		x = -x % width;
		y = (height - 1) - y;
	    }
	    else if (x >= width)
	    {
		x = (width - 1) - (x % width);
		y = (height - 1) - y;
	    }
	    break;

	case EDGE_BEHAVIOUR_COLOR :
	    break;

	default :
	    assert(0);
    }

    switch (invocation->edge_behaviour_y)
    {
	case EDGE_BEHAVIOUR_WRAP :
	    if (y < 0)
		y = y % height + height;
	    else if (y >= height)
		y %= height;
	    break;

	case EDGE_BEHAVIOUR_REFLECT :
	    if (y < 0)
		y = -y % height;
	    else if (y >= height)
		y = (height - 1) - (y % height);
	    break;

	case EDGE_BEHAVIOUR_ROTATE :
	    if (y < 0)
	    {
		x = (width - 1) - x;
		y = -y % height;
	    }
	    else if (y >= height)
	    {
		x = (width - 1) - x;
		y = (height - 1) - (y % height);
	    }
	    break;

	case EDGE_BEHAVIOUR_COLOR :
	    break;

	default :
	    assert(0);
    }

    *_x = x;
    *_y = y;
}

static color_t
get_pixel (mathmap_invocation_t *invocation, int x, int y, userval_t *userval, int frame)
{ 
    if (userval->v.image.drawable == NULL)
	return MAKE_RGBA_COLOR(255, 255, 255, 255);

    apply_edge_behaviour(invocation, &x, &y, userval->v.image.drawable->width, userval->v.image.drawable->height);

    return mathmap_get_pixel(invocation, userval->v.image.drawable, frame, x, y);
}

static userval_t*
get_drawable_userval (mathmap_invocation_t *invocation, int drawable_index, float *x, float *y)
{
    userval_t *userval;

    if (drawable_index < 0 || drawable_index >= invocation->mathmap->main_filter->num_uservals)
	return 0;

    userval = &invocation->uservals[drawable_index];

    if (userval->type != USERVAL_IMAGE)
	return 0;

    *x = (*x + userval->v.image.middle_x) * userval->v.image.scale_x;
    *y = -((*y - userval->v.image.middle_y) * userval->v.image.scale_y);

    return userval;
}

color_t
get_orig_val_pixel (mathmap_invocation_t *invocation, float x, float y, int drawable_index, int frame)
{
    userval_t *userval = get_drawable_userval(invocation, drawable_index, &x, &y);

    if (userval == 0)
	return COLOR_WHITE;

    if (!invocation->supersampling)
    {
	x += 0.5;
	y += 0.5;
    }

    return get_pixel(invocation, floor(x), floor(y), userval, frame);
}

color_t
get_orig_val_intersample_pixel (mathmap_invocation_t *invocation, float x, float y, int drawable_index, int frame)
{
    int x1,
	x2,
	y1,
	y2;
    float x2fact,
	y2fact,
	x1fact,
	y1fact,
	p1fact,
	p2fact,
	p3fact,
	p4fact;
    color_t pixel1, pixel2, pixel3, pixel4, result;
    float_color_t fpixel1, fpixel2, fpixel3, fpixel4, fresult;
    userval_t *userval = get_drawable_userval(invocation, drawable_index, &x, &y);
    int pixel_inc_x, pixel_inc_y;

    if (userval == 0)
	return COLOR_WHITE;

    drawable_get_pixel_inc(invocation, userval->v.image.drawable, &pixel_inc_x, &pixel_inc_y);

    if (pixel_inc_x > 1)
    {
	x -= pixel_inc_x / 2.0;

	x1 = floor(x / pixel_inc_x) * pixel_inc_x;
	x2 = x1 + pixel_inc_x;

	x2fact = (x - x1) / pixel_inc_x;
    }
    else
    {
	x1 = floor(x);
	x2 = x1 + 1;

	x2fact = x - x1;
    }

    if (pixel_inc_y > 1)
    {
	y -= pixel_inc_y / 2.0;

	y1 = floor(y / pixel_inc_y) * pixel_inc_y;
	y2 = y1 + pixel_inc_y;

	y2fact = (y - y1) / pixel_inc_y;
    }
    else
    {
	y1 = floor(y);
	y2 = y1 + 1;

	y2fact = y - y1;
    }

    x1fact = 1.0 - x2fact;
    y1fact = 1.0 - y2fact;

    p1fact = x1fact * y1fact;
    p2fact = x1fact * y2fact;
    p3fact = x2fact * y1fact;
    p4fact = x2fact * y2fact;

    pixel1 = get_pixel(invocation, x1, y1, userval, frame);
    pixel2 = get_pixel(invocation, x1, y2, userval, frame);
    pixel3 = get_pixel(invocation, x2, y1, userval, frame);
    pixel4 = get_pixel(invocation, x2, y2, userval, frame);

    fpixel1 = COLOR_MUL_FLOAT(pixel1, p1fact);
    fpixel2 = COLOR_MUL_FLOAT(pixel2, p2fact);
    fpixel3 = COLOR_MUL_FLOAT(pixel3, p3fact);
    fpixel4 = COLOR_MUL_FLOAT(pixel4, p4fact);

    fresult = FLOAT_COLOR_ADD(fpixel1, fpixel2);
    fresult = FLOAT_COLOR_ADD(fresult, fpixel3);
    fresult = FLOAT_COLOR_ADD(fresult, fpixel4);

    result = FLOAT_COLOR_TO_COLOR(fresult);

    return result;
}

void
solve_linear_equations (int dim, float *_a, float *_b)
{
    gsl_matrix *m;
    gsl_vector *b, *r;
    int i, j;

    m = gsl_matrix_alloc(dim, dim);
    b = gsl_vector_alloc(dim);
    r = gsl_vector_alloc(dim);

    for (i = 0; i < dim; ++i)
	for (j = 0; j < dim; ++j)
	    gsl_matrix_set(m, i, j, _a[i * dim + j]);

    for (i = 0; i < dim; ++i)
	gsl_vector_set(b, i, _b[i]);

    gsl_linalg_HH_solve(m, b, r);

    for (i = 0; i < dim; ++i)
	_b[i] = gsl_vector_get(r, i);

    gsl_vector_free(r);
    gsl_vector_free(b);
    gsl_matrix_free(m);
}

void
convert_rgb_to_hsv (float *rgb, float *hsv)
{
    float max, min;
    int i;

    for (i = 0; i < 3; ++i)
	if (rgb[i] < 0.0)
	    rgb[i] = 0.0;
	else if (rgb[i] > 1.0)
	    rgb[i] = 1.0;

    max = MAX(rgb[0], MAX(rgb[1], rgb[2]));
    min = MIN(rgb[0], MIN(rgb[1], rgb[2]));
    hsv[2] = max;

    if (max != 0)
	hsv[1] = (max - min) / max;
    else
	hsv[1] = 0.0;

    if (hsv[1] == 0.0)
	hsv[0] = 0.0;		/* actually undefined */
    else
    {
	float delta = max - min;

	if (rgb[0] == max)
	    hsv[0] = (rgb[1] - rgb[2]) / delta;
	else if (rgb[1] == max)
	    hsv[0] = 2 + (rgb[2] - rgb[0]) / delta;
	else
	    hsv[0] = 4 + (rgb[0] - rgb[1]) / delta;

	hsv[0] /= 6.0;

	if (hsv[0] < 0.0)
	    hsv[0] += 1.0;
    }
}

void
convert_hsv_to_rgb (float *hsv, float *rgb)
{
    int i;

    for (i = 0; i < 3; ++i)
	if (hsv[i] < 0.0)
	    hsv[i] = 0.0;
	else if (hsv[i] > 1.0)
	    hsv[i] = 1.0;

    if (hsv[1] == 0.0)
	rgb[0] = rgb[1] = rgb[2] = hsv[2];
    else
    {
	float h = hsv[0];
	float f, p, q, t;
	int i;

	if (h >= 1.0)
	    h = 0.0;
	h *= 6.0;

	i = floor(h);
	f = h - i;
	p = hsv[2] * (1 - hsv[1]);
	q = hsv[2] * (1 - (hsv[1] * f));
	t = hsv[2] * (1 - (hsv[1] * (1 - f)));

	switch (i)
	{
	    case 0 :
		rgb[0] = hsv[2]; rgb[1] = t; rgb[2] = p;
		break;

	    case 1 :
		rgb[0] = q; rgb[1] = hsv[2]; rgb[2] = p;
		break;

	    case 2 :
		rgb[0] = p; rgb[1] = hsv[2]; rgb[2] = t;
		break;

	    case 3 :
		rgb[0] = p; rgb[1] = q; rgb[2] = hsv[2];
		break;

	    case 4 :
		rgb[0] = t; rgb[1] = p; rgb[2] = hsv[2];
		break;

	    case 5 :
		rgb[0] = hsv[2]; rgb[1] = p; rgb[2] = q;
		break;

	    default :
		assert(0);
	}
    }
}
