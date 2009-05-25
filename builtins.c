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

#include "lispreader/pools.h"

#include "builtins.h"
#include "tags.h"
#include "overload.h"
#include "mathmap.h"
#include "opmacros.h"

static void
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
get_pixel (mathmap_invocation_t *invocation, int x, int y, input_drawable_t *drawable, int frame)
{
    if (drawable == NULL)
	return MAKE_RGBA_COLOR(255, 255, 255, 255);

    apply_edge_behaviour(invocation, &x, &y, drawable->image.pixel_width, drawable->image.pixel_height);

    return mathmap_get_pixel(invocation, drawable, frame, x, y);
}

static input_drawable_t*
get_image_drawable (mathmap_invocation_t *invocation, image_t *image, float *x, float *y)
{
    input_drawable_t *drawable = image->v.drawable;

    g_assert(image->type == IMAGE_DRAWABLE);

    if (drawable == NULL)
	return NULL;

    *x = (*x + drawable->middle_x) * drawable->scale_x;
    *y = -((*y - drawable->middle_y) * drawable->scale_y);

    return drawable;
}

CALLBACK_SYMBOL
color_t
get_orig_val_pixel (mathmap_invocation_t *invocation, float x, float y, image_t *image, int frame)
{
    input_drawable_t *drawable = get_image_drawable(invocation, image, &x, &y);

    if (!invocation->supersampling)
    {
	x += 0.5;
	y += 0.5;
    }

    return get_pixel(invocation, floor(x), floor(y), drawable, frame);
}

CALLBACK_SYMBOL
color_t
get_orig_val_intersample_pixel (mathmap_invocation_t *invocation, float x, float y, image_t *image, int frame)
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
    input_drawable_t *drawable = get_image_drawable(invocation, image, &x, &y);
    int pixel_inc_x, pixel_inc_y;

    drawable_get_pixel_inc(invocation, drawable, &pixel_inc_x, &pixel_inc_y);

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

    pixel1 = get_pixel(invocation, x1, y1, drawable, frame);
    pixel2 = get_pixel(invocation, x1, y2, drawable, frame);
    pixel3 = get_pixel(invocation, x2, y1, drawable, frame);
    pixel4 = get_pixel(invocation, x2, y2, drawable, frame);

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

CALLBACK_SYMBOL
float*
get_floatmap_pixel (mathmap_invocation_t *invocation, image_t *image, float x, float y, float frame)
{
    static float black[] = { 0.0, 0.0, 0.0, 0.0 };

    int ix, iy;

    g_assert(image->type == IMAGE_FLOATMAP);

    ix = (int)lrintf(image->v.floatmap.ax * x + image->v.floatmap.bx);
    iy = (int)lrintf(image->v.floatmap.ay * y + image->v.floatmap.by);

    if (ix < 0 || ix >= image->pixel_width
	|| iy < 0 || iy >= image->pixel_height)
	return black;

    return image->v.floatmap.data + (iy * image->pixel_width + ix) * 4;
}

CALLBACK_SYMBOL
image_t*
render_image (mathmap_invocation_t *invocation, image_t *image, int width, int height, pools_t *pools, int force)
{
    image_t *new_image;

    if (!force && image->type == IMAGE_FLOATMAP)
	return image;

    new_image = floatmap_alloc(width, height, pools);

#ifdef DEBUG_OUTPUT
    g_print("rendering %dx%d\n", width, height);
#endif

    if (image->type == IMAGE_CLOSURE)
    {
	mathmap_frame_t *frame;
	mathmap_slice_t slice;

#ifdef DEBUG_OUTPUT
	g_print("image is closure\n");
#endif

	frame = invocation_new_frame(invocation, image, 0, 0.0);
	frame->frame_render_width = width;
	frame->frame_render_height = height;

	invocation_init_slice(&slice, image, frame, 0, 0, width, height, 0.0, 0.0);

	image->v.closure.funcs->calc_lines(&slice, image, 0, height, new_image->v.floatmap.data, 1);

	invocation_deinit_slice(&slice);

	invocation_free_frame(frame);
    }
    else
    {
	float ax, bx, ay, by;
	color_t (*get_orig_val_pixel_func) (mathmap_invocation_t*, float, float, image_t*, int) = get_orig_val_pixel;
	int x, y;
	float *p;
	pools_t filter_pools;

#ifdef DEBUG_OUTPUT
	g_print("image is not closure: %d\n", image->type);
#endif

	ax = new_image->v.floatmap.ax;
	bx = new_image->v.floatmap.bx;
	ay = new_image->v.floatmap.ay;
	by = new_image->v.floatmap.by;

	init_pools(&filter_pools);
	pools = &filter_pools;

	p = new_image->v.floatmap.data;
	for (y = 0; y < height; ++y)
	{
	    float fy = ((float)y - by) / ay;

	    for (x = 0; x < width; ++x)
	    {
		float fx = ((float)x - bx) / ax;
		float *tuple;

		reset_pools(&filter_pools);
		tuple = ORIG_VAL(fx, fy, image, 0.0);

		memcpy(p, tuple, sizeof(float) * 4);

		p += 4;
	    }
	}

	free_pools(&filter_pools);
    }

    return new_image;
}
