/*
 * blender_template.c
 *
 * MathMap
 *
 * Copyright (C) 2004 Mark Probst
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

#include <stdlib.h>
#include <math.h>
#include <complex.h>

#include "plugin.h"

#define NUM_INPUT_DRAWABLES        $num_input_drawables

#define DRAWABLE0                  $drawable0
#define DRAWABLE1                  $drawable1
#define DRAWABLE2                  $drawable2

#include "$opmacros_h"

#define USES_T                     $uses_t

#ifndef MIN
#define MIN(a,b)         (((a)<(b))?(a):(b))
#endif
#ifndef MAX
#define MAX(a,b)         (((a)<(b))?(b):(a))
#endif

typedef unsigned int color_t;

#define MAKE_RGBA_COLOR(r,g,b,a)            ((((color_t)(r))<<24)|(((color_t)(g))<<16)|(((color_t)(b))<<8)|((color_t)(a)))
#define RED(c)                              ((c)>>24)
#define GREEN(c)                            (((c)>>16)&0xff)
#define BLUE(c)                             (((c)>>8)&0xff)
#define ALPHA(c)                            ((c)&0xff)

typedef struct
{
    $xy_decls
} xy_const_vars_t;

typedef struct
{
    $y_decls
} y_const_vars_t;

inline static color_t
get_orig_val_pixel (float x, float y, ImBuf *img, int frame)
{
    int ix = (int)x, iy = (int)y;
    unsigned char *p;

    /* FIXME: edge behaviour */
    if (ix < 0 || ix >= img->x
	|| iy < 0 || iy >= img->y)
	return MAKE_COLOR(0,0,0,1);

    p = (unsigned char*)img->rect + (iy * img->x + ix) * 4;

    return MAKE_RGBA_COLOR(p[0],p[1],p[2],p[3]);
}

inline static color_t
get_orig_val_intersample_pixel (float x, float y, ImBuf *img, int frame)
{
    return get_orig_val_pixel(x, y, img, frame);
}

/* FIXME: all gsl functions, noise, cgamma, ... */

char name[] = "MathMap generated Plug-In";

VarStruct varstr[] =
{
    LABEL, "Input: $num_input_drawables strip(s)", 0.0, 0.0, 0.0, "",
#if USES_T
    NUMSLI|FLO, "frames per t: ", 25.0, 0.0, 1000.0, "The number of frames it takes t to go from 0 to 1",
#endif
    $var_structs
};

typedef struct Cast {
    int dummy;
#if USES_T
    float frames_per_t;
#endif
    $userval_decls
} Cast;

float cfra;

int
plugin_seq_getversion (void) 
{
    return B_PLUGIN_VERSION;
}

void
plugin_but_changed (int but) 
{
}

void
plugin_init (void)
{
}

void plugin_seq_doit (Cast *cast, float facf0, float facf1, int sx, int sy, ImBuf *ibuf1, ImBuf *ibuf2, ImBuf *out, ImBuf *use);

void
plugin_getinfo (PluginInfo *info)
{
    info->name = name;
    info->nvars = sizeof(varstr) / sizeof(VarStruct);
    info->cfra = &cfra;

    info->varstr = varstr;

    info->init = &plugin_init;
    info->seq_doit = (SeqDoit)&plugin_seq_doit;
    info->callback = &plugin_but_changed;
}

void
plugin_seq_doit (Cast *cast, float facf0, float facf1, int sx, int sy, ImBuf *ibuf0, ImBuf *ibuf1, ImBuf *out, ImBuf *ibuf2)
{
    int row, col;
    unsigned char *p;
    int size_x = ibuf1->x, size_y = ibuf1->y;

    float X = size_x / 2.0, Y = size_y / 2.0;
    float W = size_x, H = size_y;
    float R = hypot(X, Y);

#if USES_T
    float t = (cast->frames_per_t > 0.0) ? (cfra / cast->frames_per_t) : 0.0;
#endif

    xy_const_vars_t _xy_vars, *xy_vars = &_xy_vars;
    y_const_vars_t *y_vars_array;

    if (ibuf0 == 0 || out == 0)
	return;

#if NUM_INPUT_DRAWABLES >= 2
    if (ibuf1 == 0)
	return;
#endif
#if NUM_INPUT_DRAWABLES >= 3
    if (ibuf2 == 0)
	return;
#endif

    {
	$xy_code
    }

    y_vars_array = (y_const_vars_t*)malloc(sizeof(y_const_vars_t) * size_x);

    for (col = 0; col < size_x; ++col)
    {
	y_const_vars_t *y_vars = &y_vars_array[col];
	float x = col - X;

	{
	    $y_code
	}

    }

    p = (unsigned char*)out->rect;
    for (row = 0; row < size_y; ++row)
    {
	float y = Y - row;

	$x_decls

	$x_code

	for (col = 0; col < size_x; ++col)
	{
	    y_const_vars_t *y_vars = &y_vars_array[col];
	    float x = col - X;

#if $uses_ra
	    float r, a;

	    r = hypot(x, y);
	    if (r == 0.0)
		a = 0.0;
	    else
		a = acos(x / r);

	    if (y < 0)
		a = 2 * M_PI - a;
#endif

	    {
		$m
	    }

	    p += 4;
	}
    }

    free(y_vars_array);
}
