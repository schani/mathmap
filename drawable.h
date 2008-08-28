/* -*- c -*- */

/*
 * drawable.h
 *
 * MathMap
 *
 * Copyright (C) 2007-2008 Mark Probst
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

#ifndef __DRAWABLE_H__
#define __DRAWABLE_H__

#include <glib.h>
#ifndef OPENSTEP
#include <libgimp/gimp.h>
#endif
#ifdef MOVIES
#include <quicktime.h>
#endif

#include "color.h"
#include "userval.h"
#include "lispreader/pools.h"

#define INPUT_DRAWABLE_GIMP			1
#define INPUT_DRAWABLE_CMDLINE_IMAGE		2
#define INPUT_DRAWABLE_CMDLINE_MOVIE		3
#define INPUT_DRAWABLE_OPENSTEP			4

#ifdef MATHMAP_CMDLINE
struct _cache_entry_t;
#endif

#define IMAGE_DRAWABLE		1
#define IMAGE_CLOSURE		2
#define IMAGE_FLOATMAP		3
#define IMAGE_RESIZE		4

typedef float* (*filter_func_t) (struct _mathmap_invocation_t*,
				 struct _userval_t*,
				 float, float, float,
				 pools_t*);

struct _input_drawable_t;

#define NUM_FLOATMAP_CHANNELS	4

typedef struct _image_t
{
    int type;
    int pixel_width;
    int pixel_height;
    union
    {
	struct _input_drawable_t *drawable;
	struct {
	    filter_func_t func;
	    userval_t args[];
	} closure;
	struct {
	    float ax;
	    float bx;
	    float ay;
	    float by;
	    float *data;
	} floatmap;
	struct {
	    struct _image_t *original;
	    float x_factor;
	    float y_factor;
	} resize;
    } v;
} image_t;

#define FLOATMAP_VALUE_I(img,i,c)          ((img)->v.floatmap.data[(i)*NUM_FLOATMAP_CHANNELS + (c)])
#define FLOATMAP_VALUE_XY(img,x,y,c)	   FLOATMAP_VALUE_I((img), ((y)*(img)->pixel_width + (x)), (c))

typedef struct _input_drawable_t {
    gboolean used;

    image_t image;

    int kind;

    float scale_x;
    float scale_y;
    float middle_x;
    float middle_y;

    union
    {
#ifdef OPENSTEP
	struct
	{
	    int row_stride;
	    unsigned char *data;
	} openstep;
#else
	struct
	{
	    GimpDrawable *drawable;
	    gint bpp;
	    gint row;
	    gint col;
	    GimpTile *tile;
	    gboolean has_selection;
	    int fast_image_source_width;
	    int fast_image_source_height;
	    color_t *fast_image_source;
	} gimp;
#endif
#ifdef MATHMAP_CMDLINE
	struct
	{
	    int num_frames;
	    struct _cache_entry_t **cache_entries;
	    char *image_filename;
#ifdef MOVIES
	    quicktime_t *movie;
#endif
	} cmdline;
#endif
    } v;
} input_drawable_t;

input_drawable_t* alloc_input_drawable (int kind, int width, int height);

void free_input_drawable (input_drawable_t *drawable);

input_drawable_t* copy_input_drawable (input_drawable_t *drawable);

void for_each_input_drawable (void (*) (input_drawable_t *drawable));

int get_num_input_drawables (void);
input_drawable_t* get_nth_input_drawable (int n);

#ifndef OPENSTEP
input_drawable_t* alloc_gimp_input_drawable (GimpDrawable *drawable);
GimpDrawable* get_gimp_input_drawable (input_drawable_t *drawable);

input_drawable_t* get_default_input_drawable (void);
#endif

#ifdef MATHMAP_CMDLINE
input_drawable_t* alloc_cmdline_image_input_drawable (const char *filename);
#ifdef MOVIES
input_drawable_t* alloc_cmdline_movie_input_drawable (const char *filename);
#endif
#endif

image_t* floatmap_alloc (int width, int height, pools_t *pools);
image_t* floatmap_copy (image_t *floatmap, pools_t *pools);

image_t* make_resize_image (image_t *image, float x_factor, float y_factor, pools_t *pools);

void floatmap_get_channel_column (float *dst, image_t *img, int col, int channel);
void floatmap_get_channel_row (float *dst, image_t *img, int row, int channel);
void floatmap_set_channel_column (image_t *img, int col, int channel, float *src);
void floatmap_set_channel_row (image_t *img, int row, int channel, float *src);

void floatmap_get_column (float *dst, image_t *img, int col);
void floatmap_get_row (float *dst, image_t *img, int row);
void floatmap_set_column (image_t *img, int col, float *src);
void floatmap_set_row (image_t *img, int row, float *src);

void floatmap_write (image_t *img, const char *filename);

#endif
