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
#include "mmpools.h"

#define INPUT_DRAWABLE_GIMP			1
#define INPUT_DRAWABLE_CMDLINE_IMAGE		2
#define INPUT_DRAWABLE_CMDLINE_MOVIE		3
#define INPUT_DRAWABLE_OPENSTEP			4

struct _cache_entry_t;

/* TEMPLATE image_types */
#define IMAGE_DRAWABLE		1
#define IMAGE_CLOSURE		2
#define IMAGE_FLOATMAP		3
#define IMAGE_RESIZE		4
/* END */

struct _mathmap_frame_t;
struct _mathmap_slice_t;

/* TEMPLATE filter_funcs */
typedef void (*init_frame_func_t) (struct _mathmap_frame_t*, struct _image_t*);
typedef void (*init_slice_func_t) (struct _mathmap_slice_t*, struct _image_t*);
typedef void (*calc_lines_func_t) (struct _mathmap_slice_t*, struct _image_t*, int, int, void*, int);

typedef float* (*filter_func_t) (struct _mathmap_invocation_t*,
				 struct _image_t*,
				 float, float, float,
				 mathmap_pools_t*);

/* FIXME: just for LLVM - remove eventually */
typedef void* (*llvm_init_frame_func_t) (struct _mathmap_invocation_t*,
					 struct _image_t*,
					 float,
					 mathmap_pools_t*);
typedef float* (*llvm_filter_func_t) (struct _mathmap_slice_t*,
				      struct _image_t*,
				      void*, void*,
				      float, float, float,
				      mathmap_pools_t*);
typedef void* (*init_x_or_y_func_t) (struct _mathmap_slice_t*,
				     struct _image_t*,
				     float, float);
/* END */

struct _input_drawable_t;

/* TEMPLATE num_floatmap_channels */
#define NUM_FLOATMAP_CHANNELS	4
/* END */

struct _mathfuncs_t;

/* TEMPLATE image */
typedef struct _image_t
{
    int type;
    int id;			/* globally unique */
    int pixel_width;
    int pixel_height;
    union
    {
	struct _input_drawable_t *drawable;
	struct {
	    /* for rendering */
	    struct _mathfuncs_t *funcs;
	    /* for getting single pixels - never called for the root closure */
	    filter_func_t func;
	    mathmap_pools_t *pools;
	    void *xy_vars;
	    int num_args;
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
/* END */

#define FLOATMAP_VALUE_I(img,i,c)          ((img)->v.floatmap.data[(i)*NUM_FLOATMAP_CHANNELS + (c)])
#define FLOATMAP_VALUE_XY(img,x,y,c)	   FLOATMAP_VALUE_I((img), ((y)*(img)->pixel_width + (x)), (c))

typedef struct _input_drawable_t {
    gboolean used;

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
	    gboolean has_selection; /* only used for copying the drawable */
	    gint x0, y0;	    /* is honored whatever the value of has_selection */
	    gint bpp;
	    gint row;
	    gint col;
	    GimpTile *tile;
	    int fast_image_source_width;
	    int fast_image_source_height;
	    color_t *fast_image_source;
	} gimp;
#endif
	struct
	{
	    int num_frames;
	    struct _cache_entry_t **cache_entries;
	    char *image_filename;
#ifdef MOVIES
	    quicktime_t *movie;
#endif
	} cmdline;
    } v;

    image_t image;
} input_drawable_t;

/* TEMPLATE image_new_id */
int image_new_id (void);
/* END */

input_drawable_t* alloc_input_drawable (int kind, int width, int height);

void free_input_drawable (input_drawable_t *drawable);

input_drawable_t* copy_input_drawable (input_drawable_t *drawable);

void for_each_input_drawable (void (*) (input_drawable_t *drawable));

int get_num_input_drawables (void);
input_drawable_t* get_nth_input_drawable (int n);

#ifndef OPENSTEP
input_drawable_t* alloc_gimp_input_drawable (GimpDrawable *drawable, gboolean honor_selection);
GimpDrawable* get_gimp_input_drawable (input_drawable_t *drawable);

input_drawable_t* get_default_input_drawable (void);
#endif

input_drawable_t* alloc_cmdline_image_input_drawable (const char *filename);
#ifdef MOVIES
input_drawable_t* alloc_cmdline_movie_input_drawable (const char *filename);
#endif

image_t* floatmap_alloc (int width, int height, mathmap_pools_t *pools);
image_t* floatmap_copy (image_t *floatmap, mathmap_pools_t *pools);

/* TEMPLATE make_resize_image */
image_t* make_resize_image (image_t *image, float x_factor, float y_factor, mathmap_pools_t *pools);
/* END */

void floatmap_get_channel_column (float *dst, image_t *img, int col, int channel);
void floatmap_get_channel_row (float *dst, image_t *img, int row, int channel);
void floatmap_set_channel_column (image_t *img, int col, int channel, float *src);
void floatmap_set_channel_row (image_t *img, int row, int channel, float *src);

void floatmap_get_column (float *dst, image_t *img, int col);
void floatmap_get_row (float *dst, image_t *img, int row);
void floatmap_set_column (image_t *img, int col, float *src);
void floatmap_set_row (image_t *img, int row, float *src);

void floatmap_write (image_t *img, const char *filename);

/* FIXME: remove filter func */
image_t* closure_image_alloc (struct _mathfuncs_t *mathfuncs, filter_func_t filter_func,
			      int num_uservals, userval_t *uservals,
			      int pixel_width, int pixel_height);
void closure_image_free (image_t *closure);

#endif
