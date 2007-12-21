/* -*- c -*- */

/*
 * drawable.h
 *
 * MathMap
 *
 * Copyright (C) 2007 Mark Probst
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

typedef color_t (*filter_func_t) (struct _mathmap_invocation_t*,
				  struct _userval_t*,
				  float, float,
				  pools_t*);

struct _input_drawable_t;

typedef struct _image_t
{
    int type;
    union
    {
	struct _input_drawable_t *drawable;
	struct {
	    filter_func_t func;
	    userval_t args[];
	} closure;
    } v;
} image_t;

typedef struct _input_drawable_t {
    gboolean used;

    image_t image;

    int kind;

    int width;
    int height;

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

#endif
