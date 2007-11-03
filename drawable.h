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

#define INPUT_DRAWABLE_GIMP			1
#define INPUT_DRAWABLE_CMDLINE_IMAGE		2
#define INPUT_DRAWABLE_CMDLINE_MOVIE		3
#define INPUT_DRAWABLE_OPENSTEP			4

typedef struct {
    gboolean used;

    int kind;

    int width;
    int height;

    union
    {
#ifdef OPENSTEP
	struct
	{
	    int row_stride;
	    void *data;
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
#ifdef CMDLINE
	struct
	{
	    cache_entry_t *cache_entry;
	    char *image_filename;
	} cmdline_image;
#ifdef MOVIES
	struct
	{
	    int num_frames;
	    cache_entry_t **cache_entries;
	    quicktime_t *movie;
	} cmdline_movie;
#endif
#endif
    } v;
} input_drawable_t;

void free_input_drawable (input_drawable_t *drawable);

input_drawable_t* copy_input_drawable (input_drawable_t *drawable);

void for_each_input_drawable (void (*) (input_drawable_t *drawable));

input_drawable_t* get_default_input_drawable (void);

#ifndef OPENSTEP
input_drawable_t* alloc_gimp_input_drawable (GimpDrawable *drawable);
GimpDrawable* get_gimp_input_drawable (input_drawable_t *drawable);
#endif

#endif
