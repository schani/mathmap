/* -*- c -*- */

/*
 * MathMap.h
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

#ifndef __MATHMAP_H__
#define __MATHMAP_H__

#include <glib.h>
#ifdef GIMP
#include <libgimp/gimp.h>
#endif

extern char error_string[];
#ifdef GIMP
extern int auto_preview;
#endif

extern int originX, originY, img_width, img_height;

#ifdef GIMP
void dialog_update_preview (void);
#endif

void mathmap_get_pixel (int drawable_index, int frame, int x, int y, guchar *pixel);
void mathmap_get_fast_pixel(int drawable_index, int x, int y, guchar *pixel);

#ifdef GIMP
int alloc_input_drawable (GimpDrawable *drawable);
void free_input_drawable (int index);
GimpDrawable* get_input_drawable (int index);
#endif

#ifndef MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif
#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif

#ifndef M_PI
#define M_PI     3.14159265358979323846
#endif

#endif
