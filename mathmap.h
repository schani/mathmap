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

#include <libgimp/gimp.h>

extern char error_string[];
extern int auto_preview;

extern int originX, originY, img_width, img_height;

void dialog_update_preview (void);

void mathmap_get_pixel (int drawable_index, int x, int y, guchar *pixel);
void mathmap_get_fast_pixel(int drawable_index, int x, int y, guchar *pixel);

int alloc_input_drawable (GDrawable *drawable);
void free_input_drawable (int index);
GDrawable* get_input_drawable (int index);

#endif
