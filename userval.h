/*
 * userval.h
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

#ifndef __USERVAL_H__
#define __USERVAL_H__

#ifdef GIMP
#include <gtk/gtk.h>
#endif

#include "tuples.h"
#include "exprtree.h"

#define USER_CURVE_POINTS       1024

#define USERVAL_SLIDER      1
#define USERVAL_BOOL        2
#define USERVAL_COLOR       3
#define USERVAL_CURVE       4
#define USERVAL_IMAGE       5

typedef struct _userval_t
{
    ident name;
    int type;
#ifdef GIMP
    GtkWidget *widget;
#endif
    int tag;
    union
    {
	struct
	{
	    float min;
	    float max;
	    float value;
	} slider;

	struct
	{
	    float value;
	} bool;

	struct
	{
	    tuple_t value;
	} color;

	struct
	{
	    float values[USER_CURVE_POINTS];
	} curve;

	struct
	{
	    int index;
	} image;
    } v;
    struct _userval_t *next;
} userval_t;

void untag_uservals (void);
void clear_untagged_uservals (void);

userval_t* lookup_userval (const char *name, int type);

userval_t* register_slider (const char *name, float min, float max);
userval_t* register_bool (const char *name);
userval_t* register_color (const char *name);
userval_t* register_curve (const char *name);
userval_t* register_image (const char *name);

#ifdef GIMP
GtkWidget* make_userval_table (void);
#endif

void update_uservals (void);

#endif
