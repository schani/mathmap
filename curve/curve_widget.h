/* -*- c -*- */

/*
 * curve_widget.h
 *
 * MathMap
 *
 * Copyright (C) 2010 Genadz Batsyan
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

#ifndef __CURVE_WIDGET_H__
#define __CURVE_WIDGET_H__

#include <glib.h>
#include <gtk/gtk.h>
#include "gegl-curve.h"

#define CURVE_WIDGET_LINE_WIDTH 1.0
#define CURVE_WIDGET_POINT_RADIUS 3.0
#define CURVE_WIDGET_MAX_POINT_GRAB_DISTANCE 5.0

#define CURVE_GRID_RESOLUTION_X 4
#define CURVE_GRID_RESOLUTION_Y 4

#define CURVE_WIDGET_BG_COLOR 1.0, 1.0, 1.0
#define CURVE_WIDGET_CURVE_COLOR 1.0, 0.0, 0.0
#define CURVE_WIDGET_POINT_COLOR 0.0, 0.0, 0.0
#define CURVE_WIDGET_BORDER_COLOR 0.0, 0.0, 0.0
#define CURVE_WIDGET_GRID_COLOR 0.7, 0.7, 0.7


typedef void (*curve_widget_curve_changed_callback_t) (GtkWidget *widget, void *data);

GtkWidget *curve_widget_new(curve_widget_curve_changed_callback_t curve_changed_callback, void *callback_data);
void curve_widget_set_curve(GtkWidget *widget, GeglCurve *curve);
GeglCurve *curve_widget_get_curve(GtkWidget *widget);


#endif // __CURVE_WIDGET_H__
