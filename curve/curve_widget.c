/* -*- c -*- */

/*
 * curve_widget.c
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

#include <malloc.h>
#include <math.h>
#include <assert.h>
#include "curve_widget.h"

typedef struct
{
	GtkWidget *drawing_area;
	int width;
	int height;
	double *xs;
	double *ys;
	int num_samples;
	GeglCurve *curve;

	// value ranges
	double min_x;
	double max_x;
	double min_y;
	double max_y;

	int last_mouse_x;
	int last_mouse_y;

	int drag_index;
	GdkCursor *cursor_move;
	GdkCursor *cursor_add;
	curve_widget_curve_changed_callback_t curve_changed_callback;
	void *callback_data;
} widget_data_t;


typedef enum {
	CR_NONE = 0,
	CR_ENTER_MARGIN,
	CR_LEAVE_MARGIN
} crossing_type_t;


static widget_data_t *get_widget_data(GtkWidget *widget)
{
	widget_data_t *data = g_object_get_data(G_OBJECT(widget), "curve-data");

	g_assert(data != NULL);
	return data;
}


static gboolean expose_event(GtkWidget *widget, GdkEventExpose *event) {

	cairo_t *cr;

	// cairo coordinate ranges
	double max_px;
	double max_py;

	double *xs;
	double *ys;

	double scale_x;
	double scale_y;

	double grid_cell_size;

	int i;

	widget_data_t *data = get_widget_data(widget);

	assert(data->curve);

	xs = data->xs;
	ys = data->ys;

	max_px = (double)data->width;
	max_py = (double)data->height;

	cr = gdk_cairo_create(GTK_WIDGET(data->drawing_area)->window);

	scale_x = 1.0;
	scale_y = -1.0;

	cairo_translate(cr, 0.0, (double)data->height);

	cairo_scale(cr, scale_x, scale_y);

	cairo_set_line_width(cr, CURVE_WIDGET_LINE_WIDTH);

	cairo_set_source_rgb(cr, CURVE_WIDGET_BG_COLOR);
	cairo_paint(cr);

	// draw grid
	cairo_set_source_rgb(cr, CURVE_WIDGET_GRID_COLOR);

	grid_cell_size = max_px / (double)CURVE_GRID_RESOLUTION_X;
	for (i = 1; i < CURVE_GRID_RESOLUTION_X; i++) {
		double x = i * grid_cell_size;
		cairo_move_to(cr, x, 0.0);
		cairo_line_to(cr, x, max_py);
		cairo_stroke(cr);
	}
	grid_cell_size = max_py / (double)CURVE_GRID_RESOLUTION_Y;
	for (i = 1; i < CURVE_GRID_RESOLUTION_Y; i++) {
		double y = i * grid_cell_size;
		cairo_move_to(cr, 0.0, y);
		cairo_line_to(cr, max_px, y);
		cairo_stroke(cr);
	}

	// draw border
	cairo_set_source_rgb(cr, CURVE_WIDGET_BORDER_COLOR);
	cairo_move_to(cr, 0.0, 0.0);
	cairo_line_to(cr, max_px, 0.0);
	cairo_line_to(cr, max_px, max_py);
	cairo_line_to(cr, 0.0, max_py);
	cairo_line_to(cr, 0.0, 0.0);
	cairo_stroke(cr);


	cairo_set_source_rgb(cr, CURVE_WIDGET_CURVE_COLOR);
	cairo_move_to(cr, xs[0] * max_px, ys[0] * max_py);

	for (i = 1; i < data->num_samples; i++) {
		cairo_line_to(cr, xs[i] * max_px, ys[i] * max_py);
	}
	cairo_stroke(cr);

	for (i = 0; i < gegl_curve_num_points(data->curve); i++) {
		double px;
		double py;
		gegl_curve_get_point(data->curve, i, &px, &py);
		px *= max_px;
		py *= max_py;

		cairo_save(cr);
		cairo_move_to(cr, px, py);
		cairo_set_source_rgb(cr, CURVE_WIDGET_POINT_COLOR);
		cairo_arc(cr, px, py, CURVE_WIDGET_POINT_RADIUS, 0.0, 2 * M_PI);
		cairo_fill_preserve(cr);
		cairo_stroke(cr);
		cairo_restore(cr);
	}

	cairo_destroy(cr);

	return FALSE;
}

static void calc_curve(widget_data_t *data) {
	assert(data->curve);

	gegl_curve_calc_values(
		data->curve,
        data->min_x,
		data->max_x,
		data->num_samples,
		data->xs,
		data->ys
	);
}

// gegl does not have a routine to delete a point
// creating a new curve in place of current and copying points
static void remove_point(widget_data_t *data, int index) {
	int i;
	GeglCurve *curve = gegl_curve_new(data->min_y, data->max_y);
	for (i = 0; i < gegl_curve_num_points(data->curve); i++) {
		if (i != index) {
			double x, y;
			gegl_curve_get_point(data->curve, i, &x, &y);
			gegl_curve_add_point(curve, x, y);
		}
	}
	g_object_unref(data->curve);
	data->curve = curve;
	g_object_ref(data->curve);
}


static gboolean configure_event(GtkWidget *widget, GdkEventConfigure *event) {
	widget_data_t *data = get_widget_data(widget);

	data->width = event->width;
	data->height = event->height;
	data->num_samples = data->width;

	if (data->xs)
		data->xs = (double *)realloc(data->xs, data->num_samples * sizeof(double));
	else
		data->xs = (double *)malloc(data->num_samples * sizeof(double));

	if (data->ys)
		data->ys = (double *)realloc(data->ys, data->num_samples * sizeof(double));
	else 
		data->ys = (double *)malloc(data->num_samples * sizeof(double));

	calc_curve(data);

	return FALSE;
}

static void points_changed(widget_data_t *data) {
	assert(data->curve);

	calc_curve(data);
	GtkWidget *widget = data->drawing_area;
	gtk_widget_queue_draw(widget);
	if (data->curve_changed_callback)
		data->curve_changed_callback(data->drawing_area, data->callback_data);
}

static gboolean is_inside_margin(widget_data_t *data, int x, int y) {
	int m = CURVE_WIDGET_MOUSE_MARGIN_SIZE;
	return (x > - m
		&& y > - m
		&& x < data->width + m
		&& y < data->height + m);
}

static void process_mouse_crossing(GtkWidget *widget, int mouse_x, int mouse_y) {
	crossing_type_t type;	
	gboolean was_inside, is_inside;
	widget_data_t *data = get_widget_data(widget);

	was_inside = is_inside_margin(data, data->last_mouse_x, data->last_mouse_y);
	is_inside = is_inside_margin(data, mouse_x, mouse_y);

	type = CR_NONE;
	if (! was_inside && is_inside) {
		type = CR_ENTER_MARGIN;
	} else {
		if (was_inside && ! is_inside)
			type = CR_LEAVE_MARGIN;
	}
		
	switch (type) {
		case CR_ENTER_MARGIN:
			data->drag_index = gegl_curve_num_points(data->curve);
			gegl_curve_add_point(data->curve, 0.0, 0.0);
			gdk_window_set_cursor((GTK_WIDGET(data->drawing_area)->window), data->cursor_move);
			points_changed(data);
			break;
		case CR_LEAVE_MARGIN:
			if (data->drag_index >= 0) {
				remove_point(data, data->drag_index);
				data->drag_index = -1;
				points_changed(data);
			}
			gdk_window_set_cursor((GTK_WIDGET(data->drawing_area)->window), data->cursor_add);
			break;
		case CR_NONE:
			break;
		default:
			assert(0);
	}
	data->last_mouse_x = mouse_x;
	data->last_mouse_y = mouse_y;
}

static void process_mouse(GtkWidget *widget, double mouse_x, double mouse_y, int is_press) {
	widget_data_t *data = get_widget_data(widget);
	double val_x, val_y, dx, dy, w, h;
	GdkCursor *cursor = NULL;
	int i;
	int nearest_index = -1;
	double nearest_distS = 10000000.0;
	double distS;
	double maxDist = CURVE_WIDGET_MAX_POINT_GRAB_DISTANCE;
	double maxDistS = maxDist * maxDist;

	assert(data->curve);

	w = (double)data->width;
	h = (double)data->height;

	val_x = mouse_x / w;
	val_y = (h - mouse_y) / h;

	if (val_x < data->min_x)
		val_x = data->min_x;
	if (val_x > data->max_x)
		val_x = data->max_x;

	if (val_y < data->min_y)
		val_y = data->min_y;
	if (val_y > data->max_y)
		val_y = data->max_y;

	if (data->drag_index >= 0) { // drag mode
		gegl_curve_set_point(data->curve, data->drag_index, val_x, val_y);
		points_changed(data);
		gdk_window_set_cursor ((GTK_WIDGET(data->drawing_area)->window), data->cursor_move);
		return;
	}

	// check points
	for (i = 0; i < gegl_curve_num_points(data->curve); i++) {
		double x, y;
		gegl_curve_get_point(data->curve, i, &x, &y);
		x *= w;
		y = h - y * h;
		dx = x - mouse_x;
		dy = y - mouse_y;
		distS = dx * dx + dy * dy;
		if (distS <= maxDistS && distS < nearest_distS) {
			nearest_distS = distS;
			nearest_index = i;
		}
	}
	if (nearest_index >= 0) { // found point near enough
		cursor = data->cursor_move;
		if (is_press) {
			data->drag_index = nearest_index;
			gegl_curve_set_point(data->curve, nearest_index, val_x, val_y);
			points_changed(data);
		}
	} else {
		// cursor far enough from any points
		cursor = data->cursor_add;
		if (is_press && data->drag_index == -1) {
			data->drag_index = gegl_curve_num_points(data->curve);
			gegl_curve_add_point(data->curve, val_x, val_y);
			points_changed(data);

			/* Always add point regardless how far is the cursor from the curve

			double y_at_x = data->ys[(int)mouse_x]; // assumes that num_samples == width
			y_at_x = h - y_at_x * h;

			double ydiff = y_at_x - (double)mouse_y;
			if (ydiff < 0.0)
				ydiff = -ydiff;

			// will suck for high slopes, need sort of distance to tangent
			// printf("YDIFF: %f\n", ydiff);

			if (ydiff < 3.0) {
				data->drag_index = gegl_curve_num_points(data->curve);
				gegl_curve_add_point(data->curve, val_x, val_y);
				points_changed(data);
			}
			*/
		}
	}

	gdk_window_set_cursor ((GTK_WIDGET(data->drawing_area)->window), cursor);
}


static gboolean motion_notify_event (GtkWidget *widget, GdkEventMotion *event) {
	if (event->state) // only if mouse button is held down
		process_mouse_crossing(widget, (int)event->x, (int)event->y);

	process_mouse(widget, event->x, event->y, 0);
	return FALSE;
}

static gboolean button_press_event (GtkWidget *widget, GdkEventButton *event) {
	// to ensure was_inside is true.
	widget_data_t *data = get_widget_data(widget);
	data->last_mouse_x = 1;
	data->last_mouse_y = 1;

	process_mouse(widget, event->x, event->y, 1);
	return FALSE;
}

static gboolean button_release_event (GtkWidget *widget, GdkEventButton *event) {
	widget_data_t *data = get_widget_data(widget);
	data->drag_index = -1;
	return FALSE;
}

GtkWidget *curve_widget_new(curve_widget_curve_changed_callback_t curve_changed_callback, void *callback_data) {
	widget_data_t *data;
	GtkWidget *drawing_area;

	data = g_new(widget_data_t, 1);

	data->curve_changed_callback = curve_changed_callback;
	data->callback_data = callback_data;

	data->xs = NULL;
	data->ys = NULL;

	data->min_x = 0.0;
	data->max_x = 1.0;
	data->min_y = 0.0;
	data->max_y = 1.0;

	GeglCurve *curve = gegl_curve_new(data->min_y, data->max_y);
	// gegl_curve_add_point(curve, data->min_x, data->min_y);
	// gegl_curve_add_point(curve, data->max_x, data->max_y);
	data->curve = curve;

	drawing_area = gtk_drawing_area_new();
	data->drawing_area = drawing_area;

	data->drag_index = -1;

	data->cursor_move = gdk_cursor_new(GDK_FLEUR);
	data->cursor_add = gdk_cursor_new(GDK_CROSS);

	gtk_widget_show(drawing_area);

	g_object_set_data(G_OBJECT(drawing_area), "curve-data", data);

	gtk_signal_connect(GTK_OBJECT(drawing_area), "expose_event",
		(GtkSignalFunc)expose_event, NULL);
	gtk_signal_connect(GTK_OBJECT(drawing_area), "configure_event",
		(GtkSignalFunc)configure_event, NULL);
	gtk_signal_connect(GTK_OBJECT(drawing_area), "motion_notify_event",
		(GtkSignalFunc)motion_notify_event, NULL);
	gtk_signal_connect(GTK_OBJECT(drawing_area), "button_press_event",
		(GtkSignalFunc)button_press_event, NULL);
	gtk_signal_connect(GTK_OBJECT(drawing_area), "button_release_event",
		(GtkSignalFunc)button_release_event, NULL);


	gtk_widget_set_events(drawing_area, GDK_EXPOSURE_MASK
		| GDK_LEAVE_NOTIFY_MASK
		| GDK_BUTTON_PRESS_MASK
		| GDK_BUTTON_RELEASE_MASK
		| GDK_POINTER_MOTION_MASK
		| GDK_POINTER_MOTION_HINT_MASK);

	return drawing_area;
}

void curve_widget_set_curve(GtkWidget *widget, GeglCurve *curve) {
	widget_data_t *data = get_widget_data(widget);
	if (data->curve) {
		g_object_unref(data->curve);
	}
	g_object_ref(curve);
	data->curve = curve;
}
GeglCurve *curve_widget_get_curve(GtkWidget *widget) {
	widget_data_t *data = get_widget_data(widget);
	return data->curve;
}

void curve_widget_destroy(GtkWidget *widget) {
	widget_data_t *data = get_widget_data(widget);
	if (data->xs)
		free(data->xs);
	if (data->ys)
		free(data->ys);	

	if (data->curve)
		g_object_unref(data->curve);

	gdk_cursor_destroy(data->cursor_move);
	gdk_cursor_destroy(data->cursor_add);
	gtk_widget_destroy(widget);
}

