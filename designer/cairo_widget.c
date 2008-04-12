/* -*- c -*- */

/*
 * cairo_widget.c
 *
 * MathMap
 *
 * Copyright (C) 2008 Herbert Poetzl
 * Copyright (C) 2008 Mark Probst
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
#include <string.h>
#include <math.h>

#include <gtk/gtk.h>

#include "../lispreader/lispreader.h"

#include "designer.h"

typedef struct
{
    designer_design_t *design;
    designer_design_changed_callback_t design_changed_callback;
    designer_node_focussed_callback_t node_focussed_callback;
    GtkWidget *widget;
    GtkWidget *drawing_area;
    GtkWidget *hscrollbar;
    GtkWidget *vscrollbar;
    gboolean dragging;
    double x;
    double y;
} widget_data_t;

static void
signal_design_change (widget_data_t *data)
{
    if (data->design_changed_callback != NULL)
	data->design_changed_callback(data->widget, data->design);
}

static widget_data_t*
get_widget_data (GtkWidget *widget)
{
    widget_data_t *data = g_object_get_data(G_OBJECT(widget), "designer-data");

    g_assert(data != NULL);

    return data;
}

static gboolean
expose_event (GtkWidget *widget, GdkEventExpose *event)
{
    widget_data_t *data = get_widget_data(widget);

    cairo_t *cairo = gdk_cairo_create(GTK_LAYOUT(widget)->bin_window);
    double width = widget->allocation.width;
    double height = widget->allocation.height;

    double x1, y1, x2, y2, dx, dy, f, fx, fy;
    double cp_x1, cp_y1, cp_x2, cp_y2;
    int hor = 1;

    g_assert(cairo != NULL);

    x1 = 100 + 0.5; y1 = 100 + 0.5;
    x2 = data->x + 0.5; y2 = data->y + 0.5;

    dx = x2 - x1; dy = y2 - y1;
    f = 0.5;

#if 1
    if (dx*dx > dy*dy) {
    	cp_x1 = x1 + dx*f; cp_y1 = y1;
    	cp_x2 = x2 - dx*f; cp_y2 = y2;
    } else {
    	cp_x1 = x1; cp_y1 = y1 + dy*f;
    	cp_x2 = x2; cp_y2 = y2 - dy*f;
    }
#else
    if (dx*dx < dy*dy) {
	fx = (dx*dx)/(dy*dy)*f;
	fy = 1-fx;
    } else {
	fy = (dy*dy)/(dx*dx)*f;
	fx = 1-fy;
    }
    cp_x1 = x1 + dx*fx; cp_y1 = y1 + dy*fy;
    cp_x2 = x2 - dx*fx; cp_y2 = y2 - dy*fy;
#endif

    cairo_set_source_rgba(cairo, 0.0, 0.0, 0.0, 0.6);
    cairo_set_line_width(cairo, 1.0);
    cairo_arc (cairo, x1, y1, 4.0, 0, 2 * M_PI);
    cairo_stroke(cairo);

    cairo_set_source_rgba(cairo, 0.0, 0.0, 1.0, 0.6);
    cairo_arc (cairo, cp_x1, cp_y1, 2.0, 0, 2 * M_PI);
    cairo_stroke(cairo);
    cairo_set_source_rgba(cairo, 0.0, 1.0, 0.0, 0.6);
    cairo_arc (cairo, cp_x2, cp_y2, 2.0, 0, 2 * M_PI);
    cairo_stroke(cairo);

    cairo_set_source_rgba(cairo, 1.0, 0.0, 0.0, 0.4);
    cairo_set_line_width(cairo, 4.0);
    cairo_set_line_cap(cairo, CAIRO_LINE_CAP_ROUND);

    cairo_new_path(cairo);
    cairo_move_to(cairo, x1, y1);
    cairo_curve_to(cairo, cp_x1, cp_y1, cp_x2, cp_y2, data->x, data->y);
    cairo_stroke(cairo);

    if (data->dragging)
    {
	cairo_text_extents_t extents;
	const char *utf8 = "hyva bertlomi";
	double x,y;

	cairo_set_source_rgba(cairo, 0.0, 0.0, 0.0, 0.7);

	cairo_translate(cairo, data->x, data->y);

	x = data->x - width / 2;
	y = data->y - height / 2;

	cairo_rotate(cairo, atan2(y, x) + M_PI / 2);

	cairo_select_font_face (cairo, "Sans",
				CAIRO_FONT_SLANT_NORMAL,
				CAIRO_FONT_WEIGHT_NORMAL);

	cairo_set_font_size (cairo, (width + height) / 15.0);
	cairo_text_extents (cairo, utf8, &extents);
	x = -(extents.width/2 + extents.x_bearing);
	y = -(extents.height/2 + extents.y_bearing);

	cairo_move_to (cairo, x, y);
	cairo_show_text (cairo, utf8);
    }

    return FALSE;
}

static gboolean
button_press_event (GtkWidget *widget, GdkEventButton *event)
{
    widget_data_t *data = get_widget_data(widget);

    data->dragging = TRUE;

    gtk_widget_queue_draw(widget);

    return TRUE;
}

static gboolean
button_release_event (GtkWidget *widget, GdkEventButton *event)
{
    widget_data_t *data = get_widget_data(widget);

    data->dragging = FALSE;

    gtk_widget_queue_draw(widget);

    return TRUE;
}

static gboolean
motion_notify_event (GtkWidget *widget, GdkEventMotion *event)
{
    widget_data_t *data = get_widget_data(widget);
    int x, y;
    GdkModifierType state;

    if (event->is_hint)
	gdk_window_get_pointer (event->window, &x, &y, &state);
    else
    {
	x = event->x;
	y = event->y;
	state = event->state;
    }

    data->x = x;
    data->y = y;

    gtk_widget_queue_draw(widget);

    return TRUE;
}

static void
populate_table (widget_data_t *data)
{
    GtkWidget *table = data->widget;
    GtkWidget *drawing_area, *w;

    drawing_area = gtk_layout_new(NULL, NULL);

    gtk_widget_show(drawing_area);
    data->drawing_area = drawing_area;

    gtk_table_attach (GTK_TABLE (table), drawing_area,
		      0, 1, 0, 1,
		      GTK_EXPAND | GTK_FILL | GTK_SHRINK,
		      GTK_EXPAND | GTK_FILL | GTK_SHRINK,
		      0, 0);

    w = gtk_hscrollbar_new (GTK_LAYOUT (drawing_area)->hadjustment);
    gtk_table_attach (GTK_TABLE (table), w,
		      0, 1, 1, 2,
		      GTK_EXPAND | GTK_FILL | GTK_SHRINK,
		      GTK_FILL,
		      0, 0);
    gtk_widget_show (w);
    data->hscrollbar = w;

    w = gtk_vscrollbar_new (GTK_LAYOUT (drawing_area)->vadjustment);
    gtk_table_attach (GTK_TABLE (table), w,
		      1, 2, 0, 1,
		      GTK_FILL,
		      GTK_EXPAND | GTK_FILL | GTK_SHRINK,
		      0, 0);
    gtk_widget_show (w);
    data->vscrollbar = w;

    gtk_signal_connect(GTK_OBJECT(drawing_area), "expose_event",
		       (GtkSignalFunc)expose_event, NULL);
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
}

GtkWidget*
designer_widget_new (designer_design_t *design,
		     designer_design_changed_callback_t design_changed_callback,
		     designer_node_focussed_callback_t node_focussed_callback)
{
    GtkWidget *table;
    widget_data_t *data;

    data = g_new0(widget_data_t, 1);
    data->design = design;
    data->design_changed_callback = design_changed_callback;
    data->node_focussed_callback = node_focussed_callback;

    table = gtk_table_new (2, 2, FALSE);
    gtk_table_set_row_spacings (GTK_TABLE (table), 4);
    gtk_table_set_col_spacings (GTK_TABLE (table), 4);
    gtk_widget_show (table);

    data->widget = table;

    populate_table(data);

    g_object_set_data(G_OBJECT(table), "designer-data", data);
    g_object_set_data(G_OBJECT(data->drawing_area), "designer-data", data);

    return table;
}

void
designer_widget_add_node (GtkWidget *widget, designer_node_t *node, double x, double y)
{
    widget_data_t *data = get_widget_data(widget);

    g_print("widget %p adds node %s at %gx%g\n", data, node->name, x, y);
}

void
designer_widget_set_design (GtkWidget *widget, designer_design_t *design)
{
    widget_data_t *data = get_widget_data(widget);

    g_print("widget %p sets design to %p\n", data, design);

    data->design = design;
}

void
designer_widget_get_node_position (GtkWidget *widget, designer_node_t *node, double *x, double *y)
{
    widget_data_t *data = get_widget_data(widget);

    g_print("widget %p retrieves position of node %s\n", data, node->name);

    *x = *y = 0.0;
}

void
designer_widget_move_node (GtkWidget *widget, designer_node_t *node, double x, double y)
{
    widget_data_t *data = get_widget_data(widget);

    g_print("widget %p moves node %s to %gx%g\n", data, node->name, x, y);
}

void
designer_widget_design_loaded_callback (designer_design_t *design, gpointer user_data)
{
    GtkWidget *widget = GTK_WIDGET(user_data);
    widget_data_t *data = get_widget_data(widget);

    g_print("design loaded for widget %p\n", data);

    designer_widget_set_design(widget, design);
}

void
designer_widget_node_aux_load_callback (designer_node_t *node, lisp_object_t *obj, gpointer user_data)
{
    lisp_object_t *x = lisp_proplist_lookup_symbol(obj, ":x");
    lisp_object_t *y = lisp_proplist_lookup_symbol(obj, ":y");

    g_assert(lisp_number_p(x) && lisp_number_p(y));

    designer_widget_move_node(GTK_WIDGET(user_data), node, lisp_real(x), lisp_real(y));
}

void
designer_widget_node_aux_print (designer_node_t *node, gpointer user_data, FILE *out)
{
    double x, y;

    designer_widget_get_node_position(GTK_WIDGET(user_data), node, &x, &y);

    lisp_print_open_paren(out);

    lisp_print_symbol(":x", out);
    lisp_print_real(x, out);
    lisp_print_symbol(":y", out);
    lisp_print_real(y, out);

    lisp_print_close_paren(out);
}
