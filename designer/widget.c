/* -*- c -*- */

/*
 * widget.c
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

#include <stdlib.h>

#include <gtk/gtkmain.h>
#include <gtk/gtkwindow.h>
#include <libgnomecanvas/libgnomecanvas.h>

#include "designer.h"

#define SLOT_RADIUS			5.0
#define SLOT_DIAMETER			(SLOT_RADIUS * 2)
#define SLOT_SPACING			3.0

typedef struct
{
    GnomeCanvas *canvas;
    gboolean dragging;
    double x;
    double y;
    GnomeCanvasItem *slot;
    GtkObject *destroy_object;
} widget_data_t;

static void
set_bpath_path (GnomeCanvasItem *bpath)
{
    GnomeCanvasItem *slot1 = g_object_get_data(G_OBJECT(bpath), "slot1");
    GnomeCanvasItem *slot2 = g_object_get_data(G_OBJECT(bpath), "slot2");
    GnomeCanvasPathDef *path_def;
    double x1, x2, y1, y2;

    g_assert(slot1 != NULL && slot2 != NULL);

    g_object_get(slot1, "x1", &x1, "y1", &y1, NULL);
    gnome_canvas_item_i2w(slot1, &x1, &y1);

    g_object_get(slot2, "x1", &x2, "y1", &y2, NULL);
    gnome_canvas_item_i2w(slot2, &x2, &y2);

    path_def = gnome_canvas_path_def_new ();

    gnome_canvas_path_def_moveto(path_def, x1 + SLOT_RADIUS, y1 + SLOT_RADIUS);
    gnome_canvas_path_def_lineto(path_def, x2 + SLOT_RADIUS, y2 + SLOT_RADIUS);

    g_object_set(bpath, "bpath", path_def, NULL);

    gnome_canvas_path_def_unref(path_def);
}

static void
update_node_edges (GnomeCanvasGroup *group)
{
    GList *list;

    for (list = group->item_list; list != NULL; list = list->next)
    {
	GnomeCanvasItem *bpath = g_object_get_data(G_OBJECT(list->data), "slot-bpath");

	if (bpath != NULL)
	    set_bpath_path(bpath);
    }
}

static gint
rectangle_event (GnomeCanvasItem *item, GdkEvent *event, widget_data_t *data)
{
    GnomeCanvasGroup *group;
    double item_x, item_y;

    g_object_get(G_OBJECT(item), "parent", &group, NULL);
    g_assert(group != NULL);

    item_x = event->button.x;
    item_y = event->button.y;
    gnome_canvas_item_w2i(GNOME_CANVAS_ITEM(gnome_canvas_root(data->canvas)), &item_x, &item_y);

    switch (event->type)
    {
	case GDK_BUTTON_PRESS :
	    if (event->button.button == 1)
	    {
		data->x = item_x;
		data->y = item_y;
		data->dragging = TRUE;
		return TRUE;
	    }
	    break;

	case GDK_MOTION_NOTIFY :
	    if (data->dragging)
	    {
		double new_x = item_x;
		double new_y = item_y;

		gnome_canvas_item_move(GNOME_CANVAS_ITEM(group), new_x - data->x, new_y - data->y);
		update_node_edges(group);

		data->x = new_x;
		data->y = new_y;
		return TRUE;
	    }
	    break;

	case GDK_BUTTON_RELEASE :
	    data->dragging = FALSE;
	    return TRUE;

	default :
	    break;
    }

    return FALSE;
}

static designer_node_t*
slot_get_node (GnomeCanvasItem *slot)
{
    GnomeCanvasGroup *group;
    designer_node_t *node;

    g_object_get(slot, "parent", &group, NULL);

    g_assert(group != NULL);

    node = g_object_get_data(G_OBJECT(group), "node");

    g_assert(node != NULL);

    return node;
}

static gboolean
get_edge_data (GnomeCanvasItem *slot1, GnomeCanvasItem *slot2,
	       designer_node_t **output_node, char **output_slot_name,
	       designer_node_t **input_node, char **input_slot_name)
{
    gboolean slot1_is_input = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(slot1), "slot-is-input"));
    gboolean slot2_is_input = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(slot2), "slot-is-input"));

    if (slot1_is_input == slot2_is_input)
	return FALSE;

    /* Make sure slot1 is the output slot */
    if (slot1_is_input)
    {
	GnomeCanvasItem *tmp = slot1;

	slot1 = slot2;
	slot2 = tmp;
    }

    *output_node = slot_get_node(slot1);
    *output_slot_name = g_object_get_data(G_OBJECT(slot1), "slot-name");

    *input_node = slot_get_node(slot2);
    *input_slot_name = g_object_get_data(G_OBJECT(slot2), "slot-name");

    return TRUE;
}

static GnomeCanvasItem*
make_edge (GnomeCanvas *canvas, GnomeCanvasItem *slot1, GnomeCanvasItem *slot2)
{
    GnomeCanvasItem *bpath;
    designer_node_t *output_node, *input_node;
    char *output_node_name, *input_node_name;

    if (!get_edge_data(slot1, slot2, &output_node, &output_node_name, &input_node, &input_node_name))
	return NULL;

    if (!designer_connect_nodes(output_node, output_node_name, input_node, input_node_name))
	return NULL;

    bpath = gnome_canvas_item_new(gnome_canvas_root(canvas),
				  gnome_canvas_bpath_get_type(),
				  "outline_color", "blue",
				  "width_pixels", 5,
				  "cap_style", GDK_CAP_ROUND,
				  NULL);

    g_object_set_data(G_OBJECT(bpath), "slot1", slot1);
    g_object_set_data(G_OBJECT(bpath), "slot2", slot2);

    g_object_set_data(G_OBJECT(slot1), "slot-bpath", bpath);
    g_object_set_data(G_OBJECT(slot2), "slot-bpath", bpath);

    set_bpath_path(bpath);

    return bpath;
}

static void
set_destroy_object (widget_data_t *data, GtkObject *object)
{
    g_assert(data->destroy_object == NULL);
    data->destroy_object = object;
}

static void
do_destroy_object (widget_data_t *data)
{
    if (data->destroy_object == NULL)
	return;

    gtk_object_destroy(data->destroy_object);

    data->destroy_object = NULL;
}

static void
remove_edge (GnomeCanvasItem *bpath, widget_data_t *data)
{
    GnomeCanvasItem *slot1 = g_object_get_data(G_OBJECT(bpath), "slot1");
    GnomeCanvasItem *slot2 = g_object_get_data(G_OBJECT(bpath), "slot2");
    designer_node_t *output_node, *input_node;
    char *output_slot_name, *input_slot_name;
    gboolean result;

    g_assert(slot1 != NULL && slot2 != NULL);

    result = get_edge_data(slot1, slot2, &output_node, &output_slot_name, &input_node, &input_slot_name);
    g_assert(result);

    designer_disconnect_nodes(output_node, output_slot_name, input_node, input_slot_name);

    g_object_set_data(G_OBJECT(slot1), "slot-bpath", NULL);
    g_object_set_data(G_OBJECT(slot2), "slot-bpath", NULL);

    gnome_canvas_item_hide(bpath);

    set_destroy_object(data, GTK_OBJECT(bpath));
}

static GnomeCanvasItem*
get_slot_at (GnomeCanvas *canvas, double x, double y)
{
    GList *node_list;
    GnomeCanvasGroup *root = gnome_canvas_root(canvas);

    g_print("looking for slot at %g:%g\n", x, y);

    /* Walk through all the canvas items to find nodes */
    for (node_list = root->item_list; node_list != NULL; node_list = node_list->next)
    {
	if (g_object_get_data(G_OBJECT(node_list->data), "node") != NULL)
	{
	    GnomeCanvasGroup *group = GNOME_CANVAS_GROUP(node_list->data);
	    GList *item_list;

	    /* Now we walk through all the group items to find slots */
	    for (item_list = group->item_list; item_list != NULL; item_list = item_list->next)
	    {
		GnomeCanvasItem *item = GNOME_CANVAS_ITEM(item_list->data);

		if (g_object_get_data(G_OBJECT(item), "slot-name") != NULL)
		{
		    double x1, y1, x2, y2;

		    gnome_canvas_item_get_bounds(item, &x1, &y1, &x2, &y2);

		    gnome_canvas_item_i2w(item, &x1, &y1);
		    gnome_canvas_item_i2w(item, &x2, &y2);

		    g_print("slot at %g:%g - %g:%g\n", x1, y1, x2, y2);

		    if (x >= x1 && x <= x2 && y >= y1 && y <= y2)
			return item;
		}
	    }
	}
    }

    return NULL;
}

static void
remove_slot_edge (GnomeCanvasItem *slot, widget_data_t *data)
{
    GnomeCanvasItem *bpath;

    bpath = g_object_get_data(G_OBJECT(slot), "slot-bpath");
    if (bpath != NULL)
	remove_edge(bpath, data);
}

static gint
root_event (GnomeCanvasGroup *root, GdkEvent *event, widget_data_t *data)
{
    //g_print("root event\n");

    switch (event->type)
    {
	case GDK_BUTTON_PRESS :
	    if (event->button.button == 1)
	    {
		GnomeCanvasItem *slot = get_slot_at(data->canvas, event->button.x, event->button.y);

		if (slot == NULL)
		    return FALSE;

		remove_slot_edge(slot, data);

		g_assert(data->slot == NULL);
		data->slot = slot;
		return TRUE;
	    }
	    break;

	case GDK_MOTION_NOTIFY :
	    if (data->slot != NULL)
	    {
		return TRUE;
	    }
	    break;

	case GDK_BUTTON_RELEASE :
	    do_destroy_object(data);
	    if (event->button.button == 1)
	    {
		printf("slot released\n");
		if (data->slot != NULL)
		{
		    GnomeCanvasItem *other_slot = get_slot_at(data->canvas, event->button.x, event->button.y);

		    if (other_slot != NULL && g_object_get_data(G_OBJECT(other_slot), "slot-name") != NULL)
		    {
			remove_slot_edge(other_slot, data);
			do_destroy_object(data);
			make_edge(data->canvas, data->slot, other_slot);
		    }

		    data->slot = NULL;
		    return TRUE;
		}
	    }
	    break;

	default :
	    break;
    }

    return FALSE;
}

static GnomeCanvasGroup*
make_node (GnomeCanvas *canvas, designer_node_t *node, float x1, float y1, widget_data_t *data)
{
    GnomeCanvasGroup *group;
    GnomeCanvasItem *rectangle;
    GnomeCanvasItem *ellipse;
    double width, height;
    int num_input_slots = g_slist_length(node->type->input_slot_specs);
    int num_output_slots = g_slist_length(node->type->output_slot_specs);
    int max_slots = MAX(num_input_slots, num_output_slots);
    int i;

    width = 60;
    height = max_slots * (SLOT_DIAMETER + SLOT_SPACING) + SLOT_SPACING;

    group = GNOME_CANVAS_GROUP(gnome_canvas_item_new(gnome_canvas_root(canvas),
						     gnome_canvas_group_get_type(),
						     "x", x1,
						     "y", y1,
						     NULL));

    g_object_set_data(G_OBJECT(group), "node", node);

    rectangle = gnome_canvas_item_new(group,
				      gnome_canvas_rect_get_type(),
				      "x1", 0.0,
				      "y1", 0.0,
				      "x2", width,
				      "y2", height,
				      "fill_color", "darkred",
				      NULL);
    g_signal_connect(rectangle, "event", G_CALLBACK(rectangle_event), data);

    for (i = 0; i < num_input_slots; ++i)
    {
	designer_slot_spec_t *slot_spec = g_slist_nth_data(node->type->input_slot_specs, i);

	ellipse = gnome_canvas_item_new(group,
					gnome_canvas_ellipse_get_type(),
					"x1", SLOT_SPACING,
					"y1", i * (SLOT_SPACING + SLOT_DIAMETER) + SLOT_SPACING,
					"x2", SLOT_SPACING + SLOT_DIAMETER,
					"y2", (i + 1) * (SLOT_SPACING + SLOT_DIAMETER),
					"fill_color", "yellow",
					NULL);
	g_object_set_data(G_OBJECT(ellipse), "slot-name", slot_spec->name);
	g_object_set_data(G_OBJECT(ellipse), "slot-is-input", GINT_TO_POINTER(1));
    }

    for (i = 0; i < num_output_slots; ++i)
    {
	designer_slot_spec_t *slot_spec = g_slist_nth_data(node->type->output_slot_specs, i);

	ellipse = gnome_canvas_item_new(group,
					gnome_canvas_ellipse_get_type(),
					"x1", width - SLOT_SPACING - SLOT_DIAMETER,
					"y1", i * (SLOT_SPACING + SLOT_DIAMETER) + SLOT_SPACING,
					"x2", width - SLOT_SPACING,
					"y2", (i + 1) * (SLOT_SPACING + SLOT_DIAMETER),
					"fill_color", "yellow",
					NULL);
	g_object_set_data(G_OBJECT(ellipse), "slot-name", slot_spec->name);
	g_object_set_data(G_OBJECT(ellipse), "slot-is-input", GINT_TO_POINTER(0));
    }

    return group;
}

designer_design_type_t*
setup_design_type (void)
{
    designer_design_type_t *design_type = designer_make_design_type(FALSE);
    designer_node_type_t *desaturate, *combine;

    designer_add_type(design_type, "image");

    desaturate = designer_add_node_type(design_type, "desaturate");
    designer_add_input_slot_spec(desaturate, "in", "image");
    designer_add_output_slot_spec(desaturate, "out", "image");

    combine = designer_add_node_type(design_type, "combine");
    designer_add_input_slot_spec(combine, "in1", "image");
    designer_add_input_slot_spec(combine, "in2", "image");
    designer_add_output_slot_spec(combine, "out", "image");

    return design_type;
}

int
main(int argc, char** argv)
{
    GtkWidget *window;
    GtkWidget *canvas;
    widget_data_t *data;
    designer_design_type_t *design_type = setup_design_type();
    designer_design_t *design = designer_make_design(design_type);
    designer_node_t *node1 = designer_add_node(design, "desat", "desaturate");
    designer_node_t *node2 = designer_add_node(design, "comb", "combine");

    designer_verify_design(design);

    gtk_init(&argc, &argv);

    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    g_signal_connect(window, "destroy",
		     G_CALLBACK(gtk_main_quit), NULL);

    canvas = gnome_canvas_new_aa();

    data = g_new0(widget_data_t, 1);
    data->canvas = GNOME_CANVAS(canvas);

    g_signal_connect(gnome_canvas_root(GNOME_CANVAS(canvas)), "event", G_CALLBACK(root_event), data);

    make_node(GNOME_CANVAS(canvas), node1, 10, 10, data);
    make_node(GNOME_CANVAS(canvas), node2, 110, 110, data);

    gtk_container_add(GTK_CONTAINER(window), canvas);
    gtk_widget_show_all(window);
    gtk_main();

    return 0;
}
