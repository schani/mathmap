/* -*- c -*- */

/*
 * widget.c
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

#include <stdlib.h>

#include <gtk/gtk.h>
#include <libgnomecanvas/libgnomecanvas.h>

#include "designer.h"

#define SLOT_RADIUS			5.0
#define SLOT_DIAMETER			(SLOT_RADIUS * 2)
#define SLOT_SPACING			3.0
#define TITLE_PADDING			3.0
#define SLOT_NAME_PADDING		10.0
#define CANVAS_PADDING			20.0

typedef struct
{
    designer_design_t *design;
    GtkWidget *widget;
    GnomeCanvas *canvas;
    gboolean dragging;
    double x;
    double y;
    GnomeCanvasItem *slot;
    GtkObject *destroy_object;
    designer_design_changed_callback_t design_changed_callback;
    designer_node_focussed_callback_t node_focussed_callback;
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

static designer_node_t*
group_get_node (GnomeCanvasGroup *group)
{
    designer_node_t *node = g_object_get_data(G_OBJECT(group), "node");

    g_assert(node != NULL);

    return node;
}

static designer_node_t*
slot_get_node (GnomeCanvasItem *slot)
{
    GnomeCanvasGroup *group;

    g_object_get(slot, "parent", &group, NULL);

    g_assert(group != NULL);

    return group_get_node(group);
}

static void
set_scroll_region (GnomeCanvas *canvas)
{
    double x_max = 0.0;
    double y_max = 0.0;
    GnomeCanvasGroup *root = gnome_canvas_root(canvas);
    GList *list;

    for (list = root->item_list; list != NULL; list = list->next)
	if (g_object_get_data(G_OBJECT(list->data), "node") != NULL)
	{
	    GnomeCanvasGroup *group = list->data;
	    GList *sublist;

	    for (sublist = group->item_list; sublist != NULL; sublist = sublist->next)
	    {
		double x1, y1, x2, y2;

		gnome_canvas_item_get_bounds(sublist->data, &x1, &y1, &x2, &y2);
		gnome_canvas_item_i2w(sublist->data, &x2, &y2);

		x_max = MAX(x_max, x2);
		y_max = MAX(y_max, y2);
	    }
	}

    gnome_canvas_set_scroll_region(GNOME_CANVAS(canvas), 0, 0, x_max + CANVAS_PADDING, y_max + CANVAS_PADDING);
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

		set_scroll_region(data->canvas);

		data->x = new_x;
		data->y = new_y;
		return TRUE;
	    }
	    break;

	case GDK_BUTTON_RELEASE :
	    if (data->node_focussed_callback != NULL)
	    {
		designer_node_t *node = group_get_node(group);

		g_assert(node != NULL);

		data->node_focussed_callback(data->widget, node);
	    }

	    data->dragging = FALSE;
	    return TRUE;

	default :
	    break;
    }

    return FALSE;
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
make_edge (GnomeCanvas *canvas, GnomeCanvasItem *slot1, GnomeCanvasItem *slot2, widget_data_t *data)
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

    if (data->design_changed_callback != NULL)
	data->design_changed_callback(data->widget, data->design);

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

    if (data->design_changed_callback != NULL)
	data->design_changed_callback(data->widget, data->design);
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
			make_edge(data->canvas, data->slot, other_slot, data);
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

static void
get_text_size (const char *text, GtkWidget *widget, PangoFontDescription *font_desc, int *width, int *height)
{
    PangoContext *context = gtk_widget_get_pango_context(widget);
    PangoLayout *layout = pango_layout_new(context);

    g_assert(layout != NULL);

    pango_layout_set_text(layout, text, -1);

    pango_layout_get_pixel_size(layout, width, height);

    g_object_unref(G_OBJECT(layout));
}

static GnomeCanvasGroup*
make_node (GnomeCanvas *canvas, designer_node_t *node, float x1, float y1, widget_data_t *data)
{
    GnomeCanvasGroup *group;
    GnomeCanvasItem *rectangle, *ellipse, *title;
    double width, height;
    int num_input_slots = g_slist_length(node->type->input_slot_specs);
    int num_output_slots = g_slist_length(node->type->output_slot_specs);
    int max_slots = MAX(num_input_slots, num_output_slots);
    int i;
    /* FIXME: get some standard font from preferences here */
    PangoFontDescription *font_desc = pango_font_description_from_string("Sans 10");
    int title_width, title_height;
    double slots_y1;
    double widest_line;

    g_assert(font_desc != NULL);

    get_text_size(node->name, GTK_WIDGET(canvas), font_desc, &title_width, &title_height);

    widest_line = 0.0;
    for (i = 0; i < MAX(num_input_slots, num_output_slots); ++i)
    {
	designer_slot_spec_t *input = g_slist_nth_data(node->type->input_slot_specs, i);
	designer_slot_spec_t *output = g_slist_nth_data(node->type->output_slot_specs, i);
	int dummy, input_width, output_width;
	double width;

	if (input != NULL)
	    get_text_size(input->name, GTK_WIDGET(canvas), font_desc, &input_width, &dummy);
	else
	    input_width = 0;

	if (output != NULL)
	    get_text_size(output->name, GTK_WIDGET(canvas), font_desc, &output_width, &dummy);
	else
	    output_width = 0;

	width = (SLOT_DIAMETER + SLOT_SPACING * 2) * 2 + SLOT_NAME_PADDING + input_width + output_width;
	if (width > widest_line)
	    widest_line = width;
    }

    width = MAX(widest_line, title_width + TITLE_PADDING * 2);
    height = title_height + TITLE_PADDING * 2 + max_slots * (SLOT_DIAMETER + SLOT_SPACING) + SLOT_SPACING;

    slots_y1 = title_height + TITLE_PADDING * 2 + SLOT_SPACING;

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
				      "fill-color", "lightblue",
				      NULL);
    g_signal_connect(rectangle, "event", G_CALLBACK(rectangle_event), data);

    title = gnome_canvas_item_new(group,
				  gnome_canvas_text_get_type(),
				  "text", node->name,
				  "x", width / 2,
				  "y", TITLE_PADDING,
				  "font-desc", font_desc,
				  "anchor", GTK_ANCHOR_N,
				  "fill-color", "white",
				  NULL);

    for (i = 0; i < num_input_slots; ++i)
    {
	designer_slot_spec_t *slot_spec = g_slist_nth_data(node->type->input_slot_specs, i);
	GnomeCanvasItem *text;
	double y1 = slots_y1 + i * (SLOT_SPACING + SLOT_DIAMETER);

	ellipse = gnome_canvas_item_new(group,
					gnome_canvas_ellipse_get_type(),
					"x1", SLOT_SPACING,
					"y1", y1,
					"x2", SLOT_SPACING + SLOT_DIAMETER,
					"y2", y1 + SLOT_DIAMETER,
					"fill_color", "yellow",
					NULL);
	g_object_set_data(G_OBJECT(ellipse), "slot-name", slot_spec->name);
	g_object_set_data(G_OBJECT(ellipse), "slot-is-input", GINT_TO_POINTER(1));

	text = gnome_canvas_item_new(group,
				     gnome_canvas_text_get_type(),
				     "text", slot_spec->name,
				     "x", SLOT_SPACING * 2 + SLOT_DIAMETER,
				     "y", y1,
				     "font-desc", font_desc,
				     "anchor", GTK_ANCHOR_NW,
				     "fill-color", "white",
				     NULL);
    }

    for (i = 0; i < num_output_slots; ++i)
    {
	designer_slot_spec_t *slot_spec = g_slist_nth_data(node->type->output_slot_specs, i);
	GnomeCanvasItem *text;
	double y1 = slots_y1 + i * (SLOT_SPACING + SLOT_DIAMETER);

	ellipse = gnome_canvas_item_new(group,
					gnome_canvas_ellipse_get_type(),
					"x1", width - SLOT_SPACING - SLOT_DIAMETER,
					"y1", y1,
					"x2", width - SLOT_SPACING,
					"y2", y1 + SLOT_DIAMETER,
					"fill_color", "yellow",
					NULL);
	g_object_set_data(G_OBJECT(ellipse), "slot-name", slot_spec->name);
	g_object_set_data(G_OBJECT(ellipse), "slot-is-input", GINT_TO_POINTER(0));

	text = gnome_canvas_item_new(group,
				     gnome_canvas_text_get_type(),
				     "text", slot_spec->name,
				     "x", width - SLOT_SPACING * 2 - SLOT_DIAMETER,
				     "y", y1,
				     "font-desc", font_desc,
				     "anchor", GTK_ANCHOR_NE,
				     "fill-color", "white",
				     NULL);
    }

    set_scroll_region(data->canvas);

    return group;
}

GtkWidget*
designer_widget_new (designer_design_t *design,
		     designer_design_changed_callback_t design_changed_callback,
		     designer_node_focussed_callback_t node_focussed_callback)
{
    GtkWidget *canvas, *table, *w;
    widget_data_t *data;

    canvas = gnome_canvas_new();

    gnome_canvas_set_center_scroll_region(GNOME_CANVAS(canvas), FALSE);
    gnome_canvas_set_scroll_region(GNOME_CANVAS(canvas), 0, 0, CANVAS_PADDING, CANVAS_PADDING);

    gtk_widget_show(canvas);

    table = gtk_table_new (2, 2, FALSE);
    gtk_table_set_row_spacings (GTK_TABLE (table), 4);
    gtk_table_set_col_spacings (GTK_TABLE (table), 4);
    gtk_widget_show (table);

    gtk_table_attach (GTK_TABLE (table), canvas,
		      0, 1, 0, 1,
		      GTK_EXPAND | GTK_FILL | GTK_SHRINK,
		      GTK_EXPAND | GTK_FILL | GTK_SHRINK,
		      0, 0);

    w = gtk_hscrollbar_new (GTK_LAYOUT (canvas)->hadjustment);
    gtk_table_attach (GTK_TABLE (table), w,
		      0, 1, 1, 2,
		      GTK_EXPAND | GTK_FILL | GTK_SHRINK,
		      GTK_FILL,
		      0, 0);
    gtk_widget_show (w);

    w = gtk_vscrollbar_new (GTK_LAYOUT (canvas)->vadjustment);
    gtk_table_attach (GTK_TABLE (table), w,
		      1, 2, 0, 1,
		      GTK_FILL,
		      GTK_EXPAND | GTK_FILL | GTK_SHRINK,
		      0, 0);
    gtk_widget_show (w);

    data = g_new0(widget_data_t, 1);
    data->design = design;
    data->widget = table;
    data->canvas = GNOME_CANVAS(canvas);
    data->design_changed_callback = design_changed_callback;
    data->node_focussed_callback = node_focussed_callback;

    g_object_set_data(G_OBJECT(table), "designer-data", data);

    g_signal_connect(gnome_canvas_root(GNOME_CANVAS(canvas)), "event", G_CALLBACK(root_event), data);

    return table;
}

void
designer_widget_add_node (GtkWidget *widget, designer_node_t *node, double x, double y)
{
    widget_data_t *data = g_object_get_data(G_OBJECT(widget), "designer-data");

    g_assert(data != NULL);

    make_node(data->canvas, node, x, y, data);
}

#ifdef DESIGNER_TEST
static designer_design_type_t*
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
    GtkWidget *window, *designer;
    designer_design_type_t *design_type = setup_design_type();
    designer_design_t *design = designer_make_design(design_type);
    designer_node_t *node1 = designer_add_node(design, "desat", "desaturate");
    designer_node_t *node2 = designer_add_node(design, "comb", "combine");

    gtk_init(&argc, &argv);

    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    g_signal_connect(window, "destroy",
		     G_CALLBACK(gtk_main_quit), NULL);


    designer = designer_widget_new(design);

    designer_widget_add_node(designer, node1, 10, 10);
    designer_widget_add_node(designer, node2, 70, 170);

    gtk_container_add(GTK_CONTAINER(window), designer);
    gtk_widget_show_all(window);
    gtk_main();

    return 0;
}
#endif
