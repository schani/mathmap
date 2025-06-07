/* -*- c -*- */

/*
 * designer_widget.h
 *
 * MathMap
 *
 * Copyright (C) 2007-2012 Mark Probst
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

#ifndef __DESIGNER_WIDGET_H__
#define __DESIGNER_WIDGET_H__

typedef void* designer_widget_t;

typedef void (*designer_design_changed_callback_t) (designer_widget_t widget, designer_design_t *design);
typedef void (*designer_node_focussed_callback_t) (designer_widget_t widget, designer_node_t *node);
typedef gboolean (*designer_node_title_change_callback_t) (designer_widget_t widget, designer_node_t *node, const char *name);

/* widget_data */

#define designer_node_type_set_widget_data(nt,d)	((nt)->widget_data = (d))
#define designer_node_type_get_widget_data(nt)		((nt)->widget_data)

#define designer_slot_spec_set_widget_data(ss,d)	((ss)->widget_data = (d))
#define designer_slot_spec_get_widget_data(ss)		((ss)->widget_data)

#define designer_node_set_widget_data(n,d)		((n)->widget_data = (d))
#define designer_node_get_widget_data(n)		((n)->widget_data)

#define designer_slot_set_widget_data(sl,d)		((sl)->widget_data = (d))
#define designer_slot_get_widget_data(sl)		((sl)->widget_data)

/* widget */

extern designer_widget_t designer_widget_new (designer_design_t *design,
					      designer_design_changed_callback_t design_changed_callback,
					      designer_node_focussed_callback_t node_focussed_callback,
					      designer_node_title_change_callback_t node_title_change_callback);

extern void designer_widget_set_design (designer_widget_t widget, designer_design_t *design);

extern void designer_widget_add_node (designer_widget_t widget, designer_node_t *node, double x, double y);

extern void designer_widget_get_node_position (designer_widget_t widget, designer_node_t *node, double *x, double *y);
extern void designer_widget_move_node (designer_widget_t widget, designer_node_t *node, double x, double y);

extern void designer_widget_design_loaded_callback (designer_design_t *design, gpointer user_data);
extern void designer_widget_node_aux_load_callback (designer_node_t *node, lisp_object_t *obj, gpointer user_data);

extern void designer_widget_node_aux_print (designer_node_t *node, gpointer user_data, FILE *out);

#endif
