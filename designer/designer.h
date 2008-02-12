/* -*- c -*- */

/*
 * designer.h
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

#ifndef __DESIGNER_H__
#define __DESIGNER_H__

#include <glib.h>
#include <gtk/gtk.h>

#include "../lispreader/lispreader.h"

typedef struct _designer_design_type_t designer_design_type_t;
typedef struct _designer_design_t designer_design_t;
typedef struct _designer_node_t designer_node_t;

typedef struct
{
    char *name;
} designer_type_t;

typedef struct
{
    char *name;
    designer_type_t *type;
    gpointer data;
} designer_slot_spec_t;

typedef struct
{
    designer_design_type_t *design_type;
    char *name;
    GSList *input_slot_specs;
    GSList *output_slot_specs;
    gpointer data;
} designer_node_type_t;

typedef struct
{
    designer_node_t *partner; /* NULL if not assigned */
    designer_slot_spec_t *partner_slot_spec;
} designer_slot_t;

struct _designer_node_t
{
    designer_design_t *design;
    designer_node_type_t *type;
    char *name;
    designer_slot_t *input_slots;
};

struct _designer_design_type_t
{
    gboolean allow_cycles;
    GSList *types;
    GSList *node_types;
};

struct _designer_design_t
{
    char *name;
    designer_design_type_t *type;
    designer_node_t *root;
    GSList *nodes;
};

typedef void (*designer_design_changed_callback_t) (GtkWidget *widget, designer_design_t *design);
typedef void (*designer_node_focussed_callback_t) (GtkWidget *widget, designer_node_t *node);

typedef void (*designer_design_loaded_callback_t) (designer_design_t *design, gpointer user_data);
typedef void (*designer_node_aux_load_callback_t) (designer_node_t *node, lisp_object_t *obj, gpointer user_data);
typedef void (*designer_design_aux_load_callback_t) (designer_design_t *design, lisp_object_t *obj, gpointer user_data);

typedef void (*designer_node_aux_print_func_t) (designer_node_t *node, gpointer user_data, FILE *out);
typedef void (*designer_design_aux_print_func_t) (designer_design_t *design, gpointer user_data, FILE *out);

/* design type */

extern designer_design_type_t* designer_make_design_type (gboolean allow_cycles);

extern void designer_add_type (designer_design_type_t *design_type, const char *name);

extern designer_node_type_t* designer_add_node_type (designer_design_type_t *design_type, const char *name, gpointer data);
extern void designer_add_input_slot_spec (designer_node_type_t *node_type, const char *name,
					  const char *type_name, gpointer data);
extern void designer_add_output_slot_spec (designer_node_type_t *node_type, const char *name,
					   const char *type_name, gpointer data);

extern designer_node_type_t* designer_get_node_type_by_name (designer_design_type_t *design_type, const char *name);

/* design */

extern designer_design_t* designer_make_design (designer_design_type_t *type, const char *name);

extern designer_node_t* designer_add_node (designer_design_t *design, const char *name, const char *node_type_name);
extern void designer_delete_node (designer_node_t *node);

extern gboolean designer_connect_nodes (designer_node_t *source, const char *output_slot_name,
					designer_node_t *dest, const char *input_slot_name);
extern void designer_disconnect_nodes (designer_node_t *source, const char *output_slot_name,
				       designer_node_t *dest, const char *input_slot_name);

extern void designer_set_design_name (designer_design_t *design, const char *name);

extern designer_slot_t* designer_node_get_input_slot (designer_node_t *node, const char *name);

extern designer_node_t* designer_get_node_by_name (designer_design_t *design, const char *name);

extern void designer_set_root (designer_design_t *design, designer_node_t *root);

extern gboolean designer_verify_design (designer_design_t *design);
extern gboolean designer_design_contains_cycles (designer_design_t *design);

/* load/save */

extern designer_design_t* designer_load_design (designer_design_type_t *design_type, const char *filename,
						designer_design_loaded_callback_t loaded_callback,
						designer_node_aux_load_callback_t node_aux_load,
						designer_design_aux_load_callback_t design_aux_load,
						gpointer user_data);
extern gboolean designer_save_design (designer_design_t *design, const char *filename,
				      designer_node_aux_print_func_t node_aux_print,
				      designer_design_aux_print_func_t design_aux_print,
				      gpointer user_data);

/* widget */

extern GtkWidget* designer_widget_new (designer_design_t *design,
				       designer_design_changed_callback_t design_changed_callback,
				       designer_node_focussed_callback_t node_focussed_callback);

extern void designer_widget_set_design (GtkWidget *widget, designer_design_t *design);

extern void designer_widget_add_node (GtkWidget *widget, designer_node_t *node, double x, double y);

extern designer_node_t* designer_widget_get_focussed_node (GtkWidget *widget);

extern void designer_widget_design_loaded_callback (designer_design_t *design, gpointer user_data);
extern void designer_widget_node_aux_load_callback (designer_node_t *node, lisp_object_t *obj, gpointer user_data);

extern void designer_widget_node_aux_print (designer_node_t *node, gpointer user_data, FILE *out);

#endif
