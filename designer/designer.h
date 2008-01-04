/* -*- c -*- */

/*
 * designer.h
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

#ifndef __DESIGNER_H__
#define __DESIGNER_H__

#include <glib.h>

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
} designer_slot_spec_t;

typedef struct
{
    designer_design_type_t *design_type;
    char *name;
    GSList *input_slot_specs;
    GSList *output_slot_specs;
} designer_node_type_t;

typedef struct
{
    designer_node_t *partner; /* NULL if not assigned */
    int partner_slot_index;
} designer_slot_t;

struct _designer_node_t
{
    designer_design_t *design;
    designer_node_type_t *type;
    char *name;
    designer_slot_t *input_slots;
    designer_slot_t *output_slots;
};

struct _designer_design_type_t
{
    gboolean allow_cycles;
    GSList *types;
    GSList *node_types;
};

struct _designer_design_t
{
    designer_design_type_t *type;
    GSList *nodes;
};

extern gboolean designer_verify_design (designer_design_t *design);
extern gboolean designer_design_contains_cycles (designer_design_t *design);

extern designer_design_type_t* designer_make_design_type (gboolean allow_cycles);

extern void designer_add_type (designer_design_type_t *design_type, const char *name);

extern designer_node_type_t* designer_add_node_type (designer_design_type_t *design_type, const char *name);
extern void designer_add_input_slot_spec (designer_node_type_t *node_type, const char *name, const char *type_name);
extern void designer_add_output_slot_spec (designer_node_type_t *node_type, const char *name, const char *type_name);

extern designer_design_t* designer_make_design (designer_design_type_t *type);

extern designer_node_t* designer_add_node (designer_design_t *design, const char *name, const char *node_type_name);

extern gboolean designer_connect_nodes (designer_node_t *source, const char *output_slot_name,
					designer_node_t *dest, const char *input_slot_name);
extern void designer_disconnect_nodes (designer_node_t *source, const char *output_slot_name,
				       designer_node_t *dest, const char *input_slot_name);

extern char* make_filter_source_from_node (designer_node_t *root, const char *filter_name);


#endif
