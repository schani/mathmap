/* -*- c -*- */

/*
 * designer.c
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

#include <string.h>

#include "designer.h"

static gboolean
verify_slots (designer_node_t *node, gboolean input)
{
    GSList *slot_specs = input ? node->type->input_slot_specs : node->type->output_slot_specs;
    designer_slot_t *slots = input ? node->input_slots : node->output_slots;
    GSList *slot_list;
    int i;

    for (i = 0, slot_list = slot_specs;
	 slot_list != NULL;
	 ++i, slot_list = slot_list->next)
    {
	designer_slot_spec_t *slot_spec = slot_list->data;
	designer_slot_t *slot = &slots[i];

	if (slot->partner != NULL)
	{
	    designer_slot_t *partner_slots = input ? slot->partner->output_slots : slot->partner->input_slots;
	    GSList *partner_slot_specs = input ? slot->partner->type->output_slot_specs
					       : slot->partner->type->input_slot_specs;
	    designer_slot_t *partner_slot = &partner_slots[slot->partner_slot_index];
	    designer_slot_spec_t *partner_slot_spec = g_slist_nth_data(partner_slot_specs, slot->partner_slot_index);

	    g_assert(slot_spec->type == partner_slot_spec->type);
	    g_assert(partner_slot->partner == node);
	    g_assert(partner_slot->partner_slot_index == i);
	}
    }

    return TRUE;
}

gboolean
designer_verify_design (designer_design_t *design)
{
    GSList *node_list;

    for (node_list = design->nodes; node_list != NULL; node_list = node_list->next)
    {
	designer_node_t *node = node_list->data;

	/* design type contains node type */
	g_assert(g_slist_find(design->type->node_types, node->type) != NULL);

	g_assert(verify_slots(node, TRUE));
	g_assert(verify_slots(node, FALSE));
    }

    if (!design->type->allow_cycles)
	g_assert(!designer_design_contains_cycles(design));

    return TRUE;
}

designer_design_type_t*
designer_make_design_type (gboolean allow_cycles)
{
    designer_design_type_t *design_type = g_new0(designer_design_type_t, 1);

    design_type->allow_cycles = allow_cycles;

    return design_type;
}

void
designer_add_type (designer_design_type_t *design_type, const char *name)
{
    designer_type_t *type = g_new0(designer_type_t, 1);

    type->name = g_strdup(name);

    design_type->types = g_slist_prepend(design_type->types, type);
}

designer_node_type_t*
designer_add_node_type (designer_design_type_t *design_type, const char *name)
{
    designer_node_type_t *node_type = g_new0(designer_node_type_t, 1);

    node_type->design_type = design_type;
    node_type->name = g_strdup(name);

    design_type->node_types = g_slist_prepend(design_type->node_types, node_type);

    return node_type;
}

static designer_type_t*
lookup_type (designer_design_type_t *design_type, const char *name)
{
    GSList *list;

    for (list = design_type->types; list != NULL; list = list->next)
    {
	designer_type_t *type = list->data;

	if (strcmp(type->name, name) == 0)
	    return type;
    }

    return NULL;
}

static void
add_slot_spec (designer_node_type_t *node_type, const char *name, const char *type_name, GSList **slot_specs)
{
    designer_type_t *type = lookup_type(node_type->design_type, type_name);
    designer_slot_spec_t *slot_spec;

    g_assert(type != NULL);

    slot_spec = g_new0(designer_slot_spec_t, 1);

    slot_spec->name = g_strdup(name);
    slot_spec->type = type;

    *slot_specs = g_slist_append(*slot_specs, slot_spec);
}

void
designer_add_input_slot_spec (designer_node_type_t *node_type, const char *name, const char *type_name)
{
    add_slot_spec(node_type, name, type_name, &node_type->input_slot_specs);
}

void
designer_add_output_slot_spec (designer_node_type_t *node_type, const char *name, const char *type_name)
{
    add_slot_spec(node_type, name, type_name, &node_type->output_slot_specs);
}

designer_design_t*
designer_make_design (designer_design_type_t *type)
{
    designer_design_t *design = g_new0(designer_design_t, 1);

    design->type = type;

    return design;
}

static designer_node_type_t*
lookup_node_type (designer_design_type_t *design_type, const char *name)
{
    GSList *list;

    for (list = design_type->node_types; list != NULL; list = list->next)
    {
	designer_node_type_t *type = list->data;

	if (strcmp(type->name, name) == 0)
	    return type;
    }

    return NULL;
}

designer_node_t*
designer_add_node (designer_design_t *design, const char *name, const char *node_type_name)
{
    designer_node_type_t *node_type = lookup_node_type(design->type, node_type_name);
    int num_input_slots;
    int num_output_slots;
    designer_node_t *node;

    g_assert(node_type != NULL);

    num_input_slots = g_slist_length(node_type->input_slot_specs);
    num_output_slots = g_slist_length(node_type->output_slot_specs);

    node = g_new0(designer_node_t, 1);

    node->design = design;
    node->name = g_strdup(name);
    node->type = node_type;
    node->input_slots = g_new0(designer_slot_t, num_input_slots);
    node->output_slots = g_new0(designer_slot_t, num_output_slots);

    design->nodes = g_slist_prepend(design->nodes, node);

    return node;
}

static designer_slot_spec_t*
lookup_slot_spec (GSList *list, const char *name, int *index)
{
    int i;

    for (i = 0;
	 list != NULL;
	 ++i, list = list->next)
    {
	designer_slot_spec_t *spec = list->data;

	if (strcmp(spec->name, name) == 0)
	{
	    *index = i;
	    return spec;
	}
    }

    return NULL;
}

gboolean
designer_connect_nodes (designer_node_t *source, const char *output_slot_name,
			designer_node_t *dest, const char *input_slot_name)
{
    designer_node_type_t *source_type = source->type;
    designer_node_type_t *dest_type = dest->type;
    designer_design_type_t *design_type = source_type->design_type;
    int output_slot_index, input_slot_index;
    designer_slot_spec_t *output_slot_spec = lookup_slot_spec(source_type->output_slot_specs, output_slot_name,
							      &output_slot_index);
    designer_slot_spec_t *input_slot_spec = lookup_slot_spec(dest_type->input_slot_specs, input_slot_name,
							     &input_slot_index);

    g_assert(source->design == dest->design);
    g_assert(output_slot_spec != NULL);
    g_assert(input_slot_spec != NULL);

    if (source_type->design_type != dest_type->design_type)
	return FALSE;

    g_assert(source->output_slots[output_slot_index].partner == NULL);
    g_assert(dest->input_slots[input_slot_index].partner == NULL);

    source->output_slots[output_slot_index].partner = dest;
    source->output_slots[output_slot_index].partner_slot_index = input_slot_index;

    dest->input_slots[input_slot_index].partner = source;
    dest->input_slots[input_slot_index].partner_slot_index = output_slot_index;

    if (!design_type->allow_cycles && designer_design_contains_cycles(source->design))
    {
	source->output_slots[output_slot_index].partner = NULL;
	dest->input_slots[input_slot_index].partner = NULL;
	return FALSE;
    }

    return TRUE;
}

void
designer_disconnect_nodes (designer_node_t *source, const char *output_slot_name,
			   designer_node_t *dest, const char *input_slot_name)
{
    designer_node_type_t *source_type = source->type;
    designer_node_type_t *dest_type = dest->type;
    int output_slot_index, input_slot_index;
    designer_slot_spec_t *output_slot_spec = lookup_slot_spec(source_type->output_slot_specs, output_slot_name,
							      &output_slot_index);
    designer_slot_spec_t *input_slot_spec = lookup_slot_spec(dest_type->input_slot_specs, input_slot_name,
							     &input_slot_index);

    g_assert(source->design == dest->design);
    g_assert(source_type->design_type == dest_type->design_type);
    g_assert(output_slot_spec != NULL);
    g_assert(input_slot_spec != NULL);

    g_assert(source->output_slots[output_slot_index].partner == dest);
    g_assert(source->output_slots[output_slot_index].partner_slot_index == input_slot_index);

    g_assert(dest->input_slots[input_slot_index].partner == source);
    g_assert(dest->input_slots[input_slot_index].partner_slot_index == output_slot_index);

    source->output_slots[output_slot_index].partner = NULL;
    dest->input_slots[input_slot_index].partner = NULL;
}
