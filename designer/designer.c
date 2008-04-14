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
verify_slots (designer_node_t *node)
{
    GSList *slot_list;
    int i;

    for (i = 0, slot_list = node->type->input_slot_specs;
	 slot_list != NULL;
	 ++i, slot_list = slot_list->next)
    {
	designer_slot_spec_t *slot_spec = slot_list->data;
	designer_slot_t *slot = &node->input_slots[i];

	if (slot->partner != NULL)
	    g_assert(slot_spec->type == slot->partner_slot_spec->type);
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

	g_assert(verify_slots(node));
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
designer_free_design_type (designer_design_type_t *type)
{
    /* FIXME: implement */
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

void
designer_add_type (designer_design_type_t *design_type, const char *name)
{
    designer_type_t *type;

    g_assert(lookup_type(design_type, name) == NULL);

    type = g_new0(designer_type_t, 1);
    type->name = g_strdup(name);

    design_type->types = g_slist_prepend(design_type->types, type);
}

designer_node_type_t*
designer_add_node_type (designer_design_type_t *design_type, const char *name, gpointer data)
{
    designer_node_type_t *node_type;

    if (designer_get_node_type_by_name(design_type, name) != NULL)
	return NULL;

    node_type = g_new0(designer_node_type_t, 1);
    node_type->design_type = design_type;
    node_type->name = g_strdup(name);
    node_type->data = data;

    design_type->node_types = g_slist_prepend(design_type->node_types, node_type);

    return node_type;
}

static void
add_slot_spec (designer_node_type_t *node_type, const char *name,
	       const char *type_name, gpointer data,
	       GSList **slot_specs)
{
    designer_type_t *type = lookup_type(node_type->design_type, type_name);
    designer_slot_spec_t *slot_spec;

    g_assert(type != NULL);

    slot_spec = g_new0(designer_slot_spec_t, 1);

    slot_spec->name = g_strdup(name);
    slot_spec->type = type;
    slot_spec->data = data;

    *slot_specs = g_slist_append(*slot_specs, slot_spec);
}

void
designer_add_input_slot_spec (designer_node_type_t *node_type, const char *name,
			      const char *type_name, gpointer data)
{
    add_slot_spec(node_type, name, type_name, data, &node_type->input_slot_specs);
}

void
designer_add_output_slot_spec (designer_node_type_t *node_type, const char *name,
			       const char *type_name, gpointer data)
{
    add_slot_spec(node_type, name, type_name, data, &node_type->output_slot_specs);
}

designer_design_t*
designer_make_design (designer_design_type_t *type, const char *name)
{
    designer_design_t *design = g_new0(designer_design_t, 1);

    design->type = type;
    design->name = g_strdup(name);

    return design;
}

void
designer_free_design (designer_design_t *design)
{
    /* FIXME: implement */
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
    designer_node_t *node;
    int i;

    if (node_type == NULL)
	return NULL;

    if (designer_get_node_by_name(design, name) != NULL)
	return NULL;

    num_input_slots = g_slist_length(node_type->input_slot_specs);

    node = g_new0(designer_node_t, 1);

    node->design = design;
    node->name = g_strdup(name);
    node->type = node_type;
    node->input_slots = g_new0(designer_slot_t, num_input_slots);

    for (i = 0; i < num_input_slots; ++i)
	node->input_slots[i].node = node;

    design->nodes = g_slist_prepend(design->nodes, node);

    /* FIXME: remove eventually */
    designer_verify_design(design);

    return node;
}

void
designer_delete_node (designer_node_t *node)
{
    designer_design_t *design = node->design;
    int num_slots = g_slist_length(node->type->input_slot_specs);
    int i;

    for (i = 0; i < num_slots; ++i)
	g_assert(node->input_slots[i].partner == NULL);

    if (design->root == node)
	design->root = NULL;

    node->design->nodes = g_slist_remove(node->design->nodes, node);

    g_free(node->input_slots);
    g_free(node);

    /* FIXME: remove this eventually, but assert on output slots */
    designer_verify_design(design);
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

    if (output_slot_spec == NULL || input_slot_spec == NULL)
	return FALSE;

    if (source_type->design_type != dest_type->design_type)
	return FALSE;

    g_assert(dest->input_slots[input_slot_index].partner == NULL);

    dest->input_slots[input_slot_index].partner = source;
    dest->input_slots[input_slot_index].partner_slot_spec = output_slot_spec;

    if (!design_type->allow_cycles && designer_design_contains_cycles(source->design))
    {
	dest->input_slots[input_slot_index].partner = NULL;
	return FALSE;
    }

    /* FIXME: remove eventually */
    designer_verify_design(source->design);

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

    g_assert(dest->input_slots[input_slot_index].partner == source);
    g_assert(dest->input_slots[input_slot_index].partner_slot_spec == output_slot_spec);

    dest->input_slots[input_slot_index].partner = NULL;

    /* FIXME: remove eventually */
    designer_verify_design(source->design);
}

void
designer_set_design_name (designer_design_t *design, const char *name)
{
    g_free(design->name);
    design->name = g_strdup(name);
}

designer_slot_t*
designer_node_get_input_slot (designer_node_t *node, const char *name)
{
    int i;
    GSList *list;

    for (i = 0, list = node->type->input_slot_specs;
	 list != NULL;
	 ++i, list = list->next)
    {
	designer_slot_spec_t *spec = list->data;

	if (strcmp(spec->name, name) == 0)
	    return &node->input_slots[i];
    }

    return NULL;
}

designer_node_type_t*
designer_get_node_type_by_name (designer_design_type_t *design_type, const char *name)
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
designer_get_node_by_name (designer_design_t *design, const char *name)
{
    GSList *list;

    for (list = design->nodes; list != NULL; list = list->next)
    {
	designer_node_t *node = list->data;

	if (strcmp(node->name, name) == 0)
	    return node;
    }

    return NULL;
}

void
designer_set_root (designer_design_t *design, designer_node_t *root)
{
    if (root != NULL)
	g_assert(root->design == design);

    design->root = root;
}

designer_design_t*
designer_migrate_design (designer_design_t *design, designer_design_type_t *new_type)
{
    designer_design_t *new_design = designer_make_design(new_type, design->name);
    GSList *node_list;

    /* create nodes */
    for (node_list = design->nodes; node_list != NULL; node_list = node_list->next)
    {
	designer_node_t *node = node_list->data;
	designer_node_type_t *new_node_type = designer_get_node_type_by_name(new_type, node->type->name);

	if (new_node_type == NULL)
	    continue;

	designer_add_node(new_design, node->name, new_node_type->name);
    }

    /* make connections */
    for (node_list = design->nodes; node_list != NULL; node_list = node_list->next)
    {
	designer_node_t *node = node_list->data;
	designer_node_t *new_node = designer_get_node_by_name(new_design, node->name);
	GSList *slot_list;
	int i;

	if (new_node == NULL)
	    continue;

	for (i = 0, slot_list = node->type->input_slot_specs;
	     slot_list != NULL;
	     ++i, slot_list = slot_list->next)
	{
	    designer_slot_spec_t *spec = slot_list->data;
	    designer_node_t *new_partner;

	    if (node->input_slots[i].partner == NULL)
		continue;

	    new_partner = designer_get_node_by_name(new_design, node->input_slots[i].partner->name);
	    if (new_partner == NULL)
		continue;

	    designer_connect_nodes(new_partner, node->input_slots[i].partner_slot_spec->name,
				   new_node, spec->name);
	}
    }

    /* root */
    if (design->root != NULL)
	new_design->root = designer_get_node_by_name(new_design, design->root->name);

    return new_design;
}
