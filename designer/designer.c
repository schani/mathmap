/* -*- c -*- */

/*
 * designer.c
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

#include "designer.h"

static gboolean
verify_slots (designer_node_t *node, gboolean input)
{
    GSList *slot_types = input ? node->type->input_slot_types : node->type->output_slot_types;
    designer_slot_t *slots = input ? node->input_slots : node->output_slots;
    GSList *slot_list;
    int i;

    for (i = 0, slot_list = slot_types;
	 slot_list != NULL;
	 ++i, slot_list = slot_list->next)
    {
	designer_slot_type_t *slot_type = slot_list->data;
	designer_slot_t *slot = &slots[i];

	if (slot->partner != NULL)
	{
	    designer_slot_t *partner_slots = input ? slot->partner->output_slots : slot->partner->input_slots;
	    GSList *partner_slot_types = input ? slot->partner->type->output_slot_types
					       : slot->partner->type->input_slot_types;
	    designer_slot_t *partner_slot = &partner_slots[slot->partner_slot_index];
	    designer_slot_type_t *partner_slot_type = g_slist_nth_data(partner_slot_types, slot->partner_slot_index);

	    g_assert(slot_type->type == partner_slot_type->type);
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
