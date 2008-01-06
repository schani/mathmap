/* -*- c -*- */

/*
 * designer_filter.c
 *
 * MathMap
 *
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

#include "designer/designer.h"
#include "userval.h"
#include "expression_db.h"

static void
append_node_slot (GString *string, designer_node_t *node, userval_info_t *info)
{
    g_string_append_printf(string, "%s_%s", node->name, info->name);
}

static void
append_node_result (GString *string, designer_node_t *node)
{
    designer_slot_spec_t *slot_spec;

    g_assert(g_slist_length(node->type->output_slot_specs) == 1);

    slot_spec = node->type->output_slot_specs->data;

    g_string_append_printf(string, "%s_%s", node->name, slot_spec->name);
}

static void
compute_node (designer_node_t *node, GString *string, GSList **computed_nodes)
{
    expression_db_t *edb = node->type->data;
    userval_info_t *args = get_expression_args(edb);
    int num_slots, i;
    gboolean first;
    GSList *list;

    if (g_slist_find(*computed_nodes, node) != NULL)
	return;

    num_slots = g_slist_length(node->type->input_slot_specs);

    /* compute all the dependencies first */
    for (i = 0; i < num_slots; ++i)
	if (node->input_slots[i].partner != NULL)
	    compute_node(node->input_slots[i].partner, string, computed_nodes);

    g_string_append(string, "    ");
    append_node_result(string, node);
    g_string_append_printf(string, " = %s(", node->type->name);

    first = TRUE;
    while (args != NULL)
    {
	designer_slot_t *slot = designer_node_get_input_slot(node, args->name);

	if (!first)
	    g_string_append(string, ", ");
	else
	    first = FALSE;

	if (slot == NULL || slot->partner == NULL)
	    append_node_slot(string, node, args);
	else
	    append_node_result(string, slot->partner);

	args = args->next;
    }

    g_string_append(string, ");\n");

    g_assert(g_slist_find(*computed_nodes, node) == NULL);
    *computed_nodes = g_slist_prepend(*computed_nodes, node);
}

static void
append_limits_and_defaults (GString *string, userval_info_t *info)
{
    switch (info->type)
    {
	case USERVAL_INT_CONST :
	    g_string_append_printf(string, " : %d - %d (%d)",
				   info->v.int_const.min, info->v.int_const.max,
				   info->v.int_const.default_value);
	    break;

	case USERVAL_FLOAT_CONST :
	    g_string_append_printf(string, " : %g - %g (%g)",
				   info->v.float_const.min, info->v.float_const.max,
				   info->v.float_const.default_value);
	    break;

	case USERVAL_BOOL_CONST :
	    g_string_append_printf(string, " (%c)", info->v.bool_const.default_value ? '1' : '0');
	    break;

	default :
	    break;
    }
}

char*
make_filter_source_from_designer_node (designer_node_t *root, const char *filter_name)
{
    GSList *nodes = g_slist_prepend(NULL, root);
    GString *string;
    gboolean first;
    GSList *list;
    GSList *computed_nodes = NULL;

    for (;;)
    {
	gboolean finished = TRUE;

	for (list = nodes; list != NULL; list = list->next)
	{
	    designer_node_t *node = list->data;
	    int num_slots = g_slist_length(node->type->input_slot_specs);
	    int i;

	    for (i = 0; i < num_slots; ++i)
	    {
		designer_node_t *partner = node->input_slots[i].partner;

		if (partner != NULL && g_slist_find(nodes, partner) == NULL)
		{
		    nodes = g_slist_prepend(nodes, partner);
		    finished = FALSE;
		}
	    }
	}

	if (finished)
	    break;
    }

    string = g_string_new("");

    /* include all the filter sources */
    for (list = nodes; list != NULL; list = list->next)
    {
	designer_node_t *node = list->data;
	expression_db_t *edb = node->type->data;
	const char *path = edb->v.expression.path;
	char *source;

	g_assert(path != NULL);

	if (!g_file_get_contents(path, &source, NULL, NULL))
	{
	    /* FIXME: free string! */
	    return NULL;
	}

	g_string_append_printf(string, "%s\n\n", source);

	g_free(source);
    }

    g_string_append_printf(string, "filter %s (", filter_name);

    first = TRUE;
    for (list = nodes; list != NULL; list = list->next)
    {
	designer_node_t *node = list->data;
	expression_db_t *edb = node->type->data;
	userval_info_t *args = get_expression_args(edb);
	GSList *slot_list;
	int i;

	while (args != NULL)
	{
	    designer_slot_t *slot = designer_node_get_input_slot(node, args->name);

	    if (slot == NULL || slot->partner == NULL)
	    {
		if (!first)
		    g_string_append(string, ", ");
		else
		    first = FALSE;

		g_string_append_printf(string, "%s ", userval_type_name(args->type));
		append_node_slot(string, node, args);
		append_limits_and_defaults(string, args);
	    }

	    args = args->next;
	}
    }
    g_string_append(string, ")\n");

    compute_node(root, string, &computed_nodes);

    g_string_append(string, "    ");
    append_node_result(string, root);
    g_string_append(string, "(xy)\nend\n");

    g_slist_free(nodes);
    g_slist_free(computed_nodes);

    return g_string_free(string, FALSE);
}
