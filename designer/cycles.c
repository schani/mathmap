/* -*- c -*- */

/*
 * cycles.c
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

#include "designer.h"

typedef struct _node_t
{
    gboolean alive;
    int num_edges;
    struct _node_t **edges;
} node_t;

static node_t*
nodes_from_design (designer_design_t *design, int *_num_nodes, int *_num_edges, node_t ***_edges)
{
    int num_nodes = g_slist_length(design->nodes);
    int num_total_edges;
    node_t *nodes = g_new0(node_t, num_nodes);
    node_t **edges;
    GSList *node_list;
    GHashTable *node_hash = g_hash_table_new(g_direct_hash, g_direct_equal);
    int i;
    int edge_index;

    num_total_edges = 0;
    for (i = 0, node_list = design->nodes;
	 node_list != NULL;
	 ++i, node_list = node_list->next)
    {
	designer_node_t *node = node_list->data;
	int num_slots = g_slist_length(node->type->output_slot_specs);
	int num_edges = 0;
	int j;

	for (j = 0; j < num_slots; ++j)
	    if (node->output_slots[j].partner != NULL)
		++num_edges;

	num_total_edges += num_edges;

	nodes[i].alive = TRUE;
	nodes[i].num_edges = num_edges;
	g_hash_table_insert(node_hash, node, &nodes[i]);
    }
    g_assert(i == num_nodes);

    edges = g_new0(node_t*, num_total_edges);

    edge_index = 0;
    for (i = 0, node_list = design->nodes;
	 node_list != NULL;
	 ++i, node_list = node_list->next)
    {
	designer_node_t *node = node_list->data;
	int num_slots = g_slist_length(node->type->output_slot_specs);
	int j, k;

	nodes[i].edges = &edges[edge_index];
	edge_index += nodes[i].num_edges;

	k = 0;
	for (j = 0; j < num_slots; ++j)
	    if (node->output_slots[j].partner != NULL)
	    {
		nodes[i].edges[k] = g_hash_table_lookup(node_hash, node->output_slots[j].partner);

		g_assert(nodes[i].edges[k] != NULL);

		++k;
	    }
    }

    g_assert(edge_index == num_total_edges);

    g_hash_table_destroy(node_hash);

    *_num_nodes = num_nodes;
    *_edges = edges;
    *_num_edges = num_total_edges;

    return nodes;
}

gboolean
designer_design_contains_cycles (designer_design_t *design)
{
    int num_nodes;
    int num_edges;
    node_t **edges;
    node_t *nodes = nodes_from_design(design, &num_nodes, &num_edges, &edges);
    int i;

    for (;;)
    {
	gboolean deleted_node = FALSE;

	for (i = 0; i < num_nodes; ++i)
	    if (nodes[i].alive)
	    {
		int j;

		for (j = 0; j < nodes[i].num_edges; ++j)
		    if (nodes[i].edges[j] != NULL)
			break;

		if (j == nodes[i].num_edges)
		{
		    /* no edges found - kill node */
		    nodes[i].alive = FALSE;

		    /* remove all edges to the node */
		    for (j = 0; j < num_edges; ++j)
			if (edges[j] == &nodes[i])
			    edges[j] = NULL;

		    deleted_node = TRUE;
		}
	    }

	if (!deleted_node)
	    break;
    }

    g_free(edges);

    for (i = 0; i < num_nodes; ++i)
	if (nodes[i].alive)
	    break;

    g_free(nodes);

    return i < num_nodes;
}
