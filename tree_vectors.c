/*
 * tree_vectors.c
 *
 * MathMap
 *
 * Copyright (C) 2009 Mark Probst
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
#include <stdlib.h>

#include <glib.h>

#include "tree_vectors.h"

#define LENGTH_FOR_DEPTH(d)	(1 << (((d) + 1) * TREE_VECTOR_SHIFT))

static int
get_depth_for_length (int length)
{
    int depth = 0;

    --length;
    while (length >= TREE_VECTOR_ARITY)
    {
	length >>= TREE_VECTOR_SHIFT;
	++depth;
    }

    return depth;
}

static void
populate (pools_t *pools, tree_vector_node_t *node, int length, int depth, float *data)
{
    int i;

    if (depth == 0)
    {
	g_assert(length <= TREE_VECTOR_ARITY);
	memcpy(node->data, data, sizeof(float) * length);
	return;
    }

    for (i = 0; i < TREE_VECTOR_ARITY; ++i)
    {
	if (length > 0)
	{
	    int sub_length = MIN(length, LENGTH_FOR_DEPTH(depth - 1));

	    node->subs[i] = pools_alloc(pools, sizeof(tree_vector_node_t));
	    populate(pools, node->subs[i], sub_length, depth - 1, data);
	    length -= sub_length;
	    data += sub_length;
	}
	else
	    node->subs[i] = NULL;
    }
    g_assert(length == 0);
}

tree_vector_t*
new_tree_vector (pools_t *pools, int length, float *data)
{
    tree_vector_t *tv = pools_alloc(pools, sizeof(tree_vector_t));

    g_assert(length > 0);

    tv->length = length;
    tv->depth = get_depth_for_length(length);
    populate(pools, &tv->root, length, tv->depth, data);

    return tv;
}

float
tree_vector_get (tree_vector_t *tv, int index)
{
    int depth = tv->depth;
    tree_vector_node_t *node = &tv->root;

    if (index < 0)
	index = 0;
    else if (index >= tv->length)
	index = tv->length - 1;

    while (depth > 0)
    {
	node = node->subs[index >> (depth * TREE_VECTOR_SHIFT)];
	index &= (1 << (depth * TREE_VECTOR_SHIFT)) - 1;
	--depth;
    }
    return node->data[index];
}

tree_vector_t*
tree_vector_set (pools_t *pools, tree_vector_t *tv, int index, float value)
{
    int depth = tv->depth;
    tree_vector_t *new;
    tree_vector_node_t *node;

    if (index < 0)
	index = 0;
    else if (index >= tv->length)
	index = tv->length - 1;

    if (depth == 0)
    {
	new = new_tree_vector(pools, tv->length, tv->root.data);
	new->root.data[index] = value;
	return new;
    }

    new = pools_alloc(pools, sizeof(tree_vector_t));
    new->length = tv->length;
    new->depth = tv->depth;
    memcpy(new->root.subs, tv->root.subs, sizeof(tree_vector_node_t*) * TREE_VECTOR_ARITY);
    node = &new->root;

    while (depth > 0)
    {
	tree_vector_node_t *new_sub = pools_alloc(pools, sizeof(tree_vector_node_t));
	int sub_index = index >> (depth * TREE_VECTOR_SHIFT);

	memcpy(new_sub->subs, node->subs[sub_index]->subs, sizeof(tree_vector_node_t*) * TREE_VECTOR_ARITY);
	node = node->subs[sub_index] = new_sub;

	index &= (1 << (depth * TREE_VECTOR_SHIFT)) - 1;
	--depth;
    }

    node->data[index] = value;

    return new;
}

#ifdef TEST_TREE_VECTORS
static void
check (int length, tree_vector_t *tv, float *data)
{
    int i;
    for (i = 0; i < length; ++i)
	g_assert(tree_vector_get(tv, i) == data[i]);
}

static void
run_test (int length)
{
    pools_t pools;
    float data[length];
    tree_vector_t *tv;
    int i;

    g_print("testing %d\n", length);

    for (i = 0; i < length; ++i)
	data[i] = (float)i;

    init_pools(&pools);

    tv = new_tree_vector(&pools, length, data);

    for (i = 0; i < 1024; ++i)
    {
	int index = random() % length;
	float value = (float)random();
	tree_vector_t *new = tree_vector_set(&pools, tv, index, value);
	check(length, tv, data);
	data[index] = value;
	tv = new;
    }
    check(length, tv, data);

    free_pools(&pools);

}

int
main (void)
{
    int i;
    for (i = 1; i < 1024; ++i)
	run_test(i);
    return 0;
}
#endif
