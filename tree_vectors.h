/*
 * tree_vectors.h
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

#ifndef __TREE_VECTORS_H__
#define __TREE_VECTORS_H__

#include "mmpools.h"

#define TREE_VECTOR_ARITY	4
#define TREE_VECTOR_SHIFT	2

typedef union _tree_vector_node_t
{
    union _tree_vector_node_t *subs[TREE_VECTOR_ARITY];
    float data[TREE_VECTOR_ARITY];
} tree_vector_node_t;

typedef struct _tree_vector_t
{
    int length;
    int depth;
    tree_vector_node_t root;
} tree_vector_t;

/* TEMPLATE tree_vector_funcs */
extern tree_vector_t* new_tree_vector (mathmap_pools_t *pools, int length, float *data);
extern float tree_vector_get (tree_vector_t *tv, int index);
extern tree_vector_t* tree_vector_set (mathmap_pools_t *pools, tree_vector_t *tv, int index, float value);
/* END */

#endif
