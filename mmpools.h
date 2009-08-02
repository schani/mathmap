/*
 * mmpools.h
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

#ifndef __MMPOOLS_H__
#define __MMPOOLS_H__

#include "lispreader/pools.h"

/* TEMPLATE mmpools */

typedef struct _mathmap_pools_chunk_t {
    struct _mathmap_pools_chunk_t *next;
    double data[];		/* double for alignment */
} mathmap_pools_chunk_t;

typedef struct {
    int is_global;
    pools_t pools;			 /* only for local pools */
    mathmap_pools_chunk_t *chunks; /* only for global pools */
} mathmap_pools_t;

void mathmap_pools_init_global (mathmap_pools_t *pools);
void mathmap_pools_init_local (mathmap_pools_t *pools);

void mathmap_pools_reset (mathmap_pools_t *pools);

void mathmap_pools_free (mathmap_pools_t *pools);

void* _mathmap_pools_alloc (mathmap_pools_t *pools, size_t size);

static inline void*
mathmap_pools_alloc (mathmap_pools_t *pools, size_t size)
{
    if (pools->is_global)
	return _mathmap_pools_alloc(pools, size);
    return pools_alloc(&pools->pools, size);
}

/* END */

#endif
