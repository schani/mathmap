/*
 * pools.h
 *
 * lispreader
 *
 * Copyright (C) 2002-2007 Mark Probst
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

#ifndef __POOLS_H__
#define __POOLS_H__

#include <stdlib.h>

/* these settings allow a pools to grow to up to 16 GB (last pool 8GB) */
#define GRANULARITY                sizeof(long)
#define FIRST_POOL_SIZE            ((size_t)2048)
#define NUM_POOLS                  20

typedef struct
{
    int active_pool;
    size_t fill_ptr;
    long *pools[NUM_POOLS];
} pools_t;

int init_pools (pools_t *pools);
void free_pools (pools_t *pools);

#ifdef __GNUC__
void* _pools_alloc (pools_t *pools, size_t size);

static inline void*
pools_alloc (pools_t *pools, size_t size)
{
    void *p;
    size_t padded_size = (size + GRANULARITY - 1) / GRANULARITY;

    if (pools->fill_ptr + padded_size >= (FIRST_POOL_SIZE << pools->active_pool))
	return _pools_alloc(pools, size);

    p = pools->pools[pools->active_pool] + pools->fill_ptr;
    pools->fill_ptr += padded_size;

    return p;
}
#else
void* pools_alloc (pools_t *pools, size_t size);
#endif

#ifdef __GNUC__
static inline void
reset_pools (pools_t *pools)
{
    pools->active_pool = 0;
    pools->fill_ptr = 0;
}
#else
void reset_pools (pools_t *pools);
#endif

#endif
