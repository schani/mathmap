/*
 * mmpools.c
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

#include <glib.h>

#include "mmpools.h"

void
mathmap_pools_init_global (mathmap_pools_t *pools)
{
    pools->is_global = 1;
    pools->chunks = NULL;
}

void
mathmap_pools_init_local (mathmap_pools_t *pools)
{
    pools->is_global = 0;
    init_pools(&pools->pools);
}

void
mathmap_pools_reset (mathmap_pools_t *pools)
{
    g_assert(!pools->is_global);
    reset_pools(&pools->pools);
}

void
mathmap_pools_free (mathmap_pools_t *pools)
{
    if (pools->is_global)
    {
	mathmap_pools_chunk_t *chunk = pools->chunks;
	int num_chunks = 0;

	while (chunk != NULL)
	{
	    mathmap_pools_chunk_t *next = chunk->next;
	    free(chunk);
	    chunk = next;
	    ++num_chunks;
	}

	//g_print("%d chunks freed\n", num_chunks);
    }
    else
	free_pools(&pools->pools);
}

void*
_mathmap_pools_alloc (mathmap_pools_t *pools, size_t size)
{
    if (pools->is_global)
    {
	mathmap_pools_chunk_t *chunk = malloc(sizeof(mathmap_pools_chunk_t) + size);
	do
	{
	    chunk->next = pools->chunks;
	} while (!g_atomic_pointer_compare_and_exchange((gpointer*)&pools->chunks, chunk->next, chunk));
	return chunk->data;
    }

    return pools_alloc(&pools->pools, size);
}
