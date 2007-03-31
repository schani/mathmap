/*
 * internals.c
 *
 * MathMap
 *
 * Copyright (C) 1997-2002 Mark Probst
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

#include <stdlib.h>
#include <string.h>

#include "tags.h"
#include "internals.h"

internal_t*
register_internal (internal_t **internals, const char *name, int const_type)
{
    internal_t *internal = (internal_t*)malloc(sizeof(internal_t));

    strncpy(internal->name, name, MAX_INTERNAL_LENGTH);
    internal->name[MAX_INTERNAL_LENGTH] = '\0';
    internal->const_type = const_type;
    internal->is_used = 0;
    internal->next = 0;

    internal->index = 0;
    while (*internals != 0)
    {
	internals = &(*internals)->next;
	++internal->index;
    }
    *internals = internal;

    return internal;
}

internal_t*
lookup_internal (internal_t *internals, const char *name, int dont_touch)
{
    internal_t *internal;

    for (internal = internals; internal != 0; internal = internal->next)
	if (strcmp(internal->name, name) == 0)
	{
	    if (!dont_touch)
		internal->is_used = 1;
	    return internal;
	}

    return 0;
}

/*
tuple_t*
instantiate_internals (internal_t *internals)
{
    int n, i;
    internal_t *internal;
    tuple_t *tuples;

    n = 0;
    for (internal = internals; internal != 0; internal = internal->next)
	++n;

    tuples = (tuple_t*)malloc(n * sizeof(tuple_t));

    for (i = 0, internal = internals; i < n; ++i, internal = internals->next)
    {
	tuples[i].number = nil_tag_number;
	tuples[i].length = 1;
    }

    return tuples;
}
*/

void
free_internals (internal_t *internals)
{
    while (internals != 0)
    {
	internal_t *next = internals->next;

	free(internals);

	internals = next;
    }
}

int
number_of_internals (internal_t *internals)
{
    int i;

    while (internals != 0)
    {
	++i;

	internals = internals->next;
    }

    return i;
}
