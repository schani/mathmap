/*
 * internals.c
 *
 * MathMap
 *
 * Copyright (C) 1997-2000 Mark Probst
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

static internal_t *first = 0;

internal_t*
register_internal (const char *name, int number, int length)
{
    internal_t *internal = (internal_t*)malloc(sizeof(internal_t));

    strncpy(internal->name, name, MAX_INTERNAL_LENGTH);
    internal->name[MAX_INTERNAL_LENGTH] = '\0';
    internal->value.number = number;
    internal->value.length = length;
    internal->next = first;
    first = internal;

    return internal;
}

internal_t*
lookup_internal (const char *name, tuple_info_t *type)
{
    internal_t *internal;

    for (internal = first; internal != 0; internal = internal->next)
	if (strcmp(internal->name, name) == 0)
	{
	    *type = make_tuple_info(internal->value.number, internal->value.length);
	    internal->is_used = 1;
	    return internal;
	}

    return 0;
}

void
internals_clear_used (void)
{
    internal_t *internal;

    for (internal = first; internal != 0; internal = internal->next)
	internal->is_used = 0;
}
