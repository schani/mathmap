/* -*- c -*- */

/*
 * internals.h
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

#ifndef __INTERNALS_H__
#define __INTERNALS_H__

#include "tuples.h"

#define MAX_INTERNAL_LENGTH    63

typedef struct _internal_t
{
    char name[MAX_INTERNAL_LENGTH + 1];
    tuple_info_t type;
    int index;

    unsigned int is_used : 1;
    unsigned int can_be_precomputed : 1;

    struct _internal_t *next;
} internal_t;

internal_t* register_internal (internal_t **internals, const char *name, tuple_info_t type, int can_be_precomputed);
internal_t* lookup_internal (internal_t *internals, const char *name, tuple_info_t *type);

tuple_t* instantiate_internals (internal_t *internals);
void free_internals (internal_t *internals);

#endif
