/*
 * vars.h
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

#ifndef __VARS_H__
#define __VARS_H__

#define VAR_MAX_LENGTH     32

#include "tuples.h"

typedef struct _variable_t
{
    char name[VAR_MAX_LENGTH];
    tuple_info_t type;

    tuple_t value;

    struct _variable_t *next;
} variable_t;

variable_t* register_variable (const char *name, tuple_info_t type);
variable_t* lookup_variable (const char *name, tuple_info_t *type);
variable_t* new_temporary_variable (tuple_info_t type);

void clear_all_variables (void);

extern variable_t *firstVariable;

#endif
