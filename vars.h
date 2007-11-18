/*
 * vars.h
 *
 * MathMap
 *
 * Copyright (C) 1997-2007 Mark Probst
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

#include "tuples.h"

struct _compvar_t;
struct _value_t;

typedef struct _variable_t
{
    char *name;
    tuple_info_t type;
    int index;

    struct _compvar_t **compvar; /* only for the compiler */
    int *last_index; /* ditto */

    struct _variable_t *next;
} variable_t;

variable_t* register_variable (variable_t **vars, const char *name, tuple_info_t type);
variable_t* lookup_variable (variable_t *vars, const char *name, tuple_info_t *type);
variable_t* new_temporary_variable (variable_t **vars, tuple_info_t type);

void compiler_reset_variables (variable_t *vars);

tuple_t** instantiate_variables (variable_t *vars);
void free_variables (variable_t *vars);

#endif
