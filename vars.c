/*
 * vars.c
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

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "vars.h"

variable_t*
register_variable (variable_t **vars, const char *name, tuple_info_t type)
{
    variable_t *var;

    var = (variable_t*)malloc(sizeof(variable_t));
    assert(strlen(name) < VAR_MAX_LENGTH);
    strcpy(var->name, name);
    var->type = type;
    var->next = 0;

    var->index = 0;
    while (*vars != 0)
    {
	vars = &(*vars)->next;
	++var->index;
    }
    *vars = var;

    return var;
}

variable_t*
lookup_variable (variable_t *vars, const char *name, tuple_info_t *type)
{
    variable_t *var;

    for (var = vars; var != 0; var = var->next)
	if (strcmp(name, var->name) == 0)
	{
	    *type = var->type;
	    return var;
	}

    return 0;
}

variable_t*
new_temporary_variable (variable_t **vars, tuple_info_t type)
{
    static int num = 0;

    int i;

    variable_t *var = (variable_t*)malloc(sizeof(variable_t));

    sprintf(var->name, "tmp____%d", ++num);
    var->type = type;
    var->next = 0;

    for (i = 0; i < type.length; ++i)
	var->current[i] = 0;

    var->index = 0;
    while (*vars != 0)
    {
	vars = &(*vars)->next;
	++var->index;
    }
    *vars = var;

    return var;
}

tuple_t*
instantiate_variables (variable_t *vars)
{
    int n, i;
    variable_t *var;
    tuple_t *tuples;

    n = 0;
    for (var = vars; var != 0; var = var->next)
	++n;

    tuples = (tuple_t*)malloc(n * sizeof(tuple_t));

    for (i = 0, var = vars; i < n; ++i, var = var->next)
    {
	tuples[i].number = var->type.number;
	tuples[i].length = var->type.length;
    }

    return tuples;
}

void
free_variables (variable_t *vars)
{
    while (vars != 0)
    {
	variable_t *next = vars->next;

	free(vars);

	vars = next;
    }
}
