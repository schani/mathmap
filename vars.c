/*
 * vars.c
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

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "vars.h"

variable_t*
alloc_variable (tuple_info_t type)
{
    variable_t *var;
    int i;

    var = (variable_t*)malloc(sizeof(variable_t));

    var->name = 0;
    var->type = type;
    var->next = 0;

    var->compvar = (struct _compvar_t**)malloc(sizeof(struct _compvar_t*) * type.length);
    assert(var->compvar != 0);

    var->last_index = (int*)malloc(sizeof(int) * type.length);
    assert(var->last_index != 0);

    for (i = 0; i < type.length; ++i)
    {
	var->compvar[i] = 0;
	var->last_index[i] = 0;
    }

    return var;
}

variable_t*
register_variable (variable_t **vars, const char *name, tuple_info_t type)
{
    variable_t *var = alloc_variable(type);

    var->name = strdup(name);
    assert(var->name != 0);

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
    static char buf[64];

    variable_t *var = alloc_variable(type);

    sprintf(buf, "tmp____%d", ++num);
    var->name = strdup(buf);
    assert(var->name != 0);

    var->index = 0;
    while (*vars != 0)
    {
	vars = &(*vars)->next;
	++var->index;
    }
    *vars = var;

    return var;
}

void
free_variables (variable_t *vars)
{
    while (vars != 0)
    {
	variable_t *next = vars->next;

	free(vars->name);
	free(vars->compvar);
	free(vars->last_index);
	free(vars);

	vars = next;
    }
}
