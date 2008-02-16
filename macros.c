/*
 * macros.c
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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#include "internals.h"
#include "tags.h"
#include "exprtree.h"
#include "overload.h"
#include "mathmap.h"

#include "macros.h"

static var_macro_t *first = 0;

void
register_variable_macro (const char *name,  macro_function_t function, tuple_info_t info)
{
    var_macro_t *macro = (var_macro_t*)malloc(sizeof(var_macro_t));

    strncpy(macro->name, name, MAX_MACRO_LENGTH);
    macro->name[MAX_MACRO_LENGTH] = '\0';
    macro->info = info;
    macro->function = function;

    macro->next = first;
    first = macro;
}

macro_function_t
lookup_variable_macro (const char *name, tuple_info_t *info)
{
    var_macro_t *macro;

    for (macro = first; macro != 0; macro = macro->next)
	if (strcmp(macro->name, name) == 0)
	{
	    if (info != NULL)
		*info = macro->info;
	    return macro->function;
	}

    return 0;
}

exprtree*
macro_var_xy (exprtree *args)
{
    return make_cast("xy", make_tuple_exprtree(exprlist_append(make_var("x"), make_var("y"))));
}

exprtree*
macro_var_ra (exprtree *args)
{
    return make_cast("ra", make_tuple_exprtree(exprlist_append(make_var("r"), make_var("a"))));
}

exprtree*
macro_var_big_xy (exprtree *args)
{
    return make_cast("xy", make_tuple_exprtree(exprlist_append(make_var("X"), make_var("Y"))));
}

exprtree*
macro_var_big_wh (exprtree *args)
{
    return make_cast("xy", make_tuple_exprtree(exprlist_append(make_var("W"), make_var("H"))));
}

exprtree*
macro_var_big_i (exprtree *args)
{
    return make_cast("ri", make_tuple_exprtree(exprlist_append(make_int_number(0), make_int_number(1))));
}

exprtree*
macro_var_pi (exprtree *args)
{
    return make_float_number(M_PI);
}

exprtree*
macro_var_e (exprtree *args)
{
    return make_float_number(M_E);
}

exprtree*
macro_func_origValImage (exprtree *args)
{
    variable_t *tmpvar = new_temporary_variable(&the_mathmap->current_filter->v.mathmap.variables, args->result);

    return make_sequence(make_assignment(tmpvar->name, args),
			 make_function("__origVal", exprlist_append(make_function("toXY", make_var(tmpvar->name)),
								    exprlist_append(make_var("t"),
										    args->next))));
}

exprtree*
macro_func_origValImageFrame (exprtree *args)
{
    variable_t *tmpvar = new_temporary_variable(&the_mathmap->current_filter->v.mathmap.variables, args->result);

    return make_sequence(make_assignment(tmpvar->name, args),
			 make_function("__origVal", exprlist_append(make_function("toXY", make_var(tmpvar->name)),
								    exprlist_append(args->next,
										    args->next->next))));
}

void
init_macros (void)
{
    register_variable_macro("xy", macro_var_xy, make_tuple_info(xy_tag_number, 2));
    register_variable_macro("ra", macro_var_ra, make_tuple_info(ra_tag_number, 2));
    register_variable_macro("XY", macro_var_big_xy, make_tuple_info(xy_tag_number, 2));
    register_variable_macro("WH", macro_var_big_wh, make_tuple_info(xy_tag_number, 2));

    register_variable_macro("I", macro_var_big_i, make_tuple_info(ri_tag_number, 2));
    register_variable_macro("pi", macro_var_pi, make_tuple_info(nil_tag_number, 1));
    register_variable_macro("e", macro_var_e, make_tuple_info(nil_tag_number, 1));

    register_overloaded_macro("__origVal", "((rgba 4) (xy 2) (image 1))", macro_func_origValImage);
    register_overloaded_macro("__origVal", "((rgba 4) (ra 2) (image 1))", macro_func_origValImage);
    register_overloaded_macro("__origVal", "((rgba 4) (ra 2) (nil 1) (image 1))", macro_func_origValImageFrame);
}
