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

static scanner_ident_t*
get_xy_ident (void)
{
    static scanner_ident_t *ident = NULL;
    if (ident == NULL)
	ident = scanner_make_ident(scanner_null_region, "xy");
    return ident;
}

static scanner_ident_t*
get_ra_ident (void)
{
    static scanner_ident_t *ident = NULL;
    if (ident == NULL)
	ident = scanner_make_ident(scanner_null_region, "ra");
    return ident;
}

static scanner_ident_t*
get_ri_ident (void)
{
    static scanner_ident_t *ident = NULL;
    if (ident == NULL)
	ident = scanner_make_ident(scanner_null_region, "ri");
    return ident;
}

exprtree*
macro_var_xy (exprtree *args)
{
    return make_cast(get_xy_ident(), make_tuple_exprtree(exprlist_append(make_var_from_string("x"), make_var_from_string("y"))));
}

exprtree*
macro_var_ra (exprtree *args)
{
    return make_cast(get_ra_ident(), make_tuple_exprtree(exprlist_append(make_var_from_string("r"), make_var_from_string("a"))));
}

exprtree*
macro_var_big_xy (exprtree *args)
{
    return make_cast(get_xy_ident(), make_tuple_exprtree(exprlist_append(make_var_from_string("X"), make_var_from_string("Y"))));
}

exprtree*
macro_var_big_wh (exprtree *args)
{
    return make_cast(get_xy_ident(), make_tuple_exprtree(exprlist_append(make_var_from_string("W"), make_var_from_string("H"))));
}

exprtree*
macro_var_big_i (exprtree *args)
{
    return make_cast(get_ri_ident(), make_tuple_exprtree(exprlist_append(make_int_number(0, scanner_null_region),
									 make_int_number(1, scanner_null_region))));
}

exprtree*
macro_var_pi (exprtree *args)
{
    return make_float_number(M_PI, scanner_null_region);
}

exprtree*
macro_var_e (exprtree *args)
{
    return make_float_number(M_E, scanner_null_region);
}

exprtree*
macro_func_origValImage (exprtree *args)
{
    variable_t *tmpvar = new_temporary_variable(&the_mathmap->current_filter->v.mathmap.variables, args->result);
    scanner_ident_t *ident = scanner_make_ident(scanner_null_region, tmpvar->name);
    exprtree *tree;

    tree = make_sequence(make_assignment(ident, args),
			 make_function_from_string("__origVal",
						   exprlist_append(make_function_from_string("toXY",
											     make_var_from_string(tmpvar->name),
											     scanner_null_region),
								   exprlist_append(make_var_from_string("t"),
										   args->next)),
						   scanner_null_region));

    free(ident);

    return tree;
}

exprtree*
macro_func_origValImageFrame (exprtree *args)
{
    variable_t *tmpvar = new_temporary_variable(&the_mathmap->current_filter->v.mathmap.variables, args->result);
    scanner_ident_t *ident = scanner_make_ident(scanner_null_region, tmpvar->name);
    exprtree *tree;

    tree = make_sequence(make_assignment(ident, args),
			 make_function_from_string("__origVal",
						   exprlist_append(make_function_from_string("toXY",
											     make_var_from_string(tmpvar->name),
											     scanner_null_region),
								   exprlist_append(args->next,
										   args->next->next)),
						   scanner_null_region));

    free(ident);

    return tree;
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
