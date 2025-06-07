/*
 * exprtree.c
 *
 * MathMap
 *
 * Copyright (C) 1997-2009 Mark Probst
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "exprtree.h"
#include "userval.h"
#include "builtins/builtins.h"
#include "tags.h"
#include "internals.h"
#include "macros.h"
#include "overload.h"
#include "mathmap.h"
#include "jump.h"

#define MAX_GENSYM_LEN	64

char error_string[1024];
scanner_region_t error_region;

static char*
gensym (char *buf)
{
    static int index = 0;

    sprintf(buf, "___tmp___%d___", index++);

    return buf;
}

static arg_decl_t*
make_arg_decl (int type, scanner_ident_t *name, scanner_ident_t *docstring, option_t *options)
{
    arg_decl_t *arg = (arg_decl_t*)malloc(sizeof(arg_decl_t));

    assert(arg != 0);

    arg->type = type;

    arg->name = strdup(name->str);
    assert(arg->name != 0);

    if (docstring != NULL)
    {
	arg->docstring = strdup(docstring->str);
	assert(arg->docstring != NULL);
    }
    else
	arg->docstring = NULL;

    arg->options = options;

    arg->region = name->region;

    arg->next = 0;

    return arg;
}

arg_decl_t*
make_simple_arg_decl (int type, scanner_ident_t *name, scanner_ident_t *docstring, option_t *options)
{
    arg_decl_t *arg_decl = make_arg_decl(type, name, docstring, options);

    if (type == ARG_TYPE_INT)
    {
	arg_decl->v.integer.have_limits = 0;
	arg_decl->v.integer.default_value = 0;
    }
    else if (type == ARG_TYPE_FLOAT)
    {
	arg_decl->v.floating.have_limits = 0;
	arg_decl->v.floating.default_value = 0.0;
    }
    else if (type == ARG_TYPE_BOOL)
	arg_decl->v.boolean.default_value = 0;

    return arg_decl;
}

arg_decl_t*
make_filter_arg_decl (scanner_ident_t *name, arg_decl_t *args, scanner_ident_t *docstring, option_t *options)
{
    arg_decl_t *arg = make_arg_decl(ARG_TYPE_FILTER, name, docstring, options);

    arg->v.filter.args = args;

    return arg;
}

arg_decl_t*
arg_decl_list_append (arg_decl_t *list1, arg_decl_t *list2)
{
    arg_decl_t *list = list1;

    if (list1 == 0)
	return list2;

    while (list->next != 0)
	list = list->next;

    list->next = list2;

    return list1;
}

void
free_arg_decls (arg_decl_t *list)
{
    while (list != 0)
    {
	arg_decl_t *next = list->next;

	free(list->name);
	if (list->docstring != 0)
	    free(list->docstring);
	if (list->type == ARG_TYPE_FILTER)
	    free_arg_decls(list->v.filter.args);

	free(list);

	list = next;
    }
}

static top_level_decl_t*
make_top_level_decl (int type, scanner_ident_t *name, scanner_ident_t *docstring)
{
    top_level_decl_t *top_level = (top_level_decl_t*)malloc(sizeof(top_level_decl_t));

    assert(top_level != NULL);

    top_level->type = type;

    top_level->name = strdup(name->str);
    assert(top_level->name != NULL);

    top_level->region = name->region;

    if (docstring != NULL)
    {
	top_level->docstring = strdup(docstring->str);
	assert(top_level->docstring != NULL);
    }
    else
	top_level->docstring = NULL;

    return top_level;
}

top_level_decl_t*
make_filter_decl (scanner_ident_t *name, scanner_ident_t *docstring, arg_decl_t *args, option_t *options)
{
    top_level_decl_t *top_level = make_top_level_decl(TOP_LEVEL_FILTER, name, docstring);

    top_level->v.filter.args = args;
    top_level->v.filter.options = options;
    top_level->v.filter.body = NULL; /* will be filled in by parser */

    return top_level;
}

void
free_top_level_decl (top_level_decl_t *list)
{
    free(list->name);
    if (list->docstring != 0)
	free(list->docstring);

    if (list->type == TOP_LEVEL_FILTER)
    {
	free_arg_decls(list->v.filter.args);
	free_exprtree(list->v.filter.body);
    }
    else
	assert(0);

    free(list);
}

option_t*
make_option (scanner_ident_t *name, option_t *suboptions)
{
    option_t *option = (option_t*)malloc(sizeof(option_t));

    assert(option != 0);

    option->name = strdup(name->str);
    assert(option->name != 0);

    option->suboptions = suboptions;

    option->next = 0;

    return option;
}

option_t*
options_append (option_t *o1, option_t *o2)
{
    if (o1 == 0)
	return o2;
    else
    {
	option_t *o = o1;

	while (o1->next != 0)
	    o1 = o1->next;

	o1->next = o2;

	return o;
    }
}

option_t*
find_option_with_name (option_t *options, const char *name)
{
    while (options != 0)
    {
	if (strcmp(options->name, name) == 0)
	    return options;

	options = options->next;
    }

    return 0;
}

static limits_t*
alloc_limits (int type)
{
    limits_t *limits = (limits_t*)malloc(sizeof(limits_t));

    assert(limits != 0);

    limits->type = type;

    return limits;
}

limits_t*
make_int_limits (exprtree *min_tree, exprtree *max_tree)
{
    scanner_region_t region = scanner_region_merge (min_tree->region, max_tree->region);
    limits_t *limits;
    int min, max;

    g_assert(min_tree->type == EXPR_INT_CONST && max_tree->type == EXPR_INT_CONST);

    min = min_tree->val.int_const;
    max = max_tree->val.int_const;

    if (min >= max)
    {
	strcpy(error_string, _("Lower limit must be less than upper limit"));
	error_region = region;
	JUMP(1);
    }

    limits = alloc_limits(LIMITS_INT);

    limits->region = region;
    limits->v.integer.min = min;
    limits->v.integer.max = max;

    return limits;
}

limits_t*
make_float_limits (exprtree *min_tree, exprtree *max_tree)
{
    scanner_region_t region = scanner_region_merge (min_tree->region, max_tree->region);
    limits_t *limits;
    float min, max;

    g_assert((min_tree->type == EXPR_INT_CONST || min_tree->type == EXPR_FLOAT_CONST) &&
	     (max_tree->type == EXPR_INT_CONST || max_tree->type == EXPR_FLOAT_CONST));

    if (min_tree->type == EXPR_INT_CONST)
	min = (float)min_tree->val.int_const;
    else
	min = min_tree->val.float_const;

    if (max_tree->type == EXPR_INT_CONST)
	max = (float)max_tree->val.int_const;
    else
	max = max_tree->val.float_const;

    if (min >= max)
    {
	strcpy(error_string, _("Lower limit must be less than upper limit"));
	error_region = region;
	JUMP(1);
    }

    limits = alloc_limits(LIMITS_FLOAT);

    limits->region = region;
    limits->v.floating.min = min;
    limits->v.floating.max = max;

    return limits;
}

void
free_limits (limits_t *limits)
{
    free(limits);
}

void
apply_limits_to_arg_decl (arg_decl_t *arg_decl, limits_t *limits)
{
    if (arg_decl->type == ARG_TYPE_INT)
    {
	if (limits->type != LIMITS_INT)
	{
	    strcpy(error_string, _("Only integers can be limits for an int argument"));
	    error_region = limits->region;
	    JUMP(1);
	}

	arg_decl->v.integer.have_limits = 1;
	arg_decl->v.integer.min = limits->v.integer.min;
	arg_decl->v.integer.max = limits->v.integer.max;
	arg_decl->v.integer.default_value = arg_decl->v.integer.min;
    }
    else if (arg_decl->type == ARG_TYPE_FLOAT)
    {
	float min = 0.0, max = 0.0;

	if (limits->type == LIMITS_INT)
	{
	    min = (float)limits->v.integer.min;
	    max = (float)limits->v.integer.max;
	}
	else if (limits->type == LIMITS_FLOAT)
	{
	    min = limits->v.floating.min;
	    max = limits->v.floating.max;
	}
	else
	{
	    strcpy(error_string, _("Only integers and floats can be limits for a float argument"));
	    error_region = limits->region;
	    JUMP(1);
	}

	arg_decl->v.floating.have_limits = 1;
	arg_decl->v.floating.min = min;
	arg_decl->v.floating.max = max;
	arg_decl->v.floating.default_value = min;
    }
    else
    {
	strcpy(error_string, _("Limits applied to wrongly typed argument"));
	error_region = limits->region;
	JUMP(1);
    }
}

void
apply_default_to_arg_decl (arg_decl_t *arg_decl, exprtree *exprtree)
{
    switch (arg_decl->type)
    {
	case ARG_TYPE_INT :
	    if (exprtree->type != EXPR_INT_CONST)
	    {
		strcpy(error_string, _("Only integers can be defaults for an int argument"));
		error_region = exprtree->region;
		JUMP(1);
	    }

	    if (exprtree->val.int_const < arg_decl->v.integer.min
		|| exprtree->val.int_const > arg_decl->v.integer.max)
	    {
		strcpy(error_string, _("Default value outside of bounds"));
		error_region = exprtree->region;
		JUMP(1);
	    }

	    arg_decl->v.integer.default_value = exprtree->val.int_const;
	    break;

	case ARG_TYPE_FLOAT :
	    {
		float default_value = 0.0;

		if (exprtree->type == EXPR_INT_CONST)
		    default_value = (float)exprtree->val.int_const;
		else if (exprtree->type == EXPR_FLOAT_CONST)
		    default_value = exprtree->val.float_const;
		else
		{
		    strcpy(error_string, _("Only floats can be defaults for a float argument"));
		    error_region = exprtree->region;
		    JUMP(1);
		}

		if (default_value < arg_decl->v.floating.min
		    || default_value > arg_decl->v.floating.max)
		{
		    strcpy(error_string, _("Default value outside of bounds"));
		    error_region = exprtree->region;
		    JUMP(1);
		}

		arg_decl->v.floating.default_value = default_value;
	    }
	    break;

	case ARG_TYPE_BOOL :
	    if (exprtree->type != EXPR_INT_CONST)
	    {
		strcpy(error_string, _("Only integers can be defaults for a bool argument"));
		error_region = exprtree->region;
		JUMP(1);
	    }

	    arg_decl->v.boolean.default_value = exprtree->val.int_const ? 1 : 0;
	    break;

	default :
	    strcpy(error_string, _("Default applied to wrongly typed argument"));
	    error_region = exprtree->region;
	    JUMP(1);
    }
}

static exprtree*
alloc_exprtree (void)
{
    exprtree *tree = (exprtree*)malloc(sizeof(exprtree));

    assert(tree != 0);

    tree->region = scanner_null_region;
    tree->next = 0;

    return tree;
}

exprtree*
make_int_number (int num, scanner_region_t region)
{
    exprtree *tree = alloc_exprtree();

    tree->type = EXPR_INT_CONST;
    tree->val.int_const = num;

    tree->result = make_tuple_info(nil_tag_number, 1);

    tree->region = region;

    return tree;
}

exprtree*
make_float_number (float num, scanner_region_t region)
{
    exprtree *tree = alloc_exprtree();

    tree->type = EXPR_FLOAT_CONST;
    tree->val.float_const = num;

    tree->result = make_tuple_info(nil_tag_number, 1);

    tree->region = region;

    return tree;
}

static exprtree*
make_range_list (int first, int last, scanner_region_t region)
{
    exprtree *tree;

    if (first > last)
	return NULL;

    tree = make_int_number(first, region);
    tree->next = make_range_list(first + 1, last, region);

    return tree;
}

exprtree*
make_range (exprtree *first_expr, exprtree *last_expr)
{
    scanner_region_t region = scanner_region_merge (first_expr->region, last_expr->region);
    int first, last;

    g_assert(first_expr->type == EXPR_INT_CONST && last_expr->type == EXPR_INT_CONST);

    first = first_expr->val.int_const;
    last = last_expr->val.int_const;

    if (first > last)
    {
	sprintf(error_string, _("Invalid range %d..%d."), first, last);
	error_region = region;
	JUMP(1);
    }

    return make_range_list(first, last, region);
}

static scanner_region_t
exprlist_region (exprtree *list)
{
    if (list == NULL)
	return scanner_null_region;
    return scanner_region_merge(list->region, exprlist_region(list->next));
}

exprtree*
make_function_from_string (const char *name, exprtree *args, scanner_region_t name_region)
{
    scanner_ident_t *ident = scanner_make_ident(name_region, name);
    exprtree *tree = make_function(ident, args);

    free(ident);

    return tree;
}

exprtree*
make_var_from_string (const char *name)
{
    scanner_ident_t *ident = scanner_make_ident(scanner_null_region, name);
    exprtree *tree = make_var(ident);

    free(ident);

    return tree;
}

static exprtree*
make_userval (userval_info_t *info, exprtree *args, scanner_region_t region)
{
    exprtree *tree = alloc_exprtree();

    switch (info->type)
    {
	case USERVAL_INT_CONST :
	case USERVAL_FLOAT_CONST :
	case USERVAL_BOOL_CONST :
	case USERVAL_COLOR :
	    if (exprlist_length(args) != 0)
	    {
		sprintf(error_string, _("Number, bool and color inputs take no arguments."));
		error_region = region;
		JUMP(1);
	    }

	    if (info->type == USERVAL_COLOR)
	    {
		tree->result.number = rgba_tag_number;
		tree->result.length = 4;
	    }
	    else
	    {
		tree->result.number = nil_tag_number;
		tree->result.length = 1;
	    }
	    break;

	case USERVAL_CURVE :
	    tree->result.number = curve_tag_number;
	    tree->result.length = 1;
	    break;

	case USERVAL_GRADIENT :
	    tree->result.number = gradient_tag_number;
	    tree->result.length = 1;
	    break;

	case USERVAL_IMAGE :
	    tree->result.number = image_tag_number;
	    tree->result.length = 1;
	    break;

	default :
	    g_assert_not_reached();
    }

    tree->type = EXPR_USERVAL;
    tree->val.userval.info = info;
    tree->region = scanner_region_merge(region, exprlist_region(args));

    switch (info->type)
    {
	case USERVAL_CURVE :
	    if (exprlist_length(args) == 1)
		return make_function_from_string("__applyCurve", exprlist_append(tree, args), region);

	    if (exprlist_length(args) != 0)
	    {
		sprintf(error_string, _("A curve takes one argument."));
		error_region = region;
		JUMP(1);
	    }
	    break;

	case USERVAL_GRADIENT :
	    if (exprlist_length(args) == 1)
		return make_function_from_string("__applyGradient", exprlist_append(tree, args), region);

	    if (exprlist_length(args) != 0)
	    {
		sprintf(error_string, _("A gradient takes one argument."));
		error_region = region;
		JUMP(1);
	    }
	    break;

	case USERVAL_IMAGE :
	    if (exprlist_length(args) == 1 || exprlist_length(args) == 2)
		return make_function_from_string("__origVal", exprlist_append(args, tree), region);

	    if (exprlist_length(args) != 0)
	    {
		sprintf(error_string, _("An image takes one or two arguments."));
		error_region = region;
		JUMP(1);
	    }
	    break;

	default :
	    g_assert(args == NULL);
	    break;
    }

    return tree;
}

static exprtree*
make_var_exprtree (variable_t *var, tuple_info_t info, scanner_region_t region)
{
    exprtree *tree = alloc_exprtree();

    tree->type = EXPR_VARIABLE;
    tree->val.var = var;
    tree->result = info;
    tree->region = region;

    return tree;
}

exprtree*
make_var (scanner_ident_t *name_ident)
{
    char *name = name_ident->str;
    scanner_region_t region = name_ident->region;
    tuple_info_t info;
    exprtree *tree = 0;

    if (lookup_internal(the_mathmap->current_filter->v.mathmap.internals, name, 0) != 0)
    {
	tree = alloc_exprtree();

	tree->type = EXPR_INTERNAL;
	tree->val.internal = lookup_internal(the_mathmap->current_filter->v.mathmap.internals, name, 0);
	tree->result = make_tuple_info(nil_tag_number, 1);
	tree->region = region;
    }
    else if (lookup_variable_macro(name, &info) != 0)
    {
	macro_function_t function = lookup_variable_macro(name, &info);

	tree = function(0);
	tree->region = name_ident->region;
    }
    else if (lookup_userval(the_mathmap->current_filter->userval_infos, name) != 0)
    {
	userval_info_t *info = lookup_userval(the_mathmap->current_filter->userval_infos, name);

	tree = make_userval(info, 0, region);
    }
    else if (lookup_variable(the_mathmap->current_filter->v.mathmap.variables, name, &info) != 0)
    {
	variable_t *var = lookup_variable(the_mathmap->current_filter->v.mathmap.variables, name, &info);

	return make_var_exprtree(var, info, region);
    }
    else
    {
	sprintf(error_string, _("Undefined variable %s."), name);
	error_region = region;
	JUMP(1);
    }

    return tree;
}

exprtree*
make_tuple_exprtree (exprtree *elems)
{
    exprtree *tree, *elem;
    int length;

    length = 0;
    for (elem = elems; elem != 0; elem = elem->next)
    {
	++length;

	if (elem->result.length != 1)
	{
	    sprintf(error_string, _("Tuples cannot contain tuples of length other than 1."));
	    error_region = elem->region;
	    JUMP(1);
	}
    }

    tree = alloc_exprtree();

    tree->type = EXPR_TUPLE;
    tree->val.tuple.length = length;
    tree->val.tuple.elems = elems;

    tree->region = exprlist_region(elems);

    tree->result = make_tuple_info(nil_tag_number, length);

    return tree;
}

exprtree*
make_select (exprtree *tuple, exprtree *subscripts)
{
    exprtree *tree = alloc_exprtree();

    tree->type = EXPR_SELECT;
    tree->val.select.tuple = tuple;
    tree->val.select.subscripts = subscripts;
    tree->region = scanner_region_merge(tuple->region, exprlist_region(subscripts));
    if (subscripts->result.length == 1)
	tree->result = make_tuple_info(nil_tag_number, 1);
    else
	tree->result = make_tuple_info(tuple->result.number, subscripts->result.length);

    return tree;
}

exprtree*
make_cast (scanner_ident_t *tagname, exprtree *tuple)
{
    exprtree *tree = alloc_exprtree();
    int tagnum = tag_number_for_name(tagname->str);

    if (tuple->type == EXPR_TUPLE_CONST)
    {
	tree->type = EXPR_TUPLE_CONST;
	tree->val.tuple_const = copy_tuple(tuple->val.tuple_const);
	tree->val.tuple_const->number = tagnum;
    }
    else
    {
	tree->type = EXPR_CAST;
	tree->val.cast.tagnum = tagnum;
	tree->val.cast.tuple = tuple;
    }
    tree->region = scanner_region_merge(tagname->region, tuple->region);
    tree->result = make_tuple_info(tagnum, tuple->result.length);

    return tree;
}

exprtree*
make_convert (scanner_ident_t *tagname_ident, exprtree *tuple)
{
    exprtree *tree = alloc_exprtree();

    tree->type = EXPR_CONVERT;
    tree->val.convert.tagnum = tag_number_for_name(tagname_ident->str);
    tree->val.convert.tuple = tuple;
    tree->result = make_tuple_info(tree->val.convert.tagnum, tuple->result.length);
    tree->region = scanner_region_merge(tagname_ident->region, tuple->region);

    return tree;
}

filter_t*
lookup_filter (filter_t *filters, const char *name)
{
    filter_t *filter;

    for (filter = filters; filter != 0; filter = filter->next)
	if (strcmp(filter->name, name) == 0)
	    return filter;

    return 0;
}

static exprtree*
make_filter_call (filter_t *filter, exprtree *args, scanner_region_t filter_region)
{
    scanner_region_t region = scanner_region_merge(filter_region, exprlist_region(args));
    userval_info_t *info;
    exprtree **argp;
    exprtree *closure;
    int num_args = exprlist_length(args);
    gboolean is_closure;
    exprtree *closure_args, *call_args;

    if (num_args < filter->num_uservals
	|| num_args >= filter->num_uservals + 3)
    {
	sprintf(error_string, _("Filter %s takes %d to %d arguments but is called with %d."),
		filter->name, filter->num_uservals, filter->num_uservals + 2, exprlist_length(args));
	error_region = region;
	JUMP(1);
    }

    is_closure = (num_args == filter->num_uservals);

    for (info = filter->userval_infos, argp = &args;
	 info != 0;
	 info = info->next, argp = &(*argp)->next)
    {
	switch (info->type)
	{
	    case USERVAL_INT_CONST :
	    case USERVAL_FLOAT_CONST :
	    case USERVAL_BOOL_CONST :
	    case USERVAL_CURVE :
	    case USERVAL_GRADIENT :
	    case USERVAL_IMAGE :
		if ((*argp)->result.length != 1)
		{
		    sprintf(error_string, _("Can only pass tuples of length 1 as numbers, booleans, curves, gradients, or images."));
		    error_region = (*argp)->region;
		    JUMP(1);
		}
		break;

	    case USERVAL_COLOR :
		if ((*argp)->result.number != rgba_tag_number || (*argp)->result.length != 4)
		{
		    sprintf(error_string, _("Can only pass tuples of type rgba:4 as colors."));
		    error_region = (*argp)->region;
		    JUMP(1);
		}
		break;

	    default :
		g_assert_not_reached();
	}
    }

    if (filter->num_uservals == 0)
	closure_args = NULL;
    else
	closure_args = args;
    call_args = *argp;
    *argp = NULL;

    if (is_closure)
	g_assert(call_args == 0);

    closure = alloc_exprtree();

    closure->type = EXPR_FILTER_CLOSURE;
    closure->val.filter_closure.filter = filter;
    closure->val.filter_closure.args = closure_args;

    closure->result.number = image_tag_number;
    closure->result.length = 1;

    closure->region = region;

    if (is_closure)
	return closure;

    g_assert(call_args != NULL);

    if (call_args->result.length != 2
	|| (call_args->result.number != xy_tag_number
	    && call_args->result.number != ra_tag_number))
    {
	sprintf(error_string, _("The coordinate argument to a filter must be a tuple of type xy:2 or ra:2."));
	error_region = call_args->region;
	JUMP(1);
    }

    if (call_args->result.number == ra_tag_number)
    {
	exprtree *next = call_args->next;

	call_args->next = NULL;
	call_args = make_function_from_string("toXY", call_args, call_args->region);
	call_args->next = next;
    }

    g_assert(call_args->result.length == 2 && call_args->result.number == xy_tag_number);

    argp = &call_args->next;

    if (*argp == NULL)
	*argp = make_var_from_string("t");
    else if ((*argp)->result.length != 1)
    {
	sprintf(error_string, _("The time argument to a filter must be a tuple of length 1."));
	error_region = (*argp)->region;
	JUMP(1);
    }

    g_assert(exprlist_length(call_args) == 2);

    return make_function_from_string("__origVal", exprlist_append(call_args, closure), filter_region);
}

static exprtree*
make_image_call (exprtree *image, exprtree *args, scanner_region_t region)
{
    exprtree *tree;
    scanner_ident_t *ident;

    if (exprlist_length(args) != 1 && exprlist_length(args) != 2)
    {
	sprintf(error_string, _("An image must be invoked with one or two arguments."));
	error_region = region;
	JUMP(1);
    }

    if (args->result.length != 2
	|| (args->result.number != xy_tag_number
	    && args->result.number != ra_tag_number))
    {
	sprintf(error_string, _("The coordinate argument to an image must be of type xy:2 or ra:2."));
	error_region = region;
	JUMP(1);
    }
    if (args->result.number == ra_tag_number)
	args = make_function_from_string("toXY", args, args->region);

    if (args->next != NULL)
    {
	if (args->next->result.length != 1)
	{
	    sprintf(error_string, _("The time argument to an image have length 1."));
	    error_region = region;
	    JUMP(1);
	}
    }

    ident = scanner_make_ident(scanner_null_region, "__origVal");
    tree = make_function(ident, exprlist_append(args, image));
    free(ident);

    return tree;
}

static struct { const char *op, *func; } binop_table[] = {
    { "+", "__add" },
    { "-", "__sub" },
    { "*", "__mul" },
    { "/", "__div" },
    { "%", "__mod" },
    { "^", "__pow" },
    { "==", "__equal" },
    { "<", "__less" },
    { ">", "__greater" },
    { "<=", "__lessequal" },
    { ">=", "__greaterequal" },
    { "!=", "__notequal" },
    { "||", "__or" },
    { "&&", "__and" },
    { "xor", "__xor" },
    { NULL, NULL }
};

static struct { const char *op, *func; } unop_table[] = {
    { "-", "__neg" },
    { "!", "__not" },
    { NULL, NULL }
};

static const char*
get_func_name_for_op (const char *name)
{
    int i;

    for (i = 0; binop_table[i].op != NULL; ++i)
	if (strcmp(binop_table[i].op, name) == 0)
	    return binop_table[i].func;
    return NULL;
}

static const char*
get_func_name_for_unary_op (const char *name)
{
    int i;

    for (i = 0; unop_table[i].op != NULL; ++i)
	if (strcmp(unop_table[i].op, name) == 0)
	    return unop_table[i].func;
    return NULL;
}

static const char*
get_op_name_for_func (const char *name)
{
    int i;
    for (i = 0; binop_table[i].op != NULL; ++i)
	if (strcmp(binop_table[i].func, name) == 0)
	    return binop_table[i].op;
    for (i = 0; unop_table[i].op != NULL; ++i)
	if (strcmp(unop_table[i].func, name) == 0)
	    return unop_table[i].op;
    return NULL;
}

exprtree*
make_function (scanner_ident_t *name_ident, exprtree *args)
{
    char *name = name_ident->str;
    scanner_region_t name_region = name_ident->region;
    exprtree *tree = 0;
    exprtree *arg;
    function_arg_info_t *first, *last;
    overload_entry_t *entry;
    tuple_info_t info;

    if (lookup_userval(the_mathmap->current_filter->userval_infos, name) != 0)
    {
	userval_info_t *info = lookup_userval(the_mathmap->current_filter->userval_infos, name);

	return make_userval(info, args, name_region);
    }

    if (lookup_filter(the_mathmap->filters, name) != 0)
    {
	filter_t *filter = lookup_filter(the_mathmap->filters, name);

	return make_filter_call(filter, args, name_region);
    }

    first = last = (function_arg_info_t*)malloc(sizeof(function_arg_info_t));
    arg = args;
    last->info = arg->result;
    last->next = 0;
    while (arg->next != 0)
    {
	arg = arg->next;
	last = last->next = (function_arg_info_t*)malloc(sizeof(function_arg_info_t));
	last->info = arg->result;
	last->next = 0;
    }

    entry = resolve_function_call(name, first, &info);
    if (entry != 0)
    {
	if (entry->type == OVERLOAD_BUILTIN)
	{
	    for (arg = args; arg != 0; arg = arg->next)
		if (arg->type != EXPR_TUPLE_CONST)
		    break;

	    tree = alloc_exprtree();

	    tree->type = EXPR_FUNC;
	    tree->val.func.entry = entry;
	    tree->val.func.args = args;
	    tree->result = info;
	}
	else if (entry->type == OVERLOAD_MACRO)
	    tree = entry->v.macro(args);
	else
	    g_assert_not_reached();

	tree->region = scanner_region_merge(name_region, exprlist_region(args));
    }
    else if (lookup_variable(the_mathmap->current_filter->v.mathmap.variables, name, &info))
    {
	variable_t *var = lookup_variable(the_mathmap->current_filter->v.mathmap.variables, name, &info);

	if (info.number != image_tag_number
	    || info.length != 1)
	{
	    sprintf(error_string, _("Variable %s is not an image and cannot be invoked."), name);
	    error_region = name_region;
	    JUMP(1);
	}

	return make_image_call(make_var_exprtree(var, info, name_region), args, name_region);
    } else {
	const char *op_name = get_op_name_for_func(name);
	if (op_name)
	    sprintf(error_string, _("Unable to resolve invocation of operator `%s'."), op_name);
	else
	    sprintf(error_string, _("Unable to resolve invocation of function `%s'."), name);
	error_region = name_region;
	JUMP(1);
    }

    return tree;
}

exprtree*
make_operator_function (scanner_ident_t *name, exprtree *left, exprtree *right)
{
    const char *func_name = get_func_name_for_op(name->str);
    scanner_ident_t *ident;
    exprtree *tree;

    g_assert(func_name != NULL);

    ident = scanner_make_ident(name->region, func_name);
    tree = make_function(ident, exprlist_append(left, right));
    free(ident);

    return tree;
}

exprtree*
make_unary_operator_function (scanner_ident_t *name, exprtree *arg)
{
    const char *func_name = get_func_name_for_unary_op(name->str);
    scanner_ident_t *ident;
    exprtree *tree;

    g_assert(func_name != NULL);

    ident = scanner_make_ident(name->region, func_name);
    tree = make_function(ident, arg);
    free(ident);

    return tree;
}

exprtree*
make_sequence (exprtree *left, exprtree *right)
{
    exprtree *tree = alloc_exprtree();

    tree->type = EXPR_SEQUENCE;
    tree->val.op.left = left;
    tree->val.op.right = right;
    tree->result = right->result;
    tree->region = scanner_region_merge(left->region, right->region);

    return tree;
}

exprtree*
make_assignment (scanner_ident_t *name_ident, exprtree *value)
{
    char *name = name_ident->str;
    scanner_region_t region = name_ident->region;
    exprtree *tree = alloc_exprtree();
    variable_t *var = lookup_variable(the_mathmap->current_filter->v.mathmap.variables, name, &tree->result);

    if (var == NULL)
    {
	if (lookup_internal(the_mathmap->current_filter->v.mathmap.internals, name, TRUE) != NULL
	    || lookup_variable_macro(name, NULL) != NULL)
	{
	    sprintf(error_string, _("Cannot assign to internal variable `%s'."), name);
	    error_region = region;
	    JUMP(1);
	}
	if (lookup_userval(the_mathmap->current_filter->userval_infos, name) != NULL)
	{
	    sprintf(error_string, _("Cannot assign to filter argument `%s'."), name);
	    error_region = region;
	    JUMP(1);
	}

	var = register_variable(&the_mathmap->current_filter->v.mathmap.variables, name, value->result);
	tree->result = value->result;
    }

    if (tree->result.number != value->result.number || tree->result.length != value->result.length)
    {
	sprintf(error_string, _("Variable %s is being assigned two different types."), name);
	error_region = region;
	JUMP(1);
    }

    tree->type = EXPR_ASSIGNMENT;
    tree->val.assignment.var = var;
    tree->val.assignment.value = value;
    tree->region = region;

    return tree;
}

exprtree*
make_sub_assignment (scanner_ident_t *name_ident, exprtree *subscripts, exprtree *value)
{
    char *name = name_ident->str;
    scanner_region_t region = scanner_region_merge(name_ident->region,
						   scanner_region_merge(exprlist_region(subscripts), value->region));
    exprtree *tree = alloc_exprtree();
    tuple_info_t info;
    variable_t *var = lookup_variable(the_mathmap->current_filter->v.mathmap.variables, name, &info);

    if (var == 0)
    {
	sprintf(error_string, _("Undefined variable %s."), name);
	error_region = name_ident->region;
	JUMP(1);
    }

    if (subscripts->result.length != value->result.length)
    {
	sprintf(error_string, _("Lhs does not match rhs in sub assignment."));
	error_region = region;
	JUMP(1);
    }

    tree->type = EXPR_SUB_ASSIGNMENT;
    tree->val.sub_assignment.var = var;
    tree->val.sub_assignment.subscripts = subscripts;
    tree->val.sub_assignment.value = value;
    tree->result = value->result;
    tree->region = region;

    return tree;
}

exprtree*
make_if_then (exprtree *condition, exprtree *consequent)
{
    exprtree *tree = alloc_exprtree();

    if (condition->result.length != 1)
    {
	sprintf(error_string, _("Condition to if statement must have length 1."));
	error_region = condition->region;
	JUMP(1);
    }

    tree->type = EXPR_IF_THEN;
    tree->val.ifExpr.condition = condition;
    tree->val.ifExpr.consequent = consequent;
    tree->result = consequent->result;
    tree->region = scanner_region_merge(condition->region, consequent->region);

    return tree;
}

exprtree*
make_if_then_else (exprtree *condition, exprtree *consequent, exprtree *alternative)
{
    exprtree *tree = alloc_exprtree();

    if (condition->result.length != 1)
    {
	sprintf(error_string, _("Condition to if statement must have length 1."));
	error_region = condition->region;
	JUMP(1);
    }
    if (consequent->result.number != alternative->result.number
	|| consequent->result.length != alternative->result.length)
    {
	sprintf(error_string, _("Consequent and alternative must have the same type in if statement."));
	error_region = scanner_region_merge(consequent->region, alternative->region);
	JUMP(1);
    }

    tree->type = EXPR_IF_THEN_ELSE;
    tree->val.ifExpr.condition = condition;
    tree->val.ifExpr.consequent = consequent;
    tree->val.ifExpr.alternative = alternative;
    tree->result = consequent->result;
    tree->region = scanner_region_merge(condition->region,
					scanner_region_merge(consequent->region, alternative->region));

    return tree;
}

exprtree*
make_while (exprtree *invariant, exprtree *body)
{
    exprtree *tree = alloc_exprtree();

    if (invariant->result.length != 1)
    {
	sprintf(error_string, _("Invariant of while loop must have length 1."));
	error_region = invariant->region;
	JUMP(1);
    }
    tree->type = EXPR_WHILE;
    tree->val.whileExpr.invariant = invariant;
    tree->val.whileExpr.body = body;
    tree->result = make_tuple_info(nil_tag_number, 1);
    tree->region = scanner_region_merge(invariant->region, body->region);

    return tree;
}

exprtree*
make_do_while (exprtree *body, exprtree *invariant)
{
    exprtree *tree = alloc_exprtree();

    if (invariant->result.length != 1)
    {
	sprintf(error_string, _("Invariant of do-while loop must have length 1."));
	error_region = invariant->region;
	JUMP(1);
    }
    tree->type = EXPR_DO_WHILE;
    tree->val.whileExpr.invariant = invariant;
    tree->val.whileExpr.body = body;
    tree->result = make_tuple_info(nil_tag_number, 1);
    tree->region = scanner_region_merge(invariant->region, body->region);

    return tree;
}

void
check_for_start (exprtree *start)
{
    if (start->result.length != 1)
    {
	sprintf(error_string, _("The start and end of a for loop interval must be tuples of length 1."));
	error_region = start->region;
	JUMP(1);
    }
}

exprtree*
make_for (scanner_ident_t *counter_name_ident, exprtree *counter_init, exprtree *start, exprtree *end, exprtree *body)
{
    scanner_region_t region = counter_name_ident->region;

    if (start->result.length != 1 || end->result.length != 1 || start->result.number != end->result.number)
    {
	sprintf(error_string, _("The start and end of a for loop interval must be tuples of the same tag and length 1."));
	error_region = region;
	JUMP(1);
    }
    else
    {
	char end_name_buf[MAX_GENSYM_LEN];
	char *end_name = gensym(end_name_buf);
	scanner_ident_t *end_name_ident = scanner_make_ident(scanner_null_region, end_name);
	exprtree *end_init = make_assignment(end_name_ident, end);
	exprtree *init = make_sequence(counter_init, end_init);
	exprtree *inc = make_assignment(counter_name_ident,
					make_function_from_string("__add",
								  exprlist_append(make_var(counter_name_ident),
										  make_int_number(1, scanner_null_region)),
								  scanner_null_region));
	exprtree *invariant = make_function_from_string("__lessequal", exprlist_append(make_var(counter_name_ident),
										       make_var(end_name_ident)),
							scanner_null_region);

	free(end_name_ident);

	return make_sequence(init, make_while(invariant, make_sequence(body, inc)));
    }
}

void
free_exprtree (exprtree *tree)
{
    /* FIXME: implement */
}

int
exprlist_length (exprtree *list)
{
    int l;

    l = 0;
    while (list != 0)
    {
	++l;
	list = list->next;
    }

    return l;
}

exprtree*
exprlist_append (exprtree *list1, exprtree *list2)
{
    exprtree *tree = list1;

    if (list1 == 0)
	return list2;

    while (tree->next != 0)
	tree = tree->next;

    tree->next = list2;

    return list1;
}

int
is_exprtree_single_const (exprtree *tree, int *int_val, float *float_val)
{
    if (tree->type == EXPR_INT_CONST || tree->type == EXPR_FLOAT_CONST || tree->type == EXPR_TUPLE_CONST)
    {
	if (tree->type == EXPR_INT_CONST)
	{
	    if (int_val != 0)
		*int_val = tree->val.int_const;
	    if (float_val != 0)
		*float_val = (float)tree->val.int_const;
	}
	else if (tree->type == EXPR_FLOAT_CONST)
	{
	    if (int_val != 0)
		*int_val = (int)tree->val.float_const;
	    if (float_val != 0)
		*float_val = tree->val.float_const;
	}
	else
	{
	    if (int_val != 0)
		*int_val = (int)tree->val.tuple_const->data[0];
	    if (float_val != 0)
		*float_val = tree->val.tuple_const->data[0];
	}

	return 1;
    }
    else if (tree->type == EXPR_TUPLE && tree->result.length == 1)
	return is_exprtree_single_const(tree->val.tuple.elems, int_val, float_val);
    else if (tree->type == EXPR_FUNC
	     && strcmp(tree->val.func.entry->name, "__neg") == 0)
    {
	int result = is_exprtree_single_const(tree->val.func.args, int_val, float_val);

	if (result != 0 && int_val != 0)
	    *int_val = -*int_val;
	if (result != 0 && float_val != 0)
	    *float_val = -*float_val;

	return result;
    }

    return 0;
}
