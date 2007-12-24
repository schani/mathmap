/*
 * exprtree.c
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "exprtree.h"
#include "userval.h"
#include "builtins.h"
#include "tags.h"
#include "internals.h"
#include "macros.h"
#include "overload.h"
#include "mathmap.h"
#include "jump.h"

#define MAX_GENSYM_LEN	64

char error_string[1024];

static char*
gensym (char *buf)
{
    static int index = 0;

    sprintf(buf, "___tmp___%d___", index++);

    return buf;
}

static arg_decl_t*
make_arg_decl (int type, const char *name, const char *docstring, option_t *options)
{
    arg_decl_t *arg = (arg_decl_t*)malloc(sizeof(arg_decl_t));

    assert(arg != 0);

    arg->type = type;

    arg->name = strdup(name);
    assert(arg->name != 0);

    if (docstring != 0)
    {
	arg->docstring = strdup(docstring);
	assert(arg->docstring != 0);
    }
    else
	arg->docstring = 0;

    arg->options = options;

    arg->next = 0;

    return arg;
}

arg_decl_t*
make_simple_arg_decl (int type, const char *name, const char *docstring, option_t *options)
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
make_filter_arg_decl (const char *name, arg_decl_t *args, const char *docstring, option_t *options)
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
make_top_level_decl (int type, const char *name, const char *docstring)
{
    top_level_decl_t *top_level = (top_level_decl_t*)malloc(sizeof(top_level_decl_t));

    assert(top_level != 0);

    top_level->type = type;

    top_level->name = strdup(name);
    assert(top_level->name != 0);

    if (docstring != 0)
    {
	top_level->docstring = strdup(docstring);
	assert(top_level->docstring != 0);
    }
    else
	top_level->docstring = 0;

    return top_level;
}

top_level_decl_t*
make_filter_decl (const char *name, const char *docstring, arg_decl_t *args, exprtree *body, option_t *options)
{
    top_level_decl_t *top_level = make_top_level_decl(TOP_LEVEL_FILTER, name, docstring);

    top_level->v.filter.args = args;
    top_level->v.filter.options = options;
    top_level->v.filter.body = body;

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
make_option (const char *name, option_t *suboptions)
{
    option_t *option = (option_t*)malloc(sizeof(option_t));

    assert(option != 0);

    option->name = strdup(name);
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
make_int_limits (int min, int max)
{
    limits_t *limits = alloc_limits(LIMITS_INT);

    if (min >= max)
    {
	strcpy(error_string, "Lower limit must be less than upper limit");
	JUMP(1);
    }

    limits->v.integer.min = min;
    limits->v.integer.max = max;

    return limits;
}

limits_t*
make_float_limits (float min, float max)
{
    limits_t *limits = alloc_limits(LIMITS_FLOAT);

    if (min >= max)
    {
	strcpy(error_string, "Lower limit must be less than upper limit");
	JUMP(1);
    }

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
	    strcpy(error_string, "Only integers can be limits for an int argument");
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
	    strcpy(error_string, "Only integers and floats can be limits for a float argument");
	    JUMP(1);
	}

	arg_decl->v.floating.have_limits = 1;
	arg_decl->v.floating.min = min;
	arg_decl->v.floating.max = max;
	arg_decl->v.floating.default_value = min;
    }
    else
    {
	strcpy(error_string, "Limits applied to wrongly typed argument");
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
		strcpy(error_string, "Only integers can be defaults for an int argument");
		JUMP(1);
	    }

	    if (exprtree->val.int_const < arg_decl->v.integer.min
		|| exprtree->val.int_const > arg_decl->v.integer.max)
	    {
		strcpy(error_string, "Default value outside of bounds");
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
		    strcpy(error_string, "Only floats can be defaults for a float argument");
		    JUMP(1);
		}

		if (default_value < arg_decl->v.floating.min
		    || default_value > arg_decl->v.floating.max)
		{
		    strcpy(error_string, "Default value outside of bounds");
		    JUMP(1);
		}

		arg_decl->v.floating.default_value = default_value;
	    }
	    break;

	case ARG_TYPE_BOOL :
	    if (exprtree->type != EXPR_INT_CONST)
	    {
		strcpy(error_string, "Only integers can be defaults for a bool argument");
		JUMP(1);
	    }

	    arg_decl->v.boolean.default_value = exprtree->val.int_const ? 1 : 0;
	    break;

	default :
	    strcpy(error_string, "Default applied to wrongly typed argument");
	    JUMP(1);
    }
}

static exprtree*
alloc_exprtree (void)
{
    exprtree *tree = (exprtree*)malloc(sizeof(exprtree));

    assert(tree != 0);

    tree->next = 0;

    return tree;
}

exprtree*
make_int_number (int num)
{
    exprtree *tree = alloc_exprtree();

    tree->type = EXPR_INT_CONST;
    tree->val.int_const = num;

    tree->result = make_tuple_info(nil_tag_number, 1);

    return tree;
}

exprtree*
make_float_number (float num)
{
    exprtree *tree = alloc_exprtree();

    tree->type = EXPR_FLOAT_CONST;
    tree->val.float_const = num;

    tree->result = make_tuple_info(nil_tag_number, 1);

    return tree;
}

exprtree*
make_range (int first, int last)
{
    if (first > last)
    {
	sprintf(error_string, "Invalid range %d..%d.", first, last);
	JUMP(1);
    }
    else if (first == last)
	return make_int_number(first);
    else
    {
	exprtree *tree = make_int_number(first);

	tree->next = make_range(first + 1, last);

	return tree;
    }
}

static exprtree*
make_userval (userval_info_t *info, exprtree *args)
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
		sprintf(error_string, "Integer inputs take no arguments.");
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
	case USERVAL_GRADIENT :
	    if (exprlist_length(args) != 1)
	    {
		sprintf(error_string, "A curve or gradient takes one argument.");
		JUMP(1);
	    }
	    if (args->result.length != 1)
	    {
		sprintf(error_string, "The curve or gradient argument must have length 1.");
		JUMP(1);
	    }

	    if (info->type == USERVAL_CURVE)
	    {
		tree->result.number = nil_tag_number;
		tree->result.length = 1;
	    }
	    else
	    {
		tree->result.number = rgba_tag_number;
		tree->result.length = 4;
	    }
	    break;

	case USERVAL_IMAGE :
	    tree->result.number = image_tag_number;
	    tree->result.length = 1;
	    break;

	default :
	    assert(0);
    }

    tree->type = EXPR_USERVAL;
    tree->val.userval.info = info;

    if (info->type == USERVAL_IMAGE)
    {
	tree->val.userval.args = 0;

	if (exprlist_length(args) == 1 || exprlist_length(args) == 2)
	    return make_function("__origVal", exprlist_append(args, tree));

	if (exprlist_length(args) != 0)
	{
	    sprintf(error_string, "An image takes one or two arguments.");
	    JUMP(1);
	}
    }
    else
	tree->val.userval.args = args;

    return tree;
}

exprtree*
make_var_exprtree (variable_t *var, tuple_info_t info)
{
    exprtree *tree = alloc_exprtree();

    tree->type = EXPR_VARIABLE;
    tree->val.var = var;
    tree->result = info;

    return tree;
}

exprtree*
make_var (const char *name)
{
    tuple_info_t info;
    exprtree *tree = 0;

    if (lookup_internal(the_mathmap->current_filter->internals, name, 0) != 0)
    {
	tree = alloc_exprtree();

	tree->type = EXPR_INTERNAL;
	tree->val.internal = lookup_internal(the_mathmap->current_filter->internals, name, 0);
	tree->result = make_tuple_info(nil_tag_number, 1);
    }
    else if (lookup_variable_macro(name, &info) != 0)
    {
	macro_function_t function = lookup_variable_macro(name, &info);

	tree = function(0);
    }
    else if (lookup_userval(the_mathmap->current_filter->userval_infos, name) != 0)
    {
	userval_info_t *info = lookup_userval(the_mathmap->current_filter->userval_infos, name);

	tree = make_userval(info, 0);
    }
    else if (lookup_variable(the_mathmap->current_filter->variables, name, &info) != 0)
    {
	variable_t *var = lookup_variable(the_mathmap->current_filter->variables, name, &info);

	return make_var_exprtree(var, info);
    }
    else
    {
	sprintf(error_string, "Undefined variable %s.", name);
	JUMP(1);
    }

    return tree;
}

exprtree*
make_tuple_exprtree (exprtree *elems)
{
    exprtree *tree, *elem;
    int is_const = 1, length;

    length = 0;
    for (elem = elems; elem != 0; elem = elem->next)
    {
	++length;

	if (elem->result.length != 1)
	{
	    sprintf(error_string, "Tuples cannot contain tuples of length other than 1.");
	    JUMP(1);
	}

	if (elem->type != EXPR_TUPLE_CONST
	    || elem->type != EXPR_FLOAT_CONST)
	    is_const = 0;
    }

    tree = alloc_exprtree();

    if (is_const)
    {
	int i;

	tree->type = EXPR_TUPLE_CONST;
	tree->val.tuple_const = make_tuple(nil_tag_number, length);

	elem = elems;
	for (i = 0; i < length; ++i)
	{
	    if (elem->type == EXPR_TUPLE_CONST)
		tree->val.tuple_const->data[i] = elem->val.tuple_const->data[0];
	    else
		tree->val.tuple_const->data[i] = elem->val.float_const;
	    elem = elem->next;
	}
    }
    else
    {
	tree->type = EXPR_TUPLE;
	tree->val.tuple.length = length;
	tree->val.tuple.elems = elems;
    }
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
    if (subscripts->result.length == 1)
	tree->result = make_tuple_info(nil_tag_number, 1);
    else
	tree->result = make_tuple_info(tuple->result.number, subscripts->result.length);

    return tree;
}

exprtree*
make_cast (const char *tagname, exprtree *tuple)
{
    exprtree *tree = alloc_exprtree();
    int tagnum = tag_number_for_name(tagname);

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
    tree->result = make_tuple_info(tagnum, tuple->result.length);

    return tree;
}

exprtree*
make_convert (const char *tagname, exprtree *tuple)
{
    exprtree *tree = alloc_exprtree();

    tree->type = EXPR_CONVERT;
    tree->val.convert.tagnum = tag_number_for_name(tagname);
    tree->val.convert.tuple = tuple;
    tree->result = make_tuple_info(tree->val.convert.tagnum, tuple->result.length);

    return tree;
}

static filter_t*
lookup_filter (filter_t *filters, const char *name)
{
    filter_t *filter;

    for (filter = filters; filter != 0; filter = filter->next)
	if (strcmp(filter->decl->name, name) == 0)
	    return filter;

    return 0;
}

static exprtree*
make_filter_call (filter_t *filter, exprtree *args)
{
    userval_info_t *info;
    exprtree **argp;
    exprtree *closure;
    int num_args = exprlist_length(args);
    gboolean is_closure;
    exprtree *closure_args, *call_args;

    if (num_args < filter->num_uservals
	|| num_args >= filter->num_uservals + 3)
    {
	sprintf(error_string, "Filter %s takes %d to %d arguments but is called with %d.",
		filter->decl->name, filter->num_uservals, filter->num_uservals + 2, exprlist_length(args));
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
	    case USERVAL_IMAGE :
		if ((*argp)->result.length != 1)
		{
		    sprintf(error_string, "Can only pass tuples of length 1 as numbers or booleans.");
		    JUMP(1);
		}
		break;

	    default :
		sprintf(error_string, "Can only pass numbers and images to filters yet.");
		JUMP(1);
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

    if (is_closure)
	return closure;

    g_assert(call_args != NULL);

    if (call_args->result.length != 2
	|| (call_args->result.number != xy_tag_number
	    && call_args->result.number != ra_tag_number))
    {
	sprintf(error_string, "The coordinate argument to a filter must be a tuple of type xy:2 or ra:2.");
	JUMP(1);
    }

    if (call_args->result.number == ra_tag_number)
    {
	exprtree *next = call_args->next;

	call_args->next = NULL;
	call_args = make_function("toXY", call_args);
	call_args->next = next;
    }

    g_assert(call_args->result.length == 2 && call_args->result.number == xy_tag_number);

    argp = &call_args->next;

    if (*argp == NULL)
	*argp = make_var("t");
    else if ((*argp)->result.length != 1)
    {
	sprintf(error_string, "The time argument to a filter must be a tuple of length 1.");
	JUMP(1);
    }

    g_assert(exprlist_length(call_args) == 2);

    return make_function("__origVal", exprlist_append(call_args, closure));
}

static exprtree*
make_image_call (exprtree *image, exprtree *args)
{
    if (exprlist_length(args) != 1 && exprlist_length(args) != 2)
    {
	sprintf(error_string, "An image must be invoked with one or two arguments.");
	JUMP(1);
    }

    if (args->result.length != 2
	|| (args->result.number != xy_tag_number
	    && args->result.number != ra_tag_number))
    {
	sprintf(error_string, "The coordinate argument to an image must be of type xy:2 or ra:2.");
	JUMP(1);
    }
    if (args->result.number == ra_tag_number)
	args = make_function("toXY", args);

    if (args->next != NULL)
    {
	if (args->next->result.length != 1)
	{
	    sprintf(error_string, "The time argument to an image have length 1.");
	    JUMP(1);
	}
    }

    return make_function("__origVal", exprlist_append(args, image));
}

exprtree*
make_function (const char *name, exprtree *args)
{
    exprtree *tree = 0;
    exprtree *arg = args;
    function_arg_info_t *first, *last;
    overload_entry_t *entry;
    tuple_info_t info;

    if (args == 0)
    {
	sprintf(error_string, "Function %s must be called with at least one argument.", name);
	JUMP(1);
    }

    if (lookup_userval(the_mathmap->current_filter->userval_infos, name) != 0)
    {
	userval_info_t *info = lookup_userval(the_mathmap->current_filter->userval_infos, name);

	return make_userval(info, args);
    }

    if (lookup_filter(the_mathmap->filters, name) != 0)
    {
	filter_t *filter = lookup_filter(the_mathmap->filters, name);

	return make_filter_call(filter, args);
    }

    first = last = (function_arg_info_t*)malloc(sizeof(function_arg_info_t));
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
	    int is_constant = 1;

	    for (arg = args; arg != 0; arg = arg->next)
		if (arg->type != EXPR_TUPLE_CONST)
		{
		    is_constant = 0;
		    break;
		}

	    tree = alloc_exprtree();

	    tree->type = EXPR_FUNC;
	    tree->val.func.entry = entry;
	    tree->val.func.args = args;
	    tree->result = info;
	}
	else if (entry->type == OVERLOAD_MACRO)
	    tree = entry->v.macro(args);
	else
	    assert(0);
    }
    else if (lookup_variable(the_mathmap->current_filter->variables, name, &info))
    {
	variable_t *var = lookup_variable(the_mathmap->current_filter->variables, name, &info);

	if (info.number != image_tag_number
	    || info.length != 1)
	{
	    sprintf(error_string, "Variable %s is not an image and cannot be invoked.", name);
	    JUMP(1);
	}

	return make_image_call(make_var_exprtree(var, info), args);
    } else {
	sprintf(error_string, "Unable to resolve invocation of %s.", name);
	JUMP(1);
    }

    return tree;
}

exprtree*
make_sequence (exprtree *left, exprtree *right)
{
    exprtree *tree = alloc_exprtree();

    tree->type = EXPR_SEQUENCE;
    tree->val.operator.left = left;
    tree->val.operator.right = right;
    tree->result = right->result;

    return tree;
}

exprtree*
make_assignment (const char *name, exprtree *value)
{
    exprtree *tree = alloc_exprtree();
    variable_t *var = lookup_variable(the_mathmap->current_filter->variables, name, &tree->result);

    // FIXME: check whether the variable name is an internal, var macro or user val

    if (var == 0)
    {
	var = register_variable(&the_mathmap->current_filter->variables, name, value->result);
	tree->result = value->result;
    }

    if (tree->result.number != value->result.number || tree->result.length != value->result.length)
    {
	sprintf(error_string, "Variable %s is being assigned two different types.", name);
	JUMP(1);
    }

    tree->type = EXPR_ASSIGNMENT;
    tree->val.assignment.var = var;
    tree->val.assignment.value = value;

    return tree;
}

exprtree*
make_sub_assignment (char *name, exprtree *subscripts, exprtree *value)
{
    exprtree *tree = alloc_exprtree();
    tuple_info_t info;
    variable_t *var = lookup_variable(the_mathmap->current_filter->variables, name, &info);

    if (var == 0)
    {
	sprintf(error_string, "Undefined variable %s.", name);
	JUMP(1);
    }

    if (subscripts->result.length != value->result.length)
    {
	sprintf(error_string, "Lhs does not match rhs in sub assignment.");
	JUMP(1);
    }

    tree->type = EXPR_SUB_ASSIGNMENT;
    tree->val.sub_assignment.var = var;
    tree->val.sub_assignment.subscripts = subscripts;
    tree->val.sub_assignment.value = value;
    tree->result = value->result;

    return tree;
}

exprtree*
make_if_then (exprtree *condition, exprtree *consequent)
{
    exprtree *tree = alloc_exprtree();

    if (condition->result.length != 1)
    {
	sprintf(error_string, "Condition to if statement must have length 1.");
	JUMP(1);
    }

    tree->type = EXPR_IF_THEN;
    tree->val.ifExpr.condition = condition;
    tree->val.ifExpr.consequent = consequent;
    tree->result = consequent->result;

    return tree;
}

exprtree*
make_if_then_else (exprtree *condition, exprtree *consequent, exprtree *alternative)
{
    exprtree *tree = alloc_exprtree();

    if (condition->result.length != 1)
    {
	sprintf(error_string, "Condition to if statement must have length 1.");
	JUMP(1);
    }
    if (consequent->result.number != alternative->result.number
	|| consequent->result.length != alternative->result.length)
    {
	sprintf(error_string, "Consequent and alternative must have the same type in if statement.");
	JUMP(1);
    }

    tree->type = EXPR_IF_THEN_ELSE;
    tree->val.ifExpr.condition = condition;
    tree->val.ifExpr.consequent = consequent;
    tree->val.ifExpr.alternative = alternative;
    tree->result = consequent->result;

    return tree;
}

exprtree*
make_while (exprtree *invariant, exprtree *body)
{
    exprtree *tree = alloc_exprtree();

    tree->type = EXPR_WHILE;
    tree->val.whileExpr.invariant = invariant;
    tree->val.whileExpr.body = body;
    tree->result = make_tuple_info(nil_tag_number, 1);

    return tree;
}

exprtree*
make_do_while (exprtree *body, exprtree *invariant)
{
    exprtree *tree = alloc_exprtree();

    tree->type = EXPR_DO_WHILE;
    tree->val.whileExpr.invariant = invariant;
    tree->val.whileExpr.body = body;
    tree->result = make_tuple_info(nil_tag_number, 1);

    return tree;
}

void
check_for_start (exprtree *start)
{
    if (start->result.length != 1)
    {
	sprintf(error_string, "The start and end of a for loop interval must be tuples of length 1.");
	JUMP(1);
    }
}

exprtree*
make_for (const char *counter_name, exprtree *counter_init, exprtree *start, exprtree *end, exprtree *body)
{
    if (start->result.length != 1 || end->result.length != 1 || start->result.number != end->result.number)
    {
	sprintf(error_string, "The start and end of a for loop interval must be tuples of the same tag and length 1.");
	JUMP(1);
    }
    else
    {
	char end_name_buf[MAX_GENSYM_LEN];
	char *end_name = gensym(end_name_buf);
	exprtree *end_init = make_assignment(end_name, end);
	exprtree *init = make_sequence(counter_init, end_init);
	exprtree *inc = make_assignment(counter_name, make_function("__add",
								    exprlist_append(make_var(counter_name),
										    make_int_number(1))));
	exprtree *invariant = make_function("__lessequal", exprlist_append(make_var(counter_name), make_var(end_name)));

	return make_sequence(init, make_while(invariant, make_sequence(body, inc)));
    }
}

void
free_exprtree (exprtree *tree)
{
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
