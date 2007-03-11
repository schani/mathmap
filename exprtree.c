/*
 * exprtree.c
 *
 * MathMap
 *
 * Copyright (C) 1997-2005 Mark Probst
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

char error_string[1024];

top_level_decl_t *the_top_level_decls = 0;

static arg_decl_t*
make_arg_decl (int type, const char *name, const char *docstring)
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

    arg->next = 0;

    return arg;
}

arg_decl_t*
make_simple_arg_decl (const char *type_name, const char *name, const char *docstring)
{
    static struct { const char *name; int type; } types[] =
	{
	    { "int", ARG_TYPE_INT },
	    { "float", ARG_TYPE_FLOAT },
	    { "bool", ARG_TYPE_BOOL },
	    { "color", ARG_TYPE_COLOR },
	    { "gradient", ARG_TYPE_GRADIENT },
	    { "curve", ARG_TYPE_CURVE },
	    { "image", ARG_TYPE_IMAGE },
	    { 0 }
	};

    int i;

    for (i = 0; types[i].name != 0; ++i)
	if (strcmp(types[i].name, type_name) == 0)
	    break;

    if (types[i].name == 0)
    {
	sprintf(error_string, "Unknown type %s.", type_name);
	JUMP(1);
    }
    else
    {
	int type = types[i].type;
	arg_decl_t *arg_decl = make_arg_decl(type, name, docstring);

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

	return arg_decl;
    }

    return 0;
}

arg_decl_t*
make_filter_arg_decl (const char *name, arg_decl_t *args, const char *docstring)
{
    arg_decl_t *arg = make_arg_decl(ARG_TYPE_FILTER, name, docstring);

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

    top_level->next = 0;

    return top_level;
}

top_level_decl_t*
make_filter (const char *name, const char *docstring, arg_decl_t *args, exprtree *body)
{
    top_level_decl_t *top_level = make_top_level_decl(TOP_LEVEL_FILTER, name, docstring);

    top_level->v.filter.args = args;
    top_level->v.filter.body = body;

    return top_level;
}

top_level_decl_t*
top_level_list_append (top_level_decl_t *list1, top_level_decl_t *list2)
{
    top_level_decl_t *list = list1;

    if (list1 == 0)
	return list2;

    while (list->next != 0)
	list = list->next;

    list->next = list2;

    return list1;
}

void
free_top_level_decls (top_level_decl_t *list)
{
    while (list != 0)
    {
	top_level_decl_t *next = list->next;

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

	list = next;
    }
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
    else if (arg_decl->type ==ARG_TYPE_FLOAT)
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
    if (arg_decl->type == ARG_TYPE_INT)
    {
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
    }
    else if (arg_decl->type == ARG_TYPE_FLOAT)
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
    else
    {
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
	    if (exprlist_length(args) != 1)
	    {
		sprintf(error_string, "An image takes one argument.");
		JUMP(1);
	    }

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

	return make_function("origVal", exprlist_append(args, tree));
    }
    else
	tree->val.userval.args = args;

    return tree;
}

exprtree*
make_var (const char *name)
{
    tuple_info_t info;
    exprtree *tree = 0;

    if (lookup_internal(the_mathmap->internals, name, 0) != 0)
    {
	tree = alloc_exprtree();

	tree->type = EXPR_INTERNAL;
	tree->val.internal = lookup_internal(the_mathmap->internals, name, 0);
	tree->result = make_tuple_info(nil_tag_number, 1);
    }
    else if (lookup_variable_macro(name, &info) != 0)
    {
	macro_function_t function = lookup_variable_macro(name, &info);

	tree = function(0);
    }
    else if (lookup_userval(the_mathmap->userval_infos, name) != 0)
    {
	userval_info_t *info = lookup_userval(the_mathmap->userval_infos, name);

	tree = make_userval(info, 0);
    }
    else if (lookup_variable(the_mathmap->variables, name, &info) != 0)
    {
	tree = alloc_exprtree();

	tree->type = EXPR_VARIABLE;
	tree->val.var = lookup_variable(the_mathmap->variables, name, &tree->result);
	tree->result = info;
    }
    else
    {
	sprintf(error_string, "Undefined variable %s.", name);
	JUMP(1);
    }

    return tree;
}

exprtree*
make_tuple (exprtree *elems)
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

    if (length > MAX_TUPLE_LENGTH)
    {
	sprintf(error_string, "Tuples cannot be longer than %d elements.", MAX_TUPLE_LENGTH);
	JUMP(1);
    }

    tree = alloc_exprtree();

    if (is_const)
    {
	int i;

	tree->type = EXPR_TUPLE_CONST;
	tree->val.tuple_const.number = nil_tag_number;
	tree->val.tuple_const.length = length;

	elem = elems;
	for (i = 0; i < length; ++i)
	{
	    if (elem->type == EXPR_TUPLE_CONST)
		tree->val.tuple_const.data[i] = elem->val.tuple_const.data[0];
	    else
		tree->val.tuple_const.data[i] = elem->val.float_const;
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
	tree->val.tuple_const = tuple->val.tuple_const;
	tree->val.tuple_const.number = tagnum;
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

    if (lookup_userval(the_mathmap->userval_infos, name) != 0)
    {
	userval_info_t *info = lookup_userval(the_mathmap->userval_infos, name);

	return make_userval(info, args);
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

	    /* FIXME: can only do if function is pure */
	    /*
	      if (is_constant && 0)
	      {
	      tuple_t stack[32];
	      mathmap_t mathmap;
	      mathmap_invocation_t invocation;

	      tuple_t *result;
	      int i;

	      invocation.mathmap = &mathmap;

	      mathmap.expression = make_postfix(tree, &mathmap.exprlen);
	      invocation.stack = stack;

	      printf("foldings constants:\n");
	      output_postfix(mathmap.expression, mathmap.exprlen);
	      result = eval_postfix(&invocation);

	      tree->type = EXPR_TUPLE_CONST;
	      for (i = 0; i < tree->result.length; ++i)
	      tree->val.tuple_const.data[i] = result->data[i];
	      tree->val.tuple_const.number = tree->result.number;
	      tree->val.tuple_const.length = tree->result.length;

	      free(mathmap.expression);
	      }
	    */
	}
	else if (entry->type == OVERLOAD_MACRO)
	    tree = entry->v.macro(args);
	else
	    assert(0);
    }
    else
    {
	sprintf(error_string, "Unable to resolve invocation of %s.", name);
	JUMP(1);
    }

    /*
      while (first != 0)
      {
      function_arg_info_t *next = first->next;

      free(first);
      first = next;
      }
    */

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
make_assignment (char *name, exprtree *value)
{
    exprtree *tree = alloc_exprtree();
    variable_t *var = lookup_variable(the_mathmap->variables, name, &tree->result);

    // FIXME: check whether the variable name is an internal, var macro or user val

    if (var == 0)
    {
	var = register_variable(&the_mathmap->variables, name, value->result);
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
    variable_t *var = lookup_variable(the_mathmap->variables, name, &info);

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
		*int_val = (int)tree->val.tuple_const.data[0];
	    if (float_val != 0)
		*float_val = tree->val.tuple_const.data[0];
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
