#include <stdio.h>
#include <stdlib.h>
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

extern double currentX,
    currentY,
    currentR,
    currentA;
extern int imageWidth,
    imageHeight;
extern int intersamplingEnabled;

exprtree*
alloc_exprtree (void)
{
    exprtree *tree = (exprtree*)malloc(sizeof(exprtree));

    tree->next = 0;

    return tree;
}

exprtree*
make_number (float num)
{
    exprtree *tree = alloc_exprtree();

    tree->type = EXPR_TUPLE_CONST;
    tree->val.tuple_const.number = nil_tag_number;
    tree->val.tuple_const.length = 1;
    tree->val.tuple_const.data[0] = num;

    tree->result = make_tuple_info(nil_tag_number, 1);

    return tree;
}

exprtree*
make_range (int first, int last)
{
    if (first > last)
    {
	sprintf(error_string, "Invalid range %d..%d.", first, last);
	JUMP(0);
    }
    else if (first == last)
	return make_number(first);
    else
    {
	exprtree *tree = make_number(first);

	tree->next = make_range(first + 1, last);

	return tree;
    }
}

exprtree*
make_var (const char *name)
{
    tuple_info_t info;
    exprtree *tree = 0;

    if (lookup_internal(name, &info) != 0)
    {
	tree = alloc_exprtree();

	tree->type = EXPR_INTERNAL;
	tree->val.internal = lookup_internal(name, &tree->result);
	tree->result = info;
    }
    else if (lookup_variable_macro(name, &info) != 0)
    {
	macro_function_t function = lookup_variable_macro(name, &info);

	tree = function(0);
    }
    else if (lookup_variable(name, &info) != 0)
    {
	tree = alloc_exprtree();

	tree->type = EXPR_VARIABLE;
	tree->val.var = lookup_variable(name, &tree->result);
	tree->result = info;
    }
    else
    {
	sprintf(error_string, "Undefined variable %s.", name);
	JUMP(0);
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
	    JUMP(0);
	}

	if (elem->type != EXPR_TUPLE_CONST)
	    is_const = 0;
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
	    tree->val.tuple_const.data[i] = elem->val.tuple_const.data[0];
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
    exprtree *tree = 0,
	*arg = args;
    function_arg_info_t *first,
	*last;
    overload_entry_t *entry;
    tuple_info_t info;

    if (args == 0)
    {
	sprintf(error_string, "Function %s must be called with at least one argument.", name);
	JUMP(0);
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

	    if (is_constant && !entry->v.builtin.sidefx)
	    {
		tuple_t *result;
		int i;

		make_postfix(tree);
		printf("foldings constants:\n");
		output_postfix();
		result = eval_postfix();

		tree->type = EXPR_TUPLE_CONST;
		for (i = 0; i < tree->result.length; ++i)
		    tree->val.tuple_const.data[i] = result->data[i];
		tree->val.tuple_const.number = tree->result.number;
		tree->val.tuple_const.length = tree->result.length;
	    }
	}
	else if (entry->type == OVERLOAD_MACRO)
	    tree = entry->v.macro(args);
	else
	    assert(0);
    }
    else
    {
	sprintf(error_string, "Unable to resolve invocation of %s.", name);
	JUMP(0);
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
make_userval (const char *type, const char *name, exprtree *args)
{
    userval_t *userval;
    exprtree *tree = alloc_exprtree();

    if (strcmp(type, "user_slider") == 0)
    {
	float min, max;

	if (exprlist_length(args) != 2)
	{
	    sprintf(error_string, "user_slider takes 2 arguments.");
	    JUMP(0);
	}
	if (args->type != EXPR_TUPLE_CONST || args->val.tuple_const.length != 1
	    || args->next->type != EXPR_TUPLE_CONST || args->next->val.tuple_const.length != 1)
	{
	    sprintf(error_string, "user_slider min and max must be constants with length 1.");
	    JUMP(0);
	}

	min = args->val.tuple_const.data[0];
	max = args->next->val.tuple_const.data[0];

	userval = register_slider(name, min, max);

	if (userval == 0)
	{
	    sprintf(error_string, "user_slider %s has a mismatch.", name);
	    JUMP(0);
	}

	tree->result.number = nil_tag_number;
	tree->result.length = 1;
    }
    else if (strcmp(type, "user_bool") == 0)
    {
	if (exprlist_length(args) != 0)
	{
	    sprintf(error_string, "user_bool takes no arguments.");
	    JUMP(0);
	}

	userval = register_bool(name);

	if (userval == 0)
	{
	    sprintf(error_string, "user_bool %s has a mismatch.", name);
	    JUMP(0);
	}

	tree->result.number = nil_tag_number;
	tree->result.length = 1;
    }
    else if (strcmp(type, "user_color") == 0)
    {
	if (exprlist_length(args) != 0)
	{
	    sprintf(error_string, "user_bool takes no arguments.");
	    JUMP(0);
	}

	userval = register_color(name);

	if (userval == 0)
	{
	    sprintf(error_string, "user_bool %s has a mismatch.", name);
	    JUMP(0);
	}

	tree->result.number = rgba_tag_number;
	tree->result.length = 4;
    }
    else if (strcmp(type, "user_curve") == 0)
    {
	if (exprlist_length(args) != 1)
	{
	    sprintf(error_string, "user_curve takes 1 argument.");
	    JUMP(0);
	}
	if (args->result.length != 1)
	{
	    sprintf(error_string, "user_curve argument must have length 1.");
	    JUMP(0);
	}

	userval = register_curve(name);

	if (userval == 0)
	{
	    sprintf(error_string, "user_curve %s has mismatch.", name);
	    JUMP(0);
	}

	tree->result.number = nil_tag_number;
	tree->result.length = 1;
    }
    else
    {
	sprintf(error_string, "Unknown userval function %s.", type);
	JUMP(0);
    }

    tree->type = EXPR_USERVAL;
    tree->val.userval.userval = userval;
    tree->val.userval.args = args;

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
    variable_t *var = lookup_variable(name, &tree->result);

    if (var == 0)
    {
	var = register_variable(name, value->result);
	tree->result = value->result;
    }

    if (tree->result.number != value->result.number || tree->result.length != value->result.length)
    {
	sprintf(error_string, "Variable %s is being assigned two different types.", name);
	JUMP(0);
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
    variable_t *var = lookup_variable(name, &info);

    if (var == 0)
    {
	sprintf(error_string, "Undefined variable %s.", name);
	JUMP(0);
    }

    if (subscripts->result.length != value->result.length)
    {
	sprintf(error_string, "Lhs does not match rhs in sub assignment.");
	JUMP(0);
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
	JUMP(0);
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
	JUMP(0);
    }
    if (consequent->result.number != alternative->result.number
	|| consequent->result.length != alternative->result.length)
    {
	sprintf(error_string, "Consequent and alternative must have the same type in if statement.");
	JUMP(0);
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

    while (tree->next != 0)
	tree = tree->next;

    tree->next = list2;

    return list1;
}
