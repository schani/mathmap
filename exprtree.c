#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "exprtree.h"
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
make_number (double num)
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
make_select (exprtree *tuple, exprtree *num)
{
    exprtree *tree = alloc_exprtree();

    tree->type = EXPR_SELECT;
    tree->val.select.tuple = tuple;
    tree->val.select.num = num;
    tree->result = make_tuple_info(nil_tag_number, 1);

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
	    tree = alloc_exprtree();

	    tree->type = EXPR_FUNC;
	    tree->val.func.entry = entry;
	    tree->val.func.args = args;
	    tree->result = info;
	}
	else
	    tree = entry->v.macro(args);
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

exprtree*
exprlist_append (exprtree *list1, exprtree *list2)
{
    exprtree *tree = list1;

    while (tree->next != 0)
	tree = tree->next;

    tree->next = list2;

    return list1;
}
