/*
 * postfix.c
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

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "builtins.h"
#include "vars.h"
#include "exprtree.h"
#include "tags.h"
#include "tuples.h"
#include "overload.h"
#include "mathmap.h"
#include "jump.h"
#include "userval.h"

#include "postfix.h"

#define EXPRSIZE     8192

static postfix expression[EXPRSIZE];
static int exprp;

void
stack_push (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    invocation->stack[invocation->stackp] = arg->tuple;
    ++invocation->stackp;
}

void
stack_pop (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    --invocation->stackp;
}

void
stack_select (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    tuple_t result;
    int i;

    for (i = 0; i < invocation->stack[invocation->stackp - 1].length; ++i)
    {
	int index = invocation->stack[invocation->stackp - 1].data[i];

	if (index < 0 || index >= invocation->stack[invocation->stackp - 2].length)
	    result.data[i] = 0.0;
	else
	    result.data[i] = invocation->stack[invocation->stackp - 2].data[index];
    }

    memcpy(invocation->stack[invocation->stackp - 2].data, result.data, sizeof(float) * invocation->stack[invocation->stackp - 1].length);
    if (invocation->stack[invocation->stackp - 1].length == 1)
	invocation->stack[invocation->stackp - 2].number = nil_tag_number;
    invocation->stack[invocation->stackp - 2].length = invocation->stack[invocation->stackp - 1].length;
    --invocation->stackp;
}

void
stack_tuple (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    int i;

    for (i = 1; i < arg->integer; ++i)
	invocation->stack[invocation->stackp - arg->integer].data[i] = invocation->stack[invocation->stackp - arg->integer + i].data[0];
    invocation->stack[invocation->stackp - arg->integer].number = nil_tag_number;
    invocation->stack[invocation->stackp - arg->integer].length = arg->integer;

    invocation->stackp -= arg->integer - 1;
}

void
stack_dupn_i (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    int i;

    for (i = 0; i < arg->integer; ++i)
	invocation->stack[invocation->stackp + i] = invocation->stack[invocation->stackp - 1];

    invocation->stackp += arg->integer;
}

void
stack_cast (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    invocation->stack[invocation->stackp - 1].number = arg->integer;
}

void
stack_jmp (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    invocation->exprp = arg->integer - 1;
}

void
stack_jez (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    if (invocation->stack[--invocation->stackp].data[0] == 0.0)
	invocation->exprp = arg->integer - 1;
}

void
stack_jnez (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    if (invocation->stack[--invocation->stackp].data[0] != 0.0)
	invocation->exprp = arg->integer - 1;
}

void
stack_push_internal (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    invocation->stack[invocation->stackp++] = invocation->internals[arg->internal->index];
}

void
stack_push_user_var (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    invocation->stack[invocation->stackp++] = invocation->variables[arg->user_var->index];
}

void
stack_assign (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    invocation->variables[arg->user_var->index] = invocation->stack[invocation->stackp - 1];
}

void
stack_sub_assign (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    int i;

    for (i = 0; i < invocation->stack[invocation->stackp - 1].length; ++i)
    {
	int index = (int)invocation->stack[invocation->stackp - 1].data[i];

	if (index >= 0 && index < arg->user_var->type.length)
	    invocation->variables[arg->user_var->index].data[index] = invocation->stack[invocation->stackp - 2].data[i];
    }
    --invocation->stackp;
}

void
stack_userval_int_const (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    invocation->stack[invocation->stackp].data[0] = invocation->uservals[arg->userval->index].v.int_const;
    invocation->stack[invocation->stackp].length = 1;
    invocation->stack[invocation->stackp].number = nil_tag_number;
    ++invocation->stackp;
}

void
stack_userval_float_const (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    invocation->stack[invocation->stackp].data[0] = invocation->uservals[arg->userval->index].v.float_const;
    invocation->stack[invocation->stackp].length = 1;
    invocation->stack[invocation->stackp].number = nil_tag_number;
    ++invocation->stackp;
}

void
stack_userval_bool_const (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    invocation->stack[invocation->stackp].data[0] = invocation->uservals[arg->userval->index].v.bool_const;
    invocation->stack[invocation->stackp].length = 1;
    invocation->stack[invocation->stackp].number = nil_tag_number;
    ++invocation->stackp;
}

void
stack_userval_color (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    /* FIXME: change for new uservals */
    /*
    invocation->stack[invocation->stackp++] = invocation->uservals[arg->userval->index].v.color.value;
    */
}

void
stack_userval_curve (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    int index = invocation->stack[invocation->stackp - 1].data[0] * (USER_CURVE_POINTS - 1);

    if (index < 0)
	index = 0;
    else if (index >= USER_CURVE_POINTS)
	index = USER_CURVE_POINTS - 1;

    invocation->stack[invocation->stackp - 1].data[0] = invocation->uservals[arg->userval->index].v.curve.values[index];
    invocation->stack[invocation->stackp - 1].number = nil_tag_number;
}

void
stack_userval_gradient (mathmap_invocation_t *invocation, postfix_arg *arg)
{
    /* FIXME: change for new uservals */
    /*
    int index = invocation->stack[invocation->stackp - 1].data[0] * (USER_GRADIENT_POINTS - 1);
    int i;

    if (index < 0)
	index = 0;
    else if (index >= USER_GRADIENT_POINTS)
	index = USER_GRADIENT_POINTS - 1;

    for (i = 0; i < 4; ++i)
	invocation->stack[invocation->stackp - 1].data[i] = invocation->uservals[arg->userval->index].v.gradient.values[index][i];
    invocation->stack[invocation->stackp - 1].length = 4;
    invocation->stack[invocation->stackp - 1].number = rgba_tag_number;
    */
}

void
stack_userval_image (mathmap_invocation_t *invocation, postfix_arg *arg)
{
#ifndef OPENSTEP
    invocation->stack[invocation->stackp].data[0] = invocation->uservals[arg->userval->index].v.image.index;
#else
    invocation->stack[invocation->stackp].data[0] = arg->userval->index;
#endif
    invocation->stack[invocation->stackp].length = 1;
    invocation->stack[invocation->stackp].number = image_tag_number;
    ++invocation->stackp;
}

void
make_postfix_recursive (exprtree *tree)
{
    switch (tree->type)
    {
	case EXPR_INT_CONST :
	    expression[exprp].func = stack_push;
	    expression[exprp].arg.tuple.number = tree->result.number;
	    expression[exprp].arg.tuple.length = 1;
	    expression[exprp].arg.tuple.data[0] = (float)tree->val.int_const;
	    ++exprp;
	    break;

	case EXPR_FLOAT_CONST :
	    expression[exprp].func = stack_push;
	    expression[exprp].arg.tuple.number = tree->result.number;
	    expression[exprp].arg.tuple.length = 1;
	    expression[exprp].arg.tuple.data[0] = tree->val.float_const;
	    ++exprp;
	    break;

	case EXPR_TUPLE_CONST :
	    expression[exprp].func = stack_push;
	    expression[exprp].arg.tuple = tree->val.tuple_const;
	    ++exprp;
	    break;

	case EXPR_TUPLE :
	    {
		exprtree *elem;

		for (elem = tree->val.tuple.elems; elem != 0; elem = elem->next)
		    make_postfix_recursive(elem);

		expression[exprp].func = stack_tuple;
		expression[exprp].arg.integer = tree->val.tuple.length;
		++exprp;
	    }
	    break;

	case EXPR_SELECT :
	    make_postfix_recursive(tree->val.select.tuple);
	    make_postfix_recursive(tree->val.select.subscripts);
	    expression[exprp].func = stack_select;
	    ++exprp;
	    break;

	case EXPR_CAST :
	    make_postfix_recursive(tree->val.cast.tuple);
	    if (tree->val.cast.tuple->result.number != tree->val.cast.tagnum)
	    {
		expression[exprp].func = stack_cast;
		expression[exprp].arg.integer = tree->val.cast.tagnum;
		++exprp;
	    }
	    break;

	case EXPR_INTERNAL :
	    expression[exprp].func = stack_push_internal;
	    expression[exprp].arg.internal = tree->val.internal;
	    ++exprp;
	    break;

	case EXPR_FUNC :
	    {
		exprtree *arg = tree->val.func.args;

		while (arg != 0)
		{
		    make_postfix_recursive(arg);
		    arg = arg->next;
		}

		expression[exprp++].func = tree->val.func.entry->v.builtin.builtin;

	    }
	    break;

	case EXPR_VARIABLE :
	    assert(tree->val.var->type.length != 0);
	    expression[exprp].func = stack_push_user_var;
	    expression[exprp].arg.user_var = tree->val.var;
	    ++exprp;
	    break;

	case EXPR_USERVAL :
	    if (tree->val.userval.info->type == USERVAL_INT_CONST)
		expression[exprp].func = stack_userval_int_const;
	    else if (tree->val.userval.info->type == USERVAL_FLOAT_CONST)
		expression[exprp].func = stack_userval_float_const;
	    else if (tree->val.userval.info->type == USERVAL_BOOL_CONST)
		expression[exprp].func = stack_userval_bool_const;
	    else if (tree->val.userval.info->type == USERVAL_COLOR)
		expression[exprp].func = stack_userval_color;
	    else if (tree->val.userval.info->type == USERVAL_CURVE)
	    {
		make_postfix_recursive(tree->val.userval.args);
		expression[exprp].func = stack_userval_curve;
	    }
	    else if (tree->val.userval.info->type == USERVAL_GRADIENT)
	    {
		make_postfix_recursive(tree->val.userval.args);
		expression[exprp].func = stack_userval_gradient;
	    }
	    else if (tree->val.userval.info->type == USERVAL_IMAGE)
		expression[exprp].func = stack_userval_image;
	    else
		assert(0);
	    expression[exprp].arg.userval = tree->val.userval.info;
	    ++exprp;
	    break;

	case EXPR_ASSIGNMENT :
	    make_postfix_recursive(tree->val.assignment.value);

	    expression[exprp].func = stack_assign;
	    expression[exprp].arg.user_var = tree->val.assignment.var;
	    ++exprp;
	    break;

	case EXPR_SUB_ASSIGNMENT :
	    make_postfix_recursive(tree->val.sub_assignment.value);
	    make_postfix_recursive(tree->val.sub_assignment.subscripts);

	    expression[exprp].func = stack_sub_assign;
	    expression[exprp].arg.user_var = tree->val.sub_assignment.var;
	    ++exprp;
	    break;

	case EXPR_SEQUENCE :
	    make_postfix_recursive(tree->val.operator.left);
	    expression[exprp++].func = stack_pop;
	    make_postfix_recursive(tree->val.operator.right);
	    break;

	case EXPR_IF_THEN :
	    {
		int jump_pos1,
		    jump_pos2,
		    i;

		make_postfix_recursive(tree->val.ifExpr.condition);
		expression[exprp].func = stack_jez;
		jump_pos1 = exprp;
		++exprp;
		make_postfix_recursive(tree->val.ifExpr.consequent);
		expression[exprp].func = stack_jmp;
		jump_pos2 = exprp;
		++exprp;
		expression[jump_pos1].arg.integer = exprp;
		expression[exprp].func = stack_push;
		expression[exprp].arg.tuple.number = tree->result.number;
		expression[exprp].arg.tuple.length = tree->result.length;
		for (i = 0; i < tree->result.length; ++i)
		    expression[exprp].arg.tuple.data[i] = 0.0;
		++exprp;
		expression[jump_pos2].arg.integer = exprp;
	    }
	    break;

	case EXPR_IF_THEN_ELSE :
	    {
		int jump_pos1,
		    jump_pos2;

		make_postfix_recursive(tree->val.ifExpr.condition);
		expression[exprp].func = stack_jez;
		jump_pos1 = exprp;
		++exprp;
		make_postfix_recursive(tree->val.ifExpr.consequent);
		expression[exprp].func = stack_jmp;
		jump_pos2 = exprp;
		++exprp;
		expression[jump_pos1].arg.integer = exprp;
		make_postfix_recursive(tree->val.ifExpr.alternative);
		expression[jump_pos2].arg.integer = exprp;
	    }
	    break;

	case EXPR_WHILE :
	    {
		int label1,
		    jump_pos2;

		label1 = exprp;
		make_postfix_recursive(tree->val.whileExpr.invariant);
		expression[exprp].func = stack_jez;
		jump_pos2 = exprp;
		++exprp;
		make_postfix_recursive(tree->val.whileExpr.body);
		expression[exprp++].func = stack_pop;
		expression[exprp].func = stack_jmp;
		expression[exprp].arg.integer = label1;
		++exprp;
		expression[jump_pos2].arg.integer = exprp;
		expression[exprp].func = stack_push;
		expression[exprp].arg.tuple.number = nil_tag_number;
		expression[exprp].arg.tuple.length = 1;
		expression[exprp].arg.tuple.data[0] = 0.0;
		++exprp;
	    }
	    break;

	case EXPR_DO_WHILE :
	    {
		int label;

		label = exprp;
		make_postfix_recursive(tree->val.whileExpr.body);
		expression[exprp++].func = stack_pop;
		make_postfix_recursive(tree->val.whileExpr.invariant);
		expression[exprp].func = stack_jnez;
		expression[exprp].arg.integer = label;
		++exprp;
		expression[exprp].func = stack_push;
		expression[exprp].arg.tuple.number = nil_tag_number;
		expression[exprp].arg.tuple.length = 1;
		expression[exprp].arg.tuple.data[0] = 0.0;
		++exprp;
	    }
	    break;

	default :
	    fprintf(stderr, "illegal expr\n");
	    break;
    }
}

postfix*
make_postfix (exprtree *tree, int *len)
{
    postfix *result;

    exprp = 0;
    make_postfix_recursive(tree);
    *len = exprp;

    result = (postfix*)malloc(*len * sizeof(postfix));
    memcpy(result, expression, *len * sizeof(postfix));

    return result;
}

void
output_tuple (tuple_t *tuple)
{
    int i;

    printf("%s:[", tag_name_for_number(tuple->number));
    for (i = 0; i < tuple->length; ++i)
	printf(i == tuple->length - 1 ? "%f]" : "%f,", tuple->data[i]);
}

void
output_postfix (postfix *expression, int exprlen)
{
    int i;

    printf("-------------------------\n");

    for (i = 0; i < exprlen; ++i)
    {
	printf("%3d   ", i);
	if (expression[i].func == stack_push)
	{
	    printf("push ");
	    output_tuple(&expression[i].arg.tuple);
	    printf("\n");
	}
	else if (expression[i].func == stack_pop)
	    printf("pop\n");
	else if (expression[i].func == stack_select)
	    printf("select\n");
	else if (expression[i].func == stack_tuple)
	    printf("tuple %d\n", expression[i].arg.integer);
	else if (expression[i].func == stack_dupn_i)
	    printf("dupn_i %d\n", expression[i].arg.integer);
	else if (expression[i].func == stack_cast)
	    printf("cast %s\n", tag_name_for_number(expression[i].arg.integer));
	else if (expression[i].func == stack_jmp)
	    printf("jmp %d\n", expression[i].arg.integer);
	else if (expression[i].func == stack_jez)
	    printf("jez %d\n", expression[i].arg.integer);
	else if (expression[i].func == stack_jnez)
	    printf("jnez %d\n", expression[i].arg.integer);
	else if (expression[i].func == stack_push_internal)
	    printf("push_internal %s (%d)\n", expression[i].arg.internal->name, expression[i].arg.internal->index);
	else if (expression[i].func == stack_push_user_var)
	    printf("push_user_var %s\n", expression[i].arg.user_var->name);
	else if (expression[i].func == stack_assign)
	    printf("sto %s\n", expression[i].arg.user_var->name);
	else if (expression[i].func == stack_sub_assign)
	    printf("substo %s\n", expression[i].arg.user_var->name);
	else
	{
	    overload_entry_t *entry = overloaded_builtin_with_function(expression[i].func);

	    if (entry != 0)
		printf("%s\n", entry->name);
	    else
		printf("unknown opcode\n");
	}
    }
}

tuple_t*
eval_postfix (mathmap_invocation_t *invocation)
{
    postfix *expression = invocation->mathmap->expression;
    int exprlen = invocation->mathmap->exprlen;

    invocation->stackp = 0;
    for (invocation->exprp = 0; invocation->exprp < exprlen; ++invocation->exprp)
	expression[invocation->exprp].func(invocation, &expression[invocation->exprp].arg);

    invocation->stack[0].number = rgba_tag_number;
    invocation->stack[0].length = 4;

    return &invocation->stack[0];
}
