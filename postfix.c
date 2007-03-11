/*
 * postfix.c
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
#include "lispreader/pools.h"

#include "postfix.h"

#define EXPRSIZE     8192

static postfix_insn_t expression[EXPRSIZE];
static int exprp;

#define STK             (invocation->stack_machine->stack)
#define SP              (invocation->stack_machine->stackp)
#define BP              (invocation->stack_machine->basep)
#define IP              (invocation->stack_machine->ip)
#define CLOS            (invocation->stack_machine->current_closure)

static void
stack_push (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    STK[SP].type = POSTFIX_VALUE_TUPLE;
    STK[SP].v.tuple = arg->tuple;
    ++SP;
}

static void
stack_pop (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    --SP;
}

static void
stack_select (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    tuple_t result;
    int i;

    assert(STK[SP - 1].type == POSTFIX_VALUE_TUPLE
	   && STK[SP - 2].type == POSTFIX_VALUE_TUPLE);

    for (i = 0; i < STK[SP - 1].v.tuple.length; ++i)
    {
	int index = STK[SP - 1].v.tuple.data[i];

	if (index < 0 || index >= STK[SP - 2].v.tuple.length)
	    result.data[i] = 0.0;
	else
	    result.data[i] = STK[SP - 2].v.tuple.data[index];
    }

    memcpy(STK[SP - 2].v.tuple.data, result.data, sizeof(float) * STK[SP - 1].v.tuple.length);
    if (STK[SP - 1].v.tuple.length == 1)
	STK[SP - 2].v.tuple.number = nil_tag_number;
    STK[SP - 2].v.tuple.length = STK[SP - 1].v.tuple.length;
    --SP;
}

static void
stack_tuple (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    int i;

    for (i = 1; i < arg->integer.i1; ++i)
	STK[SP - arg->integer.i1].v.tuple.data[i] = STK[SP - arg->integer.i1 + i].v.tuple.data[0];
    STK[SP - arg->integer.i1].v.tuple.number = nil_tag_number;
    STK[SP - arg->integer.i1].v.tuple.length = arg->integer.i1;

    SP -= arg->integer.i1 - 1;
}

static void
stack_dupn_i (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    int i;

    for (i = 0; i < arg->integer.i1; ++i)
	STK[SP + i] = STK[SP - 1];

    SP += arg->integer.i1;
}

static void
stack_cast (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    STK[SP - 1].v.tuple.number = arg->integer.i1;
}

static void
stack_jmp (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    IP = arg->integer.i1 - 1;
}

static void
stack_jez (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    if (STK[--SP].v.tuple.data[0] == 0.0)
	IP = arg->integer.i1 - 1;
}

static void
stack_jnez (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    if (STK[--SP].v.tuple.data[0] != 0.0)
	IP = arg->integer.i1 - 1;
}

static void
stack_push_internal (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    STK[SP].type = POSTFIX_VALUE_TUPLE;
    STK[SP].v.tuple = invocation->internals[arg->internal->index];
    ++SP;
}

static void
stack_sub_set (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    int i;

    assert(STK[SP - 1].type == POSTFIX_VALUE_TUPLE
	   && STK[SP - 2].type == POSTFIX_VALUE_TUPLE
	   && STK[SP - 3].type == POSTFIX_VALUE_TUPLE);

    for (i = 0; i < STK[SP - 1].v.tuple.length; ++i)
    {
	int index = (int)STK[SP - 1].v.tuple.data[i];

	if (index >= 0 && index < STK[SP - 3].v.tuple.length)
	    STK[SP - 3].v.tuple.data[index] = STK[SP - 2].v.tuple.data[i];
    }

    SP -= 2;
}

/*** functions ***/

/* ... arg1 arg0 -> result */
static void
stack_call (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    STK[SP].type = POSTFIX_VALUE_RETURN_INFO;
    STK[SP].v.return_info.num_args = arg->integer.i2;
    STK[SP].v.return_info.return_address = IP;
    STK[SP].v.return_info.old_basep = BP;
    STK[SP].v.return_info.old_closure = CLOS;

    BP = SP;

    ++SP;

    IP = arg->integer.i1 - 1;
}

/* return */
static void
stack_return (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    int num_args = STK[BP].v.return_info.num_args;
    int return_value_src = SP - 1;
    int return_value_dst = BP - num_args;

    IP = STK[BP].v.return_info.return_address;
    CLOS = STK[BP].v.return_info.old_closure;
    BP = STK[BP].v.return_info.old_basep;

    STK[return_value_dst] = STK[return_value_src];

    SP = return_value_dst + 1;
}

/* () -> () */
static void
stack_alloc_locals (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    assert(STK[SP - 1].type == POSTFIX_VALUE_RETURN_INFO);

    SP += arg->integer.i1;
}

/* value -> () */
static void
stack_set_local (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    STK[BP + 1 + arg->integer.i1] = STK[--SP];
}

/* () -> value */
static void
stack_get_local (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    STK[SP++] = STK[BP + 1 + arg->integer.i1];
}

/* value -> () */
static void
stack_set_arg (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    STK[BP - 1 - arg->integer.i1] = STK[--SP];
}

/* () -> value */
static void
stack_get_arg (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    STK[SP++] = STK[BP - 1 - arg->integer.i1];
}

/*** closures ***/

/* ... arg1 arg0 -> closure */
static void
stack_make_closure (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    postfix_closure_t *closure;
    int i;

    closure = pools_alloc(&invocation->stack_machine->pools, sizeof(postfix_closure_t) + arg->integer.i2 * sizeof(postfix_value_t));
    assert(closure != 0);

    closure->start_address = arg->integer.i1;
    closure->num_args = arg->integer.i2;

    for (i = 0; i < closure->num_args; ++i)
	closure->args[i] = STK[--SP];

    STK[SP].type = POSTFIX_VALUE_CLOSURE;
    STK[SP].v.closure = closure;
    ++SP;
}

/* ... arg1 arg0 closure -> result */
static void
stack_call_closure (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    postfix_closure_t *closure;

    assert(STK[SP - 1].type == POSTFIX_VALUE_CLOSURE);

    closure = STK[SP - 1].v.closure;

    STK[SP - 1].type = POSTFIX_VALUE_RETURN_INFO;
    STK[SP - 1].v.return_info.num_args = arg->integer.i1;
    STK[SP - 1].v.return_info.return_address = IP;
    STK[SP - 1].v.return_info.old_basep = BP;
    STK[SP - 1].v.return_info.old_closure = CLOS;

    BP = SP - 1;
    CLOS = closure;
    IP = closure->start_address;
}

/* () -> value */
static void
stack_get_closure_binding (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    assert(arg->integer.i1 >= 0 && arg->integer.i1 < CLOS->num_args);

    STK[SP++] = CLOS->args[arg->integer.i1];
}

/* value -> () */
static void
stack_set_closure_binding (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    assert(arg->integer.i1 >= 0 && arg->integer.i1 < CLOS->num_args);

    CLOS->args[arg->integer.i1] = STK[--SP];
}

/*** vimages ***/

/* closure -> vimage */
static void
stack_make_vimage (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    /* FIXME: implement */
}

/*** user values (FIXME: obsolete) ***/

void
stack_userval_int_const (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    STK[SP].v.tuple.data[0] = invocation->uservals[arg->userval->index].v.int_const;
    STK[SP].v.tuple.length = 1;
    STK[SP].v.tuple.number = nil_tag_number;
    ++SP;
}

void
stack_userval_float_const (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    STK[SP].v.tuple.data[0] = invocation->uservals[arg->userval->index].v.float_const;
    STK[SP].v.tuple.length = 1;
    STK[SP].v.tuple.number = nil_tag_number;
    ++SP;
}

void
stack_userval_bool_const (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    STK[SP].v.tuple.data[0] = invocation->uservals[arg->userval->index].v.bool_const;
    STK[SP].v.tuple.length = 1;
    STK[SP].v.tuple.number = nil_tag_number;
    ++SP;
}

void
stack_userval_color (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    color_t color = invocation->uservals[arg->userval->index].v.color.value;

    STK[SP].v.tuple.data[0] = RED(color) / 255.0;
    STK[SP].v.tuple.data[1] = GREEN(color) / 255.0;
    STK[SP].v.tuple.data[2] = BLUE(color) / 255.0;
    STK[SP].v.tuple.data[3] = ALPHA(color) / 255.0;
    STK[SP].v.tuple.length = 4;
    STK[SP].v.tuple.number = rgba_tag_number;
    ++SP;
}

void
stack_userval_curve (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    int index = STK[SP - 1].v.tuple.data[0] * (USER_CURVE_POINTS - 1);

    if (index < 0)
	index = 0;
    else if (index >= USER_CURVE_POINTS)
	index = USER_CURVE_POINTS - 1;

    STK[SP - 1].v.tuple.data[0] = invocation->uservals[arg->userval->index].v.curve.values[index];
    STK[SP - 1].v.tuple.number = nil_tag_number;
}

void
stack_userval_gradient (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
    int index = STK[SP - 1].v.tuple.data[0] * (USER_GRADIENT_POINTS - 1);
    color_t color;

    if (index < 0)
	index = 0;
    else if (index >= USER_GRADIENT_POINTS)
	index = USER_GRADIENT_POINTS - 1;

    color = invocation->uservals[arg->userval->index].v.gradient.values[index];

    STK[SP - 1].v.tuple.data[0] = RED(color) / 255.0;
    STK[SP - 1].v.tuple.data[1] = GREEN(color) / 255.0;
    STK[SP - 1].v.tuple.data[2] = BLUE(color) / 255.0;
    STK[SP - 1].v.tuple.data[3] = ALPHA(color) / 255.0;
    STK[SP - 1].v.tuple.length = 4;
    STK[SP - 1].v.tuple.number = rgba_tag_number;
}

void
stack_userval_image (mathmap_invocation_t *invocation, postfix_arg_t *arg)
{
#ifndef OPENSTEP
    STK[SP].v.tuple.data[0] = invocation->uservals[arg->userval->index].v.image.index;
#else
    STK[SP].v.tuple.data[0] = arg->userval->index;
#endif
    STK[SP].v.tuple.length = 1;
    STK[SP].v.tuple.number = image_tag_number;
    ++SP;
}

void
make_postfix_recursive (exprtree *tree)
{
    assert(0);

    /*
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
		expression[exprp].arg.integer.i1 = tree->val.tuple.length;
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
		expression[exprp].arg.integer.i1 = tree->val.cast.tagnum;
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
		expression[jump_pos1].arg.integer.i1 = exprp;
		expression[exprp].func = stack_push;
		expression[exprp].arg.tuple.number = tree->result.number;
		expression[exprp].arg.tuple.length = tree->result.length;
		for (i = 0; i < tree->result.length; ++i)
		    expression[exprp].arg.tuple.data[i] = 0.0;
		++exprp;
		expression[jump_pos2].arg.integer.i1 = exprp;
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
		expression[jump_pos1].arg.integer.i1 = exprp;
		make_postfix_recursive(tree->val.ifExpr.alternative);
		expression[jump_pos2].arg.integer.i1 = exprp;
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
		expression[exprp].arg.integer.i1 = label1;
		++exprp;
		expression[jump_pos2].arg.integer.i1 = exprp;
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
		expression[exprp].arg.integer.i1 = label;
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
    */
}

static void
compile_filter (arg_decl_t *args, exprtree *body)
{
    
}

postfix_insn_t*
make_postfix (exprtree *tree, int *len)
{
    postfix_insn_t *result;

    exprp = 0;
    make_postfix_recursive(tree);
    *len = exprp;

    result = (postfix_insn_t*)malloc(*len * sizeof(postfix_insn_t));
    memcpy(result, expression, *len * sizeof(postfix_insn_t));

    return result;
}

void
output_tuple (tuple_t *tuple)
{
    int i;

    printf("%s:[", tag_name_for_number(tuple->number));
    for (i = 0; i < tuple->length; ++i)
	fprintf_c(stdout, i == tuple->length - 1 ? "%f]" : "%f,", tuple->data[i]);
}

void
output_postfix (postfix_insn_t *expression, int exprlen)
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
	    printf("tuple %d\n", expression[i].arg.integer.i1);
	else if (expression[i].func == stack_dupn_i)
	    printf("dupn_i %d\n", expression[i].arg.integer.i1);
	else if (expression[i].func == stack_cast)
	    printf("cast %s\n", tag_name_for_number(expression[i].arg.integer.i1));
	else if (expression[i].func == stack_jmp)
	    printf("jmp %d\n", expression[i].arg.integer.i1);
	else if (expression[i].func == stack_jez)
	    printf("jez %d\n", expression[i].arg.integer.i1);
	else if (expression[i].func == stack_jnez)
	    printf("jnez %d\n", expression[i].arg.integer.i1);
	else if (expression[i].func == stack_push_internal)
	    printf("push_internal %s (%d)\n", expression[i].arg.internal->name, expression[i].arg.internal->index);
	else if (expression[i].func == stack_sub_set)
	    printf("subset %s\n", expression[i].arg.user_var->name); /* FIXME */
	else if (expression[i].func == stack_call)
	    printf("call %d %d\n", expression[i].arg.integer.i1, expression[i].arg.integer.i2);
	else if (expression[i].func == stack_return)
	    printf("ret\n");
	else if (expression[i].func == stack_alloc_locals)
	    printf("locals %d\n", expression[i].arg.integer.i1);
	else if (expression[i].func == stack_set_local)
	    printf("setloc %d\n", expression[i].arg.integer.i1);
	else if (expression[i].func == stack_get_local)
	    printf("getloc %d\n", expression[i].arg.integer.i1);
	else if (expression[i].func == stack_set_arg)
	    printf("setarg %d\n", expression[i].arg.integer.i1);
	else if (expression[i].func == stack_get_arg)
	    printf("getarg %d\n", expression[i].arg.integer.i1);
	else if (expression[i].func == stack_make_closure)
	    printf("make_clos %d %d\n", expression[i].arg.integer.i1, expression[i].arg.integer.i2);
	else if (expression[i].func == stack_call_closure)
	    printf("call_clos %d %d\n", expression[i].arg.integer.i1, expression[i].arg.integer.i2);
	else if (expression[i].func == stack_get_closure_binding)
	    printf("getbind %d\n", expression[i].arg.integer.i1);
	else if (expression[i].func == stack_set_closure_binding)
	    printf("setbind %d\n", expression[i].arg.integer.i1);
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
    postfix_insn_t *expression = invocation->mathmap->expression;
    int exprlen = invocation->mathmap->exprlen;
    postfix_machine_t *machine = invocation->stack_machine;

    machine->stackp = 0;
    for (machine->ip = 0; machine->ip < exprlen; ++machine->ip)
	expression[machine->ip].func(invocation, &expression[machine->ip].arg);

    machine->stack[0].type = POSTFIX_VALUE_TUPLE;
    machine->stack[0].v.tuple.number = rgba_tag_number;
    machine->stack[0].v.tuple.length = 4;

    return &machine->stack[0].v.tuple;
}

postfix_machine_t*
make_postfix_machine (void)
{
    postfix_machine_t *machine = (postfix_machine_t*)malloc(sizeof(postfix_machine_t));

    assert(machine != 0);

    init_pools(&machine->pools);

    return machine;
}

void
free_postfix_machine (postfix_machine_t *machine)
{
    free_pools(&machine->pools);

    free(machine);
}
