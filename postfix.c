#include <stdio.h>
#include <math.h>
#include <string.h>
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

#define STACKSIZE    1024
#define EXPRSIZE     8192

extern int usesRA;

tuple_t stack[STACKSIZE];
int stackp,
    num_ops = 0;

postfix expression[EXPRSIZE];
int exprp,
    exprlen;

void
stack_push (postfix_arg *arg)
{
    stack[stackp] = arg->tuple;
    ++stackp;
}

void
stack_pop (postfix_arg *arg)
{
    --stackp;
}

void
stack_select (postfix_arg *arg)
{
    tuple_t result;
    int i;

    for (i = 0; i < stack[stackp - 1].length; ++i)
    {
	int index = stack[stackp - 1].data[i];

	if (index < 0 || index >= stack[stackp - 2].length)
	    result.data[i] = 0.0;
	else
	    result.data[i] = stack[stackp - 2].data[index];
    }

    memcpy(stack[stackp - 2].data, result.data, sizeof(float) * stack[stackp - 1].length);
    if (stack[stackp - 1].length == 1)
	stack[stackp - 2].number = nil_tag_number;
    stack[stackp - 2].length = stack[stackp - 1].length;
    --stackp;
}

void
stack_tuple (postfix_arg *arg)
{
    int i;

    for (i = 1; i < arg->integer; ++i)
	stack[stackp - arg->integer].data[i] = stack[stackp - arg->integer + i].data[0];
    stack[stackp - arg->integer].number = nil_tag_number;
    stack[stackp - arg->integer].length = arg->integer;

    stackp -= arg->integer - 1;
}

void
stack_dupn_i (postfix_arg *arg)
{
    int i;

    for (i = 0; i < arg->integer; ++i)
	stack[stackp + i] = stack[stackp - 1];

    stackp += arg->integer;
}

void
stack_cast (postfix_arg *arg)
{
    stack[stackp - 1].number = arg->integer;
}

void
stack_jmp (postfix_arg *arg)
{
    exprp = arg->integer - 1;
}

void
stack_jez (postfix_arg *arg)
{
    if (stack[--stackp].data[0] == 0.0)
	exprp = arg->integer - 1;
}

void
stack_jnez (postfix_arg *arg)
{
    if (stack[--stackp].data[0] != 0.0)
	exprp = arg->integer - 1;
}

void
stack_push_internal (postfix_arg *arg)
{
    stack[stackp++] = arg->internal->value;
}

void
stack_push_user_var (postfix_arg *arg)
{
    stack[stackp++] = arg->user_var->value;
}

void
stack_assign (postfix_arg *arg)
{
    arg->user_var->value = stack[stackp - 1];
}

void
stack_sub_assign (postfix_arg *arg)
{
    int i;

    for (i = 0; i < stack[stackp - 1].length; ++i)
    {
	int index = (int)stack[stackp - 1].data[i];

	if (index >= 0 && index < arg->user_var->value.length)
	    arg->user_var->value.data[index] = stack[stackp - 2].data[i];
    }
    --stackp;
}

void
stack_userval_slider (postfix_arg *arg)
{
    stack[stackp].data[0] = arg->userval->v.slider.value;
    stack[stackp].length = 1;
    stack[stackp].number = nil_tag_number;
    ++stackp;
}

void
stack_userval_bool (postfix_arg *arg)
{
    stack[stackp].data[0] = arg->userval->v.bool.value;
    stack[stackp].length = 1;
    stack[stackp].number = nil_tag_number;
    ++stackp;
}

void
stack_userval_color (postfix_arg *arg)
{
    stack[stackp++] = arg->userval->v.color.value;
}

void
stack_userval_curve (postfix_arg *arg)
{
    int index = stack[stackp - 1].data[0] * (USER_CURVE_POINTS - 1);

    if (index < 0)
	index = 0;
    else if (index >= USER_CURVE_POINTS)
	index = USER_CURVE_POINTS - 1;

    stack[stackp - 1].data[0] = arg->userval->v.curve.values[index];
    stack[stackp - 1].number = nil_tag_number;
}

void
stack_userval_image (postfix_arg *arg)
{
    stack[stackp].data[0] = arg->userval->v.image.index;
    stack[stackp].length = 1;
    stack[stackp].number = image_tag_number;
    ++stackp;
}

void
make_postfix_recursive (exprtree *tree)
{
    switch (tree->type)
    {
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
	    assert(tree->val.var->value.length != 0);
	    expression[exprp].func = stack_push_user_var;
	    expression[exprp].arg.user_var = tree->val.var;
	    ++exprp;
	    break;

	case EXPR_USERVAL :
	    if (tree->val.userval.userval->type == USERVAL_SLIDER)
		expression[exprp].func = stack_userval_slider;
	    else if (tree->val.userval.userval->type == USERVAL_BOOL)
		expression[exprp].func = stack_userval_bool;
	    else if (tree->val.userval.userval->type == USERVAL_COLOR)
		expression[exprp].func = stack_userval_color;
	    else if (tree->val.userval.userval->type == USERVAL_CURVE)
	    {
		make_postfix_recursive(tree->val.userval.args);
		expression[exprp].func = stack_userval_curve;
	    }
	    else if (tree->val.userval.userval->type == USERVAL_IMAGE)
		expression[exprp].func = stack_userval_image;
	    else
		assert(0);
	    expression[exprp].arg.userval = tree->val.userval.userval;
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

void
make_postfix (exprtree *tree)
{
    exprp = 0;
    make_postfix_recursive(tree);
    exprlen = exprp;
}

void
make_empty_postfix (void)
{
    int i;

    expression[0].func = stack_push;
    expression[0].arg.tuple.number = rgba_tag_number;
    expression[0].arg.tuple.length = 4;
    for (i = 0; i < 4; ++i)
	expression[0].arg.tuple.data[i] = 0.0;

    exprlen = exprp = 1;
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
output_postfix (void)
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
	    printf("push_internal %s\n", expression[i].arg.internal->name);
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
eval_postfix (void)
{
    stackp = 0;
    for (exprp = 0; exprp < exprlen; ++exprp)
    {
	expression[exprp].func(&expression[exprp].arg);
	++num_ops;
    }

    stack[0].number = rgba_tag_number;
    stack[0].length = 4;

    return &stack[0];
}
