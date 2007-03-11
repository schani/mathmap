/*
 * postfix.h
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

#ifndef __POSTFIX_H__
#define __POSTFIX_H__

#include "tuples.h"
#include "vars.h"
#include "internals.h"
#include "lispreader/pools.h"

#define POSTFIX_STACKSIZE     256

struct _userval_info_t;
struct _mathmap_invocation_t;

#define POSTFIX_VALUE_TUPLE        0
#define POSTFIX_VALUE_CLOSURE      1
#define POSTFIX_VALUE_RETURN_INFO  2

struct _postfix_closure_t;

typedef struct
{
    int num_args;
    int return_address;
    int old_basep;
    struct _postfix_closure_t *old_closure;
} postfix_return_info_t;

typedef struct
{
    int type;
    union
    {
	tuple_t tuple;
	struct _postfix_closure_t *closure;
	postfix_return_info_t return_info;
    } v;
} postfix_value_t;

typedef union
{
    struct
    {
	int i1;
	int i2;
    } integer;
    tuple_t tuple;
    internal_t *internal;
    variable_t *user_var;
    struct _userval_info_t *userval;
} postfix_arg_t;

typedef struct _postfix_closure_t
{
    int start_address;
    int num_args;
    postfix_value_t args[];
} postfix_closure_t;

typedef struct
{
    postfix_value_t stack[POSTFIX_STACKSIZE];
    int ip;
    int stackp;
    int basep;
    postfix_closure_t *current_closure;
    pools_t pools;
} postfix_machine_t;

typedef void (*stackfunc_t) (struct _mathmap_invocation_t*, postfix_arg_t*);

typedef struct _postfix
{
    stackfunc_t func;
    postfix_arg_t arg;
} postfix_insn_t;

struct _exprtree;

postfix_insn_t* make_postfix (struct _exprtree *tree, int *len);
void output_postfix (postfix_insn_t *expression, int exprlen);
tuple_t* eval_postfix (struct _mathmap_invocation_t *invocation);

postfix_machine_t* make_postfix_machine (void);
void free_postfix_machine (postfix_machine_t *machine);

void make_postfix_recursive (struct _exprtree *tree);

#endif
