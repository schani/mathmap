/*
 * postfix.h
 *
 * MathMap
 *
 * Copyright (C) 1997-2000 Mark Probst
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

#define POSTFIX_STACKSIZE     256

struct _userval_info_t;
struct _mathmap_invocation_t;

typedef union
{
    int integer;
    tuple_t tuple;
    internal_t *internal;
    variable_t *user_var;
    struct _userval_info_t *userval;
} postfix_arg;

typedef void (*stackfunc) (struct _mathmap_invocation_t*, postfix_arg*);

typedef struct _postfix
{
    stackfunc func;
    postfix_arg arg;
} postfix;

struct _exprtree;

postfix* make_postfix (struct _exprtree *tree, int *len);
void output_postfix (postfix *expression, int exprlen);
tuple_t* eval_postfix (struct _mathmap_invocation_t *invocation);

void make_postfix_recursive (struct _exprtree *tree);

#endif
