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

struct _userval_t;

typedef union
{
    int integer;
    tuple_t tuple;
    internal_t *internal;
    variable_t *user_var;
    struct _userval_t *userval;
} postfix_arg;

typedef void (*stackfunc) (postfix_arg*);

typedef struct _postfix
{
    stackfunc func;
    postfix_arg arg;
} postfix;

extern tuple_t stack[];
extern int stackp;

extern postfix expression[];
extern int exprp;

extern int num_ops;

struct _exprtree;

void make_postfix (struct _exprtree *tree);
void make_empty_postfix (void);
void output_postfix (void);
tuple_t* eval_postfix (void);

void make_postfix_recursive (struct _exprtree *tree);

void stack_push (postfix_arg *arg);
void stack_select_i (postfix_arg *arg);
void stack_tuple (postfix_arg *arg);
void stack_dupn_i (postfix_arg *arg);
void stack_cast (postfix_arg *arg);
void stack_push_internal (postfix_arg *arg);

#endif
