/*
 * exprtree.h
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

#ifndef __EXPRTREE_H__
#define __EXPRTREE_H__

#include "vars.h"
#include "builtins.h"
#include "internals.h"
#include "macros.h"

extern char error_string[];

#define MAX_IDENT_LENGTH    63

typedef char ident[MAX_IDENT_LENGTH + 1];

struct _overload_entry_t;
struct _userval_t;

typedef struct _exprtree
{
    int type;
    tuple_info_t result;
    int tmpvarnum;

    union
    {
	tuple_t tuple_const;
	variable_t *var;
	internal_t *internal;
	struct
	{
	    struct _userval_info_t *info;
	    struct _exprtree *args;
	} userval;
	struct
	{
	    int length;
	    struct _exprtree *elems;
	} tuple;
	struct
	{
	    struct _exprtree *tuple;
	    struct _exprtree *subscripts;
	} select;
	struct
	{
	    int tagnum;
	    struct _exprtree *tuple;
	} cast;
	struct
	{
	    int tagnum;
	    struct _exprtree *tuple;
	} convert;
	struct
	{
	    struct _overload_entry_t *entry;
	    struct _exprtree *args;
	} func;
	struct
	{
	    struct _exprtree *left;
	    struct _exprtree *right;
	} operator;
	struct
	{
	    variable_t *var;
	    struct _exprtree *value;
	} assignment;
	struct
	{
	    variable_t *var;
	    struct _exprtree *subscripts;
	    struct _exprtree *value;
	} sub_assignment;
	struct
	{
	    struct _exprtree *condition;
	    struct _exprtree *consequent;
	    struct _exprtree *alternative;
	    int label1;
	    int label2;
	} ifExpr;
	struct
	{
	    struct _exprtree *invariant;
	    struct _exprtree *body;
	    int label1;
	    int label2;
	} whileExpr;
	struct
	{
	    macro_function_t macro;
	    struct _exprtree *args;
	} macro;
    } val;

    struct _exprtree *next;
} exprtree;

#define EXPR_TUPLE_CONST     1
#define EXPR_FUNC            2
#define EXPR_INTERNAL        3
#define EXPR_SEQUENCE        4
#define EXPR_ASSIGNMENT      5
#define EXPR_VARIABLE        6
#define EXPR_IF_THEN         7
#define EXPR_IF_THEN_ELSE    8
#define EXPR_WHILE           9
#define EXPR_DO_WHILE       10
#define EXPR_TUPLE          11
#define EXPR_SELECT         12
#define EXPR_CAST           13
#define EXPR_CONVERT        14
#define EXPR_SUB_ASSIGNMENT 15
#define EXPR_USERVAL        16

exprtree* make_number (float num);
exprtree* make_range (int first, int last);
exprtree* make_var (const char *name); /* should use variable_t instead */
exprtree* make_tuple (exprtree *elems);
exprtree* make_select (exprtree *tuple, exprtree *subscripts);
exprtree* make_cast (const char *tagname, exprtree *tuple); /* should use tag number instead */
exprtree* make_convert (const char *tagname, exprtree *tuple); /* ditto */
exprtree* make_function (const char *name, exprtree *args);
exprtree* make_userval (const char *type, const char *name, exprtree *args);
exprtree* make_sequence (exprtree *left, exprtree *right);
exprtree* make_assignment (char *name, exprtree *value); /* should use variable_t instead */
exprtree* make_sub_assignment (char *name, exprtree *subscripts, exprtree *value);
exprtree* make_if_then (exprtree *condition, exprtree *consequent);
exprtree* make_if_then_else (exprtree *condition, exprtree *consequent, exprtree *alternative);
exprtree* make_while (exprtree *invariant, exprtree *body);
exprtree* make_do_while (exprtree *body, exprtree *invariant);

void free_exprtree (exprtree *tree);

int exprlist_length (exprtree *list);
exprtree* exprlist_append (exprtree *list1, exprtree *list2);
#endif
