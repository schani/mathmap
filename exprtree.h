/*
 * exprtree.h
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

#ifndef __EXPRTREE_H__
#define __EXPRTREE_H__

#include "vars.h"
#include "builtins.h"
#include "internals.h"
#include "macros.h"

extern char error_string[];

#define LIMITS_INT             1
#define LIMITS_FLOAT           2

/* This is only used during parsing. */
typedef struct
{
    int type;
    union
    {
	struct
	{
	    int min;
	    int max;
	} integer;
	struct
	{
	    float min;
	    float max;
	} floating;
    } v;
} limits_t;

typedef struct _option_t
{
    char *name;
    struct _option_t *suboptions;
    struct _option_t *next;
} option_t;

#define ARG_TYPE_INT           1
#define ARG_TYPE_FLOAT         2
#define ARG_TYPE_BOOL          3
#define ARG_TYPE_COLOR         4
#define ARG_TYPE_GRADIENT      5
#define ARG_TYPE_CURVE         6
#define ARG_TYPE_FILTER        7
#define ARG_TYPE_IMAGE         8

typedef struct _arg_decl_t
{
    char *name;
    int type;
    char *docstring;
    option_t *options;
    union
    {
	struct
	{
	    int have_limits;
	    int min;
	    int max;
	    int default_value;
	} integer;
	struct
	{
	    int have_limits;
	    float min;
	    float max;
	    float default_value;
	} floating;
	struct
	{
	    struct _arg_decl_t *args;
	} filter;
	struct
	{
	    int default_value;
	} boolean;
    } v;
    struct _arg_decl_t *next;
} arg_decl_t;

struct _overload_entry_t;
struct _userval_t;
struct _filter_t;

#define EXPR_INT_CONST       1
#define EXPR_FLOAT_CONST     2
#define EXPR_TUPLE_CONST     3
#define EXPR_FUNC            4
#define EXPR_INTERNAL        5
#define EXPR_SEQUENCE        6
#define EXPR_ASSIGNMENT      7
#define EXPR_VARIABLE        8
#define EXPR_IF_THEN         9
#define EXPR_IF_THEN_ELSE   10
#define EXPR_WHILE          11
#define EXPR_DO_WHILE       12
#define EXPR_TUPLE          13
#define EXPR_SELECT         14
#define EXPR_CAST           15
#define EXPR_CONVERT        16
#define EXPR_SUB_ASSIGNMENT 17
#define EXPR_USERVAL        18
#define EXPR_FILTER_CALL    19
#define EXPR_FILTER_CLOSURE 20
#define EXPR_IMAGE_CALL     21

typedef struct _exprtree
{
    int type;
    tuple_info_t result;
    int tmpvarnum;

    union
    {
	tuple_t *tuple_const;
	variable_t *var;
	internal_t *internal;
	int int_const;
	float float_const;
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
	struct
	{
	    struct _filter_t *filter;
	    struct _exprtree *args;
	} filter_call;
	struct
	{
	    struct _filter_t *filter;
	    struct _exprtree *args;
	} filter_closure;
	struct
	{
	    struct _exprtree *image;
	    struct _exprtree *args;
	} image_call;
    } val;

    struct _exprtree *next;
} exprtree;

#define TOP_LEVEL_FILTER            1
#define TOP_LEVEL_FUNCTION          2

typedef struct _top_level_decl_t
{
    int type;
    char *name;
    char *docstring;
    union
    {
	struct
	{
	    arg_decl_t *args;
	    option_t *options;
	    exprtree *body;
	} filter;
    } v;
} top_level_decl_t;

extern top_level_decl_t *the_top_level_decls;

top_level_decl_t* make_filter_decl (const char *name, const char *docstring, arg_decl_t *args, exprtree *body, option_t *options);

void free_top_level_decl (top_level_decl_t *list);

option_t* make_option (const char *name, option_t *suboptions);
option_t* options_append (option_t *o1, option_t *o2);
option_t* find_option_with_name (option_t *options, const char *name);

arg_decl_t* make_simple_arg_decl (int type, const char *name, const char *docstring, option_t *options);
arg_decl_t* make_filter_arg_decl (const char *name, arg_decl_t *args, const char *docstring, option_t *options);

arg_decl_t* arg_decl_list_append (arg_decl_t *list1, arg_decl_t *list2);

void free_arg_decls (arg_decl_t *list);

limits_t* make_int_limits (int min, int max);
limits_t* make_float_limits (float min, float max);
void free_limits (limits_t *limits);

void apply_limits_to_arg_decl (arg_decl_t *arg_decl, limits_t *limits);
void apply_default_to_arg_decl (arg_decl_t *arg_decl, exprtree *exprtree);

exprtree* make_int_number (int num);
exprtree* make_float_number (float num);
exprtree* make_range (int first, int last);
exprtree* make_var (const char *name); /* should use variable_t instead */
exprtree* make_tuple_exprtree (exprtree *elems);
exprtree* make_select (exprtree *tuple, exprtree *subscripts);
exprtree* make_cast (const char *tagname, exprtree *tuple); /* should use tag number instead */
exprtree* make_convert (const char *tagname, exprtree *tuple); /* ditto */
exprtree* make_function (const char *name, exprtree *args);
exprtree* make_sequence (exprtree *left, exprtree *right);
exprtree* make_assignment (const char *name, exprtree *value); /* should use variable_t instead */
exprtree* make_sub_assignment (char *name, exprtree *subscripts, exprtree *value);
exprtree* make_if_then (exprtree *condition, exprtree *consequent);
exprtree* make_if_then_else (exprtree *condition, exprtree *consequent, exprtree *alternative);
exprtree* make_while (exprtree *invariant, exprtree *body);
exprtree* make_do_while (exprtree *body, exprtree *invariant);
void check_for_start (exprtree *start);
exprtree* make_for (const char *name, exprtree *counter_init, exprtree *start, exprtree *end, exprtree *body);

void free_exprtree (exprtree *tree);

int exprlist_length (exprtree *list);
exprtree* exprlist_append (exprtree *list1, exprtree *list2);

int is_exprtree_single_const (exprtree *tree, int *int_val, float *float_val);
#endif
