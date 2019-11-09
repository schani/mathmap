/*
 * compiler.c
 *
 * MathMap
 *
 * Copyright (C) 2002-2009 Mark Probst
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
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <complex.h>
#include <unistd.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <glib.h>
#include <sys/time.h>
#ifndef OPENSTEP
#include <gmodule.h>
#else
#include <mach-o/dyld.h>
#endif

#include "internals.h"
#include "tags.h"
#include "exprtree.h"
#include "overload.h"
#include "internals.h"
#include "jump.h"
#include "scanner.h"
#include "lispreader/pools.h"
#include "opmacros.h"

#include "compiler-internals.h"

//#define NO_CONSTANTS_ANALYSIS

// defined in compiler_types.h
MAKE_CONST_PRIMARY_FUNCS
MAKE_TYPE_C_TYPE_NAME

static void init_op (int index, char *name, int num_args, type_prop_t type_prop,
		     type_t const_type, int is_pure, int is_foldable, ...);
static int rhs_is_foldable (rhs_t *rhs);

static type_t primary_type (primary_t *primary);

#include <complex.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_sf_gamma.h>
#include <gsl/gsl_sf_ellint.h>
#include <gsl/gsl_sf_elljac.h>

#include "builtins/spec_func.h"
#include "builtins/builtins.h"
#include "builtins/libnoise.h"

#define RHS_ARG(i)                (rhs->v.op.args[(i)])
#define OP_CONST_INT_VAL(i)       ({ assert(RHS_ARG((i)).kind == PRIMARY_CONST); \
				     (RHS_ARG((i)).const_type == TYPE_INT ? RHS_ARG((i)).v.constant.int_value : \
				      ({ g_assert_not_reached(); 0.0; })); })
#define OP_CONST_FLOAT_VAL(i)     ({ assert(RHS_ARG((i)).kind == PRIMARY_CONST); \
				     (RHS_ARG((i)).const_type == TYPE_INT ? (float)(RHS_ARG((i)).v.constant.int_value) : \
				      RHS_ARG((i)).const_type == TYPE_FLOAT ? RHS_ARG((i)).v.constant.float_value : \
				      ({ g_assert_not_reached(); 0.0; })); })
#define OP_CONST_COMPLEX_VAL(i)   ({ assert(RHS_ARG((i)).kind == PRIMARY_CONST); \
				     (RHS_ARG((i)).const_type == TYPE_INT ? (complex float)(RHS_ARG((i)).v.constant.int_value) : \
				      RHS_ARG((i)).const_type == TYPE_FLOAT ? RHS_ARG((i)).v.constant.float_value : \
				      RHS_ARG((i)).const_type == TYPE_COMPLEX ? RHS_ARG((i)).v.constant.complex_value : \
				      ({ g_assert_not_reached(); 0.0; })); })

#define ORIG_VAL_INTERPRETER(x,y,i,f)     ({ color_t c; \
                                 	     if (invocation->antialiasing) \
                                     	         c = get_orig_val_intersample_pixel(invocation, (x), (y), (i), (f)); \
                                 	     else \
                                     		 c = get_orig_val_pixel(invocation, (x), (y), (i), (f)); \
                                 	     /*TUPLE_FROM_COLOR(c);*/ NULL; })

#define APPLY_GRADIENT_INTERPRETER(g,p)	NULL

#define RENDER_INTERPRETER(i,w,h)	NULL

#define OUTPUT_TUPLE_INTERPRETER(t)	0

#define RESIZE_IMAGE_INTERPRETER(i,xf,yf)	NULL
#define STRIP_RESIZE_INTERPRETER(i)		NULL

#define ARG(i)	(invocation->uservals[(i)])

#include "opfuncs.h"

static pools_t compiler_pools;

static operation_t ops[NUM_OPS];

static int next_temp_number = 1;
static int next_compvar_number = 1;

static statement_t *first_stmt = NULL;
static statement_t **emit_loc = &first_stmt;
static statement_t dummy_stmt = { STMT_NIL };

static inlining_history_t *inlining_history = NULL;

static binding_values_t *binding_values = NULL;

static GHashTable *vector_variables = NULL;

#define STMT_STACK_SIZE            64

static statement_t *stmt_stack[STMT_STACK_SIZE];
static int stmt_stackp = 0;

#define CURRENT_STACK_TOP       ((stmt_stackp > 0) ? stmt_stack[stmt_stackp - 1] : 0)
#define UNSAFE_EMIT_STMT(s,l) \
    ({ (s)->parent = CURRENT_STACK_TOP; \
       (s)->next = (l); (l) = (s); })

/*** hash tables ***/

static void
_copy_entry (gpointer key, gpointer value, gpointer user_data)
{
    GHashTable *copy = (GHashTable*)user_data;
    g_hash_table_insert(copy, key, value);
}

static GHashTable*
direct_hash_table_copy (GHashTable *table)
{
    GHashTable *copy = g_hash_table_new(g_direct_hash, g_direct_equal);

    assert(copy != 0);

    g_hash_table_foreach(table, &_copy_entry, copy);

    return copy;
}

/*** value sets ***/

/* This is updated by new_value.  We assume that no new values are generated
 * at the time value sets are used.  */
static int next_value_global_index = 0;

value_set_t*
compiler_new_value_set (void)
{
    return new_bit_vector(next_value_global_index, 0);
}

void
compiler_value_set_add (value_set_t *set, value_t *val)
{
    bit_vector_set(set, val->global_index);
}

void
compiler_value_set_add_set (value_set_t *set, value_set_t *addee)
{
    bit_vector_add(set, addee);
}

gboolean
compiler_value_set_contains (value_set_t *set, value_t *val)
{
    return bit_vector_bit(set, val->global_index);
}

value_set_t*
compiler_value_set_copy (value_set_t *set)
{
    return copy_bit_vector(set);
}

void
compiler_free_value_set (value_set_t *set)
{
    free_bit_vector(set);
}

statement_t*
compiler_stmt_unlink (statement_t **stmtp)
{
    statement_t *stmt = *stmtp;

    g_assert(stmt != NULL);

    *stmtp = stmt->next;
    stmt->next = NULL;
    stmt->parent = NULL;
    return stmt;
}

statement_t**
compiler_stmt_insert_before (statement_t *stmt, statement_t **insertion_point)
{
    g_assert(stmt != NULL && stmt->next == NULL && stmt->parent == NULL);
    g_assert(*insertion_point != NULL);

    stmt->next = *insertion_point;
    stmt->parent = stmt->next->parent;
    *insertion_point = stmt;

    return &stmt->next;
}

int
compiler_op_index (operation_t *op)
{
    return op - ops;
}

#define alloc_stmt()               ((statement_t*)pools_alloc(&compiler_pools, sizeof(statement_t)))
#define alloc_value()              ((value_t*)pools_alloc(&compiler_pools, sizeof(value_t)))
#define alloc_rhs()                ((rhs_t*)pools_alloc(&compiler_pools, sizeof(rhs_t)))
#define alloc_compvar()            (compvar_t*)pools_alloc(&compiler_pools, sizeof(compvar_t))
#define alloc_primary()            (primary_t*)pools_alloc(&compiler_pools, sizeof(primary_t))

static value_t*
new_value (compvar_t *compvar)
{
    value_t *val = alloc_value();

    val->compvar = compvar;	/* dummy value */
    val->global_index = next_value_global_index++;
    val->index = -1;
    val->def = &dummy_stmt;
    val->uses = 0;
    val->const_type = CONST_NONE;
    val->least_const_type_directly_used_in = CONST_MAX;
    val->least_const_type_multiply_used_in = CONST_MAX;
    val->have_defined = 0;
    val->next = 0;

    return val;
}

compvar_t*
make_temporary (type_t type)
{
    temporary_t *temp = (temporary_t*)pools_alloc(&compiler_pools, sizeof(temporary_t));
    compvar_t *compvar = alloc_compvar();
    value_t *val = new_value(compvar);

    temp->number = next_temp_number++;
    temp->last_index = 0;

    compvar->index = next_compvar_number++;
    compvar->var = 0;
    compvar->temp = temp;
    compvar->type = type;
    compvar->current = val;
    compvar->values = val;

    return compvar;
}

static int
compiler_type_from_tuple_info (tuple_info_t *info)
{
    if (info->number == image_tag_number && info->length == 1)
	return TYPE_IMAGE;
    return TYPE_INT;
}

compvar_t*
make_variable (variable_t *var, int n)
{
    compvar_t *compvar = alloc_compvar();
    value_t *val = new_value(compvar);

    compvar->index = next_compvar_number++;
    compvar->var = var;
    compvar->temp = 0;
    compvar->n = n;
    compvar->type = compiler_type_from_tuple_info(&var->type);
    compvar->current = val;
    compvar->values = val;

    return compvar;
}

static compvar_t*
make_tree_vector_variable (variable_t *var)
{
    compvar_t *compvar = alloc_compvar();
    value_t *val = new_value(compvar);

    compvar->index = next_compvar_number++;
    compvar->var = var;
    compvar->temp = 0;
    compvar->n = 0;
    compvar->type = TYPE_TREE_VECTOR;
    compvar->current = val;
    compvar->values = val;

    return compvar;
}

value_t*
make_lhs (compvar_t *compvar)
{
    value_t *val = new_value(compvar);

    val->next = compvar->values;
    compvar->values = val;

    return val;
}

statement_list_t*
prepend_statement (statement_t *stmt, statement_list_t *rest)
{
    statement_list_t *lst = (statement_list_t*)pools_alloc(&compiler_pools, sizeof(statement_list_t));

    lst->stmt = stmt;
    lst->next = rest;

    return lst;
}

void
add_use (value_t *val, statement_t *stmt)
{
    val->uses = prepend_statement(stmt, val->uses);
}

void
remove_use (value_t *val, statement_t *stmt)
{
    statement_list_t **lst = &val->uses;

    while (*lst != 0)
    {
	statement_list_t *elem = *lst;

	if (elem->stmt == stmt)
	{
	    *lst = elem->next;

	    return;
	}

	lst = &(*lst)->next;
    }

    g_assert_not_reached();
}

void
compiler_replace_op_rhs_arg (statement_t *stmt, int arg_num, primary_t new)
{
    g_assert(stmt->kind == STMT_ASSIGN);
    g_assert(stmt->v.assign.rhs->kind == RHS_OP);
    g_assert(arg_num >= 0 && arg_num < stmt->v.assign.rhs->v.op.op->num_args);

    if (stmt->v.assign.rhs->v.op.args[arg_num].kind == PRIMARY_VALUE)
	remove_use(stmt->v.assign.rhs->v.op.args[arg_num].v.value, stmt);

    stmt->v.assign.rhs->v.op.args[arg_num] = new;

    if (new.kind == PRIMARY_VALUE)
	add_use(new.v.value, stmt);
}

value_t*
make_value_copy (value_t *val)
{
    return make_lhs(val->compvar);
}

void
set_value_current (value_t *val, value_t *new_current)
{
    val->compvar->current = new_current;
}

value_t*
current_value (compvar_t *compvar)
{
    return compvar->current;
}

void
assign_value_index_and_make_current (value_t *val)
{
    if (val->compvar->var != 0)
	val->index = ++val->compvar->var->last_index[val->compvar->n];
    else
	val->index = ++val->compvar->temp->last_index;

    set_value_current(val, val);
}

primary_t
make_value_primary (value_t *value)
{
    primary_t primary;

    primary.kind = PRIMARY_VALUE;
    primary.v.value = value;

    return primary;
}

primary_t
make_compvar_primary (compvar_t *compvar)
{
    return make_value_primary(current_value(compvar));
}

rhs_t*
make_int_const_rhs (int int_const)
{
    rhs_t *rhs = alloc_rhs();

    rhs->kind = RHS_PRIMARY;
    rhs->v.primary = make_int_const_primary(int_const);

    return rhs;
}

rhs_t*
make_float_const_rhs (float float_const)
{
    rhs_t *rhs = alloc_rhs();

    rhs->kind = RHS_PRIMARY;
    rhs->v.primary = make_float_const_primary(float_const);

    return rhs;
}

rhs_t*
make_value_rhs (value_t *val)
{
    rhs_t *rhs = alloc_rhs();

    assert(val != 0);

    rhs->kind = RHS_PRIMARY;
    rhs->v.primary.kind = PRIMARY_VALUE;
    rhs->v.primary.v.value = val;

    return rhs;
}

rhs_t*
make_primary_rhs (primary_t primary)
{
    rhs_t *rhs = alloc_rhs();

    rhs->kind = RHS_PRIMARY;
    rhs->v.primary = primary;

    return rhs;
}

rhs_t*
make_compvar_rhs (compvar_t *compvar)
{
    return make_value_rhs(current_value(compvar));
}

rhs_t*
compiler_make_internal_rhs (internal_t *internal)
{
    rhs_t *rhs = alloc_rhs();

    rhs->kind = RHS_INTERNAL;
    rhs->v.internal = internal;

    return rhs;
}

static rhs_t*
make_op_rhs_from_array (int op_index, primary_t *args)
{
    rhs_t *rhs = alloc_rhs();

    rhs->kind = RHS_OP;
    rhs->v.op.op = &ops[op_index];

    memcpy(rhs->v.op.args, args, sizeof(primary_t) * rhs->v.op.op->num_args);

    return rhs;
}

rhs_t*
make_op_rhs (int op_index, ...)
{
    primary_t args[MAX_OP_ARGS];
    va_list ap;
    int i;

    va_start(ap, op_index);
    for (i = 0; i < ops[op_index].num_args; ++i)
	args[i] = va_arg(ap, primary_t);
    va_end(ap);

    return make_op_rhs_from_array(op_index, args);
}

static rhs_t*
make_tuple_rhs_from_array (int length, primary_t *args)
{
    rhs_t *rhs = alloc_rhs();

    rhs->kind = RHS_TUPLE;
    rhs->v.tuple.length = length;
    rhs->v.tuple.args = pools_alloc(&compiler_pools, sizeof(primary_t) * length);

    memcpy(rhs->v.tuple.args, args, sizeof(primary_t) * length);

    return rhs;
}

static rhs_t*
make_tuple_rhs (int length, ...)
{
    primary_t args[length];
    va_list ap;
    int i;

    va_start(ap, length);
    for (i = 0; i < length; ++i)
	args[i] = va_arg(ap, primary_t);
    va_end(ap);

    return make_tuple_rhs_from_array(length, args);
}

static rhs_t*
make_tree_vector_rhs (int length, primary_t *args)
{
    rhs_t *rhs = alloc_rhs();

    rhs->kind = RHS_TREE_VECTOR;
    rhs->v.tuple.length = length;
    rhs->v.tuple.args = pools_alloc(&compiler_pools, sizeof(primary_t) * length);

    memcpy(rhs->v.tuple.args, args, sizeof(primary_t) * length);

    return rhs;
}

int
compiler_num_filter_args (filter_t *filter)
{
    /* uservals, x, y, t */
    return filter->num_uservals + 3;
}

static rhs_t*
make_filter_rhs (filter_t *filter, primary_t *args)
{
    rhs_t *rhs = alloc_rhs();

    rhs->kind = RHS_FILTER;
    rhs->v.filter.filter = filter;
    rhs->v.filter.args = args;
    rhs->v.filter.history = inlining_history;

    return rhs;
}

static rhs_t*
make_closure_rhs (filter_t *filter, primary_t *args)
{
    rhs_t *rhs = alloc_rhs();

    rhs->kind = RHS_CLOSURE;
    rhs->v.closure.filter = filter;
    rhs->v.closure.args = args;
    rhs->v.closure.history = inlining_history;

    return rhs;
}

gboolean
compiler_rhs_is_pure (rhs_t *rhs)
{
    switch (rhs->kind)
    {
	case RHS_OP:
	    return rhs->v.op.op->is_pure;
	case RHS_FILTER :
	    /* FIXME: We can figure this out. */
	    return FALSE;
	default :
	    return TRUE;
    }
}

statement_t*
find_phi_assign (statement_t *stmts, compvar_t *compvar)
{
    for (; stmts != 0; stmts = stmts->next)
    {
	/* we assert this because this function is called before any
	   optimization takes place, hence no statements are changed to
	   nils */

	assert(stmts->kind == STMT_PHI_ASSIGN);

	if (stmts->v.assign.lhs->compvar == compvar)
	    return stmts;
    }

    return 0;
}

primary_t*
get_rhs_primaries (rhs_t *rhs, int *num_primaries)
{
    switch (rhs->kind)
    {
	case RHS_PRIMARY :
	    *num_primaries = 1;
	    return &rhs->v.primary;

	case RHS_INTERNAL :
	    *num_primaries = 0;
	    return NULL;

	case RHS_OP :
	    *num_primaries = rhs->v.op.op->num_args;
	    return rhs->v.op.args;

	case RHS_FILTER :
	    *num_primaries = compiler_num_filter_args(rhs->v.filter.filter);
	    return rhs->v.filter.args;

	case RHS_CLOSURE :
	    *num_primaries = compiler_num_filter_args(rhs->v.closure.filter) - 3;
	    return rhs->v.closure.args;

	case RHS_TUPLE :
	case RHS_TREE_VECTOR :
	    *num_primaries = rhs->v.tuple.length;
	    return rhs->v.tuple.args;

	default :
	    g_assert_not_reached();
    }
}

primary_t*
find_value_in_rhs (value_t *val, rhs_t *rhs)
{
    int num_primaries;
    primary_t *primaries = get_rhs_primaries(rhs, &num_primaries);
    int i;

    for (i = 0; i < num_primaries; ++i)
	if (primaries[i].kind == PRIMARY_VALUE
	    && primaries[i].v.value == val)
	    return &primaries[i];
    return NULL;
}

gboolean
compiler_stmt_is_assign_with_rhs (statement_t *stmt, int rhs_kind)
{
    return stmt->kind == STMT_ASSIGN
	&& stmt->v.assign.rhs->kind == rhs_kind;
}

gboolean
compiler_stmt_is_assign_with_op (statement_t *stmt, int op_index)
{
    return compiler_stmt_is_assign_with_rhs(stmt, RHS_OP)
	&& compiler_op_index(stmt->v.assign.rhs->v.op.op) == op_index;
}

primary_t
compiler_stmt_op_assign_arg (statement_t *stmt, int arg_index)
{
    g_assert (compiler_stmt_is_assign_with_rhs(stmt, RHS_OP));

    return stmt->v.assign.rhs->v.op.args[arg_index];
}

void
compiler_for_each_value_in_rhs (rhs_t *rhs, void (*func) (value_t *value, void *info), void *info)
{
    int num_primaries;
    primary_t *primaries = get_rhs_primaries(rhs, &num_primaries);
    int i;

    for (i = 0; i < num_primaries; ++i)
	if (primaries[i].kind == PRIMARY_VALUE)
	    func(primaries[i].v.value, info);
}

/*
static int
rhs_contains (rhs_t *rhs, value_t *value)
{
    int contained = 0;

    void func (value_t *val)
	{ if (val == value) contained = 1; }

    for_each_value_in_rhs(rhs, &func);

    return contained;
}
*/

static void
for_each_assign_statement (statement_t *stmts, void (*func) (statement_t *stmt, void *info), void *info)
{
    while (stmts != 0)
    {
	switch (stmts->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
	    case STMT_PHI_ASSIGN :
		func(stmts, info);
		break;

	    case STMT_IF_COND :
		for_each_assign_statement(stmts->v.if_cond.consequent, func, info);
		for_each_assign_statement(stmts->v.if_cond.alternative, func, info);
		for_each_assign_statement(stmts->v.if_cond.exit, func, info);
		break;

	    case STMT_WHILE_LOOP :
		for_each_assign_statement(stmts->v.while_loop.entry, func, info);
		for_each_assign_statement(stmts->v.while_loop.body, func, info);
		break;

	    default :
		g_assert_not_reached();
	}

	stmts = stmts->next;
    }
}

#define FOR_EACH_ASSIGN_STATEMENT(stmts,func,...) do { long __clos[] = { __VA_ARGS__ }; for_each_assign_statement((stmts),(func),__clos); } while (0)

static void
_call_func (value_t *value, void *info)
{
    void (*func) (value_t *value, statement_t *stmt, void *info) = CLOSURE_GET(0, void(*)(value_t*, statement_t*, void*));
    CLOSURE_VAR(statement_t*, stmt, 1);
    CLOSURE_VAR(void*, infoinfo, 2);

    func(value, stmt, infoinfo);
}

void
compiler_for_each_value_in_statement (statement_t *stmt, void (*func) (value_t *value, statement_t *stmt, void *info), void *info)
{
    switch (stmt->kind)
    {
	case STMT_NIL :
	    break;

	case STMT_PHI_ASSIGN :
	    COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs2, &_call_func, func, stmt, info);
	case STMT_ASSIGN :
	    COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs, &_call_func, func, stmt, info);
	    func(stmt->v.assign.lhs, stmt, info);
	    break;

	case STMT_IF_COND :
	    COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.if_cond.condition, &_call_func, func, stmt, info);
	    compiler_for_each_value_in_statements(stmt->v.if_cond.consequent, func, info);
	    compiler_for_each_value_in_statements(stmt->v.if_cond.alternative, func, info);
	    compiler_for_each_value_in_statements(stmt->v.if_cond.exit, func, info);
	    break;

	case STMT_WHILE_LOOP :
	    COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.while_loop.invariant, &_call_func, func, stmt, info);
	    compiler_for_each_value_in_statements(stmt->v.while_loop.entry, func, info);
	    compiler_for_each_value_in_statements(stmt->v.while_loop.body, func, info);
	    break;

	default :
	    g_assert_not_reached();
    }
}

void
compiler_for_each_value_in_statements (statement_t *stmt, void (*func) (value_t *value, statement_t *stmt, void *info), void *info)
{
    while (stmt != 0)
    {
	compiler_for_each_value_in_statement(stmt, func, info);
	stmt = stmt->next;
    }
}

/* checks whether stmt is a direct or indirect child of limit.  if stmt ==
 * limit, it does not count as a child */
static int
stmt_is_within_limit (statement_t *stmt, statement_t *limit)
{
    if (limit == 0)
	return 1;

    do
    {
	stmt = stmt->parent;
    } while (stmt != 0 && stmt != limit);

    if (stmt == 0)
	return 0;
    return 1;
}

static statement_t*
last_stmt_of_block (statement_t *stmt)
{
    g_assert(stmt != NULL);

    while (stmt->next != NULL)
	stmt = stmt->next;

    return stmt;
}

static void
_reset_value_have_defined (value_t *value, statement_t *stmt, void *info)
{
    value->have_defined = 0;
}

void
compiler_reset_have_defined (statement_t *stmt)
{
    COMPILER_FOR_EACH_VALUE_IN_STATEMENTS(stmt, &_reset_value_have_defined);
}

/* permanent const values must be calculated once and then be
 * available for the calculation of all less const values */
gboolean
compiler_is_permanent_const_value (value_t *value)
{
    return (value->least_const_type_multiply_used_in | CONST_T) == (value->const_type | CONST_T)
	&& (value->least_const_type_directly_used_in | CONST_T) != (value->const_type | CONST_T);
}

/* temporary const values must be defined for the calculation of all const
 * types up to the least const type they are used in */
gboolean
compiler_is_temporary_const_value (value_t *value)
{
    return !compiler_is_permanent_const_value(value);
}

/* returns whether const_type is at least as const as lower_bound but not more
 * const than upper_bound */
gboolean
compiler_is_const_type_within (int const_type, int lower_bound, int upper_bound)
{
    assert((lower_bound & upper_bound) == lower_bound);

    return (const_type & lower_bound) == lower_bound
	&& (const_type & upper_bound) == const_type;
}

gboolean
compiler_is_value_needed_for_const (value_t *value, int const_type)
{
    return (value->const_type | CONST_T) == (const_type | CONST_T)
	|| (compiler_is_const_type_within(const_type | CONST_T,
					  value->least_const_type_multiply_used_in,
					  value->const_type | CONST_T));
}

/* assumes that old is used at least once in stmt */
static void
rewrite_use (statement_t *stmt, value_t *old, primary_t new)
{
    primary_t *primary;
    statement_list_t **lst;
    int found_one = 0;

    if (new.kind == PRIMARY_VALUE)
	assert(old != new.v.value);

 restart:
    /* remove stmt from the uses list of old */
    lst = &old->uses;
    for (;;)
    {
	statement_list_t *elem = *lst;

	if (elem == 0)
	{
	    /* if we don't find it now we must have done at least one
	       iteration before */
	    assert(found_one);
	    return;
	}

	if (elem->stmt == stmt)
	{
	    *lst = elem->next;
	    found_one = 1;
	    break;
	}

	lst = &elem->next;
    }

    /* now find out where the use is */
    switch (stmt->kind)
    {
	case STMT_ASSIGN :
	    primary = find_value_in_rhs(old, stmt->v.assign.rhs);
	    break;

	case STMT_PHI_ASSIGN :
	    primary = find_value_in_rhs(old, stmt->v.assign.rhs);
	    if (primary == 0)
		primary = find_value_in_rhs(old, stmt->v.assign.rhs2);
	    break;

	case STMT_IF_COND :
	    primary = find_value_in_rhs(old, stmt->v.if_cond.condition);
	    break;

	case STMT_WHILE_LOOP :
	    primary = find_value_in_rhs(old, stmt->v.while_loop.invariant);
	    break;

	default :
	    g_assert_not_reached();
    }

    assert(primary != 0 && primary->v.value == old);

    /* rewrite */
    *primary = new;

    /* add to new use list */
    if (new.kind == PRIMARY_VALUE)
	add_use(new.v.value, stmt);

    /* old might be used more than once in stmt */
    goto restart;
}

static void
rewrite_uses (value_t *old, primary_t new, statement_t *limit)
{
    statement_list_t *lst;

    if (new.kind == PRIMARY_VALUE)
	assert(old != new.v.value);

    lst = old->uses;
    while (lst != 0)
    {
	statement_t *stmt = lst->stmt;

	/* we do not rewrite phis in the loop we're currently working on */
	if (stmt_is_within_limit(stmt, limit)
	    && !(stmt->kind == STMT_PHI_ASSIGN && stmt->parent == limit))
	{
	    rewrite_use(stmt, old, new);
	    /* rewrite_use changes the list, so we have to restart -
	       very inefficient */
	    lst = old->uses;
	}
	else
	    lst = lst->next;
    }
}

static void
rewrite_uses_to_value (value_t *old, value_t *new, statement_t *limit)
{
    primary_t primary;

    if (old == new)
	return;

    primary.kind = PRIMARY_VALUE;
    primary.v.value = new;

    rewrite_uses(old, primary, limit);
}

static void
commit_assign (statement_t *stmt)
{
    statement_t *tos;

    if (stmt_stackp > 0)
    {
	tos = stmt_stack[stmt_stackp - 1];

	switch (tos->kind)
	{
	    case STMT_IF_COND :
		{
		    statement_t *phi_assign = find_phi_assign(tos->v.if_cond.exit,
							      stmt->v.assign.lhs->compvar);

		    if (phi_assign == 0)
		    {
			phi_assign = alloc_stmt();

			phi_assign->kind = STMT_PHI_ASSIGN;

			phi_assign->v.assign.lhs = make_value_copy(stmt->v.assign.lhs);

			phi_assign->v.assign.rhs = make_value_rhs(current_value(stmt->v.assign.lhs->compvar));
			add_use(current_value(stmt->v.assign.lhs->compvar), phi_assign);

			phi_assign->v.assign.rhs2 = make_value_rhs(current_value(stmt->v.assign.lhs->compvar));
			add_use(current_value(stmt->v.assign.lhs->compvar), phi_assign);

			phi_assign->v.assign.old_value = current_value(stmt->v.assign.lhs->compvar);

			phi_assign->v.assign.lhs->def = phi_assign;

			UNSAFE_EMIT_STMT(phi_assign, tos->v.if_cond.exit);
		    }

		    if (tos->v.if_cond.alternative == 0)
		    {
			assert(phi_assign->v.assign.rhs->kind == RHS_PRIMARY
			       && phi_assign->v.assign.rhs->v.primary.kind == PRIMARY_VALUE);
			remove_use(phi_assign->v.assign.rhs->v.primary.v.value, phi_assign);

			phi_assign->v.assign.rhs = make_value_rhs(stmt->v.assign.lhs);
			add_use(stmt->v.assign.lhs, phi_assign);
		    }
		    else
		    {
			assert(phi_assign->v.assign.rhs2->kind == RHS_PRIMARY
			       && phi_assign->v.assign.rhs2->v.primary.kind == PRIMARY_VALUE);
			remove_use(phi_assign->v.assign.rhs2->v.primary.v.value, phi_assign);

			phi_assign->v.assign.rhs2 = make_value_rhs(stmt->v.assign.lhs);
			add_use(stmt->v.assign.lhs, phi_assign);
		    }
		}
		break;

	    case STMT_WHILE_LOOP :
		{
		    statement_t *phi_assign = find_phi_assign(tos->v.while_loop.entry, stmt->v.assign.lhs->compvar);

		    if (phi_assign == 0)
		    {
			phi_assign = alloc_stmt();

			phi_assign->kind = STMT_PHI_ASSIGN;

			phi_assign->v.assign.lhs = make_value_copy(stmt->v.assign.lhs);

			phi_assign->v.assign.rhs = make_value_rhs(current_value(stmt->v.assign.lhs->compvar));

			add_use(current_value(stmt->v.assign.lhs->compvar), phi_assign);

			phi_assign->v.assign.lhs->def = phi_assign;

			UNSAFE_EMIT_STMT(phi_assign, tos->v.while_loop.entry);

			phi_assign->v.assign.rhs2 = make_value_rhs(stmt->v.assign.lhs);
			add_use(stmt->v.assign.lhs, phi_assign);

			phi_assign->v.assign.old_value = current_value(stmt->v.assign.lhs->compvar);

			rewrite_uses_to_value(current_value(stmt->v.assign.lhs->compvar), phi_assign->v.assign.lhs, tos);
		    }
		    else
		    {
			assert(phi_assign->v.assign.rhs2->kind == RHS_PRIMARY
			       && phi_assign->v.assign.rhs2->v.primary.kind == PRIMARY_VALUE);
			remove_use(phi_assign->v.assign.rhs2->v.primary.v.value, phi_assign);

			phi_assign->v.assign.rhs2 = make_value_rhs(stmt->v.assign.lhs);
			add_use(stmt->v.assign.lhs, phi_assign);
		    }
		}
		break;

	    default :
		g_assert_not_reached();
	}
    }

    assign_value_index_and_make_current(stmt->v.assign.lhs);
}


static void
_add_use_in_stmt (value_t *value, void *info)
{
    add_use(value, CLOSURE_GET(0, statement_t*));
}

static void
insert_stmt_before (statement_t *stmt, statement_t **loc)
{
    g_assert(stmt->next == NULL);

    stmt->next = *loc;
    *loc = stmt;
}

static void
record_stmt_def_uses (statement_t *stmt)
{
    switch (stmt->kind)
    {
	case STMT_NIL :
	    break;

	case STMT_ASSIGN :
	    COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs, &_add_use_in_stmt, stmt);
	    stmt->v.assign.lhs->def = stmt;
	    break;

	case STMT_PHI_ASSIGN :
	    COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs, &_add_use_in_stmt, stmt);
	    COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs2, &_add_use_in_stmt, stmt);
	    stmt->v.assign.lhs->def = stmt;
	    break;

	case STMT_IF_COND :
	    COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.if_cond.condition, &_add_use_in_stmt, stmt);
	    break;

	case STMT_WHILE_LOOP :
	    COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.while_loop.invariant, &_add_use_in_stmt, stmt);
	    break;

	default :
	    g_assert_not_reached();
    }
}

void
emit_stmt (statement_t *stmt)
{
    stmt->parent = CURRENT_STACK_TOP;

    insert_stmt_before(stmt, emit_loc);
    emit_loc = &stmt->next;

    record_stmt_def_uses(stmt);
}

statement_t**
compiler_emit_stmt_before (statement_t *stmt, statement_t **loc, statement_t *parent)
{
    stmt->parent = parent;
    insert_stmt_before(stmt, loc);

    record_stmt_def_uses(stmt);
    if (stmt->kind == STMT_ASSIGN)
	assign_value_index_and_make_current(stmt->v.assign.lhs);

    return &stmt->next;
}

void
emit_nil (void)
{
    statement_t *stmt = alloc_stmt();

    stmt->kind = STMT_NIL;
    stmt->next = 0;

    emit_stmt(stmt);
}

statement_t*
make_assign (value_t *lhs, rhs_t *rhs)
{
    statement_t *stmt = alloc_stmt();

    stmt->kind = STMT_ASSIGN;
    stmt->next = 0;

    stmt->v.assign.lhs = lhs;
    stmt->v.assign.rhs = rhs;

    return stmt;
}

void
emit_assign (value_t *lhs, rhs_t *rhs)
{
    statement_t *stmt = make_assign(lhs, rhs);

    emit_stmt(stmt);

    commit_assign(stmt);
}

void
start_if_cond (rhs_t *condition)
{
    statement_t *stmt = alloc_stmt();

    stmt->kind = STMT_IF_COND;
    stmt->next = 0;

    stmt->v.if_cond.condition = condition;
    stmt->v.if_cond.consequent = 0;
    stmt->v.if_cond.alternative = 0;
    stmt->v.if_cond.exit = 0;

    emit_stmt(stmt);
    stmt_stack[stmt_stackp++] = stmt;

    emit_loc = &stmt->v.if_cond.consequent;
}

static void
reset_values_for_phis (statement_t *phi, int delete)
{
    for (; phi != 0; phi = phi->next)
    {
	assert(phi->kind == STMT_PHI_ASSIGN);

	set_value_current(phi->v.assign.lhs, phi->v.assign.old_value);

	if (delete)
	    phi->v.assign.old_value = 0;
    }
}

void
switch_if_branch (void)
{
    statement_t *stmt;

    assert(stmt_stackp > 0);

    stmt = stmt_stack[stmt_stackp - 1];

    assert(stmt->kind == STMT_IF_COND && stmt->v.if_cond.alternative == 0);

    if (stmt->v.if_cond.consequent == 0)
	emit_nil();

    reset_values_for_phis(stmt->v.if_cond.exit, 0);

    emit_loc = &stmt->v.if_cond.alternative;
}

void
end_if_cond (void)
{
    statement_t *stmt, *phi;

    assert(stmt_stackp > 0);

    stmt = stmt_stack[stmt_stackp - 1];

    assert(stmt->kind == STMT_IF_COND && stmt->v.if_cond.consequent != 0);

    if (stmt->v.if_cond.alternative == 0)
	emit_nil();

    if (stmt->v.if_cond.exit == 0)
    {
	statement_t *nil = alloc_stmt();

	nil->kind = STMT_NIL;

	UNSAFE_EMIT_STMT(nil, stmt->v.if_cond.exit);
    }

    --stmt_stackp;

    reset_values_for_phis(stmt->v.if_cond.exit, 1);

    for (phi = stmt->v.if_cond.exit; phi != 0; phi = phi->next)
    {
	assert(phi->kind == STMT_PHI_ASSIGN);

	commit_assign(phi);
    }

    emit_loc = &stmt->next;
}

void
start_while_loop (rhs_t *invariant)
{
    statement_t *stmt = alloc_stmt();
    value_t *value;
    statement_t *phi_assign;

    stmt->kind = STMT_WHILE_LOOP;
    stmt->next = 0;

    stmt->v.while_loop.entry = 0;
    stmt->v.while_loop.body = 0;

    assert(invariant->kind == RHS_PRIMARY && invariant->v.primary.kind == PRIMARY_VALUE);

    value = invariant->v.primary.v.value;

    phi_assign = alloc_stmt();

    phi_assign->kind = STMT_PHI_ASSIGN;
    phi_assign->v.assign.lhs = make_value_copy(value);
    phi_assign->v.assign.rhs = make_value_rhs(current_value(value->compvar));
    phi_assign->v.assign.rhs2 = make_value_rhs(current_value(value->compvar));

    add_use(current_value(value->compvar), phi_assign);
    add_use(current_value(value->compvar), phi_assign);

    phi_assign->v.assign.lhs->def = phi_assign;

    assign_value_index_and_make_current(phi_assign->v.assign.lhs);

    stmt->v.while_loop.invariant = make_value_rhs(current_value(value->compvar));

    emit_stmt(stmt);
    stmt_stack[stmt_stackp++] = stmt;

    UNSAFE_EMIT_STMT(phi_assign, stmt->v.while_loop.entry);

    emit_loc = &stmt->v.while_loop.body;
}

void
end_while_loop (void)
{
    statement_t *stmt, *phi;

    assert(stmt_stackp > 0);

    stmt = stmt_stack[--stmt_stackp];

    assert(stmt->kind == STMT_WHILE_LOOP);

    if (stmt->v.while_loop.body == 0)
	emit_nil();

    reset_values_for_phis(stmt->v.while_loop.entry, 1);

    for (phi = stmt->v.while_loop.entry; phi != 0; phi = phi->next)
    {
	assert(phi->kind == STMT_PHI_ASSIGN);

	commit_assign(phi);
    }

    emit_loc = &stmt->next;
}

/*** inline history ***/

static inlining_history_t*
push_inlined_filter (filter_t *filter, inlining_history_t *old)
{
    inlining_history_t *new = (inlining_history_t*)pools_alloc(&compiler_pools, sizeof(inlining_history_t));

    new->filter = filter;
    new->next = old;

    return new;
}

/*** uservals ***/

static userval_representation_t*
lookup_userval_representation (int userval_type)
{
    static userval_representation_t reps[] =
        {
	    { USERVAL_INT_CONST, TYPE_INT, 1, OP_USERVAL_INT },
	    { USERVAL_FLOAT_CONST, TYPE_FLOAT, 1, OP_USERVAL_FLOAT },
	    { USERVAL_BOOL_CONST, TYPE_INT, 1, OP_USERVAL_BOOL },
	    { USERVAL_COLOR, TYPE_COLOR, 1, OP_USERVAL_COLOR },
	    { USERVAL_CURVE, TYPE_CURVE, 1, OP_USERVAL_CURVE },
	    { USERVAL_GRADIENT, TYPE_GRADIENT, 1, OP_USERVAL_GRADIENT },
	    { USERVAL_IMAGE, TYPE_IMAGE, 1, OP_USERVAL_IMAGE },
	    { -1, -1, -1 }
	};

    int i;

    for (i = 0; reps[i].userval_type != -1; ++i)
	if (reps[i].userval_type == userval_type)
	    return &reps[i];
    return NULL;
}

static binding_values_t*
lookup_binding_values (int kind, gpointer key)
{
    binding_values_t *bv;

    for (bv = binding_values; bv != NULL; bv = bv->next)
	if (bv->kind == kind && bv->key == key)
	    return bv;
    return NULL;
}

/*** debug printing ***/

static void
print_indent (int indent)
{
    int i;

    for (i = 0; i < indent; ++i)
	fputs("  ", stdout);
}

static void
print_curve (curve_t *curve)
{
    printf("CURVE");
}

static void
print_gradient (gradient_t *gradient)
{
    printf("GRADIENT");
}

static void
print_image (image_t *image)
{
    switch (image->type)
    {
	case IMAGE_DRAWABLE :
	    printf("DRAWABLE(%p)", image->v.drawable);
	case IMAGE_CLOSURE :
	    printf("CLOSURE()");
	default :
	    g_assert_not_reached();
    }
}

static void
print_tuple (float *tuple)
{
    g_assert_not_reached();
}

static void
print_tree_vector (tree_vector_t *tree_vector)
{
    g_assert_not_reached ();
}

char*
compiler_get_value_name (value_t *val)
{
    if (val->compvar->var != 0)
	return g_strdup_printf("%s[%d]_%d", val->compvar->var->name, val->compvar->n, val->index);
    else
	return g_strdup_printf("__t%d_%d", val->compvar->temp->number, val->index);
}

void
compiler_print_value (value_t *val)
{
    if (val->compvar->var != 0)
	printf("%s[%d]_%d", val->compvar->var->name, val->compvar->n, val->index);
    else
	printf("$t%d_%d", val->compvar->temp->number, val->index);
}

static void
print_primary (primary_t *primary)
{
    FILE *out = stdout;

    switch (primary->kind)
    {
	case PRIMARY_VALUE :
	    compiler_print_value(primary->v.value);
	    break;

	case PRIMARY_CONST :
	    switch (primary->const_type)
	    {
		TYPE_DEBUG_PRINTER

		default :
		    g_assert_not_reached();
	    }
	    break;

	default :
	    g_assert_not_reached();
    }
}

static void
print_rhs (rhs_t *rhs)
{
    switch (rhs->kind)
    {
	case RHS_PRIMARY :
	    print_primary(&rhs->v.primary);
	    break;

	case RHS_INTERNAL :
	    printf("%s", rhs->v.internal->name);
	    break;

	case RHS_OP :
	    {
		int i;

		printf("%s", rhs->v.op.op->name);
		for (i = 0; i < rhs->v.op.op->num_args; ++i)
		{
		    printf(" ");
		    print_primary(&rhs->v.op.args[i]);
		}
	    }
	    break;

	case RHS_FILTER :
	    {
		int num_args = compiler_num_filter_args(rhs->v.filter.filter);
		int i;

		printf("filter_%s", rhs->v.filter.filter->name);
		for (i = 0; i < num_args; ++i)
		{
		    printf(" ");
		    print_primary(&rhs->v.filter.args[i]);
		}
	    }
	    break;

	case RHS_CLOSURE :
	    {
		int num_args = compiler_num_filter_args(rhs->v.closure.filter);
		int i;

		printf("closure_%s", rhs->v.closure.filter->name);
		for (i = 0; i < num_args - 3; ++i)
		{
		    printf(" ");
		    print_primary(&rhs->v.closure.args[i]);
		}
	    }
	    break;

	case RHS_TUPLE :
	case RHS_TREE_VECTOR :
	    {
		int i;

		printf(rhs->kind == RHS_TUPLE ? "tuple" : "tree vector");
		for (i = 0; i < rhs->v.tuple.length; ++i)
		{
		    printf(" ");
		    print_primary(&rhs->v.tuple.args[i]);
		}
	    }
	    break;

	default :
	    g_assert_not_reached();
    }
}

static void
output_const_type (FILE *out, unsigned int const_type)
{
    if (const_type & CONST_X)
	fputs("x", out);
    if (const_type & CONST_Y)
	fputs("y", out);
    if (const_type & CONST_T)
	fputs("t", out);
    if (const_type == CONST_NONE)
	fputs("-", out);
}

static int
count_uses (value_t *val)
{
    statement_list_t *lst;
    int num_uses = 0;

    for (lst = val->uses; lst != 0; lst = lst->next)
	++num_uses;

    return num_uses;
}

void
compiler_print_assign_statement (statement_t *stmt)
{
    switch (stmt->kind)
    {
	case STMT_ASSIGN :
	    compiler_print_value(stmt->v.assign.lhs);
	    printf(" (%s  uses %d) = ", type_c_type_name(stmt->v.assign.lhs->compvar->type), count_uses(stmt->v.assign.lhs));
	    print_rhs(stmt->v.assign.rhs);
	    printf("   ");
	    output_const_type(stdout, stmt->v.assign.lhs->const_type);
	    printf(" ");
	    output_const_type(stdout, stmt->v.assign.lhs->least_const_type_directly_used_in);
	    printf(" ");
	    output_const_type(stdout, stmt->v.assign.lhs->least_const_type_multiply_used_in);
	    break;

	case STMT_PHI_ASSIGN :
	    compiler_print_value(stmt->v.assign.lhs);
	    printf(" (%s  uses %d) = phi(", type_c_type_name(stmt->v.assign.lhs->compvar->type), count_uses(stmt->v.assign.lhs));
	    print_rhs(stmt->v.assign.rhs);
	    printf(", ");
	    print_rhs(stmt->v.assign.rhs2);
	    printf(")   ");
	    output_const_type(stdout, stmt->v.assign.lhs->const_type);
	    printf(" ");
	    output_const_type(stdout, stmt->v.assign.lhs->least_const_type_directly_used_in);
	    printf(" ");
	    output_const_type(stdout, stmt->v.assign.lhs->least_const_type_multiply_used_in);
	    break;

	default :
	    g_assert_not_reached();
    }
}

static void
dump_code (statement_t *stmt, int indent)
{
    while (stmt != 0)
    {
	if (stmt->kind != STMT_NIL)
	    print_indent(indent);

	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
	    case STMT_PHI_ASSIGN :
		compiler_print_assign_statement(stmt);
		printf("\n");
		break;

	    case STMT_IF_COND :
		printf("if ");
		print_rhs(stmt->v.if_cond.condition);
		printf("\n");
		dump_code(stmt->v.if_cond.consequent, indent + 1);
		print_indent(indent);
		printf("else\n");
		dump_code(stmt->v.if_cond.alternative, indent + 1);
		print_indent(indent);
		printf("exit\n");
		dump_code(stmt->v.if_cond.exit, indent + 1);
		break;

	    case STMT_WHILE_LOOP :
		printf("start while");
		printf("\n");
		dump_code(stmt->v.while_loop.entry, indent + 1);
		print_indent(indent);
		printf("while ");
		print_rhs(stmt->v.while_loop.invariant);
		printf("\n");
		dump_code(stmt->v.while_loop.body, indent + 1);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

/*** ssa generation from tree code ***/

static gboolean
needs_xy_scaling (int flags)
{
    return (flags & (IMAGE_FLAG_UNIT | IMAGE_FLAG_SQUARE)) != IMAGE_FLAG_UNIT;
}

static value_t*
resize_image_if_necessary (primary_t image, int flags)
{
    compvar_t *pixel_width, *pixel_height, *x_factor, *y_factor, *resized_image;

    resized_image = make_temporary(TYPE_IMAGE);

    if (!needs_xy_scaling(flags))
    {
	emit_assign(make_lhs(resized_image), make_primary_rhs(image));
	return current_value(resized_image);
    }

    pixel_width = make_temporary(TYPE_INT);
    pixel_height = make_temporary(TYPE_INT);
    x_factor = make_temporary(TYPE_INT);
    y_factor = make_temporary(TYPE_INT);

    emit_assign(make_lhs(pixel_width), make_op_rhs(OP_IMAGE_PIXEL_WIDTH, image));
    emit_assign(make_lhs(pixel_height), make_op_rhs(OP_IMAGE_PIXEL_HEIGHT, image));

    switch (flags)
    {
	case 0 :
	    emit_assign(make_lhs(x_factor),
			make_op_rhs(OP_DIV, make_int_const_primary(2),
				    make_compvar_primary(pixel_width)));
	    emit_assign(make_lhs(y_factor),
			make_op_rhs(OP_DIV, make_int_const_primary(2),
				    make_compvar_primary(pixel_height)));
	    break;

	case IMAGE_FLAG_UNIT | IMAGE_FLAG_SQUARE :
	    {
		compvar_t *max_dim = make_temporary(TYPE_INT);

		emit_assign(make_lhs(max_dim), make_op_rhs(OP_MAX,
							   make_compvar_primary(pixel_width),
							   make_compvar_primary(pixel_height)));
		emit_assign(make_lhs(x_factor), make_op_rhs(OP_DIV,
							    make_compvar_primary(max_dim),
							    make_compvar_primary(pixel_width)));
		emit_assign(make_lhs(y_factor), make_op_rhs(OP_DIV,
							    make_compvar_primary(max_dim),
							    make_compvar_primary(pixel_height)));
	    }
	    break;

	default :
	    g_assert_not_reached();
    }

    emit_assign(make_lhs(resized_image), make_op_rhs(OP_STRIP_RESIZE, image));
    emit_assign(make_lhs(resized_image),
		make_op_rhs(OP_RESIZE_IMAGE, make_compvar_primary(resized_image),
			    make_compvar_primary(x_factor), make_compvar_primary(y_factor)));

    return current_value(resized_image);
}

static void
alloc_var_compvars_if_needed (variable_t *var)
{
    int i;

    if (g_hash_table_lookup(vector_variables, var))
    {
	if (var->compvar[0] == NULL)
	    var->compvar[0] = make_tree_vector_variable(var);
	return;
    }

    for (i = 0; i < var->type.length; ++i)
	if (var->compvar[i] == NULL)
	    var->compvar[i] = make_variable(var, i);
}

static void gen_code (filter_t *filter, exprtree *tree, compvar_t **dest, int is_alloced);

static compvar_t***
gen_args (filter_t *filter, exprtree *arg_trees, int **_arglengths, int **_argnumbers)
{
    exprtree *arg;
    int num_args = 0;
    compvar_t ***args;
    int *arglengths, *argnumbers;
    int i;

    for (arg = arg_trees; arg != 0; arg = arg->next)
	++num_args;

    args = (compvar_t***)pools_alloc(&compiler_pools, num_args * sizeof(compvar_t**));
    arglengths = (int*)pools_alloc(&compiler_pools, num_args * sizeof(int));
    argnumbers = (int*)pools_alloc(&compiler_pools, num_args * sizeof(int));

    for (i = 0, arg = arg_trees; i < num_args; ++i, arg = arg->next)
    {
	args[i] = (compvar_t**)pools_alloc(&compiler_pools, arg->result.length * sizeof(compvar_t*));
	arglengths[i] = arg->result.length;
	argnumbers[i] = arg->result.number;
	gen_code(filter, arg, args[i], 0);
    }

    *_arglengths = arglengths;
    *_argnumbers = argnumbers;

    return args;
}

static void
gen_deconstruct_color (compvar_t *color, compvar_t **dest, gboolean is_alloced)
{
    static int color_ops[] = { OP_RED, OP_GREEN, OP_BLUE, OP_ALPHA };

    int i;

    for (i = 0; i < 4; ++i)
    {
	if (!is_alloced)
	    dest[i] = make_temporary(TYPE_FLOAT);
	emit_assign(make_lhs(dest[i]), make_op_rhs(color_ops[i], make_compvar_primary(color)));
    }
}

static compvar_t*
gen_tree_vector (filter_t *filter, exprtree *tree, compvar_t **dest, gboolean is_alloced)
{
    if (tree->type == EXPR_VARIABLE && g_hash_table_lookup(vector_variables, tree->val.var))
    {
	compvar_t *tree_vector = tree->val.var->compvar[0];
	int i;

	for (i = 0; i < tree->result.length; ++i)
	{
	    if (!is_alloced)
		dest[i] = make_temporary(TYPE_FLOAT);
	    emit_assign(make_lhs(dest[i]), make_op_rhs(OP_TREE_VECTOR_NTH,
						       make_int_const_primary(i),
						       make_compvar_primary(tree_vector)));
	}

	return tree_vector;
    }
    else
    {
	compvar_t *temp;
	primary_t args[tree->result.length];
	int i;

	gen_code(filter, tree, dest, is_alloced);

	for (i = 0; i < tree->result.length; ++i)
	    args[i] = make_compvar_primary(dest[i]);
	temp = make_temporary(TYPE_TREE_VECTOR);
	emit_assign(make_lhs(temp), make_tree_vector_rhs(tree->result.length, args));

	return temp;
    }
}

static void
gen_code (filter_t *filter, exprtree *tree, compvar_t **dest, int is_alloced)
{
    int i;

    switch (tree->type)
    {
	case EXPR_INT_CONST :
	    if (!is_alloced)
		dest[0] = make_temporary(TYPE_INT);
	    emit_assign(make_lhs(dest[0]), make_int_const_rhs(tree->val.int_const));
	    break;

	case EXPR_FLOAT_CONST :
	    if (!is_alloced)
		dest[0] = make_temporary(TYPE_FLOAT);
	    emit_assign(make_lhs(dest[0]), make_float_const_rhs(tree->val.float_const));
	    break;

	case EXPR_TUPLE_CONST :
	    for (i = 0; i < tree->val.tuple_const->length; ++i)
	    {
		if (!is_alloced)
		    dest[i] = make_temporary(TYPE_FLOAT);
		emit_assign(make_lhs(dest[i]), make_float_const_rhs(tree->val.tuple_const->data[i]));
	    }
	    break;

	case EXPR_TUPLE :
	    {
		exprtree *elem;

		for (i = 0, elem = tree->val.tuple.elems; elem != 0; ++i, elem = elem->next)
		    gen_code(filter, elem, dest + i, is_alloced);
	    }
	    break;

	case EXPR_SELECT :
	    {
		compvar_t *temps[tree->val.select.tuple->result.length];
		exprtree *sub;
		int i;
		compvar_t *tree_vector = NULL;

		if (g_hash_table_lookup(vector_variables, tree))
		    tree_vector = gen_tree_vector(filter, tree->val.select.tuple, temps, FALSE);
		else
		    gen_code(filter, tree->val.select.tuple, temps, FALSE);

		g_assert(tree->val.select.subscripts->type == EXPR_TUPLE);
		for (sub = tree->val.select.subscripts->val.tuple.elems, i = 0; sub != 0; sub = sub->next, ++i)
		{
		    int subscript;

		    if (is_exprtree_single_const(sub, &subscript, 0))
		    {
			if (subscript < 0)
			    subscript = 0;
			if (subscript >= tree->val.select.tuple->result.length)
			    subscript = tree->val.select.tuple->result.length - 1;

			if (!is_alloced)
			    dest[i] = temps[subscript];
			else
			    emit_assign(make_lhs(dest[i]), make_compvar_rhs(temps[subscript]));
		    }
		    else
		    {
			compvar_t *subscript;

			g_assert(tree_vector);

			if (!is_alloced)
			    dest[i] = make_temporary(TYPE_INT);

			gen_code(filter, sub, &subscript, FALSE);

			emit_assign(make_lhs(dest[i]), make_op_rhs(OP_TREE_VECTOR_NTH,
								   make_compvar_primary(subscript),
								   make_compvar_primary(tree_vector)));
		    }
		}
	    }
	    break;

	case EXPR_VARIABLE :
	    alloc_var_compvars_if_needed(tree->val.var);
	    if (g_hash_table_lookup(vector_variables, tree->val.var))
		for (i = 0; i < tree->val.var->type.length; ++i)
		{
		    if (!is_alloced)
			dest[i] = make_temporary(TYPE_INT);
		    emit_assign(make_lhs(dest[i]), make_op_rhs(OP_TREE_VECTOR_NTH,
							       make_int_const_primary(i),
							       make_compvar_primary(tree->val.var->compvar[0])));
		}
	    else
		for (i = 0; i < tree->val.var->type.length; ++i)
		    if (!is_alloced)
			dest[i] = tree->val.var->compvar[i];
		    else
			emit_assign(make_lhs(dest[i]), make_compvar_rhs(tree->val.var->compvar[i]));
	    break;

	case EXPR_INTERNAL :
	    {
		binding_values_t *bv = lookup_binding_values(BINDING_INTERNAL,
							     tree->val.internal);

		if (!is_alloced)
		    dest[0] = make_temporary(TYPE_INT);
		if (bv != NULL)
		    emit_assign(make_lhs(dest[0]), make_value_rhs(bv->values[0]));
		else
		    emit_assign(make_lhs(dest[0]), compiler_make_internal_rhs(tree->val.internal));
	    }
	    break;

	case EXPR_ASSIGNMENT :
	    alloc_var_compvars_if_needed(tree->val.assignment.var);
	    if (g_hash_table_lookup(vector_variables, tree->val.assignment.var))
	    {
		compvar_t *tree_vector = gen_tree_vector(filter, tree->val.assignment.value, dest, is_alloced);
		emit_assign(make_lhs(tree->val.assignment.var->compvar[0]), make_compvar_rhs(tree_vector));
	    }
	    else
	    {
		gen_code(filter, tree->val.assignment.value, tree->val.assignment.var->compvar, TRUE);
		for (i = 0; i < tree->result.length; ++i)
		    if (is_alloced)
			emit_assign(make_lhs(dest[i]), make_compvar_rhs(tree->val.assignment.var->compvar[i]));
		    else
			dest[i] = tree->val.assignment.var->compvar[i];
	    }
	    break;

	case EXPR_SUB_ASSIGNMENT :
	    {
		compvar_t *temps[tree->val.sub_assignment.value->result.length];
		exprtree *sub;
		int i;
		gboolean is_tree_vector = g_hash_table_lookup(vector_variables, tree->val.sub_assignment.var) != NULL;

		alloc_var_compvars_if_needed(tree->val.sub_assignment.var);

		gen_code(filter, tree->val.sub_assignment.value, temps, 0);
		g_assert(tree->val.sub_assignment.subscripts->type == EXPR_TUPLE);

		for (sub = tree->val.sub_assignment.subscripts->val.tuple.elems, i = 0; sub != 0; sub = sub->next, ++i)
		{
		    int subscript;

		    if (is_tree_vector)
		    {
			compvar_t *tree_vector = tree->val.sub_assignment.var->compvar[0];
			compvar_t *subscript;

			gen_code(filter, sub, &subscript, FALSE);

			emit_assign(make_lhs(tree_vector), make_op_rhs(OP_SET_TREE_VECTOR_NTH,
								       make_compvar_primary(subscript),
								       make_compvar_primary(tree_vector),
								       make_compvar_primary(temps[i])));
		    }
		    else if (is_exprtree_single_const(sub, &subscript, 0))
		    {
			if (subscript < 0)
			    subscript = 0;
			if (subscript >= tree->val.sub_assignment.var->type.length)
			    subscript = tree->val.sub_assignment.var->type.length - 1;

			emit_assign(make_lhs(tree->val.sub_assignment.var->compvar[subscript]), make_compvar_rhs(temps[i]));
		    }
		    else
			g_assert_not_reached ();

		    if (is_alloced)
			emit_assign(make_lhs(dest[i]), make_compvar_rhs(temps[i]));
		    else
			dest[i] = temps[i];
		}
	    }
	    break;

	case EXPR_CAST :
	    gen_code(filter, tree->val.cast.tuple, dest, is_alloced);
	    break;

	case EXPR_FUNC :
	    {
		compvar_t ***args;
		int *arglengths, *argnumbers;

		args = gen_args(filter, tree->val.func.args, &arglengths, &argnumbers);

		if (!is_alloced)
		    for (i = 0; i < tree->result.length; ++i)
			dest[i] = make_temporary(compiler_type_from_tuple_info(&tree->result));

		tree->val.func.entry->v.builtin_generator(filter, args, arglengths, argnumbers, dest);
	    }
	    break;

	case EXPR_SEQUENCE :
	    {
		compvar_t **left_result;

		left_result = (compvar_t**)alloca(tree->val.op.left->result.length * sizeof(compvar_t*));
		gen_code(filter, tree->val.op.left, left_result, 0);

		gen_code(filter, tree->val.op.right, dest, is_alloced);
	    }
	    break;

	case EXPR_IF_THEN :
	case EXPR_IF_THEN_ELSE :
	    {
		compvar_t *condition;
		compvar_t **result = (compvar_t**)alloca(tree->result.length * sizeof(compvar_t*));

		for (i = 0; i < tree->result.length; ++i)
		    result[i] = make_temporary(compiler_type_from_tuple_info(&tree->result));

		gen_code(filter, tree->val.ifExpr.condition, &condition, 0);

		start_if_cond(make_compvar_rhs(condition));

		gen_code(filter, tree->val.ifExpr.consequent, result, 1);

		switch_if_branch();

		if (tree->type == EXPR_IF_THEN_ELSE)
		    gen_code(filter, tree->val.ifExpr.alternative, result, 1);

		end_if_cond();

		for (i = 0; i < tree->result.length; ++i)
		    if (is_alloced)
			emit_assign(make_lhs(dest[i]), make_compvar_rhs(result[i]));
		    else
			dest[i] = result[i];
	    }
	    break;

	case EXPR_DO_WHILE :
	case EXPR_WHILE :
	    {
		compvar_t *invariant = make_temporary(TYPE_INT);
		compvar_t **body_result = (compvar_t**)alloca(tree->val.whileExpr.body->result.length * sizeof(compvar_t*));

		if (tree->type == EXPR_DO_WHILE)
		    gen_code(filter, tree->val.whileExpr.body, body_result, 0);

		gen_code(filter, tree->val.whileExpr.invariant, &invariant, 1);
		start_while_loop(make_compvar_rhs(invariant));
		gen_code(filter, tree->val.whileExpr.body, body_result, 0);
		gen_code(filter, tree->val.whileExpr.invariant, &invariant, 1);
		end_while_loop();

		if (!is_alloced)
		    dest[0] = make_temporary(TYPE_INT);
		emit_assign(make_lhs(dest[0]), make_int_const_rhs(0));
	    }
	    break;

	case EXPR_USERVAL :
	    if (tree->val.userval.info->type == USERVAL_COLOR)
	    {
		compvar_t *temp = make_temporary(TYPE_INT);

		emit_assign(make_lhs(temp),
			    make_op_rhs(OP_USERVAL_COLOR,
					make_int_const_primary(tree->val.userval.info->index)));

		gen_deconstruct_color(temp, dest, is_alloced);
	    }
	    else
	    {
		userval_representation_t *rep = lookup_userval_representation(tree->val.userval.info->type);
		binding_values_t *bv = lookup_binding_values(BINDING_USERVAL, tree->val.userval.info);

		g_assert(rep != NULL);
		g_assert(bv != NULL);

		if (!is_alloced)
		    dest[0] = make_temporary(rep->var_type);
		emit_assign(make_lhs(dest[0]), make_value_rhs(bv->values[0]));
	    }
	    break;

	case EXPR_FILTER_CLOSURE :
	    {
		compvar_t ***args;
		int *arglengths, *argnumbers;
		primary_t *arg_primaries;
		int num_args = compiler_num_filter_args(tree->val.filter_closure.filter) - 3;
		userval_info_t *infos = tree->val.filter_closure.filter->userval_infos;
		userval_info_t *info;
		int i;
		compvar_t *image = make_temporary(TYPE_IMAGE);
		value_t *resized_image;

		args = gen_args(filter, tree->val.filter_closure.args, &arglengths, &argnumbers);

		arg_primaries = (primary_t*)pools_alloc(&compiler_pools, sizeof(primary_t) * num_args);

		for (i = 0, info = infos;
		     i < num_args;
		     ++i, info = info->next)
		{
		    compvar_t *compvar;

		    if (info->type == USERVAL_COLOR)
		    {
			g_assert(arglengths[i] == 4 && argnumbers[i] == rgba_tag_number);

			compvar = make_temporary(TYPE_COLOR);
			emit_assign(make_lhs(compvar), make_op_rhs(OP_MAKE_RGBA_COLOR,
								   make_compvar_primary(args[i][0]),
								   make_compvar_primary(args[i][1]),
								   make_compvar_primary(args[i][2]),
								   make_compvar_primary(args[i][3])));
		    }
		    else if (info->type == USERVAL_IMAGE)
		    {
			/* FIXME: only do this if necessary, i.e. if
			   the filter works with resized images */

			compvar = make_temporary(TYPE_IMAGE);
			emit_assign(make_lhs(compvar), make_op_rhs(OP_STRIP_RESIZE,
								   make_compvar_primary(args[i][0])));
		    }
		    else
		    {
			g_assert(arglengths[i] == 1);
			compvar = args[i][0];
		    }

		    arg_primaries[i] = make_compvar_primary(compvar);
		}

		if (!is_alloced)
		    dest[0] = make_temporary(TYPE_IMAGE);
		emit_assign(make_lhs(image), make_closure_rhs(tree->val.filter_closure.filter, arg_primaries));
		resized_image = resize_image_if_necessary(make_compvar_primary(image), filter_flags(filter));
		emit_assign(make_lhs(dest[0]), make_value_rhs(resized_image));
	    }
	    break;

	default :
	    g_assert_not_reached();
    }
}

static binding_values_t*
new_binding_values (int kind, gpointer key, binding_values_t *next, int num_values, int var_type)
{
    binding_values_t *bv = (binding_values_t*)pools_alloc(&compiler_pools, sizeof(binding_values_t)
							  + num_values * sizeof(value_t*));
    int i;

    g_assert(key != NULL);

    bv->kind = kind;
    bv->key = key;
    for (i = 0; i < num_values; ++i)
	bv->values[i] = current_value(make_temporary(var_type));
    bv->next = next;

    return bv;
}

static binding_values_t*
gen_binding_values_from_userval_infos (userval_info_t *info, binding_values_t *bvs)
{
    while (info != NULL)
    {
	userval_representation_t *rep = lookup_userval_representation(info->type);

	if (rep != NULL)
	{
	    if (info->type == USERVAL_IMAGE)
	    {
		compvar_t *image = make_temporary(TYPE_IMAGE);
		value_t *resized_image;

		emit_assign(make_lhs(image), make_op_rhs(rep->getter_op, make_int_const_primary(info->index)));
		resized_image = resize_image_if_necessary(make_compvar_primary(image), info->v.image.flags);

		bvs = new_binding_values(BINDING_USERVAL, info, bvs, rep->num_vars, rep->var_type);
		emit_assign(bvs->values[0], make_value_rhs(resized_image));
	    }
	    else
	    {
		bvs = new_binding_values(BINDING_USERVAL, info, bvs, rep->num_vars, rep->var_type);
		emit_assign(bvs->values[0], make_op_rhs(rep->getter_op, make_int_const_primary(info->index)));
	    }
	}

	info = info->next;
    }

    return bvs;
}

static value_t*
get_internal_value (filter_t *filter, const char *name, gboolean allow_bindings)
{
    internal_t *internal;
    binding_values_t *bv;

    g_assert(filter->kind == FILTER_MATHMAP);

    internal = lookup_internal(filter->v.mathmap.internals, name, TRUE);
    g_assert(internal != NULL);

    bv = lookup_binding_values(BINDING_INTERNAL, internal);
    if (allow_bindings && bv != NULL)
	return bv->values[0];
    else
    {
	compvar_t *temp = make_temporary(TYPE_INT);
	emit_assign(make_lhs(temp), compiler_make_internal_rhs(internal));
	return current_value(temp);
    }
}

static binding_values_t*
make_internal_binding_from_rhs (internal_t *internal, rhs_t *rhs, binding_values_t *bvs)
{
    bvs = new_binding_values(BINDING_INTERNAL, internal, bvs, 1, TYPE_INT);
    emit_assign(bvs->values[0], rhs);
    return bvs;
}

static binding_values_t*
make_pixel_xy_binding (filter_t *filter, internal_t *internal, const char *size_internal_name, binding_values_t *bvs)
{
    compvar_t *size_minus_one = make_temporary(TYPE_INT);

    emit_assign(make_lhs(size_minus_one),
		make_op_rhs(OP_SUB,
			    make_value_primary(get_internal_value(filter, size_internal_name, TRUE)),
			    make_int_const_primary(1)));
    return make_internal_binding_from_rhs(internal,
					  make_op_rhs(OP_DIV,
						      make_compvar_primary(size_minus_one),
						      make_int_const_primary(2)),
					  bvs);
}

static value_t*
emit_pixel_wh_max (filter_t *filter)
{
    compvar_t *max = make_temporary(TYPE_INT);

    emit_assign(make_lhs(max),
		make_op_rhs(OP_MAX,
			    make_value_primary(get_internal_value(filter, "__canvasPixelW", FALSE)),
			    make_value_primary(get_internal_value(filter, "__canvasPixelH", FALSE))));
    return current_value(max);
}

static binding_values_t*
gen_binding_values_for_limits (filter_t *filter, binding_values_t *bvs)
{
    internal_t *w_internal = lookup_internal(filter->v.mathmap.internals, "W", TRUE);
    internal_t *h_internal = lookup_internal(filter->v.mathmap.internals, "H", TRUE);
    internal_t *x_internal = lookup_internal(filter->v.mathmap.internals, "X", TRUE);
    internal_t *y_internal = lookup_internal(filter->v.mathmap.internals, "Y", TRUE);

    g_assert(w_internal != NULL && h_internal != NULL && x_internal != NULL && y_internal != NULL);

    switch (filter_flags(filter) & (IMAGE_FLAG_UNIT | IMAGE_FLAG_SQUARE))
    {
	case 0 :
	    bvs = make_internal_binding_from_rhs(w_internal,
						 make_value_rhs(get_internal_value(filter, "__canvasPixelW", FALSE)),
						 bvs);
	    bvs = make_internal_binding_from_rhs(h_internal,
						 make_value_rhs(get_internal_value(filter, "__canvasPixelH", FALSE)),
						 bvs);
	    bvs = make_pixel_xy_binding(filter, x_internal, "__canvasPixelW", bvs);
	    bvs = make_pixel_xy_binding(filter, y_internal, "__canvasPixelH", bvs);
	    break;

	case IMAGE_FLAG_UNIT :
	    bvs = make_internal_binding_from_rhs(w_internal, make_int_const_rhs(2), bvs);
	    bvs = make_internal_binding_from_rhs(h_internal, make_int_const_rhs(2), bvs);
	    bvs = make_internal_binding_from_rhs(x_internal, make_int_const_rhs(1), bvs);
	    bvs = make_internal_binding_from_rhs(y_internal, make_int_const_rhs(1), bvs);
	    break;

	case IMAGE_FLAG_UNIT | IMAGE_FLAG_SQUARE :
	    {
		value_t *pixel_wh_max = emit_pixel_wh_max(filter);
		value_t *x_value, *y_value;

		bvs = make_internal_binding_from_rhs(x_internal,
						     make_op_rhs(OP_DIV,
								 make_value_primary(get_internal_value(filter, "__canvasPixelW", FALSE)),
								 make_value_primary(pixel_wh_max)),
						     bvs);
		x_value = bvs->values[0];
		bvs = make_internal_binding_from_rhs(y_internal,
						     make_op_rhs(OP_DIV,
								 make_value_primary(get_internal_value(filter, "__canvasPixelH", FALSE)),
								 make_value_primary(pixel_wh_max)),
						     bvs);
		y_value = bvs->values[0];
		bvs = make_internal_binding_from_rhs(w_internal,
						     make_op_rhs(OP_MUL,
								 make_value_primary(x_value),
								 make_int_const_primary(2)),
						     bvs);
		bvs = make_internal_binding_from_rhs(h_internal,
						     make_op_rhs(OP_MUL,
								 make_value_primary(y_value),
								 make_int_const_primary(2)),
						     bvs);
	    }
	    break;

	default :
	    g_assert_not_reached();
    }

    return bvs;
}

static binding_values_t*
gen_binding_values_for_xy (filter_t *filter, value_t *x, value_t *y, binding_values_t *bvs)
{
    bvs = make_internal_binding_from_rhs(lookup_internal(filter->v.mathmap.internals, "x", TRUE),
					 make_op_rhs(OP_MUL,
						     make_value_primary(x),
						     make_value_primary(get_internal_value(filter, "X", TRUE))),
					 bvs);
    bvs = make_internal_binding_from_rhs(lookup_internal(filter->v.mathmap.internals, "y", TRUE),
					 make_op_rhs(OP_MUL,
						     make_value_primary(y),
						     make_value_primary(get_internal_value(filter, "Y", TRUE))),
					 bvs);

    return bvs;
}

static binding_values_t*
gen_binding_values_from_filter_args (filter_t *filter, primary_t *args, binding_values_t *bvs)
{
    int num_args = compiler_num_filter_args(filter);
    userval_info_t *info;
    int i;
    internal_t *internal;
    compvar_t *x_tmp, *y_tmp;

    g_assert(filter->kind == FILTER_MATHMAP);

    for (i = 0, info = filter->userval_infos;
	 i < num_args - 3;
	 ++i, info = info->next)
    {
	userval_representation_t *rep = lookup_userval_representation(info->type);

	g_assert(rep != NULL);
	g_assert(rep->num_vars == 1);

	bvs = new_binding_values(BINDING_USERVAL, info, bvs, rep->num_vars, rep->var_type);

	if (info->type == USERVAL_IMAGE)
	    emit_assign(bvs->values[0], make_value_rhs(resize_image_if_necessary(args[i], info->v.image.flags)));
	else
	    emit_assign(bvs->values[0], make_primary_rhs(args[i]));
    }
    g_assert(info == NULL);

    x_tmp = make_temporary(TYPE_INT);
    emit_assign(make_lhs(x_tmp), make_primary_rhs(args[num_args - 3]));

    y_tmp = make_temporary(TYPE_INT);
    emit_assign(make_lhs(y_tmp), make_primary_rhs(args[num_args - 2]));

    bvs = gen_binding_values_for_xy(filter, current_value(x_tmp), current_value(y_tmp), bvs);

    internal = lookup_internal(filter->v.mathmap.internals, "t", TRUE);
    g_assert(internal != NULL);
    bvs = new_binding_values(BINDING_INTERNAL, internal, bvs, 1, TYPE_INT);
    emit_assign(bvs->values[0], make_primary_rhs(args[num_args - 1]));

    return bvs;
}

static binding_values_t*
gen_ra_binding_values (filter_t *filter, binding_values_t *bvs)
{
    compvar_t *r = make_temporary(TYPE_FLOAT);
    compvar_t *a = make_temporary(TYPE_FLOAT);
    compvar_t *x_over_r = make_temporary(TYPE_FLOAT);
    value_t *x = get_internal_value(filter, "x", TRUE);
    value_t *y = get_internal_value(filter, "y", TRUE);
    rhs_t *rhs;

    g_assert(filter->kind == FILTER_MATHMAP);

    emit_assign(make_lhs(r), make_op_rhs(OP_HYPOT, make_value_primary(x), make_value_primary(y)));

    start_if_cond(make_op_rhs(OP_EQ, make_compvar_primary(r), make_float_const_primary(0.0)));

    emit_assign(make_lhs(a), make_float_const_rhs(0.0));

    switch_if_branch();

    emit_assign(make_lhs(x_over_r), make_op_rhs(OP_DIV, make_value_primary(x), make_compvar_primary(r)));
    emit_assign(make_lhs(a), make_op_rhs(OP_ACOS, make_compvar_primary(x_over_r)));

    end_if_cond();

    start_if_cond(make_op_rhs(OP_LESS, make_value_primary(y), make_float_const_primary(0.0)));

    rhs = make_op_rhs(OP_SUB, make_float_const_primary(2 * M_PI), make_compvar_primary(a));
    emit_assign(make_lhs(a), rhs);

    switch_if_branch();
    end_if_cond();

    bvs = new_binding_values(BINDING_INTERNAL,
			     lookup_internal(filter->v.mathmap.internals, "r", TRUE),
			     bvs, 1, TYPE_FLOAT);
    emit_assign(bvs->values[0], make_compvar_rhs(r));

    bvs = new_binding_values(BINDING_INTERNAL,
			     lookup_internal(filter->v.mathmap.internals, "a", TRUE),
			     bvs, 1, TYPE_FLOAT);
    emit_assign(bvs->values[0], make_compvar_rhs(a));

    return bvs;
}

static variable_t*
is_exprtree_variable (exprtree *exprtree)
{
    if (exprtree->type == EXPR_VARIABLE)
	return exprtree->val.var;
    return NULL;
}

static void
find_all_vector_variables (exprtree *tree)
{
    exprtree *sub;
    variable_t *var;

    switch (tree->type)
    {
	case EXPR_INT_CONST :
	case EXPR_FLOAT_CONST :
	case EXPR_TUPLE_CONST :
	case EXPR_VARIABLE :
	case EXPR_INTERNAL :
	case EXPR_USERVAL :
	    break;

	case EXPR_TUPLE :
	    for (sub = tree->val.tuple.elems; sub != 0; sub = sub->next)
		find_all_vector_variables(sub);
	    break;

	case EXPR_SELECT :
	    find_all_vector_variables(tree->val.select.tuple);
	    var = is_exprtree_variable(tree->val.select.tuple);
	    for (sub = tree->val.select.subscripts->val.tuple.elems; sub != 0; sub = sub->next)
	    {
		find_all_vector_variables(sub);
		if (!is_exprtree_single_const(sub, NULL, NULL))
		{
		    g_hash_table_insert(vector_variables, tree, GINT_TO_POINTER(1));
		    if (var)
			g_hash_table_insert(vector_variables, var, GINT_TO_POINTER(1));
		}
	    }
	    break;

	case EXPR_ASSIGNMENT :
	    find_all_vector_variables(tree->val.assignment.value);
	    break;

	case EXPR_SUB_ASSIGNMENT :
	    find_all_vector_variables(tree->val.sub_assignment.value);
	    var = tree->val.sub_assignment.var;
	    for (sub = tree->val.sub_assignment.subscripts->val.tuple.elems; sub != 0; sub = sub->next)
	    {
		find_all_vector_variables(sub);
		if (!is_exprtree_single_const(sub, NULL, NULL))
		    g_hash_table_insert(vector_variables, var, GINT_TO_POINTER(1));
	    }
	    break;

	case EXPR_CAST :
	    find_all_vector_variables(tree->val.cast.tuple);
	    break;

	case EXPR_FUNC :
	    for (sub = tree->val.func.args; sub != 0; sub = sub->next)
		find_all_vector_variables(sub);
	    break;

	case EXPR_SEQUENCE :
	    find_all_vector_variables(tree->val.op.left);
	    find_all_vector_variables(tree->val.op.right);
	    break;

	case EXPR_IF_THEN :
	case EXPR_IF_THEN_ELSE :
	    find_all_vector_variables(tree->val.ifExpr.condition);
	    find_all_vector_variables(tree->val.ifExpr.consequent);
	    if (tree->type == EXPR_IF_THEN_ELSE)
		find_all_vector_variables(tree->val.ifExpr.alternative);
	    break;

	case EXPR_DO_WHILE :
	case EXPR_WHILE :
	    find_all_vector_variables(tree->val.whileExpr.invariant);
	    find_all_vector_variables(tree->val.whileExpr.body);
	    break;

	case EXPR_FILTER_CLOSURE :
	    for (sub = tree->val.filter_closure.args; sub != 0; sub = sub->next)
		find_all_vector_variables(sub);
	    break;

	default :
	    g_assert_not_reached();
    }
}

static statement_t*
gen_filter_code (filter_t *filter, compvar_t *tuple, primary_t *args, rhs_t **tuple_rhs, inlining_history_t *history)
{
    statement_t *first_stmt_save = first_stmt;
    inlining_history_t *history_save = inlining_history;
    statement_t *stmt;
    compvar_t *result[filter->v.mathmap.decl->v.filter.body->result.length];
    rhs_t *rhs;
    binding_values_t *binding_values_save = binding_values;

    compiler_reset_variables(filter->v.mathmap.variables);

    inlining_history = push_inlined_filter(filter, history);

    first_stmt = NULL;
    emit_loc = &first_stmt;
    binding_values = gen_binding_values_for_limits(filter, NULL);
    if (args != NULL)
	binding_values = gen_binding_values_from_filter_args(filter, args, binding_values);
    else
    {
	binding_values = gen_binding_values_from_userval_infos(filter->userval_infos, binding_values);
	if (needs_xy_scaling(filter_flags(filter)))
	    binding_values = gen_binding_values_for_xy(filter,
						       get_internal_value(filter, "x", FALSE),
						       get_internal_value(filter, "y", FALSE),
						       binding_values);
    }

    if (does_filter_use_ra(filter))
	binding_values = gen_ra_binding_values(filter, binding_values);

    find_all_vector_variables(filter->v.mathmap.decl->v.filter.body);

    gen_code(filter, filter->v.mathmap.decl->v.filter.body, result, FALSE);

    rhs = make_tuple_rhs(4,
			 make_compvar_primary(result[0]), make_compvar_primary(result[1]),
			 make_compvar_primary(result[2]), make_compvar_primary(result[3]));
    if (tuple_rhs != NULL)
	*tuple_rhs = rhs;

    if (tuple != NULL)
	emit_assign(make_lhs(tuple), rhs);

    stmt = first_stmt;

    first_stmt = first_stmt_save;
    emit_loc = NULL;
    binding_values = binding_values_save;

    inlining_history = history_save;

    return stmt;
}

/*** builtins ***/

#define STK                   (invocation->stack_machine->stack)
#define STKP                  (invocation->stack_machine->stackp)

#include "new_builtins.c"

/*** dfa ***/

static statement_list_t*
prepend_value_statements (value_t *value, statement_list_t *rest)
{
    statement_list_t *lst;

    for (lst = value->uses; lst != 0; lst = lst->next)
	rest = prepend_statement(lst->stmt, rest);

    return rest;
}

static statement_list_t*
prepend_compvar_statements (compvar_t *compvar, statement_list_t *rest)
{
    value_t *value;

    for (value = compvar->values; value != 0; value = value->next)
	rest = prepend_value_statements(value, rest);

    return rest;
}

static void
_builder (statement_t *stmt, void *info)
{
    CLOSURE_VAR(statement_list_t**, worklist, 0);
    statement_list_t* (*build_worklist) (statement_t*, statement_list_t*, void*) = CLOSURE_GET(1, statement_list_t* (*) (statement_t*, statement_list_t*, void*));
    CLOSURE_VAR(void*, infoinfo, 2);

    *worklist = build_worklist(stmt, *worklist, infoinfo);
}

static void
perform_worklist_dfa (statement_t *stmts,
		      statement_list_t* (*build_worklist) (statement_t *stmt, statement_list_t *worklist, void *info),
		      statement_list_t* (*work_statement) (statement_t *stmt, statement_list_t *worklist, void *info),
		      void *info)
{
    statement_list_t *worklist = 0;

    FOR_EACH_ASSIGN_STATEMENT(stmts, &_builder, &worklist, build_worklist, info);

    do
    {
	statement_list_t *new_worklist = 0;

	while (worklist != 0)
	{
	    new_worklist = work_statement(worklist->stmt, new_worklist, info);
	    worklist = worklist->next;
	}

	worklist = new_worklist;
    } while (worklist != 0);
}

#define PERFORM_WORKLIST_DFA(stmts,build,work,...) do { long __clos[] = { __VA_ARGS__ }; perform_worklist_dfa((stmts),(build),(work),__clos); } while (0)


/*** type propagation ***/

static type_t
primary_type (primary_t *primary)
{
    switch (primary->kind)
    {
	case PRIMARY_VALUE :
	    return primary->v.value->compvar->type;

	case PRIMARY_CONST :
	    return primary->const_type;

	default :
	    g_assert_not_reached();
    }
}

static type_t
rhs_type (rhs_t *rhs)
{
    switch (rhs->kind)
    {
	case RHS_PRIMARY :
	    return primary_type(&rhs->v.primary);

	case RHS_INTERNAL :
	    return TYPE_FLOAT;	/* FIXME: actually, most internals are int */

	case RHS_OP :
	    if (rhs->v.op.op->type_prop == TYPE_PROP_CONST)
		return rhs->v.op.op->const_type;
	    else
	    {
		int max = TYPE_INT;
		int i;

		assert(rhs->v.op.op->type_prop == TYPE_PROP_MAX
		       || rhs->v.op.op->type_prop == TYPE_PROP_MAX_FLOAT);

		for (i = 0; i < rhs->v.op.op->num_args; ++i)
		{
		    int type = primary_type(&rhs->v.op.args[i]);

		    if (type > max)
			max = type;
		}

		return max;
	    }
	    break;

	case RHS_FILTER :
	case RHS_TUPLE :
	    return TYPE_TUPLE;

	case RHS_CLOSURE :
	    return TYPE_IMAGE;

	case RHS_TREE_VECTOR :
	    return TYPE_TREE_VECTOR;

	default :
	    g_assert_not_reached();
    }

    return 0;
}

static statement_list_t*
propagate_types_builder (statement_t *stmt, statement_list_t *worklist, void *info)
{
    assert(stmt->kind == STMT_ASSIGN || stmt->kind == STMT_PHI_ASSIGN);

    return prepend_statement(stmt, worklist);
}

static statement_list_t*
propagate_types_worker (statement_t *stmt, statement_list_t *worklist, void *info)
{
    switch (stmt->kind)
    {
	case STMT_NIL :
	case STMT_IF_COND :
	case STMT_WHILE_LOOP :
	    break;

	case STMT_ASSIGN :
	case STMT_PHI_ASSIGN :
	{
	    int type, type2;

	    /*
	    printf("propagating types on ");
	    print_assign_statement(stmt);
	    printf("   ***   ");
	    */

	    type = rhs_type(stmt->v.assign.rhs);
	    if (stmt->kind == STMT_PHI_ASSIGN)
	    {
		type2 = rhs_type(stmt->v.assign.rhs2);
		if (type != type2)
		{
		    assert(type <= MAX_PROMOTABLE_TYPE && type2 <= MAX_PROMOTABLE_TYPE);
		    if (type2 > type)
			type = type2;
		}
	    }

	    //printf("lhs %d   rhs %d\n", stmt->v.assign.lhs->compvar->type, type);

	    if (type > stmt->v.assign.lhs->compvar->type)
	    {
		stmt->v.assign.lhs->compvar->type = type;
		worklist = prepend_compvar_statements(stmt->v.assign.lhs->compvar, worklist);
	    }
	}
	break;

	default :
	    g_assert_not_reached();
    }

    return worklist;
}

static void
propagate_types (void)
{
    PERFORM_WORKLIST_DFA(first_stmt, &propagate_types_builder, &propagate_types_worker);
}

/*** constants analysis ***/

#define LEAST_CONST_TYPE(v)        ((v)->const_type & (v)->least_const_type_multiply_used_in)

int
primary_constant (primary_t *primary)
{
    switch (primary->kind)
    {
	case PRIMARY_VALUE :
	    return LEAST_CONST_TYPE(primary->v.value);

	case PRIMARY_CONST :
	    return CONST_MAX;

	default :
	    g_assert_not_reached();
    }
}

int
rhs_constant (rhs_t *rhs)
{
    switch (rhs->kind)
    {
	case RHS_INTERNAL :
	    return rhs->v.internal->const_type;

	case RHS_OP :
	    if (!rhs->v.op.op->is_pure)
		return CONST_NONE;
	case RHS_PRIMARY :
	case RHS_CLOSURE :
	case RHS_TUPLE :
	case RHS_TREE_VECTOR :
	    {
		int num_primaries;
		primary_t *primaries = get_rhs_primaries(rhs, &num_primaries);
		int i;
		int const_type_max = CONST_MAX;

		if (rhs->kind == RHS_CLOSURE
		    && (rhs->v.closure.filter->kind == FILTER_NATIVE
			&& !rhs->v.closure.filter->v.native.is_pure))
		    return CONST_NONE;

		for (i = 0; i < num_primaries; ++i)
		{
		    int const_type = primary_constant(&primaries[i]);

		    const_type_max &= const_type;
		}

		return const_type_max;
	    }

	case RHS_FILTER :
	    /* FIXME: Improve on this.  For each filter figure out
	       whether it's pure.  Then handle pure filters like
	       ops. */
	    return CONST_NONE;

	default :
	    g_assert_not_reached();
    }
}

void
analyze_phis_constant (statement_t *phis, int const_max, int *changed)
{
    while (phis != 0)
    {
	int const_type, const_type2;

	if (phis->kind == STMT_NIL)
	{
	    phis = phis->next;
	    continue;
	}

	assert(phis->kind == STMT_PHI_ASSIGN);

	const_type = rhs_constant(phis->v.assign.rhs);
	const_type2 = rhs_constant(phis->v.assign.rhs2);

	const_type = const_type & const_type2 & const_max;

	if (phis->v.assign.lhs->const_type != const_type)
	{
	    phis->v.assign.lhs->const_type = const_type;
	    *changed = 1;
	}

	phis = phis->next;
    }
}

static void
analyze_stmts_constants (statement_t *stmt, int *changed, unsigned int inherited_max_const)
{
    int const_type;

    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
		const_type = rhs_constant(stmt->v.assign.rhs);
		if (stmt->v.assign.lhs->const_type != (const_type & inherited_max_const))
		{
		    stmt->v.assign.lhs->const_type = (const_type & inherited_max_const);
		    *changed = 1;
		}
		break;

	    case STMT_PHI_ASSIGN :
		g_assert_not_reached();
		break;

	    case STMT_IF_COND :
		const_type = rhs_constant(stmt->v.if_cond.condition);

		analyze_stmts_constants(stmt->v.if_cond.consequent, changed, const_type & inherited_max_const);
		analyze_stmts_constants(stmt->v.if_cond.alternative, changed, const_type & inherited_max_const);
		analyze_phis_constant(stmt->v.if_cond.exit, const_type & inherited_max_const, changed);
		break;

	    case STMT_WHILE_LOOP :
		const_type = rhs_constant(stmt->v.while_loop.invariant);

		analyze_phis_constant(stmt->v.while_loop.entry, const_type & inherited_max_const, changed);
		analyze_stmts_constants(stmt->v.while_loop.body, changed, const_type & inherited_max_const);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

static void
_clear_const_bits_from_assignment (value_t *val, void *info)
{
    CLOSURE_VAR(statement_t*, stmt, 0);
    val->least_const_type_directly_used_in &= LEAST_CONST_TYPE(stmt->v.assign.lhs);
}

static void
_clear_const_bits (value_t *val, void *info)
{
    CLOSURE_VAR(unsigned int, sub_least_const_type, 0);
    val->least_const_type_directly_used_in &= sub_least_const_type;
}

static unsigned int
analyze_least_const_type_directly_used_in (statement_t *stmt)
{
    unsigned int least_const_type = CONST_MAX;

    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_PHI_ASSIGN :
		COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs2, &_clear_const_bits_from_assignment, stmt);
	    case STMT_ASSIGN :
		COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs, &_clear_const_bits_from_assignment, stmt);
		least_const_type &= LEAST_CONST_TYPE(stmt->v.assign.lhs);
		break;

	    case STMT_IF_COND :
	    {
		unsigned int sub_least_const_type = CONST_MAX;

		sub_least_const_type &= analyze_least_const_type_directly_used_in(stmt->v.if_cond.consequent);
		sub_least_const_type &= analyze_least_const_type_directly_used_in(stmt->v.if_cond.alternative);
		sub_least_const_type &= analyze_least_const_type_directly_used_in(stmt->v.if_cond.exit);

		COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.if_cond.condition, &_clear_const_bits, (void*)sub_least_const_type);

		least_const_type &= sub_least_const_type;

		break;
	    }

	    case STMT_WHILE_LOOP :
	    {
		unsigned int sub_least_const_type = CONST_MAX;

		sub_least_const_type &= analyze_least_const_type_directly_used_in(stmt->v.while_loop.entry);
		sub_least_const_type &= analyze_least_const_type_directly_used_in(stmt->v.while_loop.body);

		COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.while_loop.invariant, &_clear_const_bits, (void*)sub_least_const_type);

		least_const_type &= sub_least_const_type;

		break;
	    }

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }

    return least_const_type;
}

#undef LEAST_CONST_TYPE

static void
_update_const (value_t *val, void *info)
{
    CLOSURE_VAR(value_set_t*, update_const_set, 0);
    CLOSURE_VAR(int, update_const_mask, 1);
    CLOSURE_VAR(int*, changed, 2);

    if (compiler_value_set_contains(update_const_set, val)
	&& ((val->least_const_type_multiply_used_in & update_const_mask)
	    != val->least_const_type_multiply_used_in))
    {
	val->least_const_type_multiply_used_in &= update_const_mask;
	*changed = 1;
    }
}

static int
analyze_least_const_type_multiply_used_in (statement_t *stmt, int in_loop, value_set_t *multiply_assigned_set, int *changed)
{
    int update_const_mask;
    value_set_t *update_const_set = multiply_assigned_set;
    int least_const = CONST_MAX;

    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_PHI_ASSIGN :
		update_const_mask = stmt->v.assign.lhs->least_const_type_multiply_used_in;
		COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs2, &_update_const,
					       update_const_set, (void*)update_const_mask, changed);
	    case STMT_ASSIGN :
		update_const_mask = stmt->v.assign.lhs->least_const_type_multiply_used_in;
		COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs, &_update_const,
					       update_const_set, (void*)update_const_mask, changed);
		if (in_loop)
		    compiler_value_set_add(multiply_assigned_set, stmt->v.assign.lhs);
		least_const &= stmt->v.assign.lhs->least_const_type_multiply_used_in;
		break;

	    case STMT_IF_COND :
	    {
		int sub_least_const;

		sub_least_const = analyze_least_const_type_multiply_used_in(stmt->v.if_cond.consequent,
									    in_loop, multiply_assigned_set, changed);
		sub_least_const &= analyze_least_const_type_multiply_used_in(stmt->v.if_cond.alternative,
									     in_loop, multiply_assigned_set, changed);
		sub_least_const &= analyze_least_const_type_multiply_used_in(stmt->v.if_cond.exit,
									     in_loop, multiply_assigned_set, changed);

		update_const_mask = sub_least_const;
		COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.if_cond.condition, &_update_const,
					       update_const_set, (void*)update_const_mask, changed);

		least_const &= sub_least_const;

		break;
	    }

	    case STMT_WHILE_LOOP :
	    {
		int sub_least_const;
		value_set_t *copy;

		sub_least_const = analyze_least_const_type_multiply_used_in(stmt->v.while_loop.entry,
									    in_loop, multiply_assigned_set, changed);

		copy = compiler_value_set_copy(multiply_assigned_set);
		assert(copy != 0);

		/* we have to process the body twice because
		 * multiply_assigned_to information flows from the body to the
		 * entry as well as from the entry to the body (we could just
		 * as well have processed the entry first, then the body, and
		 * then the entry again) */
		sub_least_const &= analyze_least_const_type_multiply_used_in(stmt->v.while_loop.body,
									     1, copy, changed);
		sub_least_const &= analyze_least_const_type_multiply_used_in(stmt->v.while_loop.entry,
									     1, copy, changed);
		sub_least_const &= analyze_least_const_type_multiply_used_in(stmt->v.while_loop.body,
									     1, copy, changed);

		update_const_set = copy;
		update_const_mask = sub_least_const;
		COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.while_loop.invariant, &_update_const,
					       update_const_set, (void*)update_const_mask, changed);
		update_const_set = multiply_assigned_set;

		compiler_free_value_set(copy);

		least_const &= sub_least_const;

		break;
	    }

	    default:
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }

    return least_const;
}

static void
_init_const_type (value_t *value, statement_t *stmt, void *info)
{
    value->const_type = CONST_MAX;
}

static void
_init_least_const_types (value_t *value, statement_t *stmt, void *info)
{
    value->least_const_type_multiply_used_in = value->const_type;
    value->least_const_type_directly_used_in = value->const_type;
}

static void
analyze_constants (void)
{
    int changed;

    COMPILER_FOR_EACH_VALUE_IN_STATEMENTS(first_stmt, &_init_const_type);

    do
    {
	changed = 0;
	analyze_stmts_constants(first_stmt, &changed, CONST_MAX);
    } while (changed);

    COMPILER_FOR_EACH_VALUE_IN_STATEMENTS(first_stmt, &_init_least_const_types);

    do
    {
	value_set_t *set = compiler_new_value_set();

	changed = 0;
	analyze_least_const_type_multiply_used_in(first_stmt, 0, set, &changed);

	compiler_free_value_set(set);
    } while (changed);

    analyze_least_const_type_directly_used_in(first_stmt);
}

/*** closure application ***/

static void
optimize_closure_application (statement_t *stmt)
{
    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
	    case STMT_PHI_ASSIGN :
		break;

	    case STMT_ASSIGN :
		if (stmt->v.assign.rhs->kind == RHS_OP
		    && compiler_op_index(stmt->v.assign.rhs->v.op.op) == OP_ORIG_VAL
		    && stmt->v.assign.rhs->v.op.args[2].kind == PRIMARY_VALUE)
		{
		    statement_t *def = stmt->v.assign.rhs->v.op.args[2].v.value->def;

		    if (def->kind == STMT_ASSIGN
			&& def->v.assign.rhs->kind == RHS_CLOSURE
			&& def->v.assign.rhs->v.closure.filter->kind == FILTER_MATHMAP)
		    {
			filter_t *filter = def->v.assign.rhs->v.filter.filter;
			int num_args = compiler_num_filter_args(filter);
			primary_t *args = (primary_t*)pools_alloc(&compiler_pools, sizeof(primary_t) * num_args);
			int i;

			for (i = 0; i < num_args - 3; ++i)
			    args[i] = def->v.assign.rhs->v.filter.args[i];

			args[num_args - 3] = stmt->v.assign.rhs->v.op.args[0]; /* x */
			args[num_args - 2] = stmt->v.assign.rhs->v.op.args[1]; /* y */
			args[num_args - 1] = stmt->v.assign.rhs->v.op.args[3]; /* t */

			remove_use(stmt->v.assign.rhs->v.op.args[2].v.value, stmt); /* image */

			stmt->v.assign.rhs = make_filter_rhs(filter, args);
			stmt->v.assign.rhs->v.filter.history = def->v.assign.rhs->v.closure.history;

			for (i = 0; i < num_args - 3; ++i)
			    if (args[i].kind == PRIMARY_VALUE)
				add_use(args[i].v.value, stmt);

		    }
		}
		break;

	    case STMT_IF_COND :
		optimize_closure_application(stmt->v.if_cond.consequent);
		optimize_closure_application(stmt->v.if_cond.alternative);
		break;

	    case STMT_WHILE_LOOP :
		optimize_closure_application(stmt->v.while_loop.body);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

/*** copy propagation ***/

static void
_rewrite_if_possible (value_t *value, void *info)
{
    CLOSURE_VAR(statement_t*, stmt, 0);
    CLOSURE_VAR(GHashTable*, copy_hash, 1);
    CLOSURE_VAR(int*, changed, 2);

    primary_t *new = (primary_t*)g_hash_table_lookup(copy_hash, value);

    if (new != 0)
    {
	rewrite_use(stmt, value, *new);
	*changed = 1;
    }
}

static void
copy_propagate_recursively (statement_t *stmt, GHashTable *copy_hash, int *changed)
{
    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_PHI_ASSIGN :
		COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs, &_rewrite_if_possible, stmt, copy_hash, changed);
		COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs2, &_rewrite_if_possible, stmt, copy_hash, changed);
		break;

	    case STMT_ASSIGN :
		COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs, &_rewrite_if_possible, stmt, copy_hash, changed);

		if (stmt->v.assign.rhs->kind == RHS_PRIMARY)
		{
		    primary_t *copy_copy = alloc_primary();

		    assert(copy_copy != 0);
		    *copy_copy = stmt->v.assign.rhs->v.primary;

		    g_hash_table_insert(copy_hash, stmt->v.assign.lhs, copy_copy);
		}
		break;

	    case STMT_IF_COND :
		COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.if_cond.condition, &_rewrite_if_possible, stmt, copy_hash, changed);
		copy_propagate_recursively(stmt->v.if_cond.consequent, copy_hash, changed);
		copy_propagate_recursively(stmt->v.if_cond.alternative, copy_hash, changed);
		copy_propagate_recursively(stmt->v.if_cond.exit, copy_hash, changed);
		break;

	    case STMT_WHILE_LOOP :
		COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.while_loop.invariant, &_rewrite_if_possible, stmt, copy_hash, changed);
		copy_propagate_recursively(stmt->v.while_loop.entry, copy_hash, changed);
		copy_propagate_recursively(stmt->v.while_loop.body, copy_hash, changed);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

static int
copy_propagation (void)
{
    GHashTable *copy_hash = g_hash_table_new(g_direct_hash, g_direct_equal);
    int changed = 0;

    assert(copy_hash != 0);

    copy_propagate_recursively(first_stmt, copy_hash, &changed);

    return changed;
}

/*** constant folding ***/

static int
rhs_is_foldable (rhs_t *rhs)
{
    int i;

    if (rhs->kind != RHS_OP)
	return 0;

    if (!rhs->v.op.op->is_foldable)
	return 0;

    for (i = 0; i < rhs->v.op.op->num_args; ++i)
	if (rhs->v.op.args[i].kind == PRIMARY_VALUE)
	    return 0;

    return 1;
}

static void
fold_rhs_if_possible (rhs_t **rhs, int *changed)
{
    if (rhs_is_foldable(*rhs))
    {
	*rhs = make_primary_rhs(fold_rhs(*rhs));
	*changed = 1;
    }
}

static void
fold_constants_recursively (statement_t *stmt, int *changed)
{
    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_PHI_ASSIGN :
		fold_rhs_if_possible(&stmt->v.assign.rhs2, changed);
	    case STMT_ASSIGN :
		fold_rhs_if_possible(&stmt->v.assign.rhs, changed);
		break;

	    case STMT_IF_COND :
		fold_rhs_if_possible(&stmt->v.if_cond.condition, changed);
		fold_constants_recursively(stmt->v.if_cond.consequent, changed);
		fold_constants_recursively(stmt->v.if_cond.alternative, changed);
		fold_constants_recursively(stmt->v.if_cond.exit, changed);
		break;

	    case STMT_WHILE_LOOP :
		fold_rhs_if_possible(&stmt->v.while_loop.invariant, changed);
		fold_constants_recursively(stmt->v.while_loop.entry, changed);
		fold_constants_recursively(stmt->v.while_loop.body, changed);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

static int
constant_folding (void)
{
    int changed = 0;

    fold_constants_recursively(first_stmt, &changed);

    return changed;
}

/*** simplification ***/

static void
simplify_unit (rhs_t **rhsp, float unit, gboolean left, gboolean right, int *changed)
{
    rhs_t *rhs = *rhsp;

    if (left && RHS_ARG(0).kind == PRIMARY_CONST && OP_CONST_FLOAT_VAL(0) == unit)
    {
	*rhsp = make_primary_rhs(RHS_ARG(1));
	*changed = 1;
	return;
    }

    if (right && RHS_ARG(1).kind == PRIMARY_CONST && OP_CONST_FLOAT_VAL(1) == unit)
    {
	*rhsp = make_primary_rhs(RHS_ARG(0));
	*changed = 1;
	return;
    }
}

static void
simplify_zero (rhs_t **rhsp, float zero, int result, gboolean left, gboolean right, int *changed)
{
    rhs_t *rhs = *rhsp;

    if ((left && RHS_ARG(0).kind == PRIMARY_CONST && OP_CONST_FLOAT_VAL(0) == zero)
	|| (right && RHS_ARG(1).kind == PRIMARY_CONST && OP_CONST_FLOAT_VAL(1) == zero))
    {
	if (RHS_ARG(0).const_type == TYPE_INT)
	    *rhsp = make_int_const_rhs(result);
	else
	    *rhsp = make_float_const_rhs(result);
	*changed = 1;
    }
}

static void
simplify_rhs (rhs_t **rhsp, int *changed)
{
    rhs_t *rhs = *rhsp;

    if (rhs->kind != RHS_OP)
	return;

    switch (compiler_op_index(rhs->v.op.op))
    {
	case OP_ADD :
	    simplify_unit(rhsp, 0.0, TRUE, TRUE, changed);
	    break;

	case OP_SUB :
	    simplify_unit(rhsp, 0.0, FALSE, TRUE, changed);
	    break;

	case OP_MUL :
	    simplify_unit(rhsp, 1.0, TRUE, TRUE, changed);
	    if (rhs->kind == RHS_OP)
		simplify_zero(rhsp, 0.0, 0, TRUE, TRUE, changed);
	    break;

	case OP_DIV :
	    simplify_unit(rhsp, 1.0, FALSE, TRUE, changed);
	    break;

	case OP_POW :
	    simplify_unit(rhsp, 1.0, FALSE, TRUE, changed);
	    if (rhs->kind == RHS_OP)
		simplify_zero(rhsp, 0.0, 1, FALSE, TRUE, changed);
	    break;

	default :
	    break;
    }
}

static void
simplify_ops_recursively (statement_t *stmt, int *changed)
{
    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_PHI_ASSIGN :
		simplify_rhs(&stmt->v.assign.rhs2, changed);
	    case STMT_ASSIGN :
		simplify_rhs(&stmt->v.assign.rhs, changed);
		break;

	    case STMT_IF_COND :
		simplify_rhs(&stmt->v.if_cond.condition, changed);
		simplify_ops_recursively(stmt->v.if_cond.consequent, changed);
		simplify_ops_recursively(stmt->v.if_cond.alternative, changed);
		simplify_ops_recursively(stmt->v.if_cond.exit, changed);
		break;

	    case STMT_WHILE_LOOP :
		simplify_rhs(&stmt->v.while_loop.invariant, changed);
		simplify_ops_recursively(stmt->v.while_loop.entry, changed);
		simplify_ops_recursively(stmt->v.while_loop.body, changed);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

static int
simplify_ops (void)
{
    int changed = 0;

    simplify_ops_recursively(first_stmt, &changed);

    return changed;
}

/*** dead branch removal ***/

static int
is_rhs_const_primary (rhs_t *rhs)
{
    return rhs->kind == RHS_PRIMARY && rhs->v.primary.kind == PRIMARY_CONST;
}

static int
is_const_primary_rhs_true (rhs_t *rhs)
{
    assert(is_rhs_const_primary(rhs));

    switch (rhs->v.primary.const_type)
    {
	case TYPE_INT :
	    return rhs->v.primary.v.constant.int_value != 0;

	case TYPE_FLOAT :
	    return rhs->v.primary.v.constant.float_value != 0.0;

	default :
	    g_assert_not_reached();
    }

    return 0;
}

static int
has_indirect_parent (statement_t *stmt, statement_t *parent)
{
    assert(stmt != 0 && parent != 0);

    do
    {
	stmt = stmt->parent;
    } while (stmt != 0 && stmt != parent);

    if (stmt == parent)
	return 1;
    return 0;
}

static void
_remove_value_use (value_t *value, void *info)
{
    remove_use(value, CLOSURE_GET(0,statement_t*));
}

void
compiler_remove_uses_in_rhs (rhs_t *rhs, statement_t *stmt)
{
    COMPILER_FOR_EACH_VALUE_IN_RHS(rhs, &_remove_value_use, stmt);
}

static void
remove_dead_branches_recursively (statement_t *stmt, int *changed)
{
    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
	    case STMT_ASSIGN :
	    case STMT_PHI_ASSIGN :
		break;

	    case STMT_IF_COND :
		if (is_rhs_const_primary(stmt->v.if_cond.condition))
		{
		    int condition_true = is_const_primary_rhs_true(stmt->v.if_cond.condition);
		    statement_t *branch = condition_true ? stmt->v.if_cond.consequent : stmt->v.if_cond.alternative;
		    statement_t *insertion_point = stmt;
		    statement_t *phi;

		    while (branch != 0)
		    {
			statement_t *next = branch->next;

			if (branch->kind == STMT_ASSIGN)
			{
			    statement_list_t *lst;

			    for (lst = branch->v.assign.lhs->uses; lst != 0; lst = lst->next)
				assert(has_indirect_parent(lst->stmt, stmt));
			}

			if (branch->kind != STMT_NIL)
			{
			    branch->parent = stmt->parent;

			    branch->next = insertion_point->next;
			    insertion_point = insertion_point->next = branch;
			}

			branch = next;
		    }

		    phi = stmt->v.if_cond.exit;
		    while (phi != 0)
		    {
			statement_t *next = phi->next;

			if (phi->kind == STMT_PHI_ASSIGN)
			{
			    if (condition_true)
				compiler_remove_uses_in_rhs(phi->v.assign.rhs2, phi);
			    else
				compiler_remove_uses_in_rhs(phi->v.assign.rhs, phi);

			    phi->kind = STMT_ASSIGN;
			    if (!condition_true)
				phi->v.assign.rhs = phi->v.assign.rhs2;

			    phi->parent = stmt->parent;

			    phi->next = insertion_point->next;
			    insertion_point = insertion_point->next = phi;
			}
			else
			    assert(phi->kind == STMT_NIL);

			phi = next;
		    }

		    stmt->kind = STMT_NIL;

		    /* we don't need to handle the consequent/alternative
		     * here, because we have inserted it after the if
		     * statement, so the loop will handle them next.  */

		    *changed = 1;
		}
		else
		{
		    remove_dead_branches_recursively(stmt->v.if_cond.consequent, changed);
		    remove_dead_branches_recursively(stmt->v.if_cond.alternative, changed);
		}
		break;

	    case STMT_WHILE_LOOP :
		remove_dead_branches_recursively(stmt->v.while_loop.body, changed);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

static int
remove_dead_branches (void)
{
    int changed = 0;

    remove_dead_branches_recursively(first_stmt, &changed);

    return changed;
}

/*** dead control structure removal ***/

static gboolean
stmts_are_empty (statement_t *stmts)
{
    while (stmts != 0 && stmts->kind == STMT_NIL)
	stmts = stmts->next;

    return stmts == 0;
}

static void
remove_dead_controls_recursively (statement_t *stmt, int *changed)
{
    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
	    case STMT_ASSIGN :
	    case STMT_PHI_ASSIGN :
		break;

	    case STMT_IF_COND :
		if (stmts_are_empty(stmt->v.if_cond.consequent)
		    && stmts_are_empty(stmt->v.if_cond.alternative)
		    && stmts_are_empty(stmt->v.if_cond.exit)
		    && compiler_rhs_is_pure(stmt->v.if_cond.condition))
		{
		    compiler_remove_uses_in_rhs(stmt->v.if_cond.condition, stmt);
		    stmt->kind = STMT_NIL;
		    *changed = 1;
		}
		else
		{
		    remove_dead_controls_recursively(stmt->v.if_cond.consequent, changed);
		    remove_dead_controls_recursively(stmt->v.if_cond.alternative, changed);
		}
		break;

	    case STMT_WHILE_LOOP :
		remove_dead_controls_recursively(stmt->v.while_loop.body, changed);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

static int
remove_dead_controls (void)
{
    int changed = 0;

    remove_dead_controls_recursively(first_stmt, &changed);

    return changed;
}

/*** common subexpression eliminiation ***/

static int
images_equal (image_t *i1, image_t *i2)
{
    if (i1->type != i2->type)
	return 0;
    if (i1->type != IMAGE_DRAWABLE)
	return 0;
    return i1->v.drawable == i2->v.drawable;
}

static int
tuples_equal (float *t1, float *t2)
{
    g_assert_not_reached();
}

static int
tree_vectors_equal (tree_vector_t *tv1, tree_vector_t *tv2)
{
    g_assert_not_reached();
}

static int
curves_equal (curve_t *c1, curve_t *c2)
{
    return c1 == c2;
}

static int
gradients_equal (gradient_t *g1, gradient_t *g2)
{
    return g1 == g2;
}

static int
primaries_equal (primary_t *prim1, primary_t *prim2)
{
    if (prim1->kind != prim2->kind)
	return 0;

    switch (prim1->kind)
    {
	case PRIMARY_VALUE :
	    return prim1->v.value == prim2->v.value;

	case PRIMARY_CONST :
	    if (prim1->const_type != prim2->const_type)
		return 0;

	    switch (prim1->const_type)
	    {
		MAKE_CONST_COMPARATOR

		default :
		    g_assert_not_reached();
	    }
	    break;

	default :
	    g_assert_not_reached();
    }

    return 0;
}

static int
rhss_equal (rhs_t *rhs1, rhs_t *rhs2)
{
    if (rhs1->kind != rhs2->kind)
	return 0;

    switch (rhs1->kind)
    {
	case RHS_PRIMARY :
	    return primaries_equal(&rhs1->v.primary, &rhs2->v.primary);

	case RHS_INTERNAL :
	    return rhs1->v.internal == rhs2->v.internal;

	case RHS_OP :
	{
	    int i;

	    if (rhs1->v.op.op != rhs2->v.op.op)
		return 0;

	    for (i = 0; i < rhs1->v.op.op->num_args; ++i)
		if (!primaries_equal(&rhs1->v.op.args[i], &rhs2->v.op.args[i]))
		    return 0;
	    return 1;
	}

	case RHS_FILTER :
	{
	    int num_args = compiler_num_filter_args(rhs1->v.filter.filter);
	    int i;

	    if (rhs1->v.filter.filter != rhs2->v.filter.filter)
		return 0;

	    for (i = 0; i < num_args; ++i)
		if (!primaries_equal(&rhs1->v.filter.args[i], &rhs2->v.filter.args[i]))
		    return 0;
	    return 1;
	}

	case RHS_CLOSURE :
	{
	    int num_args = compiler_num_filter_args(rhs1->v.closure.filter);
	    int i;

	    if (rhs1->v.closure.filter != rhs2->v.closure.filter)
		return 0;

	    for (i = 0; i < num_args - 3; ++i)
		if (!primaries_equal(&rhs1->v.closure.args[i], &rhs2->v.closure.args[i]))
		    return 0;
	    return 1;
	}

	case RHS_TUPLE :
	case RHS_TREE_VECTOR :
	    {
		int i;

		if (rhs1->v.tuple.length != rhs2->v.tuple.length)
		    return FALSE;

		for (i = 0; i < rhs1->v.tuple.length; ++i)
		    if (!primaries_equal(&rhs1->v.tuple.args[i], &rhs2->v.tuple.args[i]))
			return FALSE;
		return TRUE;
	    }

	default :
	    g_assert_not_reached();
    }

    return 0;
}

void
compiler_replace_rhs (rhs_t **rhs, rhs_t *new, statement_t *stmt)
{
    compiler_remove_uses_in_rhs(*rhs, stmt);

    *rhs = new;

    COMPILER_FOR_EACH_VALUE_IN_RHS(*rhs, &_add_use_in_stmt, stmt);
}

static void
replace_rhs_with_value (rhs_t **rhs, value_t *val, statement_t *stmt)
{
    compiler_replace_rhs(rhs, make_value_rhs(val), stmt);
}

static void
replace_rhs_recursively (statement_t *stmt, rhs_t *rhs, value_t *val, int *changed)
{
    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_PHI_ASSIGN :
		if (rhss_equal(rhs, stmt->v.assign.rhs2))
		{
		    replace_rhs_with_value(&stmt->v.assign.rhs2, val, stmt);
		    *changed = 1;
		}
	    case STMT_ASSIGN :
		if (rhss_equal(rhs, stmt->v.assign.rhs))
		{
		    replace_rhs_with_value(&stmt->v.assign.rhs, val, stmt);
		    *changed = 1;
		}
		break;

	    case STMT_IF_COND :
		if (rhss_equal(rhs, stmt->v.if_cond.condition))
		{
		    replace_rhs_with_value(&stmt->v.if_cond.condition, val, stmt);
		    *changed = 1;
		}
		replace_rhs_recursively(stmt->v.if_cond.consequent, rhs, val, changed);
		replace_rhs_recursively(stmt->v.if_cond.alternative, rhs, val, changed);
		replace_rhs_recursively(stmt->v.if_cond.exit, rhs, val, changed);
		break;

	    case STMT_WHILE_LOOP :
		if (rhss_equal(rhs, stmt->v.while_loop.invariant))
		{
		    replace_rhs_with_value(&stmt->v.while_loop.invariant, val, stmt);
		    *changed = 1;
		}
		replace_rhs_recursively(stmt->v.while_loop.entry, rhs, val, changed);
		replace_rhs_recursively(stmt->v.while_loop.body, rhs, val, changed);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

static void
cse_recursively (statement_t *stmt, int *changed)
{
    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
		if (stmt->v.assign.rhs->kind == RHS_INTERNAL
		    || (stmt->v.assign.rhs->kind == RHS_OP
			&& stmt->v.assign.rhs->v.op.op->is_pure))
		    replace_rhs_recursively(stmt->next, stmt->v.assign.rhs, stmt->v.assign.lhs, changed);
		break;

	    case STMT_PHI_ASSIGN :
		break;

	    case STMT_IF_COND :
		cse_recursively(stmt->v.if_cond.consequent, changed);
		cse_recursively(stmt->v.if_cond.alternative, changed);
		break;

	    case STMT_WHILE_LOOP :
		cse_recursively(stmt->v.while_loop.body, changed);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

static int
common_subexpression_elimination (void)
{
    int changed = 0;

    cse_recursively(first_stmt, &changed);

    return changed;
}

/*** tuple_nth ***/

static void
optimize_tuple_nth_recursively (statement_t *stmt, gboolean *changed)
{
    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
	    case STMT_PHI_ASSIGN :
		break;

	    case STMT_ASSIGN :
		if (stmt->v.assign.rhs->kind == RHS_OP
		    && compiler_op_index(stmt->v.assign.rhs->v.op.op) == OP_TUPLE_NTH
		    && stmt->v.assign.rhs->v.op.args[0].kind == PRIMARY_VALUE
		    && stmt->v.assign.rhs->v.op.args[0].v.value->def->kind == STMT_ASSIGN
		    && stmt->v.assign.rhs->v.op.args[0].v.value->def->v.assign.rhs->kind == RHS_TUPLE)
		{
		    rhs_t *nth_rhs = stmt->v.assign.rhs;
		    rhs_t *def_rhs = nth_rhs->v.op.args[0].v.value->def->v.assign.rhs;
		    int n;

		    g_assert(nth_rhs->v.op.args[1].kind == PRIMARY_CONST
			     && nth_rhs->v.op.args[1].const_type == TYPE_INT);

		    n = nth_rhs->v.op.args[1].v.constant.int_value;

		    g_assert(n < def_rhs->v.tuple.length);

		    compiler_replace_rhs(&stmt->v.assign.rhs, make_primary_rhs(def_rhs->v.tuple.args[n]), stmt);

		    *changed = TRUE;
		}
		break;

	    case STMT_IF_COND :
		optimize_tuple_nth_recursively(stmt->v.if_cond.consequent, changed);
		optimize_tuple_nth_recursively(stmt->v.if_cond.alternative, changed);
		break;

	    case STMT_WHILE_LOOP :
		optimize_tuple_nth_recursively(stmt->v.while_loop.body, changed);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

static gboolean
optimize_tuple_nth (void)
{
    gboolean changed = FALSE;

    optimize_tuple_nth_recursively(first_stmt, &changed);

    return changed;
}

/*** make tuple ***/

static void
optimize_make_tuple_recursively (statement_t *stmt, gboolean *changed)
{
    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
	    case STMT_PHI_ASSIGN :
		break;

	    case STMT_ASSIGN :
		if (stmt->v.assign.rhs->kind == RHS_TUPLE)
		{
		    int i;
		    value_t *tuple = NULL;

		    for (i = 0; i < stmt->v.assign.rhs->v.tuple.length; ++i)
		    {
			primary_t *arg = &stmt->v.assign.rhs->v.tuple.args[i];
			statement_t *def;

			if (arg->kind != PRIMARY_VALUE)
			    break;

			def = arg->v.value->def;
			if (def->kind != STMT_ASSIGN
			    || def->v.assign.rhs->kind != RHS_OP
			    || compiler_op_index(def->v.assign.rhs->v.op.op) != OP_TUPLE_NTH)
			    break;

			g_assert(def->v.assign.rhs->v.op.args[1].kind == PRIMARY_CONST
				 && def->v.assign.rhs->v.op.args[1].const_type == TYPE_INT);

			if (def->v.assign.rhs->v.op.args[1].v.constant.int_value != i)
			    break;

			if (def->v.assign.rhs->v.op.args[0].kind != PRIMARY_VALUE)
			    break;

			if (tuple == NULL)
			    tuple = def->v.assign.rhs->v.op.args[0].v.value;
			else if (tuple != def->v.assign.rhs->v.op.args[0].v.value)
			    break;
		    }

		    if (i == stmt->v.assign.rhs->v.tuple.length)
		    {
			g_assert(tuple != NULL);

			compiler_replace_rhs(&stmt->v.assign.rhs, make_value_rhs(tuple), stmt);

			*changed = TRUE;
		    }
		}
		break;

	    case STMT_IF_COND :
		optimize_make_tuple_recursively(stmt->v.if_cond.consequent, changed);
		optimize_make_tuple_recursively(stmt->v.if_cond.alternative, changed);
		break;

	    case STMT_WHILE_LOOP :
		optimize_make_tuple_recursively(stmt->v.while_loop.body, changed);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

static gboolean
optimize_make_tuple (void)
{
    gboolean changed = FALSE;

    optimize_make_tuple_recursively(first_stmt, &changed);

    return changed;
}

/*** inlining ***/

static gboolean
can_inline (filter_t *filter, inlining_history_t *history)
{
    userval_info_t *info;

    if (filter->kind != FILTER_MATHMAP)
	return FALSE;

    while (history != NULL)
    {
	if (history->filter == filter)
	{
#ifdef DEBUG_OUTPUT
	    printf("cannot inline filter %s due to recursion\n", filter->name);
#endif
	    return FALSE;
	}
	history = history->next;
    }

    for (info = filter->userval_infos; info != NULL; info = info->next) 
    {
	userval_representation_t *rep = lookup_userval_representation(info->type);

	if (!rep)
	{
#ifdef DEBUG_OUTPUT
	    printf("cannot inline filter %s because userval %s cannot be represented\n",
		   filter->name, info->name);
#endif
	    return FALSE;
	}
    }

    return TRUE;
}

static void
insert_stmts_before (statement_t *stmts, statement_t **stmt, statement_t *new_parent)
{
    statement_t *iter;
    statement_t *last = last_stmt_of_block(stmts);

    for (iter = stmts; iter != NULL; iter = iter->next)
    {
	g_assert(iter->parent == NULL);
	iter->parent = new_parent;
    }

    g_assert(last->next == NULL);

    last->next = *stmt;
    *stmt = stmts;
}

static void
do_inlining_recursively (statement_t **stmt, gboolean *changed)
{
    while (*stmt != NULL)
    {
	switch ((*stmt)->kind)
	{
	    case STMT_NIL :
	    case STMT_PHI_ASSIGN :
		break;

	    case STMT_ASSIGN :
		if ((*stmt)->v.assign.rhs->kind == RHS_FILTER
		    && can_inline((*stmt)->v.assign.rhs->v.filter.filter, (*stmt)->v.assign.rhs->v.filter.history))
		{
		    filter_t *filter = (*stmt)->v.assign.rhs->v.filter.filter;
		    rhs_t *make_color_rhs;
		    statement_t *stmts = gen_filter_code(filter, NULL,
							 (*stmt)->v.assign.rhs->v.filter.args, &make_color_rhs,
							 (*stmt)->v.assign.rhs->v.filter.history);

#ifdef DEBUG_OUTPUT
		    printf("inlining filter %s\n", filter->name);
#endif

		    compiler_replace_rhs(&(*stmt)->v.assign.rhs, make_color_rhs, *stmt);

		    insert_stmts_before(stmts, stmt, (*stmt)->parent);

		    *changed = TRUE;
		}
		break;

	    case STMT_IF_COND :
		do_inlining_recursively(&(*stmt)->v.if_cond.consequent, changed);
		do_inlining_recursively(&(*stmt)->v.if_cond.alternative, changed);
		break;

	    case STMT_WHILE_LOOP :
		do_inlining_recursively(&(*stmt)->v.while_loop.body, changed);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = &(*stmt)->next;
    }
}

static gboolean
do_inlining (void)
{
    gboolean changed = FALSE;

    do_inlining_recursively(&first_stmt, &changed);

    return changed;
}

/*** ssa well-formedness check ***/

static void
_check_value (value_t *value, void *info)
{
    g_assert(compiler_value_set_contains(CLOSURE_GET(0,value_set_t*), value));
}

static void
check_rhs_defined (rhs_t *rhs, value_set_t *defined_set)
{
    void *closure[1] = { defined_set };

    compiler_for_each_value_in_rhs(rhs, &_check_value, closure);
}

static void
set_value_defined_and_current_for_checking (value_t *value, GHashTable *current_value_hash, value_set_t *defined_set)
{
    compiler_value_set_add(defined_set, value);
    g_hash_table_insert(current_value_hash, value->compvar, value);
}

static void
_check_phi_value (value_t *value, void *info)
{
    g_assert(value->index < 0 || (value->def->kind == STMT_ASSIGN || value->def->kind == STMT_PHI_ASSIGN));
}

static void
check_phis (statement_t *stmts, statement_t *parent, statement_t *body1, statement_t *body2,
	    GHashTable *current_value_hash, value_set_t *defined_set)
{
    while (stmts != 0)
    {
	assert(stmts->parent == parent);

	assert(stmts->kind == STMT_NIL || stmts->kind == STMT_PHI_ASSIGN);

	if (stmts->kind == STMT_PHI_ASSIGN)
	{
	    COMPILER_FOR_EACH_VALUE_IN_RHS(stmts->v.assign.rhs, &_check_phi_value);
	    COMPILER_FOR_EACH_VALUE_IN_RHS(stmts->v.assign.rhs2, &_check_phi_value);

/*
	    void check_value (value_t *value)
		{
		    if (value->index >= 0)
		    {
			value_t *value1, *value2;
			value_t *current_value = (value_t*)g_hash_table_lookup(current_value_hash, value->compvar);

			value1 = last_assignment_to_compvar(body1, value->compvar);
			value2 = last_assignment_to_compvar(body2, value->compvar);
			if (value1 == 0)
			    value1 = current_value;
			if (value2 == 0)
			    value2 = current_value;

			assert(value == value1 || value == value2);
		    }
		    else
			assert(g_hash_table_lookup(current_value_hash, value->compvar) == 0);
		}

	    for_each_value_in_rhs(stmts->v.assign.rhs, check_value);
	    for_each_value_in_rhs(stmts->v.assign.rhs2, check_value);
*/

	    /* FIXME: check that the values in rhs are defined in body1 and
	     * the ones in rhs2 are defined in body2 */
/*
	    check_rhs_defined(stmts->v.assign.rhs, defined_set);
	    check_rhs_defined(stmts->v.assign.rhs2, defined_set);
*/

	    set_value_defined_and_current_for_checking(stmts->v.assign.lhs, current_value_hash, defined_set);
	}

	stmts = stmts->next;
    }
}

static void check_ssa_recursively (statement_t *stmts, statement_t *parent,
				   GHashTable *current_value_hash, value_set_t *defined_set);

static void
check_ssa_with_undo (statement_t *stmts, statement_t *parent, GHashTable *current_value_hash, value_set_t *defined_set)
{
    GHashTable *current_value_hash_copy;
    value_set_t *defined_set_copy;

    current_value_hash_copy = direct_hash_table_copy(current_value_hash);
    defined_set_copy = compiler_value_set_copy(defined_set);

    check_ssa_recursively(stmts, parent, current_value_hash_copy, defined_set_copy);

    compiler_free_value_set(defined_set_copy);
    g_hash_table_destroy(current_value_hash_copy);
}

static void
check_ssa_recursively (statement_t *stmts, statement_t *parent, GHashTable *current_value_hash, value_set_t *defined_set)
{
    while (stmts != 0)
    {
	assert(stmts->parent == parent);

	switch (stmts->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
		check_rhs_defined(stmts->v.assign.rhs, defined_set);
		g_assert(!compiler_value_set_contains(defined_set, stmts->v.assign.lhs));
		set_value_defined_and_current_for_checking(stmts->v.assign.lhs, current_value_hash, defined_set);
		break;

	    case STMT_PHI_ASSIGN :
		g_assert_not_reached();
		break;

	    case STMT_IF_COND :
		check_rhs_defined(stmts->v.if_cond.condition, defined_set);

		check_ssa_with_undo(stmts->v.if_cond.consequent, stmts, current_value_hash, defined_set);
		check_ssa_with_undo(stmts->v.if_cond.alternative, stmts, current_value_hash, defined_set);

		check_phis(stmts->v.if_cond.exit, stmts, stmts->v.if_cond.consequent, stmts->v.if_cond.alternative,
			   current_value_hash, defined_set);
		break;

	    case STMT_WHILE_LOOP :
		check_phis(stmts->v.while_loop.entry, stmts, stmts->v.while_loop.body, 0,
			   current_value_hash, defined_set);

		check_rhs_defined(stmts->v.while_loop.invariant, defined_set);

		check_ssa_with_undo(stmts->v.while_loop.body, stmts, current_value_hash, defined_set);
		break;

	    default :
		g_assert_not_reached();
	}

	stmts = stmts->next;
    }
}

static void
check_ssa (statement_t *stmts)
{
    GHashTable *current_value_hash;
    value_set_t *defined_set;

    current_value_hash = g_hash_table_new(&g_direct_hash, &g_direct_equal);
    defined_set = compiler_new_value_set();

    check_ssa_recursively(stmts, 0, current_value_hash, defined_set);

    compiler_free_value_set(defined_set);
    g_hash_table_destroy(current_value_hash);
}

/*** code slicing ***/

/* returns whether the slice is non-empty */
int
compiler_slice_code (statement_t *stmt, unsigned int slice_flag, int (*predicate) (statement_t *stmt, void *info), void *info)
{
    int non_empty = 0;

    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
	    case STMT_PHI_ASSIGN :
		if (predicate(stmt, info))
		{
		    stmt->slice_flags |= slice_flag;
		    non_empty = 1;
		}
		break;

	    case STMT_IF_COND :
	    {
		int result;

		result = compiler_slice_code(stmt->v.if_cond.consequent, slice_flag, predicate, info);
		result = compiler_slice_code(stmt->v.if_cond.alternative, slice_flag, predicate, info) || result;
		result = compiler_slice_code(stmt->v.if_cond.exit, slice_flag, predicate, info) || result;

		if (result)
		{
		    compiler_slice_code(stmt->v.if_cond.exit, slice_flag, predicate, info);

		    stmt->slice_flags |= slice_flag;
		    non_empty = 1;
		}
		break;
	    }

	    case STMT_WHILE_LOOP :
	    {
		if (compiler_slice_code(stmt->v.while_loop.body, slice_flag, predicate, info))
		{
		    compiler_slice_code(stmt->v.while_loop.entry, slice_flag, predicate, info);

		    stmt->slice_flags |= slice_flag;
		    non_empty = 1;
		}
		else
		    assert(!compiler_slice_code(stmt->v.while_loop.entry, slice_flag, predicate, info));
		break;
	    }

	    default:
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }

    return non_empty;
}

unsigned int
compiler_slice_flag_for_const_type (int const_type)
{
    if (const_type == (CONST_X | CONST_Y))
	return SLICE_XY_CONST;
    else if (const_type == CONST_Y)
	return SLICE_Y_CONST;
    else if (const_type == CONST_X)
	return SLICE_X_CONST;
    else
	return SLICE_NO_CONST;
}

static int
_const_predicate (statement_t *stmt, void *info)
{
    CLOSURE_VAR(int, const_type, 0);

    assert(stmt->kind == STMT_ASSIGN || stmt->kind == STMT_PHI_ASSIGN);

    return compiler_is_value_needed_for_const(stmt->v.assign.lhs, const_type);
}

void
compiler_slice_code_for_const (statement_t *stmt, int const_type)
{
    unsigned int slice_flag = compiler_slice_flag_for_const_type(const_type);

    COMPILER_SLICE_CODE(stmt, slice_flag, &_const_predicate, (void*)const_type);
}

/*** compiling and loading ***/

#ifdef OPENSTEP
#ifndef MAX
#define	MAX(a,b)	(((a)<(b))?(b):(a))
#endif
#endif

#ifdef PEDANTIC_CHECK_SSA
#define CHECK_SSA	check_ssa(first_stmt)
#else
#define CHECK_SSA	do ; while (0)
#endif

static gboolean
optimization_time_out (struct timeval *start, int timeout)
{
    struct timeval now;

    if (timeout < 0)
	return FALSE;

    gettimeofday(&now, NULL);

    if (start->tv_sec + timeout < now.tv_sec)
	return TRUE;
    if (start->tv_sec + timeout == now.tv_sec && start->tv_usec <= now.tv_usec)
	return TRUE;
    return FALSE;
}

filter_code_t*
compiler_generate_ir_code (filter_t *filter, int constant_analysis, int convert_types, int timeout, gboolean debug_output)
{
    gboolean changed;
    filter_code_t *code;
    compvar_t *tuple_tmp, *dummy;
    struct timeval tv;

    g_assert(filter->kind == FILTER_MATHMAP);

    gettimeofday(&tv, NULL);

    next_temp_number = 1;
    next_compvar_number = 1;
    next_value_global_index = 0;
    inlining_history = NULL;

    tuple_tmp = make_temporary(TYPE_TUPLE);
    first_stmt = gen_filter_code(filter, tuple_tmp, NULL, NULL, inlining_history);

    emit_loc = &(last_stmt_of_block(first_stmt)->next);

    dummy = make_temporary(TYPE_INT);
    emit_assign(make_lhs(dummy), make_op_rhs(OP_OUTPUT_TUPLE, make_compvar_primary(tuple_tmp)));

    emit_loc = NULL;

    changed = TRUE;
    while (changed && !optimization_time_out(&tv, timeout))
    {
#ifdef DEBUG_OUTPUT
	check_ssa(first_stmt);
#endif

	if (debug_output)
	{
	    printf("--------------------------------\n");
	    dump_code(first_stmt, 0);
	}

	optimize_closure_application(first_stmt);
	CHECK_SSA;

	changed = FALSE;

	changed = do_inlining() || changed;
	CHECK_SSA;
	changed = copy_propagation() || changed;
	CHECK_SSA;
	changed = optimize_tuple_nth() || changed;
	CHECK_SSA;
	changed = optimize_make_tuple() || changed;
	CHECK_SSA;
	/*
	changed = compiler_opt_loop_invariant_code_motion(&first_stmt) || changed;
	CHECK_SSA;
	*/
	changed = common_subexpression_elimination() || changed;
	CHECK_SSA;
	changed = copy_propagation() || changed;
	CHECK_SSA;
	changed = constant_folding() || changed;
	CHECK_SSA;
	changed = simplify_ops() || changed;
	CHECK_SSA;

	if (debug_output)
	{
	    printf("-------------------------------- before resize\n");
	    dump_code(first_stmt, 0);
	}
	changed = compiler_opt_orig_val_resize(&first_stmt) || changed;
	CHECK_SSA;
	if (debug_output)
	{
	    printf("-------------------------------- after resize\n");
	    dump_code(first_stmt, 0);
	}

	changed = compiler_opt_strip_resize(&first_stmt) || changed;
	CHECK_SSA;
	changed = compiler_opt_simplify(filter, first_stmt) || changed;
	CHECK_SSA;

	changed = compiler_opt_remove_dead_assignments(first_stmt) || changed;
	CHECK_SSA;
	changed = remove_dead_branches() || changed;
	CHECK_SSA;
	changed = remove_dead_controls() || changed;
    }

    CHECK_SSA;
    propagate_types();

#ifdef DEBUG_OUTPUT
    check_ssa(first_stmt);
#endif

#ifndef NO_CONSTANTS_ANALYSIS
    if (constant_analysis)
	analyze_constants();
#endif

    if (debug_output)
    {
	printf("----------- final ---------------------\n");
	dump_code(first_stmt, 0);
    }
    check_ssa(first_stmt);

    /* no statement reordering after this point */

    code = (filter_code_t*)pools_alloc(&compiler_pools, sizeof(filter_code_t));

    code->filter = filter;
    code->first_stmt = first_stmt;

    first_stmt = 0;

    return code;
}

filter_code_t**
compiler_compile_filters (mathmap_t *mathmap, int timeout)
{
    filter_code_t **filter_codes;
    int num_filters, i;
    filter_t *filter;
#ifdef DEBUG_OUTPUT
    gboolean debug_output = TRUE;
#else
    gboolean debug_output = FALSE;
#endif

    init_pools(&compiler_pools);
    vector_variables = g_hash_table_new(g_direct_hash, g_direct_equal);

    num_filters = 0;
    for (filter = mathmap->filters; filter != 0; filter = filter->next)
	++num_filters;

    filter_codes = (filter_code_t**)pools_alloc(&compiler_pools, sizeof(filter_code_t*) * num_filters);

    for (i = 0, filter = mathmap->filters;
	 filter != 0;
	 ++i, filter = filter->next)
    {
	if (filter->kind != FILTER_MATHMAP)
	    continue;

#ifdef DEBUG_OUTPUT
	g_print("compiling filter %s\n", filter->name);
#endif
	filter_codes[i] = compiler_generate_ir_code(filter, 1, 0, timeout, debug_output && filter == mathmap->main_filter);
    }

    return filter_codes;
}

void
compiler_free_pools (mathmap_t *mathmap)
{
    g_hash_table_unref(vector_variables);
    free_pools(&compiler_pools);
}

/*** inits ***/

static void
init_op (int index, char *name, int num_args, type_prop_t type_prop, type_t const_type, int is_pure, int is_foldable, ...)
{
    va_list ap;

    assert(num_args <= MAX_OP_ARGS);

    va_start(ap, is_foldable);

    ops[index].index = index;
    ops[index].name = name;
    ops[index].num_args = num_args;
    ops[index].type_prop = type_prop;
    ops[index].const_type = const_type;
    ops[index].is_pure = is_pure;
    ops[index].is_foldable = is_foldable;

    if (type_prop == TYPE_PROP_CONST)
    {
	int i;

	for (i = 0; i < num_args; ++i)
	    ops[index].arg_types[i] = va_arg(ap, type_t);
    }

    va_end(ap);
}

void
init_compiler (void)
{
    init_ops();
}
