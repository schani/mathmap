/*
 * compiler.c
 *
 * MathMap
 *
 * Copyright (C) 2002-2008 Mark Probst
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
#ifndef OPENSTEP
#include <gmodule.h>
#else
#include <mach-o/dyld.h>
#endif

#include "mathmap.h"
#include "vars.h"
#include "internals.h"
#include "tags.h"
#include "exprtree.h"
#include "overload.h"
#include "internals.h"
#include "jump.h"
#include "scanner.h"
#include "bitvector.h"
#include "lispreader/pools.h"
#include "compiler.h"
#include "opmacros.h"

#define CLOSURE_GET(n,t)		(t)(((void**)info)[(n)])
#define CLOSURE_VAR(t,name,n)		t name = CLOSURE_GET((n),t)

//#define NO_CONSTANTS_ANALYSIS

struct _value_t;

typedef struct
{
    int number;
    int last_index;
} temporary_t;

#define MAX_PROMOTABLE_TYPE  TYPE_COMPLEX

#define CONST_MAX            (CONST_Y | CONST_X | CONST_T)

typedef int type_t;

typedef struct _compvar_t
{
    int index;
    variable_t *var;		/* 0 if compvar is a temporary */
    temporary_t *temp;		/* 0 if compvar is a variable */
    int n;			/* n/a if compvar is a temporary */
    type_t type;
    struct _value_t *current;
    struct _value_t *values;
} compvar_t;

struct _statement_list_t;
struct _statement_t;
struct _native_register_t;
struct _pre_native_insn_t;

typedef struct _value_t
{
    compvar_t *compvar;
    int global_index;
    int index;			/* SSA index */
    struct _statement_t *def;
    struct _statement_list_t *uses;
    struct _pre_native_insn_t *live_start;
    struct _pre_native_insn_t *live_end;
    struct _native_register_t *allocated_register;
    unsigned int const_type : 3; /* defined in internals.h */
    unsigned int least_const_type_directly_used_in : 3;
    unsigned int least_const_type_multiply_used_in : 3;
    unsigned int have_defined : 1; /* used in c code output */
    struct _value_t *next;	/* next value for same compvar */
} value_t;

typedef struct _value_list_t
{
    value_t *value;
    struct _value_list_t *next;
} value_list_t;

/* is_rhs_const_primary() assumes that PRIMARY_VALUE is the only non-const
 * primary type.  */
#define PRIMARY_VALUE          1
#define PRIMARY_CONST	       2

typedef struct
{
    int kind;
    int const_type;		/* different meanings for PRIMARY_VALUE vs PRIMARY_CONST! */
    union
    {
	value_t *value;
	runtime_value_t constant;
    } v;
} primary_t;

#define MAKE_CONST_PRIMARY(name, c_type, type_name)	\
	runtime_value_t \
	make_ ## name ## _runtime_value (c_type name ## _value) \
	{ \
	    runtime_value_t value; \
	    value.name ## _value = name ## _value; \
	    return value; \
	} \
	primary_t \
	make_ ## name ## _const_primary (c_type name ## _const) \
	{ \
	    primary_t primary; \
	    primary.kind = PRIMARY_CONST; \
	    primary.const_type = type_name; \
	    primary.v.constant.name ## _value = name ## _const; \
	    return primary; \
	}

// defined in compiler_types.h
MAKE_CONST_PRIMARY_FUNCS
MAKE_TYPE_C_TYPE_NAME

#define TYPE_PROP_CONST      1
#define TYPE_PROP_MAX        2
#define TYPE_PROP_MAX_FLOAT  3

typedef int type_prop_t;

typedef struct _operation_t
{
    int index;
    char *name;
    int num_args;
    type_prop_t type_prop;
    int is_pure;
    int is_foldable;
    type_t const_type;		/* used only if type_prop == TYPE_PROP_CONST */
    type_t arg_types[MAX_OP_ARGS]; /* used only if type_prop == TYPE_PROP_CONST */
} operation_t;

typedef struct _inlining_history_t
{
    filter_t *filter;
    struct _inlining_history_t *next;
} inlining_history_t;

#define RHS_PRIMARY          1
#define RHS_INTERNAL         2
#define RHS_OP               3
#define RHS_FILTER	     4
#define RHS_CLOSURE	     5
#define RHS_TUPLE	     6

typedef struct
{
    int kind;
    union
    {
	primary_t primary;
	internal_t *internal;
	struct
	{
	    operation_t *op;
	    primary_t args[MAX_OP_ARGS];
	} op;
	struct
	{
	    filter_t *filter;
	    primary_t *args;
	    inlining_history_t *history;
	} filter;
	struct
	{
	    filter_t *filter;
	    primary_t *args;
	    inlining_history_t *history;
	} closure;
	struct
	{
	    int length;
	    primary_t *args;
	} tuple;
    } v;
} rhs_t;

#define STMT_NIL             0
#define STMT_ASSIGN          1
#define STMT_PHI_ASSIGN      2
#define STMT_IF_COND         3
#define STMT_WHILE_LOOP      4

#define SLICE_XY_CONST       1
#define SLICE_X_CONST        2
#define SLICE_Y_CONST        4
#define SLICE_NO_CONST       8

typedef struct _statement_t
{
    int kind;
    union
    {
	struct
	{
	    value_t *lhs;
	    rhs_t *rhs;
	    rhs_t *rhs2;	/* only valid for STMT_PHI_ASSIGN */
	    value_t *old_value;	/* only valid for STMT_PHI_ASSIGN */
	} assign;
	struct
	{
	    rhs_t *condition;
	    struct _statement_t *consequent;
	    struct _statement_t *alternative;
	    struct _statement_t *exit;
	} if_cond;
	struct
	{
	    struct _statement_t *entry;
	    rhs_t *invariant;
	    struct _statement_t *body;
	} while_loop;
    } v;
    struct _statement_t *parent;
    unsigned int slice_flags;
    struct _statement_t *next;
} statement_t;

typedef struct _statement_list_t
{
    statement_t *stmt;
    struct _statement_list_t *next;
} statement_list_t;

#define PRE_NATIVE_INSN_LABEL                  0
#define PRE_NATIVE_INSN_GOTO                   1
#define PRE_NATIVE_INSN_ASSIGN                 2
#define PRE_NATIVE_INSN_PHI_ASSIGN             3
#define PRE_NATIVE_INSN_IF_COND_FALSE_GOTO     4

typedef struct _pre_native_insn_t
{
    int kind;
    int index;
    statement_t *stmt;
    union
    {
	struct _pre_native_insn_t *target;
	int phi_rhs;
    } v;
    struct _pre_native_insn_t *next;
} pre_native_insn_t;

typedef struct
{
    filter_t *filter;
    statement_t *first_stmt;
    pre_native_insn_t *first_pre_native_insn;
} filter_code_t;

typedef struct
{
    int userval_type;
    int var_type;
    int num_vars;
    int getter_op;
} userval_representation_t;

#define BINDING_USERVAL		1
#define BINDING_INTERNAL	2

typedef struct _binding_values_t
{
    int kind;
    gpointer key;
    struct _binding_values_t *next;
    value_t *values[];
} binding_values_t;

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

#include "spec_func.h"
#include "builtins.h"
#include "noise.h"

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

#define ARG(i)	(invocation->uservals[(i)])

#include "opdefs.h"

static pools_t compiler_pools;

static operation_t ops[NUM_OPS];

static int next_temp_number = 1;
static int next_compvar_number = 1;

static statement_t *first_stmt = NULL;
static statement_t **emit_loc = &first_stmt;
static statement_t dummy_stmt = { STMT_NIL };

static inlining_history_t *inlining_history = NULL;

static binding_values_t *binding_values = NULL;

static pre_native_insn_t *first_pre_native_insn;
static pre_native_insn_t *last_pre_native_insn;

#define STMT_STACK_SIZE            64

static statement_t *stmt_stack[STMT_STACK_SIZE];
static int stmt_stackp = 0;

#define CURRENT_STACK_TOP       ((stmt_stackp > 0) ? stmt_stack[stmt_stackp - 1] : 0)
#define UNSAFE_EMIT_STMT(s,l) \
    ({ (s)->parent = CURRENT_STACK_TOP; \
       (s)->next = (l); (l) = (s); })

static filter_code_t *main_filter_code;
static filter_code_t **filter_codes;

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

typedef bit_vector_t value_set_t;

/* This is updated by new_value.  We assume that no new values are generated
 * at the time value sets are used.  */
static int next_value_global_index = 0;

static value_set_t*
new_value_set (void)
{
    return new_bit_vector(next_value_global_index, 0);
}

static void
value_set_add (value_set_t *set, value_t *val)
{
    bit_vector_set(set, val->global_index);
}

static int
value_set_contains (value_set_t *set, value_t *val)
{
    return bit_vector_bit(set, val->global_index);
}

static value_set_t*
value_set_copy (value_set_t *set)
{
    return copy_bit_vector(set);
}

static void
free_value_set (value_set_t *set)
{
    free_bit_vector(set);
}

int
op_index (operation_t *op)
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
    val->live_start = val->live_end = 0;
    val->allocated_register = 0;
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
make_internal_rhs (internal_t *internal)
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

static rhs_t*
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

static int
num_filter_args (filter_t *filter)
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
	    *num_primaries = num_filter_args(rhs->v.filter.filter);
	    return rhs->v.filter.args;

	case RHS_CLOSURE :
	    *num_primaries = num_filter_args(rhs->v.closure.filter) - 3;
	    return rhs->v.closure.args;

	case RHS_TUPLE :
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

static void
for_each_value_in_rhs (rhs_t *rhs, void (*func) (value_t *value, void *info), void *info)
{
    int num_primaries;
    primary_t *primaries = get_rhs_primaries(rhs, &num_primaries);
    int i;

    for (i = 0; i < num_primaries; ++i)
	if (primaries[i].kind == PRIMARY_VALUE)
	    func(primaries[i].v.value, info);
}

#define FOR_EACH_VALUE_IN_RHS(rhs,func,...) do { void *__clos[] = { __VA_ARGS__ }; for_each_value_in_rhs((rhs),(func),__clos); } while (0)

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

#define FOR_EACH_ASSIGN_STATEMENT(stmts,func,...) do { void *__clos[] = { __VA_ARGS__ }; for_each_assign_statement((stmts),(func),__clos); } while (0)

static void
_call_func (value_t *value, void *info)
{
    void (*func) (value_t *value, statement_t *stmt, void *info) = CLOSURE_GET(0, void(*)(value_t*, statement_t*, void*));
    CLOSURE_VAR(statement_t*, stmt, 1);
    CLOSURE_VAR(void*, infoinfo, 2);

    func(value, stmt, infoinfo);
}

static void
for_each_value_in_statements (statement_t *stmt, void (*func) (value_t *value, statement_t *stmt, void *info), void *info)
{
    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_PHI_ASSIGN :
		FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs2, &_call_func, func, stmt, info);
	    case STMT_ASSIGN :
		FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs, &_call_func, func, stmt, info);
		func(stmt->v.assign.lhs, stmt, info);
		break;

	    case STMT_IF_COND :
		FOR_EACH_VALUE_IN_RHS(stmt->v.if_cond.condition, &_call_func, func, stmt, info);
		for_each_value_in_statements(stmt->v.if_cond.consequent, func, info);
		for_each_value_in_statements(stmt->v.if_cond.alternative, func, info);
		for_each_value_in_statements(stmt->v.if_cond.exit, func, info);
		break;

	    case STMT_WHILE_LOOP :
		FOR_EACH_VALUE_IN_RHS(stmt->v.while_loop.invariant, &_call_func, func, stmt, info);
		for_each_value_in_statements(stmt->v.while_loop.entry, func, info);
		for_each_value_in_statements(stmt->v.while_loop.body, func, info);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

#define FOR_EACH_VALUE_IN_STATEMENTS(stmt,func,...) do { void *__clos[] = { __VA_ARGS__ }; for_each_value_in_statements((stmt),(func),__clos); } while (0)

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

void
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
			assert(phi_assign->v.assign.rhs2->kind = RHS_PRIMARY
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

void
emit_stmt (statement_t *stmt)
{
    assert(stmt->next == 0);

    stmt->parent = CURRENT_STACK_TOP;

    stmt->next = *emit_loc;
    *emit_loc = stmt;
    emit_loc = &stmt->next;

    switch (stmt->kind)
    {
	case STMT_NIL :
	    break;

	case STMT_ASSIGN :
	    FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs, &_add_use_in_stmt, stmt);
	    stmt->v.assign.lhs->def = stmt;
	    break;

	case STMT_PHI_ASSIGN :
	    FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs, &_add_use_in_stmt, stmt);
	    FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs2, &_add_use_in_stmt, stmt);
	    stmt->v.assign.lhs->def = stmt;
	    break;

	case STMT_IF_COND :
	    FOR_EACH_VALUE_IN_RHS(stmt->v.if_cond.condition, &_add_use_in_stmt, stmt);
	    break;

	case STMT_WHILE_LOOP :
	    FOR_EACH_VALUE_IN_RHS(stmt->v.while_loop.invariant, &_add_use_in_stmt, stmt);
	    break;

	default :
	    g_assert_not_reached();
    }
}

void
emit_nil (void)
{
    statement_t *stmt = alloc_stmt();

    stmt->kind = STMT_NIL;
    stmt->next = 0;

    emit_stmt(stmt);
}

static statement_t*
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

#define STK                   (invocation->stack_machine->stack)
#define STKP                  (invocation->stack_machine->stackp)

#include "new_builtins.c"

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
print_value (value_t *val)
{
    if (val->compvar->var != 0)
	printf("%s[%d]_%d", val->compvar->var->name, val->compvar->n, val->index);
    else
	printf("$t%d_%d", val->compvar->temp->number, val->index);
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
print_primary (primary_t *primary)
{
    FILE *out = stdout;

    switch (primary->kind)
    {
	case PRIMARY_VALUE :
	    print_value(primary->v.value);
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
		int num_args = num_filter_args(rhs->v.filter.filter);
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
		int num_args = num_filter_args(rhs->v.closure.filter);
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
	    {
		int i;

		printf("tuple");
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

static void
print_assign_statement (statement_t *stmt)
{
    switch (stmt->kind)
    {
	case STMT_ASSIGN :
	    print_value(stmt->v.assign.lhs);
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
	    print_value(stmt->v.assign.lhs);
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
		print_assign_statement(stmt);
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

static void
print_liveness (value_t *value)
{
    if (value->live_start != 0 && value->live_end != 0)
	printf("  (%d - %d)", value->live_start->index, value->live_end->index);
}

static rhs_t*
pre_native_condition (pre_native_insn_t *insn)
{
    assert(insn->kind == PRE_NATIVE_INSN_IF_COND_FALSE_GOTO);

    switch (insn->stmt->kind)
    {
	case STMT_IF_COND :
	    return insn->stmt->v.if_cond.condition;
	    break;

	case STMT_WHILE_LOOP :
	    return insn->stmt->v.while_loop.invariant;
	    break;

	default :
	    g_assert_not_reached();
	    return 0;
    }
}

static void
dump_pre_native_code (void)
{
    pre_native_insn_t *insn = first_pre_native_insn;

    while (insn != 0)
    {
	printf("%4d ", insn->index);
	switch (insn->kind)
	{
	    case PRE_NATIVE_INSN_LABEL :
		printf("label\n");
		break;

	    case PRE_NATIVE_INSN_GOTO :
		printf("  goto %d\n", insn->v.target->index);
		break;

	    case PRE_NATIVE_INSN_ASSIGN :
		printf("  ");
		print_value(insn->stmt->v.assign.lhs);
		printf(" = ");
		print_rhs(insn->stmt->v.assign.rhs);
		print_liveness(insn->stmt->v.assign.lhs);
		printf("\n");
		break;

	    case PRE_NATIVE_INSN_PHI_ASSIGN :
		printf("  ");
		print_value(insn->stmt->v.assign.lhs);
		printf(" = ");
		if (insn->v.phi_rhs == 1)
		    print_rhs(insn->stmt->v.assign.rhs);
		else
		    print_rhs(insn->stmt->v.assign.rhs2);
		print_liveness(insn->stmt->v.assign.lhs);
		printf("\n");
		break;

	    case PRE_NATIVE_INSN_IF_COND_FALSE_GOTO :
		assert(insn->stmt->kind == STMT_IF_COND || insn->stmt->kind == STMT_WHILE_LOOP);

		printf("  if not ");
		print_rhs(pre_native_condition(insn));
		printf(" goto %d\n", insn->v.target->index);
		break;

	    default :
		g_assert_not_reached();
	}

	insn = insn->next;
    }
}

/*** ssa generation from tree code ***/

static void
alloc_var_compvars_if_needed (variable_t *var)
{
    int i;

    for (i = 0; i < var->type.length; ++i)
	if (var->compvar[i] == 0)
	    var->compvar[i] = make_variable(var, i);
}

static void gen_code (exprtree *tree, compvar_t **dest, int is_alloced);

static compvar_t***
gen_args (exprtree *arg_trees, int **_arglengths, int **_argnumbers)
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
	gen_code(arg, args[i], 0);
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

static void
gen_code (exprtree *tree, compvar_t **dest, int is_alloced)
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
		    gen_code(elem, dest + i, is_alloced);
	    }
	    break;

	case EXPR_SELECT :
	    {
		compvar_t *temps[tree->val.select.tuple->result.length];
		exprtree *sub;
		int i;

		gen_code(tree->val.select.tuple, temps, 0);

		for (sub = tree->val.select.subscripts, i = 0; sub != 0; sub = sub->next, ++i)
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
			int length = tree->val.select.tuple->result.length;
			int j;

			if (!is_alloced)
			    dest[i] = make_temporary(TYPE_INT);

			gen_code(sub, &subscript, 0);

			for (j = 1; j < length; ++j)
			{
			    start_if_cond(make_op_rhs(OP_LESS_INT, make_compvar_primary(subscript), make_int_const_primary(j)));
			    emit_assign(make_lhs(dest[i]), make_compvar_rhs(temps[j - 1]));
			    switch_if_branch();
			}
			emit_assign(make_lhs(dest[i]), make_compvar_rhs(temps[length - 1]));
			for (j = 0; j < length - 1; ++j)
			    end_if_cond();
		    }
		}
	    }
	    break;

	case EXPR_VARIABLE :
	    alloc_var_compvars_if_needed(tree->val.var);
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
		    emit_assign(make_lhs(dest[0]), make_internal_rhs(tree->val.internal));
	    }
	    break;

	case EXPR_ASSIGNMENT :
	    alloc_var_compvars_if_needed(tree->val.assignment.var);
	    gen_code(tree->val.assignment.value, tree->val.assignment.var->compvar, 1);
	    for (i = 0; i < tree->result.length; ++i)
		if (is_alloced)
		    emit_assign(make_lhs(dest[i]), make_compvar_rhs(tree->val.assignment.var->compvar[i]));
		else
		    dest[i] = tree->val.assignment.var->compvar[i];
	    break;

	case EXPR_SUB_ASSIGNMENT :
	    {
		compvar_t *temps[tree->val.sub_assignment.value->result.length];
		exprtree *sub;
		int i;

		alloc_var_compvars_if_needed(tree->val.sub_assignment.var);

		gen_code(tree->val.sub_assignment.value, temps, 0);

		for (sub = tree->val.sub_assignment.subscripts, i = 0; sub != 0; sub = sub->next, ++i)
		{
		    int subscript;
		    
		    if (is_exprtree_single_const(sub, &subscript, 0))
		    {
			if (subscript < 0)
			    subscript = 0;
			if (subscript >= tree->val.sub_assignment.var->type.length)
			    subscript = tree->val.sub_assignment.var->type.length - 1;

			emit_assign(make_lhs(tree->val.sub_assignment.var->compvar[subscript]), make_compvar_rhs(temps[i]));
		    }
		    else
		    {
			compvar_t *subscript;
			int length = tree->val.sub_assignment.var->type.length;
			int j;

			if (!is_alloced)
			    dest[i] = make_temporary(TYPE_INT);

			gen_code(sub, &subscript, 0);

			for (j = 1; j < length; ++j)
			{
			    start_if_cond(make_op_rhs(OP_LESS_INT, make_compvar_primary(subscript), make_int_const_primary(j)));
			    emit_assign(make_lhs(tree->val.sub_assignment.var->compvar[j - 1]), make_compvar_rhs(temps[i]));
			    switch_if_branch();
			}
			emit_assign(make_lhs(tree->val.sub_assignment.var->compvar[length - 1]), make_compvar_rhs(temps[i]));
			for (j = 0; j < length - 1; ++j)
			    end_if_cond();
		    }

		    if (is_alloced)
			emit_assign(make_lhs(dest[i]), make_compvar_rhs(temps[i]));
		    else
			dest[i] = temps[i];
		}
	    }
	    break;

	case EXPR_CAST :
	    gen_code(tree->val.cast.tuple, dest, is_alloced);
	    break;

	case EXPR_FUNC :
	    {
		compvar_t ***args;
		int *arglengths, *argnumbers;

		args = gen_args(tree->val.func.args, &arglengths, &argnumbers);

		if (!is_alloced)
		    for (i = 0; i < tree->result.length; ++i)
			dest[i] = make_temporary(compiler_type_from_tuple_info(&tree->result));

		tree->val.func.entry->v.builtin_generator(args, arglengths, argnumbers, dest);
	    }
	    break;
 
	case EXPR_SEQUENCE :
	    {
		compvar_t **left_result;

		left_result = (compvar_t**)alloca(tree->val.operator.left->result.length * sizeof(compvar_t*));
		gen_code(tree->val.operator.left, left_result, 0);

		gen_code(tree->val.operator.right, dest, is_alloced);
	    }
	    break;

	case EXPR_IF_THEN :
	case EXPR_IF_THEN_ELSE :
	    {
		compvar_t *condition;
		compvar_t **result = (compvar_t**)alloca(tree->result.length * sizeof(compvar_t*));

		for (i = 0; i < tree->result.length; ++i)
		    result[i] = make_temporary(compiler_type_from_tuple_info(&tree->result));

		gen_code(tree->val.ifExpr.condition, &condition, 0);

		start_if_cond(make_compvar_rhs(condition));

		gen_code(tree->val.ifExpr.consequent, result, 1);

		switch_if_branch();

		if (tree->type == EXPR_IF_THEN_ELSE)
		    gen_code(tree->val.ifExpr.alternative, result, 1);

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
		    gen_code(tree->val.whileExpr.body, body_result, 0);

		gen_code(tree->val.whileExpr.invariant, &invariant, 1);
		start_while_loop(make_compvar_rhs(invariant));
		gen_code(tree->val.whileExpr.body, body_result, 0);
		gen_code(tree->val.whileExpr.invariant, &invariant, 1);
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
		int num_args = num_filter_args(tree->val.filter_closure.filter) - 3;
		userval_info_t *infos = tree->val.filter_closure.filter->userval_infos;
		userval_info_t *info;
		int i;

		args = gen_args(tree->val.filter_closure.args, &arglengths, &argnumbers);

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
		    else
		    {
			g_assert(arglengths[i] == 1);
			compvar = args[i][0];
		    }

		    arg_primaries[i] = make_compvar_primary(compvar);
		}

		if (!is_alloced)
		    dest[0] = make_temporary(TYPE_IMAGE);
		emit_assign(make_lhs(dest[0]), make_closure_rhs(tree->val.filter_closure.filter, arg_primaries));
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
gen_binding_values_from_userval_infos (userval_info_t *info)
{
    binding_values_t *bvs = NULL;

    while (info != NULL)
    {
	userval_representation_t *rep = lookup_userval_representation(info->type);

	if (rep != NULL)
	{
	    bvs = new_binding_values(BINDING_USERVAL, info, bvs, rep->num_vars, rep->var_type);

	    emit_assign(bvs->values[0],
			make_op_rhs(rep->getter_op, make_int_const_primary(info->index)));
	}

	info = info->next;
    }

    return bvs;
}

static binding_values_t*
gen_binding_values_from_filter_args (filter_t *filter, primary_t *args)
{
    int num_args = num_filter_args(filter);
    userval_info_t *info;
    int i;
    binding_values_t *bvs = NULL;
    internal_t *internal;

    g_assert(filter->kind == FILTER_MATHMAP);

    for (i = 0, info = filter->userval_infos;
	 i < num_args - 3;
	 ++i, info = info->next)
    {
	userval_representation_t *rep = lookup_userval_representation(info->type);

	g_assert(rep != NULL);
	g_assert(rep->num_vars == 1);

	bvs = new_binding_values(BINDING_USERVAL, info, bvs, rep->num_vars, rep->var_type);

	emit_assign(bvs->values[0], make_primary_rhs(args[i]));
    }
    g_assert(info == NULL);

    internal = lookup_internal(filter->v.mathmap.internals, "x", TRUE);
    g_assert(internal != NULL);
    bvs = new_binding_values(BINDING_INTERNAL, internal, bvs, 1, TYPE_INT);
    emit_assign(bvs->values[0], make_primary_rhs(args[num_args - 3]));

    internal = lookup_internal(filter->v.mathmap.internals, "y", TRUE);
    g_assert(internal != NULL);
    bvs = new_binding_values(BINDING_INTERNAL, internal, bvs, 1, TYPE_INT);
    emit_assign(bvs->values[0], make_primary_rhs(args[num_args - 2]));

    internal = lookup_internal(filter->v.mathmap.internals, "t", TRUE);
    g_assert(internal != NULL);
    bvs = new_binding_values(BINDING_INTERNAL, internal, bvs, 1, TYPE_INT);
    emit_assign(bvs->values[0], make_primary_rhs(args[num_args - 1]));

    return bvs;
}

static value_t*
get_internal_value (filter_t *filter, const char *name)
{
    internal_t *internal;
    binding_values_t *bv;

    g_assert(filter->kind == FILTER_MATHMAP);

    internal = lookup_internal(filter->v.mathmap.internals, name, TRUE);
    g_assert(internal != NULL);

    bv = lookup_binding_values(BINDING_INTERNAL, internal);
    if (bv != NULL)
	return bv->values[0];
    else
    {
	compvar_t *temp = make_temporary(TYPE_INT);
	emit_assign(make_lhs(temp), make_internal_rhs(internal));
	return current_value(temp);
    }
}

static binding_values_t*
gen_ra_binding_values (filter_t *filter, binding_values_t *bvs)
{
    compvar_t *r = make_temporary(TYPE_FLOAT);
    compvar_t *a = make_temporary(TYPE_FLOAT);
    compvar_t *x_over_r = make_temporary(TYPE_FLOAT);
    value_t *x = get_internal_value(filter, "x");
    value_t *y = get_internal_value(filter, "y");
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

    bvs = new_binding_values(BINDING_INTERNAL, lookup_internal(filter->v.mathmap.internals, "r", TRUE), bvs, 1, TYPE_FLOAT);
    emit_assign(bvs->values[0], make_compvar_rhs(r));

    bvs = new_binding_values(BINDING_INTERNAL, lookup_internal(filter->v.mathmap.internals, "a", TRUE), bvs, 1, TYPE_FLOAT);
    emit_assign(bvs->values[0], make_compvar_rhs(a));

    return bvs;
}

static statement_t*
gen_filter_code (filter_t *filter, compvar_t *tuple, primary_t *args, rhs_t **tuple_rhs, inlining_history_t *history)
{
    statement_t *first_stmt_save = first_stmt;
    inlining_history_t *history_save = inlining_history;
    statement_t *stmt;
    compvar_t *result[filter->v.mathmap.decl->v.filter.body->result.length];
    rhs_t *rhs;

    compiler_reset_variables(filter->v.mathmap.variables);

    inlining_history = push_inlined_filter(filter, history);

    first_stmt = NULL;
    emit_loc = &first_stmt;
    if (args != NULL)
	binding_values = gen_binding_values_from_filter_args(filter, args);
    else
	binding_values = gen_binding_values_from_userval_infos(filter->userval_infos);

    if (does_filter_use_ra(filter))
	binding_values = gen_ra_binding_values(filter, binding_values);

    gen_code(filter->v.mathmap.decl->v.filter.body, result, FALSE);

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
    binding_values = NULL;

    inlining_history = history_save;

    return stmt;
}

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

#define PERFORM_WORKLIST_DFA(stmts,build,work,...) do { void *__clos[] = { __VA_ARGS__ }; perform_worklist_dfa((stmts),(build),(work),__clos); } while (0)


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
		FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs2, &_clear_const_bits_from_assignment, stmt);
	    case STMT_ASSIGN :
		FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs, &_clear_const_bits_from_assignment, stmt);
		least_const_type &= LEAST_CONST_TYPE(stmt->v.assign.lhs);
		break;

	    case STMT_IF_COND :
	    {
		unsigned int sub_least_const_type = CONST_MAX;

		sub_least_const_type &= analyze_least_const_type_directly_used_in(stmt->v.if_cond.consequent);
		sub_least_const_type &= analyze_least_const_type_directly_used_in(stmt->v.if_cond.alternative);
		sub_least_const_type &= analyze_least_const_type_directly_used_in(stmt->v.if_cond.exit);

		FOR_EACH_VALUE_IN_RHS(stmt->v.if_cond.condition, &_clear_const_bits, (void*)sub_least_const_type);

		least_const_type &= sub_least_const_type;

		break;
	    }

	    case STMT_WHILE_LOOP :
	    {
		unsigned int sub_least_const_type = CONST_MAX;

		sub_least_const_type &= analyze_least_const_type_directly_used_in(stmt->v.while_loop.entry);
		sub_least_const_type &= analyze_least_const_type_directly_used_in(stmt->v.while_loop.body);

		FOR_EACH_VALUE_IN_RHS(stmt->v.while_loop.invariant, &_clear_const_bits, (void*)sub_least_const_type);

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

    if (value_set_contains(update_const_set, val)
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
		FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs2, &_update_const,
				      update_const_set, (void*)update_const_mask, changed);
	    case STMT_ASSIGN :
		update_const_mask = stmt->v.assign.lhs->least_const_type_multiply_used_in;
		FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs, &_update_const,
				      update_const_set, (void*)update_const_mask, changed);
		if (in_loop)
		    value_set_add(multiply_assigned_set, stmt->v.assign.lhs);
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
		FOR_EACH_VALUE_IN_RHS(stmt->v.if_cond.condition, &_update_const,
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

		copy = value_set_copy(multiply_assigned_set);
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
		FOR_EACH_VALUE_IN_RHS(stmt->v.while_loop.invariant, &_update_const,
				      update_const_set, (void*)update_const_mask, changed);
		update_const_set = multiply_assigned_set;

		free_value_set(copy);

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

    FOR_EACH_VALUE_IN_STATEMENTS(first_stmt, &_init_const_type);

    do
    {
	changed = 0;
	analyze_stmts_constants(first_stmt, &changed, CONST_MAX);
    } while (changed);

    FOR_EACH_VALUE_IN_STATEMENTS(first_stmt, &_init_least_const_types);

    do
    {
	value_set_t *set = new_value_set();

	changed = 0;
	analyze_least_const_type_multiply_used_in(first_stmt, 0, set, &changed);

	free_value_set(set);
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
		    && op_index(stmt->v.assign.rhs->v.op.op) == OP_ORIG_VAL
		    && stmt->v.assign.rhs->v.op.args[2].kind == PRIMARY_VALUE)
		{
		    statement_t *def = stmt->v.assign.rhs->v.op.args[2].v.value->def;

		    if (def->kind == STMT_ASSIGN
			&& def->v.assign.rhs->kind == RHS_CLOSURE
			&& def->v.assign.rhs->v.closure.filter->kind == FILTER_MATHMAP)
		    {
			filter_t *filter = def->v.assign.rhs->v.filter.filter;
			int num_args = num_filter_args(filter);
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
		FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs, &_rewrite_if_possible, stmt, copy_hash, changed);
		FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs2, &_rewrite_if_possible, stmt, copy_hash, changed);
		break;

	    case STMT_ASSIGN :
		FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs, &_rewrite_if_possible, stmt, copy_hash, changed);

		if (stmt->v.assign.rhs->kind == RHS_PRIMARY)
		{
		    primary_t *copy_copy = alloc_primary();

		    assert(copy_copy != 0);
		    *copy_copy = stmt->v.assign.rhs->v.primary;

		    g_hash_table_insert(copy_hash, stmt->v.assign.lhs, copy_copy);
		}
		break;

	    case STMT_IF_COND :
		FOR_EACH_VALUE_IN_RHS(stmt->v.if_cond.condition, &_rewrite_if_possible, stmt, copy_hash, changed);
		copy_propagate_recursively(stmt->v.if_cond.consequent, copy_hash, changed);
		copy_propagate_recursively(stmt->v.if_cond.alternative, copy_hash, changed);
		copy_propagate_recursively(stmt->v.if_cond.exit, copy_hash, changed);
		break;

	    case STMT_WHILE_LOOP :
		FOR_EACH_VALUE_IN_RHS(stmt->v.while_loop.invariant, &_rewrite_if_possible, stmt, copy_hash, changed);
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

    switch (op_index(rhs->v.op.op))
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

/*** dead assignment removal ***/

static gboolean
rhs_is_pure (rhs_t *rhs)
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

static value_list_t*
add_value_if_new (value_list_t *list, value_t *value)
{
    value_list_t *l;

    for (l = list; l != 0; l = l->next)
	if (l->value == value)
	    return list;

    l = (value_list_t*)pools_alloc(&compiler_pools, sizeof(value_list_t));
    l->value = value;
    l->next = list;

    return l;
}

static void
_remove_value (value_t *value, void *info)
{
    CLOSURE_VAR(statement_t*, stmt, 0);
    CLOSURE_VAR(value_list_t**, worklist, 1);
    CLOSURE_VAR(int*, changed, 2);

    assert(value->index < 0 || value->def->kind != STMT_NIL);
    remove_use(value, stmt);
    if (value->uses == 0)
	*worklist = add_value_if_new(*worklist, value);

    *changed = 1;
}

static void
remove_assign_stmt_if_pure (statement_t *stmt, value_list_t **worklist, int *changed)
{
    assert(stmt->v.assign.lhs->uses == 0);

    if (!rhs_is_pure(stmt->v.assign.rhs))
	return;
    if (stmt->kind == STMT_PHI_ASSIGN
	&& !rhs_is_pure(stmt->v.assign.rhs2))
	return;

    FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs, &_remove_value, stmt, worklist, changed);
    if (stmt->kind == STMT_PHI_ASSIGN)
	FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs2, &_remove_value, stmt, worklist, changed);

    stmt->kind = STMT_NIL;
}

static void
remove_dead_code_initially (statement_t *stmt, value_list_t **worklist)
{
    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
	    case STMT_PHI_ASSIGN :
		if (stmt->v.assign.lhs->uses == 0)
		    *worklist = add_value_if_new(*worklist, stmt->v.assign.lhs);
		break;

	    case STMT_IF_COND :
		remove_dead_code_initially(stmt->v.if_cond.consequent, worklist);
		remove_dead_code_initially(stmt->v.if_cond.alternative, worklist);
		remove_dead_code_initially(stmt->v.if_cond.exit, worklist);
		break;

	    case STMT_WHILE_LOOP :
		remove_dead_code_initially(stmt->v.while_loop.entry, worklist);
		remove_dead_code_initially(stmt->v.while_loop.body, worklist);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

static void
remove_dead_code_from_worklist (value_list_t *worklist, value_list_t **new_worklist, int *changed)
{
    while (worklist != 0)
    {
	assert(worklist->value->uses == 0);

	if (worklist->value->def->kind == STMT_NIL)
	    assert(worklist->value->def == &dummy_stmt);
	else
	{
	    assert(worklist->value->def->kind == STMT_ASSIGN
		   || worklist->value->def->kind == STMT_PHI_ASSIGN);

	    remove_assign_stmt_if_pure(worklist->value->def, new_worklist, changed);
	}

	worklist = worklist->next;
    }
}

static int
remove_dead_assignments (void)
{
    int changed = 0;
    value_list_t *worklist = 0;

    remove_dead_code_initially(first_stmt, &worklist);

    do
    {
	value_list_t *new_worklist = 0;

	remove_dead_code_from_worklist(worklist, &new_worklist, &changed);
	worklist = new_worklist;
    } while (worklist != 0);

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

static void
remove_uses_in_rhs (rhs_t *rhs, statement_t *stmt)
{
    FOR_EACH_VALUE_IN_RHS(rhs, &_remove_value_use, stmt);
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
				remove_uses_in_rhs(phi->v.assign.rhs2, phi);
			    else
				remove_uses_in_rhs(phi->v.assign.rhs, phi);

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
		    && rhs_is_pure(stmt->v.if_cond.condition))
		{
		    remove_uses_in_rhs(stmt->v.if_cond.condition, stmt);
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
	    int num_args = num_filter_args(rhs1->v.filter.filter);
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
	    int num_args = num_filter_args(rhs1->v.closure.filter);
	    int i;

	    if (rhs1->v.closure.filter != rhs2->v.closure.filter)
		return 0;

	    for (i = 0; i < num_args - 3; ++i)
		if (!primaries_equal(&rhs1->v.closure.args[i], &rhs2->v.closure.args[i]))
		    return 0;
	    return 1;
	}

	case RHS_TUPLE :
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

static void
replace_rhs (rhs_t **rhs, rhs_t *new, statement_t *stmt)
{
    remove_uses_in_rhs(*rhs, stmt);

    *rhs = new;

    FOR_EACH_VALUE_IN_RHS(*rhs, &_add_use_in_stmt, stmt);
}

static void
replace_rhs_with_value (rhs_t **rhs, value_t *val, statement_t *stmt)
{
    replace_rhs(rhs, make_value_rhs(val), stmt);
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
		    && op_index(stmt->v.assign.rhs->v.op.op) == OP_TUPLE_NTH
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

		    replace_rhs(&stmt->v.assign.rhs, make_primary_rhs(def_rhs->v.tuple.args[n]), stmt);

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
			    || op_index(def->v.assign.rhs->v.op.op) != OP_TUPLE_NTH)
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

			replace_rhs(&stmt->v.assign.rhs, make_value_rhs(tuple), stmt);

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

		    replace_rhs(&(*stmt)->v.assign.rhs, make_color_rhs, *stmt);

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

/*** pre native code generation ***/

static pre_native_insn_t*
new_pre_native_insn (int kind, statement_t *stmt)
{
    pre_native_insn_t *insn = (pre_native_insn_t*)pools_alloc(&compiler_pools, sizeof(pre_native_insn_t));

    insn->kind = kind;
    insn->index = -1;
    insn->stmt = stmt;
    insn->next = 0;

    return insn;
}

static pre_native_insn_t*
new_pre_native_insn_goto (int kind, statement_t *stmt, pre_native_insn_t *target)
{
    pre_native_insn_t *insn = new_pre_native_insn(kind, stmt);

    insn->v.target = target;

    return insn;
}

static pre_native_insn_t*
new_pre_native_insn_phi_assign (statement_t *stmt, int phi_rhs)
{
    pre_native_insn_t *insn = new_pre_native_insn(PRE_NATIVE_INSN_PHI_ASSIGN, stmt);

    insn->v.phi_rhs = phi_rhs;

    return insn;
}

static void
emit_pre_native_insn (pre_native_insn_t *insn)
{
    assert(insn->next == 0);

    if (last_pre_native_insn == 0)
    {
	assert(first_pre_native_insn == 0);

	insn->index = 0;

	first_pre_native_insn = last_pre_native_insn = insn;
    }
    else
    {
	assert(first_pre_native_insn != 0);

	insn->index = last_pre_native_insn->index + 1;

	last_pre_native_insn = last_pre_native_insn->next = insn;
    }
}

/*
static int
can_be_converted (type_t src, type_t dst)
{
    return src <= MAX_PROMOTABLE_TYPE && dst <= MAX_PROMOTABLE_TYPE
	&& (src < dst
	    || (src == TYPE_FLOAT && dst == TYPE_INT));
}
*/

static int
get_conversion_op (type_t src, type_t dst)
{
    switch (src)
    {
	case TYPE_INT :
	    switch (dst)
	    {
		case TYPE_FLOAT :
		    return OP_INT_TO_FLOAT;
		case TYPE_COMPLEX :
		    return OP_INT_TO_COMPLEX;
		default :
		    g_assert_not_reached();
	    }
	    break;

	case TYPE_FLOAT :
	    switch (dst)
	    {
		case TYPE_INT :
		    return OP_FLOAT_TO_INT;
		case TYPE_COMPLEX :
		    return OP_FLOAT_TO_COMPLEX;
		default :
		    g_assert_not_reached();
	    }
	    break;

	default :
	    g_assert_not_reached();
    }
}

static rhs_t*
make_convert_rhs (value_t *value, type_t type)
{
    int op = get_conversion_op(value->compvar->type, type);

    return make_op_rhs(op, make_value_primary(value));
}

static statement_t*
convert_primary (primary_t *src, type_t type, primary_t *dst)
{
    type_t src_type = primary_type(src);
    value_t *temp = make_lhs(make_temporary(type));

    *dst = make_value_primary(temp);

    return make_assign(temp, make_op_rhs(get_conversion_op(src_type, type), *src));
}

static statement_t*
convert_rhs (rhs_t *rhs, value_t *lhs)
{
    value_t *temp = make_lhs(make_temporary(rhs_type(rhs)));
    statement_t *stmts;

    stmts = make_assign(temp, rhs);
    stmts->next = make_assign(lhs, make_convert_rhs(temp, lhs->compvar->type));

    return stmts;
}

static void
generate_pre_native_assigns (statement_t *stmt)
{
    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_ASSIGN :
		emit_pre_native_insn(new_pre_native_insn(PRE_NATIVE_INSN_ASSIGN, stmt));
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

static void
convert_args (int num_args, type_t *arg_types, primary_t *dst, primary_t *src)
{
    int i;

    for (i = 0; i < num_args; ++i)
	if (arg_types[i] != primary_type(&src[i]))
	{
	    statement_t *stmts = convert_primary(&src[i], arg_types[i], &dst[i]);

	    generate_pre_native_assigns(stmts);
	}
	else
	    dst[i] = src[i];
}

static void
emit_pre_native_assign_with_op_rhs (value_t *lhs, rhs_t *rhs)
{
    type_t lhs_type = lhs->compvar->type;
    operation_t *op;
    type_t arg_types[MAX_OP_ARGS];
    primary_t args[MAX_OP_ARGS];
    type_t op_type;
    int i;

    assert(rhs->kind == RHS_OP);

    op = rhs->v.op.op;

    if (op->type_prop == TYPE_PROP_CONST)
    {
	op_type = op->const_type;
	memcpy(arg_types, op->arg_types, sizeof(type_t) * op->num_args);
    }
    else
    {
	op_type = TYPE_INT;
	for (i = 0; i < op->num_args; ++i)
	{
	    type_t arg_type = primary_type(&rhs->v.op.args[i]);

	    if (arg_type > op_type)
	    {
		assert(arg_type <= MAX_PROMOTABLE_TYPE);
		op_type = arg_type;
	    }
	}

	for (i = 0; i < op->num_args; ++i)
	    arg_types[i] = op_type;
    }

    convert_args(op->num_args, arg_types, args, rhs->v.op.args);

    if (op_type != lhs_type)
    {
	value_t *temp = make_lhs(make_temporary(op_type));
	rhs_t *new_rhs = make_op_rhs_from_array(rhs->v.op.op->index, args);
	statement_t *stmts;

	stmts = make_assign(temp, new_rhs);
	stmts->next = make_assign(lhs, make_convert_rhs(temp, lhs_type));

	generate_pre_native_assigns(stmts);
    }
    else
    {
	statement_t *stmts = make_assign(lhs, make_op_rhs_from_array(rhs->v.op.op->index, args));

	generate_pre_native_assigns(stmts);
    }
}

static int
type_for_userval_type (int userval_type)
{
    switch (userval_type)
    {
	case USERVAL_INT_CONST :
	    return TYPE_INT;
	case USERVAL_FLOAT_CONST :
	    return TYPE_FLOAT;
	case USERVAL_BOOL_CONST :
	    return TYPE_INT;
	case USERVAL_COLOR :
	    return TYPE_COLOR;
	case USERVAL_IMAGE :
	    return TYPE_IMAGE;
	default :
	    g_assert_not_reached();
    }
}

static type_t*
get_filter_arg_types (filter_t *filter, int *_num_args)
{
    int num_args = num_filter_args(filter);
    type_t *arg_types = (type_t*)pools_alloc(&compiler_pools, sizeof(type_t) * num_args);
    int i;
    userval_info_t *info;

    for (i = 0, info = filter->userval_infos; info != 0; ++i, info = info->next)
	arg_types[i] = type_for_userval_type(info->type);
    g_assert(i == num_args - 3);

    arg_types[num_args + 0] = TYPE_FLOAT; /* x */
    arg_types[num_args + 1] = TYPE_FLOAT; /* y */
    arg_types[num_args + 2] = TYPE_FLOAT; /* t */

    *_num_args = num_args + 3;

    return arg_types;
}

static void
emit_pre_native_assign_with_filter_rhs (value_t *lhs, rhs_t *rhs)
{
    type_t lhs_type = lhs->compvar->type;
    type_t *arg_types;
    primary_t *args;
    int num_args;
    statement_t *stmts;

    g_assert(rhs->kind == RHS_FILTER);

    arg_types = get_filter_arg_types(rhs->v.filter.filter, &num_args);

    args = (primary_t*)pools_alloc(&compiler_pools, sizeof(primary_t) * num_args);

    convert_args(num_args, arg_types, args, rhs->v.filter.args);

    g_assert (lhs_type == TYPE_TUPLE);

    stmts = make_assign(lhs, make_filter_rhs(rhs->v.filter.filter, args));

    generate_pre_native_assigns(stmts);
}

static void
emit_pre_native_assign_with_closure_rhs (value_t *lhs, rhs_t *rhs)
{
    type_t lhs_type = lhs->compvar->type;
    type_t *arg_types;
    primary_t *args;
    int num_args;
    statement_t *stmts;

    g_assert(rhs->kind == RHS_CLOSURE);

    arg_types = get_filter_arg_types(rhs->v.closure.filter, &num_args);

    args = (primary_t*)pools_alloc(&compiler_pools, sizeof(primary_t) * (num_args - 3));

    convert_args(num_args - 3, arg_types, args, rhs->v.closure.args);

    g_assert (lhs_type == TYPE_IMAGE);

    stmts = make_assign(lhs, make_closure_rhs(rhs->v.closure.filter, args));

    generate_pre_native_assigns(stmts);
}

static void
emit_pre_native_assign_with_tuple_rhs (value_t *lhs, rhs_t *rhs)
{
    type_t lhs_type = lhs->compvar->type;
    type_t arg_types[rhs->v.tuple.length];
    primary_t *args;
    statement_t *stmts;
    int i;

    g_assert(rhs->kind == RHS_TUPLE);

    for (i = 0; i < rhs->v.tuple.length; ++i)
	arg_types[i] = TYPE_FLOAT;

    args = (primary_t*)pools_alloc(&compiler_pools, sizeof(primary_t) * rhs->v.tuple.length);

    convert_args(rhs->v.tuple.length, arg_types, args, rhs->v.tuple.args);

    g_assert (lhs_type == TYPE_TUPLE);

    stmts = make_assign(lhs, make_tuple_rhs_from_array(rhs->v.tuple.length, args));

    generate_pre_native_assigns(stmts);
}

static void
emit_pre_native_assign_with_conversion (value_t *lhs, rhs_t *rhs, statement_t *stmt)
{
    type_t lhs_type = lhs->compvar->type;

    switch (rhs->kind)
    {
	case RHS_PRIMARY :
	case RHS_INTERNAL :
	    if (rhs_type(rhs) != lhs_type)
		generate_pre_native_assigns(convert_rhs(rhs, lhs));
	    else
	    {
		if (stmt == 0)
		    stmt = make_assign(lhs, rhs);

		emit_pre_native_insn(new_pre_native_insn(PRE_NATIVE_INSN_ASSIGN, stmt));
	    }
	    break;

	case RHS_OP :
	    emit_pre_native_assign_with_op_rhs(lhs, rhs);
	    break;

	case RHS_FILTER :
	    emit_pre_native_assign_with_filter_rhs(lhs, rhs);
	    break;

	case RHS_CLOSURE :
	    emit_pre_native_assign_with_closure_rhs(lhs, rhs);
	    break;

	case RHS_TUPLE :
	    emit_pre_native_assign_with_tuple_rhs(lhs, rhs);
	    break;

	default :
	    g_assert_not_reached();
    }
}

static value_t*
emit_rhs_with_conversion (rhs_t *rhs, type_t target_type)
{
    type_t type = rhs_type(rhs);
    value_t *temp = make_lhs(make_temporary(type));
    value_t *final_temp;

    emit_pre_native_assign_with_conversion(temp, rhs, 0);
    if (type == target_type)
	final_temp = temp;
    else
    {
	final_temp = make_lhs(make_temporary(target_type));
	generate_pre_native_assigns(convert_rhs(make_value_rhs(temp), final_temp));
    }

    return final_temp;
}

static statement_t*
convert_if_stmt (statement_t *stmt)
{
    value_t *temp = emit_rhs_with_conversion(stmt->v.if_cond.condition, TYPE_INT);
    statement_t *new_stmt = alloc_stmt();

    memcpy(new_stmt, stmt, sizeof(statement_t));
    new_stmt->next = 0;
    new_stmt->v.if_cond.condition = make_value_rhs(temp);

    return new_stmt;
}

static statement_t*
convert_while_stmt (statement_t *stmt)
{
    value_t *temp = emit_rhs_with_conversion(stmt->v.while_loop.invariant, TYPE_INT);
    statement_t *new_stmt = alloc_stmt();

    memcpy(new_stmt, stmt, sizeof(statement_t));
    new_stmt->next = 0;
    new_stmt->v.while_loop.invariant = make_value_rhs(temp);

    return new_stmt;
}

static void
generate_pre_native_code_for_phis (statement_t *stmt, int phi_rhs, int convert_types)
{
    while (stmt != 0)
    {
	if (stmt->kind == STMT_PHI_ASSIGN)
	{
	    if (convert_types)
	    {
		rhs_t *rhs = (phi_rhs == 1) ? stmt->v.assign.rhs : stmt->v.assign.rhs2;

		if (rhs_type(rhs) != stmt->v.assign.lhs->compvar->type)
		{
		    generate_pre_native_assigns(convert_rhs(rhs, stmt->v.assign.lhs));
		    goto next_stmt;
		}
	    }

	    emit_pre_native_insn(new_pre_native_insn_phi_assign(stmt, phi_rhs));
	}
	else
	    assert(stmt->kind == STMT_NIL);

    next_stmt:
	stmt = stmt->next;
    }
}

static void
generate_pre_native_code_recursively (statement_t *stmt, int convert_types)
{
    while (stmt != 0)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
		if (convert_types)
		    emit_pre_native_assign_with_conversion(stmt->v.assign.lhs, stmt->v.assign.rhs, stmt);
		else
		    emit_pre_native_insn(new_pre_native_insn(PRE_NATIVE_INSN_ASSIGN, stmt));
		break;

	    case STMT_IF_COND :
		{
		    pre_native_insn_t *label_alternative = new_pre_native_insn(PRE_NATIVE_INSN_LABEL, 0);
		    pre_native_insn_t *label_end = new_pre_native_insn(PRE_NATIVE_INSN_LABEL, 0);
		    statement_t *converted_stmt;

		    if (convert_types)
			converted_stmt = convert_if_stmt(stmt);
		    else
			converted_stmt = stmt;

		    emit_pre_native_insn(new_pre_native_insn_goto(PRE_NATIVE_INSN_IF_COND_FALSE_GOTO,
								  converted_stmt, label_alternative));
		    generate_pre_native_code_recursively(converted_stmt->v.if_cond.consequent, convert_types);
		    generate_pre_native_code_for_phis(converted_stmt->v.if_cond.exit, 1, convert_types);
		    emit_pre_native_insn(new_pre_native_insn_goto(PRE_NATIVE_INSN_GOTO, converted_stmt, label_end));
		    emit_pre_native_insn(label_alternative);
		    generate_pre_native_code_recursively(converted_stmt->v.if_cond.alternative, convert_types);
		    generate_pre_native_code_for_phis(converted_stmt->v.if_cond.exit, 2, convert_types);
		    emit_pre_native_insn(label_end);
		}
		break;

	    case STMT_WHILE_LOOP :
		{
		    pre_native_insn_t *label_start = new_pre_native_insn(PRE_NATIVE_INSN_LABEL, 0);
		    pre_native_insn_t *label_end = new_pre_native_insn(PRE_NATIVE_INSN_LABEL, 0);
		    statement_t *converted_stmt;

		    if (convert_types)
			converted_stmt = convert_while_stmt(stmt);
		    else
			converted_stmt = stmt;

		    generate_pre_native_code_for_phis(stmt->v.while_loop.entry, 1, convert_types);
		    emit_pre_native_insn(label_start);
		    emit_pre_native_insn(new_pre_native_insn_goto(PRE_NATIVE_INSN_IF_COND_FALSE_GOTO,
								  stmt, label_end));
		    generate_pre_native_code_recursively(stmt->v.while_loop.body, convert_types);
		    generate_pre_native_code_for_phis(stmt->v.while_loop.entry, 2, convert_types);
		    emit_pre_native_insn(new_pre_native_insn_goto(PRE_NATIVE_INSN_GOTO, stmt, label_start));
		    emit_pre_native_insn(label_end);
		}
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

static void
generate_pre_native_code (int convert_types)
{
    first_pre_native_insn = 0;
    last_pre_native_insn = 0;

    generate_pre_native_code_recursively(first_stmt, convert_types);
}

/*** pre-native code typechecking ***/

static void
typecheck_pre_native_code (void)
{
    pre_native_insn_t *insn = first_pre_native_insn;

    while (insn != 0)
    {
	switch (insn->kind)
	{
	    case PRE_NATIVE_INSN_ASSIGN :
		{
		    rhs_t *rhs = insn->stmt->v.assign.rhs;
		    type_t lhs_type = insn->stmt->v.assign.lhs->compvar->type;

		    if (rhs->kind == RHS_OP)
		    {
			int num_args = rhs->v.op.op->num_args;
			type_t result_type;
			type_t arg_types[MAX_OP_ARGS];
			int i;

			if (rhs->v.op.op->type_prop == TYPE_PROP_CONST)
			{
			    result_type = rhs->v.op.op->const_type;
			    memcpy(arg_types, rhs->v.op.op->arg_types, sizeof(type_t) * num_args);
			}
			else
			{
			    result_type = lhs_type;
			    for (i = 0; i < num_args; ++i)
				arg_types[i] = result_type;
			}

			assert(lhs_type == result_type);
			for (i = 0; i < num_args; ++i)
			    assert(primary_type(&rhs->v.op.args[i]) == arg_types[i]);
		    }
		    else
		    {
			assert(lhs_type == rhs_type(rhs));

			if (rhs->kind == RHS_FILTER)
			{
			    int num_args;
			    type_t *arg_types = get_filter_arg_types(rhs->v.filter.filter, &num_args);
			    int i;

			    for (i = 0; i < num_args; ++i)
				g_assert(primary_type(&rhs->v.filter.args[i]) == arg_types[i]);
			}
			else if (rhs->kind == RHS_CLOSURE)
			{
			    int num_args;
			    type_t *arg_types = get_filter_arg_types(rhs->v.closure.filter, &num_args);
			    int i;

			    for (i = 0; i < num_args - 3; ++i)
				g_assert(primary_type(&rhs->v.closure.args[i]) == arg_types[i]);
			}
			else if (rhs->kind == RHS_TUPLE)
			{
			    int i;

			    for (i = 0; i < rhs->v.tuple.length; ++i)
			    {
				int type = primary_type(&rhs->v.tuple.args[i]);
				g_assert(type == TYPE_FLOAT || type == TYPE_INT);
			    }
			}
		    }
		}
		break;

	    case PRE_NATIVE_INSN_PHI_ASSIGN :
		{
		    rhs_t *rhs = (insn->v.phi_rhs == 1) ? insn->stmt->v.assign.rhs : insn->stmt->v.assign.rhs2;

		    assert(insn->stmt->v.assign.lhs->compvar->type == rhs_type(rhs));
		}
		break;

	    default :
		break;
	}

	insn = insn->next;
    }
}

/*** ssa well-formedness check ***/

static void
_check_value (value_t *value, void *info)
{
    assert(value_set_contains(CLOSURE_GET(0,value_set_t*), value));
}

static void
check_rhs_defined (rhs_t *rhs, value_set_t *defined_set)
{
    void *closure[1] = { defined_set };

    for_each_value_in_rhs(rhs, &_check_value, closure);
}

static value_t*
last_assignment_to_compvar (statement_t *stmts, compvar_t *compvar)
{
    value_t *last = 0;

    while (stmts != 0)
    {
	switch (stmts->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
	    case STMT_PHI_ASSIGN :
		if (stmts->v.assign.lhs->compvar == compvar)
		    last = stmts->v.assign.lhs;
		break;

	    case STMT_IF_COND :
	    {
		value_t *new_last = last_assignment_to_compvar(stmts->v.if_cond.exit, compvar);

		if (new_last != 0)
		    last = new_last;

		break;
	    }

	    case STMT_WHILE_LOOP :
	    {
		value_t *new_last = last_assignment_to_compvar(stmts->v.while_loop.entry, compvar);

		if (new_last != 0)
		    last = new_last;

		break;
	    }

	    default :
		g_assert_not_reached();
	}

	stmts = stmts->next;
    }

    return last;
}

static void
set_value_defined_and_current_for_checking (value_t *value, GHashTable *current_value_hash, value_set_t *defined_set)
{
    value_set_add(defined_set, value);
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
	    FOR_EACH_VALUE_IN_RHS(stmts->v.assign.rhs, &_check_phi_value);
	    FOR_EACH_VALUE_IN_RHS(stmts->v.assign.rhs2, &_check_phi_value);

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
    defined_set_copy = value_set_copy(defined_set);

    check_ssa_recursively(stmts, parent, current_value_hash_copy, defined_set_copy);

    free_value_set(defined_set_copy);
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
		assert(!value_set_contains(defined_set, stmts->v.assign.lhs));
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
    defined_set = new_value_set();

    check_ssa_recursively(stmts, 0, current_value_hash, defined_set);

    free_value_set(defined_set);
    g_hash_table_destroy(current_value_hash);
}

/*** code slicing ***/

/* returns whether the slice is non-empty */
static int
slice_code (statement_t *stmt, unsigned int slice_flag, int (*predicate) (statement_t *stmt, void *info), void *info)
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

		result = slice_code(stmt->v.if_cond.consequent, slice_flag, predicate, info);
		result = slice_code(stmt->v.if_cond.alternative, slice_flag, predicate, info) || result;
		result = slice_code(stmt->v.if_cond.exit, slice_flag, predicate, info) || result;

		if (result)
		{
		    slice_code(stmt->v.if_cond.exit, slice_flag, predicate, info);

		    stmt->slice_flags |= slice_flag;
		    non_empty = 1;
		}
		break;
	    }

	    case STMT_WHILE_LOOP :
	    {
		if (slice_code(stmt->v.while_loop.body, slice_flag, predicate, info))
		{
		    slice_code(stmt->v.while_loop.entry, slice_flag, predicate, info);

		    stmt->slice_flags |= slice_flag;
		    non_empty = 1;
		}
		else
		    assert(!slice_code(stmt->v.while_loop.entry, slice_flag, predicate, info));
		break;
	    }

	    default:
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }

    return non_empty;
}

#define SLICE_CODE(stmt,flag,func,...) do { void *__clos[] = { __VA_ARGS__ }; slice_code((stmt),(flag),(func),__clos); } while (0)

/*** c code output ***/

/* permanent const values must be calculated once and then available for the
 * calculation of all less const values */
static int
is_permanent_const_value (value_t *value)
{
    return (value->least_const_type_multiply_used_in | CONST_T) == (value->const_type | CONST_T)
	&& (value->least_const_type_directly_used_in | CONST_T) != (value->const_type | CONST_T);
}

/* temporary const values must be defined for the calculation of all const
 * types up to the least const type they are used in */
static int
is_temporary_const_value (value_t *value)
{
    return !is_permanent_const_value(value);
}

/* returns whether const_type is at least as const as lower_bound but not more
 * const than upper_bound */
static int
is_const_type_within (int const_type, int lower_bound, int upper_bound)
{
    assert((lower_bound & upper_bound) == lower_bound);

    return (const_type & lower_bound) == lower_bound
	&& (const_type & upper_bound) == const_type;
}

void
output_value_name (FILE *out, value_t *value, int for_decl)
{
    if (value->index < 0)
    {
	if (value->compvar->type == TYPE_IMAGE)
	    fputs("UNINITED_IMAGE /* uninitialized */ ", out);
	else
	    fputs("0 /* uninitialized */ ", out);
    }
    else
    {
#ifndef NO_CONSTANTS_ANALYSIS
	if (!for_decl && is_permanent_const_value(value))
	{
	    if ((value->const_type | CONST_T) == (CONST_X | CONST_Y | CONST_T))
		fprintf(out, "xy_vars->");
	    else if ((value->const_type | CONST_T) == (CONST_Y | CONST_T))
		fprintf(out, "y_vars->");
	}
#endif

	if (value->compvar->var != 0)
	    fprintf(out, "var_%d_%d_%d", value->compvar->index, value->compvar->n, value->index);
	else
	    fprintf(out, "tmp_%d_%d", value->compvar->temp->number, value->index);
    }
}

void
output_value_decl (FILE *out, value_t *value)
{
    if (!value->have_defined && value->index >= 0)
    {
	fprintf(out, "%s ", type_c_type_name(value->compvar->type));
	output_value_name(out, value, 1);
	fputs(";\n", out);
	value->have_defined = 1;
    }
}

static void
_reset_value_have_defined (value_t *value, statement_t *stmt, void *info)
{
    value->have_defined = 0;
}

static void
reset_have_defined (statement_t *stmt)
{
    FOR_EACH_VALUE_IN_STATEMENTS(stmt, &_reset_value_have_defined);
}

void
output_primary (FILE *out, primary_t *primary)
{
    switch (primary->kind)
    {
	case PRIMARY_VALUE :
	    output_value_name(out, primary->v.value, 0);
	    break;

	case PRIMARY_CONST :
	    switch (primary->const_type)
	    {
		TYPE_C_PRINTER

		default :
		    g_assert_not_reached();
	    }
	    break;

	default :
	    g_assert_not_reached();
    }
}

static char*
userval_element_name (userval_info_t *info)
{
    switch (info->type)
    {
	case USERVAL_INT_CONST :
	    return "int_const";
	case USERVAL_FLOAT_CONST :
	    return "float_const";
	case USERVAL_BOOL_CONST :
	    return "bool_const";
	case USERVAL_COLOR :
	    return "color.value";
	case USERVAL_CURVE :
	    return "curve";
	case USERVAL_GRADIENT :
	    return "gradient";
	case USERVAL_IMAGE :
	    return "image";
	default :
	    g_assert_not_reached();
    }
}

void
output_rhs (FILE *out, rhs_t *rhs)
{
    switch (rhs->kind)
    {
	case RHS_PRIMARY :
	    output_primary(out, &rhs->v.primary);
	    break;

	case RHS_INTERNAL :
	    fputs(rhs->v.internal->name, out);
	    /* fprintf(out, "invocation->internals[%d].data[0]", rhs->v.internal->index); */
	    break;

	case RHS_OP :
	    {
		int i;

		fprintf(out, "%s(", rhs->v.op.op->name);
		for (i = 0; i < rhs->v.op.op->num_args; ++i)
		{
		    if (i > 0)
			fputs(",", out);
		    output_primary(out, &rhs->v.op.args[i]);
		}
		fputs(")", out);
	    }
	    break;

	case RHS_FILTER :
	    {
		int i;
		int num_args = num_filter_args(rhs->v.filter.filter);
		userval_info_t *info;

		fprintf(out, "({ userval_t args[%d]; ", num_args - 3);

		for (i = 0, info = rhs->v.filter.filter->userval_infos;
		     info != 0;
		     ++i, info = info->next)
		{
		    fprintf(out, "args[%d].v.%s = ", i, userval_element_name(info));
		    output_primary(out, &rhs->v.filter.args[i]);
		    fprintf(out, "; ");
		}
		g_assert(i == num_args - 3);

		fprintf(out, "filter_%s(invocation, args, ", rhs->v.filter.filter->name);
		output_primary(out, &rhs->v.filter.args[num_args - 3]);
		fprintf(out, ", ");
		output_primary(out, &rhs->v.filter.args[num_args - 2]);
		fprintf(out, ", ");
		output_primary(out, &rhs->v.filter.args[num_args - 1]);
		fprintf(out, ", pools); })");
	    }
	    break;

	case RHS_CLOSURE :
	    {
		int i;
		int num_args = num_filter_args(rhs->v.closure.filter) - 3;
		userval_info_t *info;

		if (rhs->v.closure.filter->kind == FILTER_MATHMAP)
		{
		    fprintf(out, "({ image_t *image = ALLOC_CLOSURE_IMAGE(%d); image->type = IMAGE_CLOSURE; image->v.closure.func = filter_%s; ", num_args, rhs->v.closure.filter->name);

		    for (i = 0, info = rhs->v.closure.filter->userval_infos;
			 info != 0;
			 ++i, info = info->next)
		    {
			fprintf(out, "CLOSURE_IMAGE_ARGS(image)[%d].v.%s = ", i, userval_element_name(info));
			output_primary(out, &rhs->v.closure.args[i]);
			fprintf(out, "; ");
		    }
		    g_assert(i == num_args);

		    fprintf(out, "image; })");
		}
		else if (rhs->v.closure.filter->kind == FILTER_NATIVE)
		{
		    fprintf(out, "({ userval_t args[%d]; ", num_args);

		    for (i = 0, info = rhs->v.closure.filter->userval_infos;
			 info != 0;
			 ++i, info = info->next)
		    {
			fprintf(out, "args[%d].v.%s = ", i, userval_element_name(info));
			output_primary(out, &rhs->v.closure.args[i]);
			fprintf(out, "; ");
		    }
		    g_assert(i == num_args);

		    fprintf(out, "%s(invocation, args, pools); })", rhs->v.closure.filter->v.native.func_name);
		}
		else
		    g_assert_not_reached();
	    }
	    break;

	case RHS_TUPLE :
	    {
		int i;

		fprintf(out, "({ float *tuple = ALLOC_TUPLE(%d); ", rhs->v.tuple.length);

		for (i = 0; i < rhs->v.tuple.length; ++i)
		{
		    fprintf(out, "TUPLE_SET(tuple, %d, ", i);
		    output_primary(out, &rhs->v.tuple.args[i]);
		    fprintf(out, "); ");
		}

		fprintf(out, "tuple; })");
	    }
	    break;

	default :
	    g_assert_not_reached();
    }
}

void
output_phis (FILE *out, statement_t *phis, int branch, unsigned int slice_flag)
{
    while (phis != 0)
    {
	rhs_t *rhs;

#ifndef NO_CONSTANTS_ANALYSIS
	if ((phis->slice_flags & slice_flag) == 0)
#else
        if (phis->kind == STMT_NIL)
#endif
	{
	    phis = phis->next;
	    continue;
	}

	assert(phis->kind == STMT_PHI_ASSIGN);

	rhs = ((branch == 0) ? phis->v.assign.rhs : phis->v.assign.rhs2);

	if (rhs->kind != RHS_PRIMARY
	    || rhs->v.primary.kind != PRIMARY_VALUE
	    || rhs->v.primary.v.value != phis->v.assign.lhs)
	{
	    output_value_name(out, phis->v.assign.lhs, 0);
	    fputs(" = ", out);
	    output_rhs(out, rhs);
	    fputs(";\n", out);
	}

	phis = phis->next;
    }
}

void
output_stmts (FILE *out, statement_t *stmt, unsigned int slice_flag)
{
    while (stmt != 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	if (stmt->slice_flags & slice_flag)
#endif
	    switch (stmt->kind)
	    {
		case STMT_NIL :
#ifndef NO_CONSTANTS_ANALYSIS
		    g_assert_not_reached();
#endif
		    break;

		case STMT_ASSIGN :
		    output_value_name(out, stmt->v.assign.lhs, 0);
		    fputs(" = ", out);
		    output_rhs(out, stmt->v.assign.rhs);
		    fputs(";\n", out);
		    break;

		case STMT_PHI_ASSIGN :
		    g_assert_not_reached();
		    break;

		case STMT_IF_COND :
		    fputs("if (", out);
		    output_rhs(out, stmt->v.if_cond.condition);
		    fputs(")\n{\n", out);
		    output_stmts(out, stmt->v.if_cond.consequent, slice_flag);
		    output_phis(out, stmt->v.if_cond.exit, 0, slice_flag);
		    fputs("}\nelse\n{\n", out);
		    output_stmts(out, stmt->v.if_cond.alternative, slice_flag);
		    output_phis(out, stmt->v.if_cond.exit, 1, slice_flag);
		    fputs("}\n", out);
		    break;

		case STMT_WHILE_LOOP :
		    output_phis(out, stmt->v.while_loop.entry, 0, slice_flag);
		    fputs("while (", out);
		    output_rhs(out, stmt->v.while_loop.invariant);
		    fputs(")\n{\n", out);
		    output_stmts(out, stmt->v.while_loop.body, slice_flag);
		    output_phis(out, stmt->v.while_loop.entry, 1, slice_flag);
		    fputs("}\n", out);
		    break;

		default :
		    g_assert_not_reached();
	    }

	stmt = stmt->next;
    }
}

static void
_output_value_if_needed_decl (value_t *value, statement_t *stmt, void *info)
{
    CLOSURE_VAR(FILE*, out, 0);
    CLOSURE_VAR(int, const_type, 1);

    if ((value->const_type | CONST_T) == (const_type | CONST_T)
	&& is_permanent_const_value(value))
	output_value_decl(out, value);
}

static void
output_permanent_const_declarations (filter_code_t *code, FILE *out, int const_type)
{
    reset_have_defined(code->first_stmt);

    FOR_EACH_VALUE_IN_STATEMENTS(code->first_stmt, &_output_value_if_needed_decl, out, (void*)const_type);
}

static int
_is_value_needed (value_t *value, int const_type)
{
    return (value->const_type | CONST_T) == (const_type | CONST_T)
	|| (is_const_type_within(const_type | CONST_T,
				 value->least_const_type_multiply_used_in,
				 value->const_type | CONST_T));
}

static void
_output_value_if_needed_code (value_t *value, statement_t *stmt, void *info)
{
    CLOSURE_VAR(FILE*, out, 0);
    CLOSURE_VAR(int, const_type, 1);

    if ((is_temporary_const_value(value) || const_type == 0)
	&& _is_value_needed(value, const_type))
	output_value_decl(out, value);
}

static int
_const_predicate (statement_t *stmt, void *info)
{
    CLOSURE_VAR(int, const_type, 0);

    assert(stmt->kind == STMT_ASSIGN || stmt->kind == STMT_PHI_ASSIGN);

    return _is_value_needed(stmt->v.assign.lhs, const_type);
}

static void
output_permanent_const_code (filter_code_t *code, FILE *out, int const_type)
{
    unsigned int slice_flag;

    /* declarations */
    reset_have_defined(code->first_stmt);
    FOR_EACH_VALUE_IN_STATEMENTS(code->first_stmt, &_output_value_if_needed_code, out, (void*)const_type);

    /* code */
    if (const_type == (CONST_X | CONST_Y))
	slice_flag = SLICE_XY_CONST;
    else if (const_type == CONST_Y)
	slice_flag = SLICE_Y_CONST;
    else if (const_type == CONST_X)
	slice_flag = SLICE_X_CONST;
    else
	slice_flag = SLICE_NO_CONST;

    SLICE_CODE(code->first_stmt, slice_flag, &_const_predicate, (void*)const_type);

    output_stmts(out, code->first_stmt, slice_flag);
}

/*** generating interpreter code ***/

static int
add_interpreter_value (GArray *array, runtime_value_t value)
{
    g_array_append_val(array, value);
    return array->len - 1;
}

static int
lookup_value_index (GHashTable *value_hash, value_t *value)
{
    gpointer dummy_key, hash_pointer;

    if (g_hash_table_lookup_extended(value_hash, value, &dummy_key, &hash_pointer))
	return GPOINTER_TO_INT(hash_pointer);
    else
	return -1;
}

static int
lookup_goto_target (GHashTable *label_hash, pre_native_insn_t *target_insn)
{
    gpointer dummy_key, hash_pointer;

    assert(g_hash_table_lookup_extended(label_hash, target_insn, &dummy_key, &hash_pointer));
    return GPOINTER_TO_INT(hash_pointer);
}

static int
lookup_value_arg (value_t *value, mathmap_t *mathmap, GHashTable *value_hash)
{
    int index = lookup_value_index(value_hash, value);

    if (index < 0)
	// FIXME: uninitialized - would be nice if it was of the right type
	return add_interpreter_value(mathmap->interpreter_values, make_int_runtime_value(0));
    else
	return index;
}

static int
lookup_primary_arg (primary_t *primary, mathmap_t *mathmap, GHashTable *value_hash)
{
    switch (primary->kind)
    {
	case PRIMARY_VALUE :
	    return lookup_value_arg(primary->v.value, mathmap, value_hash);

	case PRIMARY_CONST :
	    return add_interpreter_value(mathmap->interpreter_values, primary->v.constant);

	default :
	    g_assert_not_reached();
    }
}

static int
lookup_rhs_arg (rhs_t *rhs, mathmap_t *mathmap, GHashTable *value_hash)
{
    switch (rhs->kind)
    {
	case RHS_PRIMARY :
	    return lookup_primary_arg(&rhs->v.primary, mathmap, value_hash);

	case RHS_INTERNAL :
	    return rhs->v.internal->index;

	default :
	    g_assert_not_reached();
    }
}

static interpreter_insn_t
make_interpreter_insn (builtin_func_t func, int num_args, int *arg_indexes)
{
    interpreter_insn_t insn;

    insn.func = func;
    memcpy(insn.arg_indexes, arg_indexes, sizeof(int) * num_args);

    return insn;
}

static void
builtin_copy (mathmap_invocation_t *invocation, int *arg_indexes)
{
    g_array_index(invocation->mathmap->interpreter_values, runtime_value_t, arg_indexes[0])
	= g_array_index(invocation->mathmap->interpreter_values, runtime_value_t, arg_indexes[1]);
}

static void
builtin_goto (mathmap_invocation_t *invocation, int *arg_indexes)
{
    invocation->interpreter_ip = BUILTIN_INT_ARG(0);
}

static void
builtin_if_cond_false_goto (mathmap_invocation_t *invocation, int *arg_indexes)
{
    if (!BUILTIN_INT_ARG(0))
	invocation->interpreter_ip = BUILTIN_INT_ARG(1);
}

static interpreter_insn_t
make_op_rhs_interpreter_insn (int lhs_arg_index, rhs_t *rhs, mathmap_t *mathmap, GHashTable *value_hash)
{
    int arg_indexes[MAX_OP_ARGS + 1];
    int i;
    builtin_func_t func;

    assert(rhs->kind == RHS_OP);

    arg_indexes[0] = lhs_arg_index;
    for (i = 0; i < rhs->v.op.op->num_args; ++i)
	arg_indexes[i + 1] = lookup_primary_arg(&rhs->v.op.args[i], mathmap, value_hash);
    func = get_builtin(rhs);

    return make_interpreter_insn(func, rhs->v.op.op->num_args + 1, arg_indexes);
}

static void
generate_interpreter_code_from_ir (mathmap_t *mathmap)
{
    GHashTable *value_hash = g_hash_table_new(g_direct_hash, g_direct_equal);
    GHashTable *label_hash = g_hash_table_new(g_direct_hash, g_direct_equal);
    /* the first few values are the internals */
    int num_values = number_of_internals(mathmap->main_filter->v.mathmap.internals);
    int num_insns = 0;
    pre_native_insn_t *insn;
    int index;

    assert(value_hash != 0);
    assert(label_hash != 0);

    for (insn = first_pre_native_insn; insn != 0; insn = insn->next)
    {
	switch (insn->kind)
	{
	    case PRE_NATIVE_INSN_LABEL :
		g_hash_table_insert(label_hash, insn, GINT_TO_POINTER(num_insns));
		break;

	    case PRE_NATIVE_INSN_ASSIGN :
	    case PRE_NATIVE_INSN_PHI_ASSIGN :
		{
		    value_t *value = insn->stmt->v.assign.lhs;
		    gpointer dummy_key, dummy_value;

		    if (!g_hash_table_lookup_extended(value_hash, value, &dummy_key, &dummy_value))
		    {
			g_hash_table_insert(value_hash, value, GINT_TO_POINTER(num_values));
			++num_values;
		    }
		}
		break;

	    case PRE_NATIVE_INSN_IF_COND_FALSE_GOTO :
		if (pre_native_condition(insn)->kind == RHS_OP)
		    ++num_insns;
		break;

	    default :
		break;
	}

	if (insn->kind != PRE_NATIVE_INSN_LABEL)
	    ++num_insns;
    }

    mathmap->interpreter_insns = (interpreter_insn_t*)malloc(sizeof(interpreter_insn_t) * num_insns);
    assert(mathmap->interpreter_insns != 0);

    mathmap->interpreter_values = g_array_new(FALSE, TRUE, sizeof(runtime_value_t));
    assert(mathmap->interpreter_values != 0);

    /* make room for the internals and variable values.  constants are
       appended after them.  */
    g_array_set_size(mathmap->interpreter_values, num_values);

    index = 0;
    for (insn = first_pre_native_insn; insn != 0; insn = insn->next)
    {
	switch (insn->kind)
	{
	    case PRE_NATIVE_INSN_LABEL :
		break;

	    case PRE_NATIVE_INSN_GOTO :
		{
		    int target = lookup_goto_target(label_hash, insn->v.target);
		    int arg_index;

		    assert(target >= 0 && target < num_insns);
		    arg_index = add_interpreter_value(mathmap->interpreter_values, make_int_runtime_value(target));

		    mathmap->interpreter_insns[index] = make_interpreter_insn(builtin_goto, 1, &arg_index);
		}
		break;

	    case PRE_NATIVE_INSN_IF_COND_FALSE_GOTO :
		{
		    int target;
		    int arg_indexes[2];
		    rhs_t *rhs = pre_native_condition(insn);

		    if (rhs->kind == RHS_OP)
		    {
			arg_indexes[0] = add_interpreter_value(mathmap->interpreter_values, make_int_runtime_value(0));
			mathmap->interpreter_insns[index++] = make_op_rhs_interpreter_insn(arg_indexes[0], rhs, mathmap, value_hash);
		    }
		    else
			arg_indexes[0] = lookup_rhs_arg(rhs, mathmap, value_hash);

		    target = lookup_goto_target(label_hash, insn->v.target);
		    assert(target >= 0 && target < num_insns);
		    arg_indexes[1] = add_interpreter_value(mathmap->interpreter_values, make_int_runtime_value(target));

		    mathmap->interpreter_insns[index] = make_interpreter_insn(builtin_if_cond_false_goto, 2, arg_indexes);
		}
		break;

	    case PRE_NATIVE_INSN_ASSIGN :
	    case PRE_NATIVE_INSN_PHI_ASSIGN :
		{
		    rhs_t *rhs;
		    int arg_indexes[2];

		    if (insn->kind == PRE_NATIVE_INSN_ASSIGN || insn->v.phi_rhs == 1)
			rhs = insn->stmt->v.assign.rhs;
		    else
			rhs = insn->stmt->v.assign.rhs2;

		    arg_indexes[0] = lookup_value_arg(insn->stmt->v.assign.lhs, mathmap, value_hash);

		    switch (rhs->kind)
		    {
			case RHS_OP :
			    mathmap->interpreter_insns[index]
				= make_op_rhs_interpreter_insn(arg_indexes[0], rhs, mathmap, value_hash);
			    break;

			default :
			    arg_indexes[1] = lookup_rhs_arg(rhs, mathmap, value_hash);
			    mathmap->interpreter_insns[index] = make_interpreter_insn(builtin_copy, 2, arg_indexes);
			    break;
		    }
		}
		break;

	    default :
		break;
	}

	if (insn->kind != PRE_NATIVE_INSN_LABEL)
	    ++index;
    }

    assert(index == num_insns);

    g_hash_table_destroy(value_hash);
    g_hash_table_destroy(label_hash);
}

/*** compiling and loading ***/

#ifdef OPENSTEP
#ifndef MAX
#define	MAX(a,b)	(((a)<(b))?(b):(a))
#endif
#define	CGEN_CC		"cc -c -fPIC -faltivec -o"
#define	CGEN_LD		"cc -bundle -flat_namespace -undefined suppress -o"
#endif

#ifdef PEDANTIC_CHECK_SSA
#define CHECK_SSA	check_ssa(first_stmt)
#else
#define CHECK_SSA	do ; while (0)
#endif

static filter_code_t*
generate_ir_code (filter_t *filter, int constant_analysis, int convert_types)
{
    int changed;
    filter_code_t *code;
    compvar_t *tuple_tmp, *dummy;

    g_assert(filter->kind == FILTER_MATHMAP);

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

    do
    {
#ifdef DEBUG_OUTPUT
	check_ssa(first_stmt);
#endif

#ifdef DEBUG_OUTPUT
	printf("--------------------------------\n");
	dump_code(first_stmt, 0);
#endif

	optimize_closure_application(first_stmt);
	CHECK_SSA;

	changed = 0;

	changed = do_inlining() || changed;
	CHECK_SSA;
	changed = copy_propagation() || changed;
	CHECK_SSA;
	changed = optimize_tuple_nth() || changed;
	CHECK_SSA;
	changed = optimize_make_tuple() || changed;
	CHECK_SSA;
	changed = common_subexpression_elimination() || changed;
	CHECK_SSA;
	changed = copy_propagation() || changed;
	CHECK_SSA;
	changed = constant_folding() || changed;
	CHECK_SSA;
	changed = simplify_ops() || changed;
	CHECK_SSA;
	changed = remove_dead_assignments() || changed;
	CHECK_SSA;
	changed = remove_dead_branches() || changed;
	CHECK_SSA;
	changed = remove_dead_controls() || changed;
    } while (changed);

    CHECK_SSA;
    propagate_types();

#ifdef DEBUG_OUTPUT
    check_ssa(first_stmt);
#endif

#ifndef NO_CONSTANTS_ANALYSIS
    if (constant_analysis)
	analyze_constants();
#endif

#ifdef DEBUG_OUTPUT
    printf("----------- final ---------------------\n");
    dump_code(first_stmt, 0);
#endif
    check_ssa(first_stmt);

    /* no statement reordering after this point */

    generate_pre_native_code(convert_types);
#ifdef DEBUG_OUTPUT
    printf("----------- pre-native ---------------------\n");
    dump_pre_native_code();
#endif
    if (convert_types)
	typecheck_pre_native_code();

    code = (filter_code_t*)pools_alloc(&compiler_pools, sizeof(filter_code_t));

    code->filter = filter;
    code->first_stmt = first_stmt;
    code->first_pre_native_insn = first_pre_native_insn;

    first_stmt = 0;
    first_pre_native_insn = 0;

    return code;
}

static void
forget_ir_code (mathmap_t *mathmap)
{
    free_pools(&compiler_pools);
}

static char *include_path = 0;

static void
set_include_path (const char *path)
{
    if (include_path != 0)
	free(include_path);
    include_path = strdup(path);
    assert(include_path != 0);
}

static int
filter_template_processor (mathmap_t *mathmap, const char *directive, const char *arg, FILE *out, void *data)
{
    filter_code_t *code = (filter_code_t*)data;

    if (strcmp(directive, "g") == 0)
    {
#ifdef OPENSTEP
	putc('0', out);
#else
	putc('1', out);
#endif
    }
    else if (strcmp(directive, "name") == 0)
	fputs(code->filter->name, out);
    else if (strcmp(directive, "m") == 0)
	output_permanent_const_code(code, out, 0);
    else if (strcmp(directive, "uses_ra") == 0)
	fputs("1", out);
    else
	return 0;
    return 1;
}

int
compiler_template_processor (mathmap_t *mathmap, const char *directive, const char *arg, FILE *out, void *data)
{
    assert(mathmap->main_filter->v.mathmap.decl != NULL
	   && mathmap->main_filter->v.mathmap.decl->type == TOP_LEVEL_FILTER);

    if (strcmp(directive, "g") == 0)
    {
#ifdef OPENSTEP
	putc('0', out);
#else
	putc('1', out);
#endif
    }
    else if (strcmp(directive, "m") == 0)
	output_permanent_const_code(main_filter_code, out, 0);
    else if (strcmp(directive, "p") == 0)
	fprintf(out, "%d", USER_CURVE_POINTS);
    else if (strcmp(directive, "q") == 0)
	fprintf(out, "%d", USER_GRADIENT_POINTS);
    else if (strcmp(directive, "xy_decls") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_declarations(main_filter_code, out, CONST_X | CONST_Y);
#endif
    }
    else if (strcmp(directive, "x_decls") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_declarations(main_filter_code, out, CONST_X);
#endif
    }
    else if (strcmp(directive, "y_decls") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_declarations(main_filter_code, out, CONST_Y);
#endif
    }
    else if (strcmp(directive, "xy_code") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_code(main_filter_code, out, CONST_X | CONST_Y);
#endif
    }
    else if (strcmp(directive, "x_code") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_code(main_filter_code, out, CONST_X);
#endif
    }
    else if (strcmp(directive, "y_code") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_code(main_filter_code, out, CONST_Y);
#endif
    }
    else if (strcmp(directive, "include") == 0)
    {
	fputs(include_path, out);
    }
    else if (strcmp(directive, "max_debug_tuples") == 0)
    {
	fprintf(out, "%d", MAX_DEBUG_TUPLES);
    }
    else if (strcmp(directive, "uses_ra") == 0)
    {
	fprintf(out, "%d", does_filter_use_ra(mathmap->main_filter) ? 1 : 0);
    }
    else if (strcmp(directive, "uses_t") == 0)
    {
	fprintf(out, "%d", does_filter_use_t(mathmap->main_filter) ? 1 : 0);
    }
    else if (strcmp(directive, "filter_name") == 0)
    {
	fprintf(out, "%s", mathmap->main_filter->name);
    }
    else if (strcmp(directive, "filter_docstring") == 0)
    {
	if (mathmap->main_filter->v.mathmap.decl->docstring != 0)
	    fprintf(out, "%s", mathmap->main_filter->v.mathmap.decl->docstring);
    }
    else if (strcmp(directive, "num_uservals") == 0)
    {
	fprintf(out, "%d", mathmap->main_filter->num_uservals);
    }
    else if (strcmp(directive, "native_filter_decls") == 0)
    {
	filter_t *filter;

	for (filter = mathmap->filters; filter != NULL; filter = filter->next)
	{
	    if (filter->kind != FILTER_NATIVE)
		continue;

	    fprintf(out, "DECLARE_NATIVE_FILTER(%s);\n", filter->v.native.func_name);
	}
    }
    else if (strcmp(directive, "filter_begin") == 0)
    {
	int i;
	filter_t *filter;

	g_assert(arg != 0);

	for (i = 0, filter = mathmap->filters;
	     filter != 0;
	     ++i, filter = filter->next)
	{
	    filter_code_t *code = filter_codes[i];

	    if (filter->kind != FILTER_MATHMAP)
		continue;

	    g_assert(code->filter == filter);

	    process_template(mathmap, arg, out, filter_template_processor, code);
	}
    }
    else
	return 0;
    return 1;
}

static int
exec_cmd (char *log_filename, char *format, ...)
{
    va_list ap;
    char *cmdline, *full_cmdline;
    int result;
    FILE *log;

    va_start(ap, format);
    cmdline = g_strdup_vprintf(format, ap);
    va_end(ap);

    log = fopen(log_filename, "a");
    if (log != 0)
    {
	fprintf(log, "\n%s\n", cmdline);
	fclose(log);
    }

    full_cmdline = g_strdup_printf("%s >>%s 2>&1", cmdline, log_filename);
 
    result = system(full_cmdline);

    g_free(full_cmdline);
    g_free(cmdline);

    return result;
}

#define TMP_PREFIX		"/tmp/mathfunc"

initfunc_t
gen_and_load_c_code (mathmap_t *mathmap, void **module_info, char *template_filename, char *include_path)
{
    static int last_mathfunc = 0;

    FILE *out;
    char *c_filename, *o_filename, *so_filename, *log_filename;
    int pid = getpid();
    initfunc_t initfunc;
    int num_filters;
    int i;
    filter_t *filter;
#ifndef OPENSTEP
    void *initfunc_ptr;
    GModule *module = 0;
#endif

    init_pools(&compiler_pools);

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
	filter_codes[i] = generate_ir_code(filter, 0, 0);
    }

#ifdef DEBUG_OUTPUT
    g_print("compiling main filter\n");
#endif
    main_filter_code = generate_ir_code(mathmap->main_filter, 1, 0);

    c_filename = g_strdup_printf("%s%d_%d.c", TMP_PREFIX, pid, ++last_mathfunc);
    out = fopen(c_filename, "w");
    if (out == 0)
    {
	sprintf(error_string, "Could not write temporary file `%s'", c_filename);
	return 0;
    }

    set_include_path(include_path);
    if (!process_template_file(mathmap, template_filename, out, &compiler_template_processor, 0))
    {
	sprintf(error_string, "Could not process template file `%s'", template_filename);
	return 0;
    }

    main_filter_code = 0;
    filter_codes = 0;

    fclose(out);

    o_filename = g_strdup_printf("%s%d_%d.o", TMP_PREFIX, pid, last_mathfunc);
    log_filename = g_strdup_printf("%s%d_%d.log", TMP_PREFIX, pid, last_mathfunc);

    if (exec_cmd(log_filename, "%s %s %s", CGEN_CC, o_filename, c_filename) != 0)
    {
	sprintf(error_string, "C compiler failed.  See logfile `%s'.", log_filename);
	return 0;
    }

    so_filename = g_strdup_printf("%s%d_%d.so", TMP_PREFIX, pid, last_mathfunc);

    if (exec_cmd(log_filename, "%s %s %s", CGEN_LD, so_filename, o_filename) != 0)
    {
	sprintf(error_string, "Linker failed.  See logfile `%s'.", log_filename);
	return 0;
    }

#ifndef OPENSTEP
    module = g_module_open(so_filename, 0);
    if (module == 0)
    {
	sprintf(error_string, "Could not load module `%s': %s.", so_filename, g_module_error());
	return 0;
    }

#ifdef DEBUG_OUTPUT
    printf("loaded %p\n", module);
#endif

    assert(g_module_symbol(module, "mathmapinit", &initfunc_ptr));
    initfunc = (initfunc_t)initfunc_ptr;

    *module_info = module;
#else
    {
        NSObjectFileImage objectFileImage;
        NSModule module;
        const char *moduleName = "Johnny";
        NSSymbol symbol;
        
        NSCreateObjectFileImageFromFile(so_filename, &objectFileImage);
	if (objectFileImage == 0)
	{
	    fprintf(stderr, "NSCreateObjectFileImageFromFile() failed\n");
	    return 0;
	}

        module = NSLinkModule(objectFileImage, moduleName,
			      NSLINKMODULE_OPTION_PRIVATE | NSLINKMODULE_OPTION_BINDNOW);
	if (module == 0)
	{
	    fprintf(stderr, "NSLinkModule() failed\n");
	    return 0;
	}
        NSDestroyObjectFileImage(objectFileImage);

	symbol = NSLookupSymbolInModule(module, "__init");
	if (symbol != 0)
	{
	    void (*init) (void) = NSAddressOfSymbol(symbol);
	    init();
	}

        symbol = NSLookupSymbolInModule(module, "_mathmapinit");
	assert(symbol != 0);
        initfunc = NSAddressOfSymbol(symbol);

	*module_info = module;
    }
#endif

    unlink(so_filename);
    g_free(so_filename);

    unlink(o_filename);
    g_free(o_filename);

    //unlink(c_filename);
    g_free(c_filename);

    unlink(log_filename);
    g_free(log_filename);

    forget_ir_code(mathmap);

    return initfunc;
}

void
unload_c_code (void *module_info)
{
#ifndef OPENSTEP
    GModule *module = module_info;

#ifdef DEBUG_OUTPUT
    printf("unloading %p\n", module);
#endif

    assert(g_module_close(module));
#else
    /* FIXME */
#endif
}

void
generate_interpreter_code (mathmap_t *mathmap)
{
    generate_ir_code(mathmap->main_filter, 1, 1);

    generate_interpreter_code_from_ir(mathmap);

    forget_ir_code(mathmap);
}

/*** plug-in generator ***/
#ifndef OPENSTEP
int
generate_plug_in (char *filter, char *output_filename,
		  char *template_filename, int analyze_constants,
		  template_processor_func_t template_processor)
{
    char template_path[strlen(TEMPLATE_DIR) + 1 + strlen(template_filename) + 1];
    FILE *out;
    mathmap_t *mathmap;

    sprintf(template_path, "%s/%s", TEMPLATE_DIR, template_filename);

    mathmap = parse_mathmap(filter);

    if (mathmap == 0)
    {
	fprintf(stderr, "Error: %s\n", error_string);
	return 0;
    }

    generate_ir_code(mathmap->main_filter, analyze_constants, 0);

    out = fopen(output_filename, "w");

    if (out == 0)
    {
	fprintf(stderr, "Could not open output file `%s'\n", output_filename);
	exit(1);
    }

    set_include_path(TEMPLATE_DIR);
    if (!process_template_file(mathmap, template_path, out, template_processor, 0))
    {
	fprintf(stderr, "Could not process template file `%s'\n", template_path);
	exit(1);
    }

    fclose(out);

    forget_ir_code(mathmap);

    return 1;
}
#endif

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
