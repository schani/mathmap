/*
 * compiler.c
 *
 * MathMap
 *
 * Copyright (C) 2002-2007 Mark Probst
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

#include "gtypes.h"
#include "mathmap.h"
#include "vars.h"
#include "internals.h"
#include "tags.h"
#include "exprtree.h"
#include "overload.h"
#include "postfix.h"
#include "internals.h"
#include "jump.h"
#include "scanner.h"
#include "bitvector.h"
#include "lispreader/pools.h"

//#define NO_CONSTANTS_ANALYSIS

struct _value_t;

typedef struct
{
    int number;
    int last_index;
} temporary_t;

#define TYPE_NIL             0
#define TYPE_INT             1
#define TYPE_FLOAT           2
#define TYPE_COMPLEX         3
#define TYPE_COLOR           4
#define TYPE_MATRIX          5
#define TYPE_VECTOR          6

#define MAX_TYPE             TYPE_VECTOR

#define MAX_PROMOTABLE_TYPE  TYPE_COMPLEX

#define CONST_MAX            (CONST_Y | CONST_X | CONST_T)

typedef int type_t;

typedef struct _compvar_t
{
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
#define PRIMARY_INT_CONST      2
#define PRIMARY_FLOAT_CONST    3
#define PRIMARY_COMPLEX_CONST  4
#define PRIMARY_COLOR_CONST    5

typedef struct
{
    int type;
    union
    {
	value_t *value;
	int int_const;
	float float_const;
	complex float complex_const;
	color_t color_const;
    } v;
} primary_t;

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
    type_t const_type;		/* used only if type_prop == TYPE_PROP_CONST */
    int is_pure;
    int is_foldable;
} operation_t;

#define RHS_PRIMARY          1
#define RHS_INTERNAL         2
#define RHS_OP               3

#define MAX_OP_ARGS          9

typedef struct
{
    int type;
    union
    {
	primary_t primary;
	internal_t *internal;
	struct
	{
	    operation_t *op;
	    primary_t args[MAX_OP_ARGS];
	} op;
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
    int type;
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
    int type;
    int index;
    statement_t *stmt;
    union
    {
	struct _pre_native_insn_t *target;
	int phi_rhs;
    } v;
    struct _pre_native_insn_t *next;
} pre_native_insn_t;

typedef struct _native_register_t
{
    int native_index;
    struct _value_t *value_allocated_to;
    int used_anywhere;
} native_register_t;

typedef struct
{
    int num_registers;
    native_register_t *registers;
} type_info_t;

static void init_op (int index, char *name, int num_args, type_prop_t type_prop,
		     type_t const_type, int is_pure, int is_foldable);
static int rhs_is_foldable (rhs_t *rhs);
primary_t make_int_const_primary (int int_const);
primary_t make_float_const_primary (float float_const);
primary_t make_complex_const_primary (complex float complex_const);
primary_t make_color_const_primary (color_t color_const);

#include <complex.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_specfunc.h>
#include <gsl/gsl_sf_elljac.h>

#include "spec_func.h"
#include "builtins.h"
#include "noise.h"

#define INTERPRETER
#include "opmacros.h"
#undef INTERPRETER

#define RHS_ARG(i)                (rhs->v.op.args[(i)])
#define OP_CONST_FLOAT_VAL(i)     (RHS_ARG((i)).type == PRIMARY_INT_CONST ? (float)(RHS_ARG((i)).v.int_const) : \
                                   RHS_ARG((i)).type == PRIMARY_FLOAT_CONST ? RHS_ARG((i)).v.float_const : \
                                   ({ assert(0); 0.0; }))

#include "opdefs.h"

static pools_t compiler_pools;

static operation_t ops[NUM_OPS];

static int next_temp_number = 1;

static statement_t *first_stmt = 0;
static statement_t **emit_loc = &first_stmt;
static statement_t dummy_stmt = { STMT_NIL };

static pre_native_insn_t *first_pre_native_insn;
static pre_native_insn_t *last_pre_native_insn;

static type_info_t type_infos[MAX_TYPE + 1];

#define STMT_STACK_SIZE            64

static statement_t *stmt_stack[STMT_STACK_SIZE];
static int stmt_stackp = 0;

#define CURRENT_STACK_TOP       ((stmt_stackp > 0) ? stmt_stack[stmt_stackp - 1] : 0)
#define UNSAFE_EMIT_STMT(s,l) \
    ({ (s)->parent = CURRENT_STACK_TOP; \
       (s)->next = (l); (l) = (s); })

/*** hash tables ***/

static GHashTable*
direct_hash_table_copy (GHashTable *table)
{
    GHashTable *copy = g_hash_table_new(&g_direct_hash, &g_direct_equal);

    void copy_entry (gpointer key, gpointer value, gpointer user_data)
	{ g_hash_table_insert(copy, key, value); }

    g_hash_table_foreach(table, &copy_entry, 0);

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
make_temporary (void)
{
    temporary_t *temp = (temporary_t*)pools_alloc(&compiler_pools, sizeof(temporary_t));
    compvar_t *compvar = alloc_compvar();
    value_t *val = new_value(compvar);

    temp->number = next_temp_number++;
    temp->last_index = 0;

    compvar->var = 0;
    compvar->temp = temp;
    compvar->type = TYPE_INT;
    compvar->current = val;
    compvar->values = val;

    return compvar;
}

compvar_t*
make_variable (variable_t *var, int n)
{
    compvar_t *compvar = alloc_compvar();
    value_t *val = new_value(compvar);

    compvar->var = var;
    compvar->temp = 0;
    compvar->n = n;
    compvar->type = TYPE_INT;
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

    assert(0);
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
make_int_const_primary (int int_const)
{
    primary_t primary;

    primary.type = PRIMARY_INT_CONST;
    primary.v.int_const = int_const;

    return primary;
}

primary_t
make_float_const_primary (float float_const)
{
    primary_t primary;

    primary.type = PRIMARY_FLOAT_CONST;
    primary.v.float_const = float_const;

    return primary;
}

primary_t
make_complex_const_primary (complex float complex_const)
{
    primary_t primary;

    primary.type = PRIMARY_COMPLEX_CONST;
    primary.v.complex_const = complex_const;

    return primary;
}

primary_t
make_color_const_primary (color_t color_const)
{
    primary_t primary;

    primary.type = PRIMARY_COLOR_CONST;
    primary.v.color_const = color_const;

    return primary;
}

primary_t
make_compvar_primary (compvar_t *compvar)
{
    primary_t primary;

    primary.type = PRIMARY_VALUE;
    primary.v.value = current_value(compvar);

    return primary;
}

rhs_t*
make_int_const_rhs (int int_const)
{
    rhs_t *rhs = alloc_rhs();

    rhs->type = RHS_PRIMARY;
    rhs->v.primary.type = PRIMARY_INT_CONST;
    rhs->v.primary.v.int_const = int_const;

    return rhs;
}

rhs_t*
make_float_const_rhs (float float_const)
{
    rhs_t *rhs = alloc_rhs();

    rhs->type = RHS_PRIMARY;
    rhs->v.primary.type = PRIMARY_FLOAT_CONST;
    rhs->v.primary.v.float_const = float_const;

    return rhs;
}

rhs_t*
make_value_rhs (value_t *val)
{
    rhs_t *rhs = alloc_rhs();

    assert(val != 0);

    rhs->type = RHS_PRIMARY;
    rhs->v.primary.type = PRIMARY_VALUE;
    rhs->v.primary.v.value = val;

    return rhs;
}

rhs_t*
make_primary_rhs (primary_t primary)
{
    rhs_t *rhs = alloc_rhs();

    rhs->type = RHS_PRIMARY;
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

    rhs->type = RHS_INTERNAL;
    rhs->v.internal = internal;

    return rhs;
}

rhs_t*
make_op_rhs (int op_index, ...)
{
    rhs_t *rhs = alloc_rhs();
    va_list ap;
    int i;

    rhs->type = RHS_OP;
    rhs->v.op.op = &ops[op_index];

    va_start(ap, op_index);
    for (i = 0; i < ops[op_index].num_args; ++i)
	rhs->v.op.args[i] = va_arg(ap, primary_t);
    va_end(ap);

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

	assert(stmts->type == STMT_PHI_ASSIGN);

	if (stmts->v.assign.lhs->compvar == compvar)
	    return stmts;
    }

    return 0;
}

primary_t*
find_value_in_rhs (value_t *val, rhs_t *rhs)
{
    switch (rhs->type)
    {
	case RHS_PRIMARY :
	    if (rhs->v.primary.type == PRIMARY_VALUE
		&& rhs->v.primary.v.value == val)
		return &rhs->v.primary;
	    else
		return 0;
	    break;

	case RHS_INTERNAL :
	    return 0;

	case RHS_OP :
	    {
		int i;

		for (i = 0; i < rhs->v.op.op->num_args; ++i)
		    if (rhs->v.op.args[i].type == PRIMARY_VALUE
			&& rhs->v.op.args[i].v.value == val)
			return &rhs->v.op.args[i];
		return 0;
	    }
	    break;
    }

    return 0;
}

static void
for_each_value_in_rhs (rhs_t *rhs, void (*func) (value_t *value))
{
    if (rhs->type == RHS_PRIMARY && rhs->v.primary.type == PRIMARY_VALUE)
	func(rhs->v.primary.v.value);
    else if (rhs->type == RHS_OP)
    {
	int i;

	for (i = 0; i < rhs->v.op.op->num_args; ++i)
	{
	    primary_t *arg = &rhs->v.op.args[i];

	    if (arg->type == PRIMARY_VALUE)
		func(arg->v.value);
	}
    }
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
for_each_assign_statement (statement_t *stmts, void (*func) (statement_t *stmt))
{
    while (stmts != 0)
    {
	switch (stmts->type)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
	    case STMT_PHI_ASSIGN :
		func(stmts);
		break;

	    case STMT_IF_COND :
		for_each_assign_statement(stmts->v.if_cond.consequent, func);
		for_each_assign_statement(stmts->v.if_cond.alternative, func);
		for_each_assign_statement(stmts->v.if_cond.exit, func);
		break;

	    case STMT_WHILE_LOOP :
		for_each_assign_statement(stmts->v.while_loop.entry, func);
		for_each_assign_statement(stmts->v.while_loop.body, func);
		break;

	    default :
		assert(0);
	}

	stmts = stmts->next;
    }
}

static void
for_each_value_in_statements (statement_t *stmt, void (*func) (value_t *value, statement_t *stmt))
{
    void call_func (value_t *value)
	{ func(value, stmt); }

    while (stmt != 0)
    {
	switch (stmt->type)
	{
	    case STMT_NIL :
		break;

	    case STMT_PHI_ASSIGN :
		for_each_value_in_rhs(stmt->v.assign.rhs2, &call_func);
	    case STMT_ASSIGN :
		for_each_value_in_rhs(stmt->v.assign.rhs, &call_func);
		func(stmt->v.assign.lhs, stmt);
		break;

	    case STMT_IF_COND :
		for_each_value_in_rhs(stmt->v.if_cond.condition, &call_func);
		for_each_value_in_statements(stmt->v.if_cond.consequent, func);
		for_each_value_in_statements(stmt->v.if_cond.alternative, func);
		for_each_value_in_statements(stmt->v.if_cond.exit, func);
		break;

	    case STMT_WHILE_LOOP :
		for_each_value_in_rhs(stmt->v.while_loop.invariant, &call_func);
		for_each_value_in_statements(stmt->v.while_loop.entry, func);
		for_each_value_in_statements(stmt->v.while_loop.body, func);
		break;

	    default :
		assert(0);
	}

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

static void
rewrite_uses (value_t *old, primary_t new, statement_t *limit)
{
    statement_list_t **lst;

    if (new.type == PRIMARY_VALUE)
	assert(old != new.v.value);

    lst = &old->uses;
    while (*lst != 0)
    {
	statement_list_t *elem = *lst;
	statement_t *stmt = elem->stmt;
	primary_t *primary;

	/* we do not rewrite phis in the loop we're currently working on */
	if (stmt_is_within_limit(stmt, limit)
	    && !(stmt->type == STMT_PHI_ASSIGN && stmt->parent == limit))
	{
	    switch (stmt->type)
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
		    assert(0);
	    }

	    assert(primary != 0 && primary->v.value == old);

	    *primary = new;
	    if (new.type == PRIMARY_VALUE)
		add_use(new.v.value, stmt);

	    *lst = elem->next;
	}
	else
	    lst = &elem->next;
    }
}

static void
rewrite_uses_to_value (value_t *old, value_t *new, statement_t *limit)
{
    primary_t primary;

    if (old == new)
	return;

    primary.type = PRIMARY_VALUE;
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

	switch (tos->type)
	{
	    case STMT_IF_COND :
		{
		    statement_t *phi_assign = find_phi_assign(tos->v.if_cond.exit,
							      stmt->v.assign.lhs->compvar);

		    if (phi_assign == 0)
		    {
			phi_assign = alloc_stmt();

			phi_assign->type = STMT_PHI_ASSIGN;

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
			assert(phi_assign->v.assign.rhs->type = RHS_PRIMARY
			       && phi_assign->v.assign.rhs->v.primary.type == PRIMARY_VALUE);
			remove_use(phi_assign->v.assign.rhs->v.primary.v.value, phi_assign);

			phi_assign->v.assign.rhs = make_value_rhs(stmt->v.assign.lhs);
			add_use(stmt->v.assign.lhs, phi_assign);
		    }
		    else
		    {
			assert(phi_assign->v.assign.rhs2->type = RHS_PRIMARY
			       && phi_assign->v.assign.rhs2->v.primary.type == PRIMARY_VALUE);
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

			phi_assign->type = STMT_PHI_ASSIGN;

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
			assert(phi_assign->v.assign.rhs2->type = RHS_PRIMARY
			       && phi_assign->v.assign.rhs2->v.primary.type == PRIMARY_VALUE);
			remove_use(phi_assign->v.assign.rhs2->v.primary.v.value, phi_assign);

			phi_assign->v.assign.rhs2 = make_value_rhs(stmt->v.assign.lhs);
			add_use(stmt->v.assign.lhs, phi_assign);
		    }
		}
		break;

	    default :
		assert(0);
	}
    }

    assign_value_index_and_make_current(stmt->v.assign.lhs);
}

void
emit_stmt (statement_t *stmt)
{
    void add_use_in_stmt (value_t *value)
	{ add_use(value, stmt); }

    assert(stmt->next == 0);

    stmt->parent = CURRENT_STACK_TOP;

    *emit_loc = stmt;
    emit_loc = &stmt->next;

    switch (stmt->type)
    {
	case STMT_NIL :
	    break;

	case STMT_ASSIGN :
	    for_each_value_in_rhs(stmt->v.assign.rhs, add_use_in_stmt);
	    stmt->v.assign.lhs->def = stmt;
	    break;

	case STMT_PHI_ASSIGN :
	    for_each_value_in_rhs(stmt->v.assign.rhs, add_use_in_stmt);
	    for_each_value_in_rhs(stmt->v.assign.rhs2, add_use_in_stmt);
	    stmt->v.assign.lhs->def = stmt;
	    break;

	case STMT_IF_COND :
	    for_each_value_in_rhs(stmt->v.if_cond.condition, add_use_in_stmt);
	    break;

	case STMT_WHILE_LOOP :
	    for_each_value_in_rhs(stmt->v.while_loop.invariant, add_use_in_stmt);
	    break;

	default :
	    assert(0);
    }
}

void
emit_nil (void)
{
    statement_t *stmt = alloc_stmt();

    stmt->type = STMT_NIL;
    stmt->next = 0;

    emit_stmt(stmt);
}

void
emit_assign (value_t *lhs, rhs_t *rhs)
{
    statement_t *stmt = alloc_stmt();

    stmt->type = STMT_ASSIGN;
    stmt->next = 0;

    stmt->v.assign.lhs = lhs;
    stmt->v.assign.rhs = rhs;

    emit_stmt(stmt);

    commit_assign(stmt);
}

void
start_if_cond (rhs_t *condition)
{
    statement_t *stmt = alloc_stmt();

    stmt->type = STMT_IF_COND;
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
	assert(phi->type == STMT_PHI_ASSIGN);

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

    assert(stmt->type == STMT_IF_COND && stmt->v.if_cond.alternative == 0);

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

    assert(stmt->type == STMT_IF_COND && stmt->v.if_cond.consequent != 0);

    if (stmt->v.if_cond.alternative == 0)
	emit_nil();

    if (stmt->v.if_cond.exit == 0)
    {
	statement_t *nil = alloc_stmt();

	nil->type = STMT_NIL;

	UNSAFE_EMIT_STMT(nil, stmt->v.if_cond.exit);
    }

    --stmt_stackp;

    reset_values_for_phis(stmt->v.if_cond.exit, 1);

    for (phi = stmt->v.if_cond.exit; phi != 0; phi = phi->next)
    {
	assert(phi->type == STMT_PHI_ASSIGN);

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

    stmt->type = STMT_WHILE_LOOP;
    stmt->next = 0;

    stmt->v.while_loop.entry = 0;
    stmt->v.while_loop.body = 0;

    assert(invariant->type == RHS_PRIMARY && invariant->v.primary.type == PRIMARY_VALUE);

    value = invariant->v.primary.v.value;

    phi_assign = alloc_stmt();

    phi_assign->type = STMT_PHI_ASSIGN;
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

    assert(stmt->type == STMT_WHILE_LOOP);

    if (stmt->v.while_loop.body == 0)
	emit_nil();

    reset_values_for_phis(stmt->v.while_loop.entry, 1);

    for (phi = stmt->v.while_loop.entry; phi != 0; phi = phi->next)
    {
	assert(phi->type == STMT_PHI_ASSIGN);

	commit_assign(phi);
    }

    emit_loc = &stmt->next;
}

#define STK                   (invocation->stack_machine->stack)
#define STKP                  (invocation->stack_machine->stackp)

#include "new_builtins.c"

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
print_primary (primary_t *primary)
{
    switch (primary->type)
    {
	case PRIMARY_VALUE :
	    print_value(primary->v.value);
	    break;

	case PRIMARY_INT_CONST :
	    printf("%d", primary->v.int_const);
	    break;

	case PRIMARY_FLOAT_CONST :
	    fprintf_c(stdout, "%f", primary->v.float_const);
	    break;

	case PRIMARY_COMPLEX_CONST :
	    fprintf_c(stdout, "%f + %f i", crealf(primary->v.complex_const), cimagf(primary->v.complex_const));
	    break;

	case PRIMARY_COLOR_CONST :
	    printf("(%d,%d,%d,%d)",
		   RED(primary->v.color_const), GREEN(primary->v.color_const),
		   BLUE(primary->v.color_const), ALPHA(primary->v.color_const));
	    break;

	default :
	    assert(0);
    }
}

static void
print_rhs (rhs_t *rhs)
{
    switch (rhs->type)
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

	default :
	    assert(0);
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
    switch (stmt->type)
    {
	case STMT_ASSIGN :
	    print_value(stmt->v.assign.lhs);
	    printf(" (%d) = ", count_uses(stmt->v.assign.lhs));
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
	    printf(" (%d) = phi(", count_uses(stmt->v.assign.lhs));
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
	    assert(0);
    }
}

static void
dump_code (statement_t *stmt, int indent)
{
    while (stmt != 0)
    {
	if (stmt->type != STMT_NIL)
	    print_indent(indent);

	switch (stmt->type)
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
		assert(0);
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

static void
dump_pre_native_code (void)
{
    pre_native_insn_t *insn = first_pre_native_insn;

    while (insn != 0)
    {
	printf("%4d ", insn->index);
	switch (insn->type)
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
		assert(insn->stmt->type == STMT_IF_COND || insn->stmt->type == STMT_WHILE_LOOP);

		printf("  if not ");
		if (insn->stmt->type == STMT_IF_COND)
		    print_rhs(insn->stmt->v.if_cond.condition);
		else
		    print_rhs(insn->stmt->v.while_loop.invariant);
		printf(" goto %d\n", insn->v.target->index);
		break;

	    default :
		assert(0);
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

static void
gen_code (exprtree *tree, compvar_t **dest, int is_alloced)
{
    int i;

    switch (tree->type)
    {
	case EXPR_INT_CONST :
	    if (!is_alloced)
		dest[0] = make_temporary();
	    emit_assign(make_lhs(dest[0]), make_int_const_rhs(tree->val.int_const));
	    break;

	case EXPR_FLOAT_CONST :
	    if (!is_alloced)
		dest[0] = make_temporary();
	    emit_assign(make_lhs(dest[0]), make_float_const_rhs(tree->val.float_const));
	    break;

	case EXPR_TUPLE_CONST :
	    for (i = 0; i < tree->val.tuple_const.length; ++i)
	    {
		if (!is_alloced)
		    dest[i] = make_temporary();
		emit_assign(make_lhs(dest[i]), make_float_const_rhs(tree->val.tuple_const.data[i]));
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
			    dest[i] = make_temporary();

			gen_code(sub, &subscript, 0);

			for (j = 1; j < length; ++j)
			{
			    start_if_cond(make_op_rhs(OP_LESS, make_compvar_primary(subscript), make_int_const_primary(j)));
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
	    if (!is_alloced)
		dest[0] = make_temporary();
	    emit_assign(make_lhs(dest[0]), make_internal_rhs(tree->val.internal));
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
			if (subscript >= tree->result.length)
			    subscript = tree->result.length - 1;

			emit_assign(make_lhs(tree->val.sub_assignment.var->compvar[subscript]), make_compvar_rhs(temps[i]));
		    }
		    else
		    {
			compvar_t *subscript;
			int length = tree->val.sub_assignment.var->type.length;
			int j;

			if (!is_alloced)
			    dest[i] = make_temporary();

			gen_code(sub, &subscript, 0);

			for (j = 1; j < length; ++j)
			{
			    start_if_cond(make_op_rhs(OP_LESS, make_compvar_primary(subscript), make_int_const_primary(j)));
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
		exprtree *arg;
		int num_args = 0;
		compvar_t ***args;
		int *arglengths, *argnumbers;

		for (arg = tree->val.func.args; arg != 0; arg = arg->next)
		    ++num_args;

		args = (compvar_t***)alloca(num_args * sizeof(compvar_t**));
		arglengths = (int*)alloca(num_args * sizeof(int));
		argnumbers = (int*)alloca(num_args * sizeof(int));

		for (i = 0, arg = tree->val.func.args; i < num_args; ++i, arg = arg->next)
		{
		    args[i] = (compvar_t**)alloca(arg->result.length * sizeof(compvar_t*));
		    arglengths[i] = arg->result.length;
		    argnumbers[i] = arg->result.number;
		    gen_code(arg, args[i], 0);
		}

		if (!is_alloced)
		    for (i = 0; i < tree->result.length; ++i)
			dest[i] = make_temporary();

		tree->val.func.entry->v.builtin.generator(args, arglengths, argnumbers, dest);
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
		    result[i] = make_temporary();

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
		compvar_t *invariant = make_temporary();
		compvar_t **body_result = (compvar_t**)alloca(tree->val.whileExpr.body->result.length * sizeof(compvar_t*));

		if (tree->type == EXPR_DO_WHILE)
		    gen_code(tree->val.whileExpr.body, body_result, 0);

		gen_code(tree->val.whileExpr.invariant, &invariant, 1);
		start_while_loop(make_compvar_rhs(invariant));
		gen_code(tree->val.whileExpr.body, body_result, 0);
		gen_code(tree->val.whileExpr.invariant, &invariant, 1);
		end_while_loop();

		if (!is_alloced)
		    dest[0] = make_temporary();
		emit_assign(make_lhs(dest[0]), make_int_const_rhs(0));
	    }
	    break;

	case EXPR_USERVAL :
	    switch (tree->val.userval.info->type)
	    {
		case USERVAL_INT_CONST :
		    if (!is_alloced)
			dest[0] = make_temporary();
		    emit_assign(make_lhs(dest[0]), make_op_rhs(OP_USERVAL_INT, make_int_const_primary(tree->val.userval.info->index)));
		    break;

		case USERVAL_FLOAT_CONST :
		    if (!is_alloced)
			dest[0] = make_temporary();
		    emit_assign(make_lhs(dest[0]), make_op_rhs(OP_USERVAL_FLOAT, make_int_const_primary(tree->val.userval.info->index)));
		    break;

		case USERVAL_BOOL_CONST :
		    if (!is_alloced)
			dest[0] = make_temporary();
		    emit_assign(make_lhs(dest[0]), make_op_rhs(OP_USERVAL_BOOL, make_int_const_primary(tree->val.userval.info->index)));
		    break;

		case USERVAL_CURVE :
		    {
			compvar_t *pos;

			if (!is_alloced)
			    dest[0] = make_temporary();

			gen_code(tree->val.userval.args, &pos, 0);

			emit_assign(make_lhs(dest[0]), make_op_rhs(OP_USERVAL_CURVE, make_int_const_primary(tree->val.userval.info->index),
								   make_compvar_primary(pos)));
		    }
		    break;

		case USERVAL_COLOR :
		case USERVAL_GRADIENT :
		    {
			compvar_t *pos;
			compvar_t *temp = make_temporary();
			int i;

			if (!is_alloced)
			    for (i = 0; i < 4; ++i)
				dest[i] = make_temporary();

			if (tree->val.userval.info->type == USERVAL_COLOR)
			    emit_assign(make_lhs(temp), make_op_rhs(OP_USERVAL_COLOR, make_int_const_primary(tree->val.userval.info->index)));
			else
			{
			    gen_code(tree->val.userval.args, &pos, 0);
			    emit_assign(make_lhs(temp), make_op_rhs(OP_USERVAL_GRADIENT, make_int_const_primary(tree->val.userval.info->index),
								    make_compvar_primary(pos)));
			}

			emit_assign(make_lhs(dest[0]), make_op_rhs(OP_RED, make_compvar_primary(temp)));
			emit_assign(make_lhs(dest[1]), make_op_rhs(OP_GREEN, make_compvar_primary(temp)));
			emit_assign(make_lhs(dest[2]), make_op_rhs(OP_BLUE, make_compvar_primary(temp)));
			emit_assign(make_lhs(dest[3]), make_op_rhs(OP_ALPHA, make_compvar_primary(temp)));
		    }
		    break;

		case USERVAL_IMAGE :
		    if (!is_alloced)
			dest[0] = make_temporary();
		    emit_assign(make_lhs(dest[0]), make_int_const_rhs(tree->val.userval.info->index));
		    break;

		default :
		    assert(0);
	    }
	    break;

	default :
	    assert(0);
   }
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
perform_worklist_dfa (statement_t *stmts,
		      statement_list_t* (*build_worklist) (statement_t *stmt, statement_list_t *worklist),
		      statement_list_t* (*work_statement) (statement_t *stmt, statement_list_t *worklist))
{
    statement_list_t *worklist = 0;

    void builder (statement_t *stmt)
	{ worklist = build_worklist(stmt, worklist); }

    for_each_assign_statement(stmts, &builder);

    do
    {
	statement_list_t *new_worklist = 0;

	while (worklist != 0)
	{
	    new_worklist = work_statement(worklist->stmt, new_worklist);
	    worklist = worklist->next;
	}

	worklist = new_worklist;
    } while (worklist != 0);
}


/*** type propagation ***/

static type_t
primary_type (primary_t *primary)
{
    switch (primary->type)
    {
	case PRIMARY_VALUE :
	    return primary->v.value->compvar->type;

	case PRIMARY_INT_CONST :
	    return TYPE_INT;

	case PRIMARY_FLOAT_CONST :
	    return TYPE_FLOAT;

	case PRIMARY_COMPLEX_CONST :
	    return TYPE_COMPLEX;

	case PRIMARY_COLOR_CONST :
	    return TYPE_COLOR;

	default :
	    assert(0);
    }
}

static type_t
rhs_type (rhs_t *rhs)
{
    switch (rhs->type)
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

	default :
	    assert(0);
    }

    return 0;
}

static statement_list_t*
propagate_types_builder (statement_t *stmt, statement_list_t *worklist)
{
    int type, type2;

    switch (stmt->type)
    {
	case STMT_ASSIGN :
	    type = rhs_type(stmt->v.assign.rhs);
	    if (type != stmt->v.assign.lhs->compvar->type)
	    {
		stmt->v.assign.lhs->compvar->type = type;
		worklist = prepend_compvar_statements(stmt->v.assign.lhs->compvar, worklist);
	    }
	    break;

	case STMT_PHI_ASSIGN :
	    type = rhs_type(stmt->v.assign.rhs);
	    type2 = rhs_type(stmt->v.assign.rhs2);
	    if (type != type2)
	    {
		assert(type <= MAX_PROMOTABLE_TYPE && type2 <= MAX_PROMOTABLE_TYPE);
		if (type2 > type)
		    type = type2;
	    }
	    if (type != stmt->v.assign.lhs->compvar->type)
	    {
		stmt->v.assign.lhs->compvar->type = type;
		worklist = prepend_compvar_statements(stmt->v.assign.lhs->compvar, worklist);
	    }
	    break;

	default :
	    assert(0);
    }

    return worklist;
}

static statement_list_t*
propagate_types_worker (statement_t *stmt, statement_list_t *worklist)
{
    switch (stmt->type)
    {
	case STMT_ASSIGN :
	case STMT_PHI_ASSIGN :
	{
	    int type, type2;
	    type = rhs_type(stmt->v.assign.rhs);
	    if (stmt->type == STMT_PHI_ASSIGN)
	    {
		type2 = rhs_type(stmt->v.assign.rhs2);
		if (type != type2)
		{
		    assert(type <= MAX_PROMOTABLE_TYPE && type2 <= MAX_PROMOTABLE_TYPE);
		    if (type2 > type)
			type = type2;
		}
	    }

	    if (type != stmt->v.assign.lhs->compvar->type)
	    {
		stmt->v.assign.lhs->compvar->type = type;
		worklist = prepend_compvar_statements(stmt->v.assign.lhs->compvar, worklist);
	    }
	}
	break;

	case STMT_IF_COND :
	case STMT_WHILE_LOOP :
	    break;

	default :
	    assert(0);
    }

    return worklist;
}

static void
propagate_types (void)
{
    perform_worklist_dfa(first_stmt, &propagate_types_builder, &propagate_types_worker);
}

/*** constants analysis ***/

#define LEAST_CONST_TYPE(v)        ((v)->const_type & (v)->least_const_type_multiply_used_in)

int
primary_constant (primary_t *primary)
{
    switch (primary->type)
    {
	case PRIMARY_VALUE :
	    return LEAST_CONST_TYPE(primary->v.value);

	case PRIMARY_INT_CONST :
	case PRIMARY_FLOAT_CONST :
	case PRIMARY_COMPLEX_CONST :
	case PRIMARY_COLOR_CONST :
	    return CONST_MAX;

	default :
	    assert(0);
    }
}

int
rhs_constant (rhs_t *rhs)
{
    switch (rhs->type)
    {
	case RHS_PRIMARY :
	    return primary_constant(&rhs->v.primary);

	case RHS_INTERNAL :
	    return rhs->v.internal->const_type;

	case RHS_OP :
	    if (rhs->v.op.op->is_pure)
	    {
		int i;
		int const_type_max = CONST_MAX;

		for (i = 0; i < rhs->v.op.op->num_args; ++i)
		{
		    int const_type = primary_constant(&rhs->v.op.args[i]);

		    const_type_max &= const_type;
		}

		return const_type_max;
	    }
	    else
		return CONST_NONE;
	    break;

	default :
	    assert(0);
    }
}

void
analyze_phis_constant (statement_t *phis, int const_max, int *changed)
{
    while (phis != 0)
    {
	int const_type, const_type2;

	if (phis->type == STMT_NIL)
	{
	    phis = phis->next;
	    continue;
	}

	assert(phis->type == STMT_PHI_ASSIGN);

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
	switch (stmt->type)
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
		assert(0);
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
		assert(0);
	}

	stmt = stmt->next;
    }
}

static unsigned int
analyze_least_const_type_directly_used_in (statement_t *stmt)
{
    void clear_const_bits_from_assignment (value_t *val)
	{ val->least_const_type_directly_used_in &= LEAST_CONST_TYPE(stmt->v.assign.lhs); }

    unsigned int least_const_type = CONST_MAX;

    while (stmt != 0)
    {
	switch (stmt->type)
	{
	    case STMT_NIL :
		break;

	    case STMT_PHI_ASSIGN :
		for_each_value_in_rhs(stmt->v.assign.rhs2, &clear_const_bits_from_assignment);
	    case STMT_ASSIGN :
		for_each_value_in_rhs(stmt->v.assign.rhs, &clear_const_bits_from_assignment);
		least_const_type &= LEAST_CONST_TYPE(stmt->v.assign.lhs);
		break;

	    case STMT_IF_COND :
	    {
		unsigned int sub_least_const_type = CONST_MAX;

		void clear_const_bits (value_t *val)
		    { val->least_const_type_directly_used_in &= sub_least_const_type; }

		sub_least_const_type &= analyze_least_const_type_directly_used_in(stmt->v.if_cond.consequent);
		sub_least_const_type &= analyze_least_const_type_directly_used_in(stmt->v.if_cond.alternative);
		sub_least_const_type &= analyze_least_const_type_directly_used_in(stmt->v.if_cond.exit);

		for_each_value_in_rhs(stmt->v.if_cond.condition, &clear_const_bits);

		least_const_type &= sub_least_const_type;

		break;
	    }

	    case STMT_WHILE_LOOP :
	    {
		unsigned int sub_least_const_type = CONST_MAX;

		void clear_const_bits (value_t *val)
		    { val->least_const_type_directly_used_in &= sub_least_const_type; }

		sub_least_const_type &= analyze_least_const_type_directly_used_in(stmt->v.while_loop.entry);
		sub_least_const_type &= analyze_least_const_type_directly_used_in(stmt->v.while_loop.body);

		for_each_value_in_rhs(stmt->v.while_loop.invariant, &clear_const_bits);

		least_const_type &= sub_least_const_type;

		break;
	    }

	    default :
		assert(0);
	}

	stmt = stmt->next;
    }

    return least_const_type;
}

#undef LEAST_CONST_TYPE

static int
analyze_least_const_type_multiply_used_in (statement_t *stmt, int in_loop, value_set_t *multiply_assigned_set, int *changed)
{
    int update_const_mask;
    value_set_t *update_const_set = multiply_assigned_set;
    void update_const (value_t *val)
	{
	    if (value_set_contains(update_const_set, val)
		&& ((val->least_const_type_multiply_used_in & update_const_mask)
		    != val->least_const_type_multiply_used_in))
	    {
		val->least_const_type_multiply_used_in &= update_const_mask;
		*changed = 1;
	    }
	}

    int least_const = CONST_MAX;

    while (stmt != 0)
    {
	switch (stmt->type)
	{
	    case STMT_NIL :
		break;

	    case STMT_PHI_ASSIGN :
		update_const_mask = stmt->v.assign.lhs->least_const_type_multiply_used_in;
		for_each_value_in_rhs(stmt->v.assign.rhs2, &update_const);
	    case STMT_ASSIGN :
		update_const_mask = stmt->v.assign.lhs->least_const_type_multiply_used_in;
		for_each_value_in_rhs(stmt->v.assign.rhs, &update_const);
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
		for_each_value_in_rhs(stmt->v.if_cond.condition, &update_const);

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
		for_each_value_in_rhs(stmt->v.while_loop.invariant, &update_const);
		update_const_set = multiply_assigned_set;

		free_value_set(copy);

		least_const &= sub_least_const;

		break;
	    }

	    default:
		assert(0);
	}

	stmt = stmt->next;
    }

    return least_const;
}

static void
analyze_constants (void)
{
    void init_const_type (value_t *value, statement_t *stmt)
	{ value->const_type = CONST_MAX; }

    void init_least_const_types (value_t *value, statement_t *stmt)
	{
	    value->least_const_type_multiply_used_in = value->const_type;
	    value->least_const_type_directly_used_in = value->const_type;
	}

    int changed;

    for_each_value_in_statements(first_stmt, &init_const_type);

    do
    {
	changed = 0;
	analyze_stmts_constants(first_stmt, &changed, CONST_MAX);
    } while (changed);

    for_each_value_in_statements(first_stmt, &init_least_const_types);

    do
    {
	value_set_t *set = new_value_set();

	changed = 0;
	analyze_least_const_type_multiply_used_in(first_stmt, 0, set, &changed);

	free_value_set(set);
    } while (changed);

    analyze_least_const_type_directly_used_in(first_stmt);
}

/*** make_color optimization ***/

static int
is_color_def (statement_t *stmt, int op, value_t **value)
{
    if (stmt->type == STMT_ASSIGN
	&& stmt->v.assign.rhs->type == RHS_OP
	&& op_index(stmt->v.assign.rhs->v.op.op) == op
	&& stmt->v.assign.rhs->v.op.args[0].type == PRIMARY_VALUE)
    {
	*value = stmt->v.assign.rhs->v.op.args[0].v.value;
	return 1;
    }
    return 0;
}

static void
optimize_make_color (statement_t *stmt)
{
    while (stmt != 0)
    {
	switch (stmt->type)
	{
	    case STMT_NIL :
		stmt = stmt->next;
		break;

	    case STMT_ASSIGN :
		if (stmt->v.assign.rhs->type == RHS_OP
		    && op_index(stmt->v.assign.rhs->v.op.op) == OP_MAKE_COLOR)
		{
		    static int ops[] = { OP_RED, OP_GREEN, OP_BLUE, OP_ALPHA };

		    value_t *vals[4];
		    int i;

		    for (i = 0; i < 4; ++i)
			if (stmt->v.assign.rhs->v.op.args[i].type != PRIMARY_VALUE
			    || !is_color_def(stmt->v.assign.rhs->v.op.args[i].v.value->def, ops[i], &vals[i]))
			    break;

		    if (i == 4 && vals[0] == vals[1] && vals[0] == vals[2] && vals[0] == vals[3]) /* successful? */
		    {
			for (i = 0; i < 4; ++i)
			    remove_use(stmt->v.assign.rhs->v.op.args[i].v.value, stmt);

			stmt->v.assign.rhs = make_value_rhs(vals[0]);
			add_use(vals[0], stmt);
		    }
		}
		stmt = stmt->next;
		break;

	    case STMT_PHI_ASSIGN :
		stmt = stmt->next;
		break;

	    case STMT_IF_COND :
		optimize_make_color(stmt->v.if_cond.consequent);
		optimize_make_color(stmt->v.if_cond.alternative);
		stmt = stmt->next;
		break;

	    case STMT_WHILE_LOOP :
		optimize_make_color(stmt->v.while_loop.body);
		stmt = stmt->next;
		break;

	    default :
		assert(0);
	}
    }
}

/*** copy propagation ***/

static int
copy_propagation (void)
{
    int changed_at_all = 0;
    int changed;

    void propagate_copy (statement_t *stmt)
	{
	    if (stmt->type == STMT_ASSIGN
		&& stmt->v.assign.lhs->uses != 0
		&& stmt->v.assign.rhs->type == RHS_PRIMARY
		&& (stmt->v.assign.rhs->v.primary.type != PRIMARY_VALUE
		    || stmt->v.assign.rhs->v.primary.v.value != stmt->v.assign.lhs))
	    {
		rewrite_uses(stmt->v.assign.lhs, stmt->v.assign.rhs->v.primary, 0);
		changed = 1;
	    }
	}

    do
    {
	changed = 0;
	for_each_assign_statement(first_stmt, &propagate_copy);
	if (changed)
	    changed_at_all = 1;
    } while (changed);

    return changed_at_all;
}

/*** constant folding ***/

static int
rhs_is_foldable (rhs_t *rhs)
{
    int i;

    if (rhs->type != RHS_OP)
	return 0;

    if (!rhs->v.op.op->is_foldable)
	return 0;

    for (i = 0; i < rhs->v.op.op->num_args; ++i)
	if (rhs->v.op.args[i].type == PRIMARY_VALUE)
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
	switch (stmt->type)
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
		assert(0);
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

/*** dead assignment removal ***/

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
remove_assign_stmt_if_pure (statement_t *stmt, value_list_t **worklist, int *changed)
{
    void remove_value (value_t *value)
	{
	    assert(value->index < 0 || value->def->type != STMT_NIL);
	    remove_use(value, stmt);
	    if (value->uses == 0)
		*worklist = add_value_if_new(*worklist, value);

	    *changed = 1;
	}

    assert(stmt->v.assign.lhs->uses == 0);

    if ((stmt->v.assign.rhs->type == RHS_OP
	 && !stmt->v.assign.rhs->v.op.op->is_pure)
	|| (stmt->type == STMT_PHI_ASSIGN
	    && stmt->v.assign.rhs2->type == RHS_OP
	    && !stmt->v.assign.rhs2->v.op.op->is_pure))
	return;

    for_each_value_in_rhs(stmt->v.assign.rhs, &remove_value);
    if (stmt->type == STMT_PHI_ASSIGN)
	for_each_value_in_rhs(stmt->v.assign.rhs2, &remove_value);

    stmt->type = STMT_NIL;
}

static void
remove_dead_code_initially (statement_t *stmt, value_list_t **worklist)
{
    while (stmt != 0)
    {
	switch (stmt->type)
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
		assert(0);
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

	if (worklist->value->def->type == STMT_NIL)
	    assert(worklist->value->def == &dummy_stmt);
	else
	{
	    assert(worklist->value->def->type == STMT_ASSIGN
		   || worklist->value->def->type == STMT_PHI_ASSIGN);

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
    if (rhs->type != RHS_PRIMARY)
	return 0;

    if (rhs->v.primary.type == PRIMARY_VALUE)
	return 0;

    return 1;
}

static int
is_const_primary_rhs_true (rhs_t *rhs)
{
    assert(is_rhs_const_primary(rhs));

    switch (rhs->v.primary.type)
    {
	case PRIMARY_INT_CONST :
	    return rhs->v.primary.v.int_const != 0;

	case PRIMARY_FLOAT_CONST :
	    return rhs->v.primary.v.float_const != 0.0;

	case PRIMARY_COMPLEX_CONST :
	case PRIMARY_COLOR_CONST :
	    assert(0);

	default :
	    assert(0);
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
remove_uses_in_rhs (rhs_t *rhs, statement_t *stmt)
{
    void remove (value_t *val)
	{ remove_use(val, stmt); }

    for_each_value_in_rhs(rhs, &remove);
}

static void
remove_dead_branches_recursively (statement_t *stmt, int *changed)
{
    while (stmt != 0)
    {
	switch (stmt->type)
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

			if (branch->type == STMT_ASSIGN)
			{
			    statement_list_t *lst;

			    for (lst = branch->v.assign.lhs->uses; lst != 0; lst = lst->next)
				assert(has_indirect_parent(lst->stmt, stmt));
			}

			if (branch->type != STMT_NIL)
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

			if (phi->type == STMT_PHI_ASSIGN)
			{
			    if (condition_true)
				remove_uses_in_rhs(phi->v.assign.rhs2, phi);
			    else
				remove_uses_in_rhs(phi->v.assign.rhs, phi);

			    phi->type = STMT_ASSIGN;
			    if (!condition_true)
				phi->v.assign.rhs = phi->v.assign.rhs2;

			    phi->parent = stmt->parent;

			    phi->next = insertion_point->next;
			    insertion_point = insertion_point->next = phi;
			}
			else
			    assert(phi->type == STMT_NIL);

			phi = next;
		    }

		    stmt->type = STMT_NIL;

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
		assert(0);
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

/*** common subexpression eliminiation ***/

static int
primaries_equal (primary_t *prim1, primary_t *prim2)
{
    if (prim1->type != prim2->type)
	return 0;

    switch (prim1->type)
    {
	case PRIMARY_VALUE :
	    return prim1->v.value == prim2->v.value;

	case PRIMARY_INT_CONST :
	    return prim1->v.int_const == prim2->v.int_const;

	case PRIMARY_FLOAT_CONST :
	    return prim1->v.float_const == prim2->v.float_const;

	case PRIMARY_COMPLEX_CONST :
	    return prim1->v.complex_const == prim2->v.complex_const;

	case PRIMARY_COLOR_CONST :
	    return prim1->v.color_const == prim2->v.color_const;

	default :
	    assert(0);
    }

    return 0;
}

static int
rhss_equal (rhs_t *rhs1, rhs_t *rhs2)
{
    if (rhs1->type != rhs2->type)
	return 0;

    switch (rhs1->type)
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

	default :
	    assert(0);
    }

    return 0;
}

static void
replace_rhs_with_value (rhs_t **rhs, value_t *val, statement_t *stmt)
{
    void remove_value_use (value_t *value)
	{ remove_use(value, stmt); }

    for_each_value_in_rhs(*rhs, &remove_value_use);

    *rhs = make_value_rhs(val);

    add_use(val, stmt);
}

static void
replace_rhs_recursively (statement_t *stmt, rhs_t *rhs, value_t *val, int *changed)
{
    while (stmt != 0)
    {
	switch (stmt->type)
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
		assert(0);
	}

	stmt = stmt->next;
    }
}

static void
cse_recursively (statement_t *stmt, int *changed)
{
    while (stmt != 0)
    {
	switch (stmt->type)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
		if (stmt->v.assign.rhs->type == RHS_INTERNAL
		    || (stmt->v.assign.rhs->type == RHS_OP
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
		assert(0);
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

/*** pre native code generation ***/

static pre_native_insn_t*
new_pre_native_insn (int type, statement_t *stmt)
{
    pre_native_insn_t *insn = (pre_native_insn_t*)pools_alloc(&compiler_pools, sizeof(pre_native_insn_t));

    insn->type = type;
    insn->index = -1;
    insn->stmt = stmt;
    insn->next = 0;

    return insn;
}

static pre_native_insn_t*
new_pre_native_insn_goto (int type, statement_t *stmt, pre_native_insn_t *target)
{
    pre_native_insn_t *insn = new_pre_native_insn(type, stmt);

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

static void
generate_pre_native_code_for_phis (statement_t *stmt, int phi_rhs)
{
    while (stmt != 0)
    {
	if (stmt->type == STMT_PHI_ASSIGN)
	    emit_pre_native_insn(new_pre_native_insn_phi_assign(stmt, phi_rhs));
	else
	    assert(stmt->type == STMT_NIL);

	stmt = stmt->next;
    }
}

static void
generate_pre_native_code_recursively (statement_t *stmt)
{
    while (stmt != 0)
    {
	switch (stmt->type)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
		emit_pre_native_insn(new_pre_native_insn(PRE_NATIVE_INSN_ASSIGN, stmt));
		break;

	    case STMT_IF_COND :
		{
		    pre_native_insn_t *label_alternative = new_pre_native_insn(PRE_NATIVE_INSN_LABEL, 0);
		    pre_native_insn_t *label_end = new_pre_native_insn(PRE_NATIVE_INSN_LABEL, 0);

		    emit_pre_native_insn(new_pre_native_insn_goto(PRE_NATIVE_INSN_IF_COND_FALSE_GOTO,
								  stmt, label_alternative));
		    generate_pre_native_code_recursively(stmt->v.if_cond.consequent);
		    generate_pre_native_code_for_phis(stmt->v.if_cond.exit, 1);
		    emit_pre_native_insn(new_pre_native_insn_goto(PRE_NATIVE_INSN_GOTO, stmt, label_end));
		    emit_pre_native_insn(label_alternative);
		    generate_pre_native_code_recursively(stmt->v.if_cond.alternative);
		    generate_pre_native_code_for_phis(stmt->v.if_cond.exit, 2);
		    emit_pre_native_insn(label_end);
		}
		break;

	    case STMT_WHILE_LOOP :
		{
		    pre_native_insn_t *label_start = new_pre_native_insn(PRE_NATIVE_INSN_LABEL, 0);
		    pre_native_insn_t *label_end = new_pre_native_insn(PRE_NATIVE_INSN_LABEL, 0);

		    generate_pre_native_code_for_phis(stmt->v.while_loop.entry, 1);
		    emit_pre_native_insn(label_start);
		    emit_pre_native_insn(new_pre_native_insn_goto(PRE_NATIVE_INSN_IF_COND_FALSE_GOTO,
								  stmt, label_end));
		    generate_pre_native_code_recursively(stmt->v.while_loop.body);
		    generate_pre_native_code_for_phis(stmt->v.while_loop.entry, 2);
		    emit_pre_native_insn(new_pre_native_insn_goto(PRE_NATIVE_INSN_GOTO, stmt, label_start));
		    emit_pre_native_insn(label_end);
		}
		break;

	    default :
		assert(0);
	}

	stmt = stmt->next;
    }
}

static void
generate_pre_native_code (void)
{
    first_pre_native_insn = 0;
    last_pre_native_insn = 0;

    generate_pre_native_code_recursively(first_stmt);
}

/*** register allocation ***/

/*
static statement_t*
least_common_ancestor (statement_t *stmt1, statement_t *stmt2)
{
    while (stmt1 != 0)
    {
	statement_t *stmt;

	for (stmt = stmt2; stmt != 0; stmt = stmt->parent)
	    if (stmt == stmt1)
		return stmt;

	stmt1 = stmt1->parent;
    }

    return stmt1;
}

static void
determine_live_range (value_t *value, statement_t *stmt)
{
    statement_list_t *lst;
    statement_t *largest_loop = 0;

    if (value->index < 0)
	return;

    if (value->live_start != 0)
    {
	assert(value->live_end != 0);
	return;
    }

    for (lst = value->uses; lst != 0; lst = lst->next)
	if (lst->stmt->type == STMT_PHI_ASSIGN
	    && lst->stmt->parent->type == STMT_WHILE_LOOP
	    && rhs_contains(lst->stmt->v.assign.rhs2, value))
	    if (largest_loop == 0 || lst->stmt->parent->index < largest_loop->index)
		largest_loop = lst->stmt->parent;

    if (largest_loop != 0)
    {
	value->live_start = value->def;
	value->live_end = largest_loop;
    }
    else
    {
	value->live_start = value->def;

	for (lst = value->uses; lst != 0; lst = lst->next)
	    if (value->live_end == 0 || lst->stmt->index > value->live_end->index)
		value->live_end = lst->stmt;

	if (value->live_end == 0)
	    value->live_end = value->live_start;
    }
}
*/

static void
for_each_value_in_pre_native_insn (pre_native_insn_t *insn, void (*func) (value_t *value))
{
    switch (insn->type)
    {
	case PRE_NATIVE_INSN_LABEL :
	case PRE_NATIVE_INSN_GOTO :
	    break;

	case PRE_NATIVE_INSN_ASSIGN :
	    func(insn->stmt->v.assign.lhs);
	    for_each_value_in_rhs(insn->stmt->v.assign.rhs, func);
	    break;

	case PRE_NATIVE_INSN_PHI_ASSIGN :
	    assert(insn->v.phi_rhs == 1 || insn->v.phi_rhs == 2);

	    func(insn->stmt->v.assign.lhs);
	    if (insn->v.phi_rhs == 1)
		for_each_value_in_rhs(insn->stmt->v.assign.rhs, func);
	    else
		for_each_value_in_rhs(insn->stmt->v.assign.rhs2, func);
	    break;

	case PRE_NATIVE_INSN_IF_COND_FALSE_GOTO :
	    assert(insn->stmt->type == STMT_IF_COND || insn->stmt->type == STMT_WHILE_LOOP);

	    if (insn->stmt->type == STMT_IF_COND)
		for_each_value_in_rhs(insn->stmt->v.if_cond.condition, func);
	    else
		for_each_value_in_rhs(insn->stmt->v.while_loop.invariant, func);
	    break;

	default :
	    assert(0);
    }
}

static void
determine_live_ranges (void)
{
    pre_native_insn_t *insn;

    void update (value_t *value)
	{
	    if (value->live_start == 0)
	    {
		assert(value->live_end == 0);

		value->live_start = value->live_end = insn;
	    }
	    else
	    {
		assert(value->live_end != 0);

		value->live_end = insn;
	    }
	}

    for (insn = first_pre_native_insn; insn != 0; insn = insn->next)
	for_each_value_in_pre_native_insn(insn, &update);
}

static void
init_type_info (int type, int num_registers)
{
    int i;

    assert(type >= 0 && type <= MAX_TYPE);
    assert(num_registers >= 0);

    type_infos[type].num_registers = num_registers;
    if (num_registers > 0)
	type_infos[type].registers = (native_register_t*)malloc(sizeof(native_register_t) * num_registers);
    else
	type_infos[type].registers = 0;

    for (i = 0; i < num_registers; ++i)
    {
	type_infos[type].registers[i].value_allocated_to = 0;
	type_infos[type].registers[i].used_anywhere = 0;
    }
}

static void
allocate_reg_for_value (value_t *value)
{
    int type = value->compvar->type;
    int i;

    assert(value->allocated_register == 0);

    for (i = 0; i < type_infos[type].num_registers; ++i)
	if (type_infos[type].registers[i].value_allocated_to == 0)
	{
	    value->allocated_register = &type_infos[type].registers[i];
	    type_infos[type].registers[i].value_allocated_to = value;
	    type_infos[type].registers[i].used_anywhere = 1;

	    break;
	}
}

static void
register_allocation (void)
{
    int i;
    pre_native_insn_t *insn;

    void allocate (value_t *value)
	{
	    if (value->index < 0)
		return;
	    if (value->live_start == insn && value->allocated_register == 0)
		allocate_reg_for_value(value);
	}

    void deallocate (value_t *value)
	{
	    if (value->index < 0)
		return;
	    if (value->live_end == insn)
		if (value->allocated_register != 0
		    && value->allocated_register->value_allocated_to != 0)
		{
		    assert(value->allocated_register->value_allocated_to == value);

		    value->allocated_register->value_allocated_to = 0;
		}
	}

    init_type_info(TYPE_NIL, 0);
    init_type_info(TYPE_INT, 128);
    init_type_info(TYPE_FLOAT, 128);
    init_type_info(TYPE_COMPLEX, 128);
    init_type_info(TYPE_COLOR, 128);
    init_type_info(TYPE_MATRIX, 128);
    init_type_info(TYPE_VECTOR, 128);

    for (insn = first_pre_native_insn; insn != 0; insn = insn->next)
    {
	if (insn->type == PRE_NATIVE_INSN_ASSIGN
	    || insn->type == PRE_NATIVE_INSN_PHI_ASSIGN)
	{
	    value_t *lhs = insn->stmt->v.assign.lhs;
	    rhs_t *rhs;

	    if (insn->type == PRE_NATIVE_INSN_ASSIGN
		|| insn->v.phi_rhs == 1)
		rhs = insn->stmt->v.assign.rhs;
	    else
		rhs = insn->stmt->v.assign.rhs2;

	    if (rhs->type == RHS_PRIMARY && rhs->v.primary.type == PRIMARY_VALUE
		&& lhs->index >= 0 && rhs->v.primary.v.value->index >= 0
		&& lhs->live_start == insn && rhs->v.primary.v.value->live_end == insn)
	    {
		value_t *rhs_value = rhs->v.primary.v.value;

		assert(rhs_value->allocated_register != 0);
		assert(lhs->allocated_register == 0);

		lhs->allocated_register = rhs_value->allocated_register;

		lhs->allocated_register->value_allocated_to = lhs;

		/* this may be necessary if there was no dead code removal */
		deallocate(lhs);

		continue;
	    }
	}

	for_each_value_in_pre_native_insn(insn, &allocate);
	for_each_value_in_pre_native_insn(insn, &deallocate);
    }

    for (i = 0; i < MAX_TYPE; ++i)
    {
	int num_used = 0;
	int j;

	for (j = 0; j < type_infos[i].num_registers; ++j)
	{
	    assert(type_infos[i].registers[j].value_allocated_to == 0);

	    if (type_infos[i].registers[j].used_anywhere)
		++num_used;
	}

	printf("type %d : %d regs used\n", i, num_used);
    }
}

/*** ssa well-formedness check ***/

static void
check_rhs_defined (rhs_t *rhs, value_set_t *defined_set)
{
    void check_value (value_t *value)
	{ assert(value_set_contains(defined_set, value)); }

    for_each_value_in_rhs(rhs, check_value);
}

static value_t*
last_assignment_to_compvar (statement_t *stmts, compvar_t *compvar)
{
    value_t *last = 0;

    while (stmts != 0)
    {
	switch (stmts->type)
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
		assert(0);
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
check_phis (statement_t *stmts, statement_t *parent, statement_t *body1, statement_t *body2,
	    GHashTable *current_value_hash, value_set_t *defined_set)
{
    while (stmts != 0)
    {
	assert(stmts->parent == parent);

	assert(stmts->type == STMT_NIL || stmts->type == STMT_PHI_ASSIGN);

	if (stmts->type == STMT_PHI_ASSIGN)
	{
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

	switch (stmts->type)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN:
		check_rhs_defined(stmts->v.assign.rhs, defined_set);
		assert(!value_set_contains(defined_set, stmts->v.assign.lhs));
		set_value_defined_and_current_for_checking(stmts->v.assign.lhs, current_value_hash, defined_set);
		break;

	    case STMT_PHI_ASSIGN :
		assert(0);
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
		assert(0);
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
slice_code (statement_t *stmt, unsigned int slice_flag, int (*predicate) (statement_t *stmt))
{
    int non_empty = 0;

    while (stmt != 0)
    {
	switch (stmt->type)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
	    case STMT_PHI_ASSIGN :
		if (predicate(stmt))
		{
		    stmt->slice_flags |= slice_flag;
		    non_empty = 1;
		}
		break;

	    case STMT_IF_COND :
	    {
		int result;

		result = slice_code(stmt->v.if_cond.consequent, slice_flag, predicate);
		result = slice_code(stmt->v.if_cond.alternative, slice_flag, predicate) || result;
		result = slice_code(stmt->v.if_cond.exit, slice_flag, predicate) || result;

		if (result)
		{
		    slice_code(stmt->v.if_cond.exit, slice_flag, predicate);

		    stmt->slice_flags |= slice_flag;
		    non_empty = 1;
		}
		break;
	    }

	    case STMT_WHILE_LOOP :
	    {
		if (slice_code(stmt->v.while_loop.body, slice_flag, predicate))
		{
		    slice_code(stmt->v.while_loop.entry, slice_flag, predicate);

		    stmt->slice_flags |= slice_flag;
		    non_empty = 1;
		}
		else
		    assert(!slice_code(stmt->v.while_loop.entry, slice_flag, predicate));
		break;
	    }

	    default:
		assert(0);
	}

	stmt = stmt->next;
    }

    return non_empty;
}

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
	fputs("0 /* uninitialized */ ", out);
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
	    fprintf(out, "var_%s_%d_%d", value->compvar->var->name, value->compvar->n, value->index);
	else
	    fprintf(out, "tmp_%d_%d", value->compvar->temp->number, value->index);
    }
}

void
output_value_decl (FILE *out, value_t *value)
{
    if (!value->have_defined && value->index >= 0)
    {
	switch (value->compvar->type)
	{
	    case TYPE_INT :
		fputs("int ", out);
		break;

	    case TYPE_FLOAT :
		fputs("float ", out);
		break;

	    case TYPE_COLOR :
		fputs("color_t ", out);
		break;

	    case TYPE_COMPLEX :
		fputs("complex float ", out);
		break;

	    case TYPE_MATRIX :
		fputs("gsl_matrix *", out);
		break;

	    case TYPE_VECTOR :
		fputs("gsl_vector *", out);
		break;
	}

	output_value_name(out, value, 1);
	fputs(";\n", out);
	value->have_defined = 1;
    }
}

static void
reset_have_defined (statement_t *stmt)
{
    void reset_value_have_defined (value_t *value, statement_t *stmt)
	{ value->have_defined = 0; }

    for_each_value_in_statements(stmt, &reset_value_have_defined);
}

void
output_primary (FILE *out, primary_t *prim)
{
    switch (prim->type)
    {
	case PRIMARY_VALUE :
	    output_value_name(out, prim->v.value, 0);
	    break;

	case PRIMARY_INT_CONST :
	    fprintf(out, "%d", prim->v.int_const);
	    break;

	case PRIMARY_FLOAT_CONST :
	    fprintf_c(out, "%f", prim->v.float_const);
	    break;

	case PRIMARY_COMPLEX_CONST :
	    fprintf_c(out, "COMPLEX(%f,%f)", crealf(prim->v.complex_const), cimagf(prim->v.complex_const));
	    break;

	case PRIMARY_COLOR_CONST :
	    fprintf(out, "MAKE_RGBA_COLOR(%d,%d,%d,%d)",
		    RED(prim->v.color_const), GREEN(prim->v.color_const),
		    BLUE(prim->v.color_const), ALPHA(prim->v.color_const));
	    break;

	default :
	    assert(0);
    }
}

void
output_rhs (FILE *out, rhs_t *rhs)
{
    switch (rhs->type)
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

	default :
	    assert(0);
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
        if (phis->type == STMT_NIL)
#endif
	{
	    phis = phis->next;
	    continue;
	}

	assert(phis->type == STMT_PHI_ASSIGN);

	rhs = ((branch == 0) ? phis->v.assign.rhs : phis->v.assign.rhs2);

	if (rhs->type != RHS_PRIMARY
	    || rhs->v.primary.type != PRIMARY_VALUE
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
	    switch (stmt->type)
	    {
		case STMT_NIL :
#ifndef NO_CONSTANTS_ANALYSIS
		    assert(0);
#endif
		    break;

		case STMT_ASSIGN :
		    output_value_name(out, stmt->v.assign.lhs, 0);
		    fputs(" = ", out);
		    output_rhs(out, stmt->v.assign.rhs);
		    fputs(";\n", out);
		    break;

		case STMT_PHI_ASSIGN :
		    assert(0);
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
		    assert(0);
	    }

	stmt = stmt->next;
    }
}

static void
output_permanent_const_declarations (FILE *out, int const_type)
{
    void output_value_if_needed (value_t *value, statement_t *stmt)
	{
	    if ((value->const_type | CONST_T) == (const_type | CONST_T)
		&& is_permanent_const_value(value))
		output_value_decl(out, value);
	}

    reset_have_defined(first_stmt);

    for_each_value_in_statements(first_stmt, &output_value_if_needed);
}

static void
output_permanent_const_code (FILE *out, int const_type)
{
    int is_value_needed (value_t *value)
	{
	    return (value->const_type | CONST_T) == (const_type | CONST_T)
		|| (is_const_type_within(const_type | CONST_T,
					 value->least_const_type_multiply_used_in,
					 value->const_type | CONST_T));
	}

    void output_value_if_needed (value_t *value, statement_t *stmt)
	{
	    if ((is_temporary_const_value(value) || const_type == 0)
		&& is_value_needed(value))
		output_value_decl(out, value);
	}

    int const_predicate (statement_t *stmt)
	{
	    assert(stmt->type == STMT_ASSIGN || stmt->type == STMT_PHI_ASSIGN);

	    return is_value_needed(stmt->v.assign.lhs);
	}

    unsigned int slice_flag;

    /* declarations */
    reset_have_defined(first_stmt);
    for_each_value_in_statements(first_stmt, &output_value_if_needed);

    /* code */
    if (const_type == (CONST_X | CONST_Y))
	slice_flag = SLICE_XY_CONST;
    else if (const_type == CONST_Y)
	slice_flag = SLICE_Y_CONST;
    else if (const_type == CONST_X)
	slice_flag = SLICE_X_CONST;
    else
	slice_flag = SLICE_NO_CONST;

    slice_code(first_stmt, slice_flag, &const_predicate);

    output_stmts(out, first_stmt, slice_flag);
}

/*** compiling and loading ***/

#ifdef OPENSTEP
#ifndef MAX
#define	MAX(a,b)	(((a)<(b))?(b):(a))
#endif
#define	CGEN_CC		"cc -c -fPIC -faltivec -o"
#define	CGEN_LD		"cc -bundle -flat_namespace -undefined suppress -o"
#endif

#ifdef OPENSTEP
#include <sys/param.h>
#include <sys/sysctl.h>

int
has_altivec (void)
{
    int mib[2], gHasAltivec;
    size_t len;

    mib[0] = CTL_HW;
    mib[1] = HW_VECTORUNIT;
    len = sizeof(gHasAltivec);
    sysctl(mib, 2, &gHasAltivec, &len, NULL, 0);

    if (gHasAltivec)
	printf("has altivec\n");
    else
	printf("no altivec\n");

    return (gHasAltivec != 0);
}
#endif

void
generate_ir_code (mathmap_t *mathmap, int constant_analysis)
{
    compvar_t *result[MAX_TUPLE_LENGTH];
    int changed;

    first_stmt = 0;
    emit_loc = &first_stmt;
    next_temp_number = 1;
    next_value_global_index = 0;

    init_pools(&compiler_pools);

    gen_code(mathmap->top_level_decls->v.filter.body, result, 0);
    {
	compvar_t *color_tmp = make_temporary(), *dummy = make_temporary();

	emit_assign(make_lhs(color_tmp), make_op_rhs(OP_MAKE_COLOR,
						     make_compvar_primary(result[0]), make_compvar_primary(result[1]),
						     make_compvar_primary(result[2]), make_compvar_primary(result[3])));
	emit_assign(make_lhs(dummy), make_op_rhs(OP_OUTPUT_COLOR, make_compvar_primary(color_tmp)));
    }

    propagate_types();

    check_ssa(first_stmt);

    optimize_make_color(first_stmt);

    do
    {
	printf("--------------------------------\n");
	dump_code(first_stmt, 0);

	changed = 0;

	changed = copy_propagation() || changed;
	changed = common_subexpression_elimination() || changed;
	changed = copy_propagation() || changed;
	changed = constant_folding() || changed;
	changed = remove_dead_assignments() || changed;
	changed = remove_dead_branches() || changed;
    } while (changed);

#ifndef NO_CONSTANTS_ANALYSIS
    if (constant_analysis)
	analyze_constants();
#endif

    dump_code(first_stmt, 0);
    check_ssa(first_stmt);

    /* no statement reordering after this point */
    generate_pre_native_code();
    determine_live_ranges();

    dump_pre_native_code();

    register_allocation();
}

void
forget_ir_code (mathmap_t *mathmap)
{
    first_stmt = 0;

    free_pools(&compiler_pools);
}

static char *opmacros_filename = 0;

void
set_opmacros_filename (const char *filename)
{
    if (opmacros_filename != 0)
	free(opmacros_filename);
    opmacros_filename = strdup(filename);
    assert(opmacros_filename != 0);
}

int
compiler_template_processor (mathmap_t *mathmap, const char *directive, FILE *out)
{
    assert(mathmap->top_level_decls != 0
	   && mathmap->top_level_decls->type == TOP_LEVEL_FILTER);

    if (strcmp(directive, "l") == 0)
	fprintf(out, "%d", MAX_TUPLE_LENGTH);
    else if (strcmp(directive, "g") == 0)
    {
#ifdef OPENSTEP
	putc('0', out);
#else
	putc('1', out);
#endif
    }
    else if (strcmp(directive, "m") == 0)
	output_permanent_const_code(out, 0);
    else if (strcmp(directive, "p") == 0)
	fprintf(out, "%d", USER_CURVE_POINTS);
    else if (strcmp(directive, "q") == 0)
	fprintf(out, "%d", USER_GRADIENT_POINTS);
    else if (strcmp(directive, "a") == 0)
    {
#ifdef OPENSTEP
	putc(has_altivec() ? '1' : '0', out);
#else
	putc('0', out);
#endif
    }
    else if (strcmp(directive, "xy_decls") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_declarations(out, CONST_X | CONST_Y);
#endif
    }
    else if (strcmp(directive, "x_decls") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_declarations(out, CONST_X);
#endif
    }
    else if (strcmp(directive, "y_decls") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_declarations(out, CONST_Y);
#endif
    }
    else if (strcmp(directive, "xy_code") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_code(out, CONST_X | CONST_Y);
#endif
    }
    else if (strcmp(directive, "x_code") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_code(out, CONST_X);
#endif
    }
    else if (strcmp(directive, "y_code") == 0)
    {
#ifndef NO_CONSTANTS_ANALYSIS
	output_permanent_const_code(out, CONST_Y);
#endif
    }
    else if (strcmp(directive, "opmacros_h") == 0)
    {
	fputs(opmacros_filename, out);
    }
    else if (strcmp(directive, "max_debug_tuples") == 0)
    {
	fprintf(out, "%d", MAX_DEBUG_TUPLES);
    }
    else if (strcmp(directive, "uses_ra") == 0)
    {
	fprintf(out, "%d", does_mathmap_use_ra(mathmap) ? 1 : 0);
    }
    else if (strcmp(directive, "uses_t") == 0)
    {
	fprintf(out, "%d", does_mathmap_use_t(mathmap) ? 1 : 0);
    }
    else if (strcmp(directive, "filter_name") == 0)
    {
	fprintf(out, "%s", mathmap->top_level_decls->name);
    }
    else if (strcmp(directive, "filter_docstring") == 0)
    {
	if (mathmap->top_level_decls->docstring != 0)
	    fprintf(out, "%s", mathmap->top_level_decls->docstring);
    }
    else if (strcmp(directive, "num_uservals") == 0)
    {
	fprintf(out, "%d", mathmap->num_uservals);
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
gen_and_load_c_code (mathmap_t *mathmap, void **module_info, FILE *template, char *opmacros_filename)
{
    static int last_mathfunc = 0;

    FILE *out;
    char *c_filename, *o_filename, *so_filename, *log_filename;
    int pid = getpid();
    void *initfunc_ptr;
    initfunc_t initfunc;
#ifndef OPENSTEP
    GModule *module = 0;
#endif

    assert(template != 0);

    generate_ir_code(mathmap, 1);

    c_filename = g_strdup_printf("%s%d_%d.c", TMP_PREFIX, pid, ++last_mathfunc);
    out = fopen(c_filename, "w");
    if (out == 0)
    {
	sprintf(error_string, "Could not write temporary file `%s'", c_filename);
	return 0;
    }

    set_opmacros_filename(opmacros_filename);
    process_template_file(mathmap, template, out, &compiler_template_processor);

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

    printf("loaded %p\n", module);

    assert(g_module_symbol(module, "mathmapinit", &initfunc_ptr));
    initfunc = (initfunc_t)initfunc_ptr;

    *module_info = module;
#else
    {
        NSObjectFileImage objectFileImage;
        NSModule module;
        const char *moduleName = "Johnny";
        NSSymbol symbol;
        
        NSCreateObjectFileImageFromFile(buf, &objectFileImage);
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

    unlink(c_filename);
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

    printf("unloading %p\n", module);
    assert(g_module_close(module));
#else
    /* FIXME */
#endif
}

/*** plug-in generator ***/

int
generate_plug_in (char *filter, char *output_filename,
		  char *template_filename, char *opmacros_filename, int analyze_constants,
		  template_processor_func_t template_processor)
{
    char template_path[strlen(TEMPLATE_DIR) + 1 + strlen(template_filename) + 1];
    char opmacros_path[strlen(TEMPLATE_DIR) + 1 + strlen(opmacros_filename) + 1];
    FILE *template, *out;
    mathmap_t *mathmap;

    sprintf(template_path, "%s/%s", TEMPLATE_DIR, template_filename);
    sprintf(opmacros_path, "%s/%s", TEMPLATE_DIR, opmacros_filename);

    mathmap = parse_mathmap(filter);

    if (mathmap == 0)
    {
	fprintf(stderr, "Error: %s\n", error_string);
	return 0;
    }

    generate_ir_code(mathmap, analyze_constants);

    template = fopen(template_path, "r");
    out = fopen(output_filename, "w");

    if (template == 0)
    {
	fprintf(stderr, "Could not open template file `%s'\n", template_path);
	exit(1);
    }
    if (out == 0)
    {
	fprintf(stderr, "Could not open output file `%s'\n", output_filename);
	exit(1);
    }

    set_opmacros_filename(opmacros_path);
    process_template_file(mathmap, template, out, template_processor);

    fclose(template);
    fclose(out);

    forget_ir_code(mathmap);

    return 1;
}

/*** inits ***/

static void
init_op (int index, char *name, int num_args, type_prop_t type_prop, type_t const_type, int is_pure, int is_foldable)
{
    assert(num_args <= MAX_OP_ARGS);

    ops[index].index = index;
    ops[index].name = name;
    ops[index].num_args = num_args;
    ops[index].type_prop = type_prop;
    ops[index].const_type = const_type;
    ops[index].is_pure = is_pure;
    ops[index].is_foldable = is_foldable;
}

void
init_compiler (void)
{
    init_ops();
}
