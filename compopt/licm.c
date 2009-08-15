/*
 * licm.c
 *
 * MathMap
 *
 * Copyright (C) 2009 Mark Probst
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

#include <glib.h>

#include "../compiler-internals.h"

static void
add_values_from_phis (statement_t *stmt, value_set_t *set_values)
{
    while (stmt != NULL)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_PHI_ASSIGN :
		compiler_value_set_add(set_values, stmt->v.assign.lhs);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

static void
add_values_from_stmt (statement_t *stmt, value_set_t *set_values)
{
    switch (stmt->kind)
    {
	case STMT_NIL :
	    break;

	case STMT_ASSIGN :
	case STMT_PHI_ASSIGN :
	    compiler_value_set_add(set_values, stmt->v.assign.lhs);
	    break;

	case STMT_IF_COND :
	    add_values_from_phis(stmt->v.if_cond.exit, set_values);
	    break;

	case STMT_WHILE_LOOP :
	    add_values_from_phis(stmt->v.while_loop.entry, set_values);
	    break;

	default :
	    g_assert_not_reached();
    }
}

static void
add_values_from_stmts (statement_t *stmts, value_set_t *set_values)
{
    while (stmts != NULL)
    {
	add_values_from_stmt(stmts, set_values);
	stmts = stmts->next;
    }
}

static void
_check_value_in_set (value_t *value, void *info)
{
    value_set_t *values = CLOSURE_GET(0, value_set_t*);
    gboolean *all_values_in_set = CLOSURE_GET(1, gboolean*);

    if (!compiler_value_set_contains(values, value))
	*all_values_in_set = FALSE;
}

static gboolean stmts_only_contain_values_in_set (statement_t *stmts, value_set_t *values);

static gboolean
stmt_only_contains_values_in_set (statement_t *stmt, value_set_t *values)
{
    gboolean all_values_in_set = TRUE;

    switch (stmt->kind)
    {
	case STMT_NIL :
	    return TRUE;

	case STMT_PHI_ASSIGN :
	    COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs2, &_check_value_in_set, values, &all_values_in_set);
	case STMT_ASSIGN :
	    if (all_values_in_set)
		COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.assign.rhs, &_check_value_in_set, values, &all_values_in_set);
	    return all_values_in_set;

	case STMT_IF_COND :
	    {
		value_set_t *values_copy;

		COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.if_cond.condition, &_check_value_in_set, values, &all_values_in_set);
		if (!all_values_in_set)
		    return FALSE;
		if (!stmts_only_contain_values_in_set(stmt->v.if_cond.consequent, values))
		    return FALSE;
		if (!stmts_only_contain_values_in_set(stmt->v.if_cond.alternative, values))
		    return FALSE;

		values_copy = compiler_value_set_copy(values);
		add_values_from_stmts(stmt->v.if_cond.consequent, values_copy);
		add_values_from_stmts(stmt->v.if_cond.alternative, values_copy);
		all_values_in_set = stmts_only_contain_values_in_set(stmt->v.if_cond.exit, values_copy);
		compiler_free_value_set(values_copy);

		return all_values_in_set;
	    }

	case STMT_WHILE_LOOP :
	    {
		value_set_t *values_copy;

		values_copy = compiler_value_set_copy(values);
		add_values_from_phis(stmt->v.while_loop.entry, values_copy);
		COMPILER_FOR_EACH_VALUE_IN_RHS(stmt->v.while_loop.invariant, &_check_value_in_set, values, &all_values_in_set);
		if (all_values_in_set)
		    all_values_in_set = stmts_only_contain_values_in_set(stmt->v.while_loop.body, values_copy);
		if (all_values_in_set)
		{
		    add_values_from_stmts(stmt->v.while_loop.body, values_copy);
		    all_values_in_set = stmts_only_contain_values_in_set(stmt->v.while_loop.entry, values_copy);
		}
		compiler_free_value_set(values_copy);

		return all_values_in_set;
	    }

	default :
	    g_assert_not_reached();
    }

    g_assert_not_reached();
}

static gboolean
stmts_only_contain_values_in_set (statement_t *stmts, value_set_t *values)
{
    values = compiler_value_set_copy(values);

    while (stmts != NULL)
    {
	if (!stmt_only_contains_values_in_set(stmts, values))
	{
	    compiler_free_value_set(values);
	    return FALSE;
	}

	add_values_from_stmt(stmts, values);

	stmts = stmts->next;
    }

    compiler_free_value_set(values);

    return TRUE;
}

static gboolean
process_loop (statement_t **loop, value_set_t *set_values)
{
    statement_t **iter;
    gboolean did_change = FALSE;

    set_values = compiler_value_set_copy(set_values);

    g_assert((*loop)->kind == STMT_WHILE_LOOP);

    iter = &(*loop)->v.while_loop.body;
    while (*iter != NULL)
    {
	if (stmt_only_contains_values_in_set(*iter, set_values))
	{
	    statement_t *stmt = compiler_stmt_unlink(iter);

	    loop = compiler_stmt_insert_before(stmt, loop);
	    add_values_from_stmt(stmt, set_values);
	    did_change = TRUE;
	}
	else
	    iter = &(*iter)->next;
    }

    compiler_free_value_set(set_values);

    return did_change;
}

static void
recurse (statement_t **stmtp, value_set_t *set_values, gboolean *did_change)
{
    while (*stmtp != NULL)
    {
	statement_t *stmt = *stmtp;

	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
		compiler_value_set_add(set_values, stmt->v.assign.lhs);
		break;

	    case STMT_IF_COND :
		{
		    value_set_t *set_values_copy;

		    set_values_copy = compiler_value_set_copy(set_values);
		    recurse(&stmt->v.if_cond.consequent, set_values_copy, did_change);
		    compiler_free_value_set(set_values_copy);

		    set_values_copy = compiler_value_set_copy(set_values);
		    recurse(&stmt->v.if_cond.alternative, set_values_copy, did_change);
		    compiler_free_value_set(set_values_copy);

		    add_values_from_phis(stmt->v.if_cond.exit, set_values);
		}
		break;

	    case STMT_WHILE_LOOP :
		if (process_loop(stmtp, set_values))
		{
		    *did_change = TRUE;
		    continue;
		}
		else
		{
		    value_set_t *set_values_copy;

		    add_values_from_phis(stmt->v.while_loop.entry, set_values);

		    set_values_copy = compiler_value_set_copy(set_values);
		    recurse(&stmt->v.while_loop.body, set_values_copy, did_change);
		    compiler_free_value_set(set_values_copy);
		}
		break;

	    case STMT_PHI_ASSIGN :
	    default :
		g_assert_not_reached();
	}

	stmtp = &(*stmtp)->next;
    }
}

gboolean
compiler_opt_loop_invariant_code_motion (statement_t **first_stmt)
{
    value_set_t *set_values = compiler_new_value_set();
    gboolean did_change = FALSE;

    recurse(first_stmt, set_values, &did_change);

    compiler_free_value_set(set_values);

    return did_change;
}
