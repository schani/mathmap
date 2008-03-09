/*
 * dce.c
 *
 * MathMap
 *
 * Copyright (C) 2008 Mark Probst
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

/*** dead assignment removal ***/

static void
_add_value (value_t *value, void *info)
{
    CLOSURE_VAR(GSList**, worklist, 0);

    *worklist = g_slist_prepend(*worklist, value);
}

static void
add_values_in_rhs (rhs_t *rhs, GSList **worklist)
{
    COMPILER_FOR_EACH_VALUE_IN_RHS(rhs, &_add_value, worklist);
}

static void
find_live_code_initially (statement_t *stmt, GSList **worklist)
{
    while (stmt != NULL)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_PHI_ASSIGN :
		if (!compiler_rhs_is_pure(stmt->v.assign.rhs2))
		    *worklist = g_slist_prepend(*worklist, stmt->v.assign.lhs);
	    case STMT_ASSIGN :
		if (!compiler_rhs_is_pure(stmt->v.assign.rhs))
		    *worklist = g_slist_prepend(*worklist, stmt->v.assign.lhs);
		break;

	    case STMT_IF_COND :
		add_values_in_rhs(stmt->v.if_cond.condition, worklist);
		find_live_code_initially(stmt->v.if_cond.consequent, worklist);
		find_live_code_initially(stmt->v.if_cond.alternative, worklist);
		break;

	    case STMT_WHILE_LOOP :
		add_values_in_rhs(stmt->v.while_loop.invariant, worklist);
		find_live_code_initially(stmt->v.while_loop.body, worklist);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

static void
find_live_code_from_worklist (GSList *worklist, value_set_t *live_values, GSList **new_worklist, gboolean *changed)
{
    while (worklist != NULL)
    {
	value_t *value = worklist->data;

	if (!compiler_value_set_contains(live_values, value))
	{
	    statement_t *stmt = value->def;

	    compiler_value_set_add(live_values, value);

	    switch (stmt->kind)
	    {
		case STMT_NIL :
		    break;

		case STMT_PHI_ASSIGN :
		    add_values_in_rhs(stmt->v.assign.rhs2, new_worklist);
		case STMT_ASSIGN :
		    add_values_in_rhs(stmt->v.assign.rhs, new_worklist);
		    break;

		default :
		    g_assert_not_reached();
	    }
	}

	worklist = worklist->next;
    }
}

static void
remove_dead_assignments (statement_t *stmt, value_set_t *live_values, gboolean *changed)
{
    while (stmt != NULL)
    {
	switch (stmt->kind)
	{
	    case STMT_NIL :
		break;

	    case STMT_ASSIGN :
	    case STMT_PHI_ASSIGN :
		if (!compiler_value_set_contains(live_values, stmt->v.assign.lhs))
		{
		    g_assert(compiler_rhs_is_pure(stmt->v.assign.rhs));
		    compiler_remove_uses_in_rhs(stmt->v.assign.rhs, stmt);

		    if (stmt->kind == STMT_PHI_ASSIGN)
		    {
			g_assert(compiler_rhs_is_pure(stmt->v.assign.rhs2));
			compiler_remove_uses_in_rhs(stmt->v.assign.rhs2, stmt);
		    }

		    stmt->kind = STMT_NIL;

		    *changed = TRUE;
		}
		break;

	    case STMT_IF_COND :
		remove_dead_assignments(stmt->v.if_cond.consequent, live_values, changed);
		remove_dead_assignments(stmt->v.if_cond.alternative, live_values, changed);
		remove_dead_assignments(stmt->v.if_cond.exit, live_values, changed);
		break;

	    case STMT_WHILE_LOOP :
		remove_dead_assignments(stmt->v.while_loop.entry, live_values, changed);
		remove_dead_assignments(stmt->v.while_loop.body, live_values, changed);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = stmt->next;
    }
}

gboolean
compiler_opt_remove_dead_assignments (statement_t *first_stmt)
{
    gboolean changed = FALSE;
    GSList *worklist = NULL;
    value_set_t *live_values = compiler_new_value_set();

    find_live_code_initially(first_stmt, &worklist);

    do
    {
	GSList *new_worklist = NULL;

	find_live_code_from_worklist(worklist, live_values, &new_worklist, &changed);
	g_slist_free(worklist);
	worklist = new_worklist;
    } while (worklist != NULL);

    remove_dead_assignments(first_stmt, live_values, &changed);

    compiler_free_value_set(live_values);

    return changed;
}
