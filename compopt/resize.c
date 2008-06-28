/*
 * resize.c
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

/*** RESIZE_IMAGE/ORIG_VAL simplification ***/

static void
optimize_orig_val_resize (statement_t **stmt, gboolean *changed)
{
    while ((*stmt) != NULL)
    {
	switch ((*stmt)->kind)
	{
	    case STMT_NIL :
	    case STMT_PHI_ASSIGN :
		break;

	    case STMT_ASSIGN :
		if (compiler_stmt_is_assign_with_op(*stmt, OP_ORIG_VAL)
		    && compiler_stmt_op_assign_arg(*stmt, 2).kind == PRIMARY_VALUE)
		{
		    statement_t *def = compiler_stmt_op_assign_arg(*stmt, 2).v.value->def;

		    if (compiler_stmt_is_assign_with_op(def, OP_RESIZE_IMAGE))
		    {
			primary_t orig_x = compiler_stmt_op_assign_arg(*stmt, 0);
			primary_t orig_y = compiler_stmt_op_assign_arg(*stmt, 1);
			primary_t image = compiler_stmt_op_assign_arg(def, 0);
			primary_t x_factor = compiler_stmt_op_assign_arg(def, 1);
			primary_t y_factor = compiler_stmt_op_assign_arg(def, 2);
			compvar_t *new_x = compiler_make_temporary(TYPE_INT);
			compvar_t *new_y = compiler_make_temporary(TYPE_INT);

			stmt = compiler_emit_stmt_before(compiler_make_assign(compiler_make_lhs(new_x),
									      compiler_make_op_rhs(OP_MUL,
												   orig_x, x_factor)),
							 stmt, (*stmt)->parent);
			stmt = compiler_emit_stmt_before(compiler_make_assign(compiler_make_lhs(new_y),
									      compiler_make_op_rhs(OP_MUL,
												   orig_y, y_factor)),
							 stmt, (*stmt)->parent);

			g_assert(compiler_op_index((*stmt)->v.assign.rhs->v.op.op) == OP_ORIG_VAL);
			compiler_replace_op_rhs_arg(*stmt, 0, compiler_make_compvar_primary(new_x));
			compiler_replace_op_rhs_arg(*stmt, 1, compiler_make_compvar_primary(new_y));
			compiler_replace_op_rhs_arg(*stmt, 2, image);

			*changed = TRUE;
		    }
		}
		break;

	    case STMT_IF_COND :
		optimize_orig_val_resize(&(*stmt)->v.if_cond.consequent, changed);
		optimize_orig_val_resize(&(*stmt)->v.if_cond.alternative, changed);
		break;

	    case STMT_WHILE_LOOP :
		optimize_orig_val_resize(&(*stmt)->v.while_loop.body, changed);
		break;

	    default :
		g_assert_not_reached();
	}

	stmt = &(*stmt)->next;
    }
}

gboolean
compiler_opt_orig_val_resize (statement_t **first_stmt)
{
    gboolean changed = FALSE;

    optimize_orig_val_resize(first_stmt, &changed);

    return changed;
}

/*** STRIP_RESIZE simplification ***/

/* strip(image) -> image */
/* strip(closure) -> closure */
/* strip(resize(X)) -> X */

static void
optimize_strip_resize (statement_t **stmt, gboolean *changed)
{
    while ((*stmt) != NULL)
    {
	switch ((*stmt)->kind)
	{
	    case STMT_NIL :
	    case STMT_PHI_ASSIGN :
		break;

	    case STMT_ASSIGN :
		if (compiler_stmt_is_assign_with_op(*stmt, OP_STRIP_RESIZE)
		    && compiler_stmt_op_assign_arg(*stmt, 0).kind == PRIMARY_VALUE)
		{
		    statement_t *def = compiler_stmt_op_assign_arg(*stmt, 0).v.value->def;

		    if (compiler_stmt_is_assign_with_op(def, OP_RESIZE_IMAGE))
		    {
			primary_t image = compiler_stmt_op_assign_arg(def, 0);

			compiler_replace_rhs(&(*stmt)->v.assign.rhs, make_primary_rhs(image), *stmt);

			*changed = TRUE;
		    }
		    else if (compiler_stmt_is_assign_with_rhs(def, RHS_CLOSURE)
			     || compiler_stmt_is_assign_with_op(def, OP_USERVAL_IMAGE))
		    {
			compiler_replace_rhs(&(*stmt)->v.assign.rhs, make_value_rhs(def->v.assign.lhs), *stmt);

			*changed = TRUE;
		    }
		}
		break;

	    case STMT_IF_COND :
		optimize_strip_resize(&(*stmt)->v.if_cond.consequent, changed);
		optimize_strip_resize(&(*stmt)->v.if_cond.alternative, changed);
		break;

	    case STMT_WHILE_LOOP :
		optimize_strip_resize(&(*stmt)->v.while_loop.body, changed);
		break;

	    default :
		g_assert_not_reached ();
	}

	stmt = &(*stmt)->next;
    }
}

gboolean
compiler_opt_strip_resize (statement_t **first_stmt)
{
    gboolean changed = FALSE;

    optimize_strip_resize(first_stmt, &changed);

    return changed;
}
