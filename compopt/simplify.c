/*
 * simplify.c
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

static gboolean
simplify_closure_pixel_size (filter_t *compiled_filter, statement_t *stmt, statement_t *closure)
{
    rhs_t *closure_rhs = closure->v.assign.rhs;
    filter_t *filter = closure_rhs->v.closure.filter;
    int i;
    userval_info_t *info;
    internal_t *internal;

    for (i = 0, info = filter->userval_infos;
	 i < filter->num_uservals;
	 ++i, info = info->next)
    {
	if (info->type == USERVAL_IMAGE)
	{
	    compiler_replace_op_rhs_arg(stmt, 0, closure_rhs->v.closure.args[i]);
	    return TRUE;
	}
    }
    g_assert(info == NULL);

    if (compiler_op_index(stmt->v.assign.rhs->v.op.op) == OP_IMAGE_PIXEL_WIDTH)
	internal = lookup_internal(compiled_filter->v.mathmap.internals, "__canvasPixelW", TRUE);
    else
    {
	g_assert(compiler_op_index(stmt->v.assign.rhs->v.op.op) == OP_IMAGE_PIXEL_HEIGHT);
	internal = lookup_internal(compiled_filter->v.mathmap.internals, "__canvasPixelH", TRUE);
    }
    g_assert(internal != NULL);

    compiler_replace_rhs(&stmt->v.assign.rhs, compiler_make_internal_rhs(internal), stmt);

    return TRUE;
}

#include "simplify_func.c"

gboolean
compiler_opt_simplify (filter_t *filter, statement_t *first_stmt)
{
    gboolean changed = FALSE;

    recur(filter, first_stmt, &changed);

    return changed;
}
