/* -*- c -*- */

/*
 * mathmap_common.c
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

#include <math.h>
#include <assert.h>
#include <stdlib.h>

#include "internals.h"
#include "tags.h"
#include "jump.h"
#include "scanner.h"
#include "postfix.h"
#include "cgen.h"

#include "mathmap_common.h"

mathmap_t *the_mathmap = 0;

/* from parser.y */
int mmparse (void);

void
unload_mathmap (mathmap_t *mathmap)
{
#ifdef USE_CGEN
    if (mathmap->module_info != 0)
    {
	unload_c_code(mathmap->module_info);
	mathmap->module_info = 0;
    }
#endif
}

void
free_mathmap (mathmap_t *mathmap)
{
    if (mathmap->variables != 0)
	free_variables(mathmap->variables);
    if (mathmap->exprtree != 0)
	free_exprtree(mathmap->exprtree);
    if (mathmap->userval_infos != 0)
	free_userval_infos(mathmap->userval_infos);
    unload_mathmap(mathmap);

    free(mathmap);
}

void
free_invocation (mathmap_invocation_t *invocation)
{
    /* FIXME */
}

#define XY_INTERNAL_INDEX         0
#define RA_INTERNAL_INDEX         1

static void
init_internals (mathmap_t *mathmap)
{
    register_internal(&mathmap->internals, "xy", make_tuple_info(xy_tag_number, 2), 0);
    register_internal(&mathmap->internals, "ra", make_tuple_info(ra_tag_number, 2), 0);
    register_internal(&mathmap->internals, "t", make_tuple_info(nil_tag_number, 1), 1);
    register_internal(&mathmap->internals, "XY", make_tuple_info(xy_tag_number, 2), 1);
    register_internal(&mathmap->internals, "WH", make_tuple_info(xy_tag_number, 2), 1);
    register_internal(&mathmap->internals, "R", make_tuple_info(nil_tag_number, 1), 1);
    register_internal(&mathmap->internals, "frame", make_tuple_info(nil_tag_number, 1), 1);
}

int
check_mathmap (char *expression)
{
    static mathmap_t mathmap;

    the_mathmap = &mathmap;

    DO_JUMP_CODE {
	scanFromString(expression);
	mmparse();
	endScanningFromString();

	the_mathmap = 0;

	assert(mathmap.exprtree != 0);

	if (mathmap.exprtree->result.number != rgba_tag_number
	    || mathmap.exprtree->result.length != 4)
	{
	    free_exprtree(mathmap.exprtree);
	    mathmap.exprtree = 0;

	    sprintf(error_string, "The expression must have the result type rgba:4.");
	    JUMP(1);
	}
    } WITH_JUMP_HANDLER {
	the_mathmap = 0;
    } END_JUMP_HANDLER;

    if (mathmap.exprtree != 0)
    {
	free_exprtree(mathmap.exprtree);
	return 1;
    }
    return 0;
}

mathmap_t*
compile_mathmap (char *expression)
{
    static mathmap_t *mathmap;	/* this is static to avoid problems with longjmp.  */

    mathmap = (mathmap_t*)malloc(sizeof(mathmap_t));

    mathmap->variables = 0;
    mathmap->userval_infos = 0;
    mathmap->internals = 0;

    mathmap->exprtree = 0;

    init_internals(mathmap);

    the_mathmap = mathmap;

    DO_JUMP_CODE {
	scanFromString(expression);
	mmparse();
	endScanningFromString();

	the_mathmap = 0;

	assert(mathmap->exprtree != 0);

	if (mathmap->exprtree->result.number != rgba_tag_number
	    || mathmap->exprtree->result.length != 4)
	{
	    free_mathmap(mathmap);
	    mathmap = 0;

	    sprintf(error_string, "The expression must have the result type rgba:4.");
	    JUMP(1);
	}

#ifdef USE_CGEN
	mathmap->initfunc = gen_and_load_c_code(mathmap, &mathmap->module_info);
	assert(mathmap->initfunc != 0);
#else
	mathmap->expression = make_postfix(mathmap->exprtree, &mathmap->exprlen);
	output_postfix(mathmap->expression, mathmap->exprlen);
#endif
    } WITH_JUMP_HANDLER {
	the_mathmap = 0;

	free_mathmap(mathmap);
	mathmap = 0;
    } END_JUMP_HANDLER;

    return mathmap;
}

mathmap_invocation_t*
invoke_mathmap (mathmap_t *mathmap, mathmap_invocation_t *template, int img_width, int img_height)
{
    mathmap_invocation_t *invocation = (mathmap_invocation_t*)malloc(sizeof(mathmap_invocation_t));

    invocation->mathmap = mathmap;

#ifdef USE_CGEN
    invocation->mathfunc = 0;
#endif

    invocation->uservals = instantiate_uservals(mathmap->userval_infos);
    invocation->variables = instantiate_variables(mathmap->variables);
    invocation->internals = instantiate_internals(mathmap->internals);

    invocation->antialiasing = 0;
    invocation->supersampling = 0;

    invocation->output_bpp = 4;

    invocation->origin_x = invocation->origin_y = 0;

    invocation->img_width = img_width;
    invocation->img_height = img_height;

    invocation->image_W = img_width;
    invocation->image_H = img_height;

    invocation->middle_x = img_width / 2.0;
    invocation->middle_y = img_height / 2.0;

    if (invocation->middle_x > img_width - invocation->middle_x)
	invocation->image_X = invocation->middle_x;
    else
	invocation->image_X = img_width - invocation->middle_x;

    if (invocation->middle_y > img_height - invocation->middle_y)
	invocation->image_Y = invocation->middle_y;
    else
	invocation->image_Y = img_height - invocation->middle_y;
    
    invocation->image_R = hypot(invocation->image_X, invocation->image_Y);

    invocation->current_r = invocation->current_a = 0.0;

    if (template != 0)
	carry_over_uservals_from_template(invocation, template);

#ifdef USE_CGEN
    invocation->stack = (tuple_t*)malloc(sizeof(tuple_t));
#else
    invocation->stack = (tuple_t*)malloc(STACKSIZE * sizeof(tuple_t));
#endif

    return invocation;
}

void
init_invocation (mathmap_invocation_t *invocation)
{
#ifdef USE_CGEN
    invocation->mathfunc = invocation->mathmap->initfunc(invocation);
#endif
}

void
calc_ra (mathmap_invocation_t *invocation)
{
    if (invocation->mathmap->internals->next->is_used)
    {
	double x = invocation->current_x, y = invocation->current_y;

	invocation->current_r = hypot(x, y);
	if (invocation->current_r == 0.0)
	    invocation->current_a = 0.0;
	else
	    invocation->current_a = acos(x / invocation->current_r) * 180 / M_PI;

	if (y < 0)
	    invocation->current_a = 360 - invocation->current_a;
    }
}

void
update_image_internals (mathmap_invocation_t *invocation)
{
    internal_t *internal;

    internal = lookup_internal(invocation->mathmap->internals, "t", 0);
    invocation->internals[internal->index].data[0] = invocation->current_t;

    internal = lookup_internal(invocation->mathmap->internals, "XY", 0);
    invocation->internals[internal->index].data[0] = invocation->image_X;
    invocation->internals[internal->index].data[1] = invocation->image_Y;
    
    internal = lookup_internal(invocation->mathmap->internals, "WH", 0);
    invocation->internals[internal->index].data[0] = invocation->image_W;
    invocation->internals[internal->index].data[1] = invocation->image_H;
    
    internal = lookup_internal(invocation->mathmap->internals, "R", 0);
    invocation->internals[internal->index].data[0] = invocation->image_R;

    internal = lookup_internal(invocation->mathmap->internals, "frame", 0);
    invocation->internals[internal->index].data[0] = invocation->current_frame;
}

void
update_pixel_internals (mathmap_invocation_t *invocation)
{
    invocation->internals[XY_INTERNAL_INDEX].data[0] = invocation->current_x;
    invocation->internals[XY_INTERNAL_INDEX].data[1] = invocation->current_y;

    invocation->internals[RA_INTERNAL_INDEX].data[0] = invocation->current_r;
    invocation->internals[RA_INTERNAL_INDEX].data[1] = invocation->current_a;
}

void
write_tuple_to_pixel (tuple_t *tuple, guchar *dest, int output_bpp)
{
    float redf,
	greenf,
	bluef,
	alphaf;

    tuple_to_color(tuple, &redf, &greenf, &bluef, &alphaf);

    if (output_bpp == 1 || output_bpp == 2)
	dest[0] = (0.299 * redf + 0.587 * greenf + 0.114 * bluef) * 255;
    else if (output_bpp == 3 || output_bpp == 4)
    {
	dest[0] = redf * 255;
	dest[1] = greenf * 255;
	dest[2] = bluef * 255;
    }
    else
	assert(0);

    if (output_bpp == 2 || output_bpp == 4)
	dest[output_bpp - 1] = alphaf * 255;
}

void
carry_over_uservals_from_template (mathmap_invocation_t *invocation, mathmap_invocation_t *template)
{
    userval_info_t *info;

    for (info = invocation->mathmap->userval_infos; info != 0; info = info->next)
    {
	userval_info_t *template_info = lookup_matching_userval(template->mathmap->userval_infos, info);

	if (template_info != 0)
	    copy_userval(&invocation->uservals[info->index], &template->uservals[template_info->index], info->type);
    }
}
