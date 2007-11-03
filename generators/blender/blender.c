/*
 * blender.c
 *
 * MathMap
 *
 * Copyright (C) 2005 Mark Probst
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

#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "../../jump.h"
#include "../../mathmap.h"

#include "blender.h"

static userval_info_t*
find_nth_image_userval (userval_info_t *infos, int n)
{
    userval_info_t *info;

    for (info = infos; info != 0; info = info->next)
	if (info->type == USERVAL_IMAGE)
	    if (n-- == 0)
		return info;

    return 0;
}

static int
template_processor (mathmap_t *mathmap, const char *directive, FILE *out)
{
    if (strcmp(directive, "var_structs") == 0)
    {
	userval_info_t *info;

	for (info = mathmap->main_filter->userval_infos; info != 0; info = info->next)
	{
	    switch (info->type)
	    {
		case USERVAL_INT_CONST :
		    fprintf(out, "NUMSLI|INT, \"%s: \", %d, %d, %d, \"%s\",\n",
			    info->name,
			    info->v.int_const.min, info->v.int_const.min, info->v.int_const.max,
			    info->name);
		    break;

		case USERVAL_FLOAT_CONST :
		    fprintf(out, "NUMSLI|FLO, \"%s: \", %f, %f, %f, \"%s\",\n",
			    info->name,
			    info->v.float_const.min, info->v.float_const.min, info->v.float_const.max,
			    info->name);
		    break;

		case USERVAL_BOOL_CONST :
		    fprintf(out, "TOG|INT, \"%s: \", 0, 0, 1, \"%s\",\n",
			    info->name, info->name);
		    break;

		case USERVAL_IMAGE :
		    break;

		default :
		    fprintf(stderr, "Error: type of user value `%s' is not supported in Blender.\n", info->name);
		    exit(1);
	    }

	}
    }
    else if (strcmp(directive, "userval_decls") == 0)
    {
	static struct { int type; const char *c_type; } userval_types[] =
	    {
		{ USERVAL_INT_CONST, "int" },
		{ USERVAL_FLOAT_CONST, "float" },
		{ USERVAL_BOOL_CONST, "int" },
		{ -1, 0 }
	    };

	userval_info_t *info;

	for (info = mathmap->main_filter->userval_infos; info != 0; info = info->next)
	{
	    if (info->type != USERVAL_IMAGE)
	    {
		int i;

		for (i = 0; userval_types[i].c_type != 0; ++i)
		    if (userval_types[i].type == info->type)
			break;

		if (userval_types[i].c_type == 0)
		{
		    fprintf(stderr, "Error: type of user value `%s' is not supported in Blender.\n", info->name);
		    exit(1);
		}

		fprintf(out, "%s userval_%d;\n", userval_types[i].c_type, info->index);
	    }
	}
    }
    else if (strcmp(directive, "num_input_drawables") == 0)
    {
	userval_info_t *info;
	int num = 0;

	for (info = mathmap->main_filter->userval_infos; info != 0; info = info->next)
	    if (info->type == USERVAL_IMAGE)
		++num;

	fprintf(out, "%d", num);
    }
    else if (strcmp(directive, "drawable0") == 0)
    {
	userval_info_t *info = find_nth_image_userval(mathmap->main_filter->userval_infos, 0);

	fprintf(out, "%d", (info == 0) ? -1 : info->index);
    }
    else if (strcmp(directive, "drawable1") == 0)
    {
	userval_info_t *info = find_nth_image_userval(mathmap->main_filter->userval_infos, 1);

	fprintf(out, "%d", (info == 0) ? -1 : info->index);
    }
    else if (strcmp(directive, "drawable2") == 0)
    {
	userval_info_t *info = find_nth_image_userval(mathmap->main_filter->userval_infos, 2);

	fprintf(out, "%d", (info == 0) ? -1 : info->index);
    }
    else
	return compiler_template_processor(mathmap, directive, out);
    return 1;
}

int
blender_generate_plug_in (char *filter, char *output_filename)
{
    return generate_plug_in(filter, output_filename,
			    "blender_template.c", "blender_opmacros.h", 1, template_processor);
}
