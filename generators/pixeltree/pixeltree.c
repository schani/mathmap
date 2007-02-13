/*
 * pixeltree.c
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
#include <assert.h>

#include "../../jump.h"
#include "../../mathmap.h"
#include "../../cgen.h"

static int
template_processor (mathmap_t *mathmap, const char *directive, FILE *out)
{
    static struct { int type; char *const_string; char *variant_name; char *c_type_name; } types[] = 
	{
	    { ARG_TYPE_INT , "INTEGER", "integer", "int" },
	    { ARG_TYPE_FLOAT, "REAL", "real", "float" },
	    { ARG_TYPE_BOOL, "BOOL", "boolean", "int" } ,
	    { ARG_TYPE_COLOR, "COLOR", "color", "pt_pixel_t*" },
	    { ARG_TYPE_GRADIENT, "GRADIENT", "gradient", 0 },
	    { ARG_TYPE_CURVE, "CURVE", "curve", 0 },
	    { ARG_TYPE_IMAGE, "NODE", "node", "pt_node_t*" },
	    { 0, 0 }
	};

    assert(mathmap->top_level_decls != 0
	   && mathmap->top_level_decls->type == TOP_LEVEL_FILTER);

    if (strcmp(directive, "type_inputs") == 0)
    {
	arg_decl_t *decl;
	int i;

	for (i = 0, decl = mathmap->top_level_decls->v.filter.args;
	     decl != 0;
	     ++i, decl = decl->next)
	{
	    int j;

	    for (j = 0; types[j].const_string != 0; ++j)
		if (types[j].type == decl->type)
		    break;
	    assert(types[j].const_string != 0);

	    // FIXME: documentation should be 0, not "", if docstring is 0
	    fprintf(out,
		    "type->input_infos[%d] = (pt_input_info_t) { .name = \"%s\", .documentation = \"%s\", "
		    ".type = PT_INPUT_TYPE_%s, ",
		   i, decl->name, (decl->docstring == 0) ? "" : decl->docstring, types[j].const_string);

	    if (decl->type == ARG_TYPE_INT || decl->type == ARG_TYPE_FLOAT)
	    {
		int have_limits =
		    (decl->type == ARG_TYPE_INT)
		    ? decl->v.integer.have_limits
		    : decl->v.floating.have_limits;

		fprintf(out,
			".v.%s.has_limit = %d, ",
			types[j].variant_name, have_limits);

		if (have_limits)
		{
		    if (decl->type == ARG_TYPE_INT)
			fprintf(out, ".v.integer.min = %d, .v.integer.max = %d, .default_value.integer = %d, ",
				decl->v.integer.min, decl->v.integer.max, decl->v.integer.default_value);
		    else
			fprintf(out, ".v.real.min = %f, .v.real.max = %f, .default_value.real = %f, ",
				decl->v.floating.min, decl->v.floating.max, decl->v.floating.default_value);
		}

	    }

	    fprintf(out, "};\n");
	}
    }
    else if (strcmp(directive, "make_node_args") == 0)
    {
	arg_decl_t *decl;
	int i;

	for (i = 0, decl = mathmap->top_level_decls->v.filter.args;
	     decl != 0;
	     ++i, decl = decl->next)
	{
	    int j;

	    for (j = 0; types[j].const_string != 0; ++j)
		if (types[j].type == decl->type)
		    break;
	    assert(types[j].const_string != 0);

	    fprintf(out, "%s arg%d%s", types[j].c_type_name, i, (decl->next == 0) ? "" : ", ");
	}
    }
    else if (strcmp(directive, "make_node_values") == 0)
    {
	arg_decl_t *decl;
	int i;

	for (i = 0, decl = mathmap->top_level_decls->v.filter.args;
	     decl != 0;
	     ++i, decl = decl->next)
	{
	    int j;

	    for (j = 0; types[j].const_string != 0; ++j)
		if (types[j].type == decl->type)
		    break;
	    assert(types[j].const_string != 0);

	    fprintf(out, "{ .%s = arg%d }, ", types[j].variant_name, i);
	}
    }
    else
	return compiler_template_processor(mathmap, directive, out);
    return 1;
}

int
pixeltree_generate_plug_in (char *filter, char *output_filename)
{
    return generate_plug_in(filter, output_filename,
			    "pixeltree_template.c", "pixeltree_opmacros.h", 0, template_processor);
}
