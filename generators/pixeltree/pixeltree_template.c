/*
 * pixeltree_template.c
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

#include <math.h>
#include <complex.h>

#include "/nethome/hansolo/schani/Work/unix/pixeltree/pixeltree.h"
#include "/nethome/hansolo/schani/Work/unix/mathmap/mathmap/generators/pixeltree/pixeltree_opmacros.h"

#define X 1.0
#define Y 1.0
#define W 2.0
#define H 2.0
#define R 1.41421356237309504880

typedef pt_pixel_t color_t;

static void
pixel_func (pt_node_t *node, pt_coords_t *coords, pt_pixel_t *result)
{
    float x = coords->v[0], y = coords->v[1];
#if $uses_t
    float t = 0.0;
#endif

#if $uses_ra
    float r, a;

    r = hypot(x, y);
    if (r == 0.0)
	a = 0.0;
    else
	a = acos(x / r);

    if (y < 0)
	a = 2 * M_PI - a;
#endif

    {
	$m
    }
}

void
register_filter_$filter_name (void)
{
    pt_node_type_t *type;

    type = pt_alloc_node_type($num_uservals);
    type->name = "$filter_name";
    type->documentation = "$filter_docstring";
    type->pixel_func = pixel_func;
    $type_inputs

    pt_register_node_type(type);
}

pt_node_t*
new_$filter_name$_node ($make_node_args)
{
    pt_input_value_t input[$num_uservals] = { $make_node_values };

    return pt_new_node(pt_node_type_with_name("$filter_name"), input, 0);
}
