/* -*- c -*- */

/*
 * mathmap_common.h
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

#ifndef __MATHMAP_COMMON_H__
#define __MATHMAP_COMMON_H__

#include "gtypes.h"

#include "tuples.h"
#include "mathmap.h"

#define MATHMAP_DATE          "March 2002"
#define MATHMAP_VERSION       "0.14"

void calc_ra (mathmap_invocation_t *invocation);
void update_image_internals (mathmap_invocation_t *invocation);
void update_pixel_internals (mathmap_invocation_t *invocation);

void write_tuple_to_pixel (tuple_t *tuple, guchar *dest, int output_bpp);

void carry_over_uservals_from_template (mathmap_invocation_t *invocation, mathmap_invocation_t *template);

extern int edge_behaviour_color, edge_behaviour_wrap, edge_behaviour_reflect;
extern int edge_behaviour_mode;

#endif
