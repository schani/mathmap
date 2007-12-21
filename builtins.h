/* -*- c -*- */

/*
 * builtins.h
 *
 * MathMap
 *
 * Copyright (C) 1997-2007 Mark Probst
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

#ifndef __BUILTINS_H__
#define __BUILTINS_H__

#include <stdio.h>

#include "tuples.h"
#include "color.h"

struct _mathmap_invocation_t;
struct _compvar_t;

#define MAX_BUILTIN_LENGTH     63

typedef void (*generator_function_t) (struct _compvar_t***, int*, int*, struct _compvar_t**);

color_t get_orig_val_pixel (struct _mathmap_invocation_t *invocation, float x, float y, int drawable_index, int frame);
color_t get_orig_val_intersample_pixel (struct _mathmap_invocation_t *invocation, float x, float y, int drawable_index, int frame);

void init_builtins (void);

#endif
