/* -*- c -*- */

/*
 * builtins.h
 *
 * MathMap
 *
 * Copyright (C) 1997-2008 Mark Probst
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

#include "mmpools.h"
#include "tuples.h"
#include "color.h"

struct _mathmap_invocation_t;
struct _compvar_t;
struct _image_t;
struct _filter_t;

typedef void (*generator_function_t) (struct _filter_t*, struct _compvar_t***, int*, int*, struct _compvar_t**);

/* TEMPLATE builtins */
color_t get_orig_val_pixel (struct _mathmap_invocation_t *invocation, float x, float y, struct _image_t *image, int frame);
color_t get_orig_val_intersample_pixel (struct _mathmap_invocation_t *invocation, float x, float y, struct _image_t *image, int frame);

float* get_floatmap_pixel (struct _mathmap_invocation_t *invocation, struct _image_t *image, float x, float y, float frame);

struct _image_t* render_image (struct _mathmap_invocation_t *invocation, struct _image_t *image,
			       int width, int height, mathmap_pools_t *pools, int force);
/* END */

void init_builtins (void);

#endif
