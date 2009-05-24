/* -*- c -*- */

/*
 * native-filters.h
 *
 * MathMap
 *
 * Copyright (C) 2008-2009 Mark Probst
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

#ifndef __NATIVE_FILTERS_H__
#define __NATIVE_FILTERS_H__

#include "../mathmap.h"

/* TEMPLATE native_filters */
extern image_t* native_filter_gaussian_blur (mathmap_invocation_t *invocation, userval_t *args, pools_t *pools);

extern image_t* native_filter_convolve (mathmap_invocation_t *invocation, userval_t *args, pools_t *pools);
extern image_t* native_filter_half_convolve (mathmap_invocation_t *invocation, userval_t *args, pools_t *pools);
extern image_t* native_filter_visualize_fft (mathmap_invocation_t *invocation, userval_t *args, pools_t *pools);
/* END */

#endif
