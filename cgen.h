/*
 * cgen.h
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

#ifndef __CGEN_H__
#define __CGEN_H__

#include "gtypes.h"

#include "tuples.h"
#include "exprtree.h"

struct _mathmap_t;
struct _mathmap_invocation_t;

typedef tuple_t* (*mathfunc_t) (void);
typedef mathfunc_t (*initfunc_t) (struct _mathmap_invocation_t*);

initfunc_t gen_and_load_c_code (struct _mathmap_t *mathmap, void **module_info);
void unload_c_code (void *module_info);

#endif
