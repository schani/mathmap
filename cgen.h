/*
 * cgen.h
 *
 * MathMap
 *
 * Copyright (C) 1997-2004 Mark Probst
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

typedef void (*init_frame_func_t) (struct _mathmap_invocation_t*);
typedef void (*calc_lines_func_t) (struct _mathmap_invocation_t*, int, int, unsigned char*);

typedef struct
{
    init_frame_func_t init_frame;
    calc_lines_func_t calc_lines;
} mathfuncs_t;

typedef mathfuncs_t (*initfunc_t) (struct _mathmap_invocation_t*);

void init_compiler (void);

void generate_ir_code (struct _mathmap_t *mathmap);
void forget_ir_code (struct _mathmap_t *mathmap);

void set_opmacros_filename (const char *filename);
int compiler_template_processor (struct _mathmap_t *mathmap, const char *directive, FILE *out);

initfunc_t gen_and_load_c_code (struct _mathmap_t *mathmap, void **module_info, FILE *template, char *opmacros_filename);
void unload_c_code (void *module_info);

#endif
