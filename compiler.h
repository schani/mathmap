/*
 * compiler.h
 *
 * MathMap
 *
 * Copyright (C) 1997-2009 Mark Probst
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

#ifndef __COMPILER_H__
#define __COMPILER_H__

#include "glib.h"

#include "tuples.h"
#include "exprtree.h"
#include "userval.h"
#include "drawable.h"
#include "tree_vectors.h"

#include "compiler_types.h"
#include "opmacros.h"

struct _mathmap_t;
struct _mathmap_invocation_t;
struct _mathmap_frame_t;
struct _mathmap_slice_t;

typedef union
{
    RUNTIME_VALUE_DECL
} runtime_value_t;

/* TEMPLATE mathfuncs */
/* All the functions required to render an image efficiently */
typedef struct _mathfuncs_t
{
    init_frame_func_t init_frame;
    init_slice_func_t init_slice;
    calc_lines_func_t calc_lines;

    /* FIXME: only used for LLVM - remove eventually */
    llvm_init_frame_func_t llvm_init_frame_func;
    llvm_filter_func_t main_filter_func;
    init_x_or_y_func_t init_x_func;
    init_x_or_y_func_t init_y_func;
} mathfuncs_t;
/* END */

typedef mathfuncs_t (*initfunc_t) (struct _mathmap_invocation_t*);

#define DEFAULT_OPTIMIZATION_TIMEOUT	2

#define MAX_OP_ARGS          9

struct _filter_code_t;

void init_compiler (void);

void set_opmacros_filename (const char *filename);
int compiler_template_processor (struct _mathmap_t *mathmap, const char *directive, const char *arg, FILE *out, void *data);

initfunc_t gen_and_load_c_code (struct _mathmap_t *mathmap, void **module_info,
				char *template_filename, char *include_path,
				struct _filter_code_t **filter_codes);
void unload_c_code (void *module_info);

void gen_and_load_llvm_code (struct _mathmap_t *mathmap, char *template_filename,
			     struct _filter_code_t **filter_codes);
void unload_llvm_code (struct _mathmap_t *mathmap);

#endif
