/* -*- c -*- */

/*
 * macros.h
 *
 * MathMap
 *
 * Copyright (C) 1997-2000 Mark Probst
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

#ifndef __MACROS_H__
#define __MACROS_H__

#include "tuples.h"

#define MAX_MACRO_LENGTH     63

struct _exprtree;

typedef struct _exprtree* (*macro_function_t) (struct _exprtree *args);

typedef struct _macro_t
{
    char name[MAX_MACRO_LENGTH + 1];
    tuple_info_t info;

    macro_function_t function;

    struct _macro_t *next;
} var_macro_t;

macro_function_t lookup_variable_macro (const char *name, tuple_info_t *info);

void init_macros (void);

#endif
