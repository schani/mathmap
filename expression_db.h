/* -*- c -*- */

/*
 * expression_db.h
 *
 * MathMap
 *
 * Copyright (C) 2007-2010 Mark Probst
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

#ifndef __EXPRESSION_DB_H__
#define __EXPRESSION_DB_H__

#include "userval.h"
#include "designer/designer.h"

#define EXPRESSION_DB_EXPRESSION	1
#define EXPRESSION_DB_DESIGN		2

#define EXPRESSION_ORIGIN_COMMUNITY	1
#define EXPRESSION_ORIGIN_LOCAL	2

struct _mathmap_t;

typedef struct _expression_metadata_t {
    char *name_space;
    char *name;
    char *title;
    GList *tags;
} expression_metadata_t;

typedef struct _expression_db_t
{
    int origin;
    int kind;
    expression_metadata_t meta;
    union
    {
	struct
	{
	    char *path;
	    char *docstring;
	    struct _mathmap_t *mathmap;
	} expression;
	struct
	{
	    char *path;
	    struct _mathmap_t *mathmap;
	} design;
    } v;

    struct _expression_db_t *next;
} expression_db_t;

extern void free_expression_metadata_members (expression_metadata_t *meta);

extern expression_db_t* extend_expression_db (expression_db_t *edb, char *path, int origin);
extern void free_expression_db (expression_db_t *edb);

extern expression_db_t* copy_expression_db (expression_db_t *edb);

extern char* read_expression (const char *path);
extern void save_expression_to_dir(expression_db_t *expr, char *dir);

extern char* get_expression_docstring (expression_db_t *edb);

extern char* get_expression_name_space (expression_db_t *expr);
extern char* get_expression_name (expression_db_t *expr);
// result string must be freed by the caller
extern char* get_expression_full_name (expression_db_t *expr);

extern userval_info_t* get_expression_args (expression_db_t *expr, designer_design_type_t *design_type);
extern char* get_expression_path (expression_db_t *expr);

// result string must be freed by the caller
extern char *get_expression_symbol(expression_db_t *expr);


#endif
