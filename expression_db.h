/* -*- c -*- */

/*
 * expression_db.h
 *
 * MathMap
 *
 * Copyright (C) 2007-2008 Mark Probst
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

#define EXPRESSION_DB_EXPRESSION	1
#define EXPRESSION_DB_COMPOSITION	2
#define EXPRESSION_DB_GROUP		3

struct _mathmap_t;

typedef struct _expression_db_t
{
    char *name;
    int kind;
    union
    {
	struct
	{
	    char *path;
	    struct _mathmap_t *mathmap;
	} expression;
	struct
	{
	    char *path;
	} composition;
	struct
	{
	    struct _expression_db_t *subs;
	} group;
    } v;
    struct _expression_db_t *next;
} expression_db_t;

extern expression_db_t* read_expression_db (char *path);
extern void free_expression_db (expression_db_t *edb);

extern char* read_expression (const char *path);

extern expression_db_t* merge_expression_dbs (expression_db_t *edb1, expression_db_t *edb2);

extern char* get_expression_name (expression_db_t *expr);
extern userval_info_t* get_expression_args (expression_db_t *expr);

#endif
