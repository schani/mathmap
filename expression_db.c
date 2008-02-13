/* -*- c -*- */

/*
 * expression_db.c
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

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include <glib.h>

#include "expression_db.h"
#include "mathmap.h"

static expression_db_t*
new_expression_db (int kind)
{
    expression_db_t *edb = (expression_db_t*)malloc(sizeof(expression_db_t));

    assert(edb != 0);

    memset(edb, 0, sizeof(expression_db_t));

    edb->kind = kind;

    return edb;
}

static expression_db_t*
read_expression_sub_db (char *path, char *name)
{
    int name_len = strlen(name);
    char filename[strlen(path) + name_len + 2];
    struct stat buf;

    sprintf(filename, "%s/%s", path, name);

    if (stat(filename, &buf) == -1)
    {
	fprintf(stderr, "Error accessing `%s': %m\n", filename);
	return 0;
    }

    if (S_ISREG(buf.st_mode))
    {
	// does it have extension .mm?
	if (g_str_has_suffix(name, ".mm"))
	{
	    expression_db_t *edb = new_expression_db(EXPRESSION_DB_EXPRESSION);

	    edb->name = g_strndup(name, name_len - 3);
	    edb->v.expression.path = g_strdup(filename);

	    return edb;
	}
	else if (g_str_has_suffix(name, ".mmc"))
	{
	    expression_db_t *edb = new_expression_db(EXPRESSION_DB_DESIGN);

	    edb->name = g_strndup(name, name_len - 4);
	    edb->v.design.path = g_strdup(filename);

	    return edb;
	}
    }
    else if (S_ISDIR(buf.st_mode))
    {
	expression_db_t *subs = read_expression_db(filename);

	if (subs != 0)
	{
	    expression_db_t *edb = new_expression_db(EXPRESSION_DB_GROUP);

	    edb->name = strdup(name);
	    assert(edb->name != 0);

	    edb->v.group.subs = subs;

	    return edb;
	}
    }

    return 0;
}

static expression_db_t*
insert_expression_db (expression_db_t *edb, expression_db_t *sub)
{
    assert(sub != 0 && sub->next == 0);

    if (edb == 0)
	return sub;

    if (strcmp(sub->name, edb->name) < 0)
    {
	sub->next = edb;
	return sub;
    }
    else
    {
	edb->next = insert_expression_db(edb->next, sub);
	return edb;
    }
}

static expression_db_t*
remove_expression_db (expression_db_t *edb, expression_db_t *e)
{
    assert(edb != 0 && e != 0);

    if (edb == e)
    {
	expression_db_t *next = edb->next;

	edb->next = 0;

	free_expression_db(edb);

	return next;
    }
    else
    {
	edb->next = remove_expression_db(edb->next, e);
	return edb;
    }
}

expression_db_t*
read_expression_db (char *path)
{
    DIR *dir;
    struct dirent *dirent;
    expression_db_t *edb = 0;

    //fprintf(stderr, "checking out %s\n", path);

    dir = opendir(path);

    if (dir == 0)
    {
	fprintf(stderr, "Cannot open directory `%s': %m\n", path);
	return 0;
    }

    for (;;)
    {
	dirent = readdir(dir);

	if (dirent != 0 && dirent->d_name[0] != '.')
	{
	    char *name = strdup(dirent->d_name);
	    expression_db_t *subs;

	    //fprintf(stderr, "sub %s\n", name);

	    assert(name != 0);

	    subs = read_expression_sub_db(path, name);

	    free(name);

	    if (subs != 0)
		edb = insert_expression_db(edb, subs);
	}
	else if (dirent == 0)
	    break;
    }

    closedir(dir);

    return edb;
}

void
free_expression_db (expression_db_t *edb)
{
    while (edb != 0)
    {
	expression_db_t *next = edb->next;

	free(edb->name);

	switch (edb->kind)
	{
	    case EXPRESSION_DB_EXPRESSION :
		free(edb->v.expression.path);
		if (edb->v.expression.mathmap != NULL)
		    free_mathmap(edb->v.expression.mathmap);
		break;

	    case EXPRESSION_DB_DESIGN :
		free(edb->v.design.path);
		break;

	    case EXPRESSION_DB_GROUP :
		free_expression_db(edb->v.group.subs);
		break;

	    default :
		g_assert_not_reached();
	}

	edb = next;
    }
}

static expression_db_t*
lookup (expression_db_t *edb, char *name, int kind)
{
    while (edb != 0)
    {
	if (edb->kind == kind && strcmp(edb->name, name) == 0)
	    return edb;
	edb = edb->next;
    }

    return 0;
}

static expression_db_t*
copy_expression (expression_db_t *edb)
{
    expression_db_t *copy;

    copy = new_expression_db(edb->kind);

    copy->name = g_strdup(edb->name);

    switch (edb->kind)
    {
	case EXPRESSION_DB_EXPRESSION :
	    copy->v.expression.path = g_strdup(edb->v.expression.path);
	    break;

	case EXPRESSION_DB_DESIGN :
	    copy->v.design.path = g_strdup(edb->v.design.path);
	    break;

	default :
	    g_assert_not_reached();
    }

    return copy;
}

expression_db_t*
merge_expression_dbs (expression_db_t *edb1, expression_db_t *edb2)
{
    while (edb2 != 0)
    {
	expression_db_t *e = lookup(edb1, edb2->name, edb2->kind);

	if (e != 0)
	{
	    switch (edb2->kind)
	    {
		case EXPRESSION_DB_EXPRESSION :
		case EXPRESSION_DB_DESIGN :
		    edb1 = remove_expression_db(edb1, e);
		    edb1 = insert_expression_db(edb1, copy_expression(edb2));
		    break;

		case EXPRESSION_DB_GROUP :
		    e->v.group.subs = merge_expression_dbs(e->v.group.subs, edb2->v.group.subs);
		    break;

		default :
		    g_assert_not_reached();
		    break;
	    }
	}
	else
	{
	    switch (edb2->kind)
	    {
		case EXPRESSION_DB_EXPRESSION :
		case EXPRESSION_DB_DESIGN :
		    edb1 = insert_expression_db(edb1, copy_expression(edb2));
		    break;

		case EXPRESSION_DB_GROUP :
		    e = new_expression_db(EXPRESSION_DB_GROUP);

		    e->name = strdup(edb2->name);
		    assert(e->name != 0);

		    edb1 = insert_expression_db(edb1, e);
		    e->v.group.subs = merge_expression_dbs(0, edb2->v.group.subs);
		    break;

		default :
		    g_assert_not_reached();
		    break;
	    }
	}

	edb2 = edb2->next;
    }

    return edb1;
}

char*
read_expression (const char *path)
{
    char *expr;

    if (!g_file_get_contents(path, &expr, NULL, NULL))
    {
	fprintf(stderr, "Cannot read file `%s': %m\n", path);
	return NULL;
    }

    return expr;
}

static mathmap_t*
fetch_expression_mathmap (expression_db_t *expr, designer_design_type_t *design_type)
{
    switch (expr->kind)
    {
	case EXPRESSION_DB_EXPRESSION :
	    if (expr->v.expression.mathmap == NULL)
	    {
		char *source = read_expression(expr->v.expression.path);

		if (source == NULL)
		    return NULL;

		expr->v.expression.mathmap = parse_mathmap(source);

		g_free(source);
	    }
	    return expr->v.expression.mathmap;

	case EXPRESSION_DB_DESIGN :
	    if (expr->v.design.mathmap == NULL)
	    {
		designer_design_t *design = designer_load_design(design_type, expr->v.design.path,
								 NULL, NULL, NULL, NULL);
		char *source;

		g_assert(design != NULL);

		if (design->root == NULL)
		{
		    designer_free_design(design);
		    return NULL;
		}

		source = make_filter_source_from_design(design, NULL);

		expr->v.design.mathmap = parse_mathmap(source);

		g_free(source);
		designer_free_design(design);
	    }
	    return expr->v.design.mathmap;

	default :
	    g_assert_not_reached();
    }
}

char*
get_expression_name (expression_db_t *expr, designer_design_type_t *design_type)
{
    mathmap_t *mathmap = fetch_expression_mathmap(expr, design_type);

    if (mathmap == NULL)
	return NULL;
    return mathmap->main_filter->decl->name;
}

userval_info_t*
get_expression_args (expression_db_t *expr, designer_design_type_t *design_type)
{
    mathmap_t *mathmap = fetch_expression_mathmap(expr, design_type);

    if (mathmap == NULL)
	return NULL;
    return mathmap->main_filter->userval_infos;
}

char*
get_expression_path (expression_db_t *expr)
{
    switch (expr->kind)
    {
	case EXPRESSION_DB_EXPRESSION :
	    return expr->v.expression.path;

	case EXPRESSION_DB_DESIGN :
	    return expr->v.design.path;

	default :
	    g_assert_not_reached();
    }
}
