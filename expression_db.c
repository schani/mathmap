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

#include "expression_panel.h"

#define SYMBOL_PREFIX "mathmap"

static mathmap_t*
fetch_expression_mathmap (expression_db_t *expr, designer_design_type_t *design_type);

static expression_db_t*
remove_expression_db (expression_db_t *edb, expression_db_t *e);

static char *
generate_expression_symbol(expression_db_t *expr);

static expression_db_t*
new_expression_db (int kind)
{
    expression_db_t *edb = (expression_db_t*)malloc(sizeof(expression_db_t));

    g_assert(edb != 0);

    memset(edb, 0, sizeof(expression_db_t));

    edb->kind = kind;

    return edb;
}

// assume various expression fields if missing
static void fix_expression(expression_db_t *expr) {
    static GRegex *rex = NULL;

    if (expr->meta && expr->meta->title) {
	if (expr->name)
	    g_free(expr->name);
	expr->name = g_strdup(expr->meta->title);
    } else {
	char *path = get_expression_path(expr);
	if (path) {
	    GMatchInfo *matches;
	    if (! rex) {
		rex = g_regex_new("([^/]+)/([^/.]+)\\.[^.]+$", 0, 0, NULL);
	    }
	    if (g_regex_match(rex, path, 0, &matches)) {
		expr->name = g_match_info_fetch(matches, 2);
	    }
	    g_match_info_free(matches);
	} else {
	    expr->meta->title = g_strdup("untitled");
	}
    }
    expr->symbol = generate_expression_symbol(expr);
}

// should be called only on regular files
static expression_db_t *
get_expression_from_file(char *filename) {
    // does it have extension .mm?
    if (g_str_has_suffix(filename, ".mm"))
    {
	expression_db_t *expr = new_expression_db(EXPRESSION_DB_EXPRESSION);

	expr->v.expression.path = g_strdup(filename);
	// Need this immediately to ensure that filter metadata is loaded
	// Maybe the metadata parsing code should be moved to parse_mathmap() function instead
	(void)fetch_expression_mathmap(expr, NULL);

	fix_expression(expr);

	return expr;
    }
    else if (g_str_has_suffix(filename, ".mmc"))
    {
	expression_db_t *expr = new_expression_db(EXPRESSION_DB_DESIGN);

	expr->v.design.path = g_strdup(filename);

	fix_expression(expr);

	return expr;
    } else {
	return NULL;
    }
}

expression_db_t *
extend_expression_db(expression_db_t *edb, char *path, int origin)
{
    struct stat buf;

    if (stat(path, &buf) == -1)
    {
	fprintf(stderr, "Error accessing `%s': %m\n", path);
	return edb;
    }

    if (S_ISREG(buf.st_mode))
    {
	expression_db_t *expr = get_expression_from_file(path);
	if (expr) {
	    expr->origin = origin;
	    expr->next = edb;
	    edb = expr;
	}
    }
    else if (S_ISDIR(buf.st_mode)) // recursing into directory
    {
	DIR *dir;
	struct dirent *dirent;

	dir = opendir(path);

	if (dir == 0)
	{
	    fprintf(stderr, "Cannot open directory `%s': %m\n", path);
	    return edb;
	}

	for (;;)
	{
	    dirent = readdir(dir);

	    if (dirent != 0 && dirent->d_name[0] != '.')
	    {
		char *name = dirent->d_name;
		char subpath[strlen(path) + strlen(name) + 2];
		sprintf(subpath, "%s/%s", path, name);
		edb = extend_expression_db(edb, subpath, origin);
	    }
	    else if (dirent == 0)
		break;
	}
	closedir(dir);
    }

    return edb;
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

void
free_expression_db (expression_db_t *edb)
{
    while (edb != 0)
    {
	expression_db_t *next = edb->next;

	free(edb->name);
	if (edb->symbol)
	    g_free(edb->symbol);

	switch (edb->kind)
	{
	    case EXPRESSION_DB_EXPRESSION :
		free(edb->v.expression.path);
		if (edb->v.expression.docstring != NULL)
		    free(edb->v.expression.docstring);
		if (edb->v.expression.mathmap != NULL)
		    free_mathmap(edb->v.expression.mathmap);
		break;

	    case EXPRESSION_DB_DESIGN :
		free(edb->v.design.path);
		break;

	    default :
		g_assert_not_reached();
	}
	if (edb->meta)
	    expression_metadata_free(edb->meta);

	edb = next;
    }
}

static expression_db_t*
copy_expression (expression_db_t *edb)
{
    expression_db_t *copy;

    copy = new_expression_db(edb->kind);

    copy->name = g_strdup(edb->name);
    if (edb->symbol)
	copy->symbol = g_strdup(edb->symbol);

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

    if (edb->meta)
	copy->meta = expression_metadata_copy(edb->meta);

    if (edb->v.expression.docstring != NULL)
	copy->v.expression.docstring = g_strdup(edb->v.expression.docstring);

    return copy;
}

expression_db_t*
copy_expression_db (expression_db_t *edb)
{
    expression_db_t *head = NULL;
    expression_db_t *last = NULL;

    while (edb != NULL)
    {
	expression_db_t *copy;

	copy = copy_expression(edb);

	if (last == NULL)
	{
	    g_assert(head == NULL);
	    head = last = copy;
	}
	else
	    last = last->next = copy;

	edb = edb->next;
    }

    return head;
}

char*
read_expression (const char *path)
{
    char *expr;

    if (!g_file_get_contents(path, &expr, NULL, NULL))
    {
	fprintf(stderr, "Cannot read expression from file `%s': %m\n", path);
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

		if (! expr->meta) {
		    // cur_meta and expression_comment_callback are defined in expression_panel.c
		    cur_meta = expression_metadata_new();
		    scanner_set_comment_callback(expression_comment_callback);
		}

		expr->v.expression.mathmap = parse_mathmap(source);

		if (! expr->meta) {
		    scanner_set_comment_callback(NULL);
		    expr->meta = cur_meta;
		    cur_meta = NULL;
		}

		g_free(source);
	    }
	    return expr->v.expression.mathmap;

	case EXPRESSION_DB_DESIGN :
	    if (expr->v.design.mathmap == NULL)
	    {
		designer_design_t *design = designer_load_design(design_type, expr->v.design.path,
								 NULL, NULL, NULL, NULL);
		char *source;

		if (design == NULL)
		    return NULL;

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
    return mathmap->main_filter->name;
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

char*
get_expression_docstring (expression_db_t *edb)
{
    mathmap_t *mathmap;
    char *expression;

    g_assert(edb->kind == EXPRESSION_DB_EXPRESSION);

    if (edb->v.expression.docstring != NULL)
	return edb->v.expression.docstring;

    expression = read_expression(edb->v.expression.path);
    if (expression == NULL)
	return NULL;

    mathmap = parse_mathmap(expression);
    if (mathmap == NULL)
	return NULL;

    g_free(expression);

    g_assert(mathmap->main_filter != NULL);
    if (mathmap->main_filter->v.mathmap.decl->docstring != NULL)
	edb->v.expression.docstring = g_strdup(mathmap->main_filter->v.mathmap.decl->docstring);
    else
	edb->v.expression.docstring = g_strdup("");

    free_mathmap(mathmap);

    return edb->v.expression.docstring;
}

static char *generate_expression_symbol(expression_db_t *expr) {
    int i;
    int len;
    char *symbol = NULL;

    // TODO: designer
    if (expr->kind == EXPRESSION_DB_EXPRESSION) {
	char *name = get_expression_name(expr, NULL);
	// TODO: check that names are available once fixed
	if (name)
	    symbol = g_strdup_printf("%s_%s", SYMBOL_PREFIX, name);
    }
    if (! symbol)
	symbol = g_strdup_printf("%s_%s", SYMBOL_PREFIX, expr->name);

    len = strlen(symbol);
    for (i = 0; i < len; i++) {
	if (symbol[i] == ' ' || symbol[i] == '.')
	    symbol[i] = '_';
	else
	    symbol[i] = g_ascii_tolower(symbol[i]);
    }

    return symbol;
}


