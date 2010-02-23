/* -*- c -*- */

/*
 * expression_db.c
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

    if (! expr->meta.title) {
	char *path = get_expression_path(expr);
	if (path) {
	    GMatchInfo *matches;
	    if (! rex) {
		rex = g_regex_new("([^/]+)/([^/.]+)\\.[^.]+$", 0, 0, NULL);
	    }
	    if (g_regex_match(rex, path, 0, &matches)) {
		int len;
		char *title = g_match_info_fetch(matches, 2);
		int i;
		len = strlen(title);
		// underscores are interpreted in a special way in menu, replacing with spaces
		for (i = 0; i < len; i++)
		    if (title[i] == '_')
			title[i] = ' ';
		expr->meta.title = title;
	    }
	    g_match_info_free(matches);
	} else {
	    expr->meta.title = g_strdup("untitled");
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

	designer_load_design_metadata(filename, &expr->meta);

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
free_expression_metadata_members (expression_metadata_t *meta)
{
    GList *tag;

    if (meta->name_space != NULL)
    {
	g_free(meta->name_space);
	meta->name_space = NULL;
    }
    if (meta->name != NULL)
    {
	g_free(meta->name);
	meta->name = NULL;
    }
    if (meta->title != NULL)
    {
	g_free(meta->title);
	meta->title = NULL;
    }

    tag = meta->tags;
    while (tag != NULL)
    {
	g_free(tag->data);
	tag = g_list_next(tag);
    }
    g_list_free(meta->tags);
    meta->tags = NULL;
}

void
free_expression_db (expression_db_t *edb)
{
    while (edb != 0)
    {
	expression_db_t *next = edb->next;

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

	free_expression_metadata_members(&edb->meta);

	edb = next;
    }
}

static void
copy_expression_metadata (expression_metadata_t *dst, expression_metadata_t *src)
{
    GList *tag;

    memset(dst, 0, sizeof(expression_metadata_t));

    if (src->name_space != NULL)
	dst->name_space = g_strdup(src->name_space);
    if (src->name != NULL)
	dst->name = g_strdup(src->name);
    if (src->title != NULL)
	dst->title = g_strdup(src->title);

    tag = src->tags;
    while (tag != NULL)
    {
	dst->tags = g_list_append(dst->tags, g_strdup(tag->data));
	tag = g_list_next(tag);
    }
}

static expression_db_t*
copy_expression (expression_db_t *edb)
{
    expression_db_t *copy;

    copy = new_expression_db(edb->kind);

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

    copy_expression_metadata(&copy->meta, &edb->meta);

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

		cur_meta = &expr->meta;
		scanner_set_comment_callback(expression_comment_callback);

		expr->v.expression.mathmap = parse_mathmap(source);

		scanner_set_comment_callback(NULL);
		cur_meta = NULL;

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
get_expression_name_space (expression_db_t *expr, designer_design_type_t *design_type)
{
    if (expr->kind == EXPRESSION_DB_DESIGN)
	return expr->meta.name_space;
    else
    {
	mathmap_t *mathmap = fetch_expression_mathmap(expr, design_type);

	if (mathmap == NULL)
	    return NULL;
	g_assert(mathmap->main_filter->kind == FILTER_MATHMAP);
	return mathmap->main_filter->v.mathmap.decl->name_space;
    }
}

char*
get_expression_name (expression_db_t *expr, designer_design_type_t *design_type)
{
    if (expr->kind == EXPRESSION_DB_DESIGN)
	return expr->meta.name;
    else
    {
	mathmap_t *mathmap = fetch_expression_mathmap(expr, design_type);

	if (mathmap == NULL)
	    return NULL;
	return mathmap->main_filter->name;
    }
}

char*
get_expression_full_name (expression_db_t *expr, designer_design_type_t *design_type)
{
    char *name_space = get_expression_name_space(expr, design_type);
    char *name = get_expression_name(expr, design_type);

    if (name == NULL)
	return NULL;

    if (name_space == NULL)
	return g_strdup(name);
    return g_strdup_printf("%s.%s", name_space, name);
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

// necessary for get_expression_name
static designer_design_type_t *get_design_type() {
    static designer_design_type_t *design_type = NULL;
    if (! design_type)
	design_type = make_mathmap_design_type();
    return design_type;
}

static char *generate_expression_symbol(expression_db_t *expr) {
    int i;
    int len;
    char *symbol = NULL;
    char *name;

    switch (expr->kind) {
	case EXPRESSION_DB_EXPRESSION:
	    name = get_expression_name(expr, NULL);
	    break;
	case EXPRESSION_DB_DESIGN:
	    name = get_expression_name(expr, get_design_type());
	    break;
	default:
	    assert(0);
	    return NULL;
    }
    if (name) {
	symbol = g_strdup_printf("%s_%s", SYMBOL_PREFIX, name);
    } else {
	printf("Could not retrieve filter name from file: %s falling back to %s\n",
	    get_expression_path(expr), expr->meta.title);
	symbol = g_strdup_printf("%s_%s", SYMBOL_PREFIX, expr->meta.title);
    }

    len = strlen(symbol);
    for (i = 0; i < len; i++) {
	if (symbol[i] == ' ' || symbol[i] == '.')
	    symbol[i] = '_';
	else
	    symbol[i] = g_ascii_tolower(symbol[i]);
    }

    return symbol;
}

void save_expression_to_dir(expression_db_t *expr, char *dir) {
    char *ext;
    char *source;
    char *name;
    designer_design_type_t *design_type;

    switch (expr->kind) {
	case EXPRESSION_DB_EXPRESSION:
	    ext = "mm";
	    break;
	case EXPRESSION_DB_DESIGN:
	    ext = "mmc";
	    design_type = get_design_type();
	    break;
	default:
	    assert(0);
	    return;
    }

    name = get_expression_name(expr, design_type);
    if (name) {
	source = read_expression(get_expression_path(expr));
	if (source) {
	    char *filename = g_strdup_printf("%s/%s.%s", dir, name, ext);
	    FILE *out = fopen(filename, "wb");
	    if (out) {
		fwrite(source, sizeof(char), strlen(source), out);
		fclose(out);
	    } else {
		printf("Could not write to file %s\n", filename);
	    }
	    g_free(filename);
	    g_free(source);
	}
    } else {
	printf("Could not retrieve filter name from file: %s\n", get_expression_path(expr));
    }
}


