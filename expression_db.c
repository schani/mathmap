/* -*- c -*- */

/*
 * expression_db.c
 *
 * MathMap
 *
 * Copyright (C) 2007 Mark Probst
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

#include "expression_db.h"

static expression_db_t*
new_expression_db (int kind)
{
    expression_db_t *edb = (expression_db_t*)malloc(sizeof(expression_db_t));

    assert(edb != 0);

    edb->kind = kind;
    edb->next = 0;

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
	if (name_len > 3 && strcmp(name + name_len - 3, ".mm") == 0)
	{
	    expression_db_t *edb = new_expression_db(EXPRESSION_DB_EXPRESSION);

	    edb->name = (char*)malloc(name_len - 2);
	    assert(edb->name != 0);

	    memcpy(edb->name, name, name_len - 3);
	    edb->name[name_len - 3] = '\0';

	    edb->v.expression.path = strdup(filename);
	    assert(edb->v.expression.path != 0);

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

expression_db_t*
read_expression_db (char *path)
{
    DIR *dir;
    struct dirent *dirent;
    expression_db_t *edb = 0;

    fprintf(stderr, "checking out %s\n", path);

    dir = opendir(path);

    if (dir == 0)
    {
	fprintf(stderr, "Cannot open directory `%s': %m\n", path);
	return 0;
    }

    for (;;)
    {
	dirent = readdir(dir);

	if (dirent != 0 && strcmp(dirent->d_name, ".") != 0 && strcmp(dirent->d_name, "..") != 0)
	{
	    char *name = strdup(dirent->d_name);
	    expression_db_t *subs;

	    fprintf(stderr, "sub %s\n", name);

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
	if (edb->kind == EXPRESSION_DB_EXPRESSION)
	    free(edb->v.expression.path);
	else if (edb->kind == EXPRESSION_DB_GROUP)
	    free_expression_db(edb->v.group.subs);
	else
	    assert(0);

	edb = next;
    }
}

#define START_SIZE         1024

char*
read_expression (const char *path)
{
    FILE *file;
    char *expr;
    size_t alloced, read;
    size_t result;

    file = fopen(path, "r");
    if (file == 0)
    {
	fprintf(stderr, "Cannot read file `%s': %m\n", path);
	return 0;
    }

    expr = (char*)malloc(START_SIZE);
    assert(expr != 0);
    alloced = START_SIZE;
    read = 0;

    for (;;)
    {
	result = fread(expr + read, 1, alloced - read, file);

	if (result == 0)
	{
	    fclose(file);
	    expr[read] = '\0';

	    return expr;
	}

	read += result;
	assert(read <= alloced);

	if (read == alloced)
	{
	    alloced *= 2;
	    expr = realloc(expr, alloced);
	    assert(expr != 0);
	}
    }
}
