/* $Id: lispreader.c 212 2000-09-11 00:43:43Z schani $ */
/*
 * lispreader.c
 *
 * Copyright (C) 1998-1999 Mark Probst
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include <lispreader.h>

#define TOKEN_ERROR                   -1
#define TOKEN_EOF                     0
#define TOKEN_OPEN_PAREN              1
#define TOKEN_CLOSE_PAREN             2
#define TOKEN_SYMBOL                  3
#define TOKEN_STRING                  4
#define TOKEN_INTEGER                 5
#define TOKEN_PATTERN_OPEN_PAREN      6
#define TOKEN_DOT                     7
#define TOKEN_TRUE                    8
#define TOKEN_FALSE                   9

#define MAX_TOKEN_LENGTH           4096	/* should be dynamic */

static char token_string[MAX_TOKEN_LENGTH + 1] = "";
static int token_length = 0;

static lisp_object_t end_marker = { LISP_TYPE_EOF };
static lisp_object_t error_object = { LISP_TYPE_PARSE_ERROR };
static lisp_object_t close_paren_marker = { LISP_TYPE_PARSE_ERROR };
static lisp_object_t dot_marker = { LISP_TYPE_PARSE_ERROR };

static void
_token_clear (void)
{
    token_string[0] = '\0';
    token_length = 0;
}

static void
_token_append (char c)
{
    assert(token_length < MAX_TOKEN_LENGTH);

    token_string[token_length++] = c;
    token_string[token_length] = '\0';
}

static int
_next_char (lisp_stream_t *stream)
{
    switch (stream->type)
    {
	case LISP_STREAM_FILE :
	    return getc(stream->v.file);

	case LISP_STREAM_STRING :
	    {
		char c = stream->v.string.buf[stream->v.string.pos];

		if (c == 0)
		    return EOF;

		++stream->v.string.pos;

		return c;
	    }
    }

    assert(0);
    return EOF;
}

static void
_unget_char (char c, lisp_stream_t *stream)
{
    switch (stream->type)
    {
	case LISP_STREAM_FILE :
	    ungetc(c, stream->v.file);
	    break;

	case LISP_STREAM_STRING :
	    --stream->v.string.pos;
	    break;

	default :
	    assert(0);
    }
}

static int
_scan (lisp_stream_t *stream)
{
    static char *delims = "\"();";

    int c;

    _token_clear();

    do
    {
	c = _next_char(stream);
	if (c == EOF)
	    return TOKEN_EOF;
    } while (isspace(c));

    switch (c)
    {
	case '(' :
	    return TOKEN_OPEN_PAREN;

	case ')' :
	    return TOKEN_CLOSE_PAREN;

	case '"' :
	    while (1)
	    {
		c = _next_char(stream);
		if (c == EOF)
		    return TOKEN_ERROR;
		if (c == '"')
		    break;
		if (c == '\\')
		{
		    c = _next_char(stream);

		    switch (c)
		    {
			case EOF :
			    return TOKEN_ERROR;
			
			case 'n' :
			    c = '\n';
			    break;

			case 't' :
			    c = '\t';
			    break;
		    }
		}

		_token_append(c);
	    }
	    return TOKEN_STRING;

	case '#' :
	    c = _next_char(stream);
	    if (c == EOF)
		return TOKEN_ERROR;

	    switch (c)
	    {
		case 't' :
		    return TOKEN_TRUE;

		case 'f' :
		    return TOKEN_FALSE;

		case '?' :
		    c = _next_char(stream);
		    if (c == EOF)
			return TOKEN_ERROR;

		    if (c == '(')
			return TOKEN_PATTERN_OPEN_PAREN;
		    else
			return TOKEN_ERROR;
	    }
	    return TOKEN_ERROR;

	default :
	    if (isdigit(c) || c == '-')
	    {
		int have_nondigits = 0;
		int have_digits = 0;

		do
		{
		    if (isdigit(c))
			have_digits = 1;
		    _token_append(c);
		    c = _next_char(stream);
		    if (c != EOF && !isdigit(c) && !isspace(c) && !strchr(delims, c))
			have_nondigits = 1;
		} while (c != EOF && !isspace(c) && !strchr(delims, c));

		if (c != EOF)
		    _unget_char(c, stream);

		if (have_nondigits || !have_digits)
		    return TOKEN_SYMBOL;
		return TOKEN_INTEGER;
	    }
	    else
	    {
		if (c == '.')
		{
		    c = _next_char(stream);
		    if (c != EOF && !isspace(c) && !strchr(delims, c))
			_token_append('.');
		    else
		    {
			_unget_char(c, stream);
			return TOKEN_DOT;
		    }
		}
		do
		{
		    _token_append(c);
		    c = _next_char(stream);
		} while (c != EOF && !isspace(c) && !strchr(delims, c));
		if (c != EOF)
		    _unget_char(c, stream);

		return TOKEN_SYMBOL;
	    }
    }

    assert(0);
    return TOKEN_ERROR;
}

lisp_object_t*
lisp_object_alloc (int type)
{
    lisp_object_t *obj = (lisp_object_t*)malloc(sizeof(lisp_object_t));

    obj->type = type;

    return obj;
}

lisp_stream_t*
lisp_stream_init_file (lisp_stream_t *stream, FILE *file)
{
    stream->type = LISP_STREAM_FILE;
    stream->v.file = file;

    return stream;
}

lisp_stream_t*
lisp_stream_init_string (lisp_stream_t *stream, char *buf)
{
    stream->type = LISP_STREAM_STRING;
    stream->v.string.buf = buf;
    stream->v.string.pos = 0;

    return stream;
}

lisp_object_t*
lisp_read (lisp_stream_t *in)
{
    int token = _scan(in);
    lisp_object_t *obj = 0;

    if (token == TOKEN_EOF)
	return &end_marker;

    switch (token)
    {
	case TOKEN_ERROR :
	    return &error_object;

	case TOKEN_EOF :
	    return &end_marker;

	case TOKEN_OPEN_PAREN :
	case TOKEN_PATTERN_OPEN_PAREN :
	    {
		lisp_object_t *last = 0, *car;

		do
		{
		    car = lisp_read(in);
		    if (car == &error_object || car == &end_marker)
		    {
			lisp_free(obj);
			return &error_object;
		    }
		    else if (car == &dot_marker)
		    {
			if (last == 0)
			{
			    lisp_free(obj);
			    return &error_object;
			}

			car = lisp_read(in);
			if (car == &error_object || car == &end_marker)
			{
			    lisp_free(obj);
			    return car;
			}
			else
			{
			    last->v.cons.cdr = car;

			    if (_scan(in) != TOKEN_CLOSE_PAREN)
			    {
				lisp_free(obj);
				return &error_object;
			    }

			    car = &close_paren_marker;
			}
		    }
		    else if (car != &close_paren_marker)
		    {
			if (last == 0)
			    obj = last = lisp_object_alloc(token == TOKEN_OPEN_PAREN
							   ? LISP_TYPE_CONS
							   : LISP_TYPE_PATTERN_CONS);
			else
			    last = last->v.cons.cdr = lisp_object_alloc(LISP_TYPE_CONS);
			last->v.cons.car = car;
			last->v.cons.cdr = 0;
		    }
		} while (car != &close_paren_marker);
	    }
	    return obj;

	case TOKEN_CLOSE_PAREN :
	    return &close_paren_marker;

	case TOKEN_SYMBOL :
	    obj = lisp_object_alloc(LISP_TYPE_SYMBOL);
	    obj->v.string = strdup(token_string);
	    return obj;

	case TOKEN_STRING :
	    obj = lisp_object_alloc(LISP_TYPE_STRING);
	    obj->v.string = strdup(token_string);
	    return obj;

	case TOKEN_INTEGER :
	    obj = lisp_object_alloc(LISP_TYPE_INTEGER);
	    obj->v.integer = atoi(token_string);
	    return obj;

	case TOKEN_DOT :
	    return &dot_marker;

	case TOKEN_TRUE :
	    obj = lisp_object_alloc(LISP_TYPE_BOOLEAN);
	    obj->v.integer = 1;
	    return obj;

	case TOKEN_FALSE :
	    obj = lisp_object_alloc(LISP_TYPE_BOOLEAN);
	    obj->v.integer = 0;
	    return obj;
    }

    assert(0);
    return &error_object;
}

void
lisp_free (lisp_object_t *obj)
{
    if (obj == 0)
	return;

    switch (obj->type)
    {
	case LISP_TYPE_INTERNAL :
	case LISP_TYPE_PARSE_ERROR :
	case LISP_TYPE_EOF :
	    return;

	case LISP_TYPE_SYMBOL :
	case LISP_TYPE_STRING :
	    free(obj->v.string);
	    break;

	case LISP_TYPE_CONS :
	case LISP_TYPE_PATTERN_CONS :
	    lisp_free(obj->v.cons.car);
	    lisp_free(obj->v.cons.cdr);
	    break;

	case LISP_TYPE_PATTERN_VAR :
	    lisp_free(obj->v.pattern.sub);
	    break;
    }

    free(obj);
}

lisp_object_t*
lisp_read_from_string (const char *buf)
{
    lisp_stream_t stream;

    lisp_stream_init_string(&stream, (char*)buf);
    return lisp_read(&stream);
}

static int
_compile_pattern (lisp_object_t **obj, int *index)
{
    if (*obj == 0)
	return 1;

    switch (lisp_type(*obj))
    {
	case LISP_TYPE_PATTERN_CONS :
	    {
		struct { char *name; int type; } types[] =
						 {
						     { "any", LISP_PATTERN_ANY },
						     { "symbol", LISP_PATTERN_SYMBOL },
						     { "string", LISP_PATTERN_STRING },
						     { "integer", LISP_PATTERN_INTEGER },
						     { "boolean", LISP_PATTERN_BOOLEAN },
						     { "list", LISP_PATTERN_LIST },
						     { "or", LISP_PATTERN_OR },
						     { 0, 0 }
						 };
		char *type_name;
		int type;
		int i;
		lisp_object_t *pattern;

		if (lisp_type(lisp_car(*obj)) != LISP_TYPE_SYMBOL)
		    return 0;

		type_name = lisp_symbol(lisp_car(*obj));
		for (i = 0; types[i].name != 0; ++i)
		{
		    if (strcmp(types[i].name, type_name) == 0)
		    {
			type = types[i].type;
			break;
		    }
		}

		if (types[i].name == 0)
		    return 0;

		if (type != LISP_PATTERN_OR && lisp_cdr(*obj) != 0)
		    return 0;

		pattern = lisp_object_alloc(LISP_TYPE_PATTERN_VAR);
		pattern->v.pattern.type = type;
		pattern->v.pattern.index = (*index)++;
		pattern->v.pattern.sub = 0;

		if (type == LISP_PATTERN_OR)
		{
		    lisp_object_t *cdr = lisp_cdr(*obj);

		    if (!_compile_pattern(&cdr, index))
		    {
			lisp_free(pattern);
			return 0;
		    }

		    pattern->v.pattern.sub = cdr;

		    (*obj)->v.cons.cdr = 0;
		}

		lisp_free(*obj);

		*obj = pattern;
	    }
	    break;

	case LISP_TYPE_CONS :
	    if (!_compile_pattern(&(*obj)->v.cons.car, index))
		return 0;
	    if (!_compile_pattern(&(*obj)->v.cons.cdr, index))
		return 0;
	    break;
    }

    return 1;
}

int
lisp_compile_pattern (lisp_object_t **obj, int *num_subs)
{
    int index = 0;
    int result;

    result = _compile_pattern(obj, &index);

    if (result && num_subs != 0)
	*num_subs = index;

    return result;
}

static int _match_pattern (lisp_object_t *pattern, lisp_object_t *obj, lisp_object_t **vars);

static int
_match_pattern_var (lisp_object_t *pattern, lisp_object_t *obj, lisp_object_t **vars)
{
    assert(lisp_type(pattern) == LISP_TYPE_PATTERN_VAR);

    switch (pattern->v.pattern.type)
    {
	case LISP_PATTERN_ANY :
	    break;

	case LISP_PATTERN_SYMBOL :
	    if (obj == 0 || lisp_type(obj) != LISP_TYPE_SYMBOL)
		return 0;
	    break;

	case LISP_PATTERN_STRING :
	    if (obj == 0 || lisp_type(obj) != LISP_TYPE_STRING)
		return 0;
	    break;

	case LISP_PATTERN_INTEGER :
	    if (obj == 0 || lisp_type(obj) != LISP_TYPE_INTEGER)
		return 0;
	    break;

	case LISP_PATTERN_BOOLEAN :
	    if (obj == 0 || lisp_type(obj) != LISP_TYPE_BOOLEAN)
		return 0;
	    break;

	case LISP_PATTERN_LIST :
	    if (obj == 0 || lisp_type(obj) != LISP_TYPE_CONS)
		return 0;
	    break;

	case LISP_PATTERN_OR :
	    {
		lisp_object_t *sub;
		int matched = 0;

		for (sub = pattern->v.pattern.sub; sub != 0; sub = lisp_cdr(sub))
		{
		    assert(lisp_type(sub) == LISP_TYPE_CONS);

		    if (_match_pattern(lisp_car(sub), obj, vars))
			matched = 1;
		}

		if (!matched)
		    return 0;
	    }
	    break;

	default :
	    assert(0);
    }

    if (vars != 0)
	vars[pattern->v.pattern.index] = obj;

    return 1;
}

static int
_match_pattern (lisp_object_t *pattern, lisp_object_t *obj, lisp_object_t **vars)
{
    if (pattern == 0)
	return obj == 0;

    if (obj == 0)
	return 0;

    if (lisp_type(pattern) == LISP_TYPE_PATTERN_VAR)
	return _match_pattern_var(pattern, obj, vars);

    if (lisp_type(pattern) != lisp_type(obj))
	return 0;

    switch (lisp_type(pattern))
    {
	case LISP_TYPE_SYMBOL :
	    return strcmp(lisp_symbol(pattern), lisp_symbol(obj)) == 0;

	case LISP_TYPE_STRING :
	    return strcmp(lisp_string(pattern), lisp_string(obj)) == 0;

	case LISP_TYPE_INTEGER :
	    return lisp_integer(pattern) == lisp_integer(obj);

	case LISP_TYPE_CONS :
	    {
		int result1, result2;

		result1 = _match_pattern(lisp_car(pattern), lisp_car(obj), vars);
		result2 = _match_pattern(lisp_cdr(pattern), lisp_cdr(obj), vars);

		return result1 && result2;
	    }
	    break;

	default :
	    assert(0);
    }

    return 0;
}

int
lisp_match_pattern (lisp_object_t *pattern, lisp_object_t *obj, lisp_object_t **vars, int num_subs)
{
    int i;

    if (vars != 0)
	for (i = 0; i < num_subs; ++i)
	    vars[i] = &error_object;

    return _match_pattern(pattern, obj, vars);
}

int
lisp_match_string (const char *pattern_string, lisp_object_t *obj, lisp_object_t **vars)
{
    lisp_object_t *pattern;
    int result;
    int num_subs;

    pattern = lisp_read_from_string(pattern_string);

    if (pattern != 0 && (lisp_type(pattern) == LISP_TYPE_EOF
			 || lisp_type(pattern) == LISP_TYPE_PARSE_ERROR))
	return 0;

    if (!lisp_compile_pattern(&pattern, &num_subs))
    {
	lisp_free(pattern);
	return 0;
    }

    result = lisp_match_pattern(pattern, obj, vars, num_subs);

    lisp_free(pattern);

    return result;
}

int
lisp_type (lisp_object_t *obj)
{
    if (obj == 0)
	return LISP_TYPE_NIL;
    return obj->type;
}

int
lisp_integer (lisp_object_t *obj)
{
    assert(obj->type == LISP_TYPE_INTEGER);

    return obj->v.integer;
}

char*
lisp_symbol (lisp_object_t *obj)
{
    assert(obj->type == LISP_TYPE_SYMBOL);

    return obj->v.string;
}

char*
lisp_string (lisp_object_t *obj)
{
    assert(obj->type == LISP_TYPE_STRING);

    return obj->v.string;
}

int
lisp_boolean (lisp_object_t *obj)
{
    assert(obj->type == LISP_TYPE_BOOLEAN);

    return obj->v.integer;
}

lisp_object_t*
lisp_car (lisp_object_t *obj)
{
    assert(obj->type == LISP_TYPE_CONS || obj->type == LISP_TYPE_PATTERN_CONS);

    return obj->v.cons.car;
}

lisp_object_t*
lisp_cdr (lisp_object_t *obj)
{
    assert(obj->type == LISP_TYPE_CONS || obj->type == LISP_TYPE_PATTERN_CONS);

    return obj->v.cons.cdr;
}

int
lisp_list_length (lisp_object_t *obj)
{
    int length = 0;

    while (obj != 0)
    {
	assert(obj->type == LISP_TYPE_CONS || obj->type == LISP_TYPE_PATTERN_CONS);

	++length;
	obj = obj->v.cons.cdr;
    }

    return length;
}

lisp_object_t*
lisp_list_nth (lisp_object_t *obj, int index)
{
    while (index > 0)
    {
	assert(obj != 0);
	assert(obj->type == LISP_TYPE_CONS || obj->type == LISP_TYPE_PATTERN_CONS);

	--index;
	obj = obj->v.cons.cdr;
    }

    assert(obj != 0);

    return obj->v.cons.car;
}

void
lisp_dump (lisp_object_t *obj, FILE *out)
{
    if (obj == 0)
    {
	fprintf(out, "()");
	return;
    }

    switch (lisp_type(obj))
    {
	case LISP_TYPE_EOF :
	    fputs("#<eof>", out);
	    break;

	case LISP_TYPE_PARSE_ERROR :
	    fputs("#<error>", out);
	    break;

	case LISP_TYPE_INTEGER :
	    fprintf(out, "%d", lisp_integer(obj));
	    break;

	case LISP_TYPE_SYMBOL :
	    fputs(lisp_symbol(obj), out);
	    break;

	case LISP_TYPE_STRING :
	    {
		char *p;

		fputc('"', out);
		for (p = lisp_string(obj); *p != 0; ++p)
		{
		    if (*p == '"' || *p == '\\')
			fputc('\\', out);
		    fputc(*p, out);
		}
		fputc('"', out);
	    }
	    break;

	case LISP_TYPE_CONS :
	case LISP_TYPE_PATTERN_CONS :
	    fputs(lisp_type(obj) == LISP_TYPE_CONS ? "(" : "#?(", out);
	    while (obj != 0)
	    {
		lisp_dump(lisp_car(obj), out);
		obj = lisp_cdr(obj);
		if (obj != 0)
		{
		    if (lisp_type(obj) != LISP_TYPE_CONS
			&& lisp_type(obj) != LISP_TYPE_PATTERN_CONS)
		    {
			fputs(" . ", out);
			lisp_dump(obj, out);
			break;
		    }
		    else
			fputc(' ', out);
		}
	    }
	    fputc(')', out);
	    break;

	case LISP_TYPE_BOOLEAN :
	    if (lisp_boolean(obj))
		fputs("#t", out);
	    else
		fputs("#f", out);
	    break;

	default :
	    assert(0);
    }
}
