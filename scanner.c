/*
 * scanner.c
 *
 * MathMap
 *
 * Copyright (C) 2002-2009 Mark Probst
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

#include <glib.h>
#include <ctype.h>
#include <string.h>

#include "exprtree.h"
#include "scanner.h"
#include "parser.h"
#include "overload.h"
#include "mathmap.h"
#include "jump.h"

scanner_region_t scanner_null_region = { { -1, -1, -1 }, { -1, -1, -1 } };

typedef struct
{
    char *text;
    scanner_location_t location;
} scanner_state_t;

typedef struct
{
    int token;
    scanner_region_t region;
} scanner_token_t;

typedef struct
{
    const char *name;
    int value;
} name_value_pair_t;

static int
find_name (name_value_pair_t *list, const char *name)
{
    while (list->name != NULL)
    {
	if (strcmp(list->name, name) == 0)
	    return list->value;
	++list;
    }
    return 0;
}

#define RETURN_HIGHLIGHT(r,h)	\
    do {			\
    if (highlight)		\
	*highlight = (h);	\
    return (r);			\
    } while (0)

static int
resolv_ident (const char *name, int *highlight, scanner_region_t region)
{
    static name_value_pair_t keywords[] =
	{ { "filter", T_FILTER },
	  { "if", T_IF },
	  { "then", T_THEN },
	  { "else", T_ELSE },
	  { "end", T_END },
	  { "while", T_WHILE },
	  { "do", T_DO },
	  { "for", T_FOR },
	  { "xor", T_XOR },
	  { NULL, 0 } };
    static name_value_pair_t internals[] =
        { { "x", 1 }, { "y", 1 },
	  { "r", 1 }, { "a", 1 },
	  { "t", 1 },
	  { "X", 1 }, { "Y", 1 },
	  { "W", 1 }, { "H", 1 },
	  { "R", 1 },
	  { "frame", 1 },
	  { "xy", 1 }, { "ra", 1 },
	  { "XY", 1 }, { "WH", 1 },
	  { NULL, 0 } };
    static name_value_pair_t uservals[] =
	{ { "int", T_INT_TYPE },
	  { "float", T_FLOAT_TYPE },
	  { "bool", T_BOOL_TYPE },
	  { "color", T_COLOR_TYPE },
	  { "curve", T_CURVE_TYPE },
	  { "gradient", T_GRADIENT_TYPE },
	  { "image", T_IMAGE_TYPE },
	  { NULL, 0 } };
    static name_value_pair_t constants[] =
	{ { "pi", 1 },
	  { "e", 1 },
	  { "I", 1 },
	  { NULL, 0 } };
    static name_value_pair_t errors[] =
	{ { "function", 1 },
	  { "lambda", 1 },
	  { NULL, 0 } };
    static name_value_pair_t attributes[] =
	{ { "pixel", 1 },
	  { "stretched", 1 },
	  { NULL, 0 } };

    int token;

    if ((token = find_name(keywords, name)))
	RETURN_HIGHLIGHT(token, HIGHLIGHT_KEYWORD);
    if ((token = find_name(internals, name)))
	RETURN_HIGHLIGHT(T_IDENT, HIGHLIGHT_INTERNAL);
    if ((token = find_name(uservals, name)))
	RETURN_HIGHLIGHT(token, HIGHLIGHT_USERVAL);
    if ((token = find_name(constants, name)))
	RETURN_HIGHLIGHT(T_IDENT, HIGHLIGHT_CONSTANT);
    if ((token = find_name(errors, name)))
    {
	sprintf(error_string, _("`%s' is a reserved keyword"), name);
	error_region = region;
	RETURN_HIGHLIGHT(0, HIGHLIGHT_ERROR);
    }
    if ((token = find_name(attributes, name)))
	RETURN_HIGHLIGHT(T_IDENT, HIGHLIGHT_ATTRIBUTE);
    if (highlight)
    {
	if (exists_overload_entry_with_name(name))
	    *highlight = HIGHLIGHT_BUILTIN;
	else
	    *highlight = HIGHLIGHT_VARIABLE;
    }
    return T_IDENT;
}

static char
current_char (scanner_state_t *state)
{
    return state->text[state->location.pos];
}

static gboolean
is_eof (scanner_state_t *state)
{
    return current_char(state) == 0;
}

static char
advance_char (scanner_state_t *state)
{
    char c = current_char(state);

    g_assert(c != 0);
    if (c == '\n')
    {
	++state->location.row;
	state->location.column = 0;
    }
    else
	++state->location.column;
    ++state->location.pos;
    return current_char(state);
}

static void
reverse_char (scanner_state_t *state)
{
    g_assert(state->location.pos > 0);
    --state->location.pos;
}

static int
skip_ws (scanner_state_t *state)
{
    while (!is_eof(state) && isspace(current_char(state)))
	advance_char(state);
    return current_char(state);
}

static scanner_region_t
make_region (scanner_location_t start, scanner_location_t end)
{
    scanner_region_t region;

    region.start = start;
    region.end = end;

    return region;
}

#define RESULT_ERROR		0
#define RESULT_SUCCESS		1
#define RESULT_EOF		2
#define RESULT_HIGHLIGHT	3

/*
 * Returns RESULT_SUCCESS if a token was scanned, RESULT_EOF on EOF
 * and RESULT_ERROR if an error occurred.  Returns RESULT_HIGHLIGHT if
 * highlight is non-NULL for tokens which are not parsed.
 *
 * In the cases of success and error, fills token->region.start.  In
 * those cases, end of the token is in state->location.  On success
 * and if highlight is non-NULL, also fills token->token.
 */
static int
scan_token (scanner_state_t *state, scanner_token_t *token, int *highlight)
{
    char c;

 restart:
    c = skip_ws(state);
    if (is_eof(state))
	RETURN_HIGHLIGHT(RESULT_EOF, HIGHLIGHT_EOS);

    token->region.start = state->location;

    if (c == '#')
    {
	while (advance_char(state) != '\n' && !is_eof(state))
	    ;
	if (highlight)
	    RETURN_HIGHLIGHT(RESULT_HIGHLIGHT, HIGHLIGHT_COMMENT);
	if (is_eof(state))
	    RETURN_HIGHLIGHT(RESULT_EOF, HIGHLIGHT_EOS);
	goto restart;
    }
    else if (c == '_' || isalpha(c))
    {
	char *ident;

	do
	{
	    c = advance_char(state);
	    if (c != '_' && !isalnum(c))
		break;
	} while (TRUE);

	if (highlight)
	{
	    scanner_state_t state_copy = *state;
	    /* FIXME: this is not correct anymore - filter
	       arguments also have this syntax */
	    if (skip_ws(&state_copy) == ':')
		RETURN_HIGHLIGHT(RESULT_HIGHLIGHT, HIGHLIGHT_TAG);
	}

	ident = g_strndup(state->text + token->region.start.pos,
			  state->location.pos - token->region.start.pos);
	token->token = resolv_ident(ident, highlight, make_region(token->region.start, state->location));
	g_free(ident);

	if (!token->token)
	    RETURN_HIGHLIGHT(RESULT_ERROR, HIGHLIGHT_ERROR);
	RETURN_HIGHLIGHT(RESULT_SUCCESS, *highlight);
    }
    else if (c == '\"')
    {
	do
	{
	    c = advance_char(state);
	    if (is_eof(state))
	    {
		sprintf(error_string, _("String not terminated"));
		error_region = make_region(token->region.start, state->location);
		RETURN_HIGHLIGHT(RESULT_ERROR, HIGHLIGHT_ERROR);
	    }
	} while (c != '\"');
	advance_char(state);

	token->token = T_STRING;
	RETURN_HIGHLIGHT(RESULT_SUCCESS, HIGHLIGHT_STRING);
    }
    else if (c == '.' || isdigit(c))
    {
	gboolean have_comma = (c == '.');
	gboolean have_digits = !have_comma;

	do
	{
	    char d = c;
	    c = advance_char(state);
	    if (c == '.')
	    {
		if (have_comma && !have_digits)
		{
		    advance_char(state);
		    token->token = T_RANGE;
		    RETURN_HIGHLIGHT(RESULT_SUCCESS, HIGHLIGHT_SPECIAL);
		}
		else if (have_comma)
		{
		    g_assert(have_digits);
		    if (d == '.')
		    {
			reverse_char(state);
			token->token = T_INT;
			RETURN_HIGHLIGHT(RESULT_SUCCESS, HIGHLIGHT_INT);
		    }
		    else
		    {
			token->token = T_FLOAT;
			RETURN_HIGHLIGHT(RESULT_SUCCESS, HIGHLIGHT_FLOAT);
		    }
		}
		else
		    have_comma = TRUE;
	    }
	    else if (isdigit(c))
		have_digits = TRUE;
	    else
		break;
	} while (TRUE);

	if (!have_digits)
	{
	    sprintf(error_string, _("Misplaced decimal point"));
	    error_region = make_region(token->region.start, state->location);
	    RETURN_HIGHLIGHT(RESULT_ERROR, HIGHLIGHT_ERROR);
	}

	token->token = have_comma ? T_FLOAT : T_INT;
	RETURN_HIGHLIGHT(RESULT_SUCCESS, have_comma ? HIGHLIGHT_FLOAT : HIGHLIGHT_INT);
    }
    else
    {
	static const char *single_char_tokens = "-<>!,()+*/%=;^:[]";
	static name_value_pair_t special_tokens[] =
	    { { "==", T_EQUAL },
	      { "<=", T_LESSEQUAL },
	      { ">=", T_GREATEREQUAL },
	      { "!=", T_NOTEQUAL },
	      { "||", T_OR },
	      { "&&", T_AND },
	      { "::", T_CONVERT },
	      { NULL, 0 } };

	gboolean is_single_char_token = strchr(single_char_tokens, c) == NULL ? FALSE : TRUE;
	int d = advance_char(state);
	char str[3];

	if (is_eof(state))
	{
	    if (is_single_char_token)
	    {
		token->token = c;
		RETURN_HIGHLIGHT(RESULT_SUCCESS, HIGHLIGHT_SPECIAL);
	    }
	    else
	    {
		sprintf(error_string, _("Illegal character"));
		error_region = make_region(token->region.start, state->location);
		RETURN_HIGHLIGHT(RESULT_ERROR, HIGHLIGHT_ERROR);
	    }
	}

	sprintf(str, "%c%c", c, d);
	token->token = find_name(special_tokens, str);
	if (!token->token && is_single_char_token)
	{
	    token->token = c;
	    RETURN_HIGHLIGHT(RESULT_SUCCESS, HIGHLIGHT_SPECIAL);
	}

	advance_char(state);

	if (token->token)
	    RETURN_HIGHLIGHT(RESULT_SUCCESS, HIGHLIGHT_SPECIAL);

	sprintf(error_string, _("Illegal character sequence"));
	error_region = make_region(token->region.start, state->location);
	RETURN_HIGHLIGHT(RESULT_ERROR, HIGHLIGHT_ERROR);
    }
}

int
next_highlight (const char *expr, int start, int *first, int *last)
{
    scanner_state_t state;
    scanner_token_t token;
    int highlight;

    state.text = (char*)expr;
    state.location.row = state.location.column = 0;
    state.location.pos = start;

    scan_token(&state, &token, &highlight);

    *first = token.region.start.pos;
    *last = state.location.pos;

    return highlight;
}

static scanner_state_t global_state;

void
scanFromString (const char *string)
{
    g_assert(global_state.text == NULL);
    global_state.text = g_strdup(string);
    global_state.location.row = global_state.location.column = global_state.location.pos = 0;
}

void
endScanningFromString (void)
{
    g_assert(global_state.text != NULL);
    g_free(global_state.text);
    global_state.text = NULL;
}

scanner_location_t
scanner_location (void)
{
    return global_state.location;
}

static scanner_ident_t*
make_ident (scanner_region_t region, const char *text, int length)
{
    scanner_ident_t *ident = malloc(sizeof(scanner_ident_t) + length + 1);

    ident->region = region;
    memcpy(ident->str, text, length);
    ident->str[length] = 0;

    return ident;
}

scanner_ident_t*
scanner_make_ident (scanner_region_t region, const char *str)
{
    return make_ident(region, str, strlen(str));
}

int
yylex (void)
{
    scanner_token_t token;

    g_assert(global_state.text != NULL);
    switch (scan_token(&global_state, &token, NULL))
    {
	case RESULT_ERROR :
	    sprintf(error_string, "Invalid characters.");
	    error_region.start = token.region.start;
	    error_region.end = global_state.location;
	    JUMP(1);
	    break;

	case RESULT_SUCCESS :
	    token.region.end = global_state.location;
	    switch (token.token)
	    {
		case T_IDENT :
		    yylval.ident = make_ident(token.region,
					      global_state.text + token.region.start.pos,
					      token.region.end.pos - token.region.start.pos);
		    break;

		case T_STRING :
		    yylval.ident = make_ident(token.region,
					      global_state.text + token.region.start.pos + 1,
					      token.region.end.pos - token.region.start.pos - 2);
		    ++yylval.ident->region.start.column;
		    ++yylval.ident->region.start.pos;
		    --yylval.ident->region.end.column;
		    --yylval.ident->region.end.pos;
		    break;

		case T_INT :
		    {
			char *str = g_strndup(global_state.text + token.region.start.pos,
					      token.region.end.pos - token.region.start.pos);
			yylval.exprtree = make_int_number(atoi(str), token.region);
			g_free(str);
		    }
		    break;

		case T_FLOAT :
		    {
			char *str = g_strndup(global_state.text + token.region.start.pos,
					      token.region.end.pos - token.region.start.pos);
			yylval.exprtree = make_float_number(g_ascii_strtod(str, NULL), token.region);
			g_free(str);
		    }
		    break;

		case T_OR :
		case T_AND :
		case T_XOR :
		case T_EQUAL :
		case '<' :
		case '>' :
		case T_LESSEQUAL :
		case T_GREATEREQUAL :
		case T_NOTEQUAL :
		case '+' :
		case '-' :
		case '*' :
		case '/' :
		case '%' :
		case '^' :
		case '!' :
		    yylval.ident = make_ident(token.region,
					      global_state.text + token.region.start.pos,
					      token.region.end.pos - token.region.start.pos);
		    break;
	    }
	    return token.token;

	case RESULT_EOF :
	    return 0;

	default :
	    g_assert_not_reached();
    }
    g_assert_not_reached();
}

gboolean
scanner_region_is_valid (scanner_region_t r)
{
    gboolean valid = r.start.pos >= 0;
    if (valid)
	g_assert(r.end.pos >= 0);
    else
	g_assert(r.end.pos < 0);
    return valid;
}

scanner_region_t
scanner_region_merge (scanner_region_t r1, scanner_region_t r2)
{
    scanner_region_t r;

    if (!scanner_region_is_valid(r1))
	return r2;
    if (!scanner_region_is_valid(r2))
	return r1;

    if (r1.start.pos < r2.start.pos)
	r.start = r1.start;
    else
	r.start = r2.start;

    if (r1.end.pos > r2.end.pos)
	r.end = r1.end;
    else
	r.end = r2.end;

    return r;
}
