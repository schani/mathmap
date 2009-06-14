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
#include "parser.h"
#include "overload.h"
#include "mathmap.h"
#include "jump.h"

#include "scanner.h"

typedef struct
{
    int row;
    int column;
    int pos;
} scanner_location_t;

typedef struct
{
    char *text;
    scanner_location_t location;
} scanner_state_t;

typedef struct
{
    scanner_location_t start;
    scanner_location_t end;
} scanner_region_t;

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
resolv_ident (const char *name, int *highlight)
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
	token->token = resolv_ident(ident, highlight);
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

int
scanner_line_num (void)
{
    return global_state.location.row;
}

int
yylex (void)
{
    scanner_token_t token;

    g_assert(global_state.text != NULL);
    switch (scan_token(&global_state, &token, NULL))
    {
	case RESULT_ERROR :
	    if (report_parse_error_to_user)
		set_expression_marker(token.region.start.row, token.region.start.column);
	    JUMP(1);
	    break;

	case RESULT_SUCCESS :
	    token.region.end = global_state.location;
	    switch (token.token)
	    {
		case T_IDENT :
		    yylval.ident = g_strndup(global_state.text + token.region.start.pos,
					     token.region.end.pos - token.region.start.pos);
		    break;

		case T_STRING :
		    yylval.ident = g_strndup(global_state.text + token.region.start.pos + 1,
					     token.region.end.pos - token.region.start.pos - 2);
		    break;

		case T_INT :
		    {
			char *str = g_strndup(global_state.text + token.region.start.pos,
					      token.region.end.pos - token.region.start.pos);
			yylval.exprtree = make_int_number(atoi(str));
			g_free(str);
		    }
		    break;

		case T_FLOAT :
		    {
			char *str = g_strndup(global_state.text + token.region.start.pos,
					      token.region.end.pos - token.region.start.pos);
			yylval.exprtree = make_float_number(g_ascii_strtod(str, NULL));
			g_free(str);
		    }
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
