/*
 * highlight.c
 *
 * MathMap
 *
 * Copyright (C) 2002 Mark Probst
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

#include <ctype.h>
#include <assert.h>
#include <string.h>

#include "overload.h"
#include "highlight.h"

static int
find_name (char **list, char *name)
{
    while (*list != 0)
    {
	if (strcmp(*list, name) == 0)
	    return 1;
	++list;
    }
    return 0;
}

static int
resolv_ident (char *name)
{
    static char *keywords[] = { "if", "then", "else", "end", "while", "do", "xor", 0 };
    static char *internals[] = {
        "x", "y", "r", "a", "t", "X", "Y", "W", "H", "R", "frame",
        "xy", "ra", "XY", "WH",
        0
    };
    static char *uservals[] = {
	"user_int", "user_float", "user_bool", "user_color",
	"user_curve", "user_gradient", "user_image",
	0
    };
    static char *constants[] = { "pi", "e", "I", 0 };

    if (find_name(keywords, name))
	return HIGHLIGHT_KEYWORD;
    if (find_name(internals, name))
	return HIGHLIGHT_INTERNAL;
    if (find_name(uservals, name))
	return HIGHLIGHT_USERVAL;
    if (find_name(constants, name))
        return HIGHLIGHT_CONSTANT;
    if (exists_overload_entry_with_name(name))
	return HIGHLIGHT_BUILTIN;
    return HIGHLIGHT_VARIABLE;
}

static int
skip_ws (char *s, int p)
{
    while (s[p] != 0 && isspace(s[p]))
	++p;
    return p;
}

int
next_highlight (char *expr, int start, int *first, int *last)
{
    int i;

    while (expr[start] != 0)
    {
        start = skip_ws(expr, start);
        if (expr[start] == 0)
            return HIGHLIGHT_EOS;

        if (expr[start] == '#')
        {
            for (i = start + 1; expr[i] != 0; ++i)
                if (expr[i] == '\n')
                    break;
            *first = start;
            *last = i;
            
            return HIGHLIGHT_COMMENT;
        }
        else if (expr[start] == '_' || isalpha(expr[start]))
        {
            int j;
    
            for (i = start + 1; expr[i] != 0; ++i)
                if (expr[i] != '_' && !isalnum(expr[i]))
                    break;
    
            *first = start;
            *last = i;
    
            j = skip_ws(expr, i);
            if (expr[j] == ':')
                return HIGHLIGHT_TAG;
    
            {
                char buf[i - start + 1];
    
                memcpy(buf, expr + start, i - start);
                buf[i - start] = 0;
                return resolv_ident(buf);
            }
        }
        else if (expr[start] == '\"')
        {
            *first = start;
            do
            {
                ++start;
            } while (expr[start] != 0 && expr[start] != '\"');
            if (expr[start] == 0)
            {
                *last = start;
                return HIGHLIGHT_ERROR;
            }
            else
            {
                *last = start + 1;
                return HIGHLIGHT_STRING;
            }
        }
        else if (expr[start] == '.' || isdigit(expr[start]))
        {
            int have_comma = (expr[start] == '.');
    
            *first = start;
    
            ++start;
            while (expr[start] != 0 && (expr[start] == '.' || isdigit(expr[start])))
            {
                if (expr[start] == '.')
                {
                    if (have_comma)
                    {
                        *last = start + 1;
                        return HIGHLIGHT_ERROR;
                    }
                    else
                        have_comma = 1;
                }
                ++start;
            }
            *last = start;
            if (have_comma && expr[start - 1] == '.')
                return HIGHLIGHT_ERROR;
            else
                return have_comma ? HIGHLIGHT_FLOAT : HIGHLIGHT_INT;
        }
        ++start;
    }
    return HIGHLIGHT_EOS;
}

/*
int
main (int argc, char *argv[])
{
    int first, last = 0, result;
    assert(argc == 2);
    while ((result = next_highlight(argv[1], last, &first, &last)) != HIGHLIGHT_EOS)
        printf("%d - %d : %d\n", first, last, result);
    return 0;
}
*/
