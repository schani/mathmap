/*
 * highlight.h
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

#ifndef __HIGHLIGHT_H__
#define __HIGHLIGHT_H__

#define HIGHLIGHT_EOS            0      /* end of string */
#define HIGHLIGHT_ERROR          1
#define HIGHLIGHT_COMMENT        2
#define HIGHLIGHT_KEYWORD        3
#define HIGHLIGHT_INTERNAL       4
#define HIGHLIGHT_TAG            5
#define HIGHLIGHT_BUILTIN        6
#define HIGHLIGHT_INT            7
#define HIGHLIGHT_FLOAT          8
#define HIGHLIGHT_STRING         9
#define HIGHLIGHT_USERVAL       10
#define HIGHLIGHT_VARIABLE      11
#define HIGHLIGHT_CONSTANT      12	/* pi, e, I */

int next_highlight (char *expr, int start, int *first, int *last);

#endif
