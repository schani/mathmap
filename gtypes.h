/* -*- c -*- */

/*
 * gtypes.h
 *
 * MathMap
 *
 * Copyright (C) 2002 Herbert Poetzl, Mark Probst
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

#ifndef __GTYPES_H__
#define __GTYPES_H__

#ifdef OPENSTEP
typedef		unsigned char	guchar;
typedef		int		gint;

typedef		int		gboolean;
#else
#include <glib.h>
#endif

#endif
