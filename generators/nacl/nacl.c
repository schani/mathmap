/*
 * nacl.c
 *
 * MathMap
 *
 * Copyright (C) 2012 Mark Probst
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

#include "../../jump.h"
#include "../../mathmap.h"

#include "nacl.h"

int
nacl_generate_plug_in (char *filter, char *output_filename)
{
    return generate_plug_in (filter, output_filename,
			     "nacl_template.c", 1, compiler_template_processor);
}
